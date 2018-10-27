#From https://www.kaggle.com/kailex/tidy-xgboost-glmnet-text2vec-lsa


library(tm)
library(tidyverse)
library(magrittr)
library(text2vec)
library(tokenizers)
library(xgboost)
library(lightgbm)
library(glmnet)
library(doParallel)
registerDoParallel(4)

set.seed(100)

train <- read.csv("../input/reddit_train.csv", stringsAsFactors = FALSE) 
train$BODY <- train$BODY %>% enc2utf8
test <- read.csv("../input/reddit_test.csv", stringsAsFactors = FALSE)
test$BODY <- test$BODY %>% enc2utf8

subm <- data.frame(matrix(nrow = nrow(test)))

tri <- 1:nrow(train)
target <- c("REMOVED")

#---------------------------
cat("Basic preprocessing & stats...\n")
tr   <- train %>% 
        select(-REMOVED) %>% 
        #bind_rows(test) %>%
        mutate(nlength = str_length(BODY),
               ncap = str_count(BODY, "[A-Z]"),
               ncap_len = ncap / nlength,
               nexcl = str_count(BODY, fixed("!")),
               nquest = str_count(BODY, fixed("?")),
               npunct = str_count(BODY, "[[:punct:]]"),
               nword = str_count(BODY, "\\w+"),
               nsymb = str_count(BODY, "&|@|#|\\$|%|\\*|\\^"),
               nsmile = str_count(BODY, "((?::|;|=)(?:-)?(?:\\)|D|P))")) %>%
        select(-X,-X.1) %T>% 
        glimpse()

te   <- test %>% 
        select(-REMOVED) %>% 
        #bind_rows(test) %>%
        mutate(nlength = str_length(BODY),
               ncap = str_count(BODY, "[A-Z]"),
               ncap_len = ncap / nlength,
               nexcl = str_count(BODY, fixed("!")),
               nquest = str_count(BODY, fixed("?")),
               npunct = str_count(BODY, "[[:punct:]]"),
               nword = str_count(BODY, "\\w+"),
               nsymb = str_count(BODY, "&|@|#|\\$|%|\\*|\\^"),
               nsmile = str_count(BODY, "((?::|;|=)(?:-)?(?:\\)|D|P))")) %>%
        select(-X, -X.1) %T>% 
        glimpse()
#---------------------------
cat("Parsing comments...\n")
it_tr <- tr %$%
        str_to_lower(BODY) %>%
        str_replace_all("[^[:alpha:]]", " ") %>%
        str_replace_all("\\s+", " ") %>%
        itoken(tokenizer = tokenize_word_stems)

it_te <- te %$%
        str_to_lower(BODY) %>%
        str_replace_all("[^[:alpha:]]", " ") %>%
        str_replace_all("\\s+", " ") %>%
        itoken(tokenizer = tokenize_word_stems)

vectorizer_tr <- create_vocabulary(it_tr, 
                                ngram = c(1, 3)) %>%
                prune_vocabulary(term_count_min = 10, 
                                 doc_proportion_max = 0.5,
                                 doc_proportion_min = 0.001,
                                 vocab_term_max = 2000) %>%
                vocab_vectorizer()

m_tfidf <- TfIdf$new(norm = "l1", sublinear_tf = T)
tfidf_tr <- create_dtm(it_tr, vectorizer_tr) %>%
        fit_transform(m_tfidf)
tfidf_te <- create_dtm(it_te, vectorizer_tr) %>%
        fit_transform(m_tfidf)

####LSA appears to decrease performance
#m_lsa <- LSA$new(n_topics = 25, method = "randomized")
#lsa_tr <- fit_transform(tfidf_tr, m_lsa)
#lsa_te <- tfidf_te %*% t(m_lsa$components)
#colnames(lsa_tr) <- paste("lsa topic",1:ncol(lsa_tr))
#colnames(lsa_te) <- paste("lsa topic",1:ncol(lsa_tr))
#
#colnames(tfidf_tr)[which(colnames(tfidf_tr)=="tree")] <- "TREE" #namespace issue
#                                                                #see github/xgbexplainer
#colnames(tfidf_te)[which(colnames(tfidf_te)=="tree")] <- "TREE" 

#---------------------------
cat("Preparing data for glmnet...\n")
X <- tr %>% 
        select(-BODY) %>% 
        sparse.model.matrix(~ . - 1, .) %>% 
        cbind(tfidf_tr)#, lsa_tr)

X_test <- te %>% 
        select(-BODY) %>% 
        sparse.model.matrix(~ . - 1, .) %>% 
        cbind(tfidf_te)#, lsa_te)


#rm(tr_te, tri); gc()

#---------------------------
cat("Training & predicting...\n")

y <- train[[target]]

p <- list(objective = "binary:logistic", 
          booster = "gbtree", 
          eval_metric = "auc", 
          nthread = 4, 
          eta = 0.2, 
          max_depth = 3,
          min_child_weight = 3,
          subsample = 0.7,
          colsample_bytree = 0.8)

lgb.train = lgb.Dataset(data=X, label=y)

lgb.grid = list(objective = "binary",
                metric = "auc",
                bagging_fraction = 0.8, 
                bagging_freq = 5,
                min_data_in_leaf = 100)

lgb.model.cv = lgb.cv(params = lgb.grid, data = lgb.train, learning_rate = 0.03, num_leaves = 76,
                   num_threads = 4 , nrounds = 7000, early_stopping_rounds = 50,
                   eval_freq = 20, eval = "auc", nfold = 5, stratified = TRUE)

best.iter = lgb.model.cv$best_iter


cat("\nFitting", target, "...\n")

lgb.model = lgb.train(params = lgb.grid, data = lgb.train, learning_rate = 0.03,
                      num_leaves = 76, num_threads = 4 , nrounds = best.iter,
                      eval_freq = 20, eval = "auc")
m_xgb <- xgboost(X, y, params = p, print_every_n = 50, nrounds = 275)
m_glm <- cv.glmnet(X, factor(y), alpha = 0, family = "binomial", type.measure = "auc",
                   parallel = T, standardize = T, nfolds = 8, nlambda = 50,
                   lambda=10^seq(from=2, to=-2, by=-.01))

library(pROC)


test$lgbm <- predict(lgb.model, X_test)
test$xgb <- predict(m_xgb, X_test) %>% 
        round(digits = 5)
test$glmnet <- predict(m_glm, X_test, 
                       type = "response", 
                       s = "lambda.min") %>% 
        as.vector %>% 
        round(digits = 5)

test %>% summarise(auc.lgbm = auc(REMOVED, lgbm),
                   auc.xgb = auc(REMOVED, xgb),
                   auc.glmnet = auc(REMOVED, glmnet))
cor(test[,c("lgbm", "xgb", "glmnet")], method = "spearman")


test$prob <- 0.2*test$xgb + 0.6*test$glmnet + test$lgbm*0.2

test %>% summarise(auc(REMOVED, prob))

xgb.save(m_xgb, 'xgb.model')
save(m_glm = m_glm, file = 'glmnet.model.RData')
lgb.save(lgb.model, 'lgb.model')