library(tidyverse)
library(xgboost)
library(magrittr)
library(caret)
library(irlba)
library(h2o)
library(moments)

set.seed(0)

#---------------------------
cat("Loading data...\n")

tr <- read_csv("../input/train.csv")
te <- read_csv("../input/test.csv")

#---------------------------
cat("Defining auxiliary functions...\n")

get_dup <- function(x) 
  lapply(x, c) %>% 
  duplicated %>% 
  which 

get_cor <- function(x) 
  data.matrix(x) %>% 
  cor(method = "spearman") %>% 
  findCorrelation(cutoff = 0.98)

is_nonzerovar <- function(x) 
  var(x) != 0

get_xgb <- function(num_class = 2, nfold = 5) {
  set.seed(0)
  p <- list(tree_method = "hist",
            grow_policy = "lossguide",
            objective = "multi:softprob",
            eval_metric = "mlogloss",
            num_class = num_class,
            nthread = 8,
            eta = 0.15,
            max_depth = 0,
            min_child_weight = 10,
            max_leaves = 14,
            subsample = 0.632,
            colsample_bytree = sqrt(ncol(tr_te)) / ncol(tr_te))
  dtrain <- xgb.DMatrix(data = data.matrix(tr_te[tri, ]), 
                        label = as.integer(cut(target, num_class)) - 1)
  dtest <- xgb.DMatrix(data = data.matrix(tr_te[-tri, ]))
  cv <- xgb.cv(p, dtrain, print_every_n = 50, prediction = TRUE, nfold = nfold,
               early_stopping_rounds = 50, nrounds = 500)
  nrounds <- round(cv$best_iteration * (1 + 1 / nfold))
  m_xgb <- xgb.train(p, dtrain, nrounds, verbose = -1)
  pred <- rbind(cv$pred, predict(m_xgb, dtest, reshape = TRUE)) %>% 
    as_tibble() %>% 
    mutate(which_class = apply(., 1, which.max)) %>% 
    set_names(paste0("xgb", num_class, "_", 1:ncol(.)))
}

#---------------------------
cat("Binding datasets...\n")

tri <- 1:nrow(tr)
target <- log1p(tr$target)

tr %>% 
  select(-ID, -target) %>%
  select_if(is_nonzerovar) %>%
  select(-get_dup(.)) %>%
  select(-get_cor(.)) %>% 
  bind_rows(select(te, names(.))) ->
  tr_te

rm(tr, te); gc()

#---------------------------
cat("Creating PCA features...\n")

n_pca <- 50
m_pca <- prcomp_irlba(tr_te, n = n_pca, scale. = TRUE)
tr_te_pca <- m_pca$x %>% as_tibble

rm(m_pca); gc()

#---------------------------
cat("Creating XGB features...\n")

k <- 2:6
tr_te_xgb <- data.frame(row.names = 1:nrow(tr_te))
for (i in seq_along(k)) {
  cat("k =", k[i], "\n")
  tr_te_xgb <- cbind(tr_te_xgb, get_xgb(k[i]))
  gc()
}

#---------------------------
cat("Creating AE features...\n")

h2o.no_progress()
h2o.init(nthreads = 4, max_mem_size = "10G")

tr_te_h2o <- as.h2o(tr_te)

n_ae <- 6
m_ae <- h2o.deeplearning(training_frame = tr_te_h2o,
                         x = 1:ncol(tr_te_h2o),
                         autoencoder = T,
                         activation="Tanh",
                         reproducible = TRUE,
                         seed = 0,
                         sparse = T,
                         standardize = TRUE,
                         hidden = c(32, n_ae, 32),
                         max_w2 = 5,
                         epochs = 15)
tr_te_ae <- h2o.deepfeatures(m_ae, tr_te_h2o, layer = 2) %>% as_tibble

h2o.shutdown(prompt = FALSE)

rm(m_ae); gc()

#---------------------------
cat("Binding features...\n")
tr_te %<>% 
  mutate(mean = apply(tr_te, 1, mean),
         gmean = apply(tr_te, 1, function(x) expm1(mean(log1p(x)))),
         sd = apply(tr_te, 1, sd),
         max = apply(tr_te, 1, max),
         kurt = apply(tr_te, 1, kurtosis),
         skew = apply(tr_te, 1, skewness),
         row_sum = apply(tr_te, 1, sum),
         iqr = apply(tr_te, 1, IQR),
         n_uniq = apply(tr_te, 1, n_distinct),
         n_zeros = apply(tr_te, 1, function(x) sum(x == 0)),
         zero_frac = n_zeros / ncol(tr_te)) %>% 
  bind_cols(tr_te_pca) %>% 
  bind_cols(tr_te_ae) %>% 
  bind_cols(tr_te_xgb) %>% 
  data.matrix()

#---------------------------
cat("Preparing data...\n")

dtest <- xgb.DMatrix(data = tr_te[-tri, ])
tr_te <- tr_te[tri, ]
tri <- createDataPartition(target, p = 0.9, list = F) %>% c()
dtrain <- xgb.DMatrix(data = tr_te[tri, ], label = target[tri])
dval <- xgb.DMatrix(data = tr_te[-tri, ], label = target[-tri])
cols <- colnames(tr_te)

rm(tr_te, target, tri); gc()

#---------------------------
cat("Training model...\n")

p <- list(objective = "reg:linear",
          booster = "gbtree",
          eval_metric = "rmse",
          nthread = 8,
          eta = 0.007,
          max_depth = 32,
          min_child_weight = 125,
          gamma = 8.7297171,
          subsample = 0.8525110,
          colsample_bytree = 0.27204236,
          colsample_bylevel = 0.20806632,
          alpha = 0,
          lambda = 71.414580,
          nrounds = 10000)

m_xgb <- xgb.train(p, dtrain, p$nrounds, list(val = dval), print_every_n = 100, early_stopping_rounds = 700)

xgb.importance(cols, model = m_xgb) %>% 
  xgb.plot.importance(top_n = 30)

#---------------------------
cat("Making submission file...\n")

read_csv("../input/sample_submission.csv") %>%  
  mutate(target = expm1(predict(m_xgb, dtest))) %>%
  write_csv(paste0("xgb_aec_", round(m_xgb$best_score, 5), ".csv"))