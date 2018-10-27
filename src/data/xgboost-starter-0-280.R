library(data.table)
library(Matrix)
library(xgboost)
library(caret)
library(dplyr)
library(MLmetrics)

cat("Init")
set.seed(12345)
PATH <- "../input/"

cat("Functions")
xgb_normalizedgini <- function(preds, dtrain){
  actual <- getinfo(dtrain, "label")
  score <- NormalizedGini(preds,actual)
  return(list(metric = "NormalizedGini", value = score))
}

cat("Load data")
train <- fread(paste0(PATH,"train.csv"), sep=",", na.strings = "", stringsAsFactors=T)
test <- fread(paste0(PATH,"test.csv"), sep=",", na.strings = "", stringsAsFactors=T)

cat("Combine train and test files")
test$target <- NA
data <- rbind(train, test)
rm(train,test);gc()

cat("Feature engineering")
data[, amount_nas := rowSums(data == -1, na.rm = T)]
data[, high_nas := ifelse(amount_nas>4,1,0)]
data[, ps_car_13_ps_reg_03 := ps_car_13*ps_reg_03]
data[, ps_reg_mult := ps_reg_01*ps_reg_02*ps_reg_03]
data[, ps_ind_bin_sum := ps_ind_06_bin+ps_ind_07_bin+ps_ind_08_bin+ps_ind_09_bin+ps_ind_10_bin+ps_ind_11_bin+ps_ind_12_bin+ps_ind_13_bin+ps_ind_16_bin+ps_ind_17_bin+ps_ind_18_bin]

cat("Prepare for xgb")
cvFolds <- createFolds(data$target[!is.na(data$target)], k=5, list=TRUE, returnTrain=FALSE)
varnames <- setdiff(colnames(data), c("id", "target"))
train_sparse <- Matrix(as.matrix(data[!is.na(target), varnames, with=F]), sparse=TRUE)
test_sparse <- Matrix(as.matrix(data[is.na(target), varnames, with=F]), sparse=TRUE)
y_train <- data[!is.na(target),target]
test_ids <- data[is.na(target),id]
dtrain <- xgb.DMatrix(data=train_sparse, label=y_train)
dtest <- xgb.DMatrix(data=test_sparse)

cat("Params for xgb")
param <- list(booster="gbtree",
              objective="binary:logistic",
              eta = 0.02,
              gamma = 1,
              max_depth = 6,
              min_child_weight = 1,
              subsample = 0.8,
              colsample_bytree = 0.8
)

cat("xgb cross-validation, uncomment when running locally")
# xgb_cv <- xgb.cv(data = dtrain,
#                  params = param,
#                  nrounds = 5000,
#                  feval = xgb_normalizedgini,
#                  maximize = TRUE,
#                  prediction = TRUE,
#                  folds = cvFolds,
#                  print_every_n = 25,
#                  early_stopping_round = 30)
# best_iter <- xgb_cv$best_iteration
best_iter <- 540

cat("xgb model")
xgb_model <- xgb.train(data = dtrain,
                       params = param,
                       nrounds = best_iter,
                       feval = xgb_normalizedgini,
                       maximize = TRUE,
                       watchlist = list(train = dtrain),
                       verbose = 1,
                       print_every_n = 25
)

cat("Feature importance")
names <- dimnames(train_sparse)[[2]]
importance_matrix <- xgb.importance(names, model=xgb_model)
xgb.plot.importance(importance_matrix)

cat("Predict and output csv")
preds <- data.table(id=test_ids, target=predict(xgb_model,dtest))
write.table(preds, "submission.csv", sep=",", dec=".", quote=FALSE, row.names=FALSE)
