setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(randomForest)
library(e1071)
library(naivebayes)
library(xgboost)
library(MLmetrics)
library(pryr)
library(nnet)

embeddings <- c("LSA", "Word2Vec", "FastText", "SBERT", "CodeBERT", "CodeT5")
models <- c("rf", "svm", "nb", "xgb", "lr")

#F1-Macro
macro_f1 <- function(y_true, y_pred) {
  classes <- unique(y_true)
  f1_list <- c()
  for (cls in classes) {
    y_true_bin <- ifelse(y_true == cls, 1, 0)
    y_pred_bin <- ifelse(y_pred == cls, 1, 0)
    f1 <- MLmetrics::F1_Score(y_true = y_true_bin, y_pred = y_pred_bin)
    f1_list <- c(f1_list, f1)
  }
  mean(f1_list)
}

#MCC
mcc_multiclass <- function(y_true, y_pred) {
  cm <- table(y_true, y_pred)
  n <- sum(cm)
  sum_row <- rowSums(cm)
  sum_col <- colSums(cm)
  c <- sum(diag(cm))
  numerator <- (n * c) - sum(sum_row * sum_col)
  denominator <- sqrt((n^2 - sum(sum_row^2)) * (n^2 - sum(sum_col^2)))
  if (denominator == 0) return(0)
  numerator / denominator
}

dir.create("_result", showWarnings = FALSE)
result_file <- file.path("_result", paste0("log_", format(Sys.time(), "%Y_%m_%d_%H_%M"), ".csv"))

columns <- c("embedding", "model", "seed", paste0("acc_", 1:5), "acc_mean", paste0("kappa_", 1:5), "kappa_mean", paste0("f1_", 1:5),  "f1_mean", paste0("mcc_", 1:5), "mcc_mean", "time_sec", "memory_mb")
write.table(t(columns), file = result_file, sep = ";", col.names = FALSE, row.names = FALSE, quote = FALSE)

set.seed(123)
seeds <- sample(1:10000, 10)

run_model <- function(X, y, folds, model, embedding, seed, result_file) {
  set.seed(seed)
  folds <- sample(rep(1:5, length.out = nrow(X)))
  
  acc <- f1 <- mcc <- kappa <- numeric(5)
  classes <- levels(y)
  
  start_time <- Sys.time()
  mem_before <- pryr::mem_used()
  
  for (k in 1:5) {
    tr <- folds != k
    ts <- folds == k
    
    X_tr <- X[tr, ]; y_tr <- y[tr]
    X_ts <- X[ts, ]; y_ts <- y[ts]
    
    if (model == "rf") {
      fit <- randomForest(X_tr, y_tr)
      pred <- predict(fit, X_ts)
    }
    if (model == "svm") {
      fit <- svm(X_tr, y_tr, kernel="linear")
      pred <- predict(fit, X_ts)
    }
    if (model == "nb") {
      fit <- naive_bayes(X_tr, y_tr)
      pred <- predict(fit, X_ts)
    }
    if (model == "xgb") {
      dtr <- xgb.DMatrix(X_tr, label = as.numeric(y_tr)-1)
      dts <- xgb.DMatrix(X_ts)
      params <- list(objective = "multi:softmax", num_class = length(classes), max_depth = 6, eta = 0.3, subsample = 0.8, colsample_bytree = 0.8, eval_metric = "mlogloss")
      fit <- xgboost(data = dtr, params = params, nrounds = 50, verbose = 0)
      pred <- classes[predict(fit, dts) + 1]
    }
    if (model == "lr") {
      df_tr <- data.frame(X_tr)
      df_tr$label <- y_tr
      df_ts <- data.frame(X_ts)
      fit <- nnet::multinom(label ~ ., data = df_tr, trace = FALSE)
      pred <- predict(fit, newdata = df_ts)
    }
    
    cm <- table(y_ts, pred)
    po <- sum(diag(cm)) / sum(cm)
    pe <- sum(rowSums(cm) * colSums(cm)) / (sum(cm)^2)
    
    acc[k] <- mean(pred == y_ts)
    kappa[k] <- (po - pe) / (1 - pe)
    f1[k]  <- macro_f1(y_ts, pred)
    mcc[k] <- mcc_multiclass(y_ts, pred)
  }
  
  time_sec <- as.numeric(Sys.time() - start_time, units="secs")
  mem_mb <- as.numeric(pryr::mem_used() - mem_before) / (1024^2)
  
  row <- data.frame(
    embedding = embedding,
    model = model,
    seed = seed,
    acc_1=acc[1], acc_2=acc[2], acc_3=acc[3], acc_4=acc[4], acc_5=acc[5],
    acc_mean = mean(acc),
    kappa_1=kappa[1], kappa_2=kappa[2], kappa_3=kappa[3], kappa_4=kappa[4], kappa_5=kappa[5],
    kappa_mean = mean(kappa),
    f1_1=f1[1], f1_2=f1[2], f1_3=f1[3], f1_4=f1[4], f1_5=f1[5],
    f1_mean = mean(f1),
    mcc_1=mcc[1], mcc_2=mcc[2], mcc_3=mcc[3], mcc_4=mcc[4], mcc_5=mcc[5],
    mcc_mean = mean(mcc),
    time_sec = time_sec,
    memory_mb = mem_mb
  )
  
  write.table(row, file = result_file, sep=";", append=TRUE, col.names=FALSE, row.names=FALSE, quote=FALSE)
}

for (embedding in embeddings) {
  cat("\n=== Embedding:", embedding, "===\n")
  embedding_file <- paste0("_output/comments_v6_", tolower(embedding), ".txt")
  
  df_embed <- read.csv2(embedding_file, sep = ";", stringsAsFactors = TRUE, fileEncoding = "UTF-8")
  df_embed <- subset(df_embed, label != "NOSMELL")
  df_embed$label <- droplevels(df_embed$label)
  df_embed$clean_comment <- NULL
  
  X <- as.matrix(data.frame(lapply(df_embed[, grep("^v", names(df_embed))], as.numeric)))
  y <- df_embed$label
  
  for (seed in seeds) {
    cat("  Seed:", seed, "\n")
    folds <- sample(rep(1:5, length.out = nrow(X)))
    
    for (model in models) {
      cat("Model:", model, "\n")
      run_model(X = X, y = y, folds = folds, model = model, embedding = embedding, seed = seed, result_file = result_file)
    }
  }
}