setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(tidyr)

df <- read.csv2("_result/logfile.csv", sep = ";", stringsAsFactors = FALSE)

numeric_cols <- c("acc_mean", "kappa_mean", "f1_mean", "mcc_mean", "time_sec", "memory_mb")

df[numeric_cols] <- lapply(df[numeric_cols], function(x) as.numeric(gsub(",", ".", x)))

agg <- df %>%
  group_by(model, embedding) %>%
  summarise(
    acc = mean(acc_mean, na.rm = TRUE),
    kappa = mean(kappa_mean, na.rm = TRUE),
    f1 = mean(f1_mean, na.rm = TRUE),
    mcc = mean(mcc_mean, na.rm = TRUE),
    time = mean(time_sec, na.rm = TRUE),
    memory = mean(memory_mb, na.rm = TRUE),
    .groups = "drop"
  )

make_matrix <- function(metric) {
  agg %>%
    select(model, embedding, !!metric) %>%
    pivot_wider(names_from = embedding, values_from = !!metric)
}

mat_acc   <- make_matrix("acc")
mat_kappa <- make_matrix("kappa")
mat_f1    <- make_matrix("f1")
mat_mcc   <- make_matrix("mcc")
mat_time  <- make_matrix("time")
mat_mem   <- make_matrix("memory")

bold_max <- function(x) {
  m <- max(x, na.rm = TRUE)
  sapply(x, function(v) ifelse(!is.na(v) && v == m, sprintf("\\textbf{%.3f}", v), sprintf("%.3f", v)))
}

bold_min <- function(x) {
  m <- min(x, na.rm = TRUE)
  sapply(x, function(v) ifelse(!is.na(v) && v == m, sprintf("\\textbf{%.3f}", v), sprintf("%.3f", v)))
}

mat_acc[,-1]   <- t(apply(mat_acc[,-1],   1, bold_max))
mat_kappa[,-1] <- t(apply(mat_kappa[,-1], 1, bold_max))
mat_f1[,-1]    <- t(apply(mat_f1[,-1],    1, bold_max))
mat_mcc[,-1]   <- t(apply(mat_mcc[,-1],   1, bold_max))
mat_time[,-1]  <- t(apply(mat_time[,-1],  1, bold_min))
mat_mem[,-1]   <- t(apply(mat_mem[,-1],   1, bold_min))

latex <- "\\begin{table*}[ht]
\\centering
\\renewcommand{\\arraystretch}{1.20}
\\setlength{\\tabcolsep}{14pt}
\\caption{Classifier Performance by Embedding Method (5-Fold Cross Validation)}
\\begin{tabular}{|l|l|rrrrrr|}
\\hline
\\textbf{Classifier} & \\textbf{Metric} &
\\textbf{LSA} & \\textbf{Word2Vec} & \\textbf{FastText} &
\\textbf{SBERT} & \\textbf{CodeBERT} & \\textbf{CodeT5} \\\\ \\hline
"

add_block <- function(model_code, model_label, mat_acc, mat_kappa, mat_f1, mat_mcc, mat_time, mat_mem) {
  r <- which(mat_acc$model == model_code)
  out <- ""
  out <- paste0(out, sprintf("\\multirow{6}{*}{\\textbf{%s}}\n", model_label))
  out <- paste0(out, "& Accuracy    & ", paste(mat_acc[r,-1], collapse=" & "), " \\\\ \\cline{2-8}\n")
  out <- paste0(out, "& Kappa       & ", paste(mat_kappa[r,-1], collapse=" & "), " \\\\ \\cline{2-8}\n")
  out <- paste0(out, "& F1-score    & ", paste(mat_f1[r,-1], collapse=" & "), " \\\\ \\cline{2-8}\n")
  out <- paste0(out, "& MCC         & ", paste(mat_mcc[r,-1], collapse=" & "), " \\\\ \\cline{2-8}\n")
  out <- paste0(out, "& Time (s)    & ", paste(mat_time[r,-1], collapse=" & "), " \\\\ \\cline{2-8}\n")
  out <- paste0(out, "& Memory (MB) & ", paste(mat_mem[r,-1], collapse=" & "), " \\\\ \\hline\n")
  out
}


models_order <- c("rf","nb","svm","lr","xgb")
model_names  <- c("Random Forest","Naive Bayes","SVM (Linear)","Logistic Regression","XGBoost")

for (i in seq_along(models_order)) {
  latex <- paste0(latex, add_block(model_code  = models_order[i], model_label = model_names[i], mat_acc, mat_kappa, mat_f1, mat_mcc, mat_time, mat_mem))
}

latex <- paste0(latex, "\\end{tabular}\n\\label{tab:embedding_performance}\n\\end{table*}")
writeLines(latex, "_result/classifier_vs_embedding.txt")
cat(latex)



######################
######################
library(dplyr)

model_map <- c(
  "rf"  = "Random Forest",
  "nb"  = "Naive Bayes",
  "svm" = "SVM (Linear)",
  "lr"  = "Logistic Regression",
  "xgb" = "XGBoost"
)

mini <- agg %>%
  group_by(model) %>%
  slice_max(order_by = acc, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    class = model_map[model],
    embed = embedding,
    acc    = round(acc, 2),
    f1     = round(f1, 2),
    mcc    = round(mcc, 2),
    time   = round(time, 2),
    memory = round(memory, 2)
  ) %>%
  select(class, embed, acc, f1, mcc, time, memory)

latex <- "\\begin{table}[ht]
\\centering
\\caption{Best Embedding per Classifier (Accuracy-based)}
\\renewcommand{\\arraystretch}{1.2}
\\begin{tabular}{|l|l|r|r|r|r|r|}
\\hline
\\textbf{Classifier} & \\textbf{Embedding} & \\textbf{Acc} & \\textbf{F1} & \\textbf{MCC} & \\textbf{Time} & \\textbf{Memory} \\\\ \\hline
"

for (i in 1:nrow(mini)) {
  line <- sprintf(
    "%s & %s & %.2f & %.2f & %.2f & %.2f & %.2f \\\\ \\hline",
    mini$class[i], mini$embed[i], mini$acc[i], mini$f1[i], mini$mcc[i], mini$time[i], mini$memory[i])
  latex <- paste0(latex, line, "\n")
}

latex <- paste0(latex, "\\end{tabular}\n\\label{tab:embedding_performance_mini}\n\\end{table}")
writeLines(latex, "_result/embedding_performance_mini.txt")
cat(latex)