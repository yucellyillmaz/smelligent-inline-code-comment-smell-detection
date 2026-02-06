setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(ggplot2)
library(readr)
library(patchwork)

csv_path <- "_result/logfile.csv" 
out_dir  <- "_figure"
dir.create(out_dir, showWarnings = FALSE)

df <- read_delim( csv_path, delim = ";",locale = locale(decimal_mark = "."),
  trim_ws = TRUE, show_col_types = FALSE, guess_max = 200000)

model_map <- c(lr  = "Logistic Regression",nb  = "Naive Bayes",svm = "SVM (Linear)",rf  = "Random Forest",xgb = "XGBoost")

if ("model" %in% names(df)) {
  df <- df %>% mutate(model = as.character(model),model_name = ifelse(model %in% names(model_map), model_map[model], model))
} else if ("model_name" %in% names(df)) {
  df <- df %>% mutate(model_name = as.character(model_name))
} else {
  stop("There is no model or model_name column in CSV.")
}

theme_publication <- theme(
  axis.title.x = element_text(size = 11, face = "bold", color = "black"),
  axis.title.y = element_text(size = 14, face = "bold", color = "black"),
  axis.text.x  = element_text(size = 11, face = "bold", color = "black", hjust = 0.5),
  axis.text.y  = element_text(size = 14, face = "bold", color = "black"),
  legend.title = element_text(size = 12, face = "bold", color = "black"),
  legend.text  = element_text(size = 12, color = "black"),
  strip.text   = element_text(size = 14, face = "bold", color = "black"),
  plot.title   = element_text(size = 14, face = "bold", color = "black", hjust = 0.5),
  panel.border = element_rect(color = "gray", fill  = NA, linewidth = 0.1),
  text = element_text(color = "black")
)

pd <- position_dodge(width = 0.6)

summarise_metric <- function(df, metric_col, by_x = c("embedding", "model_name")) {
  m <- metric_col
  
  df %>% filter(!is.na(.data[[m]]), is.finite(.data[[m]]), .data[[m]] != 0) %>%
    group_by(across(all_of(by_x))) %>%
    summarise(
      n = n(),
      mean_val = mean(.data[[m]], na.rm = TRUE),
      sd_val   = ifelse(n > 1, sd(.data[[m]], na.rm = TRUE), 0),
      .groups  = "drop"
    ) %>%
    filter(is.finite(mean_val), is.finite(sd_val), mean_val != 0)
}

embedding_levels <- c("CodeBERT", "CodeT5", "FastText", "LSA", "SBERT", "Word2Vec")
model_levels <- c("Logistic Regression", "Naive Bayes", "SVM (Linear)", "Random Forest", "XGBoost")

plot_metric_by_embedding <- function(df_sum, ylab, legend_title, panel_title) {
  df_sum <- df_sum %>%  mutate(embedding  = factor(embedding, levels = embedding_levels),
      model_name = factor(model_name, levels = model_levels))
  
  ggplot(df_sum, aes(x = embedding, y = mean_val, color = model_name, shape = model_name, group = model_name)) +
    geom_point(size = 2, position = pd) +
    geom_errorbar(aes(ymin = mean_val - sd_val, ymax = mean_val + sd_val), width = 0.5, position = pd) +
    geom_line(linetype = "dashed", linewidth = 0.7, alpha = 0.7, position = pd) +
    labs(title = panel_title, x = "Embedding Method", y = ylab, color = legend_title, shape = legend_title) +
    theme_minimal() +  theme_publication +  theme(legend.position = "right")
}

plot_metric_by_classifier <- function(df_sum, ylab, legend_title, panel_title) {
  df_sum <- df_sum %>%mutate( model_name = factor(model_name, levels = model_levels),
      embedding  = factor(embedding, levels = embedding_levels))
  
  ggplot(df_sum,aes(x = model_name, y = mean_val, color = embedding, shape = embedding, group = embedding)) +
    geom_point(size = 2, position = pd) +
    geom_errorbar(aes(ymin = mean_val - sd_val, ymax = mean_val + sd_val),width = 0.5, position = pd) +
    geom_line(linetype = "dashed", linewidth = 0.7, alpha = 0.7, position = pd) +
    labs(
      title = panel_title,
      x = "Classifier",
      y = ylab,
      color = legend_title,
      shape = legend_title
    ) +
    theme_minimal() +
    theme_publication +
    theme(legend.position = "right")
}

stopifnot(all(c("mcc_mean", "kappa_mean", "embedding", "model_name") %in% names(df)))

kappa_embed_sum <- summarise_metric(df, "kappa_mean", by_x = c("embedding", "model_name"))

kappa_clf_sum   <- summarise_metric(df, "kappa_mean", by_x = c("model_name", "embedding"))

p_kappa_embed <- plot_metric_by_embedding(
  kappa_embed_sum,
  ylab = "Mean Cohen's Kappa",
  legend_title = "Classifier",
  panel_title = "Cohen's Kappa across Embedding Methods"
)

p_kappa_clf <- plot_metric_by_classifier(
  kappa_clf_sum,
  ylab = "Mean Cohen's Kappa",
  legend_title = "Embedding Method",
  panel_title = "Cohen's Kappa across Classifiers"
)

library(patchwork)
row <- p_kappa_embed + p_kappa_clf + plot_layout(ncol = 2, widths = c(1, 1.1))
ggsave(file.path(out_dir, "mean_sd_kappa.pdf"), row, width = 18, height = 5, device = cairo_pdf)