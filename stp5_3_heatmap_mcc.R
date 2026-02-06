library(dplyr)
library(ggplot2)
library(patchwork)
library(readr)

csv_path <- "_result/logfile.csv"
out_dir  <- "_figure"
dir.create(out_dir, showWarnings = FALSE)

eps <- 1e-12

df <- read_delim(csv_path, delim = ";", locale = locale(decimal_mark = "."), trim_ws = TRUE, show_col_types = FALSE, guess_max = 200000)
num_cols <- c("acc_mean","kappa_mean","f1_mean","mcc_mean","time_sec","memory_mb")
num_cols <- intersect(num_cols, names(df))
df <- df %>% mutate(across(all_of(num_cols), ~ suppressWarnings(as.numeric(.x))))

model_map <- c(rf  = "Random Forest", nb  = "Naive Bayes", svm = "SVM (Linear)", lr  = "Logistic Regression", xgb = "XGBoost")

if ("model" %in% names(df)) {
  df <- df %>% mutate(model_name = model_map[as.character(model)])
} else if ("model_name" %in% names(df)) {
  df <- df %>% mutate(model_name = as.character(model_name))
} else {
  stop("There is no column name model or model_name in CSV.")
}

req <- c("embedding", "model_name", "acc_mean", "f1_mean", "mcc_mean", "kappa_mean")
missing_req <- setdiff(req, names(df))
if (length(missing_req) > 0) {
  stop(paste("Eksik kolon(lar):", paste(missing_req, collapse = ", ")))
}

agg <- df %>% group_by(model_name, embedding) %>%
  summarise(acc   = mean(acc_mean,   na.rm = TRUE), f1    = mean(f1_mean,    na.rm = TRUE),
    mcc   = mean(mcc_mean,   na.rm = TRUE), kappa = mean(kappa_mean, na.rm = TRUE), .groups = "drop")

model_levels <- c("Logistic Regression","Naive Bayes","SVM (Linear)","Random Forest","XGBoost")
embedding_levels <- c("CodeBERT","CodeT5","FastText","LSA","SBERT","Word2Vec")

heatmap_theme <- theme(
  axis.title.x = element_text(size = 13, face = "bold", color = "black"),
  axis.title.y = element_text(size = 13, face = "bold", color = "black"),
  axis.text.x  = element_text(size = 12, face = "bold", color = "black", hjust = 0.5),
  axis.text.y  = element_text(size = 12, face = "bold", color = "black"),
  legend.title = element_text(size = 14, face = "bold", color = "black"),
  legend.text  = element_text(size = 14, face = "bold", color = "black"),
  plot.title   = element_text(size = 14, face = "bold", color = "black", hjust = 0.5),
  panel.border = element_rect(color = "gray", fill  = NA, linewidth = 0.05),
  text = element_text(color = "black"),
  legend.position="top"
)

plot_df <- agg %>% mutate( model_name = factor(model_name, levels = model_levels),
    embedding  = factor(embedding,  levels = embedding_levels)) %>% group_by(model_name) %>%
  mutate(delta_mcc = mcc - max(mcc, na.rm = TRUE)) %>% ungroup() %>% mutate(mcc_plot = ifelse(!is.na(mcc) & abs(mcc) < eps, NA, mcc),
    delta_mcc_plot = ifelse(!is.na(mcc) & abs(mcc) < eps, NA, delta_mcc))

p_mcc <- ggplot(plot_df, aes(embedding, model_name, fill = mcc_plot)) +
  geom_tile(color = "white") + geom_text(aes(label = ifelse(is.na(mcc_plot), "", sprintf("%.2f", mcc_plot))), size = 4, fontface = "bold") +
  scale_fill_gradient( low = "white", high = "#2166ac", na.value = "grey90", name = "") +
  labs(title = "MCC", x = "Embedding Method", y = "Classifier") + theme_minimal(base_size = 12) + heatmap_theme
p_mcc <- p_mcc + guides(fill = guide_colorbar(title.position = "top",title.hjust = 0.1,barwidth = unit(10, "cm"),barheight = unit(0.2, "cm")))

p_delta <- ggplot(plot_df, aes(embedding, model_name, fill = delta_mcc_plot)) +
  geom_tile(color = "white") + geom_text(aes(label = ifelse(is.na(delta_mcc_plot), "", sprintf("%.2f", delta_mcc_plot))), size = 4, fontface = "bold") +
  scale_fill_gradient2(low = "#b2182b", mid = "white", high = "#2166ac", midpoint = 0,
    limits = c(min(plot_df$delta_mcc_plot, na.rm = TRUE), 0), na.value = "grey90", name = "") + scale_y_discrete(position = "right") +
  labs(title = "Delta MCC (Delta-from-Best)", x = "Embedding Method", y = "Classifier") + theme_minimal(base_size = 12) + heatmap_theme
p_delta <- p_delta +guides(fill = guide_colorbar(title.position = "top",title.hjust = 0.1,barwidth = unit(10, "cm"),barheight = unit(0.2, "cm")))

p_final <- p_mcc + p_delta + plot_layout(ncol = 2, widths = c(1, 1))
ggsave( filename = file.path(out_dir, "heatmap_mcc_and_delta_mcc.pdf"), plot = p_final, width = 14, height = 5)