library(dplyr)
library(ggplot2)
library(ggridges)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

infile <- "_result/logfile.csv"
df <- read.csv2(infile, sep = ";", stringsAsFactors = FALSE)
to_num <- function(x) as.numeric(gsub(",", ".", trimws(as.character(x))))

need_num <- c("acc_mean","f1_mean","mcc_mean","kappa_mean","time_sec","memory_mb")
df[need_num] <- lapply(df[need_num], to_num)

model_map <- c(nb  = "Naive Bayes", rf  = "Random Forest", svm = "SVM (Linear)", lr  = "Logistic Regression", xgb = "XGBoost")
out_dir <- "_figure"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

df_runtime <- df %>% mutate(model_name = model_map[model], runtime = time_sec, embedding = as.character(embedding)) %>%
  filter(!is.na(model_name), !is.na(runtime), runtime > 0)

df_all <- df_runtime %>% mutate(model_name = "ALL (combined)")
df_ridge <- bind_rows(df_runtime, df_all)

df_ridge$model_name <- factor(df_ridge$model_name,
  levels = c("Naive Bayes", "Logistic Regression", "SVM (Linear)", "Random Forest", "XGBoost", "ALL (combined)"))

p_runtime_ridge <- ggplot(df_ridge, aes(x = runtime, y = embedding, fill = embedding)) +
  geom_density_ridges(alpha = 0.7, scale = 1.1, rel_min_height = 0.01, color = "black", size = 0.25) +
  facet_wrap(~ model_name, ncol = 3, scales = "free_x") +
  labs(x = "Runtime (seconds)", y = "Embedding Method") + theme_minimal() +
  theme(
    text = element_text(family = "Helvetica", color = "black"),
    axis.title.x = element_text(face = "bold", size = 14, color = "black"),
    axis.title.y = element_text(face = "bold", size = 14, color = "black"),
    axis.text.x  = element_text(face = "bold", size = 14, color = "black"),
    axis.text.y  = element_text(face = "bold", size = 14, color = "black"),
    strip.text   = element_text(face = "bold", size = 14, color = "black"),
    legend.position = "none",
    panel.border = element_rect(color = "gray", fill = NA, linewidth = 0.05),
    panel.spacing = unit(0.5, "lines")
  )

df_mem <- df %>% mutate(model_name = model_map[model], embedding  = as.character(embedding), memory_mb  = memory_mb) %>%
  filter(!is.na(model_name), !is.na(embedding), !is.na(memory_mb), memory_mb > 0) %>%
  mutate(model_name = factor(model_name, levels = clf_levels))

df_mem_bar <- df_mem %>% group_by(embedding, model_name) %>% summarise(mean_memory = mean(memory_mb, na.rm = TRUE), .groups = "drop")

p_mem_bar_one <- ggplot(df_mem_bar, aes(y = embedding, x = mean_memory, fill = model_name)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) + scale_x_log10() +
  scale_fill_manual(values = pal, name = "Classifier") +
  labs(x = "Average Memory Usage (MB, log10 scale)", y = "Embedding Method") +  theme_minimal() +
  theme(
    text = element_text(family="Helvetica", color="black"),
    axis.title = element_text(face="bold", size=14, color="black"),
    axis.text  = element_text(face="bold", size=14, color="black"),
    legend.title = element_text(face="bold", size=14, color="black"),
    legend.text  = element_text(face="bold", size=13, color="black"),
    panel.border = element_rect(color = "gray", fill = NA, linewidth = 0.05),
    legend.position = "right",
  )
p_combo <- p_runtime_ridge + plot_spacer() + p_mem_bar_one + plot_layout(ncol = 3, widths = c(1.4,  0.1, 0.6))

ggsave(file.path(out_dir, "ridge_runtime_and_bar_memory.pdf"), p_combo, width = 16, height = 6)