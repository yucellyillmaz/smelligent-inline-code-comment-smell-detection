library(dplyr)
library(tidyr)
library(ggplot2)

infile <- "_result/logfile.csv"
df <- read.csv2(infile, sep = ";", stringsAsFactors = FALSE)
to_num <- function(x) as.numeric(gsub(",", ".", trimws(as.character(x))))

need_num <- c("acc_mean","f1_mean","mcc_mean","kappa_mean","time_sec","memory_mb")
df[need_num] <- lapply(df[need_num], to_num)

model_map <- c(nb  = "Naive Bayes", rf  = "Random Forest", svm = "SVM (Linear)", lr  = "Logistic Regression", xgb = "XGBoost")

df_acc_long <- df %>%
  select(embedding, model, seed, acc_1, acc_2, acc_3, acc_4, acc_5) %>%
  pivot_longer(cols = acc_1:acc_5, names_to = "fold", values_to = "acc") %>%
  mutate(acc = as.numeric(gsub(",", ".", acc))) %>% filter(!is.na(acc))

acc_scale <- scale_y_continuous(
  limits = c(0.30, 0.70),
  breaks = seq(0.30, 0.70, by = 0.05),
  labels = function(x) sprintf("%.2f", x)
)


p_acc_raw <- ggplot(df_acc_long, aes(x = embedding, y = acc, fill = embedding)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_jitter(width = 0.15, size = 0.6, alpha = 0.4) +
  acc_scale +
  labs(x = "Embedding Method", y = "Accuracy") +
  scale_x_discrete(expand = expansion(add = 0.6)) + theme_minimal() +  
  theme(
    text = element_text(family = "Helvetica", color = "black"),
    axis.title.x = element_text(face = "bold", size = 14, color = "black"),
    axis.title.y = element_text(face = "bold", size = 14, color = "black"),
    axis.text.x  = element_text(face = "bold", size = 13, color = "black", angle = 0, hjust = 0.5),
    axis.text.y  = element_text(face = "bold", size = 13, color = "black"),
    legend.position = "none"
  )

ggsave(file.path(out_dir, "violin_embedding_accuracy.pdf"), p_acc_raw, width = 8, height = 5)


df_acc_long_ml <- df %>%
  select(model, seed, acc_1, acc_2, acc_3, acc_4, acc_5) %>%
  mutate(model = tolower(trimws(as.character(model))), model_name = dplyr::recode(model, !!!model_map, .default = model)) %>%
  
  pivot_longer(cols = acc_1:acc_5, names_to = "fold", values_to = "acc") %>%
  mutate(acc = as.numeric(gsub(",", ".", acc))) %>% filter(!is.na(acc))

p_acc_raw_ml <- ggplot(df_acc_long_ml, aes(x = model_name, y = acc, fill = model_name)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_jitter(width = 0.15, size = 0.6, alpha = 0.4) +
  acc_scale +
  labs(x = "Classifier", y = "Accuracy") +
  scale_x_discrete(expand = expansion(add = 0.6)) + theme_minimal() +  
  theme(
    text = element_text(family = "Helvetica", color = "black"),
    axis.title.x = element_text(face = "bold", size = 14, color = "black"),
    axis.title.y = element_text(face = "bold", size = 14, color = "black"),
    axis.text.x  = element_text(face = "bold", size = 12, color = "black", angle = 0, hjust = 0.5),
    axis.text.y  = element_text(face = "bold", size = 12, color = "black"),
    legend.position = "none"
  )
ggsave(file.path(out_dir, "violin_classifier_accuracy.pdf"), p_acc_raw_ml, width = 8, height = 5)

library(patchwork)
p_combined <- p_acc_raw_ml | p_acc_raw
ggsave(file.path(out_dir, "violin_accuracy_embedding_vs_classifier.pdf"), plot = p_combined, width = 16, height = 7)