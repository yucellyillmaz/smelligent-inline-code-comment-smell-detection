library(dplyr)
library(tidyr)
library(ggplot2)
library(ggbeeswarm)

infile <- "_result/logfile.csv"
df <- read.csv2(infile, sep = ";", stringsAsFactors = FALSE)
to_num <- function(x) as.numeric(gsub(",", ".", trimws(as.character(x))))

need_num <- c("acc_mean","f1_mean","mcc_mean","kappa_mean","time_sec","memory_mb")
df[need_num] <- lapply(df[need_num], to_num)

df_f1_long_emb <- df %>% select(embedding, model, seed, f1_1, f1_2, f1_3, f1_4, f1_5) %>%
  pivot_longer(cols = f1_1:f1_5, names_to = "fold", values_to = "f1") %>%
  mutate(f1 = as.numeric(gsub(",", ".", f1))) %>% filter(!is.na(f1))

p_f1_bee_emb <- ggplot(df_f1_long_emb, aes(x = embedding, y = f1, color = embedding)) +
  geom_beeswarm(priority = "density", cex = 0.6) + 
  labs(x = "Embedding Method", y = "Macro F1-score") + theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 30, hjust = 1)) + 
  theme(
    text = element_text(family = "Helvetica", color = "black"),
    axis.title.x = element_text(face = "bold", size = 14, color = "black"),
    axis.title.y = element_text(face = "bold", size = 14, color = "black"),
    axis.text.x  = element_text(face = "bold", size = 12, color = "black", angle = 0, hjust = 0.5),
    axis.text.y  = element_text(face = "bold", size = 13, color = "black"),
    legend.position = "none"
  )

ggsave(file.path(out_dir, "beeswarm_f1score_embedding.pdf"), p_f1_bee_emb, width = 8, height = 5)

df_f1_long_ml <- df %>% select(model, seed, f1_1, f1_2, f1_3, f1_4, f1_5) %>% mutate(model_name = model_map[model]) %>%
  pivot_longer(cols = f1_1:f1_5, names_to = "fold", values_to = "f1") %>%
  mutate(f1 = as.numeric(gsub(",", ".", f1))) %>% filter(!is.na(f1))

p_f1_bee_ml <- ggplot(df_f1_long_ml, aes(x = model_name, y = f1, color = model_name)) +
  geom_beeswarm(priority = "density", cex = 0.6) +
  labs(x = "Classifier", y = "Macro F1-score") + theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 30, hjust = 1)) + 
  theme(
    text = element_text(family = "Helvetica", color = "black"),
    axis.title.x = element_text(face = "bold", size = 14, color = "black"),
    axis.title.y = element_text(face = "bold", size = 14, color = "black"),
    axis.text.x  = element_text(face = "bold", size = 12, color = "black", angle = 0, hjust = 0.5),
    axis.text.y  = element_text(face = "bold", size = 13, color = "black"),
    legend.position = "none"
  )

ggsave(file.path(out_dir, "beeswarm_f1score_classifier.pdf"), p_f1_bee_ml, width = 8, height = 5)

library(patchwork)
p_combined <- p_f1_bee_ml | p_f1_bee_emb
ggsave(file.path(out_dir, "beeswarm_f1score_embedding_vs_classifier.pdf"), plot = p_combined, width = 16, height = 7)