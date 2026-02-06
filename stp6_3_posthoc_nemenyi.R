library(dplyr)
library(tidyr)
library(PMCMRplus)
library(ggplot2)

df <- read.csv2("_result/logfile.csv", sep = ";", stringsAsFactors = FALSE)

df$f1_mean <- as.numeric(df$f1_mean)

f1_cols <- grep("^f1_[1-5]$", names(df), value = TRUE)
df[f1_cols] <- lapply(df[f1_cols], as.numeric)

friedman_emb <- df %>% group_by(embedding, model) %>% summarise(f1 = mean(f1_mean, na.rm = TRUE), .groups = "drop")
friedman_mat2 <- friedman_emb %>% pivot_wider(names_from = model, values_from = f1) %>% arrange(embedding)

emb_names <- friedman_mat2$embedding
friedman_mat2 <- as.matrix(friedman_mat2[, -1])
rownames(friedman_mat2) <- emb_names

friedman_complete <- friedman_mat2[complete.cases(friedman_mat2), ]
friedman.test(friedman_complete)

sink("_stat/friedman_complete.txt")
print(friedman_complete)
sink()

#NEMENYI
nemenyi_res <- frdAllPairsNemenyiTest(friedman_complete)
nemenyi_res
write.csv(as.data.frame(nemenyi_res$p.value), "_stat/posthoc_nemenyi_complete_3emb_5model.csv", row.names = TRUE)

delta <- read.csv("_stat/wilcoxon_cliff_delta_table.csv")
delta$Cliff_Delta <- as.numeric(delta$Cliff_Delta)
dmat <- delta %>% group_by(Model1, Model2) %>%
  summarise(Cliff = mean(Cliff_Delta, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Model2, values_from = Cliff)
dmat <- dmat %>% rename(Model = Model1)
dlong <- dmat %>% pivot_longer(cols = -Model, names_to = "Opponent", values_to = "Cliff")
dlong$Cliff <- as.numeric(dlong$Cliff)

ggplot(dlong, aes(x = Model, y = Opponent)) +
  geom_segment(aes(x = 0, xend = Cliff, y = Opponent, yend = Opponent, color = Cliff), size = 3, alpha = 0.9, na.rm = TRUE) + 
  scale_color_gradient2(low = "#2166AC", high = "#B2182B", mid="gray90", midpoint = 0, name = "Cliff???s ??") +
  theme_minimal(base_size = 14) +
  labs(title = "Signed Difference Bar Matrix (Cliffs Delta)", x = "Difference (Model1 - Model2)", y = "Model Pair") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(filename = "_figure/cliff_signed_bar_matrix.pdf", plot = last_plot(), width = 8, height = 6)