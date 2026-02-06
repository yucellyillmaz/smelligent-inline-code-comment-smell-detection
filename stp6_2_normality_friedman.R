#Friedman test : f1 score
library(dplyr)
library(tidyr)
library(PMCMRplus)

df <- read.csv2("_result/logfile.csv", sep = ";", stringsAsFactors = FALSE)

f1_cols <- grep("^f1_[1-5]$", names(df), value = TRUE)
df[f1_cols] <- lapply(df[f1_cols], as.numeric)

friedman_data <- df %>% select(embedding, model, all_of(f1_cols))

friedman_matrix <- friedman_data %>% pivot_longer(cols = f1_cols, names_to = "fold", values_to = "f1") %>%
  group_by(embedding, fold, model) %>% summarise(f1 = mean(f1, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = model, values_from = f1)

friedman_mat <- as.matrix(friedman_matrix %>% select(-embedding, -fold))
friedman_result <- friedman.test(friedman_mat)

if (!dir.exists("_stat")) dir.create("_stat")
capture.output(friedman_result, file = "_stat/friedman.txt")