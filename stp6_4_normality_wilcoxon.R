# WILCOXON TEST
library(dplyr)
library(tidyr)
library(purrr)

df <- read.csv2("_result/logfile.csv", sep=";", stringsAsFactors = FALSE)

fold_cols <- grep("^f1_[1-5]$", names(df), value = TRUE)
df[fold_cols] <- lapply(df[fold_cols], function(x) as.numeric(as.character(x)))

if (!dir.exists("_stat")) dir.create("_stat")

embeddings <- unique(df$embedding)
wilcoxon_all <- data.frame()

for (emb in embeddings) {
  sub <- df %>% filter(embedding == emb) %>% mutate(f1_mean = as.numeric(f1_mean)) %>%
    group_by(model) %>% summarise(f1_mean_avg = mean(f1_mean, na.rm = TRUE)) %>% arrange(desc(f1_mean_avg))
  if (nrow(sub) < 2) next
  m1 <- sub$model[1]
  m2 <- sub$model[2]
  
  folds_m1 <- df %>%
    filter(embedding == emb, model == m1) %>% select(all_of(fold_cols)) %>%
    pivot_longer(cols = everything(), names_to = "fold", values_to = "value") %>% pull(value)
  
  folds_m2 <- df %>%
    filter(embedding == emb, model == m2) %>% select(all_of(fold_cols)) %>%
    pivot_longer(cols = everything(), names_to = "fold", values_to = "value") %>% pull(value)
  
  folds_m1 <- as.numeric(folds_m1)
  folds_m2 <- as.numeric(folds_m2)
  
  valid <- sum(!is.na(folds_m1) & !is.na(folds_m2))
  
  p_value <- NA
  method <- ""
  effect_r <- NA
  note <- ""
  
  if (valid >= 3 && length(unique(folds_m1 - folds_m2)) != 1) {
    test <- wilcox.test(folds_m1, folds_m2, paired = TRUE, exact = FALSE)
    p_value <- test$p.value
    method  <- test$method
    
    W <- test$statistic
    N <- valid
    
    mean_W <- N*(N+1)/4
    sd_W   <- sqrt(N*(N+1)*(2*N+1)/24)
    
    Z <- (W - mean_W) / sd_W
    effect_r <- abs(Z) / sqrt(N)  
    
  } else { note <- "Insufficient or constant difference"}
  row <- data.frame( embedding = emb, best_model_1 = m1, best_model_2 = m2, valid_folds = valid, p_value = p_value,
                     effect_size_r = effect_r, method = method, note = note, stringsAsFactors = FALSE)
  wilcoxon_all <- bind_rows(wilcoxon_all, row)
}
write.csv(wilcoxon_all, "_stat/wilcoxon.csv", row.names = FALSE)
cat("Wilcoxon + Effect Size analyze is OK.\n")


#WILCOXON - PAIRWISE
library(dplyr)
library(tidyr)
library(purrr)
library(combinat)

df <- read.csv2("_result/logfile.csv", sep=";", stringsAsFactors = FALSE)

fold_cols <- grep("^f1_[1-5]$", names(df), value = TRUE)
df[fold_cols] <- lapply(df[fold_cols], function(x) as.numeric(as.character(x)))

models <- unique(df$model)
pair_list <- combn(models, 2, simplify = FALSE)
embeddings <- unique(df$embedding)
pairwise_log <- data.frame()

if (!dir.exists("_stat")) dir.create("_stat")
for (emb in embeddings) {
  cat("Processing embedding:", emb, "\n")
  for (pair in pair_list) {
    m1 <- pair[1]
    m2 <- pair[2]
    
    folds_m1 <- df %>%
      filter(embedding == emb, model == m1) %>% select(all_of(fold_cols)) %>%
      pivot_longer(cols = everything(), names_to = "fold", values_to = "value") %>% pull(value)
    
    folds_m2 <- df %>%
      filter(embedding == emb, model == m2) %>% select(all_of(fold_cols)) %>%
      pivot_longer(cols = everything(), names_to = "fold", values_to = "value") %>% pull(value)
    
    folds_m1 <- as.numeric(folds_m1)
    folds_m2 <- as.numeric(folds_m2)
    
    valid <- sum(!is.na(folds_m1) & !is.na(folds_m2))
    p_value <- NA
    effect_r <- NA
    method <- ""
    note <- ""
    
    if (valid >= 3 && length(unique(folds_m1 - folds_m2)) != 1) {
      wil <- wilcox.test(folds_m1, folds_m2, paired = TRUE, exact = FALSE)
      p_value <- wil$p.value
      method  <- wil$method
      
      W <- wil$statistic
      N <- valid
      mean_W <- N*(N+1)/4
      sd_W   <- sqrt(N*(N+1)*(2*N+1)/24)
      
      Z <- (W - mean_W) / sd_W
      effect_r <- abs(Z) / sqrt(N)
      
    } else {
      note <- "Insufficient or constant differences"
      method <- "Undefined"
    }
    row <- data.frame(embedding = emb, model1 = m1, model2 = m2, valid_folds = valid, p_value = p_value,
                      effect_size_r = effect_r, method = method, note = note, stringsAsFactors = FALSE)
    pairwise_log <- bind_rows(pairwise_log, row)
  }
}
write.csv(pairwise_log, "_stat/wilcoxon_pairwise.csv", row.names = FALSE)


#Wilcoxon - summary table
library(dplyr)
pw <- read.csv("_stat/wilcoxon_pairwise.csv")

pairwise_summary <- pw %>%
  mutate(outcome = case_when(is.na(p_value) ~ "Undefined", p_value < 0.05 ~ "Significant", TRUE ~ "Not Significant")) %>%
  group_by(model1, model2) %>%
  summarise(Significant = sum(outcome == "Significant"), Not_Significant = sum(outcome == "Not Significant"),
            Undefined = sum(outcome == "Undefined"), .groups = "drop") %>%
  mutate(pair = paste(model1, "--", model2)) %>%
  select(pair, Significant, Not_Significant, Undefined)
write.csv(pairwise_summary, "_stat/wilcoxon_pairwise_summary.csv", row.names = FALSE)


#LaTeX Tablosu ??reten
library(dplyr)
library(tidyr)
library(xtable)

summary_raw <- read.csv("_stat/wilcoxon_pairwise_summary.csv", stringsAsFactors = FALSE)

names(summary_raw) <- gsub("[ .]", "_", names(summary_raw))
model_map <- c("lr" = "LR", "nb" = "NB", "rf" = "RF", "svm" = "SVM", "xgb" = "XGB")
summary_df <- summary_raw %>%
  separate(pair, into = c("model1", "model2"), sep = " -- ") %>%
  mutate(model1_clean = model_map[model1], model2_clean = model_map[model2],
         Model_Pair = paste(model1_clean, "--", model2_clean),
         Total = Significant + Not_Significant + Undefined) %>%
  select(Model_Pair, Significant, Not_Significant, Undefined, Total)

latex_table <- xtable(summary_df, caption = "Summary of pairwise Wilcoxon signed-rank test results across all embedding methods.",
                      label = "tab:pairwise_summary", align = c("l","l","c","c","c","c"))

print(latex_table, include.rownames = FALSE, sanitize.text.function = identity, booktabs = TRUE)


library(dplyr)
library(tidyr)

df <- read.csv2("_result/logfile.csv", sep = ";", stringsAsFactors = FALSE)

f1_cols <- grep("^f1_[1-5]$", names(df), value = TRUE)
df[f1_cols] <- lapply(df[f1_cols], as.numeric)
models <- unique(df$model)
embeds <- unique(df$embedding)
safe_cliff_delta <- function(x, y) {
  x <- x[!is.na(x)]
  y <- y[!is.na(y)]
  if (length(x) == 0 | length(y) == 0) {return(list(delta = NA, magnitude = "undefined"))}
  if (length(unique(x)) == 1 & length(unique(y)) == 1) {return(list(delta = 0, magnitude = "negligible"))}
  pairs <- expand.grid(x = x, y = y)
  delta <- (sum(pairs$x > pairs$y) - sum(pairs$x < pairs$y)) / nrow(pairs)
  mag <- dplyr::case_when(abs(delta) < 0.147 ~ "negligible", abs(delta) < 0.33  ~ "small", abs(delta) < 0.474 ~ "medium", TRUE ~ "large")
  return(list(delta = delta, magnitude = mag))
}
direction_results <- data.frame()
effect_results <- data.frame()
for (emb in embeds) {
  df_emb <- df %>% filter(embedding == emb)
  for (i in 1:(length(models)-1)) {
    for (j in (i+1):length(models)) {
      m1 <- models[i]
      m2 <- models[j]
      d1 <- df_emb %>% filter(model == m1) %>% select(all_of(f1_cols)) %>% unlist()
      d2 <- df_emb %>% filter(model == m2) %>% select(all_of(f1_cols)) %>% unlist()
      wins_m1 <- sum(d1 > d2, na.rm = TRUE)
      wins_m2 <- sum(d2 > d1, na.rm = TRUE)
      direction <- ifelse(wins_m1 > wins_m2, paste(m1, ">", m2), ifelse(wins_m2 > wins_m1, paste(m2, ">", m1), "Tie"))
      cd <- safe_cliff_delta(d1, d2)
      direction_results <- rbind(direction_results, 
                                 data.frame(Embedding = emb, Model1 = m1, Model2 = m2, Wins_Model1 = wins_m1, Wins_Model2 = wins_m2, Direction = direction))
      effect_results <- rbind(effect_results,
                              data.frame(Embedding = emb, Model1 = m1, Model2 = m2, Cliff_Delta = cd$delta, Effect_Size = cd$magnitude))
    }
  }
}
write.csv(direction_results, "_stat/wilcoxon_direction_table.csv", row.names = FALSE)
write.csv(effect_results, "_stat/wilcoxon_cliff_delta_table.csv", row.names = FALSE)
