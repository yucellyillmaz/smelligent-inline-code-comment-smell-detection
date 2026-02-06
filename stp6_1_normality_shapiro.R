#SHAPIRO NORMALITY TEST
library(dplyr)
library(tidyr)
library(ggplot2)
library(PMCMRplus)
library(purrr)

df <- read.csv2("_result/logfile.csv", sep = ";", stringsAsFactors = FALSE)

metrics <- c("acc", "kappa", "f1", "mcc")
result_list <- list()
for (metric in metrics) {
  pattern <- paste0("^", metric, "_[1-5]$")
  tmp <- df %>%
    pivot_longer(cols = matches(pattern), names_to = "fold", values_to = "value") %>%
    mutate(value = as.numeric(value)) %>%
    group_by(embedding, model) %>%
    summarise(
      clean_values = list(na.omit(value)),
      valid = length(clean_values[[1]]),
      all_equal = ifelse(valid > 0, length(unique(clean_values[[1]])) == 1, FALSE),
      p_value = if (valid < 3) {  NA_real_ } else if (all_equal) { NA_real_ } else {shapiro.test(clean_values[[1]])$p.value},
      normal = if (valid < 3 || all_equal) {"Undefined"} else if (p_value > 0.05) {"Normal"} else {"Non-normal"}, .groups = "drop")
  tmp$clean_values <- NULL
  colnames(tmp)[colnames(tmp) == "p_value"] <- paste0(metric, "_p")
  colnames(tmp)[colnames(tmp) == "normal"]  <- paste0(metric, "_normal")
  result_list[[metric]] <- tmp
}
shapiro_results <- reduce(result_list, full_join, by = c("embedding", "model"))
if (!dir.exists("_stat")) { dir.create("_stat") }
write.csv(shapiro_results, "_stat/shapiro_wilk.csv", row.names = FALSE)