library(fmsb)
library(dplyr)
library(scales)

infile <- "_result/logfile.csv"  
df <- read.csv2(infile, sep = ";", stringsAsFactors = FALSE)
to_num <- function(x) as.numeric(gsub(",", ".", trimws(as.character(x))))

need_num <- c("acc_mean","f1_mean","mcc_mean","kappa_mean","time_sec","memory_mb")
df[need_num] <- lapply(df[need_num], to_num)

df2 <- df %>%
  mutate(
    model_raw = tolower(trimws(as.character(model))),
    embedding = as.character(embedding),
    model_name = case_when(
      grepl("^nb$|naive\\s*bayes|\\bnb\\b|bayes", model_raw) ~ "Naive Bayes",
      grepl("^rf$|random\\s*forest|randomforest|\\brf\\b", model_raw) ~ "Random Forest",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(model_name))

selected_models <- c("Naive Bayes", "Random Forest")

if (nrow(df2) == 0) {
  stop("CSV'de NB/RF tespit edilemedi.")
}
radar_metrics <- c("Accuracy","MacroF1","MCC","Kappa","Runtime_inv","Memory_inv")

df_radar <- df2 %>%
  group_by(embedding, model_name) %>%
  summarise(
    Accuracy = mean(acc_mean,   na.rm = TRUE),
    MacroF1  = mean(f1_mean,    na.rm = TRUE),
    MCC      = mean(mcc_mean,   na.rm = TRUE),
    Kappa    = mean(kappa_mean, na.rm = TRUE),
    time     = mean(time_sec,   na.rm = TRUE),
    memory   = mean(memory_mb,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(embedding) %>%
  mutate(
    Runtime_inv = max(time,   na.rm = TRUE) - time,
    Memory_inv  = max(memory, na.rm = TRUE) - memory
  ) %>%
  ungroup() %>%
  select(embedding, model_name, all_of(radar_metrics))

norm01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (!is.finite(rng[1]) || !is.finite(rng[2]) || rng[1] == rng[2]) return(rep(0.5, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}

df_radar_norm <- df_radar %>% mutate(across(all_of(radar_metrics), norm01))

embs <- unique(df_radar_norm$embedding)
embs6 <- embs
if (length(embs6) < 6) embs6 <- c(embs6, rep("", 6 - length(embs6)))
if (length(embs6) > 6) embs6 <- embs6[1:6]

out_dir <- "_figure"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
outfile <- file.path(out_dir, "radar_allmetrics_nb_vs_rf.pdf")

pdf(outfile, width = 14, height = 8, family = "Helvetica", useDingbats = FALSE)
on.exit(dev.off(), add = TRUE)

par(mfrow = c(2, 3), mar   = c(1.0, 1.0, 4.0, 1.0), cex   = 1.15, font  = 2)

plot_one <- function(emb_name) {
  if (emb_name == "") { plot.new(); return(invisible(NULL)) }
  
  df_plot <- df_radar_norm %>%
    filter(embedding == emb_name) %>%
    mutate(model_name = factor(model_name, levels = selected_models)) %>%
    arrange(model_name) %>%
    select(-embedding, -model_name) %>%
    as.data.frame()

  if (nrow(df_plot) != 2) {
    plot.new()
    title(main = emb_name, font.main = 2, cex.main = 1.2)
    text(0.5, 0.5, "There is no NB/RF data for this embedding.", cex = 1.1)
    return(invisible(NULL))
  }
  
  rownames(df_plot) <- selected_models
  df_plot <- rbind(setNames(rep(1, length(radar_metrics)), radar_metrics),
                   setNames(rep(0, length(radar_metrics)), radar_metrics), 
                   df_plot)
  radarchart(df_plot, axistype = 0, pcol  = c("#E41A1C", "#377EB8"),
    pfcol = alpha(c("#E41A1C", "#377EB8"), 0.25), plwd  = 3, plty  = c(1, 1), cglcol = "grey75",
    cglwd  = 1.2, axislabcol = NA, vlabels = radar_metrics, vlcex = 1.10)
  mtext(emb_name, side = 3, line = 0.6, cex = 1.35, font = 2)
}

for (e in embs6) plot_one(e)
par(fig = c(0, 1, 0, 1), new = TRUE, mar = c(0, 0, 0, 0))
plot.new()
legend("topleft", legend = selected_models, col = c("#E41A1C", "#377EB8"),lwd = 3, bty = "n", cex = 1.05, text.font = 2, horiz = TRUE)
dev.off()
cat("Saved:", outfile, "\n")