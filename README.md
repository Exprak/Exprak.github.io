# Exprak.github.io

---
title: "Pavlovia Datenanalyse"
author: "Exprak"
date: "2024-01-10"
output: html_document
---

# --------------------
# --- Preparations ---
# --------------------

# set working directory:
```{r, eval = TRUE}
setwd("/Documents/ExPrak/Auswertung")

```

# attach required packages:
```{r}
library('dplyr')
library('psych')
library('openxlsx')
library('readxl')
library('ez')
library('reshape2')

```

# ----------------
# --- HPS Data ---
# ----------------

# load HPS data and create a scale with HPS_Score:
```{r}
# load HPS data
HPS <- openxlsx::read.xlsx("Daten_HPS.xlsx")

# remove missings
has_missing <- any(is.na(HPS)) # no missings?

# 1. recode
HPS[HPS == "stimmt"] <- 1
HPS[HPS == "stimmt nicht"] <- 0

# 2.  
HPS_IDs <- HPS[,6]
names(HPS)[6] <- "ID"
HPS_Score <- HPS[,8:55]
colnames(HPS_Score) <- c(1:48)

# 3.
negative_coding <- c(1, 2, 6, 14, 16, 17, 21, 24,25, 27, 31, 47, 48)
HPS_Score[negative_coding][HPS_Score[negative_coding] == 1] <- "eins"
HPS_Score[negative_coding][HPS_Score[negative_coding] == 0] <- 1
HPS_Score[negative_coding][HPS_Score[negative_coding] == "eins"] <- 0

# 4.
HPS_Score <- as.data.frame(sapply(HPS_Score, as.numeric))

HPS_Score <- rowSums(HPS_Score)

HPS_Score <- data.frame(
  ID = HPS_IDs,
  Score = HPS_Score
)
rownames(HPS_Score)<- NULL

HPS_Score$ID <- toupper(HPS_Score$ID)

```

# -----------------------------
# --- Emotional Stroop Data ---
# -----------------------------

# load data emotional stroop:
```{r}
n1 <- read.csv("AJ69SK.csv", header = T, na.strings = c("NA", ""))
n2 <- read.csv("Al75WI.csv", header = T, na.strings = c("NA", ""))
n3 <- read.csv("AN63TN.csv", header = T, na.strings = c("NA", ""))
n4 <- read.csv("BI67NM.csv", header = T, na.strings = c("NA", ""))
n5 <- read.csv("CA97LG.csv", header = T, na.strings = c("NA", ""))
n6 <- read.csv("DO69HG.csv", header = T, na.strings = c("NA", ""))
n7 <- read.csv("EL01LE.csv", header = T, na.strings = c("NA", ""))
n8 <- read.csv("HE21GE.csv", header = T, na.strings = c("NA", ""))
n9 <- read.csv("IN21LE.csv", header = T, na.strings = c("NA", ""))
n10 <- read.csv("JE38BN.csv", header = T, na.strings = c("NA", ""))
n11 <- read.csv("KA22MG.csv", header = T, na.strings = c("NA", ""))
n12 <- read.csv("LO14ON.csv", header = T, na.strings = c("NA", ""))
n13 <- read.csv("MA96LG.csv", header = T, na.strings = c("NA", ""))
n14 <- read.csv("ME19ER.csv", header = T, na.strings = c("NA", ""))
n15 <- read.csv("MI14BN.csv", header = T, na.strings = c("NA", ""))
n16 <- read.csv("MI68LF.csv", header = T, na.strings = c("NA", ""))
n17 <- read.csv("NI96LE.csv", header = T, na.strings = c("NA", ""))
n18 <- read.csv("SA24LE.csv", header = T, na.strings = c("NA", ""))
n19 <- read.csv("SA43BN.csv", header = T, na.strings = c("NA", ""))
n20 <- read.csv("SE07MN.csv", header = T, na.strings = c("NA", ""))
n21 <- read.csv("SI02MU.csv", header = T, na.strings = c("NA", ""))
n22 <- read.csv("SU07BH.csv", header = T, na.strings = c("NA", ""))
n23 <- read.csv("SU22NN.csv", header = T, na.strings = c("NA", ""))
n24 <- read.csv("TA83SF.csv", header = T, na.strings = c("NA", ""))
n25 <- read.csv("TI54BN.csv", header = T, na.strings = c("NA", ""))
n26 <- read.csv("TO12LG.csv", header = T, na.strings = c("NA", ""))
n27 <- read.csv("UR92MT.csv", header = T, na.strings = c("NA", ""))
n28 <- read.csv("VI59MN.csv", header = T, na.strings = c("NA", ""))

# get list of all files
files <- dir(pattern = ".csv")

```

# prepare result data.frame:
```{r}
results <- data.frame(matrix(nrow = length(files), ncol = 8))
names(results) <- c("ID", "file_name", "trials_total", "trials_correct", "num_outliers", "mean_euphoria", "mean_neutral", "mean_depression")

```

# for-loop:
```{r}
for (i in 1:length(files)) {
  
  # read data
  message(sprintf('[%d/%d] Reading %s',i,length(files),files[i]))
  skip_to_next = FALSE
  tryCatch(
    {tmp <- read.delim(files[i], header = T, sep = ',') }, 
      error=function(e) { message(sprintf(' - %s',e)); skip_to_next <<- TRUE }
  )
  if (skip_to_next) { next }
    
  # add ID 
  ID <- tmp$textbox.text[2]
  
  # remove all columns and rows we don't need
  # left over colums: "congruent", "Trials_key.rt" and "category"
  tmp <- tmp[c(-(1:15)), c( -(1:46), -(48:54), -(57:62), -(64:68))]
  tmp <- na.omit(tmp)
  rownames(tmp) <- NULL
  
  # number of trials
  total <- nrow(tmp)
  
  # new scale with correct answers only
  tmp <- tmp[(tmp$conAns == "B" & tmp$Trials_key.keys == "b") | 
             (tmp$conAns == "G" & tmp$Trials_key.keys == "g") | 
             (tmp$conAns == "R" & tmp$Trials_key.keys == "r"), ]
  
  # number of correct trials
  correct <- nrow(tmp)
  
  # remove outliers
  mean_total <- mean(tmp$Trials_key.rt)
  sd_total <- sd(tmp$Trials_key.rt)
  outliers <- (tmp$Trials_key.rt > (mean_total + 2.58*sd_total)) | (tmp$Trials_key.rt < (mean_total - 2.58*sd_total))
  num_outliers <- sum(outliers)
  tmp <- tmp[!outliers, ]
  
  
  # calculate the mean
  mean_euphoria <- mean(subset(tmp, category == "euphoria")$Trials_key.rt, na.rm =  TRUE)
  
  mean_neutral <- mean(subset(tmp, category == "neutral")$Trials_key.rt, na.rm = TRUE)
  
  mean_depression <- mean(subset(tmp, category == "depression")$Trials_key.rt, na.rm = TRUE)

  # save results in data.frame 
  results[i,] <- c(ID, files[i], total, correct, num_outliers, mean_euphoria, mean_neutral, mean_depression)
  
  # delete variable for next iteration
  rm(tmp, ID, total, correct, num_outliers, mean_euphoria, mean_neutral, mean_depression)
  }

```

# correcting ID:
```{r}
idx <- is.na(results$ID) | (nchar(results$ID) < 6)
results$ID[idx] <- gsub(pattern = ".csv", replacement = "", results$file_name[idx])
results$ID <- gsub(pattern = " ", replacement = "", results$ID)
results$ID <- gsub(pattern = "\n", replacement = "", results$ID)
results$ID <- substr(results$ID, 1, 6)
results$ID <- toupper(results$ID)
results$ID[results$ID == "VI59MN"] <- "VI58MN"

```

# combine HPS and Emotional Stroop:
```{r}
names(HPS_Score)[1] <- "ID"
df <- dplyr::full_join(results, HPS_Score, by = "ID")
df[,c("ID", "file_name", "Score")]

```

# ----------------------------
# --- Statistical Analysis ---
# ----------------------------

# add group-variable:
```{r}
ngroups <- 2
cutoff <- quantile(df$Score, c(0:ngroups/ngroups))
df$group <- with(df, cut(Score, cutoff, include.lowest = T, labels =
                           c('low', 'high')))
table(df$group)

```

# change variable to numeric:
```{r}
df$mean_depression <- as.numeric(df$mean_depression)
df$mean_euphoria <- as.numeric(df$mean_euphoria)
df$mean_neutral <- as.numeric(df$mean_neutral)
df$ID <- as.factor(df$ID)

```

# run ANOVA for levels of within-subjects factor (repeated measures factor):
```{r}
uniANOVA_euphoria <- ezANOVA(df, dv = mean_euphoria, wid = ID, between = group, type = 3)
uniANOVA_neutral <- ezANOVA(df, dv = mean_neutral, wid = ID, between = group, type = 3)
uniANOVA_depression <- ezANOVA(df, dv = mean_depression, wid = ID, between = group, type = 3)

# results: not significant
# null hypothesis is retained: no significant differences between people with high hypomania and people with low hypomania for reaction times on items associated with euphoria, depression or neutral items.

```

# run 2x3 repeated measures ANOVA:
```{r}
df.anova <- df[, c('ID', 'mean_euphoria', 'mean_neutral', 'mean_depression')]
df.anova <- melt(df.anova, id="ID", variable.name="condition", value.name="RT", na.rm = F)
df.anova <- dplyr::left_join(df.anova, df[, c('ID', 'group')], by = 'ID')

twoANOVA <- ezANOVA(df.anova, dv = RT, wid = ID, within = condition, between = group, type = 3)

# results: not significant
# null hypothesis is retained: no signficiant effects between the different groups.

```

# ---------------------
# --- Visualisation ---
# ---------------------

# plot results with boxplot:
```{r}
library(ggplot2)

# 1. plot for euphoria related items
plot_euphoria <- ggplot(df, aes(x = group, y = mean_euphoria, fill = group)) +
  ggtitle("Euphoria") +
  geom_boxplot(alpha = 1, outlier.shape = NA, lwd = 0.2) +
  stat_boxplot(geom = "errorbar", width = 0.5, lwd = 0.2) +
  geom_point(alpha = 0.30, size = 2.0, shape = 16, position = position_jitterdodge(), show.legend = FALSE) +
  scale_x_discrete(name = "HPS score") +
  scale_y_continuous(limits = c(0.4, 1.2), name = "reaction time (s)") +
  guides(fill = guide_legend("HPS")) +
  theme_bw() +
  theme(plot.margin = margin(5.5, 5.5, -1, 5.5),
        plot.title = element_text(hjust = 0.5, size = 10, face = 'bold'),
        legend.title = element_text (size = 9),
        axis.title = element_text(size = 8),
        axis.title.x = element_text(size = 9, margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 9, margin = margin(t = 0, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(size = 8, angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(size = 0.25),
        line = element_line(size = 0.25))

# 2. plot for depression related items
plot_depression <-ggplot(df, aes(x = group, y = mean_depression, fill = group)) +
  ggtitle("Depression") +
  geom_boxplot(alpha = 1, outlier.shape = NA, lwd = 0.2) +
  stat_boxplot(geom = "errorbar", width = 0.5, lwd = 0.2) +
  geom_point(alpha = 0.30, size = 2.0, shape = 16, position = position_jitterdodge(), show.legend = FALSE) +
  scale_x_discrete(name = "HPS score") +
  scale_y_continuous(limits = c(0.4, 1.2), name = "reaction time (s)") +
  guides(fill = guide_legend("HPS")) +
  theme_bw() +
  theme(plot.margin = margin(5.5, 5.5, -1, 5.5),
        plot.title = element_text(hjust = 0.5, size = 10, face = 'bold'),
        legend.title = element_text (size = 9),
        axis.title = element_text(size = 8),
        axis.title.x = element_text(size = 9, margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 9, margin = margin(t = 0, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(size = 8, angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(size = 0.25),
        line = element_line(size = 0.25))

# 3. plot for neutral items
plot_neutral <-ggplot(df, aes(x = group, y = mean_neutral, fill = group)) +
  ggtitle("Neutral") +
  geom_boxplot(alpha = 1, outlier.shape = NA, lwd = 0.2) +
  stat_boxplot(geom = "errorbar", width = 0.5, lwd = 0.2) +
  geom_point(alpha = 0.30, size = 2.0, shape = 16, position = position_jitterdodge(), show.legend = FALSE) +
  scale_x_discrete(name = "HPS score") +
  scale_y_continuous(limits = c(0.4, 1.2), name = "reaction time (s)") +
  guides(fill = guide_legend("HPS")) +
  theme_bw() +
  theme(plot.margin = margin(5.5, 5.5, -1, 5.5),
        plot.title = element_text(hjust = 0.5, size = 10, face = 'bold'),
        legend.title = element_text (size = 9), 
        axis.title = element_text(size = 8),
        axis.title.x = element_text(size = 9, margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 9, margin = margin(t = 0, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(size = 8, angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(size = 0.25),
        line = element_line(size = 0.25))

```

# merge boxplots:
```{r}
library(patchwork)
merged_plot <- plot_depression + plot_neutral + plot_euphoria + plot_layout(guides = "collect")

```

# save plot:
```{r}
ggsave('merged_plot.png', merged_plot, width = 5.4, height = 3.6, dpi = 300)

```

# plots for descriptive statistics
```{r}
# 2 mal 3 repeated measures ANOVA

# Beispiel-Daten erstellen
mean_niedrig_hypo_eupho <- mean(as.numeric(df$mean_euphoria[df$Score < 16.5]))
mean_hoch_hypo_eupho <- mean(as.numeric(df$mean_euphoria[df$Score > 16.5]))
mean_niedrig_hypo_neutral <- mean(as.numeric(df$mean_neutral[df$Score < 16.5]))
mean_hoch_hypo_neutral <- mean(as.numeric(df$mean_neutral[df$Score > 16.5]))
mean_niedrig_hypo_depress <- mean(as.numeric(df$mean_depression[df$Score < 16.5]))
mean_hoch_hypo_depress <- mean(as.numeric(df$mean_depression[df$Score > 16.5]))
a1_b1 <- mean_niedrig_hypo_eupho
a1_b2 <- mean_niedrig_hypo_neutral 
a1_b3 <- mean_niedrig_hypo_depress
a2_b1 <- mean_hoch_hypo_eupho
a2_b2 <- mean_hoch_hypo_neutral 
a2_b3 <- mean_hoch_hypo_depress

#Matrix erstellen (mit Gruppierungs und 3-stufigem Messwiederholungsfaktor?)
daten <- matrix( c(a1_b1 <- mean_niedrig_hypo_eupho,
a1_b2 <- mean_niedrig_hypo_neutral,
a1_b3 <- mean_niedrig_hypo_depress,
a2_b1 <- mean_hoch_hypo_eupho,
a2_b2 <- mean_hoch_hypo_neutral, 
a2_b3 <- mean_hoch_hypo_depress),
  nrow = 2,  # Anzahl der Zeilen
  ncol = 3,  # Anzahl der Spalten
  byrow = TRUE 
)

# Benennung der Zeilen und Spalten
rownames(daten) <- c("hoch_hypoman", "niedrig_hypoman")
colnames(daten) <- c("mean_euphoria", "mean_neutral", "mean_depression")
```

