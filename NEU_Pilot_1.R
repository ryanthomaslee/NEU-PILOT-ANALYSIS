install.packages("tidyverse")
install.packages("ggplot2")
install.packages("readr")
install.packages("dplyr")
install.packages("tidyr")

r
library(tidyverse)
library(ggplot2)
library(readr)
library(dplyr)

# ============================================================
# NEU Pilot 1 — Data Analysis Script
# JAI Behavioural | March 2026
# ============================================================

# Load libraries
library(tidyverse)
library(ggplot2)

# ============================================================
# 1. LOAD DATA
# ============================================================

df <- read.csv("NEU_Pilot1_Data", na.strings = "missing")
df <- NEU_Pilot1_Data
df <- read.csv("~/Desktop/JAI BEHAVIOURAL/NEU Pilot Tools/NEU_Pilot_1/Data/NEU_Pilot1_Data.csv", 
               na.strings = "missing")
getwd()
df <- read.csv("Data/NEU_Pilot1_Data.csv", na.strings = "missing")
df[df == "missing"] <- NA
str(df)
# Convert D14 and Change columns to numeric
df <- df %>%
  mutate(across(c(D14_Cog, D14_Emo, D14_Mot, D14_Rel,
                  Change_Cog, Change_Emo, Change_Mot, Change_Rel),
                as.numeric))
library(tidyverse)
str(df[, c("D14_Cog", "D14_Emo", "D14_Mot", "D14_Rel")])
df <- df %>%
  mutate(across(c(D14_Cog, D14_Emo, D14_Mot, D14_Rel,
                  Change_Cog, Change_Emo, Change_Mot, Change_Rel),
                as.numeric))
str(df[, c("D14_Cog", "D14_Emo", "D14_Mot", "D14_Rel")])
df <- df %>%
  mutate(
    D0_Total = D0_Cog + D0_Emo + D0_Mot + D0_Rel,
    D14_Total = D14_Cog + D14_Emo + D14_Mot + D14_Rel,
    Total_Change = D14_Total - D0_Total,
    Pct_Reduction = round(((D0_Total - D14_Total) / D0_Total) * 100, 1)
  )

completers <- df %>%
  filter(!is.na(D0_Total) & !is.na(D14_Total))

cat("Total participants:", nrow(df), "\n")
cat("Completers:", nrow(completers), "\n")
completers <- completers %>%
  mutate(
    Outlier = case_when(
      Participant_Name == "Louis Carmichael" ~ "Low reliability",
      Participant_Name == "Taylor Wakefield" ~ "Score increase",
      TRUE ~ "Standard"
    )
  )

core <- completers %>%
  filter(Outlier != "Low reliability")

cat("Core sample (Louis excluded):", nrow(core), "\n")
# ============================================================
# 3. DESCRIPTIVE STATISTICS
# ============================================================

# D0 domain means across completers
cat("\n--- D0 Domain Means (all completers) ---\n")
completers %>%
  summarise(
    Cog = round(mean(D0_Cog, na.rm=T), 2),
    Emo = round(mean(D0_Emo, na.rm=T), 2),
    Mot = round(mean(D0_Mot, na.rm=T), 2),
    Rel = round(mean(D0_Rel, na.rm=T), 2)
  ) %>% print()

# D14 domain means across completers
cat("\n--- D14 Domain Means (all completers) ---\n")
completers %>%
  summarise(
    Cog = round(mean(D14_Cog, na.rm=T), 2),
    Emo = round(mean(D14_Emo, na.rm=T), 2),
    Mot = round(mean(D14_Mot, na.rm=T), 2),
    Rel = round(mean(D14_Rel, na.rm=T), 2)
  ) %>% print()

# Overall reduction
cat("\n--- Overall Reduction (all completers) ---\n")
completers %>%
  summarise(
    Mean_D0 = round(mean(D0_Total), 2),
    Mean_D14 = round(mean(D14_Total), 2),
    Mean_Change = round(mean(Total_Change), 2),
    Mean_Pct_Reduction = round(mean(Pct_Reduction), 1),
    SD_Pct_Reduction = round(sd(Pct_Reduction), 1)
  ) %>% print()

# Core sample reduction
cat("\n--- Overall Reduction (core sample, Louis excluded) ---\n")
core %>%
  summarise(
    Mean_D0 = round(mean(D0_Total), 2),
    Mean_D14 = round(mean(D14_Total), 2),
    Mean_Change = round(mean(Total_Change), 2),
    Mean_Pct_Reduction = round(mean(Pct_Reduction), 1),
    SD_Pct_Reduction = round(sd(Pct_Reduction), 1)
  ) %>% print()
completers %>%
  select(Participant_Name, D0_Total, D14_Total, 
         Total_Change, Pct_Reduction, Outlier) %>%
  arrange(Pct_Reduction) %>%
  print()
completers %>%
  summarise(
    Cog = round(mean(D0_Cog, na.rm=T), 2),
    Emo = round(mean(D0_Emo, na.rm=T), 2),
    Mot = round(mean(D0_Mot, na.rm=T), 2),
    Rel = round(mean(D0_Rel, na.rm=T), 2)
  ) %>% print()
# ============================================================
# 4. DOMAIN LEVEL CHANGE ANALYSIS
# ============================================================

cat("\n--- Mean Change Per Domain (all completers) ---\n")
completers %>%
  summarise(
    Cog = round(mean(Change_Cog, na.rm=T), 2),
    Emo = round(mean(Change_Emo, na.rm=T), 2),
    Mot = round(mean(Change_Mot, na.rm=T), 2),
    Rel = round(mean(Change_Rel, na.rm=T), 2)
  ) %>% print()

cat("\n--- Mean Change Per Domain (core sample) ---\n")
core %>%
  summarise(
    Cog = round(mean(Change_Cog, na.rm=T), 2),
    Emo = round(mean(Change_Emo, na.rm=T), 2),
    Mot = round(mean(Change_Mot, na.rm=T), 2),
    Rel = round(mean(Change_Rel, na.rm=T), 2)
  ) %>% print()

cat("\n--- Primary Domain Distribution at D0 ---\n")
completers %>%
  mutate(Primary_Domain = case_when(
    D0_Emo >= D0_Cog & D0_Emo >= D0_Mot & D0_Emo >= D0_Rel ~ "Emotional",
    D0_Mot >= D0_Cog & D0_Mot >= D0_Emo & D0_Mot >= D0_Rel ~ "Motivation",
    D0_Rel >= D0_Cog & D0_Rel >= D0_Emo & D0_Rel >= D0_Mot ~ "Relational",
    TRUE ~ "Cognitive"
  )) %>%
  count(Primary_Domain) %>%
  print()
# ============================================================
# 5. VISUALISATIONS
# ============================================================

# Plot 1: Total score change per participant
completers_long <- completers %>%
  select(Participant_Name, D0_Total, D14_Total, Outlier) %>%
  pivot_longer(cols = c(D0_Total, D14_Total),
               names_to = "Timepoint",
               values_to = "Score") %>%
  mutate(Timepoint = recode(Timepoint,
                            "D0_Total" = "Day 0",
                            "D14_Total" = "Day 14"))

ggplot(completers_long, aes(x = Timepoint, y = Score,
                            group = Participant_Name,
                            colour = Outlier)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_colour_manual(values = c(
    "Standard" = "#2E75B6",
    "Score increase" = "#E67E22",
    "Low reliability" = "#999999"
  )) +
  labs(
    title = "TBP Total Score: Day 0 vs Day 14",
    subtitle = "NEU Pilot 1 — All Completers",
    x = "Timepoint",
    y = "TBP Total Score (/36)",
    colour = "Data Quality"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))
ggplot(completers_long, aes(x = reorder(Participant_Name, Score), 
                             y = Score,
                             fill = Timepoint)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Day 0" = "#1B3A6B", 
                                "Day 14" = "#2E75B6")) +
  coord_flip() +
  labs(
    title = "TBP Total Score: Day 0 vs Day 14",
    subtitle = "NEU Pilot 1 — All Completers",
    x = NULL,
    y = "TBP Total Score (/36)",
    fill = "Timepoint"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))


ggplot(completers_long, aes(x = reorder(Participant_Name, Score), 
                            y = Score,
                            fill = Timepoint)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Day 0" = "#1B3A6B", 
                               "Day 14" = "#2E75B6")) +
  coord_flip() +
  labs(
    title = "TBP Total Score: Day 0 vs Day 14",
    subtitle = "NEU Pilot 1 — All Completers",
    x = NULL,
    y = "TBP Total Score (/36)",
    fill = "Timepoint"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))


completers_long <- completers_long %>%
  mutate(Participant_ID = paste0("P", 
                                 match(Participant_Name, unique(Participant_Name))))

ggplot(completers_long, aes(x = reorder(Participant_ID, Score), 
                            y = Score,
                            fill = Timepoint)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Day 0" = "#1B3A6B", 
                               "Day 14" = "#2E75B6")) +
  coord_flip() +
  labs(
    title = "TBP Total Score: Day 0 vs Day 14",
    subtitle = "NEU Pilot 1 — All Completers",
    x = NULL,
    y = "TBP Total Score (/36)",
    fill = "Timepoint"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))
library(ggplot2)

completers_wide <- completers %>%
  mutate(Participant_ID = paste0("P", 
                                 match(Participant_Name, unique(Participant_Name))))

ggplot(completers_wide, aes(y = reorder(Participant_ID, -D0_Total))) +
  geom_segment(aes(x = D14_Total, xend = D0_Total,
                   yend = reorder(Participant_ID, -D0_Total)),
               colour = "#CCCCCC", size = 1.5) +
  geom_point(aes(x = D0_Total), colour = "#1B3A6B", size = 4) +
  geom_point(aes(x = D14_Total, shape = Outlier), 
             colour = "#2E75B6", size = 4) +
  scale_shape_manual(values = c("Standard" = 16,
                                "Score increase" = 17,
                                "Low reliability" = 4)) +
  labs(
    title = "TBP Total Score Change: Day 0 to Day 14",
    subtitle = "NEU Pilot 1 — All Completers | Dark = Day 0, Light = Day 14",
    x = "TBP Total Score (/36)",
    y = NULL,
    shape = "Data Quality"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, colour = "#555555"),
    panel.grid.major.y = element_blank()
  )
completers <- completers %>%
  mutate(Participant_ID = paste0("P", 
                                 match(Participant_Name, unique(Participant_Name))))

ggplot(completers, aes(y = reorder(Participant_ID, -D0_Total))) +
  geom_segment(aes(x = D14_Total, xend = D0_Total,
                   yend = reorder(Participant_ID, -D0_Total)),
               colour = "#CCCCCC", size = 1.5) +
  geom_point(aes(x = D0_Total), colour = "#1B3A6B", size = 4) +
  geom_point(aes(x = D14_Total, shape = Outlier), 
             colour = "#2E75B6", size = 4) +
  scale_shape_manual(values = c("Standard" = 16,
                                "Score increase" = 17,
                                "Low reliability" = 4)) +
  labs(
    title = "TBP Total Score Change: Day 0 to Day 14",
    subtitle = "NEU Pilot 1 — All Completers | Dark = Day 0, Light = Day 14",
    x = "TBP Total Score (/36)",
    y = NULL,
    shape = "Data Quality"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, colour = "#555555"),
    panel.grid.major.y = element_blank()
  )


completers$Participant_ID <- paste0("P", 
                                    match(completers$Participant_Name, unique(completers$Participant_Name)))

completers %>% select(Participant_Name, Participant_ID) %>% print()
ggplot(completers, aes(y = reorder(Participant_ID, -D0_Total))) +
  geom_segment(aes(x = D14_Total, xend = D0_Total,
                   yend = reorder(Participant_ID, -D0_Total)),
               colour = "#CCCCCC", size = 1.5) +
  geom_point(aes(x = D0_Total), colour = "#1B3A6B", size = 4) +
  geom_point(aes(x = D14_Total, shape = Outlier), 
             colour = "#2E75B6", size = 4) +
  scale_shape_manual(values = c("Standard" = 16,
                                "Score increase" = 17,
                                "Low reliability" = 4)) +
  labs(
    title = "TBP Total Score Change: Day 0 to Day 14",
    subtitle = "NEU Pilot 1 — All Completers | Dark = Day 0, Light = Day 14",
    x = "TBP Total Score (/36)",
    y = NULL,
    shape = "Data Quality"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, colour = "#555555"),
    panel.grid.major.y = element_blank()
  )

ggplot(completers, aes(y = reorder(Participant_ID, D0_Total))) +
  geom_segment(aes(x = D0_Total, xend = D14_Total,
                   yend = reorder(Participant_ID, D0_Total)),
               colour = "#CCCCCC", size = 1.5) +
  geom_point(aes(x = D0_Total), colour = "#1B3A6B", size = 4) +
  geom_point(aes(x = D14_Total), colour = "#E67E22", size = 4) +
  annotate("text", x = 3, y = 9.5, label = "● Day 0", 
           colour = "#1B3A6B", size = 3.5) +
  annotate("text", x = 6, y = 9.5, label = "● Day 14", 
           colour = "#E67E22", size = 3.5) +
  labs(
    title = "TBP Total Score Change: Day 0 to Day 14",
    subtitle = "NEU Pilot 1 — All Completers",
    x = "TBP Total Score (/36)",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, colour = "#555555"),
    panel.grid.major.y = element_blank()
  )
ggplot(completers, aes(y = reorder(Participant_ID, D0_Total))) +
  geom_segment(aes(x = D0_Total, xend = D14_Total,
                   yend = reorder(Participant_ID, D0_Total)),
               colour = "#CCCCCC", size = 1.5) +
  geom_point(aes(x = D0_Total), colour = "#1B3A6B", size = 4) +
  geom_point(aes(x = D14_Total), colour = "#E67E22", size = 4) +
  scale_x_continuous(limits = c(0, 36), breaks = seq(0, 36, by = 6)) +
  labs(
    title = "TBP Total Score Change: Day 0 to Day 14",
    subtitle = "NEU Pilot 1 — All Completers  |  ● Navy = Day 0     ● Orange = Day 14",
    x = "TBP Total Score (/36)",
    y = NULL,
    caption = "P3 = low reliability flag. P1 = score increase. P2 = low baseline outlier."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, colour = "#555555"),
    plot.caption = element_text(size = 8, colour = "#888888", hjust = 0),
    panel.grid.major.y = element_blank(),
    axis.line.x = element_line(colour = "black", size = 0.5),
    axis.line.y = element_line(colour = "black", size = 0.5),
    axis.ticks = element_line(colour = "black")
  )
ggplot(completers, aes(y = reorder(Participant_ID, D0_Total))) +
  geom_segment(aes(x = D0_Total, xend = D14_Total,
                   yend = reorder(Participant_ID, D0_Total)),
               colour = "#CCCCCC", size = 1.5) +
  geom_point(aes(x = D0_Total), colour = "#1B3A6B", size = 4) +
  geom_point(aes(x = D14_Total), colour = "#E67E22", size = 4) +
  scale_x_continuous(limits = c(0, 28), breaks = seq(0, 28, by = 6)) +
  labs(
    title = "TBP Total Score Change: Day 0 to Day 14",
    subtitle = "NEU Pilot 1 — All Completers  |  ● Navy = Day 0     ● Orange = Day 14",
    x = "TBP Total Score (/36)",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, colour = "#555555"),
    panel.grid.major.y = element_blank(),
    axis.line.x = element_line(colour = "black", size = 0.5),
    axis.line.y = element_line(colour = "black", size = 0.5),
    axis.ticks = element_line(colour = "black")
  )
domain_means <- completers %>%
  summarise(
    Cog_D0 = mean(Cog_D0, na.rm = TRUE),
    Cog_D14 = mean(Cog_D14, na.rm = TRUE),
    Emo_D0 = mean(Emo_D0, na.rm = TRUE),
    Emo_D14 = mean(Emo_D14, na.rm = TRUE),
    Mot_D0 = mean(Mot_D0, na.rm = TRUE),
    Mot_D14 = mean(Mot_D14, na.rm = TRUE),
    Rel_D0 = mean(Rel_D0, na.rm = TRUE),
    Rel_D14 = mean(Rel_D14, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(),
               names_to = c("Domain", "Time"),
               names_sep = "_",
               values_to = "Mean_Score") %>%
  mutate(Time = factor(Time, levels = c("D0", "D14")))

print(domain_means)
# Fix subtitle encoding
subtitle_text <- "NEU Pilot 1 - All Completers  |  Navy = Day 0  |  Orange = Day 14"
library(tidyr)

domain_means <- completers %>%
  summarise(
    Cog_D0 = mean(Cog_D0, na.rm = TRUE),
    Cog_D14 = mean(Cog_D14, na.rm = TRUE),
    Emo_D0 = mean(Emo_D0, na.rm = TRUE),
    Emo_D14 = mean(Emo_D14, na.rm = TRUE),
    Mot_D0 = mean(Mot_D0, na.rm = TRUE),
    Mot_D14 = mean(Mot_D14, na.rm = TRUE),
    Rel_D0 = mean(Rel_D0, na.rm = TRUE),
    Rel_D14 = mean(Rel_D14, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(),
               names_to = c("Domain", "Time"),
               names_sep = "_",
               values_to = "Mean_Score") %>%
  mutate(Time = factor(Time, levels = c("D0", "D14")))

print(domain_means)


library(dplyr)
library(tidyr)
library(ggplot2)

df <- read.csv("~/Desktop/JAI BEHAVIOURAL/NEU Pilot Tools/DATA FOLDER/NEU_Pilot1_Data.csv")
df[df == "missing"] <- NA

df <- df %>%
  mutate(across(c(D14_Cog, D14_Emo, D14_Mot, D14_Rel,
                  Change_Cog, Change_Emo, Change_Mot, Change_Rel,
                  D14_Total, Total_Change, Pct_Reduction), as.numeric))

completers <- df %>% filter(!is.na(D14_Total))
completers$Outlier <- "Standard"
completers$Outlier[completers$Participant_Name == "Taylor Wakefield"] <- "Score increase"
completers$Outlier[completers$Participant_Name == "Louis Carmichael"] <- "Low reliability"
completers$Participant_ID <- paste0("P", seq_len(nrow(completers)))

ls()

library(dplyr)
library(tidyr)
library(ggplot2)
df <- read.csv("~/Desktop/JAI BEHAVIOURAL/NEU Pilot Tools/DATA FOLDER/NEU_Pilot1_Data.csv")
df[df == "missing"] <- NA
df <- df %>%
  mutate(across(c(D14_Cog, D14_Emo, D14_Mot, D14_Rel,
                  Change_Cog, Change_Emo, Change_Mot, Change_Rel,
                  D14_Total, Total_Change, Pct_Reduction), as.numeric))
df <- df %>%
  mutate(across(c(D14_Cog, D14_Emo, D14_Mot, D14_Rel,
                  Change_Cog, Change_Emo, Change_Mot, Change_Rel), as.numeric))
df <- df %>%
  mutate(
    D0_Total = D0_Cog + D0_Emo + D0_Mot + D0_Rel,
    D14_Total = D14_Cog + D14_Emo + D14_Mot + D14_Rel,
    Total_Change = D14_Total - D0_Total,
    Pct_Reduction = round((Total_Change / D0_Total) * 100, 1)
  )
completers <- df %>% filter(!is.na(D14_Total))
completers$Outlier <- "Standard"
completers$Outlier[completers$Participant_Name == "Taylor Wakefield"] <- "Score increase"
completers$Outlier[completers$Participant_Name == "Louis Carmichael"] <- "Low reliability"
completers$Participant_ID <- paste0("P", seq_len(nrow(completers)))
ls()
domain_means <- completers %>%
  summarise(
    D0_Cog = mean(D0_Cog, na.rm = TRUE),
    D14_Cog = mean(D14_Cog, na.rm = TRUE),
    D0_Emo = mean(D0_Emo, na.rm = TRUE),
    D14_Emo = mean(D14_Emo, na.rm = TRUE),
    D0_Mot = mean(D0_Mot, na.rm = TRUE),
    D14_Mot = mean(D14_Mot, na.rm = TRUE),
    D0_Rel = mean(D0_Rel, na.rm = TRUE),
    D14_Rel = mean(D14_Rel, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(),
               names_to = c("Time", "Domain"),
               names_sep = "_",
               values_to = "Mean_Score") %>%
  mutate(Time = factor(Time, levels = c("D0", "D14")))

print(domain_means)
domain_means <- 42
print(domain_means)
completers %>% summarise(D0_Cog = mean(D0_Cog, na.rm = TRUE))
domain_means <- completers %>% summarise(D0_Cog = mean(D0_Cog, na.rm = TRUE))
print(domain_means)
domain_means <- completers %>% summarise(D0_Cog = mean(D0_Cog, na.rm = TRUE))
print(domain_means)
domain_means <- completers %>%
  summarise(
    D0_Cog = mean(D0_Cog, na.rm = TRUE),
    D14_Cog = mean(D14_Cog, na.rm = TRUE),
    D0_Emo = mean(D0_Emo, na.rm = TRUE),
    D14_Emo = mean(D14_Emo, na.rm = TRUE),
    D0_Mot = mean(D0_Mot, na.rm = TRUE),
    D14_Mot = mean(D14_Mot, na.rm = TRUE),
    D0_Rel = mean(D0_Rel, na.rm = TRUE),
    D14_Rel = mean(D14_Rel, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(),
               names_to = c("Time", "Domain"),
               names_sep = "_",
               values_to = "Mean_Score") %>%
  mutate(Time = factor(Time, levels = c("D0", "D14")))

print(domain_means)


domain_means <- completers %>%
  summarise(
    D0_Cog = mean(D0_Cog, na.rm = TRUE),
    D14_Cog = mean(D14_Cog, na.rm = TRUE),
    D0_Emo = mean(D0_Emo, na.rm = TRUE),
    D14_Emo = mean(D14_Emo, na.rm = TRUE),
    D0_Mot = mean(D0_Mot, na.rm = TRUE),
    D14_Mot = mean(D14_Mot, na.rm = TRUE),
    D0_Rel = mean(D0_Rel, na.rm = TRUE),
    D14_Rel = mean(D14_Rel, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(),
               names_to = c("Time", "Domain"),
               names_sep = "_",
               values_to = "Mean_Score") %>%
  mutate(Time = factor(Time, levels = c("D0", "D14")))

print(domain_means)

ggplot(domain_means, aes(x = Domain, y = Mean_Score, fill = Time)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  scale_fill_manual(values = c("D0" = "#1B3A6B", "D14" = "#E67E22"),
                    labels = c("D0" = "Day 0", "D14" = "Day 14")) +
  scale_y_continuous(limits = c(0, 9), breaks = seq(0, 9, by = 3)) +
  labs(
    title = "Domain Mean Scores: Day 0 to Day 14",
    subtitle = "NEU Pilot 1 - All Completers",
    x = "Burnout Domain",
    y = "Mean Score (/9)",
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, colour = "#555555"),
    panel.grid.major.x = element_blank(),
    axis.line.x = element_line(colour = "black", linewidth = 0.5),
    axis.line.y = element_line(colour = "black", linewidth = 0.5),
    axis.ticks = element_line(colour = "black")
  )
domain_means$Domain <- factor(domain_means$Domain,
                              levels = c("Emo", "Mot", "Rel", "Cog"))

domain_means$Domain <- recode(domain_means$Domain,
                              "Cog" = "Cognitive",
                              "Emo" = "Emotional",
                              "Mot" = "Motivation",
                              "Rel" = "Relational")

ggplot(domain_means, aes(x = Domain, y = Mean_Score, fill = Time)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  scale_fill_manual(values = c("D0" = "#1B3A6B", "D14" = "#E67E22"),
                    labels = c("D0" = "Day 0", "D14" = "Day 14")) +
  scale_y_continuous(limits = c(0, 9), breaks = seq(0, 9, by = 3)) +
  labs(
    title = "Domain Mean Scores: Day 0 to Day 14",
    subtitle = "NEU Pilot 1 - All Completers",
    x = "Burnout Domain",
    y = "Mean Score (/9)",
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, colour = "#555555"),
    panel.grid.major.x = element_blank(),
    axis.line.x = element_line(colour = "black", linewidth = 0.5),
    axis.line.y = element_line(colour = "black", linewidth = 0.5),
    axis.ticks = element_line(colour = "black")
  )
list.files("~/Desktop/JAI BEHAVIOURAL/NEU Pilot Tools/DATA FOLDER/")
names(google)
head(google, 3)
summary(google)
table(google$Would.you.want.to.see.the.full.NEU.Toolkit.implemented.at.your.school.)
table(google$Before.this.pilot..how.long.did.you.see.yourself.continuing.in.your.current.role.)
table(google$How.many.times.a.week.do.you.work.through.a.designated.break.)
table(google$How.often.do.you.feel.you.have.no.spare.capacity.for.anything.additional.)
table(google$How.many.evenings.week.does.your.work.extend.beyond.7pm.)
google$Which.routine.gave.you.the.most.back...time..energy..or.headspace..And.if.you.d.like.to.know.more.about.the.full.NEU.Toolkit..say.so.here.

library(dplyr)
library(tidyr)
library(ggplot2)

df <- read.csv("~/Desktop/JAI BEHAVIOURAL/NEU Pilot Tools/DATA FOLDER/NEU_Pilot1_Data.csv")
df[df == "missing"] <- NA

df <- df %>%
  mutate(across(c(D14_Cog, D14_Emo, D14_Mot, D14_Rel,
                  Change_Cog, Change_Emo, Change_Mot, Change_Rel), as.numeric)) %>%
  mutate(
    D0_Total = D0_Cog + D0_Emo + D0_Mot + D0_Rel,
    D14_Total = D14_Cog + D14_Emo + D14_Mot + D14_Rel,
    Total_Change = D14_Total - D0_Total,
    Pct_Reduction = round((Total_Change / D0_Total) * 100, 1)
  )

completers <- df %>% filter(!is.na(D14_Total))
completers$Outlier <- "Standard"
completers$Outlier[completers$Participant_Name == "Taylor Wakefield"] <- "Score increase"
completers$Outlier[completers$Participant_Name == "Louis Carmichael"] <- "Low reliability"
completers$Participant_ID <- paste0("P", seq_len(nrow(completers)))

nrow(completers)
library(dplyr)
library(tidyr)
library(ggplot2)

df <- read.csv("~/Desktop/JAI BEHAVIOURAL/NEU Pilot Tools/DATA FOLDER/NEU_Pilot1_Data.csv")
df[df == "missing"] <- NA

df <- df %>%
  mutate(across(c(D14_Cog, D14_Emo, D14_Mot, D14_Rel,
                  Change_Cog, Change_Emo, Change_Mot, Change_Rel), as.numeric)) %>%
  mutate(
    D0_Total = D0_Cog + D0_Emo + D0_Mot + D0_Rel,
    D14_Total = D14_Cog + D14_Emo + D14_Mot + D14_Rel,
    Total_Change = D14_Total - D0_Total,
    Pct_Reduction = round((Total_Change / D0_Total) * 100, 1)
  )

completers <- df %>% filter(!is.na(D14_Total))
completers$Outlier <- "Standard"
completers$Outlier[completers$Participant_Name == "Taylor Wakefield"] <- "Score increase"
completers$Outlier[completers$Participant_Name == "Louis Carmichael"] <- "Low reliability"
completers$Participant_ID <- paste0("P", seq_len(nrow(completers)))

nrow(completers)
df %>% select(Participant_Name, D14_Cog) %>% print(n=12)
library(dplyr)
df %>% select(Participant_Name, D14_Cog) %>% print(n=12)
print(df[, c("Participant_Name", "D14_Cog")], n=12)
df[, c("Participant_Name", "D14_Cog")]
df <- read.csv("~/Desktop/JAI BEHAVIOURAL CC/NEU Pilot Tools/DATA FOLDER/NEU_Pilot1_Data.csv")
df[, c("Participant_Name", "D14_Cog")]
completers %>%
  summarise(
    n = n(),
    Mean_D0 = round(mean(D0_Total), 1),
    Mean_D14 = round(mean(D14_Total), 1),
    Mean_Change = round(mean(Total_Change), 1),
    Mean_Pct = round(mean(Pct_Reduction), 1),
    SD_Pct = round(sd(Pct_Reduction), 1)
  )
completers <- df %>% filter(!is.na(D14_Total))
nrow(completers)
df[df$Participant_Name == "Siree Mcready", c("D14_Cog", "D14_Emo", "D14_Mot", "D14_Rel", "D14_Total")]
names(df)
df[df == "missing"] <- NA
df$D14_Cog <- as.numeric(df$D14_Cog)
df$D14_Emo <- as.numeric(df$D14_Emo)
df$D14_Mot <- as.numeric(df$D14_Mot)
df$D14_Rel <- as.numeric(df$D14_Rel)
df$D0_Total <- df$D0_Cog + df$D0_Emo + df$D0_Mot + df$D0_Rel
df$D14_Total <- df$D14_Cog + df$D14_Emo + df$D14_Mot + df$D14_Rel
df$Total_Change <- df$D14_Total - df$D0_Total
df$Pct_Reduction <- round((df$Total_Change / df$D0_Total) * 100, 1)
completers <- df[!is.na(df$D14_Total), ]
nrow(completers)
df[df$Participant_Name == "Siree Mcready", c("D14_Cog", "D14_Emo", "D14_Mot", "D14_Rel", "D14_Total")]
df[5, c("D14_Cog", "D14_Emo", "D14_Mot", "D14_Rel")]
df[5, "D14_Total"]
df$D14_Total <- df$D14_Cog + df$D14_Emo + df$D14_Mot + df$D14_Rel
df$D14_Cog <- as.numeric(as.character(df$D14_Cog))
df$D14_Emo <- as.numeric(as.character(df$D14_Emo))
df$D14_Mot <- as.numeric(as.character(df$D14_Mot))
df$D14_Rel <- as.numeric(as.character(df$D14_Rel))
df$D14_Total <- df$D14_Cog + df$D14_Emo + df$D14_Mot + df$D14_Rel
df[5, "D14_Total"]
df[["D14_Total"]] <- df[["D14_Cog"]] + df[["D14_Emo"]] + df[["D14_Mot"]] + df[["D14_Rel"]]
class(df$D14_Cog)
df$D14_Cog <- as.numeric(as.character(df$D14_Cog))
class(df$D14_Cog)
df$D14_Cog <- as.numeric(as.character(df$D14_Cog))
class(df$D14_Cog)
