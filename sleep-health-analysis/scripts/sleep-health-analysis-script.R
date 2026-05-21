# =============================================================
# Sleep Health & Lifestyle Analysis
# Author: B. Shamim
# Data: Sleep Health and Lifestyle Dataset (Kaggle)
# =============================================================


# ── INSTALL PACKAGES ─────────────────────────────────────────
install.packages("tidyverse")
install.packages("janitor")

# ── LOAD PACKAGES ────────────────────────────────────────────
library(tidyverse)
library(janitor)

# ── LOAD PACKAGES ────────────────────────────────────────────
setwd("~/Documents/sleep-health-analysis/data")

# Exact file path will depend on your folder structures and naming

# ── LOAD DATA ────────────────────────────────────────────────
sleep <- read_csv("~/Documents/sleep-health-analysis/data/Sleep_health_and_lifestyle_dataset.csv")

# Quick look
glimpse(sleep)
summary(sleep)

# ── CLEAN ────────────────────────────────────────────────────
# Standardise column names
sleep <- sleep %>%
  janitor::clean_names()  

# Check for missing values
colSums(is.na(sleep))

# ── EXPLORE ──────────────────────────────────────────────────

# 1. Sleep duration distribution
ggplot(sleep, aes(x = sleep_duration)) +
  geom_histogram(bins = 15, fill = "#1B6B7B",
                 colour = "white", alpha = 0.85) +
  geom_vline(xintercept = 7, linetype = "dashed",
             colour = "#C47A1E") +
  annotate("text", x = 7.1, y = Inf,
           label = "7h guideline", hjust = 0,
           vjust = 2, colour = "#C47A1E", size = 3.5) +
  labs(title = "Distribution of Sleep Duration",
       subtitle = "Sample of 374 healthy adults",
       x = "Sleep duration (hours)",
       y = "Count") +
  theme_minimal()

ggsave("~/Documents/sleep-health-analysis/outputs/01_sleep_duration.png",
       width = 8, height = 5, dpi = 150)

# 2. Stress level vs sleep quality
ggplot(sleep, aes(x = stress_level,
                  y = quality_of_sleep)) +
  geom_point(alpha = 0.4, colour = "#1B6B7B", size = 2.5) +
  geom_smooth(method = "lm", colour = "#C47A1E",
              fill = "#C47A1E", alpha = 0.15) +
  labs(title = "Stress Level vs Sleep Quality",
       subtitle = "Higher stress is associated with lower sleep quality ratings",
       x = "Stress level (1-10 scale)",
       y = "Sleep quality rating (1-10)") +
  theme_minimal()

ggsave("~/Documents/sleep-health-analysis/outputs/02_stress_vs_sleep_quality.png",
       width = 8, height = 5, dpi = 150)

# 3. Sleep duration by BMI category
ggplot(sleep, aes(x = bmi_category,
                  y = sleep_duration,
                  fill = bmi_category)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21) +
  geom_jitter(width = 0.1, alpha = 0.4, size = 1.5) +
  scale_fill_manual(values = c(
    "Normal"       = "#1B6B7B",
    "Normal Weight"= "#1B6B7B",
    "Overweight"   = "#C47A1E",
    "Obese"        = "#8B1A2E"
  )) +
  labs(title = "Sleep Duration by BMI Category",
       x = NULL,
       y = "Sleep duration (hours)") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("~/Documents/sleep-health-analysis/outputs/03_sleep_by_bmi.png",
       width = 8, height = 5, dpi = 150)

# 4. Physical activity vs sleep duration
ggplot(sleep, aes(x = physical_activity_level,
                  y = sleep_duration)) +
  geom_point(alpha = 0.4, colour = "#1B6B7B", size = 2.5) +
  geom_smooth(method = "lm", colour = "#C47A1E",
              fill = "#C47A1E", alpha = 0.15) +
  labs(title = "Physical Activity vs Sleep Duration",
       subtitle = "Do more active people sleep longer?",
       x = "Physical activity level (min/day)",
       y = "Sleep duration (hours)") +
  theme_minimal()

ggsave("~/Documents/sleep-health-analysis/outputs/04_activity_vs_sleep.png",
       width = 8, height = 5, dpi = 150)

# 5. Sleep disorders breakdown
sleep %>%
  count(sleep_disorder) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  ggplot(aes(x = reorder(sleep_disorder, n),
             y = n, fill = sleep_disorder)) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = paste0(pct, "%")),
            hjust = -0.1, size = 3.5) +
  scale_fill_manual(values = c(
    "None"         = "#1B6B7B",
    "Sleep Apnea"  = "#C47A1E",
    "Insomnia"     = "#8B1A2E"
  )) +
  coord_flip() +
  labs(title = "Sleep Disorder Prevalence in Sample",
       x = NULL, y = "Count") +
  theme_minimal() +
  theme(legend.position = "none") +
  expand_limits(y = max(sleep %>% count(sleep_disorder) %>% pull(n)) * 1.15)

ggsave("~/Documents/sleep-health-analysis/outputs/05_sleep_disorders.png",
       width = 8, height = 4, dpi = 150)

# ── SUMMARY TABLE ────────────────────────────────────────────
# Key stats by sleep disorder group
summary_table <- sleep %>%
  group_by(sleep_disorder) %>%
  summarise(
    n               = n(),
    mean_sleep_h    = round(mean(sleep_duration), 1),
    mean_quality    = round(mean(quality_of_sleep), 1),
    mean_stress     = round(mean(stress_level), 1),
    mean_hr         = round(mean(heart_rate), 1),
    .groups = "drop"
  )

print(summary_table)
write_csv(summary_table,
          "~/Documents/sleep-health-analysis/outputs/summary_by_disorder.csv")

cat("\nAnalysis complete. Figures saved to outputs/\n")