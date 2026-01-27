# Visualizations for Diabetes Prevalence Comparison
# Compares prevalence estimates from different data sources

library(tidyverse)
library(scales)

# ==============================================================================
# LOAD DATA
# ==============================================================================

merged_data <- read_csv("merged_data.csv", show_col_types = FALSE)

# Order age groups properly
age_order <- c("18-34", "35-44", "45-54", "55-64", "65-74", "75+")
merged_data <- merged_data %>%
  mutate(age_group = factor(age_group, levels = age_order))



# ==============================================================================
# 1. SCATTER PLOT FACETED BY RACE: Mass DPH vs ICD
# ==============================================================================

scatter_by_race <- ggplot(merged_data, aes(x = pct_diabetes_icd, y = pct_Mass_DPH_dm)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(aes(color = age_group,shape=sex) , alpha = 0.6, size = 1.5) +
  facet_wrap(~race, scales = "fixed") +
  labs(
    title = "Mass DPH vs ICD Prevalence by Race",
    subtitle = "Points above line = Mass DPH estimate higher",
    x = "ICD-Based Prevalence (%)",
    y = "Mass DPH Prevalence (%)",
    color = "Age Group"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 9)
  )

ggsave("plots/plot_scatter_by_race.png", scatter_by_race, width = 12, height = 8, dpi = 150, bg = "white")

# ==========

# ==============================================================================
# 2. SCATTER PLOT FACETED BY RACE: Mass DPH vs Hb1A1c
# ==============================================================================

scatter_by_race <- ggplot(merged_data, aes(x = pct_a1c_65_plus, y = pct_Mass_DPH_dm)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(aes(color = age_group,shape=sex) , alpha = 0.6, size = 1.5) +
  facet_wrap(~race, scales = "fixed") +
  labs(
    title = "Mass DPH vs HbA1c>6.5 Prevalence by Race",
    subtitle = "Points above line = Mass DPH estimate higher",
    x = "HbA1c>6.5 (%)",
    y = "Mass DPH Prevalence (%)",
    color = "Age Group"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 9)
  )

ggsave("plots/plot_scatter_by_race_hba1c.png", scatter_by_race, width = 12, height = 8, dpi = 150, bg = "white")

# ==========


# ==============================================================================
# 3. SCATTER PLOT FACETED BY age: Mass DPH vs ICD
# ==============================================================================

scatter_by_race <- ggplot(merged_data, aes(x = pct_diabetes_icd, y = pct_Mass_DPH_dm)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(aes(color = race,shape=sex) , alpha = 0.6, size = 1.5) +
  facet_wrap(~age_group, scales = "fixed") +
  labs(
    title = "Mass DPH vs ICD Prevalence by Age",
    subtitle = "Points above line = Mass DPH estimate higher",
    x = "ICD-Based Prevalence (%)",
    y = "Mass DPH Prevalence (%)",
    color = "Race"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 9)
  )

ggsave("plots/plot_scatter_by_age.png", scatter_by_race, width = 12, height = 8, dpi = 150, bg = "white")

# ==========

# ==============================================================================
# 2. SCATTER PLOT FACETED BY RACE: Mass DPH vs Hb1A1c
# ==============================================================================

scatter_by_race <- ggplot(merged_data, aes(x = pct_a1c_65_plus, y = pct_Mass_DPH_dm)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(aes(color = race,shape=sex) , alpha = 0.6, size = 1.5) +
  facet_wrap(~age_group, scales = "fixed") +
  labs(
    title = "Mass DPH vs HbA1c>6.5% Prevalence by Age",
    subtitle = "Points above line = Mass DPH estimate higher",
    x = "HbA1c>6.5 (%)",
    y = "Mass DPH Prevalence (%)",
    color = "Race"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 9)
  )

ggsave("plots/plot_scatter_by_age_hba1c.png", scatter_by_race, width = 12, height = 8, dpi = 150, bg = "white")

# ==========