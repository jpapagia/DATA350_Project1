# ============================================================
# Figure 7: Average Sleep Hours vs Diastolic and Systolic
# Script: Figure 7.R
# Author: Alexandra Julka, Yianni Papagiannopoulos
# Modified: 2025-11-11 (added Gaussian noise to SleepHrsNight)
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(scales)

NHANESraw <- read.csv("NHANESraw.csv")

df_bp <- NHANESraw %>%
  transmute(
    SleepHrsNight = suppressWarnings(as.numeric(SleepHrsNight)),
    BPSysAve, BPDiaAve, Age
  ) %>%
  filter(
    Age >= 16,
    between(SleepHrsNight, 2, 12),
    !is.na(BPSysAve),  between(BPSysAve, 70, 250),
    !is.na(BPDiaAve),  between(BPDiaAve, 40, 150)
  ) %>%
  pivot_longer(
    c(BPDiaAve, BPSysAve),
    names_to = "BP_type",
    values_to = "BP_value"
  ) %>%
  mutate(
    BP_type = recode(BP_type,
                     BPDiaAve = "Diastolic (mmHg)",
                     BPSysAve = "Systolic (mmHg)")
  ) %>%
  # add small normal noise (~±15 min) for plotting only ---
  mutate(SleepHrs_jit = SleepHrsNight + rnorm(n(), mean = 0, sd = 0.15))

# Plot
ggplot(df_bp, aes(x = BP_value, y = SleepHrs_jit, color = BP_type)) +
  geom_point(alpha = 0.20, size = 1.6, shape = 16) +
  # regression line uses TRUE sleep hours (no jitter)
  geom_smooth(
    data = df_bp,
    aes(y = SleepHrsNight),
    method = "lm", se = TRUE,
    color = "#377EB8", linewidth = 1.1
  ) +
  facet_wrap(~ BP_type, nrow = 1, scales = "free_x") +
  scale_color_manual(values = c(
    "Diastolic (mmHg)" = "#d62728",   # red-orange
    "Systolic (mmHg)"  = "#1B9E77"    # green
  )) +
  labs(
    title = "Sleep Hours vs. Blood Pressure (Adults 16+)",
    subtitle = "OLS regression fits (blue) with 95% CIs; NHANES 2009–2011, unweighted",
    x = "Blood Pressure (mmHg)",
    y = "Average Sleep Hours per Night",
    color = "Blood Pressure Type",
    caption = "Note: NHANES sleep hours are integer values; small N(0, 0.25) noise added for visualization."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title.position = "plot",
    legend.position = "top"
  )
