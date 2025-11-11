# ============================================================
# Figure 6: Daily Screen Time (Computer + TV) vs. Sleep Duration
# Script: Figure 6.R
# Author: Yianni Papagiannopoulos
# Modified: 2025-11-11 (added Gaussian noise for visualization)
# ============================================================

library(dplyr)
library(ggplot2)
library(stringr)

# Load and process ------------------------------------------------------------
NHANESraw <- read.csv("NHANESraw.csv")

# Helper functions
`%||%` <- function(a, b) if (is.null(a)) b else a

to_hours <- function(x) {
  x_chr <- tolower(trimws(as.character(x)))
  bad <- c("^$", "^don.?t know$", "^refused$", "^do not watch", "^do not use", "^none$")
  x_chr[grepl(paste(bad, collapse = "|"), x_chr)] <- NA
  m <- str_match(x_chr, "(\\d+(?:\\.\\d+)?)\\s*(?:-|–|—|to)?\\s*(\\d+(?:\\.\\d+)?)?")
  n1 <- suppressWarnings(as.numeric(m[, 2]))
  n2 <- suppressWarnings(as.numeric(m[, 3]))
  plus <- str_detect(x_chr %||% "", "\\+")
  ifelse(!is.na(n1) & !is.na(n2), (n1 + n2) / 2,
         ifelse(!is.na(n1) & plus, n1 + 1,
                ifelse(!is.na(n1), n1, NA_real_)))
}

# Build dataset ---------------------------------------------------------------
plot_data <- NHANESraw %>%
  mutate(
    TV_hrs_adult   = to_hours(TVHrsDay),
    Comp_hrs_adult = to_hours(CompHrsDay),
    TotalScreenTime = coalesce(TV_hrs_adult, 0) + coalesce(Comp_hrs_adult, 0)
  ) %>%
  filter(!is.na(SleepHrsNight), TotalScreenTime > 0, Age >= 13)

# Fit for reference (no jitter in model) --------------------------------------
fit <- lm(SleepHrsNight ~ TotalScreenTime, data = plot_data)
slope <- coef(fit)[2]
r2 <- summary(fit)$r.squared

# Add light Gaussian noise for display ----------------------------------------
set.seed(78)
noise_y <- 0.25   # ≈ ±9 minutes
noise_x <- 0.15   # slight horizontal variation
plot_data <- plot_data %>%
  mutate(
    SleepHrs_jit = SleepHrsNight + rnorm(n(), 0, noise_y),
    TST_jit      = TotalScreenTime + rnorm(n(), 0, noise_x)
  )

# Final plot ------------------------------------------------------------------
ggplot(plot_data, aes(x = TST_jit, y = SleepHrs_jit)) +
  stat_bin_2d(bins = 25, aes(fill = after_stat(count / sum(count)))) +
  scale_fill_gradient(
    low = "grey95", high = "#2b6cb0",
    labels = scales::percent_format(accuracy = 1),
    name = "Proportion of sample"
  ) +
  # Regression on true data (no noise)
  geom_smooth(
    data = plot_data,
    aes(x = TotalScreenTime, y = SleepHrsNight),
    method = "lm", se = TRUE, color = "#2b6cb0", linewidth = 0.8
  ) +
  scale_x_continuous(limits = c(0, 10), breaks = 0:10) +
  scale_y_continuous(limits = c(2, 12.5), breaks = seq(2, 12, 1)) +
  labs(
    title = "Daily Screen Time vs. Sleep Duration",
    x = "Total Screen Time (hours/day)",
    y = "Sleep Hours per Night",
    caption = "Note: NHANES sleep hours are integer values; small N(0, 0.25) noise added for visualization."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.caption = element_text(hjust = 0, size = 10, color = "grey30"),
    plot.title.position = "plot",
    legend.position = "right",
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA)
  )
