# ============================================================
# Figure 5: Reported Physical Activity vs. Sleep Duration
# Script: Figure 5.R
# Author: Yianni Papagiannopoulos
# Modified: 2025-11-11 (added Gaussian noise for visualization)
# ============================================================

library(dplyr)
library(ggplot2)

# Load and process 
NHANESraw <- read.csv("NHANESraw.csv")

plot_data <- NHANESraw %>%
  filter(!is.na(SleepHrsNight), !is.na(PhysActive)) %>%
  mutate(
    PhysActive = factor(
      PhysActive,
      levels = c("No", "Yes"),
      labels = c("Not Physically Active", "Physically Active")
    )
  )

# Group means (TRUE values; used for the red line)
group_means <- plot_data %>%
  group_by(PhysActive) %>%
  summarise(mean_sleep = mean(SleepHrsNight, na.rm = TRUE), .groups = "drop")

# ---- Add light Gaussian noise for display (points only) ----
set.seed(78)
noise_y <- 0.25  # ≈ ±12 minutes
plot_points <- plot_data %>%
  mutate(SleepHrs_jit = SleepHrsNight + rnorm(n(), 0, noise_y))

# Colors
fills  <- c("Not Physically Active" = "#cbd5e1",   # slate-200
            "Physically Active"     = "#5b8fd9")   # aegean-ish blue
outln  <- "#9aa1aa"                                # softer outline
ptcols <- c("Not Physically Active" = "#6b7280",   # neutral-500
            "Physically Active"     = "#335f9e")   # deeper blue

ggplot() +
  # Boxplots on TRUE values
  geom_boxplot(
    data = plot_data,
    aes(x = PhysActive, y = SleepHrsNight, fill = PhysActive),
    alpha = 0.85, width = 0.55, outlier.shape = NA, color = outln
  ) +
  # Jittered points (visual only)
  geom_point(
    data = plot_points,
    aes(x = PhysActive, y = SleepHrs_jit, color = PhysActive),
    position = position_jitter(width = 0.13, height = 0),  # x jitter only; y is from rnorm
    alpha = 0.28, size = 1.25, stroke = 0
  ) +
  # Group means (TRUE values)
  geom_segment(
    data = group_means,
    aes(x = as.numeric(PhysActive) - 0.26, xend = as.numeric(PhysActive) + 0.26,
        y = mean_sleep, yend = mean_sleep),
    color = "#c1121f", linewidth = 1.15
  ) +
  scale_fill_manual(values = fills) +
  scale_color_manual(values = ptcols, guide = "none") +
  scale_y_continuous(limits = c(2, 12.5), breaks = seq(2, 12, 1), expand = expansion(mult = c(0.02, 0.04))) +
  labs(
    title = "Reported Physical Activity vs. Sleep Duration",
    x = NULL,
    y = "Sleep Hours per Night",
    caption = "Note: Sleep hours are integer in NHANES; small N(0, 0.25) noise added to points for visualization only."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#e5e7eb"),
    axis.title.y = element_text(margin = margin(r = 6)),
    plot.title.position = "plot"
  )
