# ============================================================
# Figure 8: Sleep Duration vs. Days of Poor Mental Health
# Script: Figure 8.R 
# Author: Yianni Papagiannopoulos
# Modified: 2025-11-11  (added Gaussian noise for visualization)
# ============================================================

library(dplyr)
library(ggplot2)
library(gridExtra)

NHANESraw <- read.csv("NHANESraw.csv")

df <- NHANESraw %>%
  filter(Age >= 16, !is.na(SleepHrsNight), !is.na(DaysMentHlthBad)) %>%
  mutate(
    AgeGroup = cut(
      Age,
      breaks = c(15, 24, 34, 44, 54, 64, 74, Inf),
      labels = c("16–24","25–34","35–44","45–54","55–64","65–74","75+"),
      right = TRUE
    ),
    DaysMentHlthBad = pmin(pmax(DaysMentHlthBad, 0), 30)
  )

# Spearman stats by age group
stats_tbl <- df %>%
  group_by(AgeGroup) %>%
  summarise(
    n   = n(),
    rho = suppressWarnings(cor(SleepHrsNight, DaysMentHlthBad, method = "spearman")),
    p   = suppressWarnings(cor.test(SleepHrsNight, DaysMentHlthBad, method = "spearman")$p.value),
    .groups = "drop"
  ) %>%
  mutate(
    rho = round(rho, 2),
    `p-value` = ifelse(is.na(p), NA_character_,
                       ifelse(p < 0.001, "< 0.001", sprintf("%.3f", p))
    )
  ) %>%
  select(`Age Group` = AgeGroup, `ρ (Spearman)` = rho, `p-value`, n)

# Consistent visualization noise (points only)
set.seed(78)
noise_sleep <- 0.20    # ≈ ±12 minutes
noise_days  <- 0.50    # ≈ ±½ day

df_plot <- df %>%
  mutate(
    SleepHrs_jit = SleepHrsNight   + rnorm(n(), 0, noise_sleep),
    DaysMH_jit   = DaysMentHlthBad + rnorm(n(), 0, noise_days)
  )

pal <- c("#1f77b4","#ff7f0e","#2ca02c","#d62728",
         "#9467bd","#8c564b","#17becf")

# Scatter + LOESS (LOESS uses TRUE values; points use jittered values)
p_scatter <- ggplot() +
  geom_point(
    data = df_plot,
    aes(SleepHrs_jit, DaysMH_jit, color = AgeGroup),
    alpha = 0.18, size = 1
  ) +
  geom_smooth(
    data = df,
    aes(SleepHrsNight, DaysMentHlthBad, color = AgeGroup),
    method = "loess", se = TRUE, linewidth = 0.9, alpha = 0.10
  ) +
  scale_color_manual(values = pal, name = "Age Group") +
  scale_x_continuous(limits = c(2, 12), breaks = 2:12) +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, 5)) +
  labs(
    title = "Sleep Duration vs. Days of Poor Mental Health (Past 30 Days)",
    subtitle = "Scatter with LOESS fits by age group; table below lists Spearman ρ, p-values, and sample sizes (NHANES 2009–2011)",
    x = "Sleep Hours per Night",
    y = "Days of Poor Mental Health (0–30)",
    caption = "Note: Both variables are integer in NHANES; small Gaussian noise (N(0, 0.20) for sleep, N(0, 0.50) for days) added to points for visualization only."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title.position = "plot",
    legend.position = "right"
  )

tg <- tableGrob(
  stats_tbl,
  rows = NULL,
  theme = ttheme_minimal(
    core    = list(fg_params = list(cex = 0.85)),
    colhead = list(fg_params = list(fontface = 2, cex = 0.9)),
    padding = unit(c(6, 3), "pt")
  )
)

grid.arrange(p_scatter, tg, ncol = 1, heights = c(3, 0.8))
