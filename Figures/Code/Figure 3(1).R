library(dplyr)
library(ggplot2)
library(forcats)
library(scales)

NHANESraw <- read.csv("NHANESraw.csv")

# Keep 16+ and needed vars; drop NA SleepTrouble as requested
base <- NHANESraw %>%
  filter(Age >= 16, !is.na(SleepTrouble)) %>%
  mutate(
    SleepTrouble = factor(SleepTrouble, levels = c("No","Yes")),
    AgeGroup = cut(Age,
                   breaks = c(15, 24, 34, 44, 54, 64, 74, Inf),
                   labels = c("16–24","25–34","35–44","45–54","55–64","65–74","75+"),
                   right = TRUE
    )
  )

# small helper for normal-approx 95% CI (good for EDA)
add_prop_ci <- function(df, num, den) {
  df %>%
    mutate(
      p_hat = {{num}} / {{den}},
      se    = sqrt(p_hat * (1 - p_hat) / {{den}}),
      lo    = pmax(0, p_hat - 1.96 * se),
      hi    = pmin(1, p_hat + 1.96 * se)
    )
}

# uses `base` and add_prop_ci() already defined above

library(forcats)

# Summarize by Race x AgeGroup (across all genders)
race_age <- base %>%
  filter(!is.na(Race1)) %>%
  group_by(AgeGroup, Race1) %>%
  summarise(
    n_yes = sum(SleepTrouble == "Yes"),
    n_tot = n(),
    .groups = "drop"
  ) %>%
  add_prop_ci(num = n_yes, den = n_tot)

# Order races by overall prevalence (avg across ages) for a stable legend/order
race_levels <- race_age %>%
  group_by(Race1) %>% summarise(avg = mean(p_hat), .groups="drop") %>%
  arrange(desc(avg)) %>% pull(Race1)

race_age <- race_age %>% mutate(Race1 = factor(Race1, levels = race_levels))

pd <- position_dodge(width = 0.75)

ggplot(race_age, aes(x = AgeGroup, y = p_hat, fill = Race1)) +
  geom_col(position = pd, width = 0.65, color = "gray35") +
  geom_errorbar(aes(ymin = lo, ymax = hi), position = pd, width = 0.24, color = "gray35") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_fill_brewer(palette = "Set2", name = "Race / Ethnicity") +
  # scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 0.4)) + # Scale y axis to 40%
  labs(
    title = "Reported Trouble Sleeping by Age Group (16+): Race/Ethnicity",
    subtitle = "Bars show % reporting trouble sleeping; error bars are 95% CIs (NHANES 2009–2011)",
    x = "Age Group",
    y = "Proportion of Group"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    plot.title.position = "plot"
  )
