library(tidyverse)
library(scales)

monthly_losses_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-17/monthly_losses_data.csv')
#monthly_mortality_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-17/monthly_mortality_data.csv')

#write.csv(monthly_losses_data, "monthly_losses_data.csv")
#write.csv(monthly_mortality_data, "monthly_mortality_data.csv")

# data cleaning - only interested in the county level data
monthly_losses_data <- monthly_losses_data |> filter(geo_group == 'county')
#monthly_mortality_data <- monthly_mortality_data |> filter(geo_group == 'county')

# total losses for the most recent month
monthly_losses_data |>
  filter(species == 'rainbowtrout') |>
  group_by(date) |>
  summarize(total_losses = sum(losses)) |>
  filter(date == max(date)) |>
  pull(total_losses)

# percent change from the previous month
monthly_losses_data |>
  filter(species == 'rainbowtrout') |>
  group_by(date) |>
  summarize(total_losses = sum(losses)) |>
  mutate(pct_change = (total_losses - lag(total_losses)) / lag(total_losses) * 100) |>
  filter(date == max(date)) |>
  pull(pct_change)

# table of percent changes over the last 12 months
monthly_losses_data |>
  filter(species == 'rainbowtrout') |>
  group_by(date) |>
  summarize(total_losses = sum(losses)) |>
  mutate(pct_change = (total_losses - lag(total_losses)) / lag(total_losses) * 100) |>
  filter(between(date, max(date) - months(12), max(date)))

# chart of losses by county over the last 12 months
monthly_losses_data |>
  filter(species == 'rainbowtrout') |>
  group_by(region, date) |>
  summarize(total_losses = sum(losses)) |>
  filter(between(date, max(date) - months(12), max(date))) |>
  ggplot(aes(x = date, y = total_losses, color = region, group = region)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  labs(x = "Month", y = "Total losses", title = "Salmonid losses by county", subtitle = "Previous 12 months")


  






