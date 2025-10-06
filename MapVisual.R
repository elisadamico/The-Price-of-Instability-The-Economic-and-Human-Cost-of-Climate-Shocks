# =============================================================================
# Climate Shocks, Food Prices & Conflict Analysis - Volatility Map
# Author: Elisa D'Amico
# Date Created: July 24, 2025
# Date Last Modified: August 24, 2025
# =============================================================================

# Load libraries
library(dplyr)
library(ggplot2)
library(maps)
library(countrycode)
library(viridis)
library(readxl)

# Load data
panel_data <- read_excel("panel_data.xlsx")
ged <- read_excel("GEDEvent_v25_1.xlsx")

# Prepare conflict event data
ged_data <- ged %>%
  filter(!is.na(longitude) & !is.na(latitude))

# Calculate country-level volatility
country_volatility <- panel_data %>%
  group_by(iso3c) %>%
  summarise(
    avg_food_inflation_volatility_3yr = mean(food_inflation_volatility_3yr, na.rm = TRUE),
    n_years = sum(!is.na(food_inflation_volatility_3yr)),
    .groups = 'drop'
  ) %>%
  filter(!is.na(avg_food_inflation_volatility_3yr) & n_years >= 3) %>%
  mutate(country_name = countrycode(iso3c, "iso3c", "country.name"))

# Cap extreme values at 95th percentile
volatility_95th <- quantile(country_volatility$avg_food_inflation_volatility_3yr, 0.95, na.rm = TRUE)

country_volatility <- country_volatility %>%
  mutate(avg_food_inflation_volatility_capped = pmin(avg_food_inflation_volatility_3yr, volatility_95th))

# Prepare map data
world_map <- map_data("world")

map_data <- country_volatility %>%
  mutate(region = countrycode(iso3c, "iso3c", "country.name"),
         region = case_when(
           region == "United States" ~ "USA",
           region == "United Kingdom" ~ "UK",
           TRUE ~ region
         ))

world_volatility <- world_map %>%
  left_join(map_data, by = "region")

# Create map
volatility_map <- ggplot(world_volatility, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = avg_food_inflation_volatility_capped), color = "black", size = 0.1) +
  scale_fill_viridis_c(
    name = "Food Inflation\nVolatility\n(5-year avg)",
    na.value = "grey20",
    option = "plasma",
    direction = 1,
    breaks = quantile(country_volatility$avg_food_inflation_volatility_capped, 
                      probs = c(0, 0.2, 0.4, 0.6, 0.8, 1.0), na.rm = TRUE),
    labels = function(x) sprintf("%.1f", x)
  ) +
  theme_void() +
  coord_cartesian(ylim = c(-41, 69)) +
  theme(
    plot.background = element_rect(fill = "black", color = "black"),
    panel.background = element_rect(fill = "black", color = "black"),
    text = element_text(family = "Times New Roman", color = "white"),
    legend.position = "bottom",
    legend.background = element_rect(fill = "black", color = "black"),
    legend.title = element_text(size = 11, family = "Times New Roman", color = "white"),
    legend.text = element_text(size = 10, family = "Times New Roman", color = "white"),
    legend.key.width = unit(1.5, "cm")
  )

# Add conflict events overlay
volatility_map_with_ged <- volatility_map +
  geom_point(
    data = ged_data,
    aes(x = longitude, y = latitude, color = "Conflict Event"),
    inherit.aes = FALSE,
    alpha = 0.01,
    size = 0.000001
  ) +
  scale_color_manual(
    name = "",
    values = c("Conflict Event" = "cyan")
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 4, alpha = 1))
  )

print(volatility_map_with_ged)

# Optional: Save the map
# ggsave("food_inflation_volatility_map.png", volatility_map_with_ged, 
#        width = 12, height = 8, dpi = 300)