# =============================================================================
# Climate Shocks, Food Prices & Conflict - Robustness Checks
# Author: Elisa D'Amico
# Date Created: July 23, 2025
# Date Last Modified: August 24, 2025
# =============================================================================

# Load libraries
library(did)
library(dplyr)
library(ggplot2)
library(plm)
library(readr)
library(readxl)

# Load data
panel_data <- read_excel("panel_data.xlsx")
capacity <- read_csv("ndgain/resources/vulnerability/capacity.csv")

# Prepare capacity data
capacity_long <- capacity %>%
  pivot_longer(cols = -c(ISO3, Name), names_to = "year", values_to = "capacity_value") %>%
  mutate(year = as.numeric(year))

panel_data <- panel_data %>%
  left_join(capacity_long, by = c("iso3c" = "ISO3", "year" = "year"))

pdata_full <- pdata.frame(panel_data, index = c("iso3c", "year"))

# Create treatment data
first_flood_treatment <- as.data.frame(pdata_full) %>%
  select(iso3c, year, disaster_subtype_flood_general) %>%
  filter(disaster_subtype_flood_general > 0) %>%
  arrange(iso3c, year) %>%
  group_by(iso3c) %>%
  slice(1) %>%
  ungroup() %>%
  select(-disaster_subtype_flood_general) %>%
  rename(flood_treat = year)

did_data <- as.data.frame(pdata_full) %>%
  left_join(first_flood_treatment, by = "iso3c") %>%
  mutate(
    flood_treat = ifelse(is.na(flood_treat), 0, flood_treat),
    year = as.numeric(year),
    iso3c = as.numeric(as.factor(iso3c))
  )

# =============================================================================
# 1. Alternative Control Group: "nevertreated" instead of "notyettreated"
# =============================================================================

outcomes <- c("log_fatalities_best", "log_conflicteventcount")
titles <- c("Log Fatalities", "Log Conflict Events")

for(i in 1:length(outcomes)) {
  set.seed(2025)
  
  sc_result <- att_gt(
    yname = outcomes[i],
    tname = "year",
    idname = "iso3c",
    gname = "flood_treat",
    data = did_data,
    control_group = "nevertreated",
    bstrap = TRUE
  )
  
  dynamic_att <- aggte(sc_result, type = "dynamic")
  
  p <- ggdid(dynamic_att) + 
    labs(title = paste("Never Treated Control:", titles[i]),
         x = "Time to Treatment", y = "Average Treatment Effect") +
    scale_color_manual(values = c("#fb9b06", "#CC00CC")) +
    theme_minimal()
  
  print(p)
  print(summary(dynamic_att))
}

# =============================================================================
# 2. Placebo Test: Random Treatment Assignment
# =============================================================================

set.seed(666)

unique_countries <- unique(as.data.frame(pdata_full)$iso3c)
selected_countries <- sample(unique_countries, size = floor(length(unique_countries) / 2))

placebo_treatment <- data.frame(
  iso3c = selected_countries,
  placebo_treat = sample(1:65, size = length(selected_countries), replace = TRUE)
)

placebo_did_data <- as.data.frame(pdata_full) %>%
  mutate(iso3c = as.character(iso3c)) %>%
  left_join(placebo_treatment, by = "iso3c") %>%
  mutate(
    placebo_treat = ifelse(is.na(placebo_treat), 0, placebo_treat),
    year = as.numeric(year),
    iso3c = as.numeric(as.factor(iso3c))
  )

for(i in 1:length(outcomes)) {
  set.seed(2025)
  
  sc_result <- att_gt(
    yname = outcomes[i],
    tname = "year",
    idname = "iso3c",
    gname = "placebo_treat",
    data = placebo_did_data,
    control_group = "notyettreated",
    bstrap = TRUE
  )
  
  dynamic_att <- aggte(sc_result, type = "dynamic")
  
  p <- ggdid(dynamic_att) + 
    labs(title = paste("Placebo Test:", titles[i]),
         x = "Time to Treatment", y = "Average Treatment Effect") +
    scale_color_manual(values = c("#fb9b06", "#CC00CC")) +
    theme_minimal()
  
  print(p)
  print(summary(dynamic_att))
}

# =============================================================================
# 3. Controlling for Adaptive Capacity
# =============================================================================

for(i in 1:length(outcomes)) {
  set.seed(666)
  
  sc_result <- att_gt(
    yname = outcomes[i],
    tname = "year",
    idname = "iso3c",
    gname = "flood_treat",
    xformla = ~food_inflation_avg + capacity_value,
    data = did_data,
    control_group = "notyettreated",
    bstrap = TRUE
  )
  
  dynamic_att <- aggte(sc_result, type = "dynamic")
  
  p <- ggdid(dynamic_att) + 
    labs(title = paste("With Capacity Controls:", titles[i]),
         x = "Time to Treatment", y = "Average Treatment Effect") +
    scale_color_manual(values = c("#fb9b06", "#CC00CC")) +
    theme_minimal()
  
  print(p)
  print(summary(dynamic_att))
}