# Enhanced Data Loading Script for Multi-Source Analysis
# =======================================================
# Author: Elisa D'Amico
# Date Created: July 23, 2025
# Date Last Modified: July 24, 2025
# 
# Research Abstract:
# This research leverages novel Random Forest methodologies to identify which 
# climate shocks—such as droughts, floods, storms, temperature extremes, and 
# so on—most significantly impact food price fluctuations, which in turn 
# exacerbate existing conflict, measured by battle deaths. By examining the 
# economic costs of food price shocks, the study will highlight not only the 
# direct financial burdens (e.g., increased food prices leading to economic 
# instability) but also the human costs, as rising food insecurity can intensify 
# existing crises and contribute to violence and conflict. To observe this effect, 
# the second part of the study employs an interaction-weighted estimator 
# difference-in-differences approach with staggered treatment, allowing for a 
# nuanced analysis of how food price shocks, driven by climate events, influence 
# conflict escalation over time. The goal is to understand how climate-induced 
# food price shocks act as a catalyst for conflict, particularly in vulnerable 
# regions, and to provide evidence-based insights that could guide policies to 
# mitigate both the economic and human costs of these interconnected crises.
#
# Purpose: Load and prepare datasets for climate shock, food price, conflict,
#          and governance analysis using Random Forest and DiD methodologies

# Clear 
rm(list = ls())

# Load Packages
library(readxl)
library(dplyr)
library(readr)
library(vdemdata)
library(countrycode)
library(tidyr)
library(moments)  # for skewness function
library(openxlsx)
library(zoo)  # for rollapply


# =======================================================
# DISASTER DATA
# =======================================================
# Emergency Events Database (EM-DAT)
# Source: https://www.emdat.be/
emdat <- read_excel("emdat.xlsx")

# =======================================================
# CONFLICT DATA  
# =======================================================
# Uppsala Conflict Data Program (UCDP) - Armed Conflict Dataset
# Source: https://ucdp.uu.se/downloads/index.html#armedconflict
ucdp_prio <- read_excel("UcdpPrioConflict_v25_1.xlsx")

# UCDP Georeferenced Event Dataset (GED)
# Source: https://ucdp.uu.se/downloads/index.html#ged_global
ucdp_ged <- read_excel("GEDEvent_v25_1.xlsx")

# =======================================================
# FOOD SECURITY & PRICE DATA
# =======================================================
# FAO Food Price Index (Global)
# Source: https://www.fao.org/worldfoodsituation/foodpricesindex/en
fao_food_price_index <- read_excel("global_fao_food_price_index_nominal_real_jul.xlsx")

# FAO Consumer Price Index (Domestic)
# Source: https://www.fao.org/faostat/en/#data/CP
fao_consumer_prices <- read_csv("CPI_FAOSTAT_data_en_7-23-2025.csv")

# FAO Producer Prices (Domestic)
# Source: https://www.fao.org/faostat/en/#data/PP
# Note: This dataset requires reshaping from wide to long format
fao_producer_prices <- read_csv("Prices_E_All_Data.csv")

# =======================================================
# WORLD BANK DATA
# =======================================================
# World Development Indicators
# Source: https://databank.worldbank.org/source/world-development-indicators
wb_development_indicators <- read_excel("P_Data_Extract_From_World_Development_Indicators.xlsx")

# Worldwide Governance Indicators
# Source: https://databank.worldbank.org/source/worldwide-governance-indicators
wb_governance_indicators <- read_excel("P_Data_Extract_From_Worldwide_Governance_Indicators.xlsx")

# World Bank Commodity Prices (Pink Sheet)
# Source: https://thedocs.worldbank.org/en/doc/18675f1d1639c7a34d463f59263ba0a2-0050012025/world-bank-commodities-price-data-the-pink-sheet
wb_commodity_prices <- read_excel("CMO-Historical-Data-Annual.xlsx", sheet = "Sheet1")

# =======================================================
# DEMOCRACY & GOVERNANCE DATA
# =======================================================
# Varieties of Democracy (V-Dem) Dataset
# Source: https://www.v-dem.net/
vdem_full <- vdem
country_year_data <- vdem

# Extract democracy indices
democracy_indicators <- vdem %>% 
  select(
    country_name, 
    country_text_id, 
    year, 
    v2x_polyarchy  # Main electoral democracy index (0-1 scale)
  ) %>%
  rename(
    country = country_name,
    iso3c = country_text_id,
    democracy_index = v2x_polyarchy
  )

# Extract political transition data
political_transitions <- vdem %>% 
  select(
    country_name, 
    country_text_id, 
    year, 
    v2x_regime_amb,  # Regime classification (ambiguous cases)
    v2x_regime,      # Regime type (closed autocracy, electoral autocracy, etc.)
    v2xdd_dd         # Democratic breakdown/transition indicator
  ) %>%
  rename(
    country = country_name,
    iso3c = country_text_id,
    regime_ambiguous = v2x_regime_amb,
    regime_type = v2x_regime,
    democratic_transition = v2xdd_dd
  )

# =======================================================
# STANDARDIZING COUNTRY NAMES FOR MERGE
# =======================================================
# emdat = iso3c
# ucdp_prio = gwno_loc
# ucdp_ged = Country (or lat/long)
# fao_consumer_prices = Country
# wb_development_indicators = wb_ccode
# wb_governance_indicators = wb_ccode
# democracy_indicators = iso3c
# political_transitions = isoec

ucdp_prio <- ucdp_prio %>%
  separate_rows(gwno_loc, sep = ",") %>%
  mutate(gwno_loc = as.numeric(trimws(gwno_loc))) %>%
  mutate(iso3c = countrycode(gwno_loc, origin = "gwn", destination = "iso3c")) %>%
  filter(!is.na(iso3c))

ucdp_ged <- ucdp_ged %>%
  mutate(Country = ifelse(Country == "Yemen (North Yemen)", "Yemen", Country)) %>%
  mutate(iso3c = countrycode(Country, origin = "country.name", destination = "iso3c")) %>%
  filter(!is.na(iso3c))

fao_consumer_prices <- fao_consumer_prices %>%
  mutate(Country = ifelse(Country == "Netherlands Antilles (former)", "Netherlands", Country)) %>%
  mutate(iso3c = countrycode(Country, origin = "country.name", destination = "iso3c")) %>%
  filter(!is.na(iso3c))

fao_producer_prices <- fao_producer_prices %>%
  mutate(
    # Fix specific country names that don't match
    Country = case_when(
      Country == "Netherlands Antilles (former)" ~ "Netherlands",
      Country == "Belgium-Luxembourg" ~ "Belgium",  # or choose "Luxembourg" if preferred
      Country == "Czechoslovakia" ~ "Czech Republic",  # or choose "Slovakia" if preferred  
      Country == "Serbia and Montenegro" ~ "Serbia",  # or choose "Montenegro" if preferred
      TRUE ~ Country
    )
  ) %>%
  mutate(iso3c = countrycode(Country, origin = "country.name", destination = "iso3c")) %>%
  filter(!is.na(iso3c))


wb_development_indicators <- wb_development_indicators %>%
  mutate(iso3c = countrycode(Country, origin = "country.name", destination = "iso3c")) %>%
  filter(!is.na(iso3c))

wb_governance_indicators <- wb_governance_indicators %>%
  mutate(Country = case_when(
    Country == "Kosovo" ~ "Kosovo",
    Country %in% c("Netherlands Antilles", "Netherlands Antilles (former)") ~ "Netherlands",
    TRUE ~ Country
  )) %>%
  mutate(iso3c = countrycode(Country, origin = "country.name", destination = "iso3c")) %>%
  filter(!is.na(iso3c))


# =======================================================
# CREATING SHELL FOR MERGE
# =======================================================

# Get all objects in environment
all_objs <- ls()

# Loop over objects, check if data.frame and if it has 'year' column
for(obj_name in all_objs) {
  obj <- get(obj_name)
  
  if (is.data.frame(obj) && "year" %in% names(obj)) {
    yr_min <- min(obj$year, na.rm = TRUE)
    yr_max <- max(obj$year, na.rm = TRUE)
    cat(sprintf("%s: first year = %d, last year = %d\n", obj_name, yr_min, yr_max))
  }
}

# Get all unique iso3c observations across all data
all_iso3c <- lapply(all_objs, function(obj_name) {
  obj <- get(obj_name)
  if (is.data.frame(obj) && "iso3c" %in% names(obj)) {
    unique(obj$iso3c)
  } else {
    NULL
  }
}) %>%
  unlist() %>%
  unique() %>%
  na.omit() %>%
  sort()

# Create panel data

panel_shell <- expand.grid(
  iso3c = all_iso3c,
  year = 1960:2024
) %>%
  arrange(iso3c, year)


# Figuring out merge types

all_objs <- ls()

for (obj_name in all_objs) {
  obj <- get(obj_name)
  if (is.data.frame(obj)) {
    has_iso3c <- "iso3c" %in% names(obj)
    has_year <- "year" %in% names(obj)
    
    if (has_iso3c && has_year) {
      total_rows <- nrow(obj)
      unique_pairs <- nrow(unique(obj[, c("iso3c", "year")]))
      
      if (total_rows == unique_pairs) {
        uniqueness_msg <- "unique (iso3c, year) pairs"
      } else {
        uniqueness_msg <- sprintf("duplicates in (iso3c, year): %d duplicate rows",
                                  total_rows - unique_pairs)
      }
      
      cat(sprintf("%s: has iso3c and year — %s\n", obj_name, uniqueness_msg))
      
    } else if (has_iso3c || has_year) {
      cat(sprintf("%s: has ", obj_name))
      if (has_iso3c) cat("iso3c ")
      if (has_year) cat("year")
      cat(" but not both\n")
    }
  }
}

# =======================================================
# Collapsing data as needed
# =======================================================


# -------------------------------------------------------
# ucdp_ged: has iso3c and year — duplicates in (iso3c, year): 383872 duplicate rows

ucdp_ged_summary <- ucdp_ged %>%
  group_by(iso3c, year) %>%
  summarise(
    conflicteventcount = n(),
    fatalities_best = sum(best, na.rm = TRUE),
    .groups = "drop"
  )


# -------------------------------------------------------
#wb_governance_indicators: has iso3c and year — duplicates in (iso3c, year): 25 duplicate rows

wb_governance_collapsed <- wb_governance_indicators %>%
  group_by(iso3c, year) %>%
  summarise(
    # Average all numeric variables, but use first value if only one observation or all are NA
    across(where(is.numeric), ~ if(all(is.na(.x))) NA_real_ else if(length(na.omit(.x)) == 1) first(na.omit(.x)) else mean(.x, na.rm = TRUE)),
    .groups = 'drop'
  )

# -------------------------------------------------------
# emdat: has iso3c and year — duplicates in (iso3c, year): 8996 duplicate rows

emdat_collapsed_alt <- emdat %>%
  group_by(iso3c, year) %>%
  summarise(
    # Count total disasters
    total_disasters = n(),
    
    # Retain first value of Region
    Region = first(Region),
    
    # Sum the requested numeric variables
    aid_contribution = sum(`AID Contribution ('000 US$)`, na.rm = TRUE),
    magnitude_sum = sum(Magnitude, na.rm = TRUE),
    total_deaths = sum(`Total Deaths`, na.rm = TRUE),
    no_injured = sum(`No. Injured`, na.rm = TRUE),
    no_affected = sum(`No. Affected`, na.rm = TRUE),
    no_homeless = sum(`No. Homeless`, na.rm = TRUE),
    total_affected = sum(`Total Affected`, na.rm = TRUE),
    reconstruction_costs = sum(`Reconstruction Costs ('000 US$)`, na.rm = TRUE),
    reconstruction_costs_adj = sum(`Reconstruction Costs, Adjusted ('000 US$)`, na.rm = TRUE),
    insured_damage = sum(`Insured Damage ('000 US$)`, na.rm = TRUE),
    insured_damage_adj = sum(`Insured Damage, Adjusted ('000 US$)`, na.rm = TRUE),
    total_damage = sum(`Total Damage ('000 US$)`, na.rm = TRUE),
    total_damage_adj = sum(`Total Damage, Adjusted ('000 US$)`, na.rm = TRUE),
    cpi_sum = sum(CPI, na.rm = TRUE),
    
    .groups = 'drop'
  )

# Create dummy variables for Disaster Subgroup
subgroup_dummies <- emdat %>%
  select(iso3c, year, `Disaster Subgroup`) %>%
  filter(!is.na(`Disaster Subgroup`)) %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = `Disaster Subgroup`, 
              values_from = value, 
              values_fn = sum,
              values_fill = 0,
              names_prefix = "subgroup_") %>%
  group_by(iso3c, year) %>%
  summarise(across(starts_with("subgroup_"), sum), .groups = 'drop')

# Create dummy variables for Disaster Type
type_dummies <- emdat %>%
  select(iso3c, year, `Disaster Type`) %>%
  filter(!is.na(`Disaster Type`)) %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = `Disaster Type`, 
              values_from = value, 
              values_fn = sum,
              values_fill = 0,
              names_prefix = "type_") %>%
  group_by(iso3c, year) %>%
  summarise(across(starts_with("type_"), sum), .groups = 'drop')

# Create dummy variables for Disaster Subtype
subtype_dummies <- emdat %>%
  select(iso3c, year, `Disaster Subtype`) %>%
  filter(!is.na(`Disaster Subtype`)) %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = `Disaster Subtype`, 
              values_from = value, 
              values_fn = sum,
              values_fill = 0,
              names_prefix = "subtype_") %>%
  group_by(iso3c, year) %>%
  summarise(across(starts_with("subtype_"), sum), .groups = 'drop')

# Join all pieces together
emdat_final <- emdat_collapsed_alt %>%
  left_join(subgroup_dummies, by = c("iso3c", "year")) %>%
  left_join(type_dummies, by = c("iso3c", "year")) %>%
  left_join(subtype_dummies, by = c("iso3c", "year"))

# Replace NAs with 0s for the dummy variables
emdat_final <- emdat_final %>%
  mutate(across(starts_with(c("subgroup_", "type_", "subtype_")), ~ replace_na(.x, 0)))



# -------------------------------------------------------
# fao_consumer_prices: has iso3c and year — duplicates in (iso3c, year): 175553 duplicate rows
food_indices <- fao_consumer_prices %>%
  filter(Item == "Consumer Prices, Food Indices (2015 = 100)") %>%
  group_by(iso3c, year) %>%
  summarise(
    food_indices_sum = sum(Value, na.rm = TRUE),
    food_indices_avg = mean(Value, na.rm = TRUE),
    .groups = 'drop'
  )

general_indices <- fao_consumer_prices %>%
  filter(Item == "Consumer Prices, General Indices (2015 = 100)") %>%
  group_by(iso3c, year) %>%
  summarise(
    general_indices_sum = sum(Value, na.rm = TRUE),
    general_indices_avg = mean(Value, na.rm = TRUE),
    .groups = 'drop'
  )

food_inflation <- fao_consumer_prices %>%
  filter(Item == "Food price inflation") %>%
  group_by(iso3c, year) %>%
  summarise(
    food_inflation_sum = sum(Value, na.rm = TRUE),
    food_inflation_avg = mean(Value, na.rm = TRUE),
    .groups = 'drop'
  )


# -------------------------------------------------------
# fao_producer_prices: needs to go from wide year to long year

# First, convert all Y columns to character to avoid type conflicts
fao_producer_prices_clean <- fao_producer_prices %>%
  mutate(across(starts_with("Y"), as.character))

# Then reshape from wide to long format
fao_producer_long <- fao_producer_prices_clean %>%
  pivot_longer(cols = starts_with("Y"), 
               names_to = "year", 
               values_to = "Value") %>%
  mutate(
    year = as.numeric(gsub("Y", "", year)),  # Remove "Y" and convert to numeric
    Value = as.numeric(Value)  # Convert Value to numeric (NAs will be created for non-numeric values)
  ) %>%
  filter(!is.na(year))  # Drop rows where year is missing


# Create index dataframe (Producer Price Index)
producer_index <- fao_producer_long %>%
  filter(Element == "Producer Price Index (2014-2016 = 100)") %>%
  group_by(iso3c, year) %>%
  summarise(
    producer_index_sum = sum(Value, na.rm = TRUE),
    producer_index_avg = mean(Value, na.rm = TRUE),
    .groups = 'drop'
  )

# Create USD dataframe (Producer Price in USD)
producer_usd <- fao_producer_long %>%
  filter(Element == "Producer Price (USD/tonne)") %>%
  group_by(iso3c, year) %>%
  summarise(
    producer_usd_sum = sum(Value, na.rm = TRUE),
    producer_usd_avg = mean(Value, na.rm = TRUE),
    .groups = 'drop'
  )


# -------------------------------------------------------
# ucdp_prio: has iso3c and year — duplicates in (iso3c, year): 834 duplicate rows

# Collapse ucdp_prio data by iso3c and year
ucdp_collapsed <- ucdp_prio %>%
  group_by(iso3c, year) %>%
  summarise(
    # Retain first values
    conflict_id = first(conflict_id),
    side_a = first(side_a),
    side_b = first(side_b),
    side_a_id = first(side_a_id),
    side_b_id = first(side_b_id),
    
    # Average intensity level
    intensity_level_avg = mean(intensity_level, na.rm = TRUE),
    
    # Sum cumulative intensity
    cumulative_intensity_sum = sum(cumulative_intensity, na.rm = TRUE),
    
    .groups = 'drop'
  )


# Create dummy variables for type_of_conflict
conflict_type_dummies <- ucdp_prio %>%
  select(iso3c, year, type_of_conflict) %>%
  filter(!is.na(type_of_conflict)) %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = type_of_conflict, 
              values_from = value, 
              values_fn = sum,
              values_fill = 0,
              names_prefix = "conflict_type_") %>%
  group_by(iso3c, year) %>%
  summarise(across(starts_with("conflict_type_"), sum), .groups = 'drop')

# Create dummy variables for incompatibility
incompatibility_dummies <- ucdp_prio %>%
  select(iso3c, year, incompatibility) %>%
  filter(!is.na(incompatibility)) %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = incompatibility, 
              values_from = value, 
              values_fn = sum,
              values_fill = 0,
              names_prefix = "incompatibility_") %>%
  group_by(iso3c, year) %>%
  summarise(across(starts_with("incompatibility_"), sum), .groups = 'drop')

# Join all pieces together
ucdp_final <- ucdp_collapsed %>%
  left_join(conflict_type_dummies, by = c("iso3c", "year")) %>%
  left_join(incompatibility_dummies, by = c("iso3c", "year"))

# Replace NAs with 0s for the dummy variables
ucdp_final <- ucdp_final %>%
  mutate(across(starts_with(c("conflict_type_", "incompatibility_")), ~ replace_na(.x, 0)))



# =======================================================
# MERGING DATA
# =======================================================
# Merge onto panel_shell by year (and/or) iso3c

# BY YEAR
# fao_food_price_index: has year but not both
# wb_commodity_prices: has year but not both

# BY ISO3C YEAE
#emdat_final - collapsed
#food_indices  - collapsed
#food_inflation  - collapsed
#general_indices - collapsed
#producer_index - collapsed
#producer_usd - collapsed
#wb_governance_collapsed - collapsed
# democracy_indicators: has iso3c and year — unique (iso3c, year) pairs
# political_transitions: has iso3c and year — unique (iso3c, year) pairs
# wb_development_indicators: has iso3c and year — unique (iso3c, year) pairs


# Use the existing panel_shell and merge all datasets into it
panel_data <- panel_shell %>%
  # Merge iso3c + year datasets
  left_join(democracy_indicators, by = c("iso3c", "year")) %>%
  left_join(political_transitions, by = c("iso3c", "year")) %>%
  left_join(wb_development_indicators, by = c("iso3c", "year")) %>%
  left_join(emdat_final, by = c("iso3c", "year")) %>%
  left_join(food_indices, by = c("iso3c", "year")) %>%
  left_join(ucdp_ged_summary, by = c("iso3c", "year")) %>%
  left_join(food_inflation, by = c("iso3c", "year")) %>%
  left_join(general_indices, by = c("iso3c", "year")) %>%
  left_join(producer_index, by = c("iso3c", "year")) %>%
  left_join(producer_usd, by = c("iso3c", "year")) %>%
  left_join(wb_governance_collapsed, by = c("iso3c", "year")) %>%
  left_join(ucdp_final, by = c("iso3c", "year")) %>%
  
  # Merge year-only datasets (these will repeat the same values for all countries in each year)
  left_join(fao_food_price_index, by = "year") %>%
  left_join(wb_commodity_prices, by = "year") %>%
  
  # Clean up duplicate country columns
  select(-country.x,-country.y) %>%
  group_by(iso3c) %>%
  mutate(Country = ifelse(is.na(Country), first(na.omit(Country)), Country)) %>%
  ungroup()

View(panel_data)
# =======================================================
# CLEANING
# =======================================================

# Check all variable names
colnames(panel_data)

# Clean variable names in panel_data
panel_data <- panel_data %>%
  select(-`Time Code`, -wb_ccode) %>%  # Delete these variables
  # Replace ".." with NA across all columns
  mutate(across(everything(), ~ ifelse(.x == "..", NA, .x))) %>%
  # Reorder columns to put Country, iso3c, year first
  select(Country, iso3c, year, Region, everything()) %>%
  rename(
    # Keep basic identifiers as is
    # iso3c, year, Country, Region - keep as is
    
    
    # World Bank Development Indicators - Food & Agriculture
    food_exports_pct = `Food exports (% of merchandise exports) [TX.VAL.FOOD.ZS.UN]`,
    food_imports_pct = `Food imports (% of merchandise imports) [TM.VAL.FOOD.ZS.UN]`,
    food_production_index = `Food production index (2014-2016 = 100) [AG.PRD.FOOD.XD]`,
    food_beverage_tobacco_pct = `Food, beverages and tobacco (% of value added in manufacturing) [NV.MNF.FBTO.ZS.UN]`,
    undernourishment_pct = `Prevalence of undernourishment (% of population) [SN.ITK.DEFC.ZS]`,
    
    # Economic indicators
    gdp_per_capita_ppp = `GDP per capita, PPP (current international $) [NY.GDP.PCAP.PP.CD]`,
    gdp_per_capita_growth = `GDP per capita growth (annual %) [NY.GDP.PCAP.KD.ZG]`,
    gdp_growth = `GDP growth (annual %) [NY.GDP.MKTP.KD.ZG]`,
    trade_pct_gdp = `Trade (% of GDP) [NE.TRD.GNFS.ZS]`,
    inflation_cpi = `Inflation, consumer prices (annual %) [FP.CPI.TOTL.ZG]`,
    final_consumption_pct_gdp = `Final consumption expenditure (% of GDP) [NE.CON.TOTL.ZS]`,
    
    # Population indicators
    population_density = `Population density (people per sq. km of land area) [EN.POP.DNST]`,
    population_growth = `Population growth (annual %) [SP.POP.GROW]`,
    population_urban_1mil_pct = `Population in urban agglomerations of more than 1 million (% of total population) [EN.URB.MCTY.TL.ZS]`,
    population_total = `Population, total [SP.POP.TOTL]`,
    population_0_14 = `Population ages 0-14, total [SP.POP.0014.TO]`,
    
    # Sectoral indicators
    agriculture_value_added_pct = `Agriculture, forestry, and fishing, value added (% of GDP) [NV.AGR.TOTL.ZS]`,
    employment_agriculture_pct = `Employment in agriculture (% of total employment) (modeled ILO estimate) [SL.AGR.EMPL.ZS]`,
    employment_population_ratio = `Employment to population ratio, 15+, total (%) (national estimate) [SL.EMP.TOTL.SP.NE.ZS]`,
    education_bachelor_female_pct = `Educational attainment, at least Bachelor's or equivalent, population 25+, female (%) (cumulative) [SE.TER.CUAT.BA.FE.ZS]`,
    
    # Health indicators
    health_expenditure_pct_gdp = `Current health expenditure (% of GDP) [SH.XPD.CHEX.GD.ZS]`,
    govt_health_expenditure_pct_gdp = `Domestic general government health expenditure (% of GDP) [SH.XPD.GHED.GD.ZS]`,
    govt_health_expenditure_pct_govt = `Domestic general government health expenditure (% of general government expenditure) [SH.XPD.GHED.GE.ZS]`,
    
    # Investment and capital
    fdi_net_bop = `Foreign direct investment, net (BoP, current US$) [BN.KLT.DINV.CD]`,
    fdi_net_pct_gdp = `Foreign direct investment, net inflows (% of GDP) [BX.KLT.DINV.WD.GD.ZS]`,
    gross_capital_formation = `Gross capital formation (current US$) [NE.GDI.TOTL.CD]`,
    
    # Social protection
    poverty_headcount_societal = `Poverty headcount ratio at societal poverty line (% of population) [SI.POV.SOPO]`,
    social_safety_net_coverage = `Coverage of social safety net programs (% of population) [per_sa_allsa.cov_pop_tot]`,
    social_protection_adequacy = `Adequacy of social protection and labor programs (% of total welfare of beneficiary households) [per_allsp.adq_pop_tot]`,
    cpia_social_protection = `CPIA social protection rating (1=low to 6=high) [IQ.CPA.PROT.XQ]`,
    
    # Natural resources
    coal_rents_pct_gdp = `Coal rents (% of GDP) [NY.GDP.COAL.RT.ZS]`,
    oil_rents_pct_gdp = `Oil rents (% of GDP) [NY.GDP.PETR.RT.ZS]`,
    mineral_rents_pct_gdp = `Mineral rents (% of GDP) [NY.GDP.MINR.RT.ZS]`,
    natural_resources_rents_pct_gdp = `Total natural resources rents (% of GDP) [NY.GDP.TOTL.RT.ZS]`,
    natural_resources_depletion_pct_gni = `Adjusted savings: natural resources depletion (% of GNI) [NY.ADJ.DRES.GN.ZS]`,
    
    # Land and environment
    arable_land_pct = `Arable land (% of land area) [AG.LND.ARBL.ZS]`,
    agricultural_land_pct = `Agricultural land (% of land area) [AG.LND.AGRI.ZS]`,
    freshwater_withdrawals_pct = `Annual freshwater withdrawals, total (% of internal resources) [ER.H2O.FWTL.ZS]`,
    
    # Displacement
    forcibly_displaced_people = `Forcibly displaced people [SM.POP.FDIP]`,
    
    # Governance indicators (simplified names)
    corruption_control_est = `Control of Corruption: Estimate [CC.EST]`,
    corruption_control_rank = `Control of Corruption: Percentile Rank [CC.PER.RNK]`,
    corruption_control_se = `Control of Corruption: Standard Error [CC.STD.ERR]`,
    govt_effectiveness_est = `Government Effectiveness: Estimate [GE.EST]`,
    govt_effectiveness_rank = `Government Effectiveness: Percentile Rank [GE.PER.RNK]`,
    govt_effectiveness_se = `Government Effectiveness: Standard Error [GE.STD.ERR]`,
    political_stability_est = `Political Stability and Absence of Violence/Terrorism: Estimate [PV.EST]`,
    political_stability_rank = `Political Stability and Absence of Violence/Terrorism: Percentile Rank [PV.PER.RNK]`,
    political_stability_rank_upper = `Political Stability and Absence of Violence/Terrorism: Percentile Rank, Upper Bound of 90% Confidence Interval [PV.PER.RNK.UPPER]`,
    political_stability_se = `Political Stability and Absence of Violence/Terrorism: Standard Error [PV.STD.ERR]`,
    regulatory_quality_est = `Regulatory Quality: Estimate [RQ.EST]`,
    regulatory_quality_rank = `Regulatory Quality: Percentile Rank [RQ.PER.RNK]`,
    regulatory_quality_rank_lower = `Regulatory Quality: Percentile Rank, Lower Bound of 90% Confidence Interval [RQ.PER.RNK.LOWER]`,
    regulatory_quality_rank_upper = `Regulatory Quality: Percentile Rank, Upper Bound of 90% Confidence Interval [RQ.PER.RNK.UPPER]`,
    regulatory_quality_se = `Regulatory Quality: Standard Error [RQ.STD.ERR]`,
    rule_of_law_est = `Rule of Law: Estimate [RL.EST]`,
    rule_of_law_rank = `Rule of Law: Percentile Rank [RL.PER.RNK]`,
    rule_of_law_rank_lower = `Rule of Law: Percentile Rank, Lower Bound of 90% Confidence Interval [RL.PER.RNK.LOWER]`,
    rule_of_law_rank_upper = `Rule of Law: Percentile Rank, Upper Bound of 90% Confidence Interval [RL.PER.RNK.UPPER]`,
    rule_of_law_se = `Rule of Law: Standard Error [RL.STD.ERR]`,
    voice_accountability_est = `Voice and Accountability: Estimate [VA.EST]`,
    voice_accountability_rank = `Voice and Accountability: Percentile Rank [VA.PER.RNK]`,
    voice_accountability_rank_lower = `Voice and Accountability: Percentile Rank, Lower Bound of 90% Confidence Interval [VA.PER.RNK.LOWER]`,
    voice_accountability_rank_upper = `Voice and Accountability: Percentile Rank, Upper Bound of 90% Confidence Interval [VA.PER.RNK.UPPER]`,
    voice_accountability_se = `Voice and Accountability: Standard Error [VA.STD.ERR]`,
    
    # Disaster variables - keep most as is, just clean spaces
    disaster_subgroup_hydrological = subgroup_Hydrological,
    disaster_subgroup_geophysical = subgroup_Geophysical,
    disaster_subgroup_meteorological = subgroup_Meteorological,
    disaster_subgroup_climatological = subgroup_Climatological,
    disaster_type_flood = type_Flood,
    disaster_type_earthquake = type_Earthquake,
    disaster_type_storm = type_Storm,
    disaster_type_mass_movement_wet = `type_Mass movement (wet)`,
    disaster_type_extreme_temperature = `type_Extreme temperature`,
    disaster_type_drought = type_Drought,
    
    # Keep disaster subtypes but clean the problematic ones
    disaster_subtype_riverine_flood = `subtype_Riverine flood`,
    disaster_subtype_ground_movement = `subtype_Ground movement`,
    disaster_subtype_flood_general = `subtype_Flood (General)`,
    disaster_subtype_tropical_cyclone = `subtype_Tropical cyclone`,
    disaster_subtype_storm_general = `subtype_Storm (General)`,
    disaster_subtype_tornado = subtype_Tornado,
    disaster_subtype_landslide_wet = `subtype_Landslide (wet)`,
    disaster_subtype_cold_wave = `subtype_Cold wave`,
    disaster_subtype_avalanche_wet = `subtype_Avalanche (wet)`,
    disaster_subtype_heat_wave = `subtype_Heat wave`,
    disaster_subtype_tsunami = subtype_Tsunami,
    disaster_subtype_coastal_flood = `subtype_Coastal flood`,
    disaster_subtype_severe_weather = `subtype_Severe weather`,
    disaster_subtype_flash_flood = `subtype_Flash flood`,
    disaster_subtype_blizzard_winter_storm = `subtype_Blizzard/Winter storm`,
    disaster_subtype_drought = subtype_Drought,
    disaster_subtype_hail = subtype_Hail,
    disaster_subtype_lightning_thunderstorms = `subtype_Lightning/Thunderstorms`,
    disaster_subtype_mudslide = subtype_Mudslide,
    disaster_subtype_sand_dust_storm = `subtype_Sand/Dust storm`,
    disaster_subtype_severe_winter_conditions = `subtype_Severe winter conditions`,
    disaster_subtype_extratropical_storm = `subtype_Extra-tropical storm`,
    disaster_subtype_sudden_subsidence_wet = `subtype_Sudden Subsidence (wet)`,
    disaster_subtype_rockfall_wet = `subtype_Rockfall (wet)`,
    disaster_subtype_storm_surge = `subtype_Storm surge`,
    disaster_subtype_derecho = subtype_Derecho,
    
    # Conflict variables - clean the numeric ones
    conflict_type_interstate = conflict_type_1,
    conflict_type_internal = conflict_type_2,
    conflict_type_internationalized_internal = conflict_type_3,
    conflict_type_extrasystemic = conflict_type_4,
    incompatibility_territory = incompatibility_1,
    incompatibility_government = incompatibility_2,
    incompatibility_territory_government = incompatibility_3,
    
    # Commodity prices - clean the complex ones
    nominal_index = Nominal,
    real_index = Real,
    crude_oil_average = `Crude oil, average`,
    crude_oil_brent = `Crude oil, Brent`,
    crude_oil_dubai = `Crude oil, Dubai`,
    crude_oil_wti = `Crude oil, WTI`,
    coal_australian = `Coal, Australian`,
    coal_south_african = `Coal, South Afican`,
    natural_gas_us = `Natural gas, US`,
    natural_gas_europe = `Natural gas, Europe`,
    lng_japan = `Liquefied natural gas, Japan`,
    natural_gas_index = `Natural gas index`,
    coffee_arabica = `Coffee, Arabica`,
    coffee_robusta = `Coffee, Robusta`,
    tea_avg_3_auctions = `Tea, avg 3 auctions`,
    tea_colombo = `Tea, Colombo`,
    tea_kolkata = `Tea, Kolkata`,
    tea_mombasa = `Tea, Mombasa`,
    coconut_oil = `Coconut oil`,
    fish_meal = `Fish meal`,
    groundnut_oil = `Groundnut oil`,
    palm_oil = `Palm oil`,
    palm_kernel_oil = `Palm kernel oil`,
    soybean_oil = `Soybean oil`,
    soybean_meal = `Soybean meal`,
    rice_thai_5pct = `Rice, Thai 5%`,
    rice_thai_25pct = `Rice, Thai 25%`,
    rice_thai_a1 = `Rice, Thai A.1`,
    rice_vietnamese_5pct = `Rice, Viet Namese 5%`,
    wheat_us_srw = `Wheat, US SRW`,
    wheat_us_hrw = `Wheat, US HRW`,
    banana_europe = `Banana, Europe`,
    banana_us = `Banana, US`,
    shrimps_mexican = `Shrimps, Mexican`,
    sugar_eu = `Sugar, EU`,
    sugar_us = `Sugar, US`,
    sugar_world = `Sugar, world`,
    tobacco_us_import_uv = `Tobacco, US import u.v.`,
    logs_cameroon = `Logs, Cameroon`,
    logs_malaysian = `Logs, Malaysian`,
    sawnwood_cameroon = `Sawnwood, Cameroon`,
    sawnwood_malaysian = `Sawnwood, Malaysian`,
    cotton_a_index = `Cotton, A Index`,
    rubber_tsr20 = `Rubber, TSR20 **`,
    rubber_rss3 = `Rubber, RSS3`,
    phosphate_rock = `Phosphate rock`,
    potassium_chloride = `Potassium chloride`,
    iron_ore_cfr_spot = `Iron ore, cfr spot`,
    muv_index = `MUV Index`
  )

# Check the cleaned names
print(colnames(panel_data))


# =======================================================
# REPLACE MISSING WITH 0
# =======================================================
# Define the variables to replace missing values with 0
disaster_vars <- c(
  "conflicteventcount", "fatalities_best","total_disasters", "aid_contribution", "magnitude_sum", "total_deaths", 
  "no_injured", "no_affected", "no_homeless", "total_affected", 
  "reconstruction_costs", "reconstruction_costs_adj", "insured_damage", 
  "insured_damage_adj", "total_damage", "total_damage_adj", "cpi_sum",
  "disaster_subgroup_hydrological", "disaster_subgroup_geophysical", 
  "disaster_subgroup_meteorological", "disaster_subgroup_climatological",
  "disaster_type_flood", "disaster_type_earthquake", "disaster_type_storm", 
  "disaster_type_mass_movement_wet", "disaster_type_extreme_temperature", 
  "disaster_type_drought", "disaster_subtype_riverine_flood", 
  "disaster_subtype_ground_movement", "disaster_subtype_flood_general", 
  "disaster_subtype_tropical_cyclone", "disaster_subtype_storm_general", 
  "disaster_subtype_tornado", "disaster_subtype_landslide_wet", 
  "disaster_subtype_cold_wave", "disaster_subtype_avalanche_wet", 
  "disaster_subtype_heat_wave", "disaster_subtype_tsunami", 
  "disaster_subtype_coastal_flood", "disaster_subtype_severe_weather", 
  "disaster_subtype_flash_flood", "disaster_subtype_blizzard_winter_storm", 
  "disaster_subtype_drought", "disaster_subtype_hail", 
  "disaster_subtype_lightning_thunderstorms", "disaster_subtype_mudslide", 
  "disaster_subtype_sand_dust_storm", "disaster_subtype_severe_winter_conditions", 
  "disaster_subtype_extratropical_storm", "disaster_subtype_sudden_subsidence_wet", 
  "disaster_subtype_rockfall_wet", "disaster_subtype_storm_surge", 
  "disaster_subtype_derecho", "intensity_level_avg", "cumulative_intensity_sum",
  "conflict_type_internationalized_internal", "conflict_type_internal",
  "conflict_type_extrasystemic", "conflict_type_interstate",
  "incompatibility_territory", "incompatibility_government",
  "incompatibility_territory_government"
)

# Replace missing values with 0 for the specified variables
panel_data <- panel_data %>%
  mutate(across(all_of(disaster_vars), ~ replace_na(.x, 0)))



# =======================================================
# LOGS/SKEWNESS
# =======================================================

# Function to calculate skewness for numeric variables
calculate_skewness <- function(x) {
  if(is.numeric(x) && sum(!is.na(x)) > 0) {
    return(skewness(x, na.rm = TRUE))
  } else {
    return(NA)
  }
}

# Calculate skewness for all numeric variables
skewness_results <- panel_data %>%
  select(where(is.numeric)) %>%
  summarise(across(everything(), calculate_skewness)) %>%
  gather(variable, skewness_value) %>%
  arrange(desc(abs(skewness_value)))

# Add interpretation of skewness
skewness_results <- skewness_results %>%
  mutate(
    skewness_interpretation = case_when(
      is.na(skewness_value) ~ "No data",
      abs(skewness_value) < 0.5 ~ "Approximately symmetric",
      abs(skewness_value) >= 0.5 & abs(skewness_value) < 1 ~ "Moderately skewed",
      abs(skewness_value) >= 1 & abs(skewness_value) < 2 ~ "Highly skewed",
      abs(skewness_value) >= 2 ~ "Extremely skewed"
    ),
    log_recommended = abs(skewness_value) >= 1 & skewness_value > 0  # Only for positive skew >= 1
  )

# Show results
cat("Skewness Analysis for Numeric Variables\n")
View(skewness_results)

# List of highly skewed and extremely skewed variables to log transform
skewed_vars <- c(
  "conflicteventcount", "fatalities_best", "conflict_type_internal", "disaster_subtype_cold_wave", "population_0_14", "disaster_subtype_riverine_flood", 
  "disaster_type_mass_movement_wet", "disaster_subtype_ground_movement", "disaster_subgroup_meteorological", 
  "disaster_subgroup_geophysical", "disaster_type_earthquake", "disaster_subtype_flood_general", 
  "disaster_subtype_heat_wave", "population_density", "disaster_subtype_flash_flood", "conflict_type_extrasystemic", 
  "freshwater_withdrawals_pct", "cpi_sum", "disaster_subtype_tropical_cyclone", "incompatibility_territory", 
  "aid_contribution", "total_disasters", "disaster_type_flood", "conflict_type_internationalized_internal", 
  "disaster_type_extreme_temperature", "disaster_subgroup_hydrological", "food_production_index", 
  "cumulative_intensity_sum", "disaster_subgroup_climatological", "disaster_type_drought", 
  "disaster_subtype_drought", "natural_gas_europe", "incompatibility_government", "sugar_us", 
  "coal_australian", "voice_accountability_se", "sugar_world", "intensity_level_avg", 
  "trade_pct_gdp", "rule_of_law_se", "corruption_control_se", "Orange", "govt_effectiveness_se", 
  "democratic_transition", "potassium_chloride", "TSP", "Nickel", "producer_usd_sum", 
  "conflict_id", "political_stability_se", "regulatory_quality_se", "coal_south_african", 
  "gdp_growth", "Cocoa", "phosphate_rock", "coffee_robusta", "population_urban_1mil_pct", 
  "gdp_per_capita_growth", "soybean_meal", "Urea", "coffee_arabica", "rice_thai_5pct", 
  "tobacco_us_import_uv", "natural_gas_index", "arable_land_pct", "iron_ore_cfr_spot", 
  "natural_gas_us", "rubber_tsr20", "rubber_rss3", "Zinc", "tea_avg_3_auctions", 
  "agriculture_value_added_pct", "food_exports_pct", "Silver", "banana_us", "food_imports_pct", 
  "Gold", "tea_kolkata", "tea_mombasa", "logs_malaysian", "Platinum",
  # Adding the variables from the original extremely skewed list that weren't in the new document
  "no_injured", "no_homeless", "disaster_subtype_rockfall_wet", "producer_index_sum", 
  "producer_index_avg", "food_inflation_sum", "food_inflation_avg", "food_beverage_tobacco_pct", 
  "inflation_cpi", "disaster_subtype_derecho", "general_indices_avg", "food_indices_avg", 
  "general_indices_sum", "food_indices_sum", "insured_damage_adj", "disaster_subtype_storm_surge", 
  "insured_damage", "disaster_subtype_severe_weather", "disaster_subtype_sand_dust_storm", 
  "total_damage", "total_damage_adj", "no_affected", "total_affected", "disaster_subtype_tornado", 
  "reconstruction_costs", "reconstruction_costs_adj", "incompatibility_territory_government", 
  "disaster_subtype_hail", "disaster_subtype_lightning_thunderstorms", "disaster_subtype_tsunami", 
  "conflict_type_interstate", "disaster_subtype_blizzard_winter_storm", "magnitude_sum", 
  "producer_usd_avg", "disaster_subtype_coastal_flood", "disaster_subtype_mudslide", 
  "disaster_subtype_storm_general", "disaster_subtype_avalanche_wet", "disaster_subtype_severe_winter_conditions", 
  "disaster_subtype_extratropical_storm", "gross_capital_formation", "disaster_subtype_landslide_wet", 
  "population_total"
)

# Remove duplicates
skewed_vars <- unique(skewed_vars)

# Log transform the skewed variables (add 0.0000001 to avoid log(0))
panel_data <- panel_data %>%
  mutate(across(all_of(skewed_vars), ~ log(.x + 0.0000001), .names = "log_{.col}"))



# =======================================================
# Check completeness by year and country groups and drop small
# =======================================================

# Get all numeric variable names (excluding year since we group by it)
numeric_vars <- panel_data %>% select(where(is.numeric), -year) %>% colnames()

# Check completeness by country (across ALL numeric variables except year)
country_completeness <- panel_data %>%
  group_by(iso3c) %>%
  summarise(
    years = n(),
    total_numeric_cells = years * length(numeric_vars),
    missing_cells = sum(is.na(c_across(all_of(numeric_vars)))),
    missing_pct = round((missing_cells / total_numeric_cells) * 100, 1),
    .groups = 'drop'
  ) %>%
  arrange(desc(missing_pct))

# Check completeness by year (across ALL numeric variables except year)  
year_completeness <- panel_data %>%
  group_by(year) %>%
  summarise(
    countries = n(),
    total_numeric_cells = countries * length(numeric_vars),
    missing_cells = sum(is.na(c_across(all_of(numeric_vars)))),
    missing_pct = round((missing_cells / total_numeric_cells) * 100, 1),
    .groups = 'drop'
  ) %>%
  arrange(desc(missing_pct))

# Show top 10 most missing for each
cat("TOP 10 COUNTRIES WITH MOST MISSING DATA:\n")
print(head(country_completeness, 10))

cat("\nTOP 10 YEARS WITH MOST MISSING DATA:\n") 
print(head(year_completeness, 10))


# =======================================================
# VOLATILITY VARIABLES
# =======================================================

panel_data <- panel_data %>%
  arrange(iso3c, year) %>%
  group_by(iso3c) %>%
  mutate(
    # Food price indices volatility measures
    food_indices_volatility_3yr = rollapply(food_indices_avg, width=3, FUN=sd, fill=NA, align="right"),
    food_indices_volatility_5yr = rollapply(food_indices_avg, width=5, FUN=sd, fill=NA, align="right"),
    
    # Food inflation volatility measures
    food_inflation_volatility_3yr = rollapply(food_inflation_avg, width=3, FUN=sd, fill=NA, align="right"),
    food_inflation_volatility_5yr = rollapply(food_inflation_avg, width=5, FUN=sd, fill=NA, align="right")
  ) %>%
  ungroup()


# =======================================================
# EXPORT FINAL DATA
# =======================================================
rm(list = setdiff(ls(), "panel_data"))

write.xlsx(panel_data, "panel_data.xlsx", rowNames = FALSE)
