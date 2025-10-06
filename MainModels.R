# =============================================================================
# Climate Shocks, Food Prices & Battle Deaths
# Author: Elisa D'Amico
# Date Created: July 23, 2025
# Date Last Modified: August 24, 2025
# =============================================================================

# --------------------------
# Environment Setup
# --------------------------

# Clear 
rm(list = ls())

# Load Packages
library(AER)            # ivreg()
library(broom)          # tidy model output
library(countrycode)
library(corrplot)
library(data.table)     # fast data wrangling
library(did)
library(dplyr)
library(fastDummies)
library(fixest)         # for panel event study models & alternative IV estimation
library(furrr)          # parallel processing for heavy jobs
library(future)
library(ggplot2)
library(ivreg)          # phase 2 instrumental variables
library(lmtest)         # coeftest()
library(modelsummary)   # for table output
library(parallel)
library(plm)
library(randomForest)   # phase 1
library(readr)
library(readxl)
library(sandwich)       # robust standard errors
library(tidyr)
library(tidyverse)      # data manipulation + plotting
library(vdemdata)


#------------------------
  # 1. Data Loading & Preparation
  # --------------------------
# Loading in panel data

panel_data <- read_excel("panel_data.xlsx")


Inflation_data <- read_excel("Inflation-data.xlsx", 
                             sheet = "fcpi_a")

Inflation_data <- Inflation_data %>%
  pivot_longer(
    cols = `1970`:`2024`,
    names_to = "Year",
    values_to = "Inflation_Rate"
  )

Inflation_data <- Inflation_data %>%
  mutate(iso3c = countrycode(`Country Code`, origin = "wb", destination = "iso3c"))



colnames(panel_data)


# --------------------------
# 3. Phase 2: Causal Identification (Instrumental Variables)
# --------------------------
# - Formulate first-stage regression: climate shocks → food prices
# - Construct instruments based on top climate predictors identified in Phase 1
# - Formulate second-stage regression: predicted food prices → conflict outcomes
# - Estimate two-stage least squares (2SLS) models
# - Perform diagnostic tests for instrument relevance and validity
# - Interpret causal estimates and robustness

# --------------------------
# 1. Data Loading & Preparation
# --------------------------

# Create pdata.frame for panel analysis
pdata_full <- pdata.frame(panel_data, index = c("iso3c", "year"))

# --------------------------
# 2. Comprehensive Instrument Testing
# --------------------------

# Define all potential climate instruments from original analysis
expanded_climate_instruments <- c(
  # Disaster impacts (log versions)
  "log_aid_contribution",
  "log_magnitude_sum", 
  "total_deaths",
  "log_no_injured",
  "log_no_homeless", 
  "log_total_affected",
  "log_reconstruction_costs_adj",
  "log_insured_damage_adj",
  "log_total_damage_adj",
  
  # Disaster subgroups
  "log_disaster_subgroup_hydrological",
  "log_disaster_subgroup_geophysical", 
  "log_disaster_subgroup_meteorological",
  "log_disaster_subgroup_climatological",
  
  # Disaster types
  "log_disaster_type_flood",
  "log_disaster_type_earthquake",
  "disaster_type_storm",
  "log_disaster_type_mass_movement_wet",
  "log_disaster_type_extreme_temperature",
  "log_disaster_type_drought",
  
  # Disaster subtypes
  "log_disaster_subtype_riverine_flood",
  "log_disaster_subtype_ground_movement", 
  "log_disaster_subtype_flood_general",
  "log_disaster_subtype_tropical_cyclone",
  "log_disaster_subtype_storm_general",
  "log_disaster_subtype_tornado",
  "log_disaster_subtype_landslide_wet",
  "log_disaster_subtype_cold_wave",
  "log_disaster_subtype_avalanche_wet",
  "log_disaster_subtype_heat_wave",
  "log_disaster_subtype_tsunami", 
  "log_disaster_subtype_coastal_flood",
  "log_disaster_subtype_severe_weather",
  "log_disaster_subtype_flash_flood",
  "log_disaster_subtype_blizzard_winter_storm",
  "log_disaster_subtype_drought",
  "log_disaster_subtype_hail",
  "log_disaster_subtype_lightning_thunderstorms",
  "log_disaster_subtype_mudslide",
  "log_disaster_subtype_sand_dust_storm",
  "log_disaster_subtype_severe_winter_conditions",
  "log_disaster_subtype_extratropical_storm",
  "disaster_subtype_sudden_subsidence_wet",
  "log_disaster_subtype_rockfall_wet",
  "log_disaster_subtype_storm_surge",
  "log_disaster_subtype_derecho"
)

# Create lagged versions for top Random Forest performers
top_rf_instruments <- c(
  "log_disaster_subtype_derecho",
  "log_disaster_subtype_heat_wave", 
  "log_disaster_subtype_ground_movement",
  "log_disaster_subtype_riverine_flood",
  "log_disaster_subtype_flood_general"
)

# Add lagged versions
lagged_instruments <- c(
  paste0(top_rf_instruments, "_lag1"),
  paste0(top_rf_instruments, "_lag2")
)

# Combine all potential instruments
all_potential_instruments <- c(expanded_climate_instruments, lagged_instruments)

# Check which instruments are available in data
available_instruments <- all_potential_instruments[all_potential_instruments %in% names(pdata_full)]

cat("Total potential instruments:", length(all_potential_instruments), "\n")
cat("Available in panel_data:", length(available_instruments), "\n")

# --------------------------
# 3. Systematic Instrument Testing Function
# --------------------------

test_all_climate_instruments <- function(data, instruments) {
  results_list <- list()
  
  for(instrument in instruments) {
    if(instrument %in% names(data)) {
      cat("Testing:", instrument, "\n")
      
      formula_first <- as.formula(paste("food_inflation_volatility_3yr ~", instrument))
      
      tryCatch({
        first_stage <- plm(formula_first, 
                           data = data,
                           model = "within",      
                           effect = "individual")
        
        coef_summary <- summary(first_stage)
        
        results_list[[instrument]] <- data.frame(
          instrument = instrument,
          coefficient = coef_summary$coefficients[instrument, "Estimate"],
          std_error = coef_summary$coefficients[instrument, "Std. Error"],
          t_statistic = coef_summary$coefficients[instrument, "t-value"],
          p_value = coef_summary$coefficients[instrument, "Pr(>|t|)"],
          f_statistic = coef_summary$fstatistic$statistic,
          r_squared = coef_summary$r.squared[1],
          n_obs = nobs(first_stage)
        )
        
      }, error = function(e) {
        cat("Failed for instrument:", instrument, "Error:", e$message, "\n")
      })
    }
  }
  
  if(length(results_list) > 0) {
    final_results <- do.call(rbind, results_list)
    rownames(final_results) <- NULL
    return(final_results)
  } else {
    return(data.frame())
  }
}

# --------------------------
# 4. Run Comprehensive Instrument Testing
# --------------------------

# Test all available instruments
cat("Running comprehensive instrument testing...\n")
full_results <- test_all_climate_instruments(pdata_full, available_instruments)

# Sort by F-statistic (strongest first)
full_results <- full_results[order(full_results$f_statistic, decreasing = TRUE), ]

# Add instrument categories
full_results$category <- case_when(
  grepl("subtype.*flood", full_results$instrument) ~ "Floods",
  grepl("subtype.*storm|tornado|cyclone", full_results$instrument) ~ "Storms", 
  grepl("subtype.*heat|cold|temperature", full_results$instrument) ~ "Temperature",
  grepl("subtype.*drought", full_results$instrument) ~ "Drought",
  grepl("subtype", full_results$instrument) ~ "Other Disaster Subtypes",
  grepl("disaster_type", full_results$instrument) ~ "Disaster Types",
  grepl("disaster_subgroup", full_results$instrument) ~ "Disaster Subgroups",
  grepl("damage|costs|affected|injured|homeless|deaths", full_results$instrument) ~ "Impact Measures",
  TRUE ~ "Other"
)

# --------------------------
# 5. Results Analysis
# --------------------------

# Show top 20 instruments
cat("\n=== TOP 20 CLIMATE INSTRUMENTS ===\n")
print(head(full_results, 20))

# Category performance summary
category_performance <- full_results %>%
  group_by(category) %>%
  summarise(
    count = n(),
    mean_f = round(mean(f_statistic, na.rm = TRUE), 3),
    max_f = round(max(f_statistic, na.rm = TRUE), 3), 
    best_instrument = instrument[which.max(f_statistic)],
    n_above_5 = sum(f_statistic > 5, na.rm = TRUE),
    n_above_10 = sum(f_statistic > 10, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(max_f))

cat("\n=== INSTRUMENT PERFORMANCE BY CATEGORY ===\n")
print(category_performance)

# Identify strong instruments
strong_instruments <- full_results[full_results$f_statistic > 10, ]

if(nrow(strong_instruments) > 0) {
  cat("\n=== STRONG INSTRUMENTS (F > 10) ===\n")
  print(strong_instruments)
  
  # Use the strongest instrument for IV analysis
  best_instrument <- strong_instruments$instrument[1]
  best_f_stat <- strong_instruments$f_statistic[1]
  
} else {
  cat("\n=== NO INSTRUMENTS WITH F > 10 ===\n")
  decent_instruments <- full_results[full_results$f_statistic > 5, ]
  cat("Best available instruments (F > 5):\n")
  print(head(decent_instruments, 5))
  
  # Use the best available instrument
  best_instrument <- full_results$instrument[1]
  best_f_stat <- full_results$f_statistic[1]
}

cat("\nSelected instrument for IV analysis:", best_instrument, "\n")
cat("F-statistic:", round(best_f_stat, 2), "\n")

# --------------------------
# 6. First Stage Analysis
# --------------------------

# Run first stage with best instrument
first_stage_formula <- as.formula(paste("food_inflation_volatility_3yr ~", best_instrument))

first_stage <- plm(first_stage_formula,
                   data = pdata_full,
                   model = "within",
                   effect = "individual")

cat("\n=== FIRST STAGE RESULTS ===\n")
print(summary(first_stage))

#log_disaster_subtype_flood_general 
## F-statistic: 18.30 (well above the F > 10 threshold)
## t-statistic: 4.28 (highly statistically significant)
## Coefficient: 23.00 (large, economically meaningful effect)
## R-squared: 0.43% (explains meaningful variation)

# --------------------------
# 8. Second Stage IV Analysis
# --------------------------
# --------------------------
# Comprehensive Robustness Check: All Conflict Variables
# --------------------------

# Get all conflict-related variables
all_conflict_vars <- names(pdata_full)[grepl("conflict|incompatibility|intensity|fatalities", 
                                             names(pdata_full), ignore.case = TRUE)]

cat("\n=== TESTING ALL", length(all_conflict_vars), "CONFLICT VARIABLES ===\n")
print(all_conflict_vars)

# Function to test IV with each conflict outcome
test_iv_all_outcomes <- function(data, instrument, outcomes) {
  results_list <- list()
  
  for(outcome in outcomes) {
    if(outcome %in% names(data)) {
      cat("\nTesting outcome:", outcome, "\n")
      
      tryCatch({
        # Run IV regression
        iv_formula <- as.formula(paste(outcome, "~ food_inflation_volatility_3yr |", instrument))
        iv_model <- plm(iv_formula, data = data, model = "within", effect = "individual")
        
        # Extract results
        coef_val <- coef(iv_model)["food_inflation_volatility_3yr"]
        se_val <- sqrt(diag(vcov(iv_model)))["food_inflation_volatility_3yr"]
        t_stat <- coef_val / se_val
        p_val <- 2 * (1 - pnorm(abs(t_stat)))
        n_obs <- nobs(iv_model)
        
        # Store results
        results_list[[outcome]] <- data.frame(
          outcome = outcome,
          coefficient = coef_val,
          std_error = se_val,
          t_statistic = t_stat,
          p_value = p_val,
          significant_05 = p_val < 0.05,
          significant_10 = p_val < 0.10,
          n_obs = n_obs,
          stringsAsFactors = FALSE
        )
        
        cat("Coefficient:", round(coef_val, 6), 
            "(SE:", round(se_val, 6), 
            ", t:", round(t_stat, 2),
            ", p:", round(p_val, 3),
            ", n:", n_obs, ")\n")
        
        # Flag if significant
        if(p_val < 0.05) {
          cat("*** SIGNIFICANT AT 5% ***\n")
        } else if(p_val < 0.10) {
          cat("* SIGNIFICANT AT 10% *\n")
        }
        
      }, error = function(e) {
        cat("FAILED for", outcome, "- Error:", e$message, "\n")
        
        # Store failed result
        results_list[[outcome]] <- data.frame(
          outcome = outcome,
          coefficient = NA,
          std_error = NA,
          t_statistic = NA,
          p_value = NA,
          significant_05 = FALSE,
          significant_10 = FALSE,
          n_obs = NA,
          error = e$message,
          stringsAsFactors = FALSE
        )
      })
    } else {
      cat("Variable", outcome, "not found in data\n")
    }
  }
  
  # Combine results
  if(length(results_list) > 0) {
    final_results <- do.call(rbind, results_list)
    rownames(final_results) <- NULL
    return(final_results)
  } else {
    return(data.frame())
  }
}

# Run comprehensive test
cat("\nRunning comprehensive IV analysis with all conflict outcomes...\n")
comprehensive_iv_results <- test_iv_all_outcomes(pdata_full, best_instrument, all_conflict_vars)

# Sort by significance (p-value)
comprehensive_iv_results <- comprehensive_iv_results[order(comprehensive_iv_results$p_value, na.last = TRUE), ]

cat("\n=== SUMMARY: ALL CONFLICT OUTCOMES ===\n")
print(comprehensive_iv_results[, c("outcome", "coefficient", "std_error", "t_statistic", "p_value", "significant_05", "significant_10", "n_obs")])

# Identify significant results
significant_05 <- comprehensive_iv_results[comprehensive_iv_results$significant_05 == TRUE & !is.na(comprehensive_iv_results$significant_05), ]
significant_10 <- comprehensive_iv_results[comprehensive_iv_results$significant_10 == TRUE & !is.na(comprehensive_iv_results$significant_10), ]

if(nrow(significant_05) > 0) {
  cat("\n=== SIGNIFICANT RESULTS AT 5% LEVEL ===\n")
  print(significant_05[, c("outcome", "coefficient", "std_error", "p_value")])
} else {
  cat("\n=== NO RESULTS SIGNIFICANT AT 5% LEVEL ===\n")
}

if(nrow(significant_10) > 0) {
  cat("\n=== SIGNIFICANT RESULTS AT 10% LEVEL ===\n") 
  print(significant_10[, c("outcome", "coefficient", "std_error", "p_value")])
} else {
  cat("\n=== NO RESULTS SIGNIFICANT AT 10% LEVEL ===\n")
}

# Summary statistics
total_outcomes <- nrow(comprehensive_iv_results[!is.na(comprehensive_iv_results$p_value), ])
n_sig_05 <- sum(comprehensive_iv_results$significant_05, na.rm = TRUE)
n_sig_10 <- sum(comprehensive_iv_results$significant_10, na.rm = TRUE)

cat("\n=== OVERALL SUMMARY ===\n")
cat("Total conflict outcomes tested:", total_outcomes, "\n")
cat("Significant at 5%:", n_sig_05, "(", round(100*n_sig_05/total_outcomes, 1), "%)\n")
cat("Significant at 10%:", n_sig_10, "(", round(100*n_sig_10/total_outcomes, 1), "%)\n")
cat("Non-significant:", total_outcomes - n_sig_10, "(", round(100*(total_outcomes - n_sig_10)/total_outcomes, 1), "%)\n")

# Create category analysis
comprehensive_iv_results$category <- case_when(
  grepl("conflict.*count|conflict_id", comprehensive_iv_results$outcome) ~ "Conflict Incidence",
  grepl("fatalities", comprehensive_iv_results$outcome) ~ "Fatalities", 
  grepl("intensity", comprehensive_iv_results$outcome) ~ "Conflict Intensity",
  grepl("incompatibility", comprehensive_iv_results$outcome) ~ "Conflict Type",
  grepl("conflict_type", comprehensive_iv_results$outcome) ~ "Conflict Classification",
  grepl("log_", comprehensive_iv_results$outcome) ~ "Log Transformed",
  TRUE ~ "Other"
)

# Category summary
category_summary <- comprehensive_iv_results %>%
  filter(!is.na(p_value)) %>%
  group_by(category) %>%
  summarise(
    count = n(),
    mean_p_value = round(mean(p_value, na.rm = TRUE), 3),
    min_p_value = round(min(p_value, na.rm = TRUE), 3),
    n_sig_05 = sum(significant_05, na.rm = TRUE),
    n_sig_10 = sum(significant_10, na.rm = TRUE),
    best_outcome = outcome[which.min(p_value)],
    .groups = 'drop'
  ) %>%
  arrange(min_p_value)

cat("\n=== RESULTS BY CONFLICT CATEGORY ===\n")
print(category_summary)

# Effect size analysis for significant results
if(nrow(significant_10) > 0) {
  cat("\n=== EFFECT SIZE ANALYSIS (Significant Results) ===\n")
  
  for(i in 1:nrow(significant_10)) {
    outcome <- significant_10$outcome[i]
    coef <- significant_10$coefficient[i]
    
    cat("\n", outcome, ":\n")
    cat("  Coefficient:", round(coef, 6), "\n")
    
    # Provide economic interpretation based on variable type
    if(grepl("log_", outcome)) {
      pct_effect <- (exp(coef) - 1) * 100
      cat("  Economic interpretation: 1-unit increase in food volatility increases", 
          outcome, "by", round(pct_effect, 3), "%\n")
    } else if(grepl("fatalities", outcome)) {
      cat("  Economic interpretation: 1-unit increase in food volatility increases fatalities by", 
          round(coef, 3), "deaths\n")
    } else if(grepl("count", outcome)) {
      cat("  Economic interpretation: 1-unit increase in food volatility increases", 
          outcome, "by", round(coef, 3), "events\n")
    } else {
      cat("  Economic interpretation: 1-unit increase in food volatility changes", 
          outcome, "by", round(coef, 3), "units\n")
    }
  }
}


# --------------------------
# 4. Phase 3: Dynamic Effects (Event Study Design)
# --------------------------
# - Define treatment events: major climate shocks identified from Phase 1
# - Create event time variables relative to shock occurrence (e.g., months before/after)
# - Prepare panel dataset for event study framework
# - Estimate event study models with fixed effects controlling for confounders
# - Plot and interpret dynamic treatment effects on conflict intensity over time


# Create first flood treatment variable
first_flood_treatment <- as.data.frame(pdata_full) %>%
  # Subset to just the variables we need
  select(iso3c, year, disaster_subtype_flood_general) %>%
  # Remove observations where no flood occurred
  filter(disaster_subtype_flood_general > 0) %>%
  # Sort by country and year
  arrange(iso3c, year) %>%
  # Keep only the first flood observation per country
  group_by(iso3c) %>%
  slice(1) %>%
  ungroup() %>%
  # Drop the flood variable and rename year to flood_treat
  select(-disaster_subtype_flood_general) %>%
  rename(flood_treat = year)

did_data <- as.data.frame(pdata_full) %>%
  left_join(first_flood_treatment, by = "iso3c") %>%
  mutate(
    flood_treat = ifelse(is.na(flood_treat), 0, flood_treat),
    year = as.numeric(year),
    # Create numeric country ID
    iso3c = as.numeric(as.factor(iso3c))
  )

# --------------------------
# Sant'Anna-Callaway DiD for 4 Conflict Outcomes
# --------------------------

# Two outcomes
outcomes <- c("log_fatalities_best", "log_conflicteventcount")
titles <- c("Log Fatalities", "Log Conflict Events")

# Run analysis for each outcome
for(i in 1:length(outcomes)) {
  
  outcome <- outcomes[i]
  title <- titles[i]
  
  cat("Running DiD for:", outcome, "\n")
  
  set.seed(2025)
  
  # Sant'Anna-Callaway estimation
  sc_result <- att_gt(
    yname = outcome,
    tname = "year",
    idname = "iso3c",
    gname = "flood_treat",
   # xformla = ~food_inflation_avg,
    data = did_data,
    control_group = "notyettreated",
    #  est_method = "reg",
    bstrap = TRUE
    # ,biters = 500,
    # print_details = FALSE
  )
  
  # Get dynamic effects
  dynamic_att <- aggte(sc_result, type = "dynamic")
  
  p <- ggdid(dynamic_att) + 
    labs(title = "",
         x = "Time to Treatment",
         y = "Average Treatment Effect") +
    scale_color_manual(values = c("#fb9b06", "#CC00CC"), 
                       labels = c("Pre-Treatment", "Post-Treatment")) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5, family = "Times New Roman", color = "black"),
      axis.text.x = element_text(size = 6, angle = 45, hjust = 1, family = "Times New Roman"),
      axis.text.y = element_text(size = 8, family = "Times New Roman"),
      axis.title.x = element_text(size = 10, family = "Times New Roman"),
      axis.title.y = element_text(size = 10, family = "Times New Roman"),
      legend.text = element_text(family = "Times New Roman"),
      legend.title = element_text(family = "Times New Roman"),
      legend.position = "bottom"  # Add this line
    )
  
  print(p)
  print(summary(dynamic_att))
  cat("\n", paste(rep("=", 50), collapse = ""), "\n\n")
}


# Two outcomes
outcomes <- c("log_fatalities_best", "log_conflicteventcount")
titles <- c("Log Fatalities", "Log Conflict Events")

# Run analysis for each outcome
for(i in 1:length(outcomes)) {
  
  outcome <- outcomes[i]
  title <- titles[i]
  
  cat("Running DiD for:", outcome, "\n")
  
  set.seed(666)
  
  # Sant'Anna-Callaway estimation
  sc_result <- att_gt(
    yname = outcome,
    tname = "year",
    idname = "iso3c",
    gname = "flood_treat",
    xformla = ~food_inflation_avg,
    data = did_data,
    control_group = "notyettreated",
    #  est_method = "reg",
    bstrap = TRUE
    # ,biters = 500,
    # print_details = FALSE
  )
  
  # Get dynamic effects
  dynamic_att <- aggte(sc_result, type = "dynamic")
  
  p <- ggdid(dynamic_att) + 
    labs(title = "",
         x = "Time to Treatment",
         y = "Average Treatment Effect") +
    scale_color_manual(values = c("#fb9b06", "#CC00CC"), 
                       labels = c("Pre-Treatment", "Post-Treatment")) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5, family = "Times New Roman", color = "black"),
      axis.text.x = element_text(size = 6, angle = 45, hjust = 1, family = "Times New Roman"),
      axis.text.y = element_text(size = 8, family = "Times New Roman"),
      axis.title.x = element_text(size = 10, family = "Times New Roman"),
      axis.title.y = element_text(size = 10, family = "Times New Roman"),
      legend.text = element_text(family = "Times New Roman"),
      legend.title = element_text(family = "Times New Roman"),
      legend.position = "bottom"  # Add this line
    )
  
  print(p)
  print(summary(dynamic_att))
  cat("\n", paste(rep("=", 50), collapse = ""), "\n\n")
}






set.seed(2025)

# Run att_gt
sc_result <- att_gt(
  yname = "food_indices_volatility_5yr",
  tname = "year",
  idname = "iso3c",
  gname = "flood_treat",
  data = did_data,
  control_group = "notyettreated",
  bstrap = TRUE
)

# Aggregate into dynamic ATT
dynamic_att <- aggte(sc_result, type = "dynamic")

# Plot
p <- ggdid(dynamic_att) + 
  labs(title = "",
       x = "Time to Treatment",
       y = "Average Treatment Effect") +
  scale_color_manual(values = c("#fb9b06", "#CC00CC"), 
                     labels = c("Pre-Treatment", "Post-Treatment")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, 
                              family = "Times New Roman", color = "black"),
    axis.text.x = element_text(size = 6, angle = 45, hjust = 1, 
                               family = "Times New Roman"),
    axis.text.y = element_text(size = 8, family = "Times New Roman"),
    axis.title.x = element_text(size = 10, family = "Times New Roman"),
    axis.title.y = element_text(size = 10, family = "Times New Roman"),
    legend.text = element_text(family = "Times New Roman"),
    legend.title = element_text(family = "Times New Roman"),
    legend.position = "bottom"
  )

# Print results
print(p)
print(summary(dynamic_att))
cat("\n", paste(rep("=", 50), collapse = ""), "\n\n")


