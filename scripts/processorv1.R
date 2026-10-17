
# Load essential biostatistical and data management packages
library(tidyverse) # For data manipulation (dplyr, tidyr)
library(lubridate) # For precise date handling and DOW extraction
library(haven)     # To read INDEPTH Stata (.dta) files [Source 5 summary]
library(hrbrthemes)
library(readr)
library(gtsummary)

UTCI_daily <- read_csv("Data/UTCI_daily_sites_2009_2016.csv")


 


UTCI_daily <- UTCI_daily %>%
  mutate(
    # Convert dates to standard Date objects
    date = as.Date(date),
    
    # Subtract 273.15 from each metric to convert Kelvin to Celsius
    utci_min  = utci_min - 273.15,
    utci_mean = utci_mean - 273.15,
    utci_max  = utci_max - 273.15
  )


#################### CLIMATE



unique_sites <- unique(UTCI_daily$site_name)

# 3. Loop through each site, generate its graph, and save it
walk(unique_sites, function(current_site) {
  
  # Filter data for just this specific site and transform
  site_data <- UTCI_daily %>%
    filter(site_name == current_site) %>%
    mutate(date = as.Date(date)) %>%
    pivot_longer(
      cols = c(utci_min, utci_mean, utci_max),
      names_to = "index_type",
      values_to = "utci_value"
    ) %>%
    mutate(index_type = case_when(
      index_type == "utci_max"  ~ "Maximum UTCI",
      index_type == "utci_mean" ~ "Mean UTCI",
      index_type == "utci_min"  ~ "Minimum UTCI"
    ))
  
  # Build the site-specific plot
  p <- ggplot(site_data, aes(x = date, y = utci_value, color = index_type, group = index_type)) +
    
    # Visual layers: daily noise faded out, clear trend line in focus
    geom_line(alpha = 0.20, linewidth = 0.4) +
    geom_smooth(method = "gam", se = FALSE, linewidth = 1.3, alpha = 0.9) +
    
    # Distinct, high-contrast thermal colors
    scale_color_manual(
      values = c("Maximum UTCI" = "#d95f02", 
                 "Mean UTCI"    = "#7570b3", 
                 "Minimum UTCI" = "#1b9e77")
    ) +
    
    # Chronological X-axis formatting
    scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
    
    # Custom titles pulling the specific site name dynamically
    labs(
      title = paste("Universal Thermal Climate Index (UTCI) Trends:", current_site),
      subtitle = "Daily minimum, mean, and maximum ambient thermal stress indicators",
      x = "Timeline (Year)",
      y = "UTCI (°C)",
      color = "UTCI Metrics",
      caption = "Data Source: INDEPTH-iSHARE | Trend smoothing calculated via GAM"
    ) +
    
    # Professional, minimalist styling
    theme_ipsum(grid = "XY", axis_text_size = 11) +
    theme(
      legend.position = "top",
      legend.box.background = element_rect(color = "gray90", linewidth = 0.5),
      plot.title = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(color = "gray40", size = 11)
    )
  
  # Create a clean, safe filename (removes spaces/special characters)
  clean_filename <- paste0("utci_trend_", gsub("[[:space:]]|\\/|\\\\", "_", current_site), ".png")
  
  # Save the plot directly to disk
  ggsave(filename = clean_filename, plot = p, width = 10, height = 6, dpi = 300)
  
  # Print progress tracker message to your R console
  message("Saved graph for: ", current_site, " as ", clean_filename)
})







# 4. (Optional) Export directly to Microsoft Word for your manuscript
# library(flextable)
# utci_summary_table %>% as_flex_table() %>% flextable::save_as_docx(path = "utci_summary_table.docx")


# --- PHASE 1: LOAD AND CLEAN INDEPTH MICRODATA ---
# Use the file path corresponding to the ConsolidatedData2018 dataset
raw_data <- read_dta("Data/ConsolidatedData2018_analysis.dta") %>% 
  rename_all(str_to_lower) # Standardize variable names to lowercase

# Define West African Centre Prefixes to ensure Nanoro (BF021) is included
# even if its CountryId is miscoded in some tabulations [16, Dictionary]
wa_prefixes <- c("BF", "CI", "GH", "GM", "NG", "SN")

west_africa_mortality <- raw_data %>%
  # Filter by string prefix to capture all West African sites accurately
  filter(str_sub(centreid, 1, 2) %in% wa_prefixes) %>%
  # Isolate confirmed deaths (EventCode 7) [14, Roadmap]
  filter(eventcode == 7) %>%
  # Ensure the individual was a resident at the site on the day of death
  filter(residence == 1)

# --- PHASE 2: TEMPORAL & DEMOGRAPHIC ATTRIBUTION ---
processed_cases <- west_africa_mortality %>%
  mutate(
    # Convert character/double dates to R Date objects
    eventdate = as.Date(eventdate),
    dob = as.Date(dob),
    # Calculate exact Age at Death for stratified analysis (Objective 4) [Roadmap]
    age_at_death = as.numeric(difftime(eventdate, dob, units = "days")) / 365.25,
    # Extract markers for time-stratified matching
    year = year(eventdate),
    month = month(eventdate),
    dow = wday(eventdate, label = TRUE, abbr = FALSE),
    # Unique ID for each individual death event (The Stratum)
    stratum_id = row_number()
  ) %>%
  select(countryid,stratum_id, concat_individualid, centreid, locationid, 
         sex, age_at_death, eventdate, year, month, dow)




# Assuming your dataset is named 'health_data'
health_data <- health_data %>%
  mutate(
    # Create standard age groups based on INDEPTH research standards [2]
    age_group = case_when(
      age_at_death <= 5  ~ "Young Children (<=5)",
      age_at_death > 5 & age_at_death <= 18 ~ "School-age (5-18)",
      age_at_death > 18 & age_at_death <= 65 ~ "Working-age (18-65)",
      age_at_death > 65  ~ "Elderly (>65)",
      TRUE ~ "Unknown" # Handles potential NA or negative age errors
    ),
    # Convert to factor with specific order for modeling and plots
    age_group = factor(age_group, levels = c(
      "Young Children (<=5)", 
      "School-age (5-18)", 
      "Working-age (18-65)", 
      "Elderly (>65)"
    ))
  )

# Verify the distribution of your new age groups
table(health_data$age_group)





# Mapping derived from the INDEPTH HDSS Country ID Dictionary [Turn 12]
health_data <- health_data %>%
  mutate(
    country_name = case_when(
      countryid == 854 ~ "Burkina Faso",
      countryid == 231 ~ "Ethiopia",
      countryid == 270 ~ "The Gambia",
      countryid == 288 ~ "Ghana",
      countryid == 384 ~ "Côte d'Ivoire",
      countryid == 404 ~ "Kenya",
      countryid == 454 ~ "Malawi",
      countryid == 508 ~ "Mozambique",
      countryid == 566 ~ "Nigeria",
      countryid == 686 ~ "Senegal",
      countryid == 710 ~ "South Africa",
      countryid == 800 ~ "Uganda",
      countryid == 834 ~ "Tanzania",
      TRUE ~ "Unknown"
    )
  )

# --- STEP 2: EXPERT CORRECTION FOR DATA INTEGRITY ---
# Because Nanoro (BF021) is aggregated into Ethiopia (231) in this dataset,
# we must use the 'centreid' prefix to ensure records are correctly attributed [Turn 13, 81].
health_data <- health_data %>%
  mutate(
    country_name = if_else(str_sub(centreid, 1, 2) == "BF", "Burkina Faso", country_name)
  )

# Verify the conversion
health_data %>% 
  group_by(country_name, countryid) %>% 
  summarise(n = n()) %>% 
  print()

# --- PHASE 3: TIME-STRATIFIED CONTROL SELECTION ---
# Function to identify all referent days (Controls) in the same month/year
# that fall on the same day of the week as the death [25, Roadmap]
get_referent_days <- function(case_date) {
  # Find the boundaries of the month
  m_start <- floor_date(case_date, "month")
  m_end <- ceiling_date(case_date, "month") - days(1)
  
  # Generate all days in that month
  month_days <- seq(m_start, m_end, by = "1 day")
  
  # Select days matching the Case Day-of-Week (excluding the Case Day)
  referent_days <- month_days[wday(month_days) == wday(case_date) & month_days != case_date]
  return(referent_days)
}

# Expand dataset into Case-Crossover Matrix (1 Case : 3-4 Controls)
cc_matrix <- processed_cases %>%
  group_by(stratum_id) %>%
  do({
    case_row <- .
    control_dates <- get_referent_days(case_row$eventdate)
    
    # Create control rows (Outcome = 0) with identical demographics
    control_rows <- map_df(control_dates, ~ {
      case_row %>% mutate(eventdate = .x, outcome = 0)
    })
    
    # Define the case row (Outcome = 1)
    case_row <- case_row %>% mutate(outcome = 1)
    
    bind_rows(case_row, control_rows)
  }) %>%
  ungroup()

# --- PHASE 4: PRE-MERGE CLEANING ---
# Standardize date variable for joining with ERA5-Land UTCI metrics
final_health_data <- cc_matrix %>%
  rename(date = eventdate) %>%
  # Optional: Create 'time' variable for long-term trends
  mutate(time_in_study = as.numeric(date) - min(as.numeric(date))) %>%
  arrange(stratum_id, date)

# Validation check: Ensure approximately 4-5 rows per stratum_id
print(head(final_health_data, 10))
summary(final_health_data$age_at_death)



health_data <- final_health_data %>% 
  filter(year >= 2009 & year <= 2016)


#write.csv(health_data,"Data/Health_data.csv")


# --- ADD TIER LOGIC TO PREVIOUS SCRIPT ---

processed_health_data <- health_data %>%
  mutate(
    # Tier 1: Includes every West African site (Highest statistical power)
    is_tier1 = if_else(year >= 2011 & year <= 2014, 1, 0),
    
    # Tier 2: Includes the "Stable 4" countries through 2015 (Recent trends)
    # 270 (Gambia), 384 (Ivory Coast), 686 (Senegal), 854 (Burkina Faso)
    is_tier2 = if_else(year >= 2009 & year <= 2015 & 
                         str_sub(centreid, 1, 2) %in% c("GM", "CI", "SN", "BF"), 1, 0)
  )

#


################################### Merging  #####################################


#UTCI_daily <- UTCI_daily %>% rename(centreid = site_id)


analysis_ready_matrix <- processed_health_data %>%
  # Use a left join to keep all case/control rows
  left_join(UTCI_daily, by = c("centreid", "date")) 
# %>%
#   # Check for any records that failed to match a grid cell
#   filter(!is.na(utci_mean)) 

# Verification: The number of rows should remain identical to your health_data
nrow(analysis_ready_matrix) == nrow(health_data)




























### **The Biostatistical Strategy for DLNM and MMT**
# The **Minimum Mortality Temperature (MMT)** is the specific thermal level where the risk of death is lowest. In your West African study, the MMT serves as the essential "safe" reference point against which all heat and cold risks are measured. 
# 
# According to the sources, you must follow these two steps for the MMT:
# **Empirical Identification:** Fit the model and use the `crosspred()` function to find the UTCI value where the cumulative relative risk is at its minimum. 
# **Centering:** Re-run or re-center the model predictions using this MMT as the reference (`cen = MMT`) to ensure all Relative Risks (RR) for high and low temperatures are relative to this "optimal" climate for the specific population.


  
  ### **Full R Script: High-Precision DLNM Analysis**
  

# Load advanced biostatistical packages
library(tidyverse)
library(dlnm)     # For cross-basis and lag-response functions
library(survival) # For conditional logistic regression (case-crossover)
library(splines)  # For natural cubic splines

# --- STEP 1: DEFINE THE CROSS-BASIS ---
# Based on West African literature, we use a 0-7 day lag window
# and 4 degrees of freedom for both UTCI and Lag splines.
cb_utci <- crossbasis(final_analysis_matrix$utci_mean, 
                      lag = 7, 
                      argvar = list(fun = "ns", df = 4), 
                      arglag = list(fun = "ns", df = 4))

# --- STEP 2: FIT THE MAIN MODEL (OVERALL CUMULATIVE) ---
# We use clogit to account for the stratified case-crossover design
model_overall <- clogit(outcome ~ cb_utci + strata(stratum_id), 
                        data = final_analysis_matrix)

# --- STEP 3: IDENTIFY THE MMT ---
# Extract overall cumulative relative risks to find the minimum point
pred_init <- crosspred(cb_utci, model_overall, by = 0.1)
MMT <- pred_init$predvar[which.min(pred_init$allRRfit)]
print(paste("The identified MMT for West Africa is:", MMT))

# Re-run prediction centered at the MMT
overall_results <- crosspred(cb_utci, model_overall, cen = MMT, by = 0.5)

# --- STEP 4: STRATIFIED ANALYSIS FUNCTION ---
# This function automates the analysis for country, age, and sex
run_stratified_dlnm <- function(subset_data, label) {
  # Re-define cross-basis for the specific subset
  cb_sub <- crossbasis(subset_data$utci_mean, lag = 7, 
                       argvar = list(fun = "ns", df = 4), 
                       arglag = list(fun = "ns", df = 4))
  
  # Fit conditional model
  mod_sub <- clogit(outcome ~ cb_sub + strata(stratum_id), data = subset_data)
  
  # Predict using the overall MMT for comparison across groups
  res_sub <- crosspred(cb_sub, mod_sub, cen = MMT, by = 0.5)
  
  return(res_sub)
}

# --- STEP 5: EXECUTE STRATIFIED TIERS ---

# 1. By Country (Scenario 1 & 2) [Turn 13]
results_senegal <- run_stratified_dlnm(filter(final_analysis_matrix, str_sub(centreid, 1, 2) == "SN"), "Senegal")
results_ghana <- run_stratified_dlnm(filter(final_analysis_matrix, str_sub(centreid, 1, 2) == "GH"), "Ghana")

# 2. By Age Group (Objective 4: Under-5s vs. Elderly)
results_children <- run_stratified_dlnm(filter(final_analysis_matrix, age_at_death <= 5), "Under-5")
results_elderly <- run_stratified_dlnm(filter(final_analysis_matrix, age_at_death > 65), "Elderly")

# 3. By Sex
results_male <- run_stratified_dlnm(filter(final_analysis_matrix, sex == 1), "Male")
results_female <- run_stratified_dlnm(filter(final_analysis_matrix, sex == 2), "Female")

# --- STEP 6: VISUALIZATION ---
# Plotting the overall cumulative UTCI-Mortality association
plot(overall_results, "overall", xlab = "UTCI (°C)", ylab = "Relative Risk",
     main = "Overall Cumulative Heat-Mortality Risk in West Africa")
abline(v = MMT, col = "red", lty = 2) # Highlight the MMT





































# Load essential packages
library(tidyverse)
library(dlnm)     # Core DLNM engine [3]
library(survival) # Conditional Logistic Regression [6]
library(splines)  # Natural splines [7]
library(viridis)  # Scientific color scales [8]

# --- STEP 1: CALCULATE PERCENTILES FOR ANALYSIS ---
# Percentiles identify site-specific extremes (e.g., 99th percentile for 'Extreme Heat')
percs <- quantile(final_analysis_matrix$utci_mean, 
                  probs = c(0.01, 0.05, 0.1, 0.5, 0.9, 0.95, 0.99), na.rm = TRUE)

# --- STEP 2: DEFINE CROSS-BASIS ---
# Using 4 df for UTCI and 4 df for Lag (0-7 days) as recommended [9, 10]
cb_utci <- crossbasis(final_analysis_matrix$utci_mean, 
                      lag = 7, 
                      argvar = list(fun = "ns", df = 4), 
                      arglag = list(fun = "ns", df = 4))

# --- STEP 3: MAIN MODEL & MMT IDENTIFICATION ---
model_overall <- clogit(outcome ~ cb_utci + strata(stratum_id), 
                        data = final_analysis_matrix)

# Find the MMT (Minimum Mortality Temperature) from the cumulative association [11, 12]
pred_init <- crosspred(cb_utci, model_overall, by = 0.1)
MMT <- pred_init$predvar[which.min(pred_init$allRRfit)]
message("MMT identified at: ", MMT, "°C UTCI")

# Re-center predictions at MMT [13, 14]
res_overall <- crosspred(cb_utci, model_overall, cen = MMT, by = 0.1)

# --- STEP 4: STRATIFIED PERCENTILE TABLE FUNCTION ---
# This extracts 'Gold Standard' RR values for specific percentiles
get_summary_table <- function(pred_obj, label) {
  # Indices for percentiles in the prediction vector
  idx_p1  <- which.min(abs(pred_obj$predvar - percs[15]))
  idx_p99 <- which.min(abs(pred_obj$predvar - percs[16]))
  
  data.frame(
    Group = label,
    P1_RR = round(pred_obj$allRRfit[idx_p1], 2),
    P1_CI = paste0("(", round(pred_obj$allRRlow[idx_p1], 2), "-", round(pred_obj$allRRhigh[idx_p1], 2), ")"),
    P99_RR = round(pred_obj$allRRfit[idx_p99], 2),
    P99_CI = paste0("(", round(pred_obj$allRRlow[idx_p99], 2), "-", round(pred_obj$allRRhigh[idx_p99], 2), ")")
  )
}

# --- STEP 5: GOLD STANDARD VISUALIZATIONS ---

# 1. 3D Surface Plot (Exposure-Lag-Response) [4, 5]
plot(res_overall, xlab = "UTCI (°C)", ylab = "Lag (Days)", zlab = "Relative Risk",
     main = "3D Association between UTCI, Lag, and Mortality",
     theta = 40, phi = 30, lphi = 30, col = "lightblue")

# 2. 2D Overall Cumulative Association [3, 17]
plot(res_overall, "overall", xlab = "UTCI (°C)", ylab = "Cumulative Relative Risk",
     main = "Overall UTCI-Mortality Risk (Referent: MMT)", 
     col = "blue", lwd = 2, ci = "area")
abline(v = MMT, col = "red", lty = 2) # Mark MMT

# 3. Lag-Response 'Slices' for Extreme Heat (P99) [18, 19]
plot(res_overall, ptype = "slices", var = percs[16], 
     main = paste0("Lagged Effects of Extreme Heat (UTCI: ", round(percs[16], 1), "°C)"),
     xlab = "Lag (Days)", ylab = "Relative Risk", ci = "bars", col = "red")
