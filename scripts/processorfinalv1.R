

library(tidyverse)
library(lubridate)
library(haven)
library(hrbrthemes)
library(gtsummary)
library(flextable)
library(visdat)
library(naniar)
library(dlnm)
library(splines)
library(gnm)       # conditional quasi-Poisson (replaces bare clogit for inference)
library(survival)  # kept only as an optional cross-check against gnm

source("Scripts/lookup.R")
# --- CONFIG: edit these paths once ---
PATH_ROOT       <- "D:/yaku-alex/west_Africa"
PATH_UTCI       <- file.path(PATH_ROOT, "Data/UTCI_daily_sites_2009_2016.csv")
PATH_HEALTH_DTA <- file.path(PATH_ROOT, "Data/ConsolidatedData2018_analysis.dta")
MAX_LAG         <- 28   # days; revisit vs. literature (many mortality DLNMs use 21d for slow-onset causes)

setwd(PATH_ROOT)


raw_data <- read_dta(PATH_HEALTH_DTA) %>%rename_all(str_to_lower)


# 1. Load and clean INDEPTH mortality microdata


wa_prefixes <- c("BF", "CI", "GH", "GM", "NG", "SN")

west_africa_mortality <- raw_data %>%
  filter(str_sub(centreid, 1, 2) %in% wa_prefixes) %>%
  filter(eventcode == 7) %>%   # confirmed deaths
  filter(residence == 1)       # resident at site on day of death


processed_cases <-
  
  west_africa_mortality %>%
  
  mutate(
    eventdate = as.Date(eventdate),
    dob = as.Date(dob),
    age_at_death =
      as.numeric(difftime(eventdate,dob,units="days"))/365.25,
    year = year(eventdate),
    month = month(eventdate),
    dow = wday(eventdate,label=TRUE),
    stratum_id = row_number()
  ) %>%
  
  select(
    stratum_id,
    concat_individualid,
    centreid,
    locationid,
    sex,
    age_at_death,
    eventdate,
    year,
    month,
    dow
  ) %>%
  
  left_join(
    centre_lookup,
    by="centreid"
  ) 
stopifnot(!any(is.na(processed_cases$centre_name)))
stopifnot(!any(is.na(processed_cases$country)))
stopifnot(!any(is.na(processed_cases$climate_zone)))


processed_cases <-
  processed_cases %>%
  mutate(
    
    age_group = case_when(
      
      age_at_death < 5 ~ "0–4 years",
      
      age_at_death >=5 &
        age_at_death <15 ~ "5–14 years",
      
      age_at_death >=15 &
        age_at_death <65 ~ "15–64 years",
      
      age_at_death >=65 ~ "65+ years",
      
      TRUE ~ NA_character_
      
    ),
    
    age_group=factor(
      age_group,
      levels=c(
        "0–4 years",
        "5–14 years",
        "15–64 years",
        "65+ years"
      )
    )
    
  )

processed_cases <- processed_cases %>%
  mutate(
    
    centre_name =
      factor(
        centre_name,
        levels = centre_lookup$centre_name
      ),
    
    centre_label =
      factor(
        centre_label,
        levels = centre_lookup$centre_label
      ),
    
    country =
      factor(
        country,
        levels = unique(centre_lookup$country)
      ),
    
    climate_zone =
      factor(
        climate_zone,
        levels = unique(centre_lookup$climate_zone)
      )
    
  )
    
processed_cases %>% count(country)

processed_cases %>%
  count(centreid, centre_name)

# 2. Time-stratified case-crossover matrix (Bateson & Schwartz design)

# Referents are all other days in the same calendar month falling on the same
# day of week as the death -- this design inherently controls for long-term
# trend, season, and day-of-week without needing separate spline terms for
# those factors.

get_referent_days <- function(case_date) {
  m_start <- floor_date(case_date, "month")
  m_end   <- ceiling_date(case_date, "month") - days(1)
  month_days <- seq(m_start, m_end, by = "1 day")
  month_days[wday(month_days) == wday(case_date) & month_days != case_date]
}

cc_matrix <- processed_cases %>%
  group_by(stratum_id) %>%
  do({
    case_row <- .
    control_dates <- get_referent_days(case_row$eventdate)
    control_rows <- map_df(control_dates, ~ case_row %>% mutate(eventdate = .x, outcome = 0))
    case_row <- case_row %>% mutate(outcome = 1)
    bind_rows(case_row, control_rows)
  }) %>%
  ungroup()

final_health_data <- cc_matrix %>%
  rename(date = eventdate) %>%
  mutate(time_in_study = as.numeric(date) - min(as.numeric(date))) %>%
  arrange(stratum_id, date) %>%
  filter(year >= 2009 & year <= 2016) %>%
  mutate(
    is_tier1 = if_else(year >= 2011 & year <= 2014, 1, 0),
    is_tier2 = if_else(year >= 2009 & year <= 2015 &
                         str_sub(centreid, 1, 2) %in% c("GM", "CI", "SN", "BF"), 1, 0)
  )

write_csv(final_health_data, "final_health.csv")


# 3. Load and process UTCI climate data, build lag matrix

UTCI_daily <- read_csv(PATH_UTCI)
names(UTCI_daily)



UTCI_daily <- UTCI_daily %>%
  mutate(
    date = as.Date(date),
    across(c(utci_min, utci_mean, utci_max), ~ . - 273.15)
  ) %>%
  rename(centreid = site_id) %>%
  arrange(centreid, date) %>%
  group_by(centreid) 
# %>%
#   mutate(
#     utci_lag0 = utci_mean
#   )

UTCI_daily$utci_lag0 <- UTCI_daily$utci_mean

for(i in seq_len(MAX_LAG)){
  
  UTCI_daily <-
    UTCI_daily %>%
    mutate(
      !!paste0("utci_lag",i) :=
        lag(utci_mean,i)
    )
  
}


UTCI_daily <- UTCI_daily %>%
  ungroup()

UTCI_daily %>%
  group_by(centreid) %>%
  summarise(
    ordered = all(diff(date)>=0)
  )

saveRDS(
  processed_cases,
  "Data/processed_cases.rds"
)
# 4. Merge exposure onto the case-crossover matrix

analysis_ready_matrix <- final_health_data %>%
  left_join(UTCI_daily, by = c("centreid", "date"))

stopifnot(nrow(analysis_ready_matrix) == nrow(final_health_data))
n_missing_exposure <- sum(is.na(analysis_ready_matrix$utci_mean))
message(n_missing_exposure, " rows failed to match a UTCI grid cell (dropped below).")

analysis_ready_matrix <-analysis_ready_matrix %>%filter(
  !is.na(
    .data[[paste0("utci_lag",MAX_LAG)]]))

write.csv(analysis_ready_matrix,"mortality_health_analysis_readydata.csv")

# Save your dataset as a compressed R data file (.rds)
saveRDS(analysis_ready_matrix, "mortality_health_analysis_readydata.rds")



