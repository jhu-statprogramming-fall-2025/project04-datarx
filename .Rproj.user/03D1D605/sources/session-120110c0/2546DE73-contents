## Core libraries
library(DBI)
library(RSQLite)
library(dplyr)
library(janitor)
library(lubridate)
library(tidyverse)

## Data sources
library(cdcfluview)
library(nasapower)

## Modeling & Shiny
library(tidymodels)
library(forecast)
library(dbplyr)
library(shiny)
library(shinydashboard)
library(sf)
library(tigris)
library(usmap)


## 2. State Lookup Tables ----
# Master lookup of all US states
all_states_df <- tibble(
  state_name = state.name,
  state_abbr = state.abb,
  state_id   = seq_along(state.name))

# Northeast states of interest
ne_states <- c("Maine", "New Hampshire", "Vermont", "Massachusetts",
  "Rhode Island", "Connecticut", "New York", "New Jersey",
  "Pennsylvania")

# Subset lookup table for NE region
ne_states_lookup <- all_states_df %>%
  filter(state_name %in% ne_states)

# Add state coordinates
state_points <- tibble(
  state_name = ne_states,
  state_abbr = state.abb[match(ne_states, state.name)],
  lat = c(45.253, 43.680, 44.072, 42.259, 41.675, 41.599, 42.953, 40.143, 40.872),
  lon = c(-69.445, -71.581, -72.668, -71.808, -71.556, -72.699, -75.526, -74.729, -77.799)
) %>%
  left_join(ne_states_lookup, by = c("state_name", "state_abbr"))



## Creating and connecting to SQLite databases
#central
con <- dbConnect(SQLite(), "flu_northeast.db")

#states
dbExecute(con, "
  CREATE TABLE IF NOT EXISTS states (
    state_id    INTEGER PRIMARY KEY,
    state_name  TEXT NOT NULL,
    state_abbr  TEXT NOT NULL);")

#influenza
dbExecute(con, "
  CREATE TABLE IF NOT EXISTS ili (
    ili_id         INTEGER PRIMARY KEY,
    region       TEXT NOT NULL,
    state_id     INTEGER,
    state_abbr   TEXT,
    year           INTEGER NOT NULL,
    week           INTEGER NOT NULL,
    unweighted_ili REAL,
    weighted_ili   REAL,
    ili_total      INTEGER,
    total_patients INTEGER,
    provider_count INTEGER,
    week_start     DATE,
    FOREIGN KEY(state_id) REFERENCES states(state_id));")

#mortality
dbExecute(con, "
  CREATE TABLE IF NOT EXISTS mortality (
    mort_id      INTEGER PRIMARY KEY,
    region       TEXT NOT NULL,
    state_id     INTEGER,
    state_abbr   TEXT,
    year         INTEGER NOT NULL,
    week         INTEGER NOT NULL,
    total_deaths INTEGER,
    pi_deaths    INTEGER,
    percent_pi   REAL,
    week_start   DATE,
    week_end     DATE);")

#climate
dbExecute(con, "
  CREATE TABLE IF NOT EXISTS climate (
    clim_id     INTEGER PRIMARY KEY,
    state_id    INTEGER NOT NULL,
    state_name  TEXT NOT NULL,
    state_abbr  TEXT NOT NULL,
    date        DATE NOT NULL,
    t2m         REAL,
    rh2m        REAL,
    precip      REAL,
    lat         REAL,
    lon         REAL,
    FOREIGN KEY(state_id) REFERENCES states(state_id)
  );
")

## 4. Populate States Table 
dbWriteTable(con, "states", ne_states_lookup, append = TRUE)
states_lookup <- dbReadTable(con, "states")

## CDCFluView API
# Creating a cleaning master function
clean_cdc_api <- function(data, state_col, states_lookup) {
  data %>%
    janitor::clean_names() %>%
    filter({{ state_col }} %in% states_lookup$state_name) %>%
    left_join(
      states_lookup,
      by = setNames("state_name", rlang::as_name(rlang::enquo(state_col))))}


# 6. ILI Cleaning Wrapper

get_clean_ili <- function(years = 2010:2024, states_lookup) {
  raw <- ilinet(region = "state", years = years)
  clean_cdc_api(raw, region, states_lookup) %>%
    transmute(
      state_id,
      state_abbr,
      region,
      year,
      week,
      unweighted_ili,
      weighted_ili,
      ili_total = ilitotal,
      total_patients,
      provider_count = num_of_providers,
      week_start = as.Date(week_start))}


# 7. Mortality Cleaning Wrapper
get_clean_mortality <- function(years = 2010:2024, states_lookup) {
  raw <- pi_mortality("state", years = years)
  clean_cdc_api(raw, region_name, states_lookup) %>%
    transmute(
      state_id,
      state_abbr,
      region = region_name,
      year  = as.integer(format(as.Date(week_start), "%Y")),
      week  = weeknumber,
      total_deaths = all_deaths,
      pi_deaths    = total_pni,
      percent_pi   = percent_pni,
      week_start = as.Date(week_start),
      week_end   = as.Date(week_end))}


#8. Writing Clean Data to SQLite 
# ILI
ili_clean <- get_clean_ili(2010:2024, states_lookup)
dbWriteTable(con, "ili", ili_clean, append = TRUE)

# Mortality
mort_clean <- get_clean_mortality(2010:2024, states_lookup)
dbWriteTable(con, "mortality", mort_clean, append = TRUE)


#################################3

get_state_climate <- function(lat, lon, start, end) {
  raw <- nasapower::get_power(
    community    = "AG",
    pars         = c("T2M", "RH2M", "PRECTOTCORR"),
    temporal_api = "DAILY",
    lonlat       = c(lon, lat),
    dates        = c(start, end))
  
  message("Returned object names: ", paste(names(raw), collapse = ", "))
  
  # In your setup, the whole object is the data frame
  if (is.data.frame(raw) && all(c("YEAR", "MM", "DD") %in% names(raw))) {
    return(raw)
  }
  
  # Fallbacks for other nasapower formats
  if ("POWER" %in% names(raw) && is.data.frame(raw$POWER)) {
    return(raw$POWER)
  }
  if ("data" %in% names(raw) && is.data.frame(raw$data)) {
    return(raw$data)
  }
  
  warning("NASA POWER returned no usable data object.")
  return(NULL)
}

clean_climate <- function(power_df, state_id) {
  
  if (is.null(power_df)) return(NULL)
  
  df <- janitor::clean_names(power_df)
  
  # CASE 1: yyyymmdd is already a Date
  if ("yyyymmdd" %in% names(df) && inherits(df$yyyymmdd, "Date")) {
    
    df$date <- df$yyyymmdd
    
    # CASE 2: year/mo/day exist
  } else if (all(c("year", "mm", "dd") %in% names(df))) {
    
    df$date <- lubridate::ymd(
      sprintf("%04d-%02d-%02d", df$year, df$mm, df$dd)
    )
    
  } else {
    warning("No usable date column found.")
    return(NULL)
  }
  
  df %>%
    transmute(
      state_id = state_id,
      date     = date,
      t2m      = t2m,
      rh2m     = rh2m,
      precip   = prectotcorr
    )
}


all_climate <- purrr::pmap_dfr(
  list(
    state_id   = state_points$state_id,
    state_name = state_points$state_name,
    state_abbr = state_points$state_abbr,
    lat        = state_points$lat,
    lon        = state_points$lon
  ),
  function(state_id, state_name, state_abbr, lat, lon) {
    
    raw <- get_state_climate(
      lat  = lat,
      lon  = lon,
      start = "2010-01-01",
      end   = "2024-12-31"
    )
    
    out <- clean_climate(raw, state_id = state_id)
    
    out %>%
      mutate(
        state_name = state_name,
        state_abbr = state_abbr
      )
  })

all_climate <- tibble::as_tibble(as.data.frame(all_climate))

dbWriteTable(con, "climate", all_climate, append = TRUE)


#Converting daily to Weekly climate features
weekly_climate <- all_climate %>%
  mutate(
    year = lubridate::year(date),
    week = lubridate::isoweek(date)
  ) %>%
  group_by(state_id, state_name, state_abbr, year, week) %>%
  summarise(
    t2m_mean   = mean(t2m,   na.rm = TRUE),
    rh2m_mean  = mean(rh2m,  na.rm = TRUE),
    precip_sum = sum(precip, na.rm = TRUE),
    .groups    = "drop"
  )

# Store in SQLite
dbExecute(con, "DROP TABLE IF EXISTS climate_weekly;")
dbWriteTable(con, "climate_weekly", weekly_climate)