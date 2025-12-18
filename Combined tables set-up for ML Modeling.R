#Building Data for modeling (for combined Mortality/OPD/Climate)
## ILI ~ climate + mortality (2016–2024)
ili_tbl  <- tbl(con, "ili")
mort_tbl <- tbl(con, "mortality")
clim_wk  <- tbl(con, "climate_weekly")

model_df <- ili_tbl %>%
  inner_join(
    mort_tbl,
    by = c("state_id", "state_abbr", "year", "week", "region")
  ) %>%
  inner_join(
    clim_wk,
    by = c(
      "state_id",
      "state_abbr",
      "year",
      "week",
      region = "state_name"   
    )
  ) %>%
  select(
    state_id,
    state_name = region,
    state_abbr,
    year,
    week,
    weighted_ili,
    unweighted_ili,
    ili_total,
    total_patients,
    pi_deaths,
    percent_pi,
    t2m_mean,
    rh2m_mean,
    precip_sum
  ) %>%
  collect() %>%
  # climate ends at 2024, so drop 2025
  filter(year <= 2024) %>%
  mutate(
    pandemic = if_else(year %in% 2020:2021, 1L, 0L)
  )


#Building Data for modeling (OPD/Climate)
# Model A: ILI ~ climate only (2010–2024)
model_df_A <- ili_tbl %>%
  inner_join(
    clim_wk,
    by = c(
      "state_id",
      "state_abbr",
      "year",
      "week",
      region = "state_name"   
    )
  ) %>%
  select(
    state_id,
    state_name = region,
    state_abbr,
    year,
    week,
    weighted_ili,
    unweighted_ili,
    ili_total,
    total_patients,
    t2m_mean,
    rh2m_mean,
    precip_sum
  ) %>%
  collect() %>%
  mutate(
    pandemic = if_else(year %in% 2020:2021, 1L, 0L)
  )


# Adding lag features to Model
# ---- Add lags for Model A (ILI + climate) ----
model_A <- model_df_A %>%
  arrange(state_id, year, week) %>%
  group_by(state_id) %>%
  mutate(
    # ILI lags
    ili_lag1 = lag(unweighted_ili, 1),
    ili_lag2 = lag(unweighted_ili, 2),
    ili_lag3 = lag(unweighted_ili, 3),
    ili_lag4 = lag(unweighted_ili, 4),
    
    # Climate lags (mean weekly)
    t2m_lag1 = lag(t2m_mean, 1),
    t2m_lag2 = lag(t2m_mean, 2),
    t2m_lag3 = lag(t2m_mean, 3),
    t2m_lag4 = lag(t2m_mean, 4),
    
    rh2m_lag1 = lag(rh2m_mean, 1),
    rh2m_lag2 = lag(rh2m_mean, 2),
    rh2m_lag3 = lag(rh2m_mean, 3),
    rh2m_lag4 = lag(rh2m_mean, 4),
    
    precip_lag1 = lag(precip_sum, 1),
    precip_lag2 = lag(precip_sum, 2),
    precip_lag3 = lag(precip_sum, 3),
    precip_lag4 = lag(precip_sum, 4)
  ) %>%
  ungroup() %>%
  # drop early weeks where lags are NA
  filter(!is.na(ili_lag4))


# ---- Add lags for Model B (ILI + climate + mortality) ----
model_B <- model_df %>%
  arrange(state_id, year, week) %>%
  group_by(state_id) %>%
  mutate(
    # ILI lags
    ili_lag1 = lag(unweighted_ili, 1),
    ili_lag2 = lag(unweighted_ili, 2),
    ili_lag3 = lag(unweighted_ili, 3),
    ili_lag4 = lag(unweighted_ili, 4),
    
    # Climate lags
    t2m_lag1 = lag(t2m_mean, 1),
    t2m_lag2 = lag(t2m_mean, 2),
    t2m_lag3 = lag(t2m_mean, 3),
    t2m_lag4 = lag(t2m_mean, 4),
    
    rh2m_lag1 = lag(rh2m_mean, 1),
    rh2m_lag2 = lag(rh2m_mean, 2),
    rh2m_lag3 = lag(rh2m_mean, 3),
    rh2m_lag4 = lag(rh2m_mean, 4),
    
    precip_lag1 = lag(precip_sum, 1),
    precip_lag2 = lag(precip_sum, 2),
    precip_lag3 = lag(precip_sum, 3),
    precip_lag4 = lag(precip_sum, 4)
  ) %>%
  ungroup() %>%
  # Remove early rows that have NA lags
  filter(!is.na(ili_lag4))




#Splitting data in training and testing
## Since 2020 and 2021 were peak pandemic period, they can skew the data so they will be separated

set.seed(123)

# Model A splits (ILI + climate, pre-pandemic training)
A_train <- model_A %>% filter(year >= 2010, year <= 2019)
A_pand  <- model_A %>% filter(year %in% 2020:2021)
A_post  <- model_A %>% filter(year >= 2022, year <= 2024)

# Model B splits (ILI + climate + mortality, 2016+)
B_train <- model_B %>% filter(year >= 2016, year <= 2019) %>%
  filter(!is.na(percent_pi))
B_pand  <- model_B %>% filter(year %in% 2020:2021)
B_post  <- model_B %>% filter(year >= 2022, year <= 2024)

