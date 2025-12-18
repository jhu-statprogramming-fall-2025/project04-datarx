#Using ML Models

set.seed(123)

## ------------------------------------------------------------------
## LINEAR MODELS (LM)
## ------------------------------------------------------------------

# ---- Model A LM: predict unweighted_ili ----
lm_A_rec <- recipe(unweighted_ili ~
                     ili_lag1 + ili_lag2 + ili_lag3 + ili_lag4 +
                     t2m_lag1 + t2m_lag2 + t2m_lag3 + t2m_lag4 +
                     rh2m_lag1 + rh2m_lag2 + rh2m_lag3 + rh2m_lag4 +
                     precip_lag1 + precip_lag2 + precip_lag3 + precip_lag4,
                   data = A_train) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors())

lm_spec <- linear_reg() %>%
  set_engine("lm")

lm_A_wf <- workflow() %>%
  add_model(lm_spec) %>%
  add_recipe(lm_A_rec)

lm_A_fit <- lm_A_wf %>% fit(A_train)


# ---- Model B LM: predict percent_pi ----
lm_B_rec <- recipe(percent_pi ~
                     ili_lag1 + ili_lag2 + ili_lag3 + ili_lag4 +
                     t2m_lag1 + t2m_lag2 + t2m_lag3 + t2m_lag4 +
                     rh2m_lag1 + rh2m_lag2 + rh2m_lag3 + rh2m_lag4 +
                     precip_lag1 + precip_lag2 + precip_lag3 + precip_lag4,
                   data = B_train) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors())

lm_B_wf <- workflow() %>%
  add_model(lm_spec) %>%
  add_recipe(lm_B_rec)

lm_B_fit <- lm_B_wf %>% fit(B_train)


## ------------------------------------------------------------------
## RANDOM FOREST MODELS (RF) + FEATURE IMPORTANCE
## ------------------------------------------------------------------
rf_spec <- rand_forest(
  mtry  = 6,
  min_n = 5,
  trees = 500
) %>%
  set_mode("regression") %>%
  # importance = "impurity" exposes variable.importance in ranger object
  set_engine("ranger", importance = "impurity")


# ---- Model A RF: predict unweighted_ili ----
rf_A_rec <- recipe(unweighted_ili ~
                     ili_lag1 + ili_lag2 + ili_lag3 + ili_lag4 +
                     t2m_lag1 + t2m_lag2 + t2m_lag3 + t2m_lag4 +
                     rh2m_lag1 + rh2m_lag2 + rh2m_lag3 + rh2m_lag4 +
                     precip_lag1 + precip_lag2 + precip_lag3 + precip_lag4,
                   data = A_train) %>%
  step_impute_median(all_numeric_predictors())

rf_A_wf <- workflow() %>%
  add_model(rf_spec) %>%
  add_recipe(rf_A_rec)

rf_A_fit <- rf_A_wf %>% fit(A_train)


# ---- Model B RF: predict percent_pi ----
rf_B_rec <- recipe(percent_pi ~
                     ili_lag1 + ili_lag2 + ili_lag3 + ili_lag4 +
                     t2m_lag1 + t2m_lag2 + t2m_lag3 + t2m_lag4 +
                     rh2m_lag1 + rh2m_lag2 + rh2m_lag3 + rh2m_lag4 +
                     precip_lag1 + precip_lag2 + precip_lag3 + precip_lag4,
                   data = B_train) %>%
  step_impute_median(all_numeric_predictors())

rf_B_wf <- workflow() %>%
  add_model(rf_spec) %>%
  add_recipe(rf_B_rec)

rf_B_fit <- rf_B_wf %>% fit(B_train)



## ------------------------------------------------------------------
## 5. ARIMA, ARIMAX FOR NE-AGGREGATE percent_pi
## ------------------------------------------------------------------
# Aggregate B data across states by year + week
B_agg <- model_B %>%
  group_by(year, week) %>%
  summarise(
    percent_pi = mean(percent_pi, na.rm = TRUE),   # <-- new target
    ili        = mean(unweighted_ili, na.rm = TRUE),
    t2m        = mean(t2m_mean,       na.rm = TRUE),
    rh2m       = mean(rh2m_mean,      na.rm = TRUE),
    precip     = mean(precip_sum,     na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  arrange(year, week)


# Pre-pandemic subset (for "clean" training)
B_agg_pre <- B_agg %>% filter(year <= 2019)

# Weekly time series of Percent PI
pi_ts_pre <- ts(B_agg_pre$percent_pi, frequency = 52)


## 5.1 ARIMA (pi percent ~ past pi_deaths only)
arima_pi <- auto.arima(pi_ts_pre)
summary(arima_pi)


## 5.2 ARIMAX (pi_deaths ~ past climate & ILI)
# Build lagged regressors for ARIMAX

B_agg_pre_lag <- B_agg_pre %>%
  mutate(
    ili_lag1   = lag(ili, 1),
    t2m_lag1   = lag(t2m, 1),
    rh2m_lag1  = lag(rh2m, 1),
    precip_lag1 = lag(precip, 1)
  ) %>%
  filter(!is.na(ili_lag1))   # drop first row where lags are NA

y_arimax <- ts(B_agg_pre_lag$percent_pi, frequency = 52)

X_arimax <- as.matrix(B_agg_pre_lag[, c(
  "ili_lag1",
  "t2m_lag1",
  "rh2m_lag1",
  "precip_lag1")])

arimax_pi <- auto.arima(y_arimax, xreg = X_arimax)
summary(arimax_pi)
fc_arima  <- forecast::forecast(arima_pi, h = 52)


## ------------------------------------------------------------------
## 5b. ARIMA, ARIMAX FOR NE-AGGREGATE ILI (Model A)
## ------------------------------------------------------------------

# Aggregate A data across states by year + week
A_agg <- model_df_A %>%
  group_by(year, week) %>%
  summarise(
    ili       = mean(unweighted_ili, na.rm = TRUE),
    t2m       = mean(t2m_mean,       na.rm = TRUE),
    rh2m      = mean(rh2m_mean,      na.rm = TRUE),
    precip    = mean(precip_sum,     na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  arrange(year, week)

# Pre-pandemic subset
A_agg_pre <- A_agg %>% filter(year <= 2019)

# Weekly time series for ILI (Model A target)
ili_ts_pre <- ts(A_agg_pre$ili, frequency = 52)

## 5.1 ARIMA (ILI ~ past ILI)
arima_ili <- auto.arima(ili_ts_pre)
summary(arima_ili)

## 5.2 ARIMAX (ILI ~ climate + lagged ILI)
A_agg_pre_lag <- A_agg_pre %>%
  mutate(
    ili_lag1   = lag(ili, 1),   # autoregressive term
    t2m_lag1   = lag(t2m, 1),
    rh2m_lag1  = lag(rh2m, 1),
    precip_lag1 = lag(precip, 1)
  ) %>%
  filter(!is.na(ili_lag1))   # drop initial NA row

y_arimax_A <- ts(A_agg_pre_lag$ili, frequency = 52)

X_arimax_A <- as.matrix(A_agg_pre_lag[, c(
  "ili_lag1",
  "t2m_lag1",
  "rh2m_lag1",
  "precip_lag1"
)])

arimax_ili <- auto.arima(y_arimax_A, xreg = X_arimax_A)
summary(arimax_ili)
fc_arima_A  <- forecast::forecast(arima_ili, h = 52)

