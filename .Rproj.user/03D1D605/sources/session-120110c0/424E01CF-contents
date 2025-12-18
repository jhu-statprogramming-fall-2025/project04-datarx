model_metrics <- function(true, pred) {
  tibble(
    RMSE  = rmse_vec(true, pred),
    MAE   = mae_vec(true, pred),
    MAPE  = mape_vec(true, pred),
    Rsq   = rsq_vec(true, pred))}

## Generate predictions for LM and RF (Model A + Model B)

#MODEL A: predict unweighted_ili
# Predictions: PRE-PANDEMIC
A_train$pred_lm  <- predict(lm_A_fit,  A_train)$.pred
A_train$pred_rf  <- predict(rf_A_fit,  A_train)$.pred

# PANDEMIC (2020–2021)
A_pand$pred_lm <- predict(lm_A_fit, A_pand)$.pred
A_pand$pred_rf <- predict(rf_A_fit, A_pand)$.pred

# POST-PANDEMIC (2022–2024)
A_post$pred_lm <- predict(lm_A_fit, A_post)$.pred
A_post$pred_rf <- predict(rf_A_fit, A_post)$.pred

A_perf <- bind_rows(
  model_metrics(A_train$unweighted_ili, A_train$pred_lm)  %>% mutate(model="LM", period="Train"),
  model_metrics(A_pand$unweighted_ili,  A_pand$pred_lm)   %>% mutate(model="LM", period="Pandemic"),
  model_metrics(A_post$unweighted_ili,  A_post$pred_lm)   %>% mutate(model="LM", period="Post"),
  
  model_metrics(A_train$unweighted_ili, A_train$pred_rf)  %>% mutate(model="RF", period="Train"),
  model_metrics(A_pand$unweighted_ili,  A_pand$pred_rf)   %>% mutate(model="RF", period="Pandemic"),
  model_metrics(A_post$unweighted_ili,  A_post$pred_rf)   %>% mutate(model="RF", period="Post")
)

A_perf


#MODEL B: predict percent_pi
# PRE-PANDEMIC
B_train$pred_lm <- predict(lm_B_fit, B_train)$.pred
B_train$pred_rf <- predict(rf_B_fit, B_train)$.pred

# PANDEMIC
B_pand$pred_lm <- predict(lm_B_fit, B_pand)$.pred
B_pand$pred_rf <- predict(rf_B_fit, B_pand)$.pred

# POST-PANDEMIC
B_post$pred_lm <- predict(lm_B_fit, B_post)$.pred
B_post$pred_rf <- predict(rf_B_fit, B_post)$.pred

B_perf <- bind_rows(
  model_metrics(B_train$percent_pi, B_train$pred_lm)  %>% mutate(model="LM", period="Train"),
  model_metrics(B_pand$percent_pi,  B_pand$pred_lm)   %>% mutate(model="LM", period="Pandemic"),
  model_metrics(B_post$percent_pi,  B_post$pred_lm)   %>% mutate(model="LM", period="Post"),
  
  model_metrics(B_train$percent_pi, B_train$pred_rf)  %>% mutate(model="RF", period="Train"),
  model_metrics(B_pand$percent_pi,  B_pand$pred_rf)   %>% mutate(model="RF", period="Pandemic"),
  model_metrics(B_post$percent_pi,  B_post$pred_rf)   %>% mutate(model="RF", period="Post")
)

B_perf


## ------------------------------------------------------------------
## 4a. FEATURE IMPORTANCE: which lags drive pi_percent and unweighted_ili?
## ------------------------------------------------------------------

# Extract underlying ranger model
rf_A_fit_obj <- rf_A_fit %>% extract_fit_parsnip()

# Pull variable importance
rf_A_imp <- rf_A_fit_obj$fit$variable.importance

# Convert to tidy tibble
rf_A_imp_df <- tibble(
  predictor  = names(rf_A_imp),
  importance = as.numeric(rf_A_imp)
) %>%
  arrange(desc(importance))

# View ranked importance
rf_A_imp_df

rf_A_imp_df %>%
  slice_max(importance, n = 15) %>%
  ggplot(aes(x = reorder(predictor, importance), y = importance)) +
  geom_col(fill = "#2C7BB6") +
  coord_flip() +
  labs(
    title = "Random Forest (Model A): Variable Importance for unweighted_ili",
    x = "Lagged predictor",
    y = "Importance (impurity decrease)"
  ) +
  theme_minimal(base_size = 13)


#For model B
rf_B_fit_obj <- rf_B_fit %>% extract_fit_parsnip()
rf_B_imp <- rf_B_fit_obj$fit$variable.importance

rf_B_imp_df <- tibble(
  predictor  = names(rf_B_imp),
  importance = as.numeric(rf_B_imp)
) %>%
  arrange(desc(importance))

# View ranked importance
rf_B_imp_df

# Plot top 15 predictors (you will see which lags dominate)
rf_B_imp_df %>%
  slice_max(importance, n = 15) %>%
  ggplot(aes(x = reorder(predictor, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Random Forest (Model B): Variable Importance for pi_deaths",
    x = "Lagged predictor",
    y = "Importance (impurity decrease)"
  )



#Model perfomance
bind_rows(
  A_perf %>% mutate(target = "ILI"),
  B_perf %>% mutate(target = "Percent PI")
) %>%
  ggplot(aes(x = period, y = RMSE, fill = model)) +
  geom_col(position = "dodge") +
  facet_wrap(~ target, scales = "free_y") +
  labs(
    title = "Model Performance Across Periods",
    y = "RMSE",
    x = "Period"
  ) +
  theme_minimal(base_size = 14) +
  scale_fill_brewer(palette = "Set2")



## Generate predictions for ARIMA Model
y_train <- B_agg_pre$percent_pi

# Forecast horizon = number of pandemic + post weeks
h_pand  <- nrow(B_agg %>% filter(year %in% 2020:2021))
h_post  <- nrow(B_agg %>% filter(year >= 2022))
# Forecasting
fc_arima_B <- forecast(arima_pi, h = h_pand + h_post)
# Extract predicted values
pred_arima <- fc_arima_B$mean
true_future <- B_agg %>% filter(year >= 2020) %>% pull(percent_pi)
# performance
arima_perf_B <- model_metrics(true_future, pred_arima[1:length(true_future)])
arima_perf_B

#Similar for A
fc_arima_A <- forecast(arima_ili, h = nrow(A_agg %>% filter(year >= 2020)))
pred_arima_A <- fc_arima_A$mean
true_A <- A_agg %>% filter(year >= 2020) %>% pull(ili)
arima_perf_A <- model_metrics(true_A, pred_arima_A[1:length(true_A)])
arima_perf_A



##############################################################
#Plots for the ARIMA Models#
##############################################################

autoplot(fc_arima) +
  labs(
    title = "ARIMA Forecast: Percent PI",
    x = "Week",
    y = "Percent PI"
  ) +
  theme_minimal()

autoplot(fc_arima_A) +
  labs(
    title = "ARIMA Forecast: Percent PI",
    x = "Week",
    y = "Unweighted ILI"
  ) +
  theme_minimal()
