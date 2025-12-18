#Creating R objects for codes to allow for optimization of Shiny Dashboards


# Merging full dataset
full_df <- ili_tbl %>%
  left_join(mort_tbl, by = c("state_id","state_abbr","year","week")) %>%
  left_join(clim_wk, by = c("state_id","state_abbr","year","week")) %>%
  arrange(state_name, year, week)

#Getting map of Northeastern States
ne_states <- c("ME","NH","VT","MA","RI","CT","NY","NJ","PA")
ne_geom <- states(cb = TRUE, year = 2021) %>%
  filter(STUSPS %in% ne_states) %>% st_transform(4326) %>% 
  select(state = STUSPS, geometry, NAME)

saveRDS(full_df, "full_df.rds")
saveRDS(ne_geom, "ne_geom.rds")
saveRDS(lm_A_fit, "lm_A_fit.rds")
saveRDS(lm_B_fit, "lm_B_fit.rds")
saveRDS(rf_A_fit, "rf_A_fit.rds")
saveRDS(rf_B_fit, "rf_B_fit.rds")
saveRDS(A_perf, "A_perf.rds")
saveRDS(B_perf, "B_perf.rds")
saveRDS(rf_A_imp_df, "rf_A_imp_df.rds")
saveRDS(rf_B_imp_df, "rf_B_imp_df.rds")
saveRDS(fc_arima_A, "fc_arima_A.rds")
saveRDS(fc_arima, "fc_arima.rds")

