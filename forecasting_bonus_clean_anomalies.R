# LEARNING LAB 18 ----
# TIME SERIES ANOMALY DETECTION ----
# **** BONUS FOR PRO MEMBERS *****
# FORECASTING RESULTS
# METHOD: GLMNET - CLEANED ANOMALIES ----

library(timetk)
library(parsnip)
library(rsample)

# 1.0 Apply 12 forecasts using successive mutate() + map() ----
forecast_clean_anomalies_tbl <- pages_most_visited_anom_tbl %>%
    select(Page, Date, observed, visits_cleaned) %>%
    nest() %>%
    
    # Create train/test sets
    mutate(split = map(data, .f = function(data) {
        
        data %>% rsample::initial_time_split(prop = 0.8)
    
    })) %>%
    
    # Apply model to training set
    mutate(model = map(split, .f = function(split) {
        trn_tbl <- split %>% 
            training() %>%
            timetk::tk_augment_timeseries_signature() %>%
            select(-diff)
        
        model <- linear_reg(mixture = 0.5) %>%
            set_engine("glmnet") %>%
            fit.model_spec(visits_cleaned ~ . - Date - observed, data = trn_tbl)
        
        return(model)
    })) %>%
    
    # Fit the model using the last lambda (highest deviance explained on training set)
    # Use timetk::tk_augment_time_series_signature to create engineered features from the time stamp
    mutate(predictions = map2(.x = split, .y = model, .f = function(split, model) {
        
        model %>%
            predict.model_fit(
                new_data = split %>% 
                    testing() %>% 
                    timetk::tk_augment_timeseries_signature() %>% 
                    select(-diff) 
            ) %>%
            as_tibble() %>%
            # GLMnet returns values for many lambdas
            # We'll use the last one (Highest deviance explained)
            filter(.pred_lambda == last(.pred_lambda))
    })) %>%
    
    # Combine the train/test splits and predictions into one set
    mutate(clean_vs_preds = map2(.x = split, .y = predictions, .f = function(split, predictions) {
        
        training(split) %>%
            bind_rows(
                bind_cols(testing(split), predictions)
            ) %>%
            as_tibble()
        
    })) %>%
    
    # Combine observed and predictions into one set
    mutate(actual_vs_preds = map2(.x = data, .y = predictions, .f = function(data, predictions) {
        
        preds   <- predictions %>% pull(.pred_values)
        na_vals <- rep(NA, nrow(data) - nrow(predictions))
        
        data %>%
            mutate(.pred_values = c(na_vals, preds))
        
    }))


# 2.0 Visualize Predictions ----

# 2.1 Observed vs Pred -----
forecast_plot_cleaned_anoms_vs_observed <- forecast_clean_anomalies_tbl %>%
    select(Page, actual_vs_preds) %>%
    unnest() %>%

    ggplot(aes(Date, observed)) +
    geom_point(color = palette_light()[[1]], alpha = 0.25) +
    geom_point(aes(y = .pred_values), color = palette_light()[[2]], alpha = 0.25) +
    facet_wrap(~ Page, ncol = 3, scales = "free_y") +
    theme_tq() +
    expand_limits(y = 0) +
    scale_y_continuous(labels = scales::comma) +
    labs(title = "Forecast Plot - Cleaned Anomalies")

# 2.2 Cleaned Vs Pred -----
forecast_plot_cleaned_anoms_vs_cleaned <- forecast_clean_anomalies_tbl %>%
    select(Page, clean_vs_preds) %>%
    unnest() %>%

    ggplot(aes(Date, visits_cleaned)) +
    geom_point(color = palette_light()[[1]], alpha = 0.25) +
    geom_point(aes(y = .pred_values), color = palette_light()[[2]], alpha = 0.25) +
    facet_wrap(~ Page, ncol = 3, scales = "free_y") +
    theme_tq() +
    expand_limits(y = 0) +
    scale_y_continuous(labels = scales::comma) +
    labs(title = "Forecast Plot - Cleaned Anomalies - Anomalies Removed")

# 3.0 Calculate MAPE ----
mape_cleaned_anoms <- forecast_clean_anomalies_tbl %>%
    select(Page, actual_vs_preds) %>%
    unnest() %>%
    filter(!is.na(.pred_values)) %>%
    
    group_by(Page) %>%
    summarize(mape = mean(abs(.pred_values - observed) / observed )) %>%
    ungroup(Page) %>%
    
    summarize(mean(mape))
