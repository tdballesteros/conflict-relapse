
## NOT IN FUNCTION ##

'%!in%' <- function(x,y)!('%in%'(x,y))


## THREE-YEAR GROWTH FUNCTION ##

# The Three-Year Growth function estimates values in a time series ('variable_missing') based on a
# weighted-average growth rate of the most proximate four years with data of the same time series.

three_year_growth_func <- function(df = pd, iso, variable_missing, base_years = c(1950:1953),
                                   restricted = c(1949:1946)){
  
  # convert text variable input to symbols
  variable_missing <- rlang::sym(variable_missing)
  
  # calculate estimation method column name
  variable_est_method <- rlang::sym(paste0(variable_missing, ".est.method"))
  
  # determine order for sorting
  sort_order <- dplyr::case_when(
    base_years[4] > base_years[1] ~ "ascending",
    base_years[1] > base_years[4] ~ "descending"
  )

  # restrict to relevant country
  df2 <- df %>%
    dplyr::filter(iso3c == iso)
  
  # apply sorting order
  if(sort_order == "ascending"){
    df2 <- df2 %>%
      dplyr::arrange(year)
  } else if(sort_order == "descending"){
    df2 <- df2 %>%
      dplyr::arrange(desc(year))
  }
  
  # pull data from the base years
  growth_rates <- df2 %>%
    # calculate one-year growth rates
    dplyr::mutate(growth_rate = (!!variable_missing - lag(!!variable_missing)) / lag(!!variable_missing)) %>%
    # pull growth rates for select years
    dplyr::filter(year %in% base_years[2:4]) %>%
    dplyr::pull(growth_rate)
    
  # calculate weighted average
  weighted_avg <- (1/2) * growth_rates[1] + (1/3) * growth_rates[2] + (1/6) * growth_rates[3]
  
  for(y in restricted){
    
    ref_year <- df[[rlang::as_label(variable_missing)]][df$iso3c == iso & df$year == (y + 1)]
    
    df[[rlang::as_label(variable_missing)]][df$iso3c == iso & df$year == y] <- ref_year / (1 + weighted_avg)
    df[[rlang::as_label(variable_est_method)]][df$iso3c == iso & df$year == y] <- paste0("Three-year weighted growth rates, ", base_years[1], " to ", base_years[4])
    
  }
  
  return(df)
  
}


## BRIDGE REGRESSION FUNCTION ##

# The Bridge Regression function models the relationship between all time series data between
# the time series with missing values to be estimated ('variable_missing') and the time series
# used to estimate those values ('variable_complete'). This model is then used to estimate missing
# values. The model treats the relationship between the two time series as consistent over time.
# This method will work with both gaps in a time series and missing "tails" at either end of the
# time series. The estimated values will only be applied to missing values of the selected iso3c
# and within the restricted years.
bridge_regression_func <- function(df = pd, iso, variable_missing, variable_complete,
                                   restricted = c(1946:2019)){
  
  # convert text variable inputs to symbols
  variable_missing <- rlang::sym(variable_missing)
  variable_complete <- rlang::sym(variable_complete)
  
  # construct linear model formula
  model_formula <- as.formula(paste(variable_missing, "~", variable_complete))
  
  df2 <- df %>%
    dplyr::filter(
      # restrict to relevant country
      iso3c == iso,
      # filter out missing data from the time series to be approximated
      !is.na(!!variable_missing),
      !is.na(!!variable_complete)
    ) %>%
    dplyr::arrange(year)
  
  # fit the model
  fit <- lm(model_formula, data = df2)
  
  # apply the model estimates to missing data, within the iso3c and year restrictions
  df <- df %>%
    dplyr::mutate(
      !!paste0(variable_missing,".est.method") := ifelse(is.na(!!variable_missing) & iso3c == iso & year %in% restricted,
                                                         paste0("Bridge Regression with ", variable_complete), 
                                                         !!sym(paste0(variable_missing, ".est.method"))),
      !!variable_missing := ifelse(is.na(!!variable_missing) & iso3c == iso & year %in% restricted,
                                   predict(fit, newdata = .), 
                                   !!variable_missing)
    )
  
  return(df)
  
}


## GROWTH CHAINING FUNCTION ##

# The Growth Chaining function estimates missing values from a time series ('variable_missing') by
# applying the growth rate from a second time series ('variable_complete') based on the nearest
# year in which both time series have full data. If the years to be estimated are after the year of
# reference, base_year_lower should be used; if the years are before, base_year_higher should be
# used. For estimating missing data with data on both sides, both base_year_higher and
# base_year_lower should be used; estimated values will be averaged. The estimated values will only
# be applied to missing values of the selected iso3c and within the restricted years.
growth_chaining_func <- function(df = pd, iso, variable_missing, variable_complete,
                                 base_year_lower = NULL, base_year_higher = NULL,
                                 restricted = c(1946:2019)){
  
  # convert text variable inputs to symbols
  variable_missing <- rlang::sym(variable_missing)
  variable_complete <- rlang::sym(variable_complete)
  
  # restrict to relevant country
  df2 <- df %>%
    dplyr::filter(iso3c == iso) %>%
    dplyr::arrange(year)
  
  # calculate values for lower baseline, if present
  if(!is.null(base_year_lower)){
    
    # pull base values
    missing_baseline_lower <- df2 %>%
      dplyr::filter(year == base_year_lower) %>%
      dplyr::pull(!!variable_missing)
    
    complete_baseline_lower <- df2 %>%
      dplyr::filter(year == base_year_lower) %>%
      dplyr::pull(!!variable_complete)
    
    df_estimates_lower <- df2 %>%
      dplyr::mutate(estimate_lower = ifelse(is.na(!!variable_missing) & iso3c == iso & year %in% restricted,
                                            missing_baseline_lower * !!variable_complete / complete_baseline_lower,
                                            NA)) %>%
      dplyr::select(iso3c, year, estimate_lower)
    
    # rename if these are the final estimates
    if(is.null(base_year_higher)){
      
      df_estimates <- df_estimates_lower %>%
        dplyr::rename(estimate = estimate_lower) %>%
        dplyr::select(iso3c, year, estimate)
      
    }
    
  }
  
  # calculate values for higher baseline, if present
  if(!is.null(base_year_higher)){
    
    # pull base values
    missing_baseline_higher <- df2 %>%
      dplyr::filter(year == base_year_higher) %>%
      dplyr::pull(!!variable_missing)
    
    complete_baseline_higher <- df2 %>%
      dplyr::filter(year == base_year_higher) %>%
      dplyr::pull(!!variable_complete)
    
    df_estimates_higher <- df2 %>%
      dplyr::mutate(estimate_higher = ifelse(is.na(!!variable_missing) & iso3c == iso & year %in% restricted,
                                             missing_baseline_higher * !!variable_complete / complete_baseline_higher,
                                             NA)) %>%
      dplyr::select(iso3c, year, estimate_higher)
    
    # rename if these are the final estimates
    if(is.null(base_year_lower)){
      
      df_estimates <- df_estimates_higher %>%
        dplyr::rename(estimate = estimate_higher) %>%
        dplyr::select(iso3c, year, estimate)
      
    }
    
  }
  
  # if both lower and higher baselines are present, average the estimates
  # TODO: test
  if(!is.null(base_year_lower) & !is.null(base_year_higher)){
    
    df_estimates <- dplyr::full_join(df_estimates_lower, df_estimates_higher,
                                     by = c("iso3c", "year")) %>%
      dplyr::mutate(estimate = (estimate_lower + estimate_higher) / 2) %>%
      dplyr::select(iso3c, year, estimate)
    
  }
  
  # estimation method text
  if((!is.null(base_year_lower) & is.null(base_year_higher)) |
     is.null(base_year_lower) & !is.null(base_year_higher)){
    
    message <- paste0("Growth Chaining with ", variable_complete, " from ", c(base_year_lower, base_year_higher))
    
  } else if(!is.null(base_year_lower) & !is.null(base_year_higher)){
    
    message <- paste0("Growth Chaining with ", variable_complete, " from ", base_year_lower, " and ", base_year_higher, ", averaged")
    
  }
  
  # fill estimates
  df <- df %>%
    dplyr::left_join(df_estimates, by = c("iso3c", "year")) %>%
    dplyr::mutate(
      !!paste0(variable_missing,".est.method") := ifelse(is.na(!!variable_missing) & iso3c == iso & year %in% restricted,
                                                         paste0(message), 
                                                         !!sym(paste0(variable_missing, ".est.method"))),
      !!variable_missing := ifelse(is.na(!!variable_missing) & iso3c == iso & year %in% restricted,
                                   estimate,
                                   !!variable_missing
      )) %>%
    dplyr::select(-estimate)
  
  return(df)
  
}


## KALMAN FUNCTION ##

# Kalman (State-Space Model)
kalman_end_func <- function(df = pd, iso, variable_missing, variable_complete,
                            restricted = c(1946:2019)){
  
  # convert text variable inputs to symbols
  variable_missing_ts <- rlang::sym(variable_missing)
  variable_complete_ts <- rlang::sym(variable_complete)
  
  # restrict to relevant country
  df2 <- df %>%
    dplyr::filter(
      iso3c == iso,
      !is.na(!!variable_complete_ts)) %>%
    dplyr::arrange(year)
  
  # create model
  # variable_missing_t = State_t + beta*variable_complete_t + Error
  # H = Measurement noise; Q = Process (State) noise
  model_formula <- as.formula(paste(variable_missing, "~ SSMregression(~", variable_complete, ", data = df2) + SSMtrend(1, Q = NA)"))
  model_spec <- KFAS::SSModel(model_formula, data = df2, H = NA)
  
  # fit model and smooth
  # fitSSM finds the optimal variance for the 'state' (Q) and 'noise' (H)
  fit <- KFAS::fitSSM(model_spec, inits = c(0, 0))
  kfs_out <- KFAS::KFS(fit$model)
  
  df_estimates <- df2 %>%
    mutate(
      # extract smoothed estimates
      # muhat is the "best guess" for the state at every time point
      estimated_value = as.numeric(kfs_out$muhat),
      # !!paste0(variable_missing, "_se") := sqrt(as.numeric(kfs_out$V_mu))
    ) %>%
    dplyr::select(iso3c, year, estimated_value)
  
  # fill estimates
  df <- df %>%
    dplyr::left_join(df_estimates, by = c("iso3c", "year")) %>%
    dplyr::mutate(
      !!paste0(variable_missing_ts,".est.method") := ifelse(is.na(!!variable_missing_ts) & iso3c == iso & year %in% restricted,
                                                            paste0("Kalman Model with ", variable_complete), 
                                                            !!sym(paste0(variable_missing, ".est.method"))),
      !!variable_missing_ts := ifelse(is.na(!!variable_missing_ts) & iso3c == iso & year %in% restricted,
                                      estimated_value,
                                      !!variable_missing_ts)
    ) %>%
    dplyr::select(-estimated_value)
  
  return(df)
  
}


## TIME SERIES CORRELATION ##

ts_cor <- function(df = pd, iso, ts1, ts2, years = c(1949:2019)){
  
  tmp <- df %>%
    dplyr::filter(
      iso3c == iso,
      year %in% years
    ) %>%
    dplyr::arrange(year)
  
  series1 <- tmp %>%
    dplyr::pull(ts1)
  
  series2 <- tmp %>%
    dplyr::pull(ts2)
  
  cor <- cor(series1, series2)
  
  return(cor)
  
}
