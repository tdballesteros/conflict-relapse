
# These custom functions are used for the 005-Population script to estimate missing population
# values for one or all of the datasets. Estimates are applied to a new column to retain the
# original data.

# All countries extant from 1946-1949 and 2019 will need population estimates approximated.
# Additional estimates are needed on a case-by-case basis.


### estimate functions ----------------------------------------------------------------------
# this function is used to estimate cow.pop based on the relative difference in the population between two
# years within un.pop and applying the proportion to the cow.pop data
pop_growth_estimator_cow_func <- function(df = pd, iso, yr = 1950, restricted = c(1946:2019)){
  
  # the UN baseline to estimate the proportions from
  baseline <- df$un.pop[df$iso3c==iso&df$year==yr]
  
  # the COW relative estimate to base the proportions from
  relative <- df$cow.pop[df$iso3c==iso&df$year==yr]
  
  df <- df %>%
    dplyr::mutate(prop = relative * un.pop / baseline,
                  cow.pop = ifelse(iso3c==iso&is.na(cow.pop)&year %in% restricted,prop,cow.pop)) %>%
    dplyr::select(-prop)
  
  return(df)
  
}

# this function is used to estimate un.pop based on the relative difference in the population between two
# years within cow.pop and applying the proportion to the un.pop data
pop_growth_estimator_un_func <- function(df = pd, iso, yr = 1950, restricted = c(1946:2019)){
  
  # the COW baseline to estimate the proportions from
  baseline <- df$cow.pop[df$iso3c==iso&df$year==yr]
  
  # the UN relative estimate to base the proportions from
  relative <- df$un.pop[df$iso3c==iso&df$year==yr]
  
  df <- df %>%
    dplyr::mutate(prop = relative * cow.pop / baseline,
                  un.pop = ifelse(iso3c==iso&is.na(un.pop)&year %in% restricted,prop,un.pop)) %>%
    dplyr::select(-prop)
  
  return(df)
  
}

# this function is used to estimate un.pop and cow.pop based on the growth rates in the population in the mpd dataset
# and applying to the un.pop and cow.pop estimates
pop_growth_estimator_mpd_func <- function(df = pd, df2 = mpd, iso, yr = 1950, restricted = c(1946:1949)){
  
  # the MPD baseline to estimate the proportions from
  baseline <- df2$pop[df2$iso3c==iso&df2$year==yr]
  
  # the UN estimate to apply the proportions to
  relative.un <- df$un.pop[df$iso3c==iso&df$year==yr]
  
  # the UN estimate to apply the proportions to
  relative.cow <- df$cow.pop[df$iso3c==iso&df$year==yr]
  
  # pull the mpd data and calculate growth rates
  df2 <- df2 %>%
    dplyr::filter(iso3c == iso,
                  year %in% restricted) %>%
    dplyr::mutate(prop = pop / baseline,
                  un.pop = prop * relative.un,
                  cow.pop = prop * relative.cow,
                  un.pop.estimated = 1,
                  cow.pop.estimated = 1) %>%
    dplyr::select(iso3c,country,year,un.pop,cow.pop,un.pop.estimated,cow.pop.estimated)
  
  # append estimates to df
  df <- df %>%
    rbind(df2)
  
}

# this function approximates the 1946-1949 population of a country based on its growth rates the subsequent years.
# the function applies weighted growth rates of 1/2 year+1, 1/3 year+2, and 1/6 year+3
pop_growth_estimator_no_data_func <- function(df = pd, iso){
  
  # calculate growth rates - un
  un.growth.51 <- df$un.pop[df$iso3c==iso&df$year==1951]/df$un.pop[df$iso3c==iso&df$year==1950]
  un.growth.52 <- df$un.pop[df$iso3c==iso&df$year==1952]/df$un.pop[df$iso3c==iso&df$year==1951]
  un.growth.53 <- df$un.pop[df$iso3c==iso&df$year==1953]/df$un.pop[df$iso3c==iso&df$year==1952]
  
  un.growth.50 <- (1/2)*un.growth.51 + (1/3)*un.growth.52 + (1/6)*un.growth.53
  un.growth.49 <- (1/2)*un.growth.50 + (1/3)*un.growth.51 + (1/6)*un.growth.52
  un.growth.48 <- (1/2)*un.growth.49 + (1/3)*un.growth.50 + (1/6)*un.growth.51
  un.growth.47 <- (1/2)*un.growth.48 + (1/3)*un.growth.49 + (1/6)*un.growth.50
  
  # calculate growth rates - cow
  cow.growth.51 <- df$cow.pop[df$iso3c==iso&df$year==1951]/df$cow.pop[df$iso3c==iso&df$year==1950]
  cow.growth.52 <- df$cow.pop[df$iso3c==iso&df$year==1952]/df$cow.pop[df$iso3c==iso&df$year==1951]
  cow.growth.53 <- df$cow.pop[df$iso3c==iso&df$year==1953]/df$cow.pop[df$iso3c==iso&df$year==1952]
  
  cow.growth.50 <- (1/2)*cow.growth.51 + (1/3)*cow.growth.52 + (1/6)*cow.growth.53
  cow.growth.49 <- (1/2)*cow.growth.50 + (1/3)*cow.growth.51 + (1/6)*cow.growth.52
  cow.growth.48 <- (1/2)*cow.growth.49 + (1/3)*cow.growth.50 + (1/6)*cow.growth.51
  cow.growth.47 <- (1/2)*cow.growth.48 + (1/3)*cow.growth.49 + (1/6)*cow.growth.50
  
  # add 1949
  df <- df %>%
    tibble::add_row(iso3c = iso,
                    country = countrycode::countrycode(iso3c,"iso3c","country.name"),
                    year = 1949,
                    un.pop = df$un.pop[df$iso3c==iso&df$year==1950]/un.growth.50,
                    cow.pop = df$cow.pop[df$iso3c==iso&df$year==1950]/cow.growth.50,
                    un.pop.estimated = 1,
                    cow.pop.estimated = 1)
  
  # add 1948
  df <- df %>%
    tibble::add_row(iso3c = iso,
                    country = countrycode::countrycode(iso3c,"iso3c","country.name"),
                    year = 1948,
                    un.pop = df$un.pop[df$iso3c==iso&df$year==1949]/un.growth.49,
                    cow.pop = df$cow.pop[df$iso3c==iso&df$year==1949]/cow.growth.49,
                    un.pop.estimated = 1,
                    cow.pop.estimated = 1)
  
  # add 1947
  df <- df %>%
    tibble::add_row(iso3c = iso,
                    country = countrycode::countrycode(iso3c,"iso3c","country.name"),
                    year = 1947,
                    un.pop = df$un.pop[df$iso3c==iso&df$year==1948]/un.growth.48,
                    cow.pop = df$cow.pop[df$iso3c==iso&df$year==1948]/cow.growth.48,
                    un.pop.estimated = 1,
                    cow.pop.estimated = 1)
  
  # add 1946
  df <- df %>%
    tibble::add_row(iso3c = iso,
                    country = countrycode::countrycode(iso3c,"iso3c","country.name"),
                    year = 1946,
                    un.pop = df$un.pop[df$iso3c==iso&df$year==1947]/un.growth.47,
                    cow.pop = df$cow.pop[df$iso3c==iso&df$year==1947]/cow.growth.47,
                    un.pop.estimated = 1,
                    cow.pop.estimated = 1)
  
}