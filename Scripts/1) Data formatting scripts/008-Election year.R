
# This script creates three binary variables coding if a national (presidential/parliamentary)
# election was held in a country in a given year, in the previous year, and in the subsequent
# year. Note that not all elections are scheduled a year in advance, so the predictive power
# of an election in the subsequent year should be used with caution.


### load libraries ---------------------------------------------------------------------------------
library(readxl)
library(countrycode)
library(dplyr)


### load data file ---------------------------------------------------------------------------------
elec <- readxl::read_xls("~/Downloads/NELDA 6.0/NELDA.xls")


### data formatting --------------------------------------------------------------------------------
elec <- elec %>%
  dplyr::rename(country_name = country) %>%
  dplyr::mutate(
    # using the countrycode package, add iso3c code based on country name
    iso3c = dplyr::case_when(
      country_name == "Abkhazia" ~ "ABK",
      country_name == "Czechoslovakia" ~ "CZE",
      country_name == "East Germany" ~ "DDR",
      country_name == "Kosovo" ~ "KSV",
      country_name == "Republic of Vietnam" ~ "RVN",
      country_name == "South Ossetia" ~ "SOT",
      country_name == "South Yemen" ~ "YPR",
      .default = countrycode::countrycode(country_name, "country.name", "iso3c")
    ),
    # using the countrycode package, add country name based on iso3c code to standardize country
    # names
    country = dplyr::case_when(
      iso3c == "ABK" ~ "Abkhazia",
      iso3c == "DDR" ~ "East Germany",
      iso3c == "KSV" ~ "Kosovo",
      iso3c == "RVN" ~ "South Vietnam",
      iso3c == "SOT" ~ "South Ossetia",
      iso3c == "YPR" ~ "South Yemen",
      .default = countrycode::countrycode(iso3c, "iso3c", "country.name")
    )) %>%
  dplyr::select(iso3c, country, year) %>%
  unique() %>%
  # dplyr::filter(year <= 2019) %>%
  dplyr::mutate(
    # format select country's iso3c codes
    iso3c = dplyr::case_when(
      # format Soviet Union
      iso3c == "RUS" & year %in% c(1946:1990) ~ "SOV",
      # format West Germany
      iso3c == "DEU" & year %in% c(1946:1989) ~ "BRD",
      # format North Yemen
      iso3c == "YEM" & year %in% c(1946:1990) ~ "YAR",
      # format Yugoslavia
      iso3c == "SRB" & year %in% c(1946:1991) ~ "YUG",
      .default = iso3c
    ),
    # format select country's names
    country = dplyr::case_when(
      # format Soviet Union
      iso3c %in% c("RUS", "SOV") & year %in% c(1946:1990) ~ "Soviet Union",
      # format Czechoslovakia
      iso3c == "CZE" & year %in% c(1946:1992) ~ "Czechoslovakia",
      # format West Germany
      iso3c %in% c("DEU", "BRD") & year %in% c(1946:1989) ~ "West Germany",
      # format North Yemen
      iso3c %in% c("YEM", "YAR") & year %in% c(1946:1990) ~ "North Yemen",
      # format Yugoslavia
      iso3c %in% c("SRB", "YUG") & year %in% c(1946:1991) ~ "Yugoslavia",
      .default = country
    ))


### election dummy variable ------------------------------------------------------------------------
elec <- elec %>%
  # create dummy variable for years where a national parliament/presidential election was held
  dplyr::mutate(natelec = 1) %>%
  dplyr::full_join(expand.grid(iso3c = unique(elec$iso3c), year = c(1945:2019))) %>%
  dplyr::mutate(natelec = ifelse(is.na(natelec), 0, natelec)) %>%
  unique()


### prior/next year election dummy variable --------------------------------------------------------
# create dummy variables for national election happening in the next year (natelec.n) and
# happened in the prior year (natelec.l)
elec_next <- elec %>%
  dplyr::mutate(year = year - 1) %>%
  dplyr::rename(natelec.n = natelec) %>%
  dplyr::select(-country)

elec_last <- elec %>%
  dplyr::mutate(year = year + 1) %>%
  dplyr::rename(natelec.l = natelec)

# join next and prior year election variables with main dataset
elec <- elec %>%
  dplyr::select(-country) %>%
  dplyr::full_join(elec_next, by = c("iso3c", "year")) %>%
  dplyr::full_join(elec_last, by = c("iso3c", "year")) %>%
  dplyr::mutate(
    natelec.n = ifelse(is.na(natelec.n), 0, natelec.n),
    natelec.l = ifelse(is.na(natelec.l), 0, natelec.l)
    ) %>%
  # remove 2020 and 2021 - the future
  dplyr::filter(year < 2020) %>%
  dplyr::select(-country)


### expand dataset ---------------------------------------------------------------------------------
elec <- elec %>%
  # expand dataframe to have all iso3c-year combos from 1946 - 2019
  dplyr::full_join(expand.grid(iso3c = unique(elec$iso3c), year = c(1945:2019))) %>%
  dplyr::mutate(
    # using the countrycode package, add country name based on iso3c code to standardize country
    # names
    country = dplyr::case_when(
      iso3c == "ABK" ~ "Abkhazia",
      iso3c == "BRD" ~ "West Germany",
      iso3c == "DDR" ~ "East Germany",
      iso3c == "KSV" ~ "Kosovo",
      iso3c == "RVN" ~ "South Vietnam",
      iso3c == "SOT" ~ "South Ossetia",
      iso3c == "SOV" ~ "Soviet Union",
      iso3c == "YAR" ~ "North Yemen",
      iso3c == "YPR" ~ "South Yemen",
      iso3c == "YUG" ~ "Yugoslavia",
      .default = countrycode::countrycode(iso3c,"iso3c","country.name")
      )) %>%
      dplyr::arrange(iso3c,year) %>%
  
  # filter out year 1944
  dplyr::filter(year > 1944)


### years since last election ----------------------------------------------------------------------
# pull all iso3c codes in the dataset
elec_iso_list <- elec %>%
  dplyr::select(iso3c) %>%
  unique() %>%
  dplyr::pull(iso3c)

# create groupings - an election year or a string of consecutive non-election years
# create an empty placeholder dataframe
elec.df1 <- data.frame()

for(iso in elec_iso_list){
  
  df <- elec %>% #elec_yrs_since %>%
    dplyr::filter(iso3c == iso)
  
  # pull minimum year
  min_yr <- min(df$year)
  
  # start grouping at minimum year with 1
  df$grouping[df$year==min_yr] <- 1
  
  
  for(y in (min_yr+1):2019){
    
    # if year is an election year or the election year binary code differs between this year and the
    # prior year, code as a new grouping
    if((df$natelec[df$iso3c == iso & df$year == y] == 1) |
       (df$natelec[df$iso3c == iso & df$year == y] != df$natelec[df$iso3c == iso & df$year == (y - 1)])){
      
      df$grouping[df$iso3c == iso & df$year == y] <- df$grouping[df$iso3c == iso & df$year == (y - 1)] + 1
      
    } else{
      
      # if year is not an election year and prior year was coded the same, code as same grouping
      df$grouping[df$iso3c == iso & df$year == y] <- df$grouping[df$iso3c == iso & df$year == (y - 1)]
      
    }
    
  }
  
  elec.df1 <- rbind(elec.df1, df)
  
}

# create list of iso3c-groupings to run the for loop through
elec.list <- elec.df1 %>%
  dplyr::select(iso3c, grouping) %>%
  unique()

# create an empty placeholder dataframe
elec.df2 <- data.frame()

for(i in 1:nrow(elec.list)){
  
  df <- elec.df1 %>%
    dplyr::filter(iso3c == elec.list$iso3c[i],
                  grouping == elec.list$grouping[i]) %>%
    dplyr::arrange(year)
  
  df$years_since_last_elec <- row.names(df)
  
  elec.df2 <- rbind(elec.df2, df)
  
}

elec <- elec %>%
  dplyr::left_join(elec.df2 %>%
                     dplyr::select(-c(natelec, natelec.n, natelec.l, country, grouping)),
                   by=c("iso3c", "year")) %>%
  dplyr::relocate(country, .after = iso3c)


### add missing countries --------------------------------------------------------------------------
# ERI: code no elections, but code years since last election as years since 1993 (independence
# referendum)
elec_eri <- data.frame(
  iso3c = rep("ERI", 27),
  country = rep("Eritrea", 27),
  year = c(1993:2019),
  natelec = rep(0, 27),
  natelec.n = rep(0, 27),
  natelec.l = rep(0, 27),
  years_since_last_elec = c(0:26)
)

# CHN: code no elections (all national elections are indirect), but code years since last election
# as years since 1947 (parliamentary election before Communist victory)
elec_chn <- data.frame(
  iso3c = rep("CHN", 73),
  country = rep("China", 73),
  year = c(1947:2019),
  natelec = rep(0, 73),
  natelec.n = rep(0, 73),
  natelec.l = rep(0, 73),
  years_since_last_elec = c(0:72)
)

# SAU:recode years since last elections as years since creation of the Kingdom of Saudi Arabia
# (1932)
elec <- elec %>%
  dplyr::mutate(
    years_since_last_elec = dplyr::case_when(
      iso3c == "SAU" ~ as.numeric(years_since_last_elec) + 12,
      .default = as.numeric(years_since_last_elec)
    )
  )

# bind data
elec <- elec %>%
  rbind(elec_eri, elec_chn)


### write data -------------------------------------------------------------------------------------
# writes formatted dataframe as csv files
write.csv(elec, "Data files/Formatted data files/elections.csv", row.names = FALSE)
