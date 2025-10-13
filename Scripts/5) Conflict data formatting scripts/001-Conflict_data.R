
# This script formats UCDP's conflict dataset version 20.1.


### load libraries ---------------------------------------------------------------------------------
library(readxl)
library(countrycode)
library(dplyr)


### load data file ---------------------------------------------------------------------------------
ucdp_original <- read.csv("Data files/Raw data files/ucdp-prio-acd-201.csv")
# ucdp_original <- readxl::read_xlsx("Data files/Raw data files/UcdpPrioConflict_v25_1.xlsx")


### format data ------------------------------------------------------------------------------------
ucdp <- ucdp_original %>%
  # Remove colonial/imperial wars and interstate conflict
  dplyr::filter(type_of_conflict > 2) %>%
  # drop unneeded variables and rename
  dplyr::select(confid = conflict_id, country = side_a, year, intensity = intensity_level,
                initial_start_date = start_date, episode_start_date = start_date2,
                end_date = ep_end_date, ucdp_region = region) %>%
  dplyr::mutate(
    country = gsub(pattern = "Government of ", replacement = "", country),
    confid = as.character(confid),
    # replace blank end_date values with NA
    end_date = ifelse(end_date == "", NA, end_date),
    # using the countrycode package, add iso3c based on country name
    iso3c = dplyr::case_when(
      country == "Congo"~ "COG",
      country == "Russia (Soviet Union)" & year < 1991 ~ "SOV",
      country == "Russia (Soviet Union)" & year >= 1991 ~ "RUS",
      country == "Serbia (Yugoslavia)" & year < 1992 ~ "YUG",
      country == "Serbia (Yugoslavia)" & year >= 1992 ~ "SRB",
      country == "South Vietnam" ~ "RVN",
      country == "South Yemen" ~ "YPR",
      country == "Yemen (North Yemen)" & year < 1991 ~ "YAR",
      country == "Yemen (North Yemen)" & year >= 1991 ~ "YEM",
      # collapse the state of Hyderabad into India
      country == "Hyderabad" ~ "IND",
      # multilateral coalitions fighting against a non-stage group - code iso3c as NA
      country == "Australia, United Kingdom, United States of America" ~ NA,
      country == "Egypt, Iraq, Jordan, Lebanon, Syria" ~ NA,
      country == "United Kingdom, United States of America" ~ NA,
      .default = countrycode::countrycode(country, "country.name", "iso3c", warn = FALSE)
    )) %>%
  dplyr::relocate(iso3c, .before = country) %>%
  dplyr::select(-country) %>%
  dplyr::mutate(
    # recode iso3c codes for select conflict ids
    iso3c = dplyr::case_when(
      confid == "210" ~ "EST",
      confid == "211" ~ "LVA",
      confid == "212" ~ "LTU",
      confid == "213" ~ "UKR",
      confid == "230" ~ "YAR",
      confid == "376" ~ "AZE",
      confid == "377" ~ "AZE",
      confid == "384" ~ "SVN",
      confid == "385" ~ "HRV",
      confid == "402" ~ "YPR",
      confid == "412" ~ "SRB",
      confid == "13645" ~ "YAR",
      .default = iso3c
    ),
    # using the countrycode package, add country name based on iso3c code
    country = dplyr::case_when(
      iso3c == "RVN" ~ "South Vietnam",
      iso3c == "YAR" ~ "North Yemen",
      iso3c == "YPR" ~ "South Yemen",
      iso3c == "SOV" ~ "Soviet Union",
      iso3c == "YUG" ~ "Yugoslavia",
      .default = countrycode::countrycode(iso3c,"iso3c","country.name")
      ),
    # recode numeric-coded variables to string values
    ucdp_region = dplyr::case_match(
      ucdp_region,
      "1" ~ "Europe",
      "2" ~ "Middle East",
      "3" ~ "Asia",
      "4" ~ "Africa",
      "5" ~ "Americas",
      .default = NA
    ),
    # recast time variable values as time instead of character
    initial_start_date = as.POSIXct(initial_start_date, origin = "1970-01-01", tz = "UTC"),
    episode_start_date = as.POSIXct(episode_start_date, origin = "1970-01-01", tz = "UTC"),
    end_date = as.POSIXct(end_date, origin = "1970-01-01", tz = "UTC")
    ) %>%
  dplyr::relocate(country, .after = iso3c) %>%
  dplyr::arrange(confid, year) %>%
  # expand dataset to have entries for each conflict id for all years 1946-2019
  dplyr::full_join(expand.grid(confid = unique(ucdp$confid), year = c(1946:2019))) %>%
  dplyr::mutate(
    # create binary conflict variable based on intensity variable from source dataset
    conflict = ifelse(!is.na(intensity), 1, 0),
    # fill entries without intensity coding as 0
    intensity = ifelse(is.na(intensity), 0, intensity)
    ) %>%
  dplyr::arrange(confid, year)


### fill short peace gaps --------------------------------------------------------------------------
# replaces one-year peace intervals with continued conflict, per literature standard
ucdp2 <- data.frame()

for(i in unique(ucdp$confid)){
  
  tmp <- ucdp %>%
    dplyr::filter(confid == i) %>%
    dplyr::arrange(year) %>%
    dplyr::mutate(conflict_recoded = 0)
  
  for(j in 2:(nrow(tmp) - 1)){
    
    # if the preceding and proceding year are both coded as a conflict...
    if(tmp$conflict[j - 1] == 1 & tmp$conflict[j + 1] == 1){
      
      if(tmp$conflict[j] == 0){
        
        tmp$conflict_recoded[j] <- 1
        
      }
      
      # ... code year as conflict
      tmp$conflict[j] <- 1
      
    }
  }
  
  ucdp2 <- rbind(ucdp2, tmp)
  
}


### create peace/conflict groupings ----------------------------------------------------------------
ucdp2 <- ucdp2 %>%
  dplyr::group_by(confid) %>%
  dplyr::mutate(
    group_id = cumsum(conflict != dplyr::lag(conflict) | is.na(lag(conflict)))
  )


### format full conflict dataset -------------------------------------------------------------------
# pull iso3c, country, initial_start_date, and ucdp_region for each confid
confid_data <- ucdp2 %>%
  dplyr::group_by(confid) %>%
  dplyr::summarise(
    iso3c = max(iso3c, na.rm = TRUE),
    country = max(country, na.rm = TRUE),
    initial_start_date = max(initial_start_date, na.rm = TRUE),
    ucdp_region = max(ucdp_region, na.rm = TRUE),
  )

conflict_full_data <- ucdp2 %>%
  dplyr::select(-c(iso3c, country, initial_start_date, ucdp_region)) %>%
  dplyr::left_join(confid_data, by = "confid") %>%
  dplyr::select(confid, iso3c, country, year, conflict, conflict_recoded, group_id, intensity,
                ucdp_region) %>%
  dplyr::arrange(confid, year)


### collapse by groupings --------------------------------------------------------------------------
ucdp3 <- ucdp2 %>%
  dplyr::group_by(confid, group_id) %>%
  dplyr::summarise(
    start_year = min(year, na.rm = TRUE),
    end_year = max(year, na.rm = TRUE),
    conflict = first(conflict, na.rm = TRUE),
    intensity = max(intensity, na.rm = TRUE),
    episode_start_date = min(episode_start_date, na.rm = TRUE),
    end_date = max(end_date, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  dplyr::ungroup() %>%
  # merge in confid-wide variables
  dplyr::left_join(confid_data, by = "confid") %>%
  dplyr::mutate(
    # fix -Inf values
    dplyr::across(dplyr::everything(), .fns = ~ replace(., is.infinite(.), NA)),
    # calculate additional metrics
    episode_length_in_months = as.numeric(lubridate::interval(episode_start_date, end_date) /
                                            base::months(1)),
    episode_length_in_years = end_year - start_year,
    total_conflict_length_in_months = as.numeric(lubridate::interval(initial_start_date, end_date) /
                                                   base::months(1)),
    years = paste0(start_year, "–", end_year),
    length = end_year - start_year + 1
    ) %>%
  dplyr::arrange(confid, start_year) %>%
  dplyr::select(confid, iso3c, country, start_year, end_year, years, length, conflict, group_id,
                ucdp_region, intensity, initial_start_date, episode_start_date, end_date,
                episode_length_in_months, episode_length_in_years, total_conflict_length_in_months)


### write data -------------------------------------------------------------------------------------
# writes formatted dataframes as csv files

# full conflict data by confid-year
write.csv(conflict_full_data,
          "Data files/Formatted data files/conflict_full_data.csv",
          row.names = FALSE)

# collapsed conflict data by confid episode
write.csv(ucdp3,
          "Data files/Formatted data files/conflict_table.csv",
          row.names = FALSE)
