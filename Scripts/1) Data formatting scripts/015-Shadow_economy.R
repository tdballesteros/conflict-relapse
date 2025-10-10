

### load libraries ---------------------------------------------------------------------------------
library(readxl)
library(countrycode)
library(imputeTS)
library(dplyr)

### load data file ---------------------------------------------------------------------------------
qog <- read.csv("Data files/Raw data files/qog_std_ts_jan20.csv")

# Ceyhun and Oguz - not needed
# Elgin and Oztunali - in progress
# Medina and Schneider - done
# Schneider, Buehn and Montenegro - needs to be QC'd
# Schneider and Klinglmair - ID'd source
# Hassan and Schneider - ID'd source
# Buehn and Schneider - ID'd source
# Kelmanson et al - ID'd source
# "size_of_shadow_economies_around_the_world" - ID'd source
# Schneider (shadeconomycorruption_july2007.pdf) - ID'd source

### format data ------------------------------------------------------------------------------------
se <- qog %>%
  dplyr::select(cname, year, shec_se) %>%
  dplyr::mutate(
    # using the countrycode package, add iso3c based on country name
    iso3c = dplyr::case_when(
      cname == "Czechoslovakia" ~ "CZE",
      cname == "Germany, East" ~ "DDR",
      cname == "Micronesia" ~ "FSM",
      cname == "Serbia and Montenegro" ~ "SRB",
      cname == "Tibet" ~ "TBT",
      cname == "Vietnam, South" ~ "RVN",
      cname == "Yemen, North" ~ "YAR",
      cname == "Yemen, South" ~ "YPR",
      cname == "Yugoslavia" ~  "YUG",
      .default = countrycode::countrycode(cname, "country.name", "iso3c")
      ),
    # using the countrycode package, add country name based on iso3c code
    country = dplyr::case_when(
      cname == "Czechoslovakia" ~ "Czechia",
      cname == "Germany, East" ~ "East Germany",
      cname == "Micronesia" ~ "Micronesia (Federated States of)",
      cname == "Serbia and Montenegro" ~ "Serbia and Montenegro",
      cname == "Tibet" ~ "Tibet",
      cname == "Vietnam, South" ~ "South Vietnam",
      cname == "Yemen, North" ~ "North Yemen",
      cname == "Yemen, South" ~ "South Yemen",
      cname == "Yugoslavia" ~ "Yugoslavia",
      .default = countrycode::countrycode(iso3c, "iso3c", "country.name")
    )) %>%
  # filter out non-sovereign entities
  dplyr::filter(iso3c != "TBT") %>%
  dplyr::select(iso3c, country, year, shec_se)


#### fill in missing values from MS using method done on spreadsheet ####
se_filled <- read_excel("~/Documents/se_estimates_upload.xlsx") %>%
  dplyr::select(-source_method) %>%
  dplyr::mutate(country = NA)

se <- se %>%
  na.omit() %>%
  rbind(se_filled)


# IRQ
# Arab Watch on Economic and Social Rights: Informal Employment (2016) by ANND
# 2014 shadow economy is estimated at 19.4%

# AFG
# https://tolonews.com/business/afghanistan’s-underground-economy-thriving-cso accessed 15 july 2020

# cleaning
se <- se %>%
  select(iso3c, year, shec_se) %>%
  rename(shec = shec_se) %>%
  na.omit()


### write data -------------------------------------------------------------------------------------
# writes formatted dataframe as csv files
write.csv(se, "Data files/Formatted data files/shadow_economy.csv", row.names = FALSE)
