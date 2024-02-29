

### load libraries ----------------------------------------------------------------------
library(readxl)
library(countrycode)
library(imputeTS)
library(dplyr)

### load data file ----------------------------------------------------------------------
qog <- read.csv("Data files/Raw data files/qog_std_ts_jan20.csv")

### format data ----------------------------------------------------------------------
se <- qog %>%
  dplyr::select(cname,year,shec_se) %>%
  # using the countrycode package, add iso3c based on country name
  dplyr::mutate(iso3c = countrycode::countrycode(cname,"country.name","iso3c"),
                # using the countrycode package, add country name based on iso3c code
                country = countrycode::countrycode(iso3c,"iso3c","country.name")) 

# add missing iso3c codes
se$iso3c[se$cname=="Czechoslovakia"] <- "CZE"
se$iso3c[se$cname=="Germany, East"] <- "DDR"
se$iso3c[se$cname=="Micronesia"] <- "FSM"
se$iso3c[se$cname=="Serbia and Montenegro"] <- "SRB"
se$iso3c[se$cname=="Tibet"] <- "TBT"
se$iso3c[se$cname=="Vietnam, South"] <- "RVN"
se$iso3c[se$cname=="Yemen, North"] <- "YAR"
se$iso3c[se$cname=="Yemen, South"] <- "YPR"
se$iso3c[se$cname=="Yugoslavia"] <- "YUG"

# add missing country names
se$country[se$cname=="Czechoslovakia"] <- "Czechia"
se$country[se$cname=="Germany, East"] <- "East Germany"
se$country[se$cname=="Micronesia"] <- "Micronesia (Federated States of)"
se$country[se$cname=="Serbia and Montenegro"] <- "Serbia and Montenegro"
se$country[se$cname=="Tibet"] <- "Tibet"
se$country[se$cname=="Vietnam, South"] <- "South Vietnam"
se$country[se$cname=="Yemen, North"] <- "North Yemen"
se$country[se$cname=="Yemen, South"] <- "South Yemen"
se$country[se$cname=="Yugoslavia"] <- "Yugoslavia"

# filter out non-sovereign entities
se <- se %>%
  dplyr::filter(iso3c != "TBT") %>%
  dplyr::select(iso3c,country,year,shec_se)


#### fill in missing values from MS using method done on spreadsheet ####
se_filled <- read_excel("~/Documents/se_estimates_upload.xlsx") %>%
  select(-source_method)

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
  select(iso3c,year,shec_se) %>%
  rename(shec = shec_se) %>%
  na.omit()
