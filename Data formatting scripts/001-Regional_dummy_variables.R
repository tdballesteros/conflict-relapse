# This script formats two regional dummy variables at the country level, based on country_regions.xlsx and country_regions2.xlsx.
# Continent-Region level: Americas, Asia, Europe, Middle East & North Africa (MENA), and Sub-Saharan Africa (SSA)
# Sub-Region level: Western Europe & Others Group (WEOG),	Central America,	Carribean,	South America,	Eastern Europe,	Middle East,	North Africa,	East Africa,	West Africa,	Central Africa,	Southern Africa,	Central Asia,	South Asia,	East Asia, and	Southeast Asia

# load libraries
library(readxl)
library(countrycode)
library(dplyr)

# load data files and add 0s
country_regions1 <- readxl::read_excel("Data files/country_regions.xlsx") %>%
  dplyr::mutate(across(everything(), .fns = ~replace_na(.,0)))
country_regions2 <- readxl::read_excel("Data files/country_regions2.xlsx") %>%
  dplyr::mutate(across(everything(), .fns = ~replace_na(.,0)))

# data formatting
country_regions1 <- country_regions1 %>%
  # using the countrycode package, add country name based on iso3c value
  dplyr::mutate(country = countrycode::countrycode(iso3c,origin="iso3c",destination="country.name"))

country_regions2 <- country_regions2 %>%
  # using the countrycode package, add country name based on iso3c value
  dplyr::mutate(country = countrycode::countrycode(iso3c,origin="iso3c",destination="country.name"))

# manually add country names to missing iso3c codes: ANT, BRD, DDR, KSV, RVN, SOV, YAR, YPR, YUG
country_regions1$country[country_regions1$iso3c=="ANT"] <- "Netherlands Antilles"
country_regions1$country[country_regions1$iso3c=="BRD"] <- "Germany West"
country_regions1$country[country_regions1$iso3c=="DDR"] <- "Germany East"
country_regions1$country[country_regions1$iso3c=="KSV"] <- "Kosovo"
country_regions1$country[country_regions1$iso3c=="RVN"] <- "South Vietnam"
country_regions1$country[country_regions1$iso3c=="SOV"] <- "North Vietnam"
country_regions1$country[country_regions1$iso3c=="YAR"] <- "Yemen North"
country_regions1$country[country_regions1$iso3c=="YPR"] <- "Yemen South"
country_regions1$country[country_regions1$iso3c=="YUG"] <- "Yugoslavia"

country_regions2$country[country_regions2$iso3c=="ANT"] <- "Netherlands Antilles"
country_regions2$country[country_regions2$iso3c=="BRD"] <- "Germany West"
country_regions2$country[country_regions2$iso3c=="DDR"] <- "sGermany East"
country_regions2$country[country_regions2$iso3c=="KSV"] <- "Kosovo"
country_regions2$country[country_regions2$iso3c=="RVN"] <- "South Vietnam"
country_regions2$country[country_regions2$iso3c=="SOV"] <- "North Vietnam"
country_regions2$country[country_regions2$iso3c=="YAR"] <- "Yemen North"
country_regions2$country[country_regions2$iso3c=="YPR"] <- "Yemen South"
country_regions2$country[country_regions2$iso3c=="YUG"] <- "Yugoslavia"