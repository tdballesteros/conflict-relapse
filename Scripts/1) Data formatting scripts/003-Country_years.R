
# This script formats data on the existence as a sovereign entity of each country-year and
# calculates how many years the country has been independent.


### load libraries ---------------------------------------------------------------------------------
library(countrycode)
library(dplyr)
library(tibble)


### load data files --------------------------------------------------------------------------------
cyears <- read.csv("Data files/Raw data files/states2016.csv")


### format data ------------------------------------------------------------------------------------
cyears <- cyears %>%
  # using the countrycode package, add iso3c based on country COW abbreviation
  dplyr::mutate(
    iso3c = dplyr::case_when(
      stateabb == "RVN" ~ "RVN",
      stateabb == "YPR" ~ "YPR",
      stateabb == "YAR" ~ "YAR",
      stateabb == "ZAN" ~ "ZAN",
      stateabb == "KOS" ~ "KSV",
      stateabb == "YUG" ~ "YUG",
      stateabb == "CZE" ~ "CZE",
      stateabb == "GDR" ~ "DDR",
      stateabb == "GFR" ~ "BRD",
      .default = countrycode::countrycode(stateabb, "cowc", "iso3c")
      )
    ) %>%
  # rearrange variables
  dplyr::relocate(iso3c, .before = stateabb) %>%
  # filter countries who ceased existing before 1946
  dplyr::filter(endyear > 1946)


### recode years of independence -------------------------------------------------------------------

# recode MKD independence year as 1992 - 8 April 1993 date references when Greece allowed
# recognition of an independent country under the Former Yugoslav Republic of Macedonia name; SVN,
# HRV, BIH all coded as becoming independend in 1992
cyears$styear[cyears$iso3c=="MKD"] <- 1992

# recode AND's 1993 independence date with 1814 - independence from Napoleon's France
cyears$styear[cyears$iso3c=="AND"] <- 1814

# recode MCO's 1993 independence date with 1861 - Franco-Monégasque Treaty of 1861
cyears$styear[cyears$iso3c=="MCO"] <- 1861

# recode SMR's 1992 independence date with 1740 - independence restored from Ravenna's Legate
# Alberoni
cyears$styear[cyears$iso3c=="SMR"] <- 1861

# recode LIE's 1990 independence date with 1866 - end of German Confederation
cyears$styear[cyears$iso3c=="LIE"] <- 1866

# recode ZAN's 1990 end date with 1964 - change to 1963, as the merger happened in April 1964
cyears$endyear[cyears$iso3c=="ZAN"] <- 1963


### calculate year of independence -----------------------------------------------------------------
# note: CZE and SYR have two different dates
# CZE: 1945 for Czechoslovakia; 1993 for Czechia
# SYR: 1946 independence from France; 1961 dissolution of the United Arab Republic [UAR is coded as
# Egypt]
cyears_independence <- cyears %>%
  dplyr::select(iso3c,styear) %>%
  dplyr::filter(iso3c != "CZE" | styear == 1945,
                iso3c != "SYR" | styear == 1946) %>%
  # add SOV entry
  tibble::add_row(iso3c = "SOV", styear = 1922) %>%
  # add SRB entry
  tibble::add_row(iso3c = "SRB", styear = 1992)

# recode RUS's 1816 independence date with 1991 for independence from the Soviet Union
cyears_independence$styear[cyears_independence$iso3c=="RUS"] <- 1991

# recode USA's 1816 independence date with 1776
cyears_independence$styear[cyears_independence$iso3c=="USA"] <- 1776

# recode GBR's 1816 independence date with 1801 - the Acts of Union 1800's entrance into effect
# uniting Great Britain with Ireland
cyears_independence$styear[cyears_independence$iso3c=="GBR"] <- 1801

# recode CHE's 1816 independence date with 1648 - the Treaty of Westphalia
cyears_independence$styear[cyears_independence$iso3c=="CHE"] <- 1648

# recode ESP's 1816 independence date with 1479
cyears_independence$styear[cyears_independence$iso3c=="ESP"] <- 1479

# recode PRT's 1816 independence date with 1139
cyears_independence$styear[cyears_independence$iso3c=="PRT"] <- 1139

# recode ITA's 1816 independence date with 1861 - Italian unification
cyears_independence$styear[cyears_independence$iso3c=="ITA"] <- 1861

# recode SWE's 1816 independence date with 1397
cyears_independence$styear[cyears_independence$iso3c=="SWE"] <- 1397

# recode TUR's 1816 independence date with 1923 - establishment of the Republic of Turkey
cyears_independence$styear[cyears_independence$iso3c=="TUR"] <- 1923


### calculate as country-years ---------------------------------------------------------------------
# creates an empty dataframe to be populated
cyears2 <- data.frame(iso3c = NA, year = NA, cn = NA)

for(i in 1:nrow(cyears)){
  # creates a dataframe for the ith country with years starting with the country's first year of
  # existence to its last (or 2016, if the country currently exists)
  years_tmp <- c(cyears$styear[i]:cyears$endyear[i]) %>%
    as.data.frame() %>%
    dplyr::rename(year = 1) %>%
    dplyr::mutate(
      iso3c = cyears$iso3c[i],
      # dummy variable indicating the country existed in that specific year
      cn = 1
      ) %>%
    dplyr::relocate(iso3c, .before = year)
  
  # merges the country dataframe with the placeholder dataframe
  cyears2 <- cyears2 %>%
    rbind(years_tmp)

}

# removes first row of NAs
cyears2 <- cyears2[-1,]

# pull most recent year of data (2016) to replicate for 2017-2019
cyears16 <- cyears2 %>%
  dplyr::filter(year == 2016)

# append 2016 data with modified years for 2017-2019
cyears2 <- cyears2 %>%
  rbind(cyears16 %>%
          dplyr::mutate(year = 2017)
        ) %>%
  rbind(cyears16 %>%
          dplyr::mutate(year = 2018)
  ) %>%
  rbind(cyears16 %>%
          dplyr::mutate(year = 2019)
  )


### calculate divided country data -----------------------------------------------------------------
cyears2 <- cyears2 %>%
  dplyr::mutate(
    # YUG is coded as Yugoslavia and Serbia
    iso3c = ifelse(iso3c == "YUG" & year >= 1992, "SRB", iso3c),
    # RUS is coded as USSR and Russia
    iso3c = ifelse(iso3c == "RUS" & year %in% c(1922:1990), "SOV", iso3c)
    ) %>%
  # expands the dataset to include all iso3c codes for 1946 - 2019, if not present already
  dplyr::full_join(expand.grid(iso3c = unique(cyears2$iso3c), year = c(1946:2019))) %>%
  dplyr::mutate(
    cn = ifelse(is.na(cn), 0, cn),
    # using the countrycode package, add country name based on iso3c code
    country = dplyr::case_when(
      iso3c == "BRD" ~ "West Germany",
      iso3c == "DDR" ~ "East Germany",
      iso3c == "KSV" ~ "Kosovo",
      iso3c == "RVN" ~ "South Vietnam",
      iso3c == "SOV" ~ "Soviet Union",
      iso3c == "YAR" ~ "North Yemen",
      iso3c == "YPR" ~ "South Yemen",
      iso3c == "YUG" ~ "Yugoslavia",
      iso3c == "ZAN" ~ "Zanzibar",
      .default = countrycode::countrycode(iso3c, "iso3c", "country.name")
    )) %>%
  # rearrange variables
  dplyr::relocate(country, .after = iso3c)

# recode unified Yemen as not existing in 1990
cyears2$cn[cyears2$iso3c == "YEM" & cyears2$year == 1990] <- 0

# recode un-unified East & West Germany as not existing in 1990
cyears2$cn[cyears2$iso3c %in% c("BRD", "DDR") & cyears2$year == 1990] <- 0

# recode Syria as existing independent during the United Arab Republic years (1959/1960)
cyears2$cn[cyears2$iso3c == "SYR" & cyears2$year %in% c(1959:1960)] <- 1

# JPN, BRD, DDR, AUT are not in system during their occupation after WWII
# cyears2[cyears2$iso3c %in% c("JPN", "BRD", "DDR", "AUT") & cyears2$year %in% c(1946:1951)] <- 1


### calculate years since independence -------------------------------------------------------------
cyears2 <- cyears2 %>%
  dplyr::left_join(cyears_independence, by = "iso3c")

# adjust independence dates for specific countries
# CZE: Czechia gained independence in 1993
cyears2$styear[cyears2$iso3c=="CZE" & cyears2$year >= 1993] <- 1993

# SYR: Syria regained independence after leaving the United Arab Republic in 1961; Syria is coded as
# independent throughout the UAR's life, but for purposes of independence dates, code as initial
# year of independence from France (1946) for pre-UAR and during UAR, whereas code as 1961 after the
# UAR
cyears2$styear[cyears2$iso3c == "SYR" & cyears2$year >= 1961] <- 1961

# EGY: Egypt reverted back to independence following Syria's withdrawal from the United Arab
# Republic in 1961; Egypt is coded as independent throughout the UAR's life, but for purposes of
# independence dates, code as initial year of independence from the UK (1937) for pre-UAR and during
# UAR, whereas code as 1961 after the UAR
cyears2$styear[cyears2$iso3c == "EGY" & cyears2$year >= 1961] <- 1961

# calculate years since independence
cyears2 <- cyears2 %>%
  dplyr::mutate(yrs_since_indep = year - styear,
                # if the country did not exist yet (indicated by negative yrs_since_indep value),
                # replace yrs_since_indep with 0
                yrs_since_indep = ifelse(yrs_since_indep < 0 & cn == 0, 0, yrs_since_indep)) %>%
  dplyr::select(-styear) %>%
  # limit dataset to time frame of this study
  dplyr::filter(year >= 1946)


### write data -------------------------------------------------------------------------------------
# writes formatted dataframe as csv files
write.csv(cyears2, "Data files/Formatted data files/country_years.csv", row.names = FALSE)


### codebook ---------------------------------------------------------------------------------------
# iso3c
### A country's standardized iso3c code, with non-standard codes for West Germany, East Germany,
### North Yemen, South Yemen, South Vietnam, the Netherlands Antilles, the Soviet Union, and
### Yugoslavia.
# country
### A country's commonly used English-language name.
# year
### The calendar year the specific variable is measured during.
# cn
### A binary variable denoting if the country existed as a sovereign nation during that year.
# yrs_since_indep
### The number of years since the country last gained independence.
