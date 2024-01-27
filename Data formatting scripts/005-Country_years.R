# This script formats country-years data.

### load libraries ----------------------------------------------------------------------
library(countrycode)
library(dplyr)

### load data files ----------------------------------------------------------------------
cyears <- read.csv("Data files/Raw data files/states2016.csv")

### format data ----------------------------------------------------------------------
cyears <- cyears %>%
  # using the countrycode package, add iso3c based on country COW abbreviation
  dplyr::mutate(iso3c = countrycode::countrycode(stateabb,"cowc","iso3c")) %>%
  # rearrange variables
  dplyr::relocate(iso3c,.before=stateabb)

# codes iso3c values missing from the countrycode package: RVN, YPR, YAR, ZAN, KSV, CZE, DDR, BRD
cyears$iso3c[cyears$stateabb=="RVN"] <- "RVN"
cyears$iso3c[cyears$stateabb=="YPR"] <- "YPR"
cyears$iso3c[cyears$stateabb=="YAR"] <- "YAR"
cyears$iso3c[cyears$stateabb=="ZAN"] <- "ZAN"
cyears$iso3c[cyears$stateabb=="KOS"] <- "KSV"
cyears$iso3c[cyears$stateabb=="YUG"] <- "YUG"
cyears$iso3c[cyears$stateabb=="CZE"] <- "CZE"
cyears$iso3c[cyears$stateabb=="GDR"] <- "DDR"
cyears$iso3c[cyears$stateabb=="GFR"] <- "BRD"

### calculate as country-years ----------------------------------------------------------------------
# creates an empty dataframe to be populated
cyears2 <- data.frame(iso3c = NA, year = NA, cn = NA)

for(i in 1:nrow(cyears)){
  # creates a dataframe for the ith country with years starting with the country's first
  # year of existance to its last (or 2016, if the country currently exists)
  years_tmp <- c(cyears$styear[i]:cyears$endyear[i]) %>%
    as.data.frame() %>%
    dplyr::rename(year = 1) %>%
    dplyr::mutate(iso3c = cyears$iso3c[i],
                  # dummy variable indicating the country existed in that specific year
                  cn = 1) %>%
    dplyr::relocate(iso3c, .before=year)
  
  # merges the country dataframe with the placeholder dataframe
  cyears2 <- cyears2 %>%
    rbind(years_tmp)

}

# removes first row of NAs
cyears2 <- cyears2[-1,]

# creates entries for 2017 - 2019 using the same values as 2016
cyear17 <- cyears2 %>%
  dplyr::filter(year == 2016) %>%
  dplyr::mutate(year = 2017)

cyear18 <- cyears2 %>%
  dplyr::filter(year == 2016) %>%
  dplyr::mutate(year = 2018)

cyear19 <- cyears2 %>%
  dplyr::filter(year == 2016) %>%
  dplyr::mutate(year = 2019)

# merges 2017 - 2019 datasets to main dataset
cyears2 <- cyears2 %>%
  rbind(cyear17,cyear18,cyear19)

### calculate divided country data ----------------------------------------------------------------------
cyears2 <- cyears2 %>%
  # YUG is coded as Yugoslavia and Serbia
  dplyr::mutate(iso3c = ifelse(iso3c=="YUG"&year>=1992,"SRB",iso3c),
                # RUS is coded as USSR and Russia
                iso3c = ifelse(iso3c=="RUS"&year %in% c(1922:1991),"SOV",iso3c)) %>%
  # expands the dataset to include all iso3c codes for 1946 - 2019, if not present already
  dplyr::full_join(expand.grid(iso3c = unique(cyears2$iso3c), year = c(1946:2019))) %>%
  dplyr::mutate(cn = ifelse(is.na(cn),0,cn))

# JPN, BRD, DDR, AUT are not in system during their occupation after WWII

#cyears2[cyears2$iso3c=="JPN"&cyears2$year%in%c(1946:1951)] <- 1
#cyears2[cyears2$iso3c=="BRD"&cyears2$year%in%c(1946:1951)] <- 1

### write data ----------------------------------------------------------------------
# writes formatted dataframe as csv files
write.csv(cyears2,"Data files/Formatted data files/country_years.csv")
