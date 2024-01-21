# This script formats country-years data.

# load libraries
library(countrycode)
library(dplyr)

#### country-years to use ####
cyears <- read.csv("Data files/Raw data files/states2016.csv") %>%
  dplyr::mutate(iso3c = countrycode::countrycode(stateabb,"cowc","iso3c"))

cyears$iso3c[cyears$stateabb=="RVN"] <- "RVN"
cyears$iso3c[cyears$stateabb=="YPR"] <- "YPR"
cyears$iso3c[cyears$stateabb=="YAR"] <- "YAR"
cyears$iso3c[cyears$stateabb=="ZAN"] <- "ZAN"
cyears$iso3c[cyears$stateabb=="KOS"] <- "KSV"
cyears$iso3c[cyears$stateabb=="YUG"] <- "YUG"
cyears$iso3c[cyears$stateabb=="CZE"] <- "CZE"
cyears$iso3c[cyears$stateabb=="GDR"] <- "DDR"
cyears$iso3c[cyears$stateabb=="GFR"] <- "BRD"

cyears2 <- data.frame(iso3c = NA, year = NA, cn = NA)

for(i in 1:nrow(cyears)){
  iso3c_tmp <- cyears$iso3c[i]
  styear_tmp <- cyears$styear[i]
  endyear_tmp <- cyears$endyear[i]
  years_tmp <- c(styear_tmp:endyear_tmp)
  
  add <- data.frame(iso3c = NA, year = NA, cn = NA)
  
  for(j in 1:length(years_tmp)){
    add$iso3c <- iso3c_tmp
    add$year <- years_tmp[j]
    add$cn <- 1
    
    cyears2 <- rbind(cyears2,add)
  }
}

srb_years <- cyears2 %>%
  dplyr::filter(iso3c=="YUG"&year>=1992) %>%
  dplyr::mutate(iso3c = "SRB")

cyears2 <- cyears2 %>%
  rbind(srb_years)

cyears2 <- cyears2[-1,]

cyear17 <- cyears2 %>%
  dplyr::filter(year == 2016) %>%
  dplyr::mutate(year = 2017)

cyear18 <- cyears2 %>%
  dplyr::filter(year == 2016) %>%
  dplyr::mutate(year = 2018)

cyear19 <- cyears2 %>%
  dplyr::filter(year == 2016) %>%
  dplyr::mutate(year = 2019)

cyears.sov <- data.frame(iso3c = "SOV", year = c(1946:1991), cn = 1)

cyears2 <- cyears2 %>%
  rbind(cyear17,cyear18,cyear19,cyears.sov) %>%
  dplyr::filter(iso3c != "YUG" | year <= 1991) %>%
  dplyr::full_join(expand.grid(iso3c = unique(cyears2$iso3c), year = c(1946:2019)))

# JPN, BRD, DDR, AUT are not in system during their occupation after WWII

#cyears2[cyears2$iso3c=="JPN"&cyears2$year%in%c(1946:1951)] <- 1
#cyears2[cyears2$iso3c=="BRD"&cyears2$year%in%c(1946:1951)] <- 1
