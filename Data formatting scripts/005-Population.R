# This script formats a population estimate variable for all country-years.

# TODO
## BRD/DDR: 1946-1950 estimates; capture growth rates
## SOV: 1946-1950 estimates; capture growth rates
## YAR: 1946-1950 estimates; capture growth rates
## YUG et al: not finalized; capture growth rates
## ZAF/NAM: 1946-1950 estimates
## Standardized 1946-1950 estimating: unlisted countries, CZE

### load libraries ----------------------------------------------------------------------
library(readxl)
library(countrycode)
library(tibble)
library(dplyr)
library(tidyr)

### not in function ----------------------------------------------------------------------
'%!in%' <- function(x,y)!('%in%'(x,y))

### load data files ----------------------------------------------------------------------
# population data from United Nations Department of Economic and Social Affairs, Population Division
# 1950 - 1984
pd1 <- readxl::read_excel("Data files/Raw data files/AnnualTotPopMidYear-20200708071736.xlsx", sheet = 2, skip = 1)
# 1985 - 2019
pd2 <- readxl::read_excel("Data files/Raw data files/AnnualTotPopMidYear-20200708071835.xlsx", sheet = 2, skip = 1)

# Correlates of War data file
cow.pop <- read.csv("Data files/Raw data files/NMC_5_0.csv")

### format data ----------------------------------------------------------------------
#### UN datasets ----------------------------------------------------------------------
# merge datasets
pd <- dplyr::full_join(pd1,pd2,by=c("ISO 3166-1 numeric code","Location")) %>%
  # drop ISO 3166-1 code and data notes columns
  select(-c(`ISO 3166-1 numeric code`,Note.x,Note.y)) %>%
  # filter out non-countries (regions and economic groups)
  dplyr::filter(Location %!in% c("World","More developed regions","Less developed regions",
                                 "Least developed countries","Less developed regions, excluding least developed countries",
                                 "Less developed regions, excluding China","High-income countries",
                                 "Middle-income countries","Lower-middle-income countries",
                                 "Upper-middle-income countries","Low-income countries","Sub-Saharan Africa",
                                 "Africa","Eastern Africa","Middle Africa","Northern Africa","Southern Africa",
                                 "Western Africa","Asia","Eastern Asia","Central Asia","Southern Asia",
                                 "South-Eastern Asia","Western Asia","Europe","Eastern Europe","Northern Europe",
                                 "Southern Europe","Western Europe","Latin America and the Caribbean","Caribbean",
                                 "Central America","South America","North America","Oceania","Australia/New Zealand",
                                 "Melanesia","Polynesia","South-Central Asia","Northern America","Micronesia"),
                # filter out country sub-units + Holy See (not including Puerto Rico and Western Sahara)
                Location %!in% c("Mayotte","Réunion","Saint Helena","China, Hong Kong SAR","China, Macao SAR","Channel Islands",
                                 "Faeroe Islands","Isle of Man","Gibraltar","Holy See","Anguilla","Aruba","British Virgin Islands",
                                 "Caribbean Netherlands","Cayman Islands","Curaçao","Guadeloupe","Martinique","Montserrat",
                                 "Sint Maarten (Dutch part)","Turks and Caicos Islands","United States Virgin Islands",
                                 "Falkland Islands (Malvinas)","French Guiana","Bermuda","Greenland","Saint Pierre and Miquelon",
                                 "New Caledonia","Guam","Northern Mariana Islands","American Samoa","Cook Islands","French Polynesia",
                                 "Niue","Tokelau","Wallis and Futuna Islands")) %>%
  # convert to long data
  tidyr::pivot_longer(2:71, names_to = "year", values_to = "un.pop")

# add Puerto Rico and Western Sahara to USA / MAR populations, respectively
pd$Location[pd$Location=="Puerto Rico"] <- "United States of America"
pd$Location[pd$Location=="Western Sahara"] <- "Morocco"

pd <- pd %>%
  dplyr::group_by(Location,year) %>%
  dplyr::summarise(un.pop = sum(un.pop)) %>%
  dplyr::ungroup() %>%
  # convert to full value
  dplyr::mutate(un.pop = 1000 * un.pop,
                # using the countrycode package, add iso3c based on country name
                iso3c = countrycode::countrycode(Location,"country.name","iso3c"),
                # using the countrycode package, add country name based on iso3c code
                country = countrycode::countrycode(iso3c,"iso3c","country.name"),
                year = as.numeric(year)) %>%
  # move iso3c variable first
  dplyr::relocate(iso3c, .before = Location) %>%
  dplyr::relocate(country, .after = iso3c) %>%
  dplyr::select(-Location)

#### COW dataset ----------------------------------------------------------------------
cow.pop <- cow.pop %>%
  as.data.frame() %>%
  dplyr::filter(year > 1945) %>%
  # convert to full value
  dplyr::mutate(tpop = 1000 * tpop,
                # using the countrycode package, add iso3c based on country COW abbreviation
                iso3c = countrycode::countrycode(stateabb,"cowc","iso3c"),
                # using the countrycode package, add country name based on iso3c code
                country = countrycode::countrycode(iso3c,"iso3c","country.name"))

# manually add iso3c codes to missing stateabb codes: CZE, GDR, GFR, KOS, RVN, YAR, YPR, YUG, ZAN
cow.pop$iso3c[cow.pop$stateabb=="CZE"] <- "CZE"
cow.pop$iso3c[cow.pop$stateabb=="GDR"] <- "DDR"
cow.pop$iso3c[cow.pop$stateabb=="GFR"] <- "BRD"
cow.pop$iso3c[cow.pop$stateabb=="KOS"] <- "KSV"
cow.pop$iso3c[cow.pop$stateabb=="RVN"] <- "RVN"
cow.pop$iso3c[cow.pop$stateabb=="YAR"] <- "YAR"
cow.pop$iso3c[cow.pop$stateabb=="YPR"] <- "YPR"
cow.pop$iso3c[cow.pop$stateabb=="YUG"] <- "YUG"
cow.pop$iso3c[cow.pop$stateabb=="ZAN"] <- "ZAN"

# manually add country names to missing iso3c codes: BRD, CZE, DDR, KSV, RVN, YAR, YPR, YUG, ZAN
cow.pop$country[cow.pop$iso3c=="BRD"] <- "West Germany"
cow.pop$country[cow.pop$iso3c=="CZE"] <- "Czechia"
cow.pop$country[cow.pop$iso3c=="DDR"] <- "East Germany"
cow.pop$country[cow.pop$iso3c=="KSV"] <- "Kosovo"
cow.pop$country[cow.pop$iso3c=="RVN"] <- "South Vietnam"
cow.pop$country[cow.pop$iso3c=="YAR"] <- "North Yemen"
cow.pop$country[cow.pop$iso3c=="YPR"] <- "South Yemen"
cow.pop$country[cow.pop$iso3c=="YUG"] <- "Yugoslavia"
cow.pop$country[cow.pop$iso3c=="ZAN"] <- "Zanzibar"

cow.pop <- cow.pop %>%
  dplyr::select(iso3c,country,year,tpop) %>%
  dplyr::rename(cow.pop = tpop)

#### Maddison Project Dataset (MPD) ----------------------------------------------------------------------
# codebook (available on sheet "Legend")
## countrycode: 3-letter ISO country code
## country: Country name
## year: Year
## cgdppc: Real GDP per capita in 2011US$, multiple benchmarks (suitable for cross-country income comparisons)
## rgdpnapc: Real GDP per capita in 2011US$, 2011 benchmark (suitable for cross-country growth comparisons)
## pop: Population, mid-year (thousands)
## i_cig:	0/1/2: observation is extrapolated (0), benchmark (1), or interpolated (2)
## i_bm: For benchmark observations: 1: ICP PPP estimates, 2: Historical income benchmarks, 3: Real wages and urbanization,
### 4: Multiple of subsistence, 5: Braithwaite (1968) PPPs

mpd <- readxl::read_xlsx("Data files/Raw data files/mpd2018.xlsx",
                         sheet = "Full data") %>%
  dplyr::rename(iso3c = countrycode) %>%
  # convert population estimates to full number
  dplyr::mutate(pop = pop * 1000,
                # using the countrycode package, add country name based on iso3c code to standardize country names
                country = countrycode::countrycode(iso3c,"iso3c","country.name"))

# recode CSK and SUN to CZE and SOV; add YUG name
mpd$country[mpd$iso3c=="CSK"] <- "Czechoslovakia" # other entries coded as "Czechia," though this includes Czechoslovakia
mpd$country[mpd$iso3c=="SUN"] <- "Soviet Union"
mpd$country[mpd$iso3c=="YUG"] <- "Yugoslavia"
#mpd$iso3c[mpd$iso3c=="CSK"] <- "CZE"
mpd$iso3c[mpd$iso3c=="SUN"] <- "SOV"

#### merge dataset ----------------------------------------------------------------------
pd <- pd %>%
  dplyr::full_join(cow.pop,by=c("iso3c","country","year"))

### calculate unified/divided country data ----------------------------------------------------------------------
# YEM is combined population of YPR and YAR, DEU is combined DDR and BRD, VNM is combined with RVN
# SRB includes KSV, 6 YUG republics have component populations individually

# use COW populations to determine ratio of pop between YPR and YAR / DDR and BRD / VNM and RVN / SRB and KSV

#### CZE/SVK ----------------------------------------------------------------------
# UN codes CZE and SVK as separate Czechia and Slovakia throughout time series, while
# COW codes CZE as unified Czechoslovakia through 1992 (inclusive), with CZE being solely
# Czechia beginning in 1993 and Slovakia data starting in 1993

# Czechia and Slovakia coded as separate beginning in 1993

# 1950-1992: Combine UN pop CZE and SVK
# recode SVK 1950-1992 as CZE, group by iso3c-year, and sum
pd <- pd %>%
  dplyr::mutate(iso3c = ifelse(iso3c=="SVK"&year %in% c(1950:1992),"CZE",iso3c),
                country = ifelse(country=="Slovakia"&year %in% c(1950:1992),"Czechia",country)) %>%
  dplyr::group_by(iso3c,country,year) %>%
  dplyr::summarise(un.pop = sum(un.pop,na.rm=TRUE),
                   cow.pop = sum(cow.pop,na.rm=TRUE)) %>%
  dplyr::ungroup() %>%
  # replace 0s generated from summarising with NAs
  dplyr::mutate(un.pop = ifelse(un.pop==0,NA,un.pop),
                cow.pop = ifelse(cow.pop==0,NA,cow.pop))

#### DEU/BRD/DDR ----------------------------------------------------------------------
# UN codes DEU as combined East and West Germany
# COW codes BRD (1955-1990) and DDR (1954-1990) as separate

# Germany coded as unified beginning in 1990

# 1955-1989: calculate ratio between COW's BRD and DDR population estimates
# and apply to UN's DEU estimates
pd.deu.ratios <- pd %>%
  dplyr::filter(iso3c %in% c("BRD","DDR"),
                # only have DDR data for 1954
                year %in% c(1955:1989)) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(cow.pop.combined = sum(cow.pop,na.rm=TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(cow.pop.ratio = cow.pop / cow.pop.combined) %>%
  dplyr::select(-un.pop)

# pull 1955 ratios for use later
pd.ratio.brd <- pd.deu.ratios$cow.pop.ratio[pd.deu.ratios$iso3c=="BRD"&pd.deu.ratios$year==1955]
pd.ratio.ddr <- pd.deu.ratios$cow.pop.ratio[pd.deu.ratios$iso3c=="DDR"&pd.deu.ratios$year==1955]

# pull UN 1955-1989 population estimates
pd.deu.estimates <- pd %>%
  dplyr::filter(iso3c == "DEU",
                year %in% c(1955:1989)) %>%
  dplyr::select(-c(iso3c,country,cow.pop))

# merge DEU estimates with BRD/DDR ratios
pd.deu.ratios <- pd.deu.ratios %>%
  dplyr::full_join(pd.deu.estimates,by="year") %>%
  # apply ratios to un.pop
  dplyr::mutate(un.pop = un.pop*cow.pop.ratio) %>%
  dplyr::select(-c(cow.pop.combined,cow.pop.ratio))

# filter out DEU, BRD, and DDR 1955-1989 from main dataset and rbind estimate
# dataset; also filter out BRD and DDR 1990 - DEU 1990 entry contains both
# UN and COW population estimates
pd <- pd %>%
  dplyr::filter(iso3c %!in% c("DEU","BRD","DDR") | year %!in% c(1955:1989),
                iso3c %!in% c("BRD","DDR") | year != 1990) %>%
  rbind(pd.deu.ratios)

# using the pulled 1955 BRD/DDR ratios, apply to UN (combined) DEU 1950-1954 estimates
for(v in c("BRD","DDR")){
  
  country_name <- ifelse(v=="BRD","West Germany","East Germany")
  ratio <- ifelse(v=="BRD",pd.ratio.brd,pd.ratio.ddr)
  
  for(y in c(1950:1954)){
    
    pd <- pd %>%
      tibble::add_row(iso3c = v,
                      country = country_name,
                      year = y,
                      un.pop = pd$un.pop[pd$iso3c=="DEU"&pd$year==y]*ratio,
                      cow.pop = NA)
    
  }
  
}

# merge DDR 1954 entries into single line
ddr.1954.un <- pd$un.pop[pd$iso3c=="DDR"&pd$year==1954&!is.na(pd$un.pop)]
ddr.1954.cow <- pd$cow.pop[pd$iso3c=="DDR"&pd$year==1954&!is.na(pd$cow.pop)]

# remove DDR 1954 entries and DEU 1950-1954 values from main dataset
pd <- pd %>%
  dplyr::filter(iso3c != "DDR" | year != 1954,
                iso3c != "DEU" | year %!in% c(1950:1954)) %>%
  # add new row for DDR 1954 to have a single row for the country-year
  tibble::add_row(iso3c = "DDR",
                  country = "East Germany",
                  year = 1954,
                  un.pop = ddr.1954.un,
                  cow.pop = ddr.1954.cow)

# 1950-1953 (DDR)/1954 (BRD): apply UN population growth rates to COW 1954 (DDR)
# and 1955 (BRD) population estimates
pd <- pd %>%
  dplyr::mutate(ratio.brd.1955 = un.pop / pd$un.pop[pd$iso3c=="BRD"&pd$year==1955],
                ratio.ddr.1954 = un.pop / pd$un.pop[pd$iso3c=="DDR"&pd$year==1954],
                # calculate BRD 1950-1954 COW estimates
                cow.pop = ifelse(iso3c=="BRD"&year %in% c(1950:1954),ratio.brd.1955*pd$cow.pop[pd$iso3c=="BRD"&pd$year==1955],cow.pop),
                # calculate DDR 1950-1953 COW estimates
                cow.pop = ifelse(iso3c=="DDR"&year %in% c(1950:1953),ratio.ddr.1954*pd$cow.pop[pd$iso3c=="DDR"&pd$year==1954],cow.pop)) %>%
  dplyr::select(-c(ratio.brd.1955,ratio.ddr.1954))

#### ETH/ERI(x) ----------------------------------------------------------------------

#### ISR/PSE(x) ----------------------------------------------------------------------

#### MYS/SGP(x) ----------------------------------------------------------------------

#### PAK/BGD(x) ----------------------------------------------------------------------

#### SDN/SSD(x) ----------------------------------------------------------------------

#### SOV/RUS ----------------------------------------------------------------------
# UN lists each of the 15 SSR populations separately, while COW codes SOV's population
# as RUS through 1990 (inclusive)

# merge UN SSR estimates into one SOV estimate through 1990 (preserving each SSR's estimates)
pd.sov <- pd %>%
  # select the 15 USSR successor states
  dplyr::filter(iso3c %in% c("EST","LVA","LTU","MDA","BLR","UKR","RUS","GEO","ARM","AZE",
                             "KAZ","KGZ","TJK","TKM","UZB")) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(un.pop = sum(un.pop,na.rm=TRUE),
                   cow.pop = sum(cow.pop,na.rm=TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(year < 1991) %>%
  dplyr::mutate(iso3c = "SOV",
                country = "Soviet Union",
                # converts 0s created when summarising data into NAs
                un.pop = ifelse(un.pop==0,NA,un.pop))

# add SOV estimates to main dataset
pd <- pd %>%
  rbind(pd.sov)

# remove cow's SOV estimates coded as RUS
pd$cow.pop[pd$iso3c=="RUS"&pd$year<1991] <- NA

#### TZA/ZAN(x) ----------------------------------------------------------------------

#### YEM/YAR/YPR ----------------------------------------------------------------------
# COW contains estimates for YAR (1946-1990), YPR (1967-1990), and YEM (1990-2012)
# UN contains combined estimates for YAR/YPR starting in 1950

# Yemen coded as unified beginning in 1991

# 1967-1990: calculate ratio between COW's YAR and YPR population estimates
# and apply to UN's YEM estimates
pd.yem.ratios <- pd %>%
  dplyr::filter(iso3c %in% c("YAR","YPR"),
                # only have COW YPR data starting in 1967 - year of independence
                year >= 1967) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(cow.pop.combined = sum(cow.pop,na.rm=TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(cow.pop.ratio = cow.pop / cow.pop.combined) %>%
  dplyr::select(-un.pop)

# pull 1967 ratios for use later
pd.ratio.yar <- pd.yem.ratios$cow.pop.ratio[pd.yem.ratios$iso3c=="YAR"&pd.yem.ratios$year==1967]
# pd.ratio.ypr <- pd.yem.ratios$cow.pop.ratio[pd.yem.ratios$iso3c=="YPR"&pd.yem.ratios$year==1967]

# pull UN 1967-1990 population estimates
pd.yem.estimates <- pd %>%
  dplyr::filter(iso3c == "YEM",
                year %in% c(1967:1990)) %>%
  dplyr::select(-c(iso3c,country,cow.pop))

# merge YEM estimates with YAR/YPR ratios
pd.yem.ratios <- pd.yem.ratios %>%
  dplyr::full_join(pd.yem.estimates,by="year") %>%
  # apply ratios to un.pop
  dplyr::mutate(un.pop = un.pop*cow.pop.ratio) %>%
  dplyr::select(-c(cow.pop.combined,cow.pop.ratio))

# filter out YEM, YAR, and YPR 1967-1990 from main dataset and rbind
# estimate dataset 
pd <- pd %>%
  dplyr::filter(iso3c %!in% c("YEM","YAR","YPR") | year %!in% c(1967:1990)) %>%
  rbind(pd.yem.ratios)

# apply 1967 ratio to UN YEM 1950-1966 estimates to calculate YAR estimates
for(y in 1950:1966){
  
  pd$un.pop[pd$iso3c=="YAR"&pd$year==y] <- pd$un.pop[pd$iso3c=="YEM"&pd$year==y]*pd.ratio.yar

}

# filter out YEM 1950-1966 entries
pd <- pd %>%
  dplyr::filter(iso3c != "YEM" | year %!in% c(1950:1966))

#### VNM/RVN ----------------------------------------------------------------------
# UN codes VNM for both North and South Vietnam 1950-2019
# COW codes VNM for North Vietnam and RVN for South Vietnam 1954-1975, with VNM
# coded as a unified Vietnam starting in 1976

# Vietnam coded as unified beginning in 1976

# 1954-1975: calculate ratio between COW's VNM (North) and RVN population estimates
# and apply to UN's VNM estimates
pd.vnm.ratios <- pd %>%
  dplyr::filter(iso3c %in% c("VNM","RVN"),
                # filter to only contain years after Vietnamese independence and
                # before the reunification of Vietnam
                year %in% c(1954:1975)) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(cow.pop.combined = sum(cow.pop,na.rm=TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(cow.pop.ratio = cow.pop / cow.pop.combined) %>%
  dplyr::select(-un.pop)

# pull 1954 ratios for use later
#pd.ratio.vnm <- pd.vnm.ratios$cow.pop.ratio[pd.vnm.ratios$iso3c=="VNM"&pd.vnm.ratios$year==1954]
#pd.ratio.rvn <- pd.vnm.ratios$cow.pop.ratio[pd.vnm.ratios$iso3c=="RVN"&pd.vnm.ratios$year==1954]

# pull UN 1954-1975 population estimates
pd.vnm.estimates <- pd %>%
  dplyr::filter(iso3c == "VNM",
                year %in% c(1954:1975)) %>%
  dplyr::select(-c(iso3c,country,cow.pop))

# merge VNM estimates with VNM (North) and RVN ratios
pd.vnm.ratios <- pd.vnm.ratios %>%
  dplyr::full_join(pd.vnm.estimates,by="year") %>%
  # apply ratios to un.pop
  dplyr::mutate(un.pop = un.pop*cow.pop.ratio) %>%
  dplyr::select(-c(cow.pop.combined,cow.pop.ratio))

# filter out VNM and RVN 1954-1975 from main dataset and rbind
# estimate dataset
pd <- pd %>%
  dplyr::filter(iso3c %!in% c("VNM","RVN") | year %!in% c(1954:1975)) %>%
  rbind(pd.vnm.ratios)

#### YUG/SRB/MNE/KSV(x) ----------------------------------------------------------------------
# BIH: UN estimates 1950-2019; COW 1992-2012
# HRV: UN estimates 1950-2019; COW 1992-2012
# MKD: UN estimates 1950-2019; COW 1993-2012
# SVN: UN estimates 1950-2019; COW 1992-2012
# SRB: UN estimates 1950-2019, containing Serbia and Kosovo, but no Montenegro; COW no estimates
# MNE: UN estimates 1950-2019; COW 2006-2012
# KSV: UN no estimates; COW 2008-2012
# YUG: UN no estimates; COW 1946-1991 SFR Yugoslavia estimates, 1992-2012 contains Serbia, Montenegro, and Kosovo

# merge UN republic estimates into one YUG estimate through 1991 (preserving each republic's estimates)
pd.yug.un <- pd %>%
  # drop COW's estimates (already listed under "YUG" through 1991)
  dplyr::select(-cow.pop) %>%
  # select the 6 republics of Yugoslavia
  dplyr::filter(iso3c %in% c("BIH","HRV","MKD","MNE","SRB","SVN")) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(un.pop = sum(un.pop,na.rm=TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(year < 1992)

# pull COW's YUG estimates through 1991 (inclusive)
pd.yug.cow <- pd %>%
  # drop UN variable
  dplyr::select(-un.pop) %>%
  # select YUG through 1991 (inclusive)
  dplyr::filter(iso3c == "YUG",
                year < 1992)

# merge pre-dissolution YUG estimates
pd.yug <- dplyr::full_join(pd.yug.un,pd.yug.cow,by="year")

# filter out YUG 1946-1991 entries from main dataset and rbind new estimates
pd <- pd %>%
  dplyr::filter(iso3c != "YUG" | year %!in% c(1946:1991)) %>%
  rbind(pd.yug)

# apply UN 1992-1993 MKD growth rate to COW estimate to approximate 1992 population
# COW likely does not code 1992 because MKD only gained universal recognition in 1993
# when Greece dropped objections over MKD's name
pd$cow.pop[pd$iso3c=="MKD"&pd$year==1992] <- pd$cow.pop[pd$iso3c=="MKD"&pd$year==1993]/
                                                (pd$un.pop[pd$iso3c=="MKD"&pd$year==1993]/pd$un.pop[pd$iso3c=="MKD"&pd$year==1992])

# for COW YUG estimates [Serbia, Montenegro, Kosovo], subtract COW MNE estimates (2006-2012) and COW KSV estimates (2008-2012)
for(m in 2006:2012){
  
  pd$cow.pop[pd$iso3c=="YUG"&pd$year==m] <- pd$cow.pop[pd$iso3c=="YUG"&pd$year==m] - pd$cow.pop[pd$iso3c=="MNE"&pd$year==m]
  
}
for(k in 2008:2012){
  
  pd$cow.pop[pd$iso3c=="YUG"&pd$year==k] <- pd$cow.pop[pd$iso3c=="YUG"&pd$year==k] - pd$cow.pop[pd$iso3c=="KSV"&pd$year==k]
  
}

# recode YUG 1992-2012 as SRB
pd$iso3c[pd$iso3c=="YUG"&pd$year %in% c(1992:2012)] <- "SRB"
pd$country[pd$country=="Yugoslavia"&pd$year %in% c(1992:2005)] <- "Serbia and Montenegro"
pd$country[pd$country=="Yugoslavia"&pd$year >= 2006] <- "Serbia"


# pull tables for  SRB+KSV+MNE for use later
pd.srb.ksv.mne2 <- pd %>%
  dplyr::filter(iso3c == "YUG",
                year >= 1992) %>%
  dplyr::select(-un.pop) %>%
  dplyr::mutate(country = "Serbia and Montenegro and Kosovo")

pd.srb.ksv.mne <- pd %>%
  dplyr::filter(iso3c %in% c("YUG","SRB","MNE","KSV"),
                year >= 1992) %>%
  dplyr::mutate(country = ifelse(country=="Serbia","Serbia and Kosovo (UN)",country),
                country = ifelse(country=="Yugoslavia","Serbia, Montenegro, and Kosovo (COW)",country))



pd.yug <- pd %>%
  dplyr::filter(iso3c %in% c("SVN","HRV","MKD","BIH","SRB","MNE","KSV","YUG")) 

# pd.yug.test2 <- pd.yug %>%
#   dplyr::mutate(cow.pop = ifelse(iso3c %in% c("KSV","MNE"),NA,cow.pop)) %>%
#   dplyr::group_by(year) %>%
#   dplyr::summarise(un.pop = sum(un.pop,na.rm=TRUE),
#                    cow.pop = sum(cow.pop,na.rm=TRUE)) %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(diff = un.pop - cow.pop)
# 
# 
# pd.yug.test <- pd.yug %>%
#   dplyr::group_by(year) %>%
#   dplyr::summarise(un.sum = sum(un.pop,na.rm=TRUE),
#                    cow.sum = sum(cow.pop,na.rm=TRUE)) %>%
#   dplyr::ungroup()
# 
# 
# yug <- pd %>%
#   # select the 6 fully recognized successor states
#   dplyr::filter(iso3c %in% c("SVN","HRV","MKD","BIH","SRB","MNE")) %>%
#   dplyr::group_by(year) %>%
#   # sums the populations of the 6 fully recognized successor states
#   dplyr::summarise(value = sum(value)) %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(iso3c = "YUG",
#                 country = "Yugoslavia") %>%
#   # Yugoslavia broke apart starting in 1992 for purposes of this dataset
#   dplyr::filter(year <= 1991)
# 
# # filter out the 6 fully recognized successor states during the period of Yugoslavia
# pd <- pd %>%
#   dplyr::filter(iso3c %!in% c("SVN","HRV","MKD","BIH","SRB","MNE") | year >= 1992) %>%
#   # merge Yugoslavia dataset
#   rbind(yug)
# 
# # SME
# # coded as SRB / Serbia, but includes Montenegro's population 1992 - 2005, with Montenegro
# # being a separate country in this dataset beginning in 2006
# srb_mne <- pd %>%
#   dplyr::select(-country) %>%
#   # select Serbia and Montenegro
#   dplyr::filter(iso3c %in% c("SRB","MNE")) %>%
#   dplyr::group_by(year) %>%
#   # sums the populations of Serbia and Montenegro
#   dplyr::summarise(value = sum(value,na.rm=TRUE)) %>%
#   dplyr::ungroup() %>%
#   # Montenegro regained independence starting in 2006 for purposes of this dataset
#   dplyr::filter(year < 2006) %>%
#   dplyr::mutate(iso3c = "SRB",
#                 country = "Serbia and Montenegro")
# 
# # filter out Serbia and Montenegro from the main dataset during their period of unification
# pd <- pd %>%
#   dplyr::filter(iso3c %!in% c("SRB","MNE") | year >= 2006) %>%
#   # merge Serbia and Montenegro dataset
#   rbind(srb_mne)
# 
# # KSV
# cow.pop.srb <- cow.pop %>%
#   # select Yugoslavia (Serbia shares YUG as its coding) and Kosovo
#   dplyr::filter(stateabb %in% c("YUG","KOS"),
#                 # Kosovo declared and gained (de facto) independence in 2008 for purposes of this dataset
#                 year >= 2008) %>%
#   # convert both countries to having a column, with each year being a row
#   tidyr::pivot_wider(names_from = "stateabb", values_from = "tpop") %>%
#   # Serbia appears to include both Serbia and Kosovo, while Kosovo includes just Kosovo
#   # calculate a Serbia without Kosovo population and the proportion of the population in each part relative to the total population
#   dplyr::mutate(SRB = YUG - KOS,
#                 srb.p = SRB / YUG,
#                 ksv.p = KOS / YUG)
# 
# # pull Serbia population data from the UN data source (Serbia + Kosovo)
# srb <- pd %>%
#   dplyr::select(-country) %>%
#   dplyr::filter(iso3c == "SRB",
#                 # Kosovo declared and gained (de facto) independence in 2008 for purposes of this dataset
#                 year >= 2008) %>%
#   dplyr::mutate(year = as.numeric(year)) %>%
#   # merge UN and COW population data
#   dplyr::full_join(cow.pop.srb,by="year")
# 
# # use proportions for last year of data available (2012) for both Serbia and Kosovo for subsequent years missing data
# srb$srb.p[is.na(srb$srb.p)&srb$year>2012] <- 7748/9553
# srb$ksv.p[is.na(srb$ksv.p)&srb$year>2012] <- 1805/9553
# 
# # calculate estimates for 2013 - 2019 based on UN population data and 2012 proportions of populations between
# # Serbia and Kosovo
# srb <- srb %>%
#   dplyr::mutate(srb.e = value * srb.p,
#                 ksv.e = value * ksv.p)
# 
# # create dataframe for Serbia (2013 - 2019)
# srb2 <- srb %>%
#   dplyr::select(iso3c,year,srb.e) %>%
#   dplyr::rename(value = srb.e) %>%
#   dplyr::mutate(country = "Serbia")
# 
# # create dataframe for Kosovo
# ksv <- srb %>%
#   dplyr::select(iso3c,year,ksv.e) %>%
#   dplyr::mutate(iso3c = "KSV",
#                 country = "Kosovo") %>%
#   dplyr::rename(value = ksv.e)
# 
# # removes Serbia (2013 - 2019) from the main population dataframe and joins the
# # new Serbia and Kosovo dataframes
# pd <- pd %>%
#   dplyr::filter(iso3c != "SRB" | year < 2008) %>%
#   rbind(srb2) %>%
#   rbind(ksv)

#### ZAF/NAM(x) ----------------------------------------------------------------------
# Both UN and COW ZAF populations do not include NAM prior to NAM independence
# NAM coded as independent beginning in 1990

# NAM 1950-1989: apply UN population growth rates to COW's 1990 population estimate

# the UN baseline to estimate the proportions from
nam.baseline <- pd$un.pop[pd$iso3c=="NAM"&pd$year==1990]

# the COW relative population to apply the proportions to
nam.relative <- pd$cow.pop[pd$iso3c=="NAM"&pd$year==1990]

pd <- pd %>%
  dplyr::mutate(prop = nam.relative * un.pop / nam.baseline,
                cow.pop = ifelse(iso3c=="NAM"&year %in% c(1950:1989),prop,cow.pop)) %>%
  dplyr::select(-prop)

# pull ZAF 1989-1990 (excluding NAM) and NAM growth rates for use later
zaf.un.growth.1989.1990 <- pd$un.pop[pd$iso3c=="ZAF"&pd$year==1990]/pd$un.pop[pd$iso3c=="ZAF"&pd$year==1989]
zaf.cow.growth.1989.1990 <- pd$cow.pop[pd$iso3c=="ZAF"&pd$year==1990]/pd$cow.pop[pd$iso3c=="ZAF"&pd$year==1989]
nam.un.growth.1989.1990 <- pd$un.pop[pd$iso3c=="NAM"&pd$year==1990]/pd$un.pop[pd$iso3c=="NAM"&pd$year==1989]
nam.cow.growth.1989.1990 <- pd$cow.pop[pd$iso3c=="NAM"&pd$year==1990]/pd$cow.pop[pd$iso3c=="NAM"&pd$year==1989]

# merge ZAF and NAM populations for 1950-1989
for(z in 1950:1989){
  
  pd$un.pop[pd$iso3c=="ZAF"&pd$year==z] <- pd$un.pop[pd$iso3c=="ZAF"&pd$year==z] + pd$un.pop[pd$iso3c=="NAM"&pd$year==z]
  pd$cow.pop[pd$iso3c=="ZAF"&pd$year==z] <- pd$cow.pop[pd$iso3c=="ZAF"&pd$year==z] + pd$cow.pop[pd$iso3c=="NAM"&pd$year==z]
  
}

### calculate 1946-1950 estimates ----------------------------------------------------------------------

### calculate 2013-2019 estimates ----------------------------------------------------------------------

#### 1940s estimate functions ----------------------------------------------------------------------
# this function uses COW's population estimates for 1946-1950 and applies the growth rates to the 1950
# UN population estimates
pop_growth_estimator_cow_func <- function(df = pd, df2 = cow.pop, iso, yr = 1950){
  
  df2 <- df2 %>%
    dplyr::filter(stateabb == iso) %>%
    dplyr::mutate(tpop = tpop*1000)
  
  df <- dplyr::full_join(df,df2,by=c("iso3c","year"))
  
  # the UN baseline to estimate the proportions from
  baseline <- df$value[df$iso3c==iso&year==yr]
  
  # the COW relative estimate to base the proportions from
  relative <- 
  # the gdp.pwt relative gdp to apply the proportions to
  relative <- df$gdp.pwt[df$iso3c==iso&df$year==yr]
  
  if(is.na(relative)){
    # if relative based on gdp.pwt is NA, use gdp.pwt.est
    relative <- df$gdp.pwt.est[df$iso3c==iso&df$year==yr]
  }
  
  df <- df %>%
    dplyr::mutate(prop = relative * gdp.gl / baseline,
                  gdp.pwt.est = ifelse(iso3c==iso&is.na(gdp.pwt.est),prop,gdp.pwt.est)) %>%
    dplyr::select(-prop)
  
  return(df)
  
}



### add workbook estimates ----------------------------------------------------------------------
# adds population estimates for countries in the 1940s (file notes sources of estimates)
pop_40s <- readxl::read_excel("Data files/Workbooks/pop_estimates.xlsx", sheet = 1) %>%
  dplyr::select(-method) %>%
  dplyr::rename(value = pop.pd) %>%
  # using the countrycode package, add country name based on iso3c value
  dplyr::mutate(country = countrycode::countrycode(iso3c,"iso3c","country.name"))

# codes country name values missing from the countrycode package: BRD, DDR, SOV, YAR, YUG
pop_40s$country[pop_40s$iso3c=="BRD"] <- "West Germany"
pop_40s$country[pop_40s$iso3c=="DDR"] <- "East Germany"
pop_40s$country[pop_40s$iso3c=="SOV"] <- "Soviet Union"
pop_40s$country[pop_40s$iso3c=="YAR"] <- "North Yemen"
pop_40s$country[pop_40s$iso3c=="YUG"] <- "Yugoslavia"

pd <- pd %>%
  rbind(pop_40s) %>%
  dplyr::rename(population = value)

### write data ----------------------------------------------------------------------
# writes formatted dataframe as csv files
write.csv(pd,"Data files/Formatted data files/population.csv",row.names = FALSE)
