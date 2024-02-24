# This script formats a population estimate variable for all country-years.

# TODO
## SOV: 1946-1950 estimates; capture growth rates
## YAR: 1946-1950 estimates
## YUG et al: capture growth rates
## SRB + KSV UN needs modifying
## Standardized 1946-1950 estimating: unlisted countries, CZE, ETH, PAK

# UN: KSV 2013-2019
# COW: KSV 2013-2019; SRB 1992-2019

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

# add Puerto Rico population to USA population
for(u in 1940:2016){
  
  mpd$pop[mpd$iso3c=="USA"&mpd$year==u] <- mpd$pop[mpd$iso3c=="USA"&mpd$year==u] + mpd$pop[mpd$iso3c=="PRI"&mpd$year==u]
  
}

#### Gleditsch ----------------------------------------------------------------------
pop.gl <- utils::read.delim("Data files/Raw data files/gdpv6.txt") %>%
  # using the countrycode package, add iso3c based on country name
  dplyr::mutate(iso3c = countrycode::countrycode(stateid,"gwc","iso3c")) %>%
  # move iso3c variable first
  dplyr::relocate(iso3c, .before = statenum)

# codes iso3c values missing from the countrycode package
pop.gl$iso3c[pop.gl$stateid=="AAB"] <- "ATG"
pop.gl$iso3c[pop.gl$stateid=="ABK"] <- "ABK" # Abkhazia
pop.gl$iso3c[pop.gl$stateid=="AND"] <- "AND"
pop.gl$iso3c[pop.gl$stateid=="CZE"] <- "CZE" # Czechoslovakia
pop.gl$iso3c[pop.gl$stateid=="DMA"] <- "DMA"
pop.gl$iso3c[pop.gl$stateid=="DRV"] <- "VNM" # North Vietnam and unified Vietnam
pop.gl$iso3c[pop.gl$stateid=="FSM"] <- "FSM"
pop.gl$iso3c[pop.gl$stateid=="GDR"] <- "DDR"
pop.gl$iso3c[pop.gl$stateid=="GRN"] <- "GRD"
pop.gl$iso3c[pop.gl$stateid=="KBI"] <- "KIR"
pop.gl$iso3c[pop.gl$stateid=="KOS"] <- "KSV"
pop.gl$iso3c[pop.gl$stateid=="LIE"] <- "LIE"
pop.gl$iso3c[pop.gl$stateid=="MNC"] <- "MCO"
pop.gl$iso3c[pop.gl$stateid=="MSI"] <- "MHL"
pop.gl$iso3c[pop.gl$stateid=="NAU"] <- "NRU"
pop.gl$iso3c[pop.gl$stateid=="PAL"] <- "PLW"
pop.gl$iso3c[pop.gl$stateid=="SEY"] <- "SYC"
pop.gl$iso3c[pop.gl$stateid=="SKN"] <- "KNA"
pop.gl$iso3c[pop.gl$stateid=="SLU"] <- "LCA"
pop.gl$iso3c[pop.gl$stateid=="SMN"] <- "SLB"
pop.gl$iso3c[pop.gl$stateid=="SOT"] <- "SOT" # South Osseita
pop.gl$iso3c[pop.gl$stateid=="STP"] <- "STP"
pop.gl$iso3c[pop.gl$stateid=="SVG"] <- "VCT"
pop.gl$iso3c[pop.gl$stateid=="TBT"] <- "TBT" # Tibet
pop.gl$iso3c[pop.gl$stateid=="TON"] <- "TON"
pop.gl$iso3c[pop.gl$stateid=="TUV"] <- "TUV"
pop.gl$iso3c[pop.gl$stateid=="VAN"] <- "VUT"
pop.gl$iso3c[pop.gl$stateid=="WSM"] <- "WSM"
pop.gl$iso3c[pop.gl$stateid=="YEM"] <- "YEM" # North Yemen and unified Yemen
pop.gl$iso3c[pop.gl$stateid=="YPR"] <- "YPR"
pop.gl$iso3c[pop.gl$stateid=="YUG"] <- "YUG"
pop.gl$iso3c[pop.gl$stateid=="ZAN"] <- "ZAN"
pop.gl$iso3c[pop.gl$stateid=="RVN"] <- "RVN"
pop.gl$iso3c[pop.gl$stateid=="SNM"] <- "SMR"

pop.gl$iso3c[pop.gl$iso3c=="DEU"&pop.gl$year<1991] <- "BRD" # recodes Germany before 1991 as West Germany
pop.gl$iso3c[pop.gl$iso3c=="YEM"&pop.gl$year<1991] <- "YAR" # recodes Yemen before 1991 as North Yemen

pop.gl <- pop.gl %>%
  # using the countrycode package, add country name based on iso3c value
  dplyr::mutate(country = countrycode::countrycode(iso3c,"iso3c","country.name"),
                gl.pop = pop * 1000) %>%
  dplyr::select(iso3c,country,year,gl.pop)

# codes country name values missing from the countrycode package
pop.gl$country[pop.gl$iso3c=="ABK"] <- "Abkhazia"
pop.gl$country[pop.gl$iso3c=="SOT"] <- "South Ossetia"
pop.gl$country[pop.gl$iso3c=="TBT"] <- "Tibet"
pop.gl$country[pop.gl$iso3c=="BRD"] <- "West Germany"
pop.gl$country[pop.gl$iso3c=="DDR"] <- "East Germany"
pop.gl$country[pop.gl$iso3c=="KSV"] <- "Kosovo"
pop.gl$country[pop.gl$iso3c=="YUG"] <- "Yugoslavia"
pop.gl$country[pop.gl$iso3c=="YAR"] <- "North Yemen"
pop.gl$country[pop.gl$iso3c=="YPR"] <- "South Yemen"
pop.gl$country[pop.gl$iso3c=="RVN"] <- "South Vietnam"
pop.gl$country[pop.gl$iso3c=="ZAN"] <- "Zanzibar"

#### merge dataset ----------------------------------------------------------------------
pd <- pd %>%
  dplyr::full_join(cow.pop,by=c("iso3c","country","year"))

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
                  cow.pop = prop * relative.cow) %>%
    dplyr::select(iso3c,country,year,un.pop,cow.pop)
    
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
                    cow.pop = df$cow.pop[df$iso3c==iso&df$year==1950]/cow.growth.50)
  
  # add 1948
  df <- df %>%
    tibble::add_row(iso3c = iso,
                    country = countrycode::countrycode(iso3c,"iso3c","country.name"),
                    year = 1948,
                    un.pop = df$un.pop[df$iso3c==iso&df$year==1949]/un.growth.49,
                    cow.pop = df$cow.pop[df$iso3c==iso&df$year==1949]/cow.growth.49)
  
  # add 1947
  df <- df %>%
    tibble::add_row(iso3c = iso,
                    country = countrycode::countrycode(iso3c,"iso3c","country.name"),
                    year = 1947,
                    un.pop = df$un.pop[df$iso3c==iso&df$year==1948]/un.growth.48,
                    cow.pop = df$cow.pop[df$iso3c==iso&df$year==1948]/cow.growth.48)
  
  # add 1946
  df <- df %>%
    tibble::add_row(iso3c = iso,
                    country = countrycode::countrycode(iso3c,"iso3c","country.name"),
                    year = 1946,
                    un.pop = df$un.pop[df$iso3c==iso&df$year==1947]/un.growth.47,
                    cow.pop = df$cow.pop[df$iso3c==iso&df$year==1947]/cow.growth.47)
  
}

### calculate unified/divided country data ----------------------------------------------------------------------
# YEM is combined population of YPR and YAR, DEU is combined DDR and BRD, VNM is combined with RVN
# SRB includes KSV, 6 YUG republics have component populations individually

# use COW populations to determine ratio of pop between YPR and YAR / DDR and BRD / VNM and RVN / SRB and KSV

#### AND ----------------------------------------------------------------------
# UN has data from 1950-2019; COW data starts in 1993

# 1950-1992: apply UN population growth rates to COW's 1993 population estimate
pd <- pop_growth_estimator_cow_func(pd, "AND", yr = 1993, restricted = c(1950:1992))

#### AUT ----------------------------------------------------------------------
# 1950-1954: apply UN population growth rates to COW's 1955 population estimate
pd <- pop_growth_estimator_cow_func(pd, "AUT", yr = 1955, restricted = c(1950:1954))

# 1946-1949: apply MPD population growth rates to UN's and COW's 1950 population estimates
pd <- pop_growth_estimator_mpd_func(pd, mpd, "AUT", yr = 1950, restricted = c(1946:1949))

#### CZE/SVK ----------------------------------------------------------------------
# UN codes CZE and SVK as separate Czechia and Slovakia throughout time series, while
# COW codes CZE as unified Czechoslovakia through 1992 (inclusive), with CZE being solely
# Czechia beginning in 1993 and Slovakia data starting in 1993

# Czechia and Slovakia coded as separate beginning in 1993

# capture UN growth rates
cze.un.growth.1992.1993 <- pd$un.pop[pd$iso3c=="CZE"&pd$year==1993]/pd$un.pop[pd$iso3c=="CZE"&pd$year==1992]
svk.un.growth.1992.1993 <- pd$un.pop[pd$iso3c=="SVK"&pd$year==1993]/pd$un.pop[pd$iso3c=="SVK"&pd$year==1992]

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

# capture COW growth rates (Czechia + Slovakia combined / Czechoslovakia)
cze.cow.growth.1992.1993 <- (pd$cow.pop[pd$iso3c=="CZE"&pd$year==1993]+pd$cow.pop[pd$iso3c=="SVK"&pd$year==1993])/pd$cow.pop[pd$iso3c=="CZE"&pd$year==1992]
svk.cow.growth.1992.1993 <- (pd$cow.pop[pd$iso3c=="CZE"&pd$year==1993]+pd$cow.pop[pd$iso3c=="SVK"&pd$year==1993])/pd$cow.pop[pd$iso3c=="CZE"&pd$year==1992]

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

# 1946-1949: apply MPD's (combined) DEU growth rates to un.pop and cow.pop BRD and DDR estimates

for(y in 1949:1946){
  
  pd <- pd %>%
    tibble::add_row(iso3c = "BRD",
                    country = "West Germany",
                    year = y,
                    un.pop = pd$un.pop[pd$iso3c=="BRD"&pd$year==(y+1)]/(mpd$pop[mpd$iso3c=="DEU"&mpd$year==(y+1)]/mpd$pop[mpd$iso3c=="DEU"&mpd$year==y]),
                    cow.pop = pd$cow.pop[pd$iso3c=="BRD"&pd$year==(y+1)]/(mpd$pop[mpd$iso3c=="DEU"&mpd$year==(y+1)]/mpd$pop[mpd$iso3c=="DEU"&mpd$year==y])) %>%
    tibble::add_row(iso3c = "DDR",
                    country = "East Germany",
                    year = y,
                    un.pop = pd$un.pop[pd$iso3c=="DDR"&pd$year==(y+1)]/(mpd$pop[mpd$iso3c=="DEU"&mpd$year==(y+1)]/mpd$pop[mpd$iso3c=="DEU"&mpd$year==y]),
                    cow.pop = pd$cow.pop[pd$iso3c=="DDR"&pd$year==(y+1)]/(mpd$pop[mpd$iso3c=="DEU"&mpd$year==(y+1)]/mpd$pop[mpd$iso3c=="DEU"&mpd$year==y]))
  
}

# capture growth rates
deu.un.growth.1989.1990 <- pd$un.pop[pd$iso3c=="DEU"&pd$year==1990] /
                             (pd$un.pop[pd$iso3c=="BRD"&pd$year==1989] + pd$un.pop[pd$iso3c=="DDR"&pd$year==1989])
deu.cow.growth.1989.1990 <- pd$cow.pop[pd$iso3c=="DEU"&pd$year==1990] /
                             (pd$cow.pop[pd$iso3c=="BRD"&pd$year==1989] + pd$cow.pop[pd$iso3c=="DDR"&pd$year==1989])

#### ETH/ERI ----------------------------------------------------------------------
# COW codes ETH as ETH+ERI through 1992 (inclusive), with no ERI estimates before 1993
# UN codes ETH without ERI throughout

# ERI coded as independent beginning in 1993

# capture UN growth rates
eth.un.growth.1992.1993 <- pd$un.pop[pd$iso3c=="ETH"&pd$year==1993]/pd$un.pop[pd$iso3c=="ETH"&pd$year==1992]
eri.un.growth.1992.1993 <- pd$un.pop[pd$iso3c=="ERI"&pd$year==1993]/pd$un.pop[pd$iso3c=="ERI"&pd$year==1992]

# estimate 1992 ERI and ETH (excluding ERI) populations to capture growth rates
# estimate proportion of ERI and ETH 1993 populations
eri.1993.prop <- pd$cow.pop[pd$iso3c=="ERI"&pd$year==1993]/(pd$cow.pop[pd$iso3c=="ERI"&pd$year==1993]+pd$cow.pop[pd$iso3c=="ETH"&pd$year==1993])
eth.1993.prop <- pd$cow.pop[pd$iso3c=="ETH"&pd$year==1993]/(pd$cow.pop[pd$iso3c=="ERI"&pd$year==1993]+pd$cow.pop[pd$iso3c=="ETH"&pd$year==1993])

# apply 1993 proportions to COW's ETH (including ERI) 1992 population estimate
eri.1992.est <- eri.1993.prop*pd$cow.pop[pd$iso3c=="ETH"&pd$year==1992]
eth.1992.est <- eth.1993.prop*pd$cow.pop[pd$iso3c=="ETH"&pd$year==1992]

# calculate growth estimates
eri.1992.1993.growth <- 100*(pd$cow.pop[pd$iso3c=="ERI"&pd$year==1993]-eri.1992.est)/eri.1992.est
eth.1992.1993.growth <- 100*(pd$cow.pop[pd$iso3c=="ETH"&pd$year==1993]-eth.1992.est)/eth.1992.est

# add 1950-1992 ERI UN estimates to ETH UN estimates
for(e in 1950:1992){
  
  pd$un.pop[pd$iso3c=="ETH"&pd$year==e] <- pd$un.pop[pd$iso3c=="ETH"&pd$year==e] + pd$un.pop[pd$iso3c=="ERI"&pd$year==e]
  
}

# capture COW growth rates
eth.cow.growth.1992.1993 <- (pd$cow.pop[pd$iso3c=="ETH"&pd$year==1993]+pd$cow.pop[pd$iso3c=="ERI"&pd$year==1993])/pd$cow.pop[pd$iso3c=="ETH"&pd$year==1992]
eri.cow.growth.1992.1993 <- (pd$cow.pop[pd$iso3c=="ETH"&pd$year==1993]+pd$cow.pop[pd$iso3c=="ERI"&pd$year==1993])/pd$cow.pop[pd$iso3c=="ETH"&pd$year==1992]

#### ISR/PSE ----------------------------------------------------------------------
# UN codes ISR and PSE separately 1950-2019
# COW only codes ISR, no data for PSE and not included in ISR

# 1950-2012 COW PSE: Calculate UN's PSE proportion of ISR population and apply to COW's ISR population
pse.pop.prop <- pd %>%
  dplyr::filter(iso3c %in% c("PSE","ISR"),
                # only years with both UN and COW data
                year %in% c(1950:2019)) %>%
  #tidyr::pivot_wider(names_from = iso3c, values_from = cow.pop)
  dplyr::group_by(year) %>%
  dplyr::mutate(isp.pop = sum(un.pop,na.rm=TRUE),
                cow.pop = sum(cow.pop,na.rm=TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(prop = un.pop / isp.pop,
                cow.pop = ifelse(iso3c=="PSE",(prop*cow.pop / (1-prop)),cow.pop),
                # replace 0s generated by mutating to NAs
                cow.pop = ifelse(cow.pop==0,NA,cow.pop)) %>%
  # ISR estimates not needed for additional PSE estimating
  dplyr::filter(iso3c != "ISR") %>%
  dplyr::select(-c(isp.pop,prop))

# estimate UN PSE 1948-1949 population by using a weighted 3-year growth rate
# un
un.pse.1950.1951.growth <- pse.pop.prop$un.pop[pse.pop.prop$iso3c=="PSE"&pse.pop.prop$year==1951]/
                              pse.pop.prop$un.pop[pse.pop.prop$iso3c=="PSE"&pse.pop.prop$year==1950]
un.pse.1951.1952.growth <- pse.pop.prop$un.pop[pse.pop.prop$iso3c=="PSE"&pse.pop.prop$year==1952]/
                              pse.pop.prop$un.pop[pse.pop.prop$iso3c=="PSE"&pse.pop.prop$year==1951]
un.pse.1952.1953.growth <- pse.pop.prop$un.pop[pse.pop.prop$iso3c=="PSE"&pse.pop.prop$year==1953]/
                              pse.pop.prop$un.pop[pse.pop.prop$iso3c=="PSE"&pse.pop.prop$year==1952]

un.pse.1949.1950.growth <- (1/2)*un.pse.1950.1951.growth + (1/3)*un.pse.1951.1952.growth + (1/6)*un.pse.1952.1953.growth
un.pse.1948.1949.growth <- (1/2)*un.pse.1949.1950.growth + (1/3)*un.pse.1950.1951.growth + (1/6)*un.pse.1951.1952.growth

# cow
cow.pse.1950.1951.growth <- pse.pop.prop$cow.pop[pse.pop.prop$iso3c=="PSE"&pse.pop.prop$year==1951]/
                               pse.pop.prop$cow.pop[pse.pop.prop$iso3c=="PSE"&pse.pop.prop$year==1950]
cow.pse.1951.1952.growth <- pse.pop.prop$cow.pop[pse.pop.prop$iso3c=="PSE"&pse.pop.prop$year==1952]/
                               pse.pop.prop$cow.pop[pse.pop.prop$iso3c=="PSE"&pse.pop.prop$year==1951]
cow.pse.1952.1953.growth <- pse.pop.prop$cow.pop[pse.pop.prop$iso3c=="PSE"&pse.pop.prop$year==1953]/
                               pse.pop.prop$cow.pop[pse.pop.prop$iso3c=="PSE"&pse.pop.prop$year==1952]

cow.pse.1949.1950.growth <- (1/2)*cow.pse.1950.1951.growth + (1/3)*cow.pse.1951.1952.growth + (1/6)*cow.pse.1952.1953.growth
cow.pse.1948.1949.growth <- (1/2)*cow.pse.1949.1950.growth + (1/3)*cow.pse.1950.1951.growth + (1/6)*cow.pse.1951.1952.growth

# add estimates to PSE dataset
pse.pop.prop <- pse.pop.prop %>%
  tibble::add_row(iso3c = "PSE",
                  country = "Palestinian Territories",
                  year = 1949,
                  un.pop = pse.pop.prop$un.pop[pse.pop.prop$iso3c=="PSE"&pse.pop.prop$year==1950]*un.pse.1949.1950.growth,
                  cow.pop = pse.pop.prop$cow.pop[pse.pop.prop$iso3c=="PSE"&pse.pop.prop$year==1950]*cow.pse.1949.1950.growth)

pse.pop.prop <- pse.pop.prop %>%
  tibble::add_row(iso3c = "PSE",
                  country = "Palestinian Territories",
                  year = 1948,
                  un.pop = pse.pop.prop$un.pop[pse.pop.prop$iso3c=="PSE"&pse.pop.prop$year==1949]*un.pse.1948.1949.growth,
                  cow.pop = pse.pop.prop$cow.pop[pse.pop.prop$iso3c=="PSE"&pse.pop.prop$year==1949]*cow.pse.1948.1949.growth)

# 2013-2019: apply PSE UN growth rates to COW's PSE 2012 population estimate
pse.pop.prop <- pop_growth_estimator_cow_func(pse.pop.prop, "PSE", yr = 2012, restricted = c(2013:2019))

# estimate UN ISR 1948-1949 population estimates using COW ISR 1948-1949 relative proportion to
# COW ISR 1950 population estimate
pd <- pop_growth_estimator_un_func(pd, "ISR", yr = 1950, restricted = c(1948:1949))

# estimate COW ISR 2013-2019 population estimates using UN ISR relative proportion to COW ISR 2012 population estimate
pd <- pop_growth_estimator_cow_func(pd, "ISR", yr = 2012, restricted = c(2013:2019))

# filter PSE out of main dataset and rbind PSE estimate dataset
pd <- pd %>%
  dplyr::filter(iso3c != "PSE") %>%
  rbind(pse.pop.prop)
  
# calculate a combined Israel/Palestine population estimate ("ISP")
pd.isr.pse <- pd %>%
  dplyr::filter(iso3c %in% c("ISR","PSE")) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(un.pop = sum(un.pop,na.rm=TRUE),
                   cow.pop = sum(cow.pop,na.rm=TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(iso3c = "ISP",
                country = "Israel/Palestine")

# add ISP estimates to main dataset
pd <- pd %>%
  rbind(pd.isr.pse)

#### JPN ----------------------------------------------------------------------
# 1950-1951: apply UN population growth rates to COW's 1952 population estimate
pd <- pop_growth_estimator_cow_func(pd, "JPN", yr = 1952, restricted = c(1950:1951))

# 1946-1949: apply MPD population growth rates to UN's and COW's 1950 population estimates
pd <- pop_growth_estimator_mpd_func(pd, mpd, "JPN", yr = 1950, restricted = c(1946:1949))

#### KOR ----------------------------------------------------------------------
# 1949: apply COW population growth rate to UN's 1950 population estimate
pd <- pop_growth_estimator_un_func(pd, "KOR", yr = 1950, restricted = 1949)

# 1946-1948: apply MPD population growth rates to UN's and COW's 1949 population estimates
pd <- pop_growth_estimator_mpd_func(pd, mpd, "KOR", yr = 1949, restricted = c(1946:1948))

#### LIE ----------------------------------------------------------------------
# UN has data from 1950-2019; COW data starts in 1990

# 1950-1989: apply UN population growth rates to COW's 1990 population estimate
pd <- pop_growth_estimator_cow_func(pd, "LIE", yr = 1990, restricted = c(1950:1989))

#### MCO ----------------------------------------------------------------------
# UN has data from 1950-2019; COW data starts in 1993

# 1950-1992: apply UN population growth rates to COW's 1993 population estimate
pd <- pop_growth_estimator_cow_func(pd, "MCO", yr = 1993, restricted = c(1950:1992))

#### MYS/SGP ----------------------------------------------------------------------
# UN: MYS and SGP coded separately 1950-2019
# COW: 1957-1964 MYS coded as MYS+SGP, MYS 1965- is solely Malaysia; SGP coded starting 1965

# MYS coded as gaining independence in 1957
# SGP coded as gaining independence in 1965
# SGP part of MYS 1963-1964

# capture UN growth rates
mys.un.growth.1962.1963 <- (pd$un.pop[pd$iso3c=="MYS"&pd$year==1963]+pd$un.pop[pd$iso3c=="SGP"&pd$year==1963])/
                               (pd$un.pop[pd$iso3c=="MYS"&pd$year==1962]+pd$un.pop[pd$iso3c=="SGP"&pd$year==1962])
mys.un.growth.1964.1965 <- pd$un.pop[pd$iso3c=="MYS"&pd$year==1965]/pd$un.pop[pd$iso3c=="MYS"&pd$year==1964]
sgp.un.growth.1964.1965 <- pd$un.pop[pd$iso3c=="SGP"&pd$year==1965]/pd$un.pop[pd$iso3c=="SGP"&pd$year==1964]

# 1957-1964: use UN proportions to separate COW's MYS/SGP estimates
pd.mys.sgp <- pd %>%
  dplyr::filter(iso3c %in% c("MYS","SGP"),
                year %in% c(1957:1964)) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(cow.pop = sum(cow.pop,na.rm=TRUE),
                un.mys.sgp = sum(un.pop,na.rm=TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(cow.est = cow.pop * (un.pop / un.mys.sgp)) %>%
  dplyr::select(-c(un.mys.sgp,un.pop,cow.pop))

# add new COW estimates to main dataset
pd <- pd %>%
  dplyr::left_join(pd.mys.sgp,by=c("iso3c","country","year")) %>%
  dplyr::mutate(cow.pop = ifelse(!is.na(cow.est),cow.est,cow.pop)) %>%
  dplyr::select(-cow.est)

# pull MYS (excluding NAM) 1962-1963 and 1964-1965 and growth rates for use later
mys.un.growth.1962.1963 <- pd$un.pop[pd$iso3c=="MYS"&pd$year==1963]/pd$un.pop[pd$iso3c=="MYS"&pd$year==1962]
mys.cow.growth.1962.1963 <- pd$cow.pop[pd$iso3c=="MYS"&pd$year==1963]/pd$cow.pop[pd$iso3c=="MYS"&pd$year==1962]
mys.un.growth.1965.1964 <- pd$un.pop[pd$iso3c=="MYS"&pd$year==1965]/pd$un.pop[pd$iso3c=="MYS"&pd$year==1964]
mys.cow.growth.1965.1964 <- pd$cow.pop[pd$iso3c=="MYS"&pd$year==1965]/pd$cow.pop[pd$iso3c=="MYS"&pd$year==1964]

# add SGP to MYS for 1963-1964
for(s in 1963:1964){
  
  pd$un.pop[pd$iso3c=="MYS"&pd$year==s] <- pd$un.pop[pd$iso3c=="MYS"&pd$year==s] + pd$un.pop[pd$iso3c=="SGP"&pd$year==s]
  pd$cow.pop[pd$iso3c=="MYS"&pd$year==s] <- pd$cow.pop[pd$iso3c=="MYS"&pd$year==s] + pd$cow.pop[pd$iso3c=="SGP"&pd$year==s]
  
}

# capture COW growth rates
mys.cow.growth.1962.1963 <- pd$cow.pop[pd$iso3c=="MYS"&pd$year==1963]/
                               (pd$cow.pop[pd$iso3c=="MYS"&pd$year==1962]+pd$cow.pop[pd$iso3c=="SGP"&pd$year==1962])
mys.cow.growth.1964.1965 <- (pd$cow.pop[pd$iso3c=="MYS"&pd$year==1965]+pd$cow.pop[pd$iso3c=="SGP"&pd$year==1965])/
                               pd$cow.pop[pd$iso3c=="MYS"&pd$year==1964]
sgp.cow.growth.1964.1965 <- (pd$cow.pop[pd$iso3c=="MYS"&pd$year==1965]+pd$cow.pop[pd$iso3c=="SGP"&pd$year==1965])/
                               pd$cow.pop[pd$iso3c=="MYS"&pd$year==1964]


#### OMN ----------------------------------------------------------------------
# 1950-1970: apply UN population growth rates to COW's 1971 population estimate
pd <- pop_growth_estimator_cow_func(pd, "OMN", yr = 1971, restricted = c(1950:1970))

#### PAK/BGD ----------------------------------------------------------------------
# UN codes PAK and BGD as separate; COW codes PAK as PAK+BGD through 1970 (inclusive)

# pull 1970-1971 PAK (excluding BGD) and BGD growth rates for use later
# use PAK+BGD 1970-1971 COW growth for PAK and BGD
pak.un.growth.1970.1971 <- pd$un.pop[pd$iso3c=="PAK"&pd$year==1971]/pd$un.pop[pd$iso3c=="PAK"&pd$year==1970]
bgd.un.growth.1970.1971 <- pd$un.pop[pd$iso3c=="BGD"&pd$year==1971]/pd$un.pop[pd$iso3c=="BGD"&pd$year==1970]
pak.cow.growth.1970.1971 <- (pd$cow.pop[pd$iso3c=="PAK"&pd$year==1971]+pd$cow.pop[pd$iso3c=="BGD"&pd$year==1971])/pd$cow.pop[pd$iso3c=="PAK"&pd$year==1970]
bgd.cow.growth.1970.1971 <- (pd$cow.pop[pd$iso3c=="PAK"&pd$year==1971]+pd$cow.pop[pd$iso3c=="BGD"&pd$year==1971])/pd$cow.pop[pd$iso3c=="PAK"&pd$year==1970]

# combine UN BGD 1950-1970 populations into PAK
for(p in 1950:1970){
  
  pd$un.pop[pd$iso3c=="PAK"&pd$year==p] <- pd$un.pop[pd$iso3c=="PAK"&pd$year==p] + pd$un.pop[pd$iso3c=="BGD"&pd$year==p]
  
}

#### PRK ----------------------------------------------------------------------
# 1948-1949: apply COW population growth rate to UN's 1950 population estimate
pd <- pop_growth_estimator_un_func(pd, "PRK", yr = 1950, restricted = c(1948:1949))

# 1946-1947: apply MPD population growth rates to UN's and COW's 1949 population estimates
for(p in 1947:1946){
  
  un.growth.rate <- (1/2)*(df$un.pop[df$iso3c==iso&df$year==(p+2)]/df$un.pop[df$iso3c==iso&df$year==(p+1)]) +
    (1/3)*(df$un.pop[df$iso3c==iso&df$year==(p+3)]/df$un.pop[df$iso3c==iso&df$year==(p+2)]) +
    (1/6)*(df$un.pop[df$iso3c==iso&df$year==(p+4)]/df$un.pop[df$iso3c==iso&df$year==(p+3)])
  
  cow.growth.rate <- (1/2)*(df$cow.pop[df$iso3c==iso&df$year==(p+2)]/df$cow.pop[df$iso3c==iso&df$year==(p+1)]) +
    (1/3)*(df$cow.pop[df$iso3c==iso&df$year==(p+3)]/df$cow.pop[df$iso3c==iso&df$year==(p+2)]) +
    (1/6)*(df$cow.pop[df$iso3c==iso&df$year==(p+4)]/df$cow.pop[df$iso3c==iso&df$year==(p+3)])              
  
  pd <- pd %>%
    tibble::add_row(iso3c = "PRK",
                    country = "North Korea",
                    year = p,
                    un.pop = pd$un.pop[pd$iso3c=="PRK"&pd$year==(p+1)]/un.growth.rate,
                    cow.pop = pd$cow.pop[pd$iso3c=="PRK"&pd$year==(p+1)]/cow.growth.rate)
  
}

#### SDN/SSD ----------------------------------------------------------------------
# UN codes SDN as SDN (excluding SSD) throughout the time series
# COW appears to code SDN as SDN (excluding SSD) throughout the time series
# UN has full SSD time series data, while COW only starts in 2011

# pull SDN 2010-2011 (excluding SSD) and SSD growth rates for use later (use SSD UN growth rate for COW)
sdn.un.growth.2010.2011 <- pd$un.pop[pd$iso3c=="SDN"&pd$year==2011]/pd$un.pop[pd$iso3c=="SDN"&pd$year==2010]
sdn.cow.growth.2010.2011 <- pd$cow.pop[pd$iso3c=="SDN"&pd$year==2011]/pd$cow.pop[pd$iso3c=="SDN"&pd$year==2010]
ssd.un.growth.2010.2011 <- pd$un.pop[pd$iso3c=="SSD"&pd$year==2011]/pd$un.pop[pd$iso3c=="SSD"&pd$year==2010]
ssd.cow.growth.2010.2011 <- pd$un.pop[pd$iso3c=="SSD"&pd$year==2011]/pd$un.pop[pd$iso3c=="SSD"&pd$year==2010]

# 1950-2010: apply UN SSD growth rates to COW's 2011 population estimate
pd <- pop_growth_estimator_cow_func(pd, "SSD", yr = 2011, restricted = c(1950:2010))

# 1950-2010: add SSD estimates to SDN estimates
for(s in 1950:2010){
  
  pd$un.pop[pd$iso3c=="SDN"&pd$year==s] <- pd$un.pop[pd$iso3c=="SDN"&pd$year==s] + pd$un.pop[pd$iso3c=="SSD"&pd$year==s]
  pd$cow.pop[pd$iso3c=="SDN"&pd$year==s] <- pd$cow.pop[pd$iso3c=="SDN"&pd$year==s] + pd$cow.pop[pd$iso3c=="SSD"&pd$year==s]
  
}

#### SMR ----------------------------------------------------------------------
# UN has data from 1950-2019; COW data starts in 1992

# 1950-1991: apply UN population growth rates to COW's 1992 population estimate
pd <- pop_growth_estimator_cow_func(pd, "SMR", yr = 1992, restricted = c(1950:1991))

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

# estimate COW growth rates for successor states
sov.successors.cow.growth.1990.1991 <- sum(pd$cow.pop[pd$iso3c %in% c("EST","LVA","LTU","BLR","UKR","MDA","RUS",
                                                                      "GEO","ARM","AZE","KAZ","KGZ","TKM","TJK",
                                                                      "UZB")&pd$year==1991]) / pd$cow.pop[pd$iso3c=="SOV"&pd$year==1990]

#### SYR ----------------------------------------------------------------------
# COW missing 1959-1960 estimates

# use growth rates from UN, scaled by proportion alpha to ensure the end growth rate
# matches with the 1961 COW population estimate

alpha <- ((pd$cow.pop[pd$iso3c=="SYR"&pd$year==1961]*pd$un.pop[pd$iso3c=="SYR"&pd$year==1958])/
            (pd$cow.pop[pd$iso3c=="SYR"&pd$year==1958] * pd$un.pop[pd$iso3c=="SYR"&pd$year==1961]))^(1/3)

# calculate UN growth rates
syr.un.growth.59 <- pd$un.pop[pd$iso3c=="SYR"&pd$year==1959]/pd$un.pop[pd$iso3c=="SYR"&pd$year==1958]
syr.un.growth.60 <- pd$un.pop[pd$iso3c=="SYR"&pd$year==1960]/pd$un.pop[pd$iso3c=="SYR"&pd$year==1959]

# apply alpha scalar to UN growth rates
syr.cow.growth.59 <- alpha*syr.un.growth.59
syr.cow.growth.60 <- alpha*syr.un.growth.60

# calculate COW estimates
pd$cow.pop[pd$iso3c=="SYR"&pd$year==1959] <- syr.cow.growth.59*pd$cow.pop[pd$iso3c=="SYR"&pd$year==1958]
pd$cow.pop[pd$iso3c=="SYR"&pd$year==1960] <- syr.cow.growth.60*pd$cow.pop[pd$iso3c=="SYR"&pd$year==1959]

#### TWN ----------------------------------------------------------------------
# 1949: apply COW's 1949-1950 growth rate to UN data
pd$un.pop[pd$iso3c=="TWN"&pd$year==1949] <- pd$un.pop[pd$iso3c=="TWN"&pd$year==1950]/
  (pd$cow.pop[pd$iso3c=="TWN"&pd$year==1950]/pd$cow.pop[pd$iso3c=="TWN"&pd$year==1949])

# 1946-1948: apply MPD population growth rates to UN's and COW's 1949 population estimates
pd <- pop_growth_estimator_mpd_func(pd, mpd, "TWN", yr = 1949, restricted = c(1946:1948))

#### TZA/ZAN ----------------------------------------------------------------------
# UN codes TZA for Tanganyika+Zanzibar for 1950-2019
# COW TZA is for Tanganyika through 1964, Tanzania 1965 onward; COW ZAN coded for 1963-1964

# Tanzania unification coded beginning in 1964

# 1963: calculate proportion of ZAN from combined ZAN+TZA COW estimates and remove that proportion
# from 1963 TZA UN estimate

# 1961-1962: extend COW ZAN estimates by using UN's TZA+ZAN growth applied to COW's TZA estimates and the
# missing COW ZAN estimates

# add blank rows
pd <- pd %>%
  rbind(data.frame(iso3c=rep("ZAN",2),
                   country=rep("Zanzibar",2),
                   year=c(1961:1962),
                   un.pop=rep(NA,2),
                   cow.pop=rep(NA,2)))

for(z in 1962:1961){
  
  pd$cow.pop[pd$iso3c=="ZAN"&pd$year==z] <- ((pd$cow.pop[pd$iso3c=="TZA"&pd$year==(z+1)]+pd$cow.pop[pd$iso3c=="ZAN"&pd$year==(z+1)])*
    (pd$un.pop[pd$iso3c=="TZA"&pd$year==z]/pd$un.pop[pd$iso3c=="TZA"&pd$year==(z+1)]))-pd$cow.pop[pd$iso3c=="TZA"&pd$year==z]
}

# calculate COW's 1961-1964 ZAN proportion of TZA+ZAN and apply to UN's combined TZA/ZAN population estimate
# calculate COW's 1961-1964 TZA proportion of TZA+ZAN and apply to UN's combined TZA/ZAN population estimate
for(t in 1961:1964){
  
  pd$un.pop[pd$iso3c=="ZAN"&pd$year==t] <- pd$un.pop[pd$iso3c=="TZA"&pd$year==t] * pd$cow.pop[pd$iso3c=="ZAN"&pd$year==t] /
                                              (pd$cow.pop[pd$iso3c=="TZA"&pd$year==t] + pd$cow.pop[pd$iso3c=="ZAN"&pd$year==t])  
  pd$un.pop[pd$iso3c=="TZA"&pd$year==t] <- pd$un.pop[pd$iso3c=="TZA"&pd$year==t] * pd$cow.pop[pd$iso3c=="TZA"&pd$year==t] /
                                              (pd$cow.pop[pd$iso3c=="TZA"&pd$year==t] + pd$cow.pop[pd$iso3c=="ZAN"&pd$year==t])
  
}

# capture growth rates
tza.un.growth.1963.1964 <- pd$un.pop[pd$iso3c=="TZA"&pd$year==1964] /
  (pd$un.pop[pd$iso3c=="TZA"&pd$year==1963] + pd$un.pop[pd$iso3c=="ZAN"&pd$year==1963])
tza.cow.growth.1963.1964 <- pd$cow.pop[pd$iso3c=="TZA"&pd$year==1964] /
  (pd$cow.pop[pd$iso3c=="TZA"&pd$year==1963] + pd$cow.pop[pd$iso3c=="ZAN"&pd$year==1963])

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

# capture growth rates
yem.un.growth.1990.1991 <- pd$un.pop[pd$iso3c=="YEM"&pd$year==1991] /
  (pd$un.pop[pd$iso3c=="YAR"&pd$year==1990] + pd$un.pop[pd$iso3c=="YPR"&pd$year==1990])
yem.cow.growth.1990.1991 <- pd$cow.pop[pd$iso3c=="YEM"&pd$year==1991] /
  (pd$cow.pop[pd$iso3c=="YAR"&pd$year==1990] + pd$cow.pop[pd$iso3c=="YPR"&pd$year==1990])

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

# capture growth rates
vnm.un.growth.1975.1976 <- pd$un.pop[pd$iso3c=="VNM"&pd$year==1976] /
  (pd$un.pop[pd$iso3c=="VNM"&pd$year==1975] + pd$un.pop[pd$iso3c=="RVN"&pd$year==1975])
vnm.cow.growth.1975.1976 <- pd$cow.pop[pd$iso3c=="VNM"&pd$year==1976] /
  (pd$cow.pop[pd$iso3c=="VNM"&pd$year==1975] + pd$cow.pop[pd$iso3c=="RVN"&pd$year==1975])

#### YUG/SRB/MNE/KSV ----------------------------------------------------------------------
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

pd.yug <- pop_growth_estimator_un_func(pd.yug, "YUG", yr = 1950, restricted = c(1946:1949))

# filter out YUG 1946-1991 entries from main dataset and rbind new estimates
pd <- pd %>%
  dplyr::filter(iso3c != "YUG" | year %!in% c(1946:1991)) %>%
  rbind(pd.yug)

# apply UN 1992-1993 MKD growth rate to COW estimate to approximate 1992 population
# COW likely does not code 1992 because MKD only gained universal recognition in 1993
# when Greece dropped objections over MKD's name
pd$cow.pop[pd$iso3c=="MKD"&pd$year==1992] <- pd$cow.pop[pd$iso3c=="MKD"&pd$year==1993]/
                                                (pd$un.pop[pd$iso3c=="MKD"&pd$year==1993]/pd$un.pop[pd$iso3c=="MKD"&pd$year==1992])

# COW: For YUG (SRB), subtract MNE (starting in 2006) and KSV (starting in 2008)
# UN: For SRB, subtract COW's KSV (starting in 2008)

# subtract MNE from COW's estimate
for(s in 2006:2007){
  
  pd$cow.pop[pd$iso3c=="YUG"&pd$year==s] <- pd$cow.pop[pd$iso3c=="YUG"&pd$year==s] - pd$cow.pop[pd$iso3c=="MNE"&pd$year==s]
  
}

# subtract KSV and MNE from COW's estimate and KSV from UN's estimate
for(s in 2008:2012){
  
  pd$cow.pop[pd$iso3c=="YUG"&pd$year==s] <- pd$cow.pop[pd$iso3c=="YUG"&pd$year==s] - pd$cow.pop[pd$iso3c=="MNE"&pd$year==s] -
                                              pd$cow.pop[pd$iso3c=="KSV"&pd$year==s]
 
  pd$un.pop[pd$iso3c=="SRB"&pd$year==s] <- pd$un.pop[pd$iso3c=="SRB"&pd$year==s] - pd$cow.pop[pd$iso3c=="KSV"&pd$year==s]
   
}

# recode YUG 1992-2012 as SRB
for(s in 1992:2012){
  
  pd$cow.pop[pd$iso3c=="SRB"&pd$year==s] <- pd$cow.pop[pd$iso3c=="YUG"&pd$year==s]
  
}

# UN SRB/KSV 2013-2019: apply a three-year weighted growth rate to SRB and KSV (independently), then
# calculate the annual alpha ratio between the estimated SRB+KSV sum and the UN estimate for a combined SRB+KSV,
# then scale the SRB and KSV estimates annually by this alpha

# create blank dataframe to hold values calculated in the following for loop
pd.srb.ksv <- data.frame(year = c(2009:2019),
                                   ksv.growth = NA,
                                   srb.growth = NA,
                                   ksv.est = NA,
                                   srb.est = NA)

# add KSV and SRB 2009-2012 values to the table
for(b in 2009:2012){
  
  pd.srb.ksv$ksv.est[pd.srb.ksv$year==b] <- pd$cow.pop[pd$iso3c=="KSV"&pd$year==b]
  pd.srb.ksv$srb.est[pd.srb.ksv$year==b] <- pd$un.pop[pd$iso3c=="SRB"&pd$year==b&!is.na(pd$un.pop)]
  
}

for(y in 2013:2019){
  
  # calculate growth estimates
  srb.un.growth <- (1/2)*(pd.srb.ksv$srb.est[pd.srb.ksv$year==(y-1)]/
                            pd.srb.ksv$srb.est[pd.srb.ksv$year==(y-2)]) +
                   (1/3)*(pd.srb.ksv$srb.est[pd.srb.ksv$year==(y-2)]/
                            pd.srb.ksv$srb.est[pd.srb.ksv$year==(y-3)]) +
                   (1/6)*(pd.srb.ksv$srb.est[pd.srb.ksv$year==(y-3)]/
                            pd.srb.ksv$srb.est[pd.srb.ksv$year==(y-4)])
  
  ksv.cow.growth <- (1/2)*(pd.srb.ksv$ksv.est[pd.srb.ksv$year==(y-1)]/
                            pd.srb.ksv$ksv.est[pd.srb.ksv$year==(y-2)]) +
                   (1/3)*(pd.srb.ksv$ksv.est[pd.srb.ksv$year==(y-2)]/
                            pd.srb.ksv$ksv.est[pd.srb.ksv$year==(y-3)]) +
                   (1/6)*(pd.srb.ksv$ksv.est[pd.srb.ksv$year==(y-3)]/
                            pd.srb.ksv$ksv.est[pd.srb.ksv$year==(y-4)])
  
  # add growth estimates to placeholder dataframe
  pd.srb.ksv$ksv.growth[pd.srb.ksv$year==y] <- ksv.cow.growth
  pd.srb.ksv$srb.growth[pd.srb.ksv$year==y] <- srb.un.growth
  
  # calculate population estimates and add to placeholder dataframe
  pd.srb.ksv$ksv.est[pd.srb.ksv$year==y] <- pd.srb.ksv$ksv.est[pd.srb.ksv$year==(y-1)] * ksv.cow.growth
  pd.srb.ksv$srb.est[pd.srb.ksv$year==y] <- pd.srb.ksv$srb.est[pd.srb.ksv$year==(y-1)] * srb.un.growth

}

pd.srb.ksv <- pd.srb.ksv %>%
  # add original SRB 2013-2019 UN population estimates to table (representing SRB+KSV)
  dplyr::left_join(pd %>%
                     dplyr::filter(iso3c=="SRB") %>%
                     dplyr::select(year,un.pop) %>%
                     na.omit()) %>%
  # calculate combined KSV+SRB estimate, annual alpha rate ((ksv.est+srb.est)/srb.un.combined),
  # and readjusted estimates multiplied by the alpha rate
  # the alpha rate multiplication ensures the combined KSV and SRB estimates line up with the official,
  # combined UN estimates
  dplyr::mutate(combined.est = ksv.est + srb.est,
                alpha = un.pop / combined.est,
                ksv.est.alpha = alpha * ksv.est,
                srb.est.alpha = alpha * srb.est) %>%
  dplyr::select(year,ksv.est.alpha,srb.est.alpha) %>%
  tidyr::pivot_longer(2:3, names_to = "iso3c", values_to = "un.pop") %>%
  dplyr::mutate(iso3c = ifelse(iso3c=="ksv.est.alpha","KSV","SRB"),
                country = ifelse(iso3c=="KSV","Kosovo","Serbia"),
                cow.pop = NA)

# add KSV and SRB COW 2009-2012 estimates to the reformatted pd.srb.ksv dataset
for(j in 2009:2012){
  
  pd.srb.ksv$cow.pop[pd.srb.ksv$iso3c=="KSV"&pd.srb.ksv$year==j] <- pd$cow.pop[pd$iso3c=="KSV"&pd$year==j]
  pd.srb.ksv$cow.pop[pd.srb.ksv$iso3c=="SRB"&pd.srb.ksv$year==j] <- pd$cow.pop[pd$iso3c=="SRB"&pd$year==j]
  
}

# 2013-2019 COW KSV/SRB: apply three-year weighted growth rates to 2012 population estimates

for(k in 2013:2019){
  
  for(i in c("SRB","KSV")){
    
    # calculate growth rate
    growth.estimate <- (1/2)*pd.srb.ksv$cow.pop[pd.srb.ksv$iso3c==i&pd.srb.ksv$year==(k-1)]/
                                pd.srb.ksv$cow.pop[pd.srb.ksv$iso3c==i&pd.srb.ksv$year==(k-2)] +
                       (1/3)*pd.srb.ksv$cow.pop[pd.srb.ksv$iso3c==i&pd.srb.ksv$year==(k-2)]/
                                pd.srb.ksv$cow.pop[pd.srb.ksv$iso3c==i&pd.srb.ksv$year==(k-3)] +
                       (1/6)*pd.srb.ksv$cow.pop[pd.srb.ksv$iso3c==i&pd.srb.ksv$year==(k-3)]/
                                pd.srb.ksv$cow.pop[pd.srb.ksv$iso3c==i&pd.srb.ksv$year==(k-4)]
    
    # apply growth rate
    pd.srb.ksv$cow.pop[pd.srb.ksv$iso3c==i&pd.srb.ksv$year==k] <- pd.srb.ksv$cow.pop[pd.srb.ksv$iso3c==i&pd.srb.ksv$year==(k-1)]*growth.estimate
    
  }

}

# filter 2009-2019 SRB and KSV entries from main dataset and rbind new estimates
pd <- pd %>%
  dplyr::filter(iso3c %!in% c("SRB","KSV") | year %!in% 2009:2019) %>%
  rbind(pd.srb.ksv)

# apply COW's 2008-2009 growth rate to UN's 2009 estimate to approximate UN's 2008 estimate
pd$un.pop[pd$iso3c=="KSV"&pd$year==2008] <- pd$un.pop[pd$iso3c=="KSV"&pd$year==2009] /
                                               (pd$cow.pop[pd$iso3c=="KSV"&pd$year==2009]/pd$cow.pop[pd$iso3c=="KSV"&pd$year==2008])

# remove YUG 1992-2012 from main dataset
pd <- pd %>%
  dplyr::filter(iso3c != "YUG" | year %!in% c(1992:2012))

# estimate COW growth rates for YUG successor states
yug.successors.cow.growth.1991.1992 <- sum(pd$cow.pop[pd$iso3c %in% c("SVN","HRV","BIH","SRB","MKD")&pd$year==1992]) /
                                          pd$cow.pop[pd$iso3c=="YUG"&pd$year==1991]

# estimate COW SRB and MNE growth rates for 2006
srb.cow.growth.2005.2006 <- (pd$un.pop[pd$iso3c=="SRB"&pd$year==2006]+pd$un.pop[pd$iso3c=="MNE"&pd$year==2006])/pd$un.pop[pd$iso3c=="SRB"&pd$year==2005]
mne.cow.growth.2005.2006 <- (pd$un.pop[pd$iso3c=="SRB"&pd$year==2006]+pd$un.pop[pd$iso3c=="MNE"&pd$year==2006])/pd$un.pop[pd$iso3c=="SRB"&pd$year==2005]

# estimate COW SRB and KSV growth rates for 2008
srb.cow.growth.2007.2008 <- (pd$un.pop[pd$iso3c=="SRB"&pd$year==2008]+pd$un.pop[pd$iso3c=="KSV"&pd$year==2008])/pd$un.pop[pd$iso3c=="SRB"&pd$year==2007]
ksv.cow.growth.2007.2008 <- (pd$un.pop[pd$iso3c=="SRB"&pd$year==2008]+pd$un.pop[pd$iso3c=="KSV"&pd$year==2008])/pd$un.pop[pd$iso3c=="SRB"&pd$year==2007]

#### ZAF/NAM ----------------------------------------------------------------------
# Both UN and COW ZAF populations do not include NAM prior to NAM independence
# NAM coded as independent beginning in 1990

# NAM 1950-1989: apply UN population growth rates to COW's 1990 population estimate
pd <- pop_growth_estimator_cow_func(pd, "NAM", yr = 1990, restricted = c(1950:1989))

# NAM 1946-1949: apply a weighted 3-year growth rate
pd <- pop_growth_estimator_no_data_func(pd, "NAM")

# ZAF 1946-1949: apply COW population growth rates to UN's 1950 population estimate
pd <- pop_growth_estimator_un_func(pd, "ZAF", yr = 1950, restricted = c(1946:1949))

# pull ZAF 1989-1990 (excluding NAM) and NAM growth rates for use later
zaf.un.growth.1989.1990 <- pd$un.pop[pd$iso3c=="ZAF"&pd$year==1990]/pd$un.pop[pd$iso3c=="ZAF"&pd$year==1989]
zaf.cow.growth.1989.1990 <- pd$cow.pop[pd$iso3c=="ZAF"&pd$year==1990]/pd$cow.pop[pd$iso3c=="ZAF"&pd$year==1989]
nam.un.growth.1989.1990 <- pd$un.pop[pd$iso3c=="NAM"&pd$year==1990]/pd$un.pop[pd$iso3c=="NAM"&pd$year==1989]
nam.cow.growth.1989.1990 <- pd$cow.pop[pd$iso3c=="NAM"&pd$year==1990]/pd$cow.pop[pd$iso3c=="NAM"&pd$year==1989]

# merge ZAF and NAM populations for 1946-1989
for(z in 1946:1989){
  
  pd$un.pop[pd$iso3c=="ZAF"&pd$year==z] <- pd$un.pop[pd$iso3c=="ZAF"&pd$year==z] + pd$un.pop[pd$iso3c=="NAM"&pd$year==z]
  pd$cow.pop[pd$iso3c=="ZAF"&pd$year==z] <- pd$cow.pop[pd$iso3c=="ZAF"&pd$year==z] + pd$cow.pop[pd$iso3c=="NAM"&pd$year==z]
  
}

### calculate 1940s estimates ----------------------------------------------------------------------
# list of countries to estimate UN 1946-1949 populations using COW growth rates
# flagging: PAK
estimate.un.1940s <- c("AFG", "ALB", "AND", "ARG", "AUS", "BEL", "BGR", "BOL", "BRA", "CAN", "CHE", "CHL", "CHN", "COL", "CRI",
                       "CUB", "CZE", "DNK", "DOM", "ECU", "EGY", "ESP", "ETH", "FIN", "FRA", "GBR", "GRC", "GTM", "HND", "HTI",
                       "HUN", "IDN", "IND", "IRL", "IRN", "IRQ", "ISL", "ITA", "JOR", "LBN", "LBR", "LIE", "LKA", "LUX", "MCO",
                       "MEX", "MMR", "MNG", "NIC", "NLD", "NOR", "NPL", "NZL", "PAK", "PAN", "PER", "PHL", "POL", "PRT", "PRY",
                       "ROU", "SAU", "SLV", "SMR", "SOV", "SWE", "SYR", "THA", "TUR", "URY", "USA", "VEN", "YAR", "YUG")

# 1946-1949: apply COW population growth rates to UN's 1950 population estimate
for(iso in estimate.un.1940s){
  
  pd <- pop_growth_estimator_un_func(pd, iso, yr = 1950, restricted = c(1946:1949))
  
}

# list of countries to estimate UN and COW 1946-1949 populations using a three-year weighted growth rate
estimate.growth.1940s <- c("AND", "LIE", "MCO", "SMR")

# 1946-1949: apply three-year weighted growth rate
for(iso in estimate.growth.1940s){
  
  pd <- pop_growth_estimator_no_data_func(pd, iso)

}

### calculate 2013-2019 estimates ----------------------------------------------------------------------
# list of countries to estimate COW 2013-2019 populations using UN growth rates
estimate.cow.2010s <- c("AFG", "AGO", "ALB", "AND", "ARE", "ARG", "ARM", "ATG", "AUS", "AUT", "AZE", "BDI", "BEL", "BEN", "BFA",
                        "BGD", "BGR", "BHR", "BHS", "BIH", "BLR", "BLZ", "BOL", "BRA", "BRB", "BRN", "BTN", "BWA", "CAF", "CAN",
                        "CHE", "CHL", "CHN", "CIV", "CMR", "COD", "COG", "COL", "COM", "CPV", "CRI", "CUB", "CYP", "CZE", "DEU",
                        "DJI", "DMA", "DNK", "DOM", "DZA", "ECU", "EGY", "ERI", "ESP", "EST", "ETH", "FIN", "FJI", "FRA", "FSM",
                        "GAB", "GBR", "GEO", "GHA", "GIN", "GMB", "GNB", "GNQ", "GRC", "GRD", "GTM", "GUY", "HND", "HRV", "HTI",
                        "HUN", "IDN", "IND", "IRL", "IRN", "IRQ", "ISL", "ITA", "JAM", "JOR", "JPN", "KAZ", "KEN", "KGZ", "KHM",
                        "KIR", "KNA", "KOR", "KWT", "LAO", "LBN", "LBR", "LBY", "LCA", "LIE", "LKA", "LSO", "LTU", "LUX", "LVA",
                        "MAR", "MCO", "MDA", "MDG", "MDV", "MEX", "MHL", "MKD", "MLI", "MLT", "MMR", "MNE", "MNG", "MOZ", "MRT",
                        "MUS", "MWI", "MYS", "NAM", "NER", "NGA", "NIC", "NLD", "NOR", "NPL", "NRU", "NZL", "OMN", "PAK", "PAN",
                        "PER", "PHL", "PLW", "PNG", "POL", "PRK", "PRT", "PRY", "QAT", "ROU", "RUS", "RWA", "SAU", "SDN", "SEN",
                        "SGP", "SLB", "SLE", "SLV", "SMR", "SOM", "SSD", "STP", "SUR", "SVK", "SVN", "SWE", "SWZ", "SYC", "SYR",
                        "TCD", "TGO", "THA", "TJK", "TKM", "TLS", "TON", "TTO", "TUN", "TUR", "TUV", "TWN", "TZA", "UGA", "UKR",
                        "URY", "USA", "UZB", "VCT", "VEN", "VNM", "VUT", "WSM", "YEM", "ZAF", "ZMB", "ZWE")

# 2013-2019: apply UN population growth rates to COW's 2012 population estimate
for(iso in estimate.cow.2010s){
  
  pd <- pop_growth_estimator_cow_func(pd, iso, yr = 2012, restricted = c(2013:2019))
  
}

### calculate growth rates ----------------------------------------------------------------------
pd_year_prior <- pd %>%
  dplyr::mutate(year = year + 1) %>%
  dplyr::rename(un.pop.plus1 = un.pop,
                cow.pop.plus1 = cow.pop)

pd <- pd %>%
  dplyr::left_join(pd_year_prior,by=c("iso3c","country","year")) %>%
  dplyr::mutate(pop.growth.rate.un = 100 * (un.pop - un.pop.plus1) / un.pop.plus1,
                pop.growth.rate.cow = 100 * (cow.pop - cow.pop.plus1)/ cow.pop.plus1) %>%
  dplyr::select(-c(un.pop.plus1,cow.pop.plus1))

#### modify growth rates for country unification/dissolution ----------------------------------------------------------------------
# Czechia/Slovakia
pd$pop.growth.rate.un[pd$iso3c=="CZE"&pd$year==1993] <- cze.un.growth.1992.1993 - 1
pd$pop.growth.rate.un[pd$iso3c=="SVK"&pd$year==1993] <- svk.un.growth.1992.1993 - 1
pd$pop.growth.rate.cow[pd$iso3c=="CZE"&pd$year==1993] <- cze.cow.growth.1992.1993 - 1
pd$pop.growth.rate.cow[pd$iso3c=="SVK"&pd$year==1993] <- svk.cow.growth.1992.1993 - 1

# Germany
pd$pop.growth.rate.un[pd$iso3c=="DEU"&pd$year==1990] <- deu.un.growth.1989.1990 - 1
pd$pop.growth.rate.cow[pd$iso3c=="DEU"&pd$year==1990] <- deu.cow.growth.1989.1990 - 1

# Ethiopia/Eritrea
pd$pop.growth.rate.un[pd$iso3c=="ETH"&pd$year==1993] <- eth.un.growth.1992.1993 - 1
pd$pop.growth.rate.un[pd$iso3c=="ERI"&pd$year==1993] <- eri.un.growth.1992.1993 - 1
pd$pop.growth.rate.cow[pd$iso3c=="ETH"&pd$year==1993] <- eth.cow.growth.1992.1993 - 1
pd$pop.growth.rate.cow[pd$iso3c=="ERI"&pd$year==1993] <- eri.cow.growth.1992.1993 - 1

# Malaysia/Singapore
pd$pop.growth.rate.un[pd$iso3c=="MYS"&pd$year==1963] <- mys.un.growth.1962.1963 - 1
pd$pop.growth.rate.cow[pd$iso3c=="MYS"&pd$year==1963] <- mys.cow.growth.1962.1963 - 1

pd$pop.growth.rate.un[pd$iso3c=="MYS"&pd$year==1965] <- mys.un.growth.1964.1965 - 1
pd$pop.growth.rate.un[pd$iso3c=="SGP"&pd$year==1965] <- sgp.un.growth.1964.1965 - 1
pd$pop.growth.rate.cow[pd$iso3c=="MYS"&pd$year==1965] <- mys.cow.growth.1964.1965 - 1
pd$pop.growth.rate.cow[pd$iso3c=="SGP"&pd$year==1965] <- sgp.cow.growth.1964.1965 - 1

# Pakistan/Bangladesh
pd$pop.growth.rate.un[pd$iso3c=="PAK"&pd$year==1971] <- pak.un.growth.1970.1971 - 1
pd$pop.growth.rate.un[pd$iso3c=="BGD"&pd$year==1971] <- bgd.un.growth.1970.1971 - 1
pd$pop.growth.rate.cow[pd$iso3c=="PAK"&pd$year==1971] <- pak.cow.growth.1970.1971 - 1
pd$pop.growth.rate.cow[pd$iso3c=="BGD"&pd$year==1971] <- bgd.cow.growth.1970.1971 - 1

# Sudan/South Sudan
pd$pop.growth.rate.un[pd$iso3c=="SDN"&pd$year==2011] <- sdn.un.growth.2010.2011 - 1
pd$pop.growth.rate.un[pd$iso3c=="SSD"&pd$year==2011] <- ssd.un.growth.2010.2011 - 1
pd$pop.growth.rate.cow[pd$iso3c=="SDN"&pd$year==2011] <- sdn.cow.growth.2010.2011 - 1
pd$pop.growth.rate.cow[pd$iso3c=="SSD"&pd$year==2011] <- ssd.cow.growth.2010.2011 - 1

# Soviet Union Successors
pd$pop.growth.rate.cow[pd$iso3c %in% c("EST","LVA","LTU","BLR","UKR","MDA","RUS","GEO",
                                       "ARM","AZE","KAZ","KGZ","TKM","TJK","UZB")&pd$year==1991] <- sov.successors.cow.growth.1990.1991 - 1

# Tanzania/Zanzibar
pd$pop.growth.rate.un[pd$iso3c=="TZA"&pd$year==1964] <- tza.un.growth.1963.1964 - 1
pd$pop.growth.rate.cow[pd$iso3c=="TZA"&pd$year==1964] <- tza.cow.growth.1963.1964 - 1

# Yemen
pd$pop.growth.rate.un[pd$iso3c=="YEM"&pd$year==1991] <- yem.un.growth.1990.1991 - 1
pd$pop.growth.rate.cow[pd$iso3c=="YEM"&pd$year==1991] <- yem.cow.growth.1990.1991 - 1

# Vietnam
pd$pop.growth.rate.un[pd$iso3c=="VNM"&pd$year==1976] <- vnm.un.growth.1975.1976 - 1
pd$pop.growth.rate.cow[pd$iso3c=="VNM"&pd$year==1976] <- vnm.cow.growth.1975.1976 - 1

# Yugoslavia Successors
pd$pop.growth.rate.cow[pd$iso3c %in% c("SVN","HRV","BIH","SRB","MKD")&pd$year==1992] <- yug.successors.cow.growth.1991.1992 - 1

# Serbia/Montenegro
pd$pop.growth.rate.cow[pd$iso3c=="SRB"&pd$year==2006] <- srb.cow.growth.2005.2006 - 1
pd$pop.growth.rate.cow[pd$iso3c=="MNE"&pd$year==2006] <- mne.cow.growth.2005.2006 - 1

# Serbia/Kosovo
pd$pop.growth.rate.cow[pd$iso3c=="SRB"&pd$year==2008] <- srb.cow.growth.2007.2008 - 1
pd$pop.growth.rate.cow[pd$iso3c=="KSV"&pd$year==2008] <- ksv.cow.growth.2007.2008 - 1

# South Africa/Namibia
pd$pop.growth.rate.un[pd$iso3c=="ZAF"&pd$year==1990] <- zaf.un.growth.1989.1990 - 1
pd$pop.growth.rate.cow[pd$iso3c=="ZAF"&pd$year==1990] <- zaf.cow.growth.1989.1990 - 1
pd$pop.growth.rate.un[pd$iso3c=="NAM"&pd$year==1990] <- nam.un.growth.1989.1990 - 1
pd$pop.growth.rate.cow[pd$iso3c=="NAM"&pd$year==1990] <- nam.cow.growth.1989.1990 - 1

### write data ----------------------------------------------------------------------
# writes formatted dataframe as csv files
write.csv(pd,"Data files/Formatted data files/population.csv",row.names = FALSE)

### codebook ----------------------------------------------------------------------
# iso3c
### A country's standardized iso3c code, with non-standard codes for West Germany, East Germany, North Yemen, South Yemen,
### South Vietnam, the Netherlands Antilles, the Soviet Union, and Yugoslavia.
# country
### A country's commonly used English-language name.
# year
### The calendar year the specific variable is measured during.
# un.pop
### A population estimate based on United Nations data.
# cow.pop
### A population estimate based on Correlates of War data.
# pop.growth.rate.un
### Annual population growth rates based on the estimates based on the United Nations data.
# pop.growth.rate.cow
### Annual population growth rates based on the estimates based on the Correlates of War data.
