
# This script creates two population estimation variables for all country-years based on data
# from the United Nations and Correlates of War. Missing data is estimated using the other dataset
# or from the Madison Project Dataset and Kristian Skrede Gleditsch. Annual population growth rates
# for both population variables are also created.


### Setup ------------------------------------------------------------------------------------------
#### Load Libraries --------------------------------------------------------------------------------
library(readxl)
library(countrycode)
library(tibble)
library(MTS)
library(KFAS)
library(imputeTS)
library(purrr)
library(dplyr)
library(tidyr)

#### Load Data Files -------------------------------------------------------------------------------

# population data from United Nations Department of Economic and Social Affairs, Population Division
# 1950 - 1984
pd1 <- readxl::read_xlsx("Data files/Raw data files/AnnualTotPopMidYear-20200708071736.xlsx",
                          sheet = 2, skip = 1)
# 1985 - 2019
pd2 <- readxl::read_xlsx("Data files/Raw data files/AnnualTotPopMidYear-20200708071835.xlsx",
                          sheet = 2, skip = 1)

# Correlates of War data file
cow.pop <- read.csv("Data files/Raw data files/NMC_5_0.csv")

# Maddison Project Dataset (MPD)
mpd <- readxl::read_xlsx("Data files/Raw data files/mpd2018.xlsx",
                         sheet = "Full data")

# Gleditsch
pop.gl <- utils::read.delim("Data files/Raw data files/gdpv6.txt")

#### Load Estimation Functions ---------------------------------------------------------------------

# source estimation functions
source("~/R/conflict-relapse/Functions/estimation_functions.R", echo = FALSE)


### Format Data ------------------------------------------------------------------------------------
#### UN Datasets -----------------------------------------------------------------------------------

# merge datasets
pd <- dplyr::full_join(pd1, pd2, by = c("ISO 3166-1 numeric code", "Location")) %>%
  
  # drop ISO 3166-1 code and data notes columns
  select(-c(`ISO 3166-1 numeric code`, Note.x, Note.y)) %>%
  
  dplyr::filter(
    # filter out non-countries (regions and economic groups) and the Holy See
    Location %!in% c(
      "World", "More developed regions", "Less developed regions", "Least developed countries",
      "Less developed regions, excluding least developed countries",
      "Less developed regions, excluding China", "High-income countries", "Middle-income countries",
      "Lower-middle-income countries", "Upper-middle-income countries", "Low-income countries",
      "Sub-Saharan Africa", "Africa", "Eastern Africa", "Middle Africa", "Northern Africa",
      "Southern Africa", "Western Africa", "Asia", "Eastern Asia", "Central Asia", "Southern Asia",
      "South-Eastern Asia", "Western Asia", "Europe", "Eastern Europe", "Northern Europe",
      "Southern Europe", "Western Europe", "Latin America and the Caribbean", "Caribbean",
      "Central America", "South America", "North America", "Oceania", "Australia/New Zealand",
      "Melanesia", "Polynesia", "South-Central Asia", "Northern America", "Micronesia", "Holy See"
      )) %>%
  
  # convert to long data
  tidyr::pivot_longer(2:71, names_to = "year", values_to = "un.pop") %>%
  
  # add country sub-units to the country's total population
  dplyr::mutate(
    Location = dplyr::case_when(
      Location == "Western Sahara" ~ "Morocco",
      Location %in% c("Greenland", "Faeroe Islands") ~ "Denmark",
      Location %in% c("Cook Islands", "Niue", "Tokelau") ~ "New Zealand",
      Location %in% c("Puerto Rico", "Guam", "American Samoa", "Northern Mariana Islands",
                      "United States Virgin Islands") ~ "United States of America",
      Location %in% c("China, Hong Kong SAR", "China, Macao SAR") ~ "China",
      Location %in% c("Aruba", "Sint Maarten (Dutch part)", "Curaçao",
                      "Caribbean Netherlands") ~ "Netherlands",
      Location %in% c("Saint Helena", "Isle of Man", "Gibraltar", "Cayman Islands", "Bermuda",
                      "British Virgin Islands", "Falkland Islands (Malvinas)", "Channel Islands",
                      "Turks and Caicos Islands", "Anguilla", "Montserrat") ~ "United Kingdom",
      Location %in% c("Mayotte", "Réunion", "French Guiana", "New Caledonia", "French Polynesia",
                      "Saint Pierre and Miquelon", "Guadeloupe", "Martinique",
                      "Wallis and Futuna Islands") ~ "France",
      
      .default = Location
    )) %>%
  
  dplyr::group_by(Location, year) %>%
  dplyr::summarise(un.pop = sum(un.pop)) %>%
  dplyr::ungroup() %>%
  
  # convert to full value
  dplyr::mutate(
    un.pop = 1000 * un.pop,
    year = as.numeric(year),
    
    # using the countrycode package, add iso3c based on country name
    iso3c = countrycode::countrycode(Location, "country.name", "iso3c")
    ) %>%
  
  # move iso3c variable first
  dplyr::relocate(iso3c, .before = Location) %>%
  dplyr::select(-Location)

#### COW dataset -----------------------------------------------------------------------------------

cow.pop <- cow.pop %>%
  as.data.frame() %>%
  dplyr::filter(year > 1945) %>%
  
  # convert to full value
  dplyr::mutate(
    tpop = 1000 * tpop,
    year = as.numeric(year),
    
    # using the countrycode package, add iso3c based on country COW abbreviation
    iso3c = dplyr::case_when(
      # manually add iso3c codes to missing stateabb codes:
      # CZE, GDR, GFR, KOS, RVN, YAR, YPR, YUG, ZAN
      stateabb == "CZE" ~ "CZE",
      stateabb == "GDR" ~ "DDR",
      stateabb == "GFR" ~ "BRD",
      stateabb == "KOS" ~ "KSV",
      stateabb == "RVN" ~ "RVN",
      stateabb == "YAR" ~ "YAR",
      stateabb == "YPR" ~ "YPR",
      stateabb == "YUG" ~ "YUG",
      stateabb == "ZAN" ~ "ZAN",
      .default = countrycode::countrycode(stateabb,"cowc","iso3c")
      )) %>%
  
  dplyr::select(iso3c, year, cow.pop = tpop)

#### Maddison Project Dataset (MPD) ----------------------------------------------------------------

# Codebook (available on sheet "Legend")
## countrycode: 3-letter ISO country code
## country: Country name
## year: Year
## cgdppc: Real GDP per capita in 2011US$, multiple benchmarks (suitable for cross-country income
### comparisons)
## rgdpnapc: Real GDP per capita in 2011US$, 2011 benchmark (suitable for cross-country growth
### comparisons)
## pop: Population, mid-year (thousands)
## i_cig:	0/1/2: observation is extrapolated (0), benchmark (1), or interpolated (2)
## i_bm: For benchmark observations: 1: ICP PPP estimates, 2: Historical income benchmarks, 3: Real
### wages and urbanization,
### 4: Multiple of subsistence, 5: Braithwaite (1968) PPPs

mpd <- mpd %>%
  dplyr::rename(
    iso3c = countrycode,
    mpd.pop = pop
    ) %>%

  dplyr::mutate(
    # convert population estimates to full number
    mpd.pop = mpd.pop * 1000,
    
    # add in Soviet Union iso3c code and merge Puerto Rico and Hong Kong with the United States
    # and China, respectively
    iso3c = dplyr::case_when(
      iso3c == "SUN" ~ "SOV",
      iso3c == "PRI" ~ "USA", # Puerto Rico -> United States of America
      iso3c == "HGK" ~ "CHN", # Hong Kong -> China
      .default = iso3c
    )) %>% 
  
  dplyr::group_by(iso3c, year) %>%
  dplyr::summarise(mpd.pop = sum(mpd.pop)) %>%
  dplyr::ungroup()

#### Gleditsch -------------------------------------------------------------------------------------

pop.gl <- pop.gl %>%
  
  dplyr::mutate(
    # calculate cgd independent of population
    cgd = pop * cgdppc,
    
    # using the countrycode package, add iso3c based on country name
    iso3c = dplyr::case_when(
      stateid == "AAB" ~ "ATG",
      stateid == "ABK" ~ "GEO", # Abkhazia -> Georgia
      stateid == "AND" ~ "AND",
      stateid == "CZE" ~ "CZE", # Czechoslovakia
      stateid == "DMA" ~ "DMA",
      stateid == "DRV" ~ "VNM", # North Vietnam and unified Vietnam
      stateid == "FSM" ~ "FSM",
      stateid == "GDR" ~ "DDR",
      stateid == "GRN" ~ "GRD",
      stateid == "KBI" ~ "KIR",
      stateid == "KOS" ~ "KSV",
      stateid == "LIE" ~ "LIE",
      stateid == "MNC" ~ "MCO",
      stateid == "MSI" ~ "MHL",
      stateid == "NAU" ~ "NRU",
      stateid == "PAL" ~ "PLW",
      stateid == "SEY" ~ "SYC",
      stateid == "SKN" ~ "KNA",
      stateid == "SLU" ~ "LCA",
      stateid == "SMN" ~ "SLB",
      stateid == "SOT" ~ "GEO", # South Ossetia -> Georgia
      stateid == "STP" ~ "STP",
      stateid == "SVG" ~ "VCT",
      stateid == "TBT" ~ "CHN", # Tibet -> China
      stateid == "TON" ~ "TON",
      stateid == "TUV" ~ "TUV",
      stateid == "VAN" ~ "VUT",
      stateid == "WSM" ~ "WSM",
      stateid == "YEM" ~ "YEM", # North Yemen and unified Yemen
      stateid == "YPR" ~ "YPR",
      stateid == "YUG" ~ "YUG",
      stateid == "ZAN" ~ "ZAN",
      stateid == "RVN" ~ "RVN",
      stateid == "SNM" ~ "SMR",
      .default = countrycode::countrycode(stateid, "gwc", "iso3c"))
  ) %>%
  
  dplyr::group_by(iso3c, year) %>%
  dplyr::summarise(across(c(pop, realgdp, cgd), \(x) sum(x, na.rm = TRUE))) %>%
  dplyr::ungroup() %>%
  
  dplyr::mutate(
    # recode values for Germany and Yemen pre-unification as West Germany and Yemen, respectively
    iso3c = dplyr::case_when(
      iso3c == "DEU" & year < 1991 ~ "BRD",
      iso3c == "YEM" & year < 1991 ~ "YAR",
      .default = iso3c
      ),
  
    gl.pop = pop * 1000) %>%
  
  dplyr::select(iso3c, year, gl.pop)


#### Merge Population Datasets ---------------------------------------------------------------------

pd <- list(pd, cow.pop, mpd, pop.gl) %>%
  purrr::reduce(dplyr::full_join, by = c("iso3c", "year")) %>%

  # create missing flags
  dplyr::mutate(
    un.pop.estimated = dplyr::case_when(
      is.na(un.pop) ~ 1,
      .default = 0
    ),
    cow.pop.estimated = dplyr::case_when(
      is.na(cow.pop) ~ 1,
      .default = 0
    ),
    un.pop.est.method = "N/A",
    cow.pop.est.method = "N/A"
    ) %>%
  
  # filter pre-1945 years (keeping 1945 (when present) to calculate growth rates)
  dplyr::filter(year >= 1945) %>%
  
  # fill in all country-year combinations 1946-2019
  tidyr::complete(iso3c, year = 1946:2019) %>%

  dplyr::arrange(iso3c, year)


### Calculate Estimates ----------------------------------------------------------------------------

# create a list object for which to store specified growth rate calculations
special_growth_rates <- list()

### Calculate Unified/Divided Country Data ---------------------------------------------------------
# YEM is combined population of YPR and YAR, DEU is combined DDR and BRD, VNM is combined with RVN
# SRB includes KSV, 6 YUG republics have component populations individually

# use COW populations to determine ratio of pop between YPR and YAR / DDR and BRD /
# VNM and RVN / SRB and KSV

#### AND: Andorra ----------------------------------------------------------------------------------
# UN: 1950-2019
# COW: 1993-2012

# COW 1950-1992: use growth chaining to estimate cow.pop using un.pop
pd <- growth_chaining_func(df = pd, iso = "AND", variable_missing = "cow.pop",
                           variable_complete = "un.pop", base_year_higher = 1993,
                           restricted = c(1950:1992))

#### AUT: Austria ----------------------------------------------------------------------------------
# UN: 1950-2019
# COW: 1955-2012

# COW 1950-1954: use bridge regression to estimate cow.pop using un.pop

# calculate 15-year correlation
ts_cor(pd, "AUT", "cow.pop", "un.pop", c(1955:1969))

pd <- bridge_regression_func(df = pd, iso = "AUT", variable_missing = "cow.pop",
                             variable_complete = "un.pop", restricted = c(1950:1954))

#### CZE/SVK: Czechoslovakia, Czechia, and Slovakia ------------------------------------------------
# UN: 1950-2019 (CZE) / 1950-2019 (SVK)
# COW: 1946-2012 / 1993-2012 (SVK)

# UN and MPD code CZE and SVK as separate Czechia and Slovakia throughout time series, while COW and
# Gleditsch code CZE as unified Czechoslovakia through 1992 (inclusive), with CZE being solely
# Czechia beginning in 1993 and Slovakia data starting in 1993

# Code Czechoslovakia through 1992 (inclusive); code Czechia and Slovakia as separate beginning in
# 1993 (inclusive)

# capture un growth rates
special_growth_rates[["CZE_UN_1992_1993"]] <- pd$un.pop[pd$iso3c == "CZE" & pd$year == 1993] /
  pd$un.pop[pd$iso3c == "CZE" & pd$year == 1992]
special_growth_rates[["SVK_UN_1992_1993"]] <- pd$un.pop[pd$iso3c == "SVK" & pd$year == 1993] /
  pd$un.pop[pd$iso3c == "SVK" & pd$year == 1992]

# 1950-1992: Combine UN pop CZE and SVK - do not mark as estimating the population
# recode SVK 1950-1992 as CZE, group by iso3c-year, and sum
pd <- pd %>%
  dplyr::mutate(
    # recode SVK to CZE for pre-independence years
    iso3c = ifelse(iso3c == "SVK" & year %in% c(1950:1992), "CZE", iso3c),
    # code un population estimation method for CZE and SVK for 1950-1992
    un.pop.est.method = ifelse(iso3c %in% c("CZE", "SVK") & year %in% c(1950:1992),
                               "Sum of CZE (Czechia) and SVK (Slovakia) population values",
                               un.pop.est.method)
    ) %>%
  dplyr::group_by(iso3c, year) %>%
  dplyr::summarise(
    # sum across grouped country-years for numeric variables
    un.pop = if(all(is.na(un.pop))) NA else sum(un.pop, na.rm = TRUE),
    cow.pop = if(all(is.na(cow.pop))) NA else sum(cow.pop, na.rm = TRUE),
    mpd.pop = if(all(is.na(mpd.pop))) NA else sum(mpd.pop, na.rm = TRUE),
    gl.pop = if(all(is.na(gl.pop))) NA else sum(gl.pop, na.rm = TRUE),
    # take the first unique value for non-numeric variables
    un.pop.estimated = unique(un.pop.estimated)[1],
    cow.pop.estimated = unique(cow.pop.estimated)[1],
    un.pop.est.method = unique(un.pop.est.method)[1],
    cow.pop.est.method = unique(cow.pop.est.method)[1],
    .groups = "drop"
  )

# capture COW growth rates (Czechia + Slovakia combined / Czechoslovakia)
special_growth_rates[["CZE_COW_1992_1993"]] <- sum(pd$cow.pop[pd$iso3c %in% c("CZE", "SVK") &
                                                                pd$year == 1993]) /
  pd$cow.pop[pd$iso3c == "CZE" & pd$year == 1992]
special_growth_rates[["SVK_COW_1992_1993"]] <- sum(pd$cow.pop[pd$iso3c %in% c("CZE", "SVK") &
                                                                pd$year == 1993]) /
  pd$cow.pop[pd$iso3c == "CZE" & pd$year == 1992]

#### DEU/BRD/DDR: Germany, East Germany, and West Germany ------------------------------------------
# UN and MPD code DEU as combined East and West Germany
# COW and Gleditsch code BRD (1955-1990) and DDR (1954-1990) as separate

# Germany coded as unified beginning in 1990 (inclusive)

# 1955-1989: calculate ratio between COW's BRD and DDR population estimates
# and apply to UN's DEU estimates
brd_ddr_pop_ratios <- pd %>%
  dplyr::filter(
    iso3c %in% c("BRD", "DDR"),
    # only have DDR data for 1954
    year %in% c(1955:1990)
    ) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(cow_pop_combined = sum(cow.pop, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(cow_pop_ratio = cow.pop / cow_pop_combined) %>%
  dplyr::select(iso3c, year, cow_pop_ratio)

# pull 1955 ratios for use later
pd_ratio_brd <- brd_ddr_pop_ratios$cow_pop_ratio[brd_ddr_pop_ratios$iso3c == "BRD" &
                                                   brd_ddr_pop_ratios$year == 1955]
pd_ratio_ddr <- brd_ddr_pop_ratios$cow_pop_ratio[brd_ddr_pop_ratios$iso3c == "DDR" &
                                                   brd_ddr_pop_ratios$year == 1955]

# merge in UN data and calculate estimates
brd_ddr_pop_ests <- brd_ddr_pop_ratios %>%
  # merge in UN's DEU population estimates
  dplyr::left_join(pd %>%
                     dplyr::filter(iso3c == "DEU") %>%
                     dplyr::select(year, un.pop),
                   by = "year") %>%
  # calculate UN estimate
  dplyr::mutate(un_est = un.pop * cow_pop_ratio) %>%
  dplyr::select(iso3c, year, un_est)

# merge estimates into main df
pd <- pd %>%
  dplyr::left_join(brd_ddr_pop_ests, by = c("iso3c", "year")) %>%
  dplyr::mutate(
    un.pop = ifelse(iso3c %in% c("BRD", "DDR") & year %in% c(1955:1990), un_est, un.pop),
    # recode as estimated values
    un.pop.estimated = ifelse(iso3c %in% c("BRD", "DDR") & year %in% c(1955:1990), 1, un.pop.estimated),
    # add estimation methodology
    un.pop.est.method = ifelse(iso3c %in% c("BRD", "DDR") & year %in% c(1955:1990),
                               "UN estimate with COW East/West Germany population proportion estimates",
                               un.pop.est.method)
  ) %>%
  dplyr::select(-un_est)

# above methodology matches Gleditsch estimates - apply 1950-1954 (BRD) / 1950-1953 (DDR) Gleditsch
# values to cow.pop
pd <- pd %>%
  dplyr::mutate(
    cow.pop = ifelse(iso3c %in% c("BRD", "DDR") & year %in% c(1950:1955) & is.na(cow.pop),
                     gl.pop, cow.pop),
    # add estimation methodology
    cow.pop.est.method = ifelse(iso3c %in% c("BRD", "DDR") & year %in% c(1950:1955) &
                                  cow.pop.est.method == "N/A",
                                "Extension of East/West Germany methodology using Gleditsch",
                               cow.pop.est.method)
  )

# Estimate 1950-1954 BRD and DDR UN populations
pd <- bridge_regression_func(df = pd, iso = "BRD", variable_missing = "un.pop",
                             variable_complete = "cow.pop", restricted = c(1950:1954))
pd <- bridge_regression_func(df = pd, iso = "DDR", variable_missing = "un.pop",
                             variable_complete = "cow.pop", restricted = c(1950:1954))

# 1945-1949: apply MPD's (combined) DEU growth rates to un.pop and cow.pop BRD and DDR estimates

# pull DEU 1950 estimates
un_brd_1950_pop <- pd$un.pop[pd$iso3c == "BRD" & pd$year == 1950]
un_ddr_1950_pop <- pd$un.pop[pd$iso3c == "DDR" & pd$year == 1950]

cow_brd_1950_pop <- pd$cow.pop[pd$iso3c == "BRD" & pd$year == 1950]
cow_ddr_1950_pop <- pd$cow.pop[pd$iso3c == "DDR" & pd$year == 1950]

mpd_deu_1950_pop <- pd$mpd.pop[pd$iso3c == "DEU" & pd$year == 1950]

# calculate MPD DEU population growth relative to 1950
mpd_deu_growth <- pd %>%
  dplyr::filter(iso3c == "DEU") %>%
  dplyr::arrange(year) %>%
  dplyr::mutate(mpd_ratio = mpd.pop / mpd_deu_1950_pop) %>%
  dplyr::select(year, mpd_ratio) %>%
  # add in BRD and DDR iso3c codes to facilitate merging
  tidyr::crossing(iso3c = c("BRD", "DDR"))

# merge data into df
pd <- pd %>%
  dplyr::left_join(mpd_deu_growth, by = c("iso3c", "year")) %>%
  dplyr::mutate(
    # calculate estimates
    un.pop = ifelse(iso3c == "BRD" & year %in% c(1945:1949),
                    un_brd_1950_pop * mpd_ratio, un.pop),
    un.pop = ifelse(iso3c == "DDR" & year %in% c(1945:1949),
                    un_ddr_1950_pop * mpd_ratio, un.pop),
    
    cow.pop = ifelse(iso3c == "BRD" & year %in% c(1945:1949),
                     cow_brd_1950_pop * mpd_ratio, cow.pop),
    cow.pop = ifelse(iso3c == "DDR" & year %in% c(1945:1949),
                     cow_ddr_1950_pop * mpd_ratio, cow.pop),
    
    # add estimation methodology
    un.pop.est.method = ifelse(iso3c %in% c("BRD", "DDR") & year %in% c(1945:1949),
                               "Growth Chaining with mpd.pop from 1950",
                               un.pop.est.method),
    cow.pop.est.method = ifelse(iso3c %in% c("BRD", "DDR") & year %in% c(1945:1949),
                                "Growth Chaining with mpd.pop from 1950",
                                cow.pop.est.method)
  ) %>%
  dplyr::select(-mpd_ratio)

# capture growth rates
special_growth_rates[["DEU_UN_1989_1990"]] <- pd$un.pop[pd$iso3c == "DEU" & pd$year == 1990] /
  sum(pd$un.pop[pd$iso3c %in% c("BRD", "DDR") & pd$year == 1989])
special_growth_rates[["DEU_COW_1989_1990"]] <- pd$cow.pop[pd$iso3c == "DEU" & pd$year == 1990] /
  sum(pd$un.pop[pd$iso3c %in% c("BRD", "DDR") & pd$year == 1989])

#### ETH/ERI: Ethiopia/Eritrea ---------------------------------------------------------------------
# COW codes ETH as ETH+ERI through 1992 (inclusive), with no ERI estimates before 1993
# UN codes ETH without ERI throughout

# ERI coded as independent beginning in 1993

# capture UN growth rates
special_growth_rates[["ERI_UN_1992_1993"]] <- pd$un.pop[pd$iso3c == "ERI" & pd$year == 1993] /
  pd$un.pop[pd$iso3c == "ERI" & pd$year == 1992]
special_growth_rates[["ETH_UN_1992_1993"]] <- pd$un.pop[pd$iso3c == "ETH" & pd$year == 1993] /
  pd$un.pop[pd$iso3c == "ETH" & pd$year == 1992]

# estimate 1992 ERI and ETH (excluding ERI) populations to capture growth rates estimate proportion
# of ERI and ETH 1993 populations
eri.1993.prop <- pd$cow.pop[pd$iso3c == "ERI" & pd$year == 1993] /
  sum(pd$cow.pop[pd$iso3c %in% c("ETH", "ERI") & pd$year == 1993])
eth.1993.prop <- pd$cow.pop[pd$iso3c == "ETH" & pd$year == 1993] /
  sum(pd$cow.pop[pd$iso3c %in% c("ETH", "ERI") & pd$year == 1993])

# apply 1993 proportions to COW's ETH (including ERI) 1992 population estimate
eri.1992.est <- eri.1993.prop * pd$cow.pop[pd$iso3c == "ETH" & pd$year == 1992]
eth.1992.est <- eth.1993.prop * pd$cow.pop[pd$iso3c == "ETH" & pd$year == 1992]

# calculate COW growth estimates
eri.1992.1993.growth <- 100 * (pd$cow.pop[pd$iso3c == "ERI" & pd$year == 1993] - eri.1992.est) /
                          eri.1992.est
eth.1992.1993.growth <- 100 * (pd$cow.pop[pd$iso3c == "ETH" & pd$year == 1993] - eth.1992.est) /
                          eth.1992.est

# 1950-1992: Combine UN pop ERI and ETH - do not mark as estimating the population
# recode ERI 1950-1992 as ETH, group by iso3c-year, and sum
pd <- pd %>%
  dplyr::mutate(
    # recode ERI to ETH for pre-independence years
    iso3c = ifelse(iso3c == "ERI" & year %in% c(1950:1992), "ETH", iso3c),
    # code un population estimation method for ERI and ETH for 1950-1992
    un.pop.est.method = ifelse(iso3c %in% c("ERI", "ETH") & year %in% c(1950:1992),
                               "Sum of ERI and ETH population values",
                               un.pop.est.method)
  ) %>%
  dplyr::group_by(iso3c, year) %>%
  dplyr::summarise(
    # sum across grouped country-years for numeric variables
    un.pop = if(all(is.na(un.pop))) NA else sum(un.pop, na.rm = TRUE),
    cow.pop = if(all(is.na(cow.pop))) NA else sum(cow.pop, na.rm = TRUE),
    mpd.pop = if(all(is.na(mpd.pop))) NA else sum(mpd.pop, na.rm = TRUE),
    gl.pop = if(all(is.na(gl.pop))) NA else sum(gl.pop, na.rm = TRUE),
    # take the first unique value for non-numeric variables
    un.pop.estimated = unique(un.pop.estimated)[1],
    cow.pop.estimated = unique(cow.pop.estimated)[1],
    un.pop.est.method = unique(un.pop.est.method)[1],
    cow.pop.est.method = unique(cow.pop.est.method)[1],
    .groups = "drop"
  )

# capture COW growth rates
special_growth_rates[["ERI_COW_1992_1993"]] <- sum(pd$cow.pop[pd$iso3c %in% c("ETH", "ERI") &
                                                                pd$year == 1993]) /
  pd$cow.pop[pd$iso3c == "ETH" & pd$year == 1992]
special_growth_rates[["ETH_COW_1992_1993"]] <- sum(pd$cow.pop[pd$iso3c %in% c("ETH", "ERI") &
                                                                pd$year == 1993]) /
  pd$cow.pop[pd$iso3c == "ETH" & pd$year == 1992]

#### ISR/PSE: Israel/Palestine ---------------------------------------------------------------------
# UN codes ISR and PSE separately 1950-2019
# COW only codes ISR, no data for PSE and not included in ISR

# 1950-2012 COW PSE: Calculate UN's PSE proportion of ISR population and apply to COW's ISR
# population
pse.pop.prop <- pd %>%
  dplyr::filter(
    iso3c %in% c("PSE","ISR"),
    # only years with both UN and COW data
    year %in% c(1950:2019)
    ) %>%
  #tidyr::pivot_wider(names_from = iso3c, values_from = cow.pop)
  dplyr::group_by(year) %>%
  dplyr::mutate(
    isp.pop = sum(un.pop, na.rm = TRUE),
    cow.pop = sum(cow.pop, na.rm = TRUE),
    mpd.pop = sum(mpd.pop, na.rm = TRUE),
    gl.pop = sum(gl.pop, na.rm = TRUE),
    un.pop.estimated = max(un.pop.estimated, na.rm = TRUE),
    cow.pop.estimated = max(cow.pop.estimated, na.rm = TRUE),
    un.pop.est.method = NA,
    cow.pop.est.method = NA
    ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    cow.pop = ifelse(iso3c=="PSE",
                     ((un.pop / isp.pop) * cow.pop / (1 - (un.pop / isp.pop))),
                     cow.pop),
    # replace 0s generated by mutating to NAs
    cow.pop = ifelse(cow.pop == 0, NA, cow.pop)
    ) %>%
  # ISR estimates not needed for additional PSE estimating
  dplyr::filter(iso3c != "ISR") %>%
  dplyr::select(-isp.pop)

# estimate UN PSE 1948-1949 population by using a weighted 3-year growth rate

# un
un.pse.1950.1951.growth <- pse.pop.prop$un.pop[pse.pop.prop$iso3c == "PSE" & pse.pop.prop$year == 1951] /
                              pse.pop.prop$un.pop[pse.pop.prop$iso3c == "PSE" & pse.pop.prop$year == 1950]
un.pse.1951.1952.growth <- pse.pop.prop$un.pop[pse.pop.prop$iso3c == "PSE" & pse.pop.prop$year == 1952] /
                              pse.pop.prop$un.pop[pse.pop.prop$iso3c == "PSE" & pse.pop.prop$year == 1951]
un.pse.1952.1953.growth <- pse.pop.prop$un.pop[pse.pop.prop$iso3c == "PSE" & pse.pop.prop$year == 1953] /
                              pse.pop.prop$un.pop[pse.pop.prop$iso3c == "PSE" & pse.pop.prop$year == 1952]

un.pse.1949.1950.growth <- (1/2) * un.pse.1950.1951.growth +
                             (1/3) * un.pse.1951.1952.growth +
                             (1/6) * un.pse.1952.1953.growth
un.pse.1948.1949.growth <- (1/2) * un.pse.1949.1950.growth +
                             (1/3) * un.pse.1950.1951.growth +
                             (1/6) * un.pse.1951.1952.growth

# cow
cow.pse.1950.1951.growth <- pse.pop.prop$cow.pop[pse.pop.prop$iso3c == "PSE" & pse.pop.prop$year == 1951] /
                               pse.pop.prop$cow.pop[pse.pop.prop$iso3c == "PSE" & pse.pop.prop$year == 1950]
cow.pse.1951.1952.growth <- pse.pop.prop$cow.pop[pse.pop.prop$iso3c == "PSE" & pse.pop.prop$year == 1952] /
                               pse.pop.prop$cow.pop[pse.pop.prop$iso3c == "PSE" & pse.pop.prop$year == 1951]
cow.pse.1952.1953.growth <- pse.pop.prop$cow.pop[pse.pop.prop$iso3c == "PSE" & pse.pop.prop$year == 1953] /
                               pse.pop.prop$cow.pop[pse.pop.prop$iso3c == "PSE" & pse.pop.prop$year == 1952]

cow.pse.1949.1950.growth <- (1/2) * cow.pse.1950.1951.growth +
                              (1/3) * cow.pse.1951.1952.growth +
                              (1/6) * cow.pse.1952.1953.growth
cow.pse.1948.1949.growth <- (1/2) * cow.pse.1949.1950.growth +
                              (1/3) * cow.pse.1950.1951.growth +
                              (1/6) * cow.pse.1951.1952.growth

# calculate 1949 estimates
pse.un.est.1949 <- pse.pop.prop$un.pop[pse.pop.prop$iso3c == "PSE" & pse.pop.prop$year == 1950] *
                    un.pse.1949.1950.growth
pse.cow.est.1949 <- pse.pop.prop$cow.pop[pse.pop.prop$iso3c == "PSE" & pse.pop.prop$year == 1950] *
                      cow.pse.1949.1950.growth

# calculate 1948 estimates
pse.un.est.1948 <- pse.pop.prop$un.pop[pse.pop.prop$iso3c == "PSE" & pse.pop.prop$year == 1949] *
                     un.pse.1948.1949.growth
pse.cow.est.1948 <- pse.pop.prop$cow.pop[pse.pop.prop$iso3c == "PSE" & pse.pop.prop$year == 1949] *
                      cow.pse.1948.1949.growth

# add estimates to PSE dataset
pse.pop.prop <- pse.pop.prop %>%
  rbind(data.frame(
    iso3c = c("PSE", "PSE"),
    year = c(1949, 1948),
    un.pop = c(pse.un.est.1949, pse.un.est.1948),
    cow.pop = c(pse.cow.est.1949, pse.cow.est.1948),
    mpd.pop = NA,
    gl.pop = NA,
    un.pop.estimated = c(1, 1),
    cow.pop.estimated = c(1, 1),
    un.pop.est.method = NA,
    cow.pop.est.method = NA
  ))

# 2013-2019
pse.pop.prop <- bridge_regression_func(df = pse.pop.prop, iso = "PSE", variable_missing = "cow.pop",
                                       variable_complete = "un.pop", restricted = c(2013:2019))

# estimate UN ISR 1948-1949
pd <- bridge_regression_func(df = pd, iso = "ISR", variable_missing = "un.pop",
                             variable_complete = "cow.pop", restricted = c(1948:1949))

# estimate COW ISR 2013-2019
pd <- bridge_regression_func(df = pd, iso = "ISR", variable_missing = "cow.pop",
                             variable_complete = "un.pop", restricted = c(2013:2019))

# add PSE estimates to ISR values
for(i in c(1948:2019)){
  
  pd$un.pop[pd$iso3c == "ISR" & pd$year == i] <- pd$un.pop[pd$iso3c == "ISR" & pd$year == i] +
                                                   pse.pop.prop$un.pop[pse.pop.prop$iso3c == "PSE" &
                                                                         pse.pop.prop$year == i]
  
  pd$cow.pop[pd$iso3c == "ISR" & pd$year == i] <- pd$cow.pop[pd$iso3c == "ISR" & pd$year == i] +
                                                    pse.pop.prop$cow.pop[pse.pop.prop$iso3c == "PSE" &
                                                                           pse.pop.prop$year == i]
  
}

#### JPN: Japan ------------------------------------------------------------------------------------
# 1945-1951: use bridge regression to estimate cow.pop using mpd.pop

# calculate 15-year correlation
ts_cor(pd, "JPN", "cow.pop", "mpd.pop", c(1952:1966))

pd <- bridge_regression_func(df = pd, iso = "JPN", variable_missing = "cow.pop",
                              variable_complete = "mpd.pop", restricted = c(1945:1951))

# 1945-1949: use bridge regression to estimate un.pop using mpd.pop

# calculate 15-year correlation
ts_cor(pd, "JPN", "un.pop", "mpd.pop", c(1950:1964))

pd <- bridge_regression_func(df = pd, iso = "JPN", variable_missing = "un.pop",
                              variable_complete = "mpd.pop", restricted = c(1945:1949))

#### KOR: South Korea ------------------------------------------------------------------------------
# 1945-1949: use bridge regression to estimate un.pop using mpd.pop

# calculate 15-year correlation
ts_cor(pd, "KOR", "un.pop", "mpd.pop", c(1950:1964))

pd <- bridge_regression_func(df = pd, iso = "KOR", variable_missing = "un.pop",
                             variable_complete = "mpd.pop", restricted = c(1945:1949))

# 1945-1948: use bridge regression to estimate cow.pop using mpd.pop

# calculate 15-year correlation
ts_cor(pd, "KOR", "cow.pop", "mpd.pop", c(1949:1963))

pd <- bridge_regression_func(df = pd, iso = "KOR", variable_missing = "cow.pop",
                              variable_complete = "mpd.pop", restricted = c(1945:1948))

#### LIE: Liechtenstein ----------------------------------------------------------------------------
# UN has data from 1950-2019; COW data starts in 1990

# 1950-1989: use bridge regression to estimate cow.pop using un.pop

# calculate 15-year correlation
ts_cor(pd, "LIE", "cow.pop", "un.pop", c(1990:2004))

pd <- bridge_regression_func(df = pd, iso = "LIE", variable_missing = "cow.pop",
                             variable_complete = "un.pop", restricted = c(1950:1989))

#### MCO: Monaco -----------------------------------------------------------------------------------
# UN has data from 1950-2019; COW data starts in 1993

# 1950-1992: use bridge regression to estimate cow.pop using un.pop

# calculate 15-year correlation
ts_cor(pd, "MCO", "cow.pop", "un.pop", c(1993:2007))

pd2 <- bridge_regression_func(df = pd, iso = "MCO", variable_missing = "cow.pop",
                              variable_complete = "un.pop", restricted = c(1950:1992))

#### MYS/SGP: Malaysia/Singapore -------------------------------------------------------------------
# UN/MPD/Gleditsch: MYS and SGP coded separately 1950-2019
# COW: 1957-1964 MYS coded as MYS+SGP, MYS 1965- is solely Malaysia; SGP coded starting 1965

# MYS coded as gaining independence in 1957
# SGP coded as gaining independence in 1965
# SGP part of MYS 1963-1964

# capture UN growth rates
special_growth_rates[["MYS_UN_1962_1963"]] <- sum(pd$un.pop[pd$iso3c %in% c("MYS", "SGP") & pd$year == 1963]) /
                                                sum(pd$un.pop[pd$iso3c %in% c("MYS", "SGP") & pd$year == 1962])
special_growth_rates[["MYS_UN_1964_1965"]] <- pd$un.pop[pd$iso3c == "MYS" & pd$year == 1965] /
                                                pd$un.pop[pd$iso3c == "MYS" & pd$year == 1964]
special_growth_rates[["SGP_UN_1964_1965"]] <- pd$un.pop[pd$iso3c == "SGP" & pd$year == 1965] /
                                                pd$un.pop[pd$iso3c == "SGP" & pd$year == 1964]

# 1957-1964: use UN proportions to separate COW's MYS/SGP estimates
pd.mys.sgp <- pd %>%
  dplyr::filter(
    iso3c %in% c("MYS", "SGP"),
    year %in% c(1957:1964)
    ) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(
    cow.pop = sum(cow.pop, na.rm = TRUE),
    un.mys.sgp = sum(un.pop, na.rm = TRUE)
    ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(cow.est = cow.pop * (un.pop / un.mys.sgp)) %>%
  dplyr::select(-c(un.mys.sgp, un.pop, cow.pop, mpd.pop, gl.pop, un.pop.estimated,
                   cow.pop.estimated, un.pop.est.method, cow.pop.est.method))

# add new COW estimates to main dataset
pd <- pd %>%
  dplyr::left_join(pd.mys.sgp, by = c("iso3c", "year")) %>%
  dplyr::mutate(cow.pop = ifelse(!is.na(cow.est), cow.est, cow.pop)) %>%
  dplyr::select(-cow.est)

# pull MYS (excluding SGP) 1962-1963 and 1964-1965 and growth rates for use later
special_growth_rates[["MYS_UN_1962_1963"]] <- pd$un.pop[pd$iso3c == "MYS" & pd$year == 1963] /
                            pd$un.pop[pd$iso3c == "MYS" & pd$year == 1962]
special_growth_rates[["MYS_COW_1962_1963"]] <- pd$cow.pop[pd$iso3c == "MYS" & pd$year == 1963] /
                              pd$cow.pop[pd$iso3c == "MYS" & pd$year == 1962]
special_growth_rates[["MYS_UN_1964_1965"]] <- pd$un.pop[pd$iso3c == "MYS" & pd$year == 1965] /
                            pd$un.pop[pd$iso3c == "MYS" & pd$year == 1964]
special_growth_rates[["MYS_COW_1964_1965"]] <- pd$cow.pop[pd$iso3c == "MYS" & pd$year == 1965] /
                              pd$cow.pop[pd$iso3c == "MYS" & pd$year == 1964]

# add SGP to MYS for 1963-1964
for(s in 1963:1964){
  
  pd$un.pop[pd$iso3c == "MYS" & pd$year == s] <- sum(pd$un.pop[pd$iso3c %in% c("MYS", "SGP") &
                                                                 pd$year == s])
  pd$cow.pop[pd$iso3c=="MYS"&pd$year==s] <- sum(pd$un.pop[pd$iso3c %in% c("MYS", "SGP") &
                                                            pd$year == s])
  
}

# capture COW growth rates
special_growth_rates[["MYS_COW_1962_1963"]] <- pd$cow.pop[pd$iso3c == "MYS" & pd$year == 1963] /
                               sum(pd$cow.pop[pd$iso3c %in% c("MYS", "SGP") & pd$year == 1962])
special_growth_rates[["MYS_COW_1964_1965"]] <- sum(pd$cow.pop[pd$iso3c %in% c("MYS", "SGP") & pd$year == 1965]) /
                               pd$cow.pop[pd$iso3c == "MYS" & pd$year == 1964]
special_growth_rates[["SGP_COW_1964_1965"]] <- sum(pd$cow.pop[pd$iso3c %in% c("MYS", "SGP") & pd$year == 1965]) /
                               pd$cow.pop[pd$iso3c == "MYS" & pd$year == 1964]

#### OMN: Oman -------------------------------------------------------------------------------------
# 1950-1970: use bridge regression to estimate cow.pop using un.pop

# calculate 15-year correlation
ts_cor(pd, "OMN", "cow.pop", "un.pop", c(1971:1985))

pd <- bridge_regression_func(df = pd, iso = "OMN", variable_missing = "cow.pop",
                             variable_complete = "un.pop", restricted = c(1950:1970))

#### PAK/BGD: Pakistan/Bangladesh ------------------------------------------------------------------
# UN codes PAK and BGD as separate; COW codes PAK as PAK+BGD through 1970 (inclusive)

# pull 1970-1971 PAK (excluding BGD) and BGD growth rates for use later
# use PAK+BGD 1970-1971 COW growth for PAK and BGD
special_growth_rates[["PAK_UN_1970_1971"]] <- pd$un.pop[pd$iso3c == "PAK" & pd$year == 1971] /
                                                pd$un.pop[pd$iso3c == "PAK" & pd$year == 1970]
special_growth_rates[["BGD_UN_1970_1971"]] <- pd$un.pop[pd$iso3c == "BGD" & pd$year == 1971] /
                                                pd$un.pop[pd$iso3c == "BGD" & pd$year == 1970]
special_growth_rates[["PAK_COW_1970_1971"]] <- sum(pd$cow.pop[pd$iso3c %in% c("PAK", "BGD") & pd$year == 1971]) /
                                                 pd$cow.pop[pd$iso3c == "PAK" & pd$year == 1970]
special_growth_rates[["BGD_COW_1970_1971"]] <- sum(pd$cow.pop[pd$iso3c %in% c("PAK", "BGD") & pd$year == 1971]) /
                                                 pd$cow.pop[pd$iso3c == "PAK" & pd$year == 1970]

# combine UN BGD 1950-1970 populations into PAK
for(p in 1950:1970){
  
  pd$un.pop[pd$iso3c == "PAK" & pd$year == p] <- sum(pd$un.pop[pd$iso3c %in% c("PAK", "BGD") &
                                                                 pd$year == p])
  
}

#### PRK: North Korea ------------------------------------------------------------------------------

# 1948-1949: use bridge regression to estimate un.pop using cow.pop

# calculate 15-year correlation
ts_cor(pd, "PRK", "un.pop", "cow.pop", c(1950:1964))

pd <- bridge_regression_func(df = pd, iso = "PRK", variable_missing = "un.pop",
                             variable_complete = "cow.pop", restricted = c(1948:1949))

#### SDN/SSD: Sudan/South Sudan --------------------------------------------------------------------
# UN codes SDN as SDN (excluding SSD) throughout the time series
# COW appears to code SDN as SDN (excluding SSD) throughout the time series
# UN has full SSD time series data, while COW only starts in 2011

# pull SDN 2010-2011 (excluding SSD) and SSD growth rates for use later (use SSD UN growth rate for
# COW)
special_growth_rates[["SDN_UN_2010_2011"]] <- pd$un.pop[pd$iso3c == "SDN" & pd$year == 2011] /
                                                pd$un.pop[pd$iso3c == "SDN" & pd$year == 2010]
special_growth_rates[["SDN_COW_2010_2011"]] <- pd$cow.pop[pd$iso3c == "SDN" & pd$year == 2011] /
                                                 pd$cow.pop[pd$iso3c == "SDN" & pd$year == 2010]
special_growth_rates[["SSD_UN_2010_2011"]] <- pd$un.pop[pd$iso3c == "SSD" & pd$year == 2011] /
                                               pd$un.pop[pd$iso3c == "SSD" & pd$year == 2010]
special_growth_rates[["SSD_COW_2010_2011"]] <- pd$un.pop[pd$iso3c == "SSD" & pd$year == 2011] /
                                                pd$un.pop[pd$iso3c == "SSD" & pd$year == 2010]

# 1950-2010: apply UN SSD growth rates to COW's 2011 population estimate
pd <- growth_chaining_func(df = pd, iso = "SSD", variable_missing = "cow.pop",
                           variable_complete = "un.pop", base_year_higher = 2011,
                           restricted = c(1950:2010))

# 1950-2010: add SSD estimates to SDN estimates
for(s in 1950:2010){
  
  pd$un.pop[pd$iso3c == "SDN" & pd$year == s] <- sum(pd$un.pop[pd$iso3c %in% c("SDN", "SSD") &
                                                                 pd$year == s])
  pd$cow.pop[pd$iso3c=="SDN"&pd$year==s] <- sum(pd$cow.pop[pd$iso3c %in% c("SDN", "SSD") &
                                                             pd$year == s])

}

#### SMR: San Marino -------------------------------------------------------------------------------
# UN has data from 1950-2019; COW data starts in 1992

# 1950-1991: use bridge regression to estimate cow.pop using un.pop

# calculate 15-year correlation
ts_cor(pd, "SMR", "cow.pop", "un.pop", c(1992:2006))

pd <- bridge_regression_func(df = pd, iso = "SMR", variable_missing = "cow.pop",
                             variable_complete = "un.pop", restricted = c(1950:1991))

#### SOV/RUS: Soviet Union/Russia ------------------------------------------------------------------
# UN lists each of the 15 SSR populations separately, while COW codes SOV's population
# as RUS through 1990 (inclusive)

# merge UN SSR estimates into one SOV estimate through 1990 (preserving each SSR's estimates)
pd.sov <- pd %>%
  # select the 15 USSR successor states
  dplyr::filter(iso3c %in% c("EST", "LVA", "LTU", "MDA", "BLR", "UKR", "RUS", "GEO", "ARM", "AZE",
                             "KAZ", "KGZ", "TJK", "TKM", "UZB")) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(
    un.pop = sum(un.pop, na.rm = TRUE),
    cow.pop = sum(cow.pop, na.rm = TRUE)
    ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(year < 1991) %>%
  dplyr::mutate(
    iso3c = "SOV",
    # converts 0s created when summarising data into NAs
    un.pop = ifelse(un.pop == 0, NA, un.pop))

for(s in c(1946:1990)){
  
  pd$un.pop[pd$iso3c == "SOV" & pd$year == s] <- pd.sov$un.pop[pd.sov$year == s]
  pd$cow.pop[pd$iso3c == "SOV" & pd$year == s] <- pd.sov$cow.pop[pd.sov$year == s]
  
}

# # add SOV estimates to main dataset
# pd <- pd %>%
#   dplyr::filter(iso3c != "SOV") %>%
#   rbind(pd.sov)

# remove cow's SOV estimates coded as RUS
pd$cow.pop[pd$iso3c == "RUS" & pd$year < 1991] <- NA

# estimate COW growth rates for successor states
special_growth_rates[["SOV_COW_1990_1991"]] <- sum(pd$cow.pop[pd$iso3c %in% c(
  "EST", "LVA", "LTU", "BLR", "UKR", "MDA", "RUS", "GEO", "ARM", "AZE", "KAZ", "KGZ", "TKM", "TJK",
  "UZB") & pd$year == 1991]) / pd$cow.pop[pd$iso3c == "SOV" & pd$year == 1990]

# remove RUS 1946-1949 entries
pd <- pd %>%
  dplyr::filter(iso3c != "RUS" | year %!in% c(1946:1949))

#### SRB/MNE/KSV: Serbia, Montenegro, and Kosovo ---------------------------------------------------
# BIH: UN estimates 1950-2019; COW 1992-2012
# HRV: UN estimates 1950-2019; COW 1992-2012
# MKD: UN estimates 1950-2019; COW 1993-2012
# SVN: UN estimates 1950-2019; COW 1992-2012
# SRB: UN estimates 1950-2019, containing Serbia and Kosovo, but no Montenegro; COW no estimates
# MNE: UN estimates 1950-2019; COW 2006-2012
# KSV: UN no estimates; COW 2008-2012
# YUG: UN no estimates; COW 1946-1991 SFR Yugoslavia estimates, 1992-2012 contains Serbia,
## Montenegro, and Kosovo

# COW: For YUG (SRB), subtract MNE (starting in 2006) and KSV (starting in 2008)
# UN: For SRB, subtract COW's KSV (starting in 2008)

# subtract MNE from COW's estimate
for(s in 2006:2007){
  
  pd$cow.pop[pd$iso3c == "YUG" & pd$year == s] <- pd$cow.pop[pd$iso3c == "YUG" & pd$year == s] -
                                                    pd$cow.pop[pd$iso3c == "MNE" & pd$year == s]
  
}

# subtract KSV and MNE from COW's estimate and KSV from UN's estimate
for(s in 2008:2012){
  
  pd$cow.pop[pd$iso3c == "YUG" & pd$year == s] <- pd$cow.pop[pd$iso3c == "YUG" & pd$year == s] -
                                                    sum(pd$cow.pop[pd$iso3c %in% c("MNE", "KSV") &
                                                                     pd$year == s])
  
  pd$un.pop[pd$iso3c == "SRB" & pd$year == s] <- pd$un.pop[pd$iso3c == "SRB" & pd$year == s] -
                                                  pd$cow.pop[pd$iso3c == "KSV" & pd$year == s]
  
}

# recode YUG 1992-2012 as SRB
for(s in 1992:2012){
  
  pd$cow.pop[pd$iso3c == "SRB" & pd$year == s] <- pd$cow.pop[pd$iso3c == "YUG" & pd$year == s]
  
}

# UN SRB/KSV 2013-2019: apply a three-year weighted growth rate to SRB and KSV (independently), then
# calculate the annual alpha ratio between the estimated SRB+KSV sum and the UN estimate for a
# combined SRB+KSV, then scale the SRB and KSV estimates annually by this alpha

# create blank dataframe to hold values calculated in the following for loop
pd.srb.ksv <- data.frame(year = c(2009:2019),
                         ksv.growth = NA,
                         srb.growth = NA,
                         ksv.est = NA,
                         srb.est = NA)

# add KSV and SRB 2009-2012 values to the table
for(b in 2009:2012){
  
  pd.srb.ksv$ksv.est[pd.srb.ksv$year == b] <- pd$cow.pop[pd$iso3c == "KSV" & pd$year == b]
  pd.srb.ksv$srb.est[pd.srb.ksv$year == b] <- pd$un.pop[pd$iso3c == "SRB" & pd$year == b &
                                                          !is.na(pd$un.pop)]
  
}

for(y in 2013:2019){
  
  # calculate growth estimates
  srb.un.growth <- (1/2) * (pd.srb.ksv$srb.est[pd.srb.ksv$year == (y - 1)] /
                              pd.srb.ksv$srb.est[pd.srb.ksv$year == (y - 2)]) +
                   (1/3) * (pd.srb.ksv$srb.est[pd.srb.ksv$year == (y - 2)] /
                              pd.srb.ksv$srb.est[pd.srb.ksv$year == (y - 3)]) +
                   (1/6) * (pd.srb.ksv$srb.est[pd.srb.ksv$year == (y - 3)] /
                              pd.srb.ksv$srb.est[pd.srb.ksv$year == (y - 4)])
  
  ksv.cow.growth <- (1/2) * (pd.srb.ksv$ksv.est[pd.srb.ksv$year == (y - 1)] /
                               pd.srb.ksv$ksv.est[pd.srb.ksv$year == (y - 2)]) +
                    (1/3) * (pd.srb.ksv$ksv.est[pd.srb.ksv$year == (y - 2)] /
                               pd.srb.ksv$ksv.est[pd.srb.ksv$year == (y - 3)]) +
                    (1/6) * (pd.srb.ksv$ksv.est[pd.srb.ksv$year == (y - 3)] /
                               pd.srb.ksv$ksv.est[pd.srb.ksv$year == (y - 4)])
  
  # add growth estimates to placeholder dataframe
  pd.srb.ksv$ksv.growth[pd.srb.ksv$year == y] <- ksv.cow.growth
  pd.srb.ksv$srb.growth[pd.srb.ksv$year == y] <- srb.un.growth
  
  # calculate population estimates and add to placeholder dataframe
  pd.srb.ksv$ksv.est[pd.srb.ksv$year == y] <- pd.srb.ksv$ksv.est[pd.srb.ksv$year == (y - 1)] *
                                                ksv.cow.growth
  pd.srb.ksv$srb.est[pd.srb.ksv$year == y] <- pd.srb.ksv$srb.est[pd.srb.ksv$year == (y - 1)] *
                                                srb.un.growth
  
}

pd.srb.ksv <- pd.srb.ksv %>%
  # add original SRB 2013-2019 UN population estimates to table (representing SRB+KSV)
  dplyr::left_join(pd %>%
                     dplyr::filter(iso3c == "SRB") %>%
                     dplyr::select(year, un.pop) %>%
                     na.omit()) %>%
  # Calculate combined KSV+SRB estimate, annual alpha rate ((ksv.est+srb.est)/srb.un.combined), and
  # readjusted estimates multiplied by the alpha rate. The alpha rate multiplication ensures the
  # combined KSV and SRB estimates line up with the official, combined UN estimates.
  dplyr::mutate(
    combined.est = ksv.est + srb.est,
    alpha = un.pop / combined.est,
    ksv.est.alpha = ifelse(year %in% c(2013:2019), alpha * ksv.est,ksv.est),
    srb.est.alpha = ifelse(year %in% c(2013:2019), alpha * srb.est,srb.est)
    ) %>%
  dplyr::select(year, ksv.est.alpha, srb.est.alpha) %>%
  tidyr::pivot_longer(2:3, names_to = "iso3c", values_to = "un.pop") %>%
  dplyr::mutate(
    iso3c = ifelse(iso3c == "ksv.est.alpha", "KSV", "SRB"),
    cow.pop = NA
    )

# add KSV and SRB COW 2009-2012 estimates to the reformatted pd.srb.ksv dataset
for(j in 2009:2012){
  
  pd.srb.ksv$cow.pop[pd.srb.ksv$iso3c == "KSV" & pd.srb.ksv$year == j] <- pd$cow.pop[pd$iso3c == "KSV" & pd$year == j]
  pd.srb.ksv$cow.pop[pd.srb.ksv$iso3c == "SRB" & pd.srb.ksv$year == j] <- pd$cow.pop[pd$iso3c == "SRB" & pd$year == j]
  
}

# 2013-2019 COW KSV/SRB: apply three-year weighted growth rates to 2012 population estimates

for(k in 2013:2019){
  
  for(i in c("SRB","KSV")){
    
    # calculate growth rate
    growth.estimate <- (1/2) * pd.srb.ksv$cow.pop[pd.srb.ksv$iso3c == i &
                                                    pd.srb.ksv$year == (k - 1)] /
                          pd.srb.ksv$cow.pop[pd.srb.ksv$iso3c == i & pd.srb.ksv$year == (k - 2)] +
                       (1/3) * pd.srb.ksv$cow.pop[pd.srb.ksv$iso3c == i &
                                                    pd.srb.ksv$year == (k - 2)] /
                          pd.srb.ksv$cow.pop[pd.srb.ksv$iso3c == i & pd.srb.ksv$year == (k - 3)] +
                       (1/6)*pd.srb.ksv$cow.pop[pd.srb.ksv$iso3c == i &
                                                  pd.srb.ksv$year == (k - 3)] /
                          pd.srb.ksv$cow.pop[pd.srb.ksv$iso3c == i & pd.srb.ksv$year == (k - 4)]
    
    # apply growth rate
    pd.srb.ksv$cow.pop[pd.srb.ksv$iso3c == i & pd.srb.ksv$year == k] <- pd.srb.ksv$cow.pop[pd.srb.ksv$iso3c == i &
                                                                                             pd.srb.ksv$year == (k - 1)] *
                                                                          growth.estimate
    
  }
  
}

# filter 2009-2019 SRB and KSV entries from main dataset and rbind new estimates
pd <- pd %>%
  dplyr::filter(iso3c %!in% c("SRB", "KSV") | year %!in% c(2009:2019)) %>%
  rbind(pd.srb.ksv %>%
          dplyr::mutate(
            mpd.pop = NA,
            gl.pop = NA,
            un.pop.estimated = 1,
            cow.pop.estimated = 1,
            un.pop.est.method = NA,
            cow.pop.est.method = NA
          ))

# apply COW's 2008-2009 growth rate to UN's 2009 estimate to approximate UN's 2008 estimate
pd$un.pop[pd$iso3c == "KSV" & pd$year == 2008] <- pd$un.pop[pd$iso3c == "KSV" & pd$year == 2009] /
  (pd$cow.pop[pd$iso3c == "KSV" & pd$year == 2009] / pd$cow.pop[pd$iso3c == "KSV" & pd$year == 2008])

# remove YUG 1992-2012 from main dataset
pd <- pd %>%
  dplyr::filter(iso3c != "YUG" | year %!in% c(1992:2012))

# estimate COW growth rates for YUG successor states
special_growth_rates[["YUG_COW_1991_1992"]] <- sum(pd$cow.pop[pd$iso3c %in% c("SVN", "HRV", "BIH", "SRB", "MKD") & pd$year == 1992]) /
  pd$cow.pop[pd$iso3c == "YUG" & pd$year == 1991]

# capture growth rates
special_growth_rates[["SRB_UN_2005_2006"]] <- sum(pd$un.pop[pd$iso3c %in% c("SRB", "MNE") & pd$year == 2006]) /
                             pd$un.pop[pd$iso3c == "SRB" & pd$year == 2005]
special_growth_rates[["MNE_UN_2005_2006"]] <- sum(pd$un.pop[pd$iso3c %in% c("SRB", "MNE") & pd$year == 2006]) /
                             pd$un.pop[pd$iso3c=="SRB"&pd$year==2005]
special_growth_rates[["SRB_COW_2005_2006"]] <- sum(pd$cow.pop[pd$iso3c %in% c("SRB", "MNE") & pd$year == 2006]) /
                              pd$cow.pop[pd$iso3c == "SRB" & pd$year == 2005]
special_growth_rates[["MNE_COW_2005_2006"]] <- sum(pd$cow.pop[pd$iso3c %in% c("SRB", "MNE") & pd$year == 2006]) /
                              pd$cow.pop[pd$iso3c == "SRB" & pd$year == 2005]

special_growth_rates[["SRB_UN_2007_2008"]] <- sum(pd$un.pop[pd$iso3c %in% c("SRB", "KSV") & pd$year == 2008]) /
                             pd$un.pop[pd$iso3c=="SRB"&pd$year==2007]
special_growth_rates[["KSV_UN_2007_2008"]] <- sum(pd$un.pop[pd$iso3c %in% c("SRB", "KSV") & pd$year == 2008]) /
                             pd$un.pop[pd$iso3c == "SRB" & pd$year == 2007]
special_growth_rates[["SRB_COW_2007_2008"]] <- sum(pd$cow.pop[pd$iso3c %in% c("SRB", "KSV") & pd$year == 2008])/
                              pd$cow.pop[pd$iso3c == "SRB" & pd$year == 2007]
special_growth_rates[["KSV_COW_2007_2008"]] <- sum(pd$cow.pop[pd$iso3c %in% c("SRB", "KSV") & pd$year == 2008]) /
                              pd$cow.pop[pd$iso3c == "SRB" & pd$year == 2007]

#### SYR: Syria ------------------------------------------------------------------------------------
# COW missing 1959-1960 estimates
pd <- growth_chaining_func(df = pd, iso = "SYR", variable_missing = "cow.pop",
                           variable_complete = "un.pop", base_year_lower = 1958,
                           base_year_higher = 1961, restricted = c(1959:1960))

# # use growth rates from UN, scaled by proportion alpha to ensure the end growth rate
# # matches with the 1961 COW population estimate
# 
# alpha <- ((pd$cow.pop[pd$iso3c=="SYR" & pd$year==1961] *
#              pd$un.pop[pd$iso3c=="SYR" & pd$year==1958]) /
#             (pd$cow.pop[pd$iso3c=="SYR" & pd$year==1958] *
#                pd$un.pop[pd$iso3c=="SYR" & pd$year==1961]))^(1/3)
# 
# # calculate UN growth rates
# syr.un.growth.59 <- pd$un.pop[pd$iso3c=="SYR" & pd$year==1959] /
#   pd$un.pop[pd$iso3c=="SYR" & pd$year==1958]
# syr.un.growth.60 <- pd$un.pop[pd$iso3c=="SYR" & pd$year==1960] /
#   pd$un.pop[pd$iso3c=="SYR" & pd$year==1959]
# 
# # apply alpha scalar to UN growth rates
# syr.cow.growth.59 <- alpha * syr.un.growth.59
# syr.cow.growth.60 <- alpha * syr.un.growth.60
# 
# # calculate COW estimates
# pd$cow.pop[pd$iso3c=="SYR" & pd$year==1959] <- syr.cow.growth.59 *
#   pd$cow.pop[pd$iso3c=="SYR" & pd$year==1958]
# pd$cow.pop[pd$iso3c=="SYR" & pd$year==1960] <- syr.cow.growth.60 *
#   pd$cow.pop[pd$iso3c=="SYR" & pd$year==1959]

#### TWN: Taiwan -----------------------------------------------------------------------------------
# 1945-1949: use bridge regression to estimate un.pop using mpd.pop

# calculate 15-year correlation
ts_cor(pd, "TWN", "un.pop", "mpd.pop", c(1950:1964))

pd <- bridge_regression_func(df = pd, iso = "TWN", variable_missing = "un.pop",
                             variable_complete = "mpd.pop", restricted = c(1945:1949))

# 1945-1948: use bridge regression to estimate cow.pop using mpd.pop

# calculate 15-year correlation
ts_cor(pd, "TWN", "cow.pop", "mpd.pop", c(1949:1963))

pd <- bridge_regression_func(df = pd, iso = "TWN", variable_missing = "cow.pop",
                             variable_complete = "mpd.pop", restricted = c(1945:1948))

#### TZA/ZAN: Tanzania/Zanzibar --------------------------------------------------------------------
# UN codes TZA for Tanganyika+Zanzibar for 1950-2019
# COW TZA is for Tanganyika through 1964, Tanzania 1965 onward; COW ZAN coded for 1963-1964

# Tanzania unification coded beginning in 1964

# 1963: calculate proportion of ZAN from combined ZAN+TZA COW estimates and remove that proportion
# from 1963 TZA UN estimate

# 1961-1962: extend COW ZAN estimates by using UN's TZA+ZAN growth applied to COW's TZA estimates
# and the missing COW ZAN estimates

# add blank rows
pd <- pd %>%
  rbind(data.frame(iso3c = rep("ZAN", 2),
                   year = c(1961:1962),
                   un.pop = NA,
                   cow.pop = NA,
                   mpd.pop = NA,
                   gl.pop = NA,
                   un.pop.estimated = 1,
                   cow.pop.estimated = 1,
                   un.pop.est.method = NA,
                   cow.pop.est.method = NA))

for(z in 1962:1961){
  
  pd$cow.pop[pd$iso3c == "ZAN" & pd$year == z] <- (sum(pd$cow.pop[pd$iso3c %in% c("TZA", "ZAN") &
                                                                    pd$year == (z + 1)]) *
                                                     (pd$un.pop[pd$iso3c == "TZA" & pd$year == z] /
                                                        pd$un.pop[pd$iso3c == "TZA" & pd$year == (z + 1)])) -
                                                          pd$cow.pop[pd$iso3c == "TZA" & pd$year == z]
}

# calculate COW's 1961-1964 ZAN proportion of TZA+ZAN and apply to UN's combined TZA/ZAN population estimate
# calculate COW's 1961-1964 TZA proportion of TZA+ZAN and apply to UN's combined TZA/ZAN population estimate
for(t in 1961:1964){
  
  pd$un.pop[pd$iso3c == "ZAN" & pd$year == t] <- pd$un.pop[pd$iso3c == "TZA" & pd$year == t] *
                                                   pd$cow.pop[pd$iso3c == "ZAN" & pd$year == t] /
                                                   sum(pd$cow.pop[pd$iso3c %in% c("TZA", "ZAN") &
                                                                    pd$year == t])
  pd$un.pop[pd$iso3c == "TZA" & pd$year == t] <- pd$un.pop[pd$iso3c == "TZA" & pd$year == t] *
                                                    pd$cow.pop[pd$iso3c == "TZA" & pd$year == t] /
                                                    sum(pd$cow.pop[pd$iso3c %in% c("TZA", "ZAN") &
                                                                     pd$year == t])

}

# capture growth rates
special_growth_rates[["TZA_UN_1963_1964"]] <- pd$un.pop[pd$iso3c == "TZA" & pd$year == 1964] /
                                                sum(pd$un.pop[pd$iso3c %in% c("TZA", "ZAN") & pd$year == 1963])
special_growth_rates[["TZA_COW_1963_1964"]] <- pd$cow.pop[pd$iso3c=="TZA" & pd$year==1964] /
                                                sum(pd$un.pop[pd$iso3c %in% c("TZA", "ZAN") & pd$year == 1963])

#### YEM/YAR/YPR: Yemen, North Yemen, and South Yemen ----------------------------------------------
# COW and Gleditsch contain estimates for YAR (1946-1990), YPR (1967-1990), and YEM (1990-2012)
# UN and MPD contain combined estimates for YEM (combined) starting in 1950

# Yemen coded as unified beginning in 1991

# 1967-1990: calculate ratio between COW's YAR and YPR population estimates
# and apply to UN's YEM estimates
yar_ypr_pop_ratios <- pd %>%
  dplyr::filter(
    iso3c %in% c("YAR", "YPR"),
    # only have YPR data starting 1967
    year %in% c(1967:1990)
  ) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(cow_pop_combined = sum(cow.pop, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(cow_pop_ratio = cow.pop / cow_pop_combined) %>%
  dplyr::select(iso3c, year, cow_pop_ratio)

# assume 1967 ratio is consistent 1950-1966
yar_ypr_pop_ratios_est <- yar_ypr_pop_ratios %>%
  dplyr::filter(year == 1967) %>%
  dplyr::select(-year)

# append ratio estimates
yar_ypr_pop_ratios <- yar_ypr_pop_ratios_est %>%
  tidyr::crossing(year = 1950:1966) %>%
  bind_rows(yar_ypr_pop_ratios)

# merge in UN data and calculate estimates
yar_ypr_pop_ests <- yar_ypr_pop_ratios %>%
  # merge in UN's YEM population estimates
  dplyr::left_join(pd %>%
                     dplyr::filter(iso3c == "YEM") %>%
                     dplyr::select(year, un.pop),
                   by = "year") %>%
  # calculate UN estimates
  dplyr::mutate(un_est = un.pop * cow_pop_ratio) %>%
  dplyr::select(iso3c, year, un_est)

# merge estimates into main df
pd <- pd %>% 
  dplyr::left_join(yar_ypr_pop_ests, by = c("iso3c", "year")) %>%
  dplyr::mutate(
    un.pop = ifelse(iso3c %in% c("YAR", "YPR") & year %in% c(1950:1990), un_est, un.pop),
    # recode as estimated values
    un.pop.estimated = ifelse(iso3c %in% c("YAR", "YPR") & year %in% c(1950:1990), 1, un.pop.estimated),
    # add estimation methodology
    un.pop.est.method = ifelse(iso3c %in% c("YAR", "YPR") & year %in% c(1955:1990),
                               "UN estimate with COW North/South Yemen population proportion estimates",
                               un.pop.est.method)
  ) %>%
  dplyr::select(-un_est)

# YAR UN 1946-1949
pd <- growth_chaining_func(df = pd, iso = "YAR", variable_missing = "un.pop",
                           variable_complete = "cow.pop", base_year_higher = 1950,
                           restricted = c(1946:1949))

# capture growth rates
special_growth_rates[["YEM_UN_1990_1991"]] <- pd$un.pop[pd$iso3c == "YEM" & pd$year == 1991] /
                                               sum(pd$un.pop[pd$iso3c %in% c("YAR", "YPR") & pd$year == 1990])
special_growth_rates[["YEM_COW_1990_1991"]] <- pd$cow.pop[pd$iso3c == "YEM" & pd$year == 1991] /
                                                sum(pd$cow.pop[pd$iso3c %in% c("YAR", "YPR") & pd$year == 1990])

#### VNM/RVN: Vietnam and South Vietnam ------------------------------------------------------------
# UN codes VNM for both North and South Vietnam 1950-2019
# COW codes VNM for North Vietnam and RVN for South Vietnam 1954-1975, with VNM
# coded as a unified Vietnam starting in 1976

# Vietnam coded as unified beginning in 1976

# 1954-1975: calculate ratio between COW's VNM (North) and RVN population estimates
# and apply to UN's VNM estimates
pd.vnm.ratios <- pd %>%
  dplyr::filter(
    iso3c %in% c("VNM", "RVN"),
    # filter to only contain years after Vietnamese independence and before the reunification of
    # Vietnam
    year %in% c(1954:1975)
    ) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(cow.pop.combined = sum(cow.pop, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(cow.pop.ratio = cow.pop / cow.pop.combined) %>%
  dplyr::select(-c(un.pop, mpd.pop, gl.pop, un.pop.estimated, cow.pop.estimated, un.pop.est.method,
                   cow.pop.est.method))

# pull 1954 ratios for use later
pd.ratio.vnm <- pd.vnm.ratios$cow.pop.ratio[pd.vnm.ratios$iso3c == "VNM" & pd.vnm.ratios$year == 1954]
pd.ratio.rvn <- pd.vnm.ratios$cow.pop.ratio[pd.vnm.ratios$iso3c == "RVN" & pd.vnm.ratios$year == 1954]

# pull UN 1954-1975 population estimates
pd.vnm.estimates <- pd %>%
  dplyr::filter(iso3c == "VNM",
                year %in% c(1954:1975)) %>%
  dplyr::select(-c(iso3c, cow.pop))

# merge VNM estimates with VNM (North) and RVN ratios
pd.vnm.ratios <- pd.vnm.ratios %>%
  dplyr::full_join(pd.vnm.estimates, by = "year") %>%
  # apply ratios to un.pop
  dplyr::mutate(un.pop = un.pop * cow.pop.ratio) %>%
  dplyr::select(-c(cow.pop.combined, cow.pop.ratio))

# filter out VNM and RVN 1954-1975 from main dataset and rbind
# estimate dataset
pd <- pd %>%
  dplyr::filter(iso3c %!in% c("VNM", "RVN") | year %!in% c(1954:1975)) %>%
  rbind(pd.vnm.ratios)

# capture growth rates
special_growth_rates[["VNM_UN_1975_1976"]] <- pd$un.pop[pd$iso3c=="VNM" & pd$year==1976] /
                                                sum(pd$un.pop[pd$iso3c %in% c("VNM", "RVN") & pd$year==1975])
special_growth_rates[["VNM_COW_1975_1976"]] <- pd$cow.pop[pd$iso3c=="VNM" & pd$year==1976] /
                                                 sum(pd$cow.pop[pd$iso3c %in% c("VNM", "RVN") & pd$year==1975])

#### YUG/SRB: Yugoslavia/Serbia --------------------------------------------------------------------
# BIH: UN estimates 1950-2019; COW 1992-2012
# HRV: UN estimates 1950-2019; COW 1992-2012
# MKD: UN estimates 1950-2019; COW 1993-2012
# SVN: UN estimates 1950-2019; COW 1992-2012
# SRB: UN estimates 1950-2019, containing Serbia and Kosovo, but no Montenegro; COW no estimates
# MNE: UN estimates 1950-2019; COW 2006-2012
# KSV: UN no estimates; COW 2008-2012
# YUG: UN no estimates; COW 1946-1991 SFR Yugoslavia estimates, 1992-2012 contains Serbia,
## Montenegro, and Kosovo

# merge UN republic estimates into one YUG estimate through 1991 (preserving each republic's
# estimates)
pd.yug.un <- pd %>%
  # drop COW's estimates (already listed under "YUG" through 1991)
  dplyr::select(-cow.pop) %>%
  # select the 6 republics of Yugoslavia
  dplyr::filter(iso3c %in% c("BIH", "HRV", "MKD", "MNE", "SRB", "SVN")) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(un.pop = sum(un.pop, na.rm = TRUE)) %>%
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
pd.yug <- dplyr::full_join(pd.yug.un, pd.yug.cow, by = "year")

pd.yug$un.pop[pd.yug$year %in% c(1946:1949)] <- NA

pd.yug <- bridge_regression_func(df = pd.yug, iso = "YUG", variable_missing = "un.pop",
                                 variable_complete = "cow.pop", restricted = c(1946:1949))

# filter out YUG 1946-1991 entries from main dataset and rbind new estimates
pd <- pd %>%
  dplyr::filter(iso3c != "YUG" | year %!in% c(1946:1991)) %>%
  rbind(pd.yug)

# apply UN 1992-1993 MKD growth rate to COW estimate to approximate 1992 population
# COW likely does not code 1992 because MKD only gained universal recognition in 1993
# when Greece dropped objections over MKD's name
pd$cow.pop[pd$iso3c=="MKD" & pd$year==1992] <- pd$cow.pop[pd$iso3c=="MKD" & pd$year==1993]/
  (pd$un.pop[pd$iso3c=="MKD" & pd$year==1993] / pd$un.pop[pd$iso3c=="MKD" & pd$year==1992])

#### ZAF/NAM: South Africa/Namibia -----------------------------------------------------------------
# Both UN and COW ZAF populations do not include NAM prior to NAM independence
# NAM coded as independent beginning in 1990

# NAM 1950-1989: apply UN population growth rates to COW's 1990 population estimate
pd <- growth_chaining_func(df = pd, iso = "NAM", variable_missing = "cow.pop",
                           variable_complete = "un.pop", base_year_higher = 1990,
                           restricted = c(1950:1989))

# NAM 1946-1949: apply a weighted 3-year growth rate
pd <- three_year_growth_func(df = pd, iso = "NAM", variable_missing = "un.pop",
                             base_years = c(1950:1953), restricted = c(1949:1946))
pd <- three_year_growth_func(df = pd, iso = "NAM", variable_missing = "cow.pop",
                             base_years = c(1950:1953), restricted = c(1949:1946))

# ZAF 1946-1949
pd <- bridge_regression_func(df = pd, iso = "ZAF", variable_missing = "un.pop",
                             variable_complete = "cow.pop", restricted = c(1946:1949))

# pull ZAF 1989-1990 (excluding NAM) and NAM growth rates for use later
special_growth_rates[["ZAF_UN_1989_1990"]] <- pd$un.pop[pd$iso3c == "ZAF" & pd$year == 1990] /
                                                pd$un.pop[pd$iso3c == "ZAF" & pd$year == 1989]
special_growth_rates[["ZAF_COW_1989_1990"]] <- pd$cow.pop[pd$iso3c == "ZAF" & pd$year == 1990] /
                                                 pd$cow.pop[pd$iso3c == "ZAF" & pd$year == 1989]
special_growth_rates[["NAM_UN_1989_1990"]] <- pd$un.pop[pd$iso3c == "NAM" & pd$year == 1990] /
                                                pd$un.pop[pd$iso3c == "NAM" & pd$year == 1989]
special_growth_rates[["NAM_COW_1989_1990"]] <- pd$cow.pop[pd$iso3c == "NAM" & pd$year == 1990] /
                                                pd$cow.pop[pd$iso3c == "NAM" & pd$year == 1989]

# merge ZAF and NAM populations for 1946-1989
for(z in 1946:1989){
  
  pd$un.pop[pd$iso3c == "ZAF" & pd$year == z] <- sum(pd$un.pop[pd$iso3c %in% c("ZAF", "NAM") &
                                                                 pd$year == z])
  pd$cow.pop[pd$iso3c == "ZAF"&pd$year == z] <- sum(pd$cow.pop[pd$iso3c %in% c("ZAF", "NAM") &
                                                                 pd$year == z])
  
}

### calculate 1940s estimates ----------------------------------------------------------------------
# list of countries to estimate UN 1946-1949 populations using COW growth rates
# flagging: PAK
estimate.un.1940s <- c(
  "AFG", "ALB", "AND", "ARG", "AUS", "BEL", "BGR", "BOL", "BRA", "CAN", "CHE", "CHL", "CHN", "COL",
  "CRI", "CUB", "CZE", "DNK", "DOM", "ECU", "EGY", "ESP", "ETH", "FIN", "FRA", "GBR", "GRC", "GTM",
  "HND", "HTI", "HUN", "IDN", "IND", "IRL", "IRN", "IRQ", "ISL", "ITA", "JOR", "LBN", "LBR", "LIE",
  "LKA", "LUX", "MCO", "MEX", "MMR", "MNG", "NIC", "NLD", "NOR", "NPL", "NZL", "PAK", "PAN", "PER",
  "PHL", "POL", "PRT", "PRY", "ROU", "SAU", "SLV", "SMR", "SOV", "SWE", "SYR", "THA", "TUR", "URY",
  "USA", "VEN", "YAR", "YUG"
  )

# 1946-1949: apply COW population growth rates to UN's 1950 population estimate
for(iso in estimate.un.1940s){
  
  pd <- growth_chaining_func(df = pd, iso = iso, variable_missing = "un.pop",
                             variable_complete = "cow.pop", base_year_higher = 1950,
                             restricted = c(1946:1949))

}

# list of countries to estimate UN and COW 1946-1949 populations using a three-year weighted growth
# rate
estimate.growth.1940s <- c("AND", "LIE", "MCO", "SMR")

# 1946-1949: apply three-year weighted growth rate
for(iso in estimate.growth.1940s){
  
  pd <- three_year_growth_func(df = pd, iso = iso, variable_missing = "un.pop", base_years = c(1950:1953),
                               restricted = c(1949:1946))
  pd <- three_year_growth_func(df = pd, iso = iso, variable_missing = "cow.pop", base_years = c(1950:1953),
                               restricted = c(1949:1946))

}

# 1945: extend estimates using MPD values
estimate.growth.1945 <- c("ALB", "ARG", "AUS", "AUT", "BEL", "BGR", "BOL", "BRA", "CAN", "CHE",
                          "CHL", "CHN", "COL", "CRI", "CUB", "DEU", "DMA", "DNK", "ECU", "ESP",
                          "FIN", "FRA", "GBR", "GRC", "GTM", "HND", "HTI", "HUN", "IDN", "IND",
                          "IRL", "ITA", "JAM", "LKA", "MEX", "MMR", "MYS", "NIC", "NLD", "NOR",
                          "NZL", "PAN", "PER", "PHL", "PRT", "PRY", "ROU", "SLV", "SWE", "THA",
                          "TTO", "TUR", "URY", "USA", "VEN", "ZAF")
for(iso in estimate.growth.1945){
  
  pd <- growth_chaining_func(df = pd, iso = iso, variable_missing = "un.pop",
                             variable_complete = "mpd.pop", base_year_higher = 1946,
                             restricted = 1945)
  pd <- growth_chaining_func(df = pd, iso = iso, variable_missing = "cow.pop",
                             variable_complete = "mpd.pop", base_year_higher = 1946,
                             restricted = 1945)
  
}

### calculate 2013-2019 estimates ------------------------------------------------------------------
# list of countries to estimate COW 2013-2019 populations using UN growth rates
estimate.cow.2010s <- c(
  "AFG", "AGO", "ALB", "AND", "ARE", "ARG", "ARM", "ATG", "AUS", "AUT", "AZE", "BDI", "BEL", "BEN",
  "BFA", "BGD", "BGR", "BHR", "BHS", "BIH", "BLR", "BLZ", "BOL", "BRA", "BRB", "BRN", "BTN", "BWA",
  "CAF", "CAN", "CHE", "CHL", "CHN", "CIV", "CMR", "COD", "COG", "COL", "COM", "CPV", "CRI", "CUB",
  "CYP", "CZE", "DEU", "DJI", "DMA", "DNK", "DOM", "DZA", "ECU", "EGY", "ERI", "ESP", "EST", "ETH",
  "FIN", "FJI", "FRA", "FSM", "GAB", "GBR", "GEO", "GHA", "GIN", "GMB", "GNB", "GNQ", "GRC", "GRD",
  "GTM", "GUY", "HND", "HRV", "HTI", "HUN", "IDN", "IND", "IRL", "IRN", "IRQ", "ISL", "ITA", "JAM",
  "JOR", "JPN", "KAZ", "KEN", "KGZ", "KHM", "KIR", "KNA", "KOR", "KWT", "LAO", "LBN", "LBR", "LBY",
  "LCA", "LIE", "LKA", "LSO", "LTU", "LUX", "LVA", "MAR", "MCO", "MDA", "MDG", "MDV", "MEX", "MHL",
  "MKD", "MLI", "MLT", "MMR", "MNE", "MNG", "MOZ", "MRT", "MUS", "MWI", "MYS", "NAM", "NER", "NGA",
  "NIC", "NLD", "NOR", "NPL", "NRU", "NZL", "OMN", "PAK", "PAN", "PER", "PHL", "PLW", "PNG", "POL",
  "PRK", "PRT", "PRY", "QAT", "ROU", "RUS", "RWA", "SAU", "SDN", "SEN", "SGP", "SLB", "SLE", "SLV",
  "SMR", "SOM", "SSD", "STP", "SUR", "SVK", "SVN", "SWE", "SWZ", "SYC", "SYR", "TCD", "TGO", "THA",
  "TJK", "TKM", "TLS", "TON", "TTO", "TUN", "TUR", "TUV", "TWN", "TZA", "UGA", "UKR", "URY", "USA",
  "UZB", "VCT", "VEN", "VNM", "VUT", "WSM", "YEM", "ZAF", "ZMB", "ZWE"
  )

# 2013-2019: apply UN population growth rates to COW's and UN's 2012 population estimate
for(iso in estimate.cow.2010s){
  
  pd <- growth_chaining_func(df = pd, iso = iso, variable_missing = "cow.pop",
                             variable_complete = "un.pop", base_year_lower = 2012,
                             restricted = c(2013:2019))

}

### calculate growth rates -------------------------------------------------------------------------
pd <- pd %>%
  dplyr::group_by(iso3c) %>%
  dplyr::arrange(year) %>%
  dplyr::mutate(
    pop.growth.rate.un = 100 * (un.pop - lag(un.pop)) / lag(un.pop),
    pop.growth.rate.cow = 100 * (cow.pop - lag(cow.pop)) / lag(cow.pop)
  )

# pd_year_prior <- pd %>%
#   dplyr::mutate(year = year + 1) %>%
#   dplyr::select(iso3c, year, un.pop.plus1 = un.pop, cow.pop.plus1 = cow.pop)
# 
# pd <- pd %>%
#   dplyr::left_join(pd_year_prior, by = c("iso3c", "year")) %>%
#   dplyr::mutate(
#     pop.growth.rate.un = 100 * (un.pop - un.pop.plus1) / un.pop.plus1,
#     pop.growth.rate.cow = 100 * (cow.pop - cow.pop.plus1) / cow.pop.plus1
#     ) %>%
#   dplyr::select(-c(un.pop.plus1, cow.pop.plus1))

#### modify growth rates for country unification/dissolution ---------------------------------------
pd <- pd %>%
  dplyr::mutate(
    # add select un growth rates
    pop.growth.rate.un = dplyr::case_when(
      # Czechia / Slovakia
      iso3c == "CZE" & year == 1993 ~ special_growth_rates[["CZE_UN_1992_1993"]] - 1,
      iso3c == "SVK" & year == 1993 ~ special_growth_rates[["SVK_UN_1992_1993"]] - 1,
      # Germany
      iso3c == "DEU" & year == 1990 ~ special_growth_rates[["DEU_UN_1989_1990"]] - 1,
      # Eritrea / Ethiopia
      iso3c == "ERI" & year == 1993 ~ special_growth_rates[["ERI_UN_1992_1993"]] - 1,
      iso3c == "ETH" & year == 1993 ~ special_growth_rates[["ETH_UN_1992_1993"]] - 1,
      # Malaysia / Singapore
      iso3c == "MYS" & year == 1963 ~ special_growth_rates[["MYS_UN_1962_1963"]] - 1,
      iso3c == "MYS" & year == 1965 ~ special_growth_rates[["MYS_UN_1964_1965"]] - 1,
      iso3c == "SGP" & year == 1965 ~ special_growth_rates[["SGP_UN_1964_1965"]] - 1,
      # Bangladesh / Pakistan
      iso3c == "BGD" & year == 1971 ~ special_growth_rates[["BGD_UN_1970_1971"]] - 1,
      iso3c == "PAK" & year == 1971 ~ special_growth_rates[["PAK_UN_1970_1971"]] - 1,
      # South Sudan / Sudan
      iso3c == "SSD" & year == 2011 ~ special_growth_rates[["SSD_UN_2010_2011"]] - 1,
      iso3c == "SDN" & year == 2011 ~ special_growth_rates[["SDN_UN_2010_2011"]] - 1,
      # Tanzania
      iso3c == "TZA" & year == 1964 ~ special_growth_rates[["TZA_UN_1963_1964"]] - 1,
      # Yemen
      iso3c == "YEM" & year == 1991 ~ special_growth_rates[["YEM_UN_1990_1991"]] - 1,
      # Vietnam
      iso3c == "VNM" & year == 1976 ~ special_growth_rates[["VNM_UN_1975_1976"]] - 1,
      # Serbia
      iso3c == "SRB" & year == 2008 ~ special_growth_rates[["SRB_UN_2007_2008"]] - 1,
      # Namibia / South Africa
      iso3c == "NAM" & year == 1990 ~ special_growth_rates[["NAM_UN_1989_1990"]] - 1,
      iso3c == "ZAF" & year == 1990 ~ special_growth_rates[["ZAF_UN_1989_1990"]] - 1,
      .default = pop.growth.rate.un
    ),
    pop.growth.rate.cow = dplyr::case_when(
      # Czechia / Slovakia
      iso3c == "CZE" & year == 1993 ~ special_growth_rates[["CZE_COW_1992_1993"]] - 1,
      iso3c == "SVK" & year == 1993 ~ special_growth_rates[["SVK_COW_1992_1993"]] - 1,
      # Germany
      iso3c == "DEU" & year == 1990 ~ special_growth_rates[["DEU_COW_1989_1990"]] - 1,
      # Eritrea / Ethiopia
      iso3c == "ERI" & year == 1993 ~ special_growth_rates[["ERI_COW_1992_1993"]] - 1,
      iso3c == "ETH" & year == 1993 ~ special_growth_rates[["ETH_COW_1992_1993"]] - 1,
      # Malaysia / Singapore
      iso3c == "MYS" & year == 1963 ~ special_growth_rates[["MYS_COW_1962_1963"]] - 1,
      iso3c == "MYS" & year == 1965 ~ special_growth_rates[["MYS_COW_1964_1965"]] - 1,
      iso3c == "SGP" & year == 1965 ~ special_growth_rates[["SGP_COW_1964_1965"]] - 1,
      # Bangladesh / Pakistan
      iso3c == "BGD" & year == 1971 ~ special_growth_rates[["BGD_COW_1970_1971"]] - 1,
      iso3c == "PAK" & year == 1971 ~ special_growth_rates[["PAK_COW_1970_1971"]] - 1,
      # South Sudan/ Sudan
      iso3c == "SSD" & year == 2011 ~ special_growth_rates[["SSD_COW_2010_2011"]] - 1,
      iso3c == "SDN" & year == 2011 ~ special_growth_rates[["SDN_COW_2010_2011"]] - 1,
      # Soviet successor states
      iso3c %in% c("EST", "LVA", "LTU", "BLR", "UKR", "MDA", "RUS", "GEO", "ARM", "AZE", "KAZ",
                   "KGZ", "TKM", "TJK", "UZB") & year == 1991 ~ special_growth_rates[["SOV_COW_1990_1991"]] - 1,
      # Tanzania
      iso3c == "TZA" & year == 1964 ~ special_growth_rates[["TZA_COW_1963_1964"]] - 1,
      # Yemen
      iso3c == "YEM" & year == 1991 ~ special_growth_rates[["YEM_COW_1990_1991"]] - 1,
      # Vietnam
      iso3c == "VNM" & year == 1976 ~ special_growth_rates[["VNM_COW_1975_1976"]] - 1,
      # Yugoslav successor states
      iso3c %in% c("SVN","HRV","BIH","SRB","MKD") & year == 1992 ~ special_growth_rates[["YUG_COW_1991_1992"]] - 1,
      iso3c == "SRB" & year == 2006 ~ special_growth_rates[["SRB_COW_2005_2006"]] - 1,
      iso3c == "MNE" & year == 2006 ~ special_growth_rates[["MNE_COW_2005_2006"]] - 1,
      iso3c == "SRB" & year == 2008 ~ special_growth_rates[["SRB_COW_2007_2008"]] - 1,
      iso3c == "KSV" & year == 2008 ~ special_growth_rates[["KSV_COW_2007_2008"]] - 1,
      # Namibia / South Africa
      iso3c == "NAM" & year == 1990 ~ special_growth_rates[["NAM_COW_1989_1990"]] - 1,
      iso3c == "ZAF" & year == 1990 ~ special_growth_rates[["ZAF_COW_1989_1990"]] - 1,
      .default = pop.growth.rate.cow
    )
  )


### write data -------------------------------------------------------------------------------------
# writes formatted dataframe as csv files
write.csv(pd, "Data files/Formatted data files/population.csv", row.names = FALSE)


### codebook ---------------------------------------------------------------------------------------
# iso3c
### A country's standardized iso3c code, with non-standard codes for West Germany, East Germany,
### North Yemen, South Yemen, South Vietnam, the Netherlands Antilles, the Soviet Union, and
### Yugoslavia.
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
