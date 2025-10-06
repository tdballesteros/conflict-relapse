
# Correlates of War international trade dataset
# https://correlatesofwar.org/data-sets/bilateral-trade/


### load libraries ---------------------------------------------------------------------------------
library(readxl)
library(countrycode)
library(dplyr)
library(tidyverse)


### not in function --------------------------------------------------------------------------------
'%!in%' <- function(x,y)!('%in%'(x,y))


### load data files --------------------------------------------------------------------------------
# COW trade dataset
cow_trade <- read.csv("Data files/Raw data files/COW_Trade_4.0/National_COW_4.0.csv")

# country-years dataset
cyears <- read.csv("Data files/Formatted data files/country_years.csv")

# gdp dataset
gdp <- read.csv("Data files/Formatted data files/gdp.csv")

# population dataset
population <- read.csv("Data files/Formatted data files/population.csv")


### format data files ------------------------------------------------------------------------------
cow_trade <- cow_trade %>%
  dplyr::filter(
    year >= 1945,
    # filter Czechoslovakia values 2007-2009
    statename != "Czechoslovakia" | year %!in% c(2007:2009)
  ) %>%
  # using the countrycode package, add iso3c based on country COW abbreviation
  dplyr::mutate(
    iso3c = dplyr::case_when(
      stateabb == "CZE" ~ "CZE",
      stateabb == "GDR" ~ "DDR",
      stateabb == "GFR" ~ "BRD",
      stateabb == "RVN" ~ "RVN",
      stateabb == "YAR" ~ "YAR",
      stateabb == "YPR" ~ "YPR",
      stateabb == "YUG" ~ "YUG",
      stateabb == "ZAN" ~ "ZAN",
      .default = countrycode::countrycode(stateabb,"cowc","iso3c")
      ),
    # convert import and export values to total values, from $ in millions
    imports = 1000000 * imports,
    exports = 1000000 * exports,
    
    # create total trade and balance of trade metrics
    total_trade = imports + exports,
    trade_balance = imports - exports,
    ) %>%
  dplyr::select(iso3c, year, imports, exports, total_trade, trade_balance) %>%
  
  dplyr::full_join(cyears, by = c("iso3c", "year")) %>%
  # drop unneeded cyears variables
  dplyr::select(-c(country, yrs_since_indep)) %>%
  
  dplyr::left_join(gdp, by = c("iso3c", "year")) %>%
  dplyr::left_join(population, by = c("iso3c", "year")) %>%
  # drop unneeded gdp and population variables
  dplyr::select(iso3c, year, imports, exports, total_trade, trade_balance, cn, gdp.pwt.est,
                gdp.gl.est, un.pop, cow.pop)


### missing data -----------------------------------------------------------------------------------

# Small countries missing from dataset: AND, BTN, CPV, FSM, LIE, MCO, MHL, PLW, SMR, ZAN
# Notable countries missing from dataset: ERI, TLS

#### AGO: Angola -----------------------------------------------------------------------------------
# Missing: 1975-1976

# calculate compound annual growth rates from 1974-1977 - assume constant growth across missing years
ago.imports.1974 <- cow_trade$imports[cow_trade$iso3c == "AGO" & cow_trade$year == 1974]
ago.imports.1977 <- cow_trade$imports[cow_trade$iso3c == "AGO" & cow_trade$year == 1977]

ago.exports.1974 <- cow_trade$exports[cow_trade$iso3c == "AGO" & cow_trade$year == 1974]
ago.exports.1977 <- cow_trade$exports[cow_trade$iso3c == "AGO" & cow_trade$year == 1977]

# calculate cagr
ago.cagr.imports <- ((ago.imports.1977 / ago.imports.1974)^(1/3)) - 1
ago.cagr.exports <- ((ago.exports.1977 / ago.exports.1974)^(1/3)) - 1

# apply cagr to 1954 values
ago.imports.1975.est <- ago.imports.1974 * (1 + ago.cagr.imports)
ago.imports.1976.est <- ago.imports.1975.est * (1 + ago.cagr.imports)

ago.exports.1975.est <- ago.exports.1974 * (1 + ago.cagr.exports)
ago.exports.1976.est <- ago.exports.1975.est * (1 + ago.cagr.exports)

# assign value in df
cow_trade$imports[cow_trade$iso3c == "AGO" & cow_trade$year == 1975] <- ago.imports.1975.est
cow_trade$imports[cow_trade$iso3c == "AGO" & cow_trade$year == 1976] <- ago.imports.1976.est

cow_trade$exports[cow_trade$iso3c == "AGO" & cow_trade$year == 1975] <- ago.exports.1975.est
cow_trade$exports[cow_trade$iso3c == "AGO" & cow_trade$year == 1976] <- ago.exports.1976.est

cow_trade$total_trade[cow_trade$iso3c == "AGO" & cow_trade$year == 1975] <- ago.imports.1975.est + ago.exports.1975.est
cow_trade$total_trade[cow_trade$iso3c == "AGO" & cow_trade$year == 1976] <- ago.imports.1976.est + ago.exports.1976.est

cow_trade$trade_balance[cow_trade$iso3c == "AGO" & cow_trade$year == 1975] <- ago.imports.1975.est - ago.exports.1975.est
cow_trade$trade_balance[cow_trade$iso3c == "AGO" & cow_trade$year == 1976] <- ago.imports.1976.est - ago.exports.1976.est

#### ALB: Albania ----------------------------------------------------------------------------------
# Missing: 1960-1980, imports

#### ATG: Antigua and Barbuda ----------------------------------------------------------------------
# Missing: 1981-2014, imports

#### BDI: Burundi ----------------------------------------------------------------------------------
# Missing: 1962-1968, imports

#### BFA: Burkina Faso -----------------------------------------------------------------------------
# Missing: 1960, imports

#### BGD: Bangladesh -------------------------------------------------------------------------------
# Missing: 1971; 1972, imports

#### BHS: Bahamas ----------------------------------------------------------------------------------
# Missing: 1965, imports

#### BRN: Brunei -----------------------------------------------------------------------------------
# Missing: 1966, exports

#### BWA: Botswana ---------------------------------------------------------------------------------
# 1966-2014, imports

#### CHN: China ------------------------------------------------------------------------------------
# Missing: 1960, imports

# Previous 3 years have roughly equal imports and exports
# calculate average relative change in imports and exports for prior 3 years

# pull prior years import and export values
chn.imports.1956 <- cow_trade$imports[cow_trade$iso3c == "CHN" & cow_trade$year == 1956]
chn.imports.1957 <- cow_trade$imports[cow_trade$iso3c == "CHN" & cow_trade$year == 1957]
chn.imports.1958 <- cow_trade$imports[cow_trade$iso3c == "CHN" & cow_trade$year == 1958]
chn.imports.1959 <- cow_trade$imports[cow_trade$iso3c == "CHN" & cow_trade$year == 1959]

chn.exports.1956 <- cow_trade$exports[cow_trade$iso3c == "CHN" & cow_trade$year == 1956]
chn.exports.1957 <- cow_trade$exports[cow_trade$iso3c == "CHN" & cow_trade$year == 1957]
chn.exports.1958 <- cow_trade$exports[cow_trade$iso3c == "CHN" & cow_trade$year == 1958]
chn.exports.1959 <- cow_trade$exports[cow_trade$iso3c == "CHN" & cow_trade$year == 1959]
chn.exports.1960 <- cow_trade$exports[cow_trade$iso3c == "CHN" & cow_trade$year == 1960]

# calculate proportional annual change
chn.imports.1956.1957.change <- chn.imports.1957 / chn.imports.1956
chn.imports.1957.1958.change <- chn.imports.1958 / chn.imports.1957
chn.imports.1958.1959.change <- chn.imports.1959 / chn.imports.1958

chn.exports.1956.1957.change <- chn.exports.1957 / chn.exports.1956
chn.exports.1957.1958.change <- chn.exports.1958 / chn.exports.1957
chn.exports.1958.1959.change <- chn.exports.1959 / chn.exports.1958

# calculate the average of the ratio of proportional change of imports to exports
chn.imports.avg.change <- c(chn.imports.1956.1957.change,
                            chn.imports.1957.1958.change,
                            chn.imports.1958.1959.change)

chn.exports.avg.change <- c(chn.exports.1956.1957.change,
                            chn.exports.1957.1958.change,
                            chn.exports.1958.1959.change)

chn.prop.avg.change <- mean(chn.imports.avg.change / chn.exports.avg.change)
# average proportional changes between imports and exports are near identical

# calculate the 1959-1960 exports proportional change
chn.exports.1959.1960.change <- chn.exports.1960 / chn.exports.1959

# apply relative imports-exports proportional average change ratio to the 1959-1960 exports
# proportional change and apply this to the 1959 imports value
chn.imports.1960.est <- chn.imports.1959 * chn.prop.avg.change * chn.exports.1959.1960.change

# assign value in df
cow_trade$imports[cow_trade$iso3c == "CHN" & cow_trade$year == 1960] <- chn.imports.1960.est

#### DJI: Djibouti ---------------------------------------------------------------------------------
# Missing: 1977-1980, imports

#### ERI: Eritrea ----------------------------------------------------------------------------------
# Not in dataset

#### GIN: Guinea -----------------------------------------------------------------------------------
# Missing: 1961-1969, imports

#### GNQ: Equatorial Guinea ------------------------------------------------------------------------
# Missing: 1968-1973, imports

#### KHM: Cambodia ---------------------------------------------------------------------------------
# Missing: 1953

#### KIR: Kiribati ---------------------------------------------------------------------------------
# Missing: 1999-2014, imports

#### LAO: Laos -------------------------------------------------------------------------------------
# Missing: 1953

#### LBY: Libya ------------------------------------------------------------------------------------
# Missing: 1951

#### LCA: St. Lucia --------------------------------------------------------------------------------
# Missing: 1979-1980, imports

#### LSO: Lesotho ----------------------------------------------------------------------------------
# Missing: 1966-2014, imports

#### MDV: Maldives ---------------------------------------------------------------------------------
# Missing: 1965-1980, imports

#### MLI: Mali -------------------------------------------------------------------------------------
# Missing: 1960, imports; 1962, imports

#### MNG: Mongolia ---------------------------------------------------------------------------------
# Missing: 1960-1980, imports

#### MRT: Mauritania -------------------------------------------------------------------------------
# Missing: 1960-1961, imports

#### NAM: Namibia ----------------------------------------------------------------------------------
# Missing: 1990-2014, imports

#### NPL: Nepal ------------------------------------------------------------------------------------
# Missing: 1946-1963; 1964-1980, imports

#### NRU: Nauru ------------------------------------------------------------------------------------
# Missing: 1999-2014, imports

#### PRK: North Korea ------------------------------------------------------------------------------
# Missing: 1960-1980, imports

#### RWA: Rwanda -----------------------------------------------------------------------------------
# Missing: 1962-1963, imports

#### SAU: Saudi Arabia -----------------------------------------------------------------------------
# Missing: 1960-1963, imports

#### SOM: Somalia ----------------------------------------------------------------------------------
# Missing: 1960-1961, imports

#### SWZ: eSwatini ---------------------------------------------------------------------------------
# Missing: 1968-2014, imports

#### SYR: Syria ------------------------------------------------------------------------------------
# Missing: 1959

# calculate compound annual growth rates from 1958-1960 - assume constant growth across missing year
syr.imports.1958 <- cow_trade$imports[cow_trade$iso3c == "SYR" & cow_trade$year == 1958]
syr.imports.1960 <- cow_trade$imports[cow_trade$iso3c == "SYR" & cow_trade$year == 1960]

syr.exports.1958 <- cow_trade$exports[cow_trade$iso3c == "SYR" & cow_trade$year == 1958]
syr.exports.1960 <- cow_trade$exports[cow_trade$iso3c == "SYR" & cow_trade$year == 1960]

# calculate cagr
syr.cagr.imports <- ((syr.imports.1960 / syr.imports.1958)^(1/2)) - 1
syr.cagr.exports <- ((syr.exports.1960 / syr.exports.1958)^(1/2)) - 1

# apply cagr to 1958 values
syr.imports.1959.est <- syr.imports.1958 * (1 + syr.cagr.imports)
syr.exports.1959.est <- syr.exports.1958 * (1 + syr.cagr.exports)

# assign value in df
cow_trade$imports[cow_trade$iso3c == "SYR" & cow_trade$year == 1959] <- syr.imports.1959.est
cow_trade$exports[cow_trade$iso3c == "SYR" & cow_trade$year == 1959] <- syr.exports.1959.est
cow_trade$total_trade[cow_trade$iso3c == "SYR" & cow_trade$year == 1959] <- syr.imports.1959.est + syr.exports.1959.est
cow_trade$trade_balance[cow_trade$iso3c == "SYR" & cow_trade$year == 1959] <- syr.imports.1959.est - syr.exports.1959.est

#### TLS: Timor Leste ------------------------------------------------------------------------------
# Not in dataset

#### TUV: Tuvalu -----------------------------------------------------------------------------------
# Missing: 2000-2014, imports

#### TWN: Taiwan -----------------------------------------------------------------------------------
# Missing: 1960-2014, imports

#### ZAF: South Africa -----------------------------------------------------------------------------
# Missing: 1960-1997, imports

# approximate based on % annual change of exports

# pull ZAF 1959-1997 export values annual change
zaf.exports.1959.1997.perc_change <- cow_trade %>%
  dplyr::filter(
    iso3c == "ZAF"
  ) %>%
  dplyr::arrange(year) %>%
  dplyr::mutate(
    exports_lagged = lag(exports),
    exports_perc_change = (exports - exports_lagged) / exports_lagged
  ) %>%
  dplyr::filter(
    year %in% c(1960:1997)
  ) %>%
  dplyr::pull(exports_perc_change)

# apply exports percent change to imports
for(y in c(1960:1997)){
  
  # pull prior year imports value (or estimate)
  zaf.imports.yminus1 <- cow_trade$imports[cow_trade$iso3c == "ZAF" & cow_trade$year == (y-1)]
  
  # estimate imports value
  zaf.imports.est.y <- zaf.imports.yminus1 * (1 + zaf.exports.1959.1997.perc_change[y-1959])
  
  # add estimate to df
  cow_trade$imports[cow_trade$iso3c == "ZAF" & cow_trade$year == y] <- zaf.imports.est.y
  
}

### Special Cases ----------------------------------------------------------------------------------

#### Czechoslovakia --------------------------------------------------------------------------------
# CZE - Czechoslovakia 1945-1992; CZE Czechia 1993-2014; SVK Slovakia 1993-2014

# Calculate 1992 estimates for Czechia and Slovakia based on proportion of 1993 values

# # czechia
# cze_1992_imports_est <- cow_trade$imports[cow_trade$iso3c == "CZE" & cow_trade$year == 1992] *
#   cow_trade$imports[cow_trade$iso3c == "CZE" & cow_trade$year == 1993] /
#   (cow_trade$imports[cow_trade$iso3c == "CZE" & cow_trade$year == 1993] +
#      cow_trade$imports[cow_trade$iso3c == "SVK" & cow_trade$year == 1993])
# cze_1992_exports_est <- cow_trade$exports[cow_trade$iso3c == "CZE" & cow_trade$year == 1992] *
#   cow_trade$exports[cow_trade$iso3c == "CZE" & cow_trade$year == 1993] /
#   (cow_trade$exports[cow_trade$iso3c == "CZE" & cow_trade$year == 1993] +
#      cow_trade$exports[cow_trade$iso3c == "SVK" & cow_trade$year == 1993])
# 
# cze_1992_total_trade_est <- cze_1992_imports_est + cze_1992_exports_est
# cze_1992_trade_balance_est <- cze_1992_imports_est - cze_1992_exports_est
# 
# # slovakia
# svk_1992_imports_est <- cow_trade$imports[cow_trade$iso3c == "SVK" & cow_trade$year == 1992] *
#   cow_trade$imports[cow_trade$iso3c == "SVK" & cow_trade$year == 1993] /
#   (cow_trade$imports[cow_trade$iso3c == "CZE" & cow_trade$year == 1993] +
#      cow_trade$imports[cow_trade$iso3c == "SVK" & cow_trade$year == 1993])
# svk_1992_exports_est <- cow_trade$exports[cow_trade$iso3c == "SVK" & cow_trade$year == 1992] *
#   cow_trade$exports[cow_trade$iso3c == "SVK" & cow_trade$year == 1993] /
#   (cow_trade$exports[cow_trade$iso3c == "CZE" & cow_trade$year == 1993] +
#      cow_trade$exports[cow_trade$iso3c == "SVK" & cow_trade$year == 1993])
# 
# svk_1992_total_trade_est <- svk_1992_imports_est + svk_1992_exports_est
# svk_1992_trade_balance_est <- svk_1992_imports_est - svk_1992_exports_est
# 
# # pull 1993 values
# cze_1993_imports <- cow_trade$imports[cow_trade$iso3c == "CZE" & cow_trade$year == 1993]
# cze_1993_exports <- cow_trade$exports[cow_trade$iso3c == "CZE" & cow_trade$year == 1993]
# cze_1993_total_trade <- cow_trade$total_trade[cow_trade$iso3c == "CZE" & cow_trade$year == 1993]
# cze_1993_trade_balance <- cow_trade$trade_balance[cow_trade$iso3c == "CZE" & cow_trade$year == 1993]
# 
# svk_1993_imports <- cow_trade$imports[cow_trade$iso3c == "SVK" & cow_trade$year == 1993]
# svk_1993_exports <- cow_trade$exports[cow_trade$iso3c == "SVK" & cow_trade$year == 1993]
# svk_1993_total_trade <- cow_trade$total_trade[cow_trade$iso3c == "SVK" & cow_trade$year == 1993]
# svk_1993_trade_balance <- cow_trade$trade_balance[cow_trade$iso3c == "SVK" & cow_trade$year == 1993]
# 
# # recalculate % change values
# cow_trade$import.percent.change[cow_trade$iso3c == "CZE" & cow_trade$year == 1993] <- (cze_1993_imports - cze_1992_imports_est) / cze_1992_imports_est
# cow_trade$export.percent.change[cow_trade$iso3c == "CZE" & cow_trade$year == 1993] <- (cze_1993_exports - cze_1992_exports_est) / cze_1992_exports_est
# cow_trade$total.trade.percent.change[cow_trade$iso3c == "CZE" & cow_trade$year == 1993] <- (cze_1993_total_trade - cze_1992_total_trade_est) / cze_1992_total_trade_est
# cow_trade$trade.balance.percent.change[cow_trade$iso3c == "CZE" & cow_trade$year == 1993] <- (cze_1993_trade_balance - cze_1992_trade_balance_est) / cze_1992_trade_balance_est
# 
# cow_trade$import.percent.change[cow_trade$iso3c == "SVK" & cow_trade$year == 1993] <- (svk_1993_imports - svk_1992_imports_est) / svk_1992_imports_est
# cow_trade$export.percent.change[cow_trade$iso3c == "SVK" & cow_trade$year == 1993] <- (svk_1993_exports - svk_1992_exports_est) / svk_1992_exports_est
# cow_trade$total.trade.percent.change[cow_trade$iso3c == "SVK" & cow_trade$year == 1993] <- (svk_1993_total_trade - svk_1992_total_trade_est) / svk_1992_total_trade_est
# cow_trade$trade.balance.percent.change[cow_trade$iso3c == "SVK" & cow_trade$year == 1993] <- (svk_1993_trade_balance - svk_1992_trade_balance_est) / svk_1992_trade_balance_est

#### Germany ---------------------------------------------------------------------------------------
# DDR 1954-1989
# BRD 1955-1959 (missing 1960-1989)
# DEU 1960-2014

# DEU 1960-1989 values are for BRD - recode

#### Soviet Union ----------------------------------------------------------------------------------

# pull SOV data coded as RUS
cow_trade_sov <- cow_trade %>%
  dplyr::filter(
    iso3c == "RUS",
    year %in% c(1945:1990)
  ) %>%
  dplyr::mutate(iso3c = "SOV") %>%
  # drop gdp and population estimates for RUS that were appended
  dplyr::select(iso3c, year, imports, exports, total_trade, trade_balance, cn) %>%
  # rejoin gdp and population estimates
  dplyr::left_join(gdp, by = c("iso3c", "year")) %>%
  # drop unneeded gdp variables
  dplyr::select(-c(gdp.pwt.original, gdp.gl.original, imf.growth.rate.extend, wb.growth.rate,
                   mpd.cgdp.growth, mpd.rgdpna.growth, gdp.growth.rate.pwt.est,
                   gdp.growth.rate.gl.est)) %>%
  dplyr::left_join(population, by = c("iso3c", "year")) %>%
  # drop unneeded population variables
  dplyr::select(-c(pop.growth.rate.un, pop.growth.rate.cow, mpd.pop, gl.pop, un.pop.estimated,
                   cow.pop.estimated)) %>%
  dplyr::mutate(cn = ifelse(!is.na(cn), 1, NA))

# assess Russia 1991 imports/exports vs. post-Soviet 1992 imports/exports
russia_imports_1991 <- sum(cow_trade$imports[cow_trade$iso3c == "RUS" & cow_trade$year == 1991])
postsoviet_imports_1992_sum <- sum(cow_trade$imports[cow_trade$iso3c %in% c(
  "ARM", "AZE", "BLR", "EST", "GEO", "KAZ", "KGZ", "LTU", "LVA", "MDA", "RUS", "TJK", "TKM", "UKR",
  "UZB") & cow_trade$year == 1992])

russia_exports_1991 <- sum(cow_trade$exports[cow_trade$iso3c == "RUS" & cow_trade$year == 1991])
postsoviet_exports_1992_sum <- sum(cow_trade$exports[cow_trade$iso3c %in% c(
  "ARM", "AZE", "BLR", "EST", "GEO", "KAZ", "KGZ", "LTU", "LVA", "MDA", "RUS", "TJK", "TKM", "UKR",
  "UZB") & cow_trade$year == 1992])

# RUS values through 1991 (inclusive) appear to represent the entirety of the Soviet Union
# calculate the proportion of the post-Soviet 1992 imports and exports each country represents

postsoviet_1992 <- cow_trade %>%
  dplyr::filter(
    iso3c %in% c("ARM", "AZE", "BLR", "EST", "GEO", "KAZ", "KGZ", "LTU", "LVA", "MDA", "RUS", "TJK",
                 "TKM", "UKR", "UZB"),
    year == 1992
  ) %>%
  # sort alphabetically based on iso3c
  dplyr::arrange(iso3c)

postsoviet_imports_1992 <- dplyr::pull(postsoviet_1992, imports)
postsoviet_exports_1992 <- dplyr::pull(postsoviet_1992, exports)

names(postsoviet_imports_1992) <- names(postsoviet_exports_1992) <- c(
  "ARM", "AZE", "BLR", "EST", "GEO", "KAZ", "KGZ", "LTU", "LVA", "MDA", "RUS", "TJK", "TKM", "UKR",
  "UZB")

# calculate proportions
postsoviet_imports_1992 <- postsoviet_imports_1992 / postsoviet_imports_1992_sum
postsoviet_exports_1992 <- postsoviet_exports_1992 / postsoviet_exports_1992_sum

# estimate imports and exports based on 1992 proportions
for(c in names(postsoviet_imports_1992)){
  for(y in c(1946:1991)){
    
    # pull total SOV values
    total_sov_imports <- cow_trade$imports[cow_trade$iso3c == "RUS" & cow_trade$year == y]
    total_sov_exports <- cow_trade$exports[cow_trade$iso3c == "RUS" & cow_trade$year == y]
    
    # pull proportion of 1992 post-Soviet values
    prop_imports <- postsoviet_imports_1992[c]
    prop_exports <- postsoviet_exports_1992[c]
    
    # apply proportion to total SOV values
    imports_est <- total_sov_imports * prop_imports
    exports_est <- total_sov_exports * prop_exports
    
    # add estimates to df
    cow_trade$imports[cow_trade$iso3c == c & cow_trade$year == y] <- imports_est
    cow_trade$exports[cow_trade$iso3c == c & cow_trade$year == y] <- exports_est
    
  }
}

cow_trade <- cow_trade %>%
  # drop old SOV entries
  dplyr::filter(iso3c != "SOV") %>%
  # append SOV estimates back into the main df
  rbind(cow_trade_sov)

#### Sudan -----------------------------------------------------------------------------------------
# SDN 2011-2014 appears to be combined SDN/SSD data
# No SSD data in dataset

#### Vietnam ---------------------------------------------------------------------------------------
# RVN 1954-1959 (missing 1960-1975)
# VNM 1960-2014 (missing 1954-1959)

#### Yemen -----------------------------------------------------------------------------------------
# YAR: 1945-1990 (missing 1960-1969 imports, 1990)
# YEM 1990-2014
# YPR 1960-1989 (missing 1990)

# YAR 1960-1969, imports
# approximate based on % annual change of exports

# pull YAR 1959-1969 export values annual change
yar.exports.1959.1969.perc_change <- cow_trade %>%
  dplyr::filter(
    iso3c == "YAR"
  ) %>%
  dplyr::arrange(year) %>%
  dplyr::mutate(
    exports_lagged = lag(exports),
    exports_perc_change = (exports - exports_lagged) / exports_lagged
    ) %>%
  dplyr::filter(
    year %in% c(1960:1969)
  ) %>%
  dplyr::pull(exports_perc_change)

# apply exports percent change to imports
for(y in c(1960:1969)){
  
  # pull prior year imports value (or estimate)
  yar.imports.yminus1 <- cow_trade$imports[cow_trade$iso3c == "YAR" & cow_trade$year == (y-1)]
  
  # estimate imports value
  yar.imports.est.y <- yar.imports.yminus1 * (1 + yar.exports.1959.1969.perc_change[y-1959])
  
  # add estimate to df
  cow_trade$imports[cow_trade$iso3c == "YAR" & cow_trade$year == y] <- yar.imports.est.y
  
}
  

# YEM 1900 appears to be combined YAR and YPR values

#### Yugoslavia ------------------------------------------------------------------------------------


xx <- cow_trade %>%
  dplyr::filter(iso3c %in% c("YUG","SVN","HRV","BIH","SRB","MKD","KSV","MNE")) %>%
  dplyr::select(iso3c, year, imports, exports)




### 2015-2019 --------------------------------------------------------------------------------------
trade_volume_201x_approx_func <- function(df = cow_trade, years = c(2015:2019), iso){
  
  # pull 5-year range of data to approximate from
  data_range <- c((min(years)-6):(min(years)-1))
  
  df_data <- df %>%
    dplyr::filter(
      iso3c == iso,
      year %in% data_range
      ) %>%
    dplyr::arrange(year) %>%
    dplyr::mutate(
      imports_lagged = lag(imports),
      exports_lagged = lag(exports),
      perc_change_imports = (imports - imports_lagged) / imports_lagged,
      perc_change_exports = (exports - exports_lagged) / exports_lagged
      )
  
  df.imports <- dplyr::pull(df_data, perc_change_imports)[-1]
  df.exports <- dplyr::pull(df_data, perc_change_exports)[-1]
  
  df.imports.wgt.avg.change <- sum((df.imports * c(1:5))) / 15
  df.exports.wgt.avg.change <- sum((df.exports * c(1:5))) / 15
  
  for(y in years){
    df$imports[df$iso3c == iso & df$year == y] <- df$imports[df$iso3c == iso & df$year == (y-1)] * (1+df.imports.wgt.avg.change)
    df$exports[df$iso3c == iso & df$year == y] <- df$exports[df$iso3c == iso & df$year == (y-1)] * (1+df.exports.wgt.avg.change)
    df$total_trade[df$iso3c == iso & df$year == y] <- df$imports[df$iso3c == iso & df$year == y] + df$exports[df$iso3c == iso & df$year == y]
    df$trade_balance[df$iso3c == iso & df$year == y] <- df$imports[df$iso3c == iso & df$year == y] - df$exports[df$iso3c == iso & df$year == y]
  }
  
  return(df)
  
}

# pull list of countries to calculate 2015-2019 data for
trade_volume_2015_country_list <- cow_trade %>%
  dplyr::filter(
    year == 2014,
    !is.na(total_trade)
  ) %>%
  dplyr::pull(iso3c)

for(c in trade_volume_2015_country_list){
  
  cow_trade <- trade_volume_201x_approx_func(cow_trade, c(2015:2019), c)
}


### format final data ------------------------------------------------------------------------------
cow_trade <- cow_trade %>%
  dplyr::mutate(
    # calculate total_trade and trade_balance values if they are NA
    total_trade = ifelse(is.na(total_trade), imports + exports, total_trade),
    trade_balance = ifelse(is.na(trade_balance), imports + exports, trade_balance),
    
    # calculate trade values by gdp
    imports.perc.gdp.pwt = imports / gdp.pwt.est,
    imports.perc.gdp.gl = imports / gdp.gl.est,
    exports.perc.gdp.pwt = exports / gdp.pwt.est,
    exports.perc.gdp.gl = exports / gdp.gl.est,
    total.trade.perc.gdp.pwt = total_trade / gdp.pwt.est,
    total.trade.perc.gdp.gl = total_trade / gdp.gl.est,
    trade.balance.perc.gdp.pwt = trade_balance / gdp.pwt.est,
    trade.balance.perc.gdp.gl = trade_balance / gdp.gl.est,
    
    # calculate trade values by population
    imports.per.capita.un = imports / un.pop,
    imports.per.capita.cow = imports / cow.pop,
    exports.per.capita.un = exports / un.pop,
    exports.per.capita.cow = exports / cow.pop,
    total.trade.per.capita.un = total_trade / un.pop,
    total.trade.per.capita.cow = total_trade / cow.pop,
    trade.balance.per.capita.un = trade_balance / un.pop,
    trade.balance.per.capita.cow = trade_balance / cow.pop
  )

# create a lagged dataset for calculating annual change
cow_trade_lagged <- cow_trade %>%
  # drop unneeded variables
  dplyr::rename_with(~ paste0(.x, "_lagged"),
                     all_of(names(cow_trade)[names(cow_trade) %!in% c("iso3c", "year")])) %>%
  dplyr::select(-c(gdp.pwt.est_lagged, gdp.gl.est_lagged, un.pop_lagged, cow.pop_lagged,
                   cn_lagged)) %>%
  dplyr::mutate(year = year + 1)

# merge in lagged data
cow_trade <- cow_trade %>%
  dplyr::left_join(cow_trade_lagged, by = c("iso3c", "year")) %>%
  
  dplyr::mutate(
    import.percent.change = (imports - imports_lagged) / imports_lagged,
    exports.percent.change = (exports - exports_lagged) / exports_lagged,
    total.trade.percent.change = (total_trade - total_trade_lagged) / total_trade_lagged,
    trade.balance.percent.change = (trade_balance - trade_balance_lagged) / trade_balance_lagged,
    
  ) %>%
  
  dplyr::filter(cn == 1) %>%
  dplyr::select(iso3c, year, imports, exports, total_trade, trade_balance, imports.perc.gdp.pwt,
                imports.perc.gdp.gl, exports.perc.gdp.pwt, exports.perc.gdp.gl,
                total.trade.perc.gdp.pwt, total.trade.perc.gdp.gl, trade.balance.perc.gdp.pwt,
                trade.balance.perc.gdp.gl, imports.per.capita.un, imports.per.capita.cow,
                exports.per.capita.un, exports.per.capita.cow, total.trade.per.capita.un,
                total.trade.per.capita.cow, trade.balance.per.capita.un,
                trade.balance.per.capita.cow) %>%
  dplyr::mutate(trade.balance.perc.total.trade = trade_balance / total_trade)

### write data -------------------------------------------------------------------------------------
# writes formatted dataframe as csv files
write.csv(cow_trade,"Data files/Formatted data files/trade_volume.csv",row.names = FALSE)
