
# Uses Correlates of War's NMC dataset

# irst = Iron and steel production (thousands of tons)
# milex = Military Expenditures (For 1914+: thousands of current year US Dollars.)
# milper = Military Personnel (thousands)
# pec = Energy consumption (thousands of coal-ton equivalents)
# tpop = Total Population (thousands)
# upop = Urban population (population living in cities with population >100,000; in thousands)
# CINC = Composite Index of National Capability


### load libraries ---------------------------------------------------------------------------------
library(readxl)
library(countrycode)
library(dplyr)
library(tidyverse)


### not in function --------------------------------------------------------------------------------

'%!in%' <- function(x,y)!('%in%'(x,y))


### load data file ---------------------------------------------------------------------------------

national_material_capabilities <- read.csv("Data files/Raw data files/NMC_5_0.csv")

population <- read.csv("Data files/Formatted data files/population.csv")


### inflation table --------------------------------------------------------------------------------
# inflation from https://www.bls.gov/data/inflation_calculator.htm
# July to July, converting to 2019 dollars
inflation_table <- data.frame(
  year = c(1946:2020),
  multiplier = c(
    1+11.96,1+10.56,1+9.52,1+9.83, #40s
    1+9.65,1+8.91,1+8.61,1+8.57,1+8.54,1+8.57,1+8.36,1+8.07,1+7.85,1+7.79, #50s
    1+7.67,1+7.55,1+7.47,1+7.36,1+7.25,1+7.12,1+6.89,1+6.68,1+6.35,1+5.97, #60s
    1+5.58,1+5.30,1+5.12,1+4.79,1+4.19,1+3.73,1+3.49,1+3.21,1+2.91,1+2.51, #70s
    1+2.10,1+1.80,1+1.63,1+1.57,1+1.46,1+1.38,1+1.34,1+1.25,1+1.17,1+1.06, #80s
    1+0.97,1+0.88,1+0.83,1+0.78,1+0.73,1+0.68,1+0.63,1+0.60,1+0.57,1+0.54, #90s
    1+0.48,1+0.45,1+0.42,1+0.40,1+0.35,1+0.31,1+0.26,1+0.23,1+0.17,1+0.19, #00s
    1+0.18,1+0.14,1+0.12,1+0.10,1+0.08,1+0.08,1+0.07,1+0.05,1+0.02,1+0.00, #10s
    1-0.01 #20s
  )
)


### data formatting --------------------------------------------------------------------------------

nmc <- national_material_capabilities %>%
  
  # apply filters
  dplyr::filter(
    year > 1945
  ) %>%
  
  # convert COW names to standard iso3c codes
  dplyr::mutate(
    iso3c = dplyr::case_when(
      ccode == 260 ~ "BRD",
      ccode == 265 ~ "DDR",
      ccode == 315 ~ "CZE",
      ccode == 345 ~ "YUG",
      ccode == 347 ~ "KSV",
      ccode == 511 ~ "ZAN",
      ccode == 678 ~ "YAR",
      ccode == 680 ~ "YPR",
      ccode == 817 ~ "RVN",
      .default = countrycode::countrycode(ccode, "cown", "iso3c")
    )) %>%
  dplyr::relocate(iso3c, .before = "stateabb") %>%
  
  # drop unneeded variables
  dplyr::select(
    -c(stateabb, ccode, version)
  ) %>%
  
  # adjust for inflation
  dplyr::left_join(inflation_table, by = "year") %>%
  # multiples dollar values in that year's dollar value to 2019 dollars
  dplyr::mutate(milex = dplyr::case_when(
    milex > 0 ~ milex * multiplier,
    .default = milex
    )) %>%
  dplyr::select(-multiplier) %>%
  
  dplyr::mutate(
    
    # convert milex and milper to full numbers (currently in 1000s)
    milex = 1000 * milex,
    milper = 1000 * milper,
    
    # recode Serbia and Soviet Union
    iso3c = dplyr::case_when(
      iso3c == "YUG" & year >= 1992 ~ "SRB",
      iso3c == "RUS" & year <= 1990 ~ "SOV",
      .default = iso3c
      ),
    
    # create urban population % variable
    upop_perc = upop / tpop,
    # recode urban pop % > 100 as 100%
    upop_perc = dplyr::case_when(
      upop_perc > 1 ~ 1,
      .default = upop_perc
    ))

nmc_sov <- nmc %>%
  dplyr::filter(
    iso3c %in% c("SOV","EST","LVA","LTU","BLR","UKR","MDA","RUS","GEO","ARM","AZE",
                 "KAZ","KGZ","TKM","TJK","UZB")
    ) %>%
  dplyr::arrange(year)


### special coding ---------------------------------------------------------------------------------

## Special coding notes:
# DEU/BRD/DDR - 1990 has all 3 values
# ISR/PSE - only ISR, no PSE
# MYS/SGP - appears starting with independence each
# YUG et al. - need MKD 1992
# TZA/ZAN - ZAN coded as independent 1963-64
# YEM/YAR/YPR - 1990 has all 3 values

#### DEU/BRD/DDR -----------------------------------------------------------------------------------
# DEU/BRD/DDR - 1990 has all 3 values

nmc_deu_1990 <- nmc %>%
  dplyr::filter(
    iso3c %in% c("DEU", "BRD", "DDR"),
    year %in% c(1989:1991)
  ) %>%
  dplyr::arrange(year)

# estimate DEU 1990 pec by using BRD 1990 pec (milex, milper, and irst generally match
# DEU and BRD 1990)
nmc$pec[nmc$iso3c == "DEU" & nmc$year == 1990] <- nmc$pec[nmc$iso3c == "BRD" & nmc$year == 1990]

# filter out BRD and DDR 1990 values
nmc <- nmc %>%
  dplyr::filter(
    iso3c %!in% c("BRD", "DDR") | year != 1990
  )


#### MKD -------------------------------------------------------------------------------------------
# YUG et al. - need MKD 1992

nmc_yug_1992 <- nmc %>%
  dplyr::filter(
    iso3c %in% c("YUG", "SVN", "HRV", "BIH", "SRB", "MKD"),
    year %in% c(1991:1993)
  ) %>%
  dplyr::arrange(year)

# MKD 1992 appears to be missing


#### YEM/YAR/YPR -----------------------------------------------------------------------------------
# YEM/YAR/YPR - 1990 has all 3 values

nmc_yem_1990 <- nmc %>%
  dplyr::filter(
    iso3c %in% c("YEM", "YAR", "YPR"),
    year %in% c(1989:1991)
  ) %>%
  dplyr::arrange(year)

# milex YAR 1990
# pec YAR + YPR 1990
nmc$milex[nmc$iso3c == "YEM" & nmc$year == 1990] <- nmc$milex[nmc$iso3c == "YAR" & nmc$year == 1990]
nmc$pec[nmc$iso3c == "YEM" & nmc$year == 1990] <- sum(nmc$pec[nmc$iso3c %in% c("YAR", "YPR") &
                                                                nmc$year == 1990])

# filter out PAR and YPR 1990 values
nmc <- nmc %>%
  dplyr::filter(
    iso3c %!in% c("YAR", "YPR") | year != 1990
  )


### energy/steel/urbanicity values -----------------------------------------------------------------

# nmc_missing <- nmc %>%
#   dplyr::filter(
#     irst == -9 | milex == -9 | milper == -9 | pec == -9 | tpop == -9 | upop == -9 | cinc == -9
#   )

energyandsteel <- nmc %>%
  dplyr::select(iso3c, year, irst, pec, upop_perc) %>%
  
  # replace -9 with NA
  dplyr::mutate(
    irst = dplyr::case_when(
      irst == -9 ~ NA,
      .default = irst
    ),
    pec = dplyr::case_when(
      pec == -9 ~ NA,
      .default = pec
    )
  ) %>%
  
  # merge in population data
  dplyr::left_join(population, by = c("iso3c", "year")) %>%
  
  # calculate irst and pec per capita data (per 100,000 people)
  # using cow.pop estimates over un.pop estimates as this data originates from COW
  dplyr::mutate(
    irst_per_capita_un = 100000 * irst / un.pop,
    irst_per_capita_cow = 100000 * irst / cow.pop,
    pec_per_capita_un = 100000 * pec / un.pop,
    pec_per_capita_cow = 100000 * pec / cow.pop,
  ) %>%
  dplyr::select(iso3c, year, irst, pec, upop_perc, irst_per_capita_un, irst_per_capita_cow,
                pec_per_capita_un, pec_per_capita_cow)
  
energyandsteel <- energyandsteel %>%
  # apply 2012 values as 2013-2019 estimates
  rbind(energyandsteel %>%
          dplyr::filter(year == 2012) %>%
          dplyr::mutate(year = 2013),
        energyandsteel %>%
          dplyr::filter(year == 2012) %>%
          dplyr::mutate(year = 2014),
        energyandsteel %>%
          dplyr::filter(year == 2012) %>%
          dplyr::mutate(year = 2015),
        energyandsteel %>%
          dplyr::filter(year == 2012) %>%
          dplyr::mutate(year = 2016),
        energyandsteel %>%
          dplyr::filter(year == 2012) %>%
          dplyr::mutate(year = 2017),
        energyandsteel %>%
          dplyr::filter(year == 2012) %>%
          dplyr::mutate(year = 2018),
        energyandsteel %>%
          dplyr::filter(year == 2012) %>%
          dplyr::mutate(year = 2019))


### write data -------------------------------------------------------------------------------------
# writes formatted dataframe as csv files
write.csv(energyandsteel,
          "Data files/Formatted data files/energy_and_steel.csv",
          row.names = FALSE)

