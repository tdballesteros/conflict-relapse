
# This script formats land area in square kilometers by country-year.


### load libraries ---------------------------------------------------------------------------------

library(readxl)
library(countrycode)
library(tibble)
library(dplyr)
library(tidyr)


### not in function --------------------------------------------------------------------------------

'%!in%' <- function(x,y)!('%in%'(x,y))


### load data files --------------------------------------------------------------------------------

# unformatted land area data
land_area <- read.csv("Data files/Raw data files/land-area-km.csv")

# formatted population data
population <- read.csv("Data files/Formatted data files/population.csv")

# country-year data
cyears <- utils::read.csv("Data files/Formatted data files/country_years.csv")

### format data ------------------------------------------------------------------------------------

land_area <- land_area %>%
  dplyr::rename(
    land_area = `Land.area..sq..km.`,
    year = Year
    ) %>%
  # remove non-sovereign states and groupings
  dplyr::filter(
    # filter out region groupings
    Entity %!in% c(
      "East Asia and Pacific (WB)", "Europe and Central Asia (WB)", "European Union (27)",
      "High-income countries", "Latin America and Caribbean (WB)", "Low-income countries",
      "Lower-middle-income countries", "Middle East and North Africa (WB)",
      "Middle-income countries", "North America (WB)", "South Asia (WB)", "Sub-Saharan Africa (WB)",
      "Upper-middle-income countries", "World"
    )) %>%
  # using the countrycode package, add iso3c code based on country name
  dplyr::mutate(iso3c = dplyr::case_when(
    Entity %in% c("Channel Islands", "Turks and Caicos Islands", "Isle of Man", "Gibraltar",
                  "Cayman Islands", "British Virgin Islands", "Bermuda") ~ "GBR",
    Entity %in% c("United States Virgin Islands", "Northern Mariana Islands", "Guam",
                  "American Samoa", "Puerto Rico") ~ "USA",
    Entity %in% c("Sint Maarten (Dutch part)", "Curacao", "Aruba") ~ "NLD",
    Entity %in% c("Saint Martin (French part)", "New Caledonia", "French Polynesia") ~ "FRA",
    Entity %in% c("Macao", "Hong Kong") ~ "CHN",
    Entity %in% c("Greenland", "Faeroe Islands") ~ "DNK",
    Entity == "Micronesia (country)" ~ "FSM",
    .default = countrycode::countrycode(Entity, "country.name", "iso3c")
    )) %>%
  dplyr::select(iso3c, year, land_area) %>%
  # collapse estimates
  dplyr::group_by(iso3c, year) %>%
  dplyr::summarise(land_area = sum(land_area, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  # merge in cyears data
  dplyr::full_join(cyears, by = c("iso3c", "year")) %>%
  # drop entries that both have no data and are not coded as in the system in a given year
  dplyr::filter(
    !is.na(land_area) | cn == 1,
    year <= 2019)

### Calculate Estimates ----------------------------------------------------------------------------

#### CZE/SVK ---------------------------------------------------------------------------------------
# Data begins in 1993 for Czechia and Slovakia.

# Assume Czechoslovakia had combined territory of Czechia and Slovakia based on 1993 data
cze.1993 <- land_area$land_area[land_area$iso3c == "CZE" & land_area$year == 1993]
svk.1993 <- land_area$land_area[land_area$iso3c == "SVK" & land_area$year == 1993]

czechoslovakia_area_estimate <- cze.1993 + svk.1993

land_area$land_area[land_area$iso3c == "CZE" &
                      land_area$year %in% c(1961:1992)] <- czechoslovakia_area_estimate

#### DEU/BRD/DDR -----------------------------------------------------------------------------------
# Data begins 1961 for (combined) Germany.

# East and West German area per World Inequality Database (in square kilometers)
brd.area <- 248717
ddr.area <- 108333

# calculate proportions of total German area within DDR and BRD to apply to estimates in dataset
brd.area.proportion <- brd.area / (brd.area + ddr.area)
ddr.area.proportion <- ddr.area / (brd.area + ddr.area)

# data for DEU exists starting in 1961
for(d in c(1961:1989)){
  
  # calculate estimates
  brd.est <- land_area$land_area[land_area$iso3c == "DEU" & land_area$year == d] * brd.area.proportion
  ddr.est <- land_area$land_area[land_area$iso3c == "DEU" & land_area$year == d] * ddr.area.proportion
  
  # add estimates to df
  land_area$land_area[land_area$iso3c == "BRD" & land_area$year == d] <- brd.est
  land_area$land_area[land_area$iso3c == "DDR" & land_area$year == d] <- ddr.est
  
}

# for 1954 (DDR)/1955 (BRD) - 1960, use 1961 DDR and BRD estimates
land_area$land_area[land_area$iso3c == "BRD" &
                      land_area$year %in% c(1955:1960)] <- land_area$land_area[land_area$iso3c == "BRD" &
                                                                                 land_area$year == 1961]
land_area$land_area[land_area$iso3c == "DDR"
                    & land_area$year %in% c(1954:1960)] <- land_area$land_area[land_area$iso3c == "DDR" &
                                                                                 land_area$year == 1961]

#### ISR/PSE(*) ------------------------------------------------------------------------------------
# Israel and Palestine data starts 1961 and is constant.

# These values appear to be for internationally-recognized Israeli territory and Palestinian
# territory

#### MYS/SGP(*) ------------------------------------------------------------------------------------
# Malaya not coded as united in data.

#### SDN/SSD ---------------------------------------------------------------------------------------
# South Sudan 2011 not coded.

# Remove 2011 SDN coding and replace with 2012 SDN and SSD coding.
land_area$land_area[land_area$iso3c == "SDN" &
                      land_area$year == 2011] <- land_area$land_area[land_area$iso3c == "SDN" &
                                                                       land_area$year == 2012]
land_area$land_area[land_area$iso3c == "SSD" &
                      land_area$year == 2011] <- land_area$land_area[land_area$iso3c == "SSD" &
                                                                       land_area$year == 2012]

#### SOV/RUS ---------------------------------------------------------------------------------------
# Soviet Union not coded and post-Soviet countries only coded beginning 1992.

# Assume Soviet Union had combined territory of successor states based on
# 1992 data and assume successor states had same territory in 1991 as 1992.

sov_area <- sum(land_area$land_area[land_area$iso3c %in% c("EST", "LVA", "LTU", "BLR", "UKR", "MDA",
                                                           "RUS", "GEO", "ARM", "AZE", "KAZ", "KGZ",
                                                           "TKM", "TJK", "UZB") &
                                      land_area$year == 1992])

land_area$land_area[land_area$iso3c == "SOV" & land_area$year %in% c(1946:1990)] <- sov_area

# impute 1991 area as 1992 values for Soviet successor states
for(s in c("EST", "LVA", "LTU", "BLR", "UKR", "MDA", "RUS", "GEO", "ARM", "AZE", "KAZ", "KGZ",
           "TKM", "TJK", "UZB")){
  
  area.est <- land_area$land_area[land_area$iso3c == s & land_area$year == 1992]
  
  land_area$land_area[land_area$iso3c == s & land_area$year == 1991] <- area.est
  
}
  
#### YUG/SRB/MNE/KSV -------------------------------------------------------------------------------
# Yugoslavia not coded and post-Yugoslav countries only coded beginning 1992. 

# SRB appears to include Kosovo, so pre-2006 Serbia and Montenegro is the sum of their 2006 areas

srb.area <- land_area$land_area[land_area$iso3c == "SRB" & land_area$year == 2006]
mne.area <- land_area$land_area[land_area$iso3c == "MNE" & land_area$year == 2006]

land_area$land_area[land_area$iso3c == "SRB" &
                      land_area$year %in% c(1992:2005)] <- srb.area + mne.area

# Serbia/Kosovo
srb.area.total <- 88499
srb.area.exlcuding.ksv <- 77589

srb.area.proportion <- srb.area.exlcuding.ksv / srb.area.total
ksv.area.proportion <- 1 - srb.area.proportion

# apply estimates
land_area$land_area[land_area$iso3c == "KSV" & land_area$year %in% c(2008:2019)] <- ksv.area.proportion *
  land_area$land_area[land_area$iso3c == "SRB" & land_area$year %in% c(2008:2019)]

land_area$land_area[land_area$iso3c == "SRB" & land_area$year %in% c(2008:2019)] <- srb.area.proportion *
  land_area$land_area[land_area$iso3c == "SRB" & land_area$year %in% c(2008:2019)]
# Yugoslavia: assume area is constant based on post-Yugoslav area in 1992

land_area.yug <- sum(land_area$land_area[land_area$iso3c %in% c("SVN", "HRV", "BIH", "MKD", "SRB") &
                                           land_area$year == 1992])

land_area$land_area[land_area$iso3c == "YUG" & land_area$year %in% c(1961:1991)] <- land_area.yug

#### TWN(*) ----------------------------------------------------------------------------------------
# Taiwan not coded.
  
#### TZA/ZAN(*) ------------------------------------------------------------------------------------
# Tanganyika and Zanzibar not coded separately.

#### YEM/YAR/YPR -----------------------------------------------------------------------------------
# Data begins 1961 for (combined) Yemen

# Approximate North Yemeni territory as 35% of total Yemeni territory and South Yemeni
# territory as 65% through 1989.

# pull 1991 YEM area
yem.1991.area <- land_area$land_area[land_area$iso3c == "YEM" & land_area$year == 1991]

land_area$land_area[land_area$iso3c == "YAR" &
                      land_area$year %in% c(1946:1990)] <- 0.35 * yem.1991.area
land_area$land_area[land_area$iso3c == "YPR" &
                      land_area$year %in% c(1946:1990)] <- 0.52 * yem.1991.area

#### VNM/RVN ---------------------------------------------------------------------------------------
# Data begins 1961 for (combined) Vietnam.

# North and South Vietnam (in square kilometers)
vnm.area <- 157880
rvn.area <- 173809

# calculate proportions of total German area within DDR and BRD to apply to estimates in dataset
vnm.area.proportion <- vnm.area / (vnm.area + rvn.area)
rvn.area.proportion <- rvn.area / (vnm.area + rvn.area)

# pull 1976 VNM area
vnm.1976.area <- land_area$land_area[land_area$iso3c == "VNM" & land_area$year == 1976]

land_area$land_area[land_area$iso3c == "VNM" &
                      land_area$year %in% c(1954:1975)] <- vnm.area.proportion * vnm.1976.area
land_area$land_area[land_area$iso3c == "RVN" &
                      land_area$year %in% c(1954:1975)] <- rvn.area.proportion * vnm.1976.area


### Pre-1961 Values --------------------------------------------------------------------------------

#### Constant --------------------------------------------------------------------------------------

# Countries where pre-1961 values are assumed constant from their 1961 values.

# Non-Colony countries non-extant prior to 1961:
# Post-Soviet countries: ARM, AZE, BLR, EST, GEO, KAZ, KGZ, LTU, LVA, MDA, RUS, TJK, TKM, UKR, UZB
# Post-Yugoslav countries: BIH, HRV, KSV, MKD, MNE, SRB, SVN
# Other: DEU, SSD, SVK, YEM

# PAK/BGD

land_area_constant <- c(
  "AFG", "AGO", "ALB", "AND", "ARE", "ARG", "ATG", "AUS", "AUT", "BDI", "BEL", "BEN", "BFA",
  "BGR", "BHR", "BHS", "BLZ", "BOL", "BRA", "BRB", "BRD", "BRN", "BTN", "BWA", "CAF", "CAN",
  "CHE", "CHL", "CHN", "CIV", "CMR", "COD", "COG", "COL", "COM", "CPV", "CRI", "CUB", "CYP",
  "CZE", "DDR", "DJI", "DMA", "DNK", "DOM", "DZA", "ECU", "EGY", "ERI", "ESP", "ETH", "FIN",
  "FJI", "FRA", "FSM", "GAB", "GBR", "GHA", "GIN", "GMB", "GNB", "GNQ", "GRC", "GRD", "GTM",
  "GUY", "HND", "HTI", "HUN", "IDN", "IND", "IRL", "IRN", "IRQ", "ISL", "ISR", "ITA", "JAM",
  "JOR", "JPN", "KEN", "KHM", "KIR", "KNA", "KOR", "KWT", "LAO", "LBN", "LBR", "LBY", "LCA",
  "LIE", "LKA", "LSO", "LUX", "MAR", "MCO", "MDG", "MDV", "MEX", "MHL", "MLI", "MLT", "MMR",
  "MNG", "MOZ", "MRT", "MUS", "MWI", "MYS", "NAM", "NER", "NGA", "NIC", "NLD", "NOR", "NPL",
  "NRU", "NZL", "OMN", "PAN", "PER", "PHL", "PLW", "PNG", "POL", "PRK", "PRT", "PRY", "PSE",
  "QAT", "ROU", "RVN", "RWA", "SAU", "SDN", "SEN", "SGP", "SLB", "SLE", "SLV", "SMR", "SOM",
  "SOV", "STP", "SUR", "SWE", "SWZ", "SYC", "SYR", "TCD", "TGO", "THA", "TLS", "TON", "TTO",
  "TUN", "TUR", "TUV", "TWN", "TZA", "UGA", "URY", "USA", "VCT", "VEN", "VNM", "VUT", "WSM",
  "YAR", "YPD", "YUG", "ZAF", "ZMB", "ZWE"
)

for(i in land_area_constant){
  
  # pull 1961 area
  area.1961 <- land_area$land_area[land_area$iso3c == i & land_area$year == 1961]
  
  # add value to df
  land_area <- land_area %>%
    dplyr::mutate(land_area = ifelse(iso3c == i & year %in% c(1946:1960),
                                     area.1961,
                                     land_area))

}

#### PAK/BGD ---------------------------------------------------------------------------------------
# Data starts in 1961, the year Bangladesh gained independence. Reconstruct pre-1961 Pakistan by
# combining 1961 PAK and BGD values.

land_area.pak <- land_area$land_area[land_area$iso3c == "PAK" & land_area$year == 1961]
land_area.bgd <- land_area$land_area[land_area$iso3c == "BGD" & land_area$year == 1961]

land_area$land_area[land_area$iso3c == "PAK" & land_area$year %in% c(1946:1960)] <- land_area.pak +
                                                                                      land_area.bgd


### Land per Capita (Population Density) -----------------------------------------------------------

land_area <- land_area %>%
  dplyr::select(-country) %>%
  dplyr::full_join(population,by = c("iso3c", "year")) %>%
  dplyr::mutate(
    pop.per.km.un = un.pop / land_area,
    pop.per.km.cow = cow.pop / land_area
  ) %>%
  dplyr::select(-c(country, un.pop, cow.pop, pop.growth.rate.un, pop.growth.rate.cow))


landarea <- land_area %>%
  dplyr::group_by(iso3c, year) %>%
  dplyr::tally() %>%
  dplyr::filter(n > 1)


### write data -------------------------------------------------------------------------------------
# writes formatted dataframe as csv files
write.csv(land_area, "Data files/Formatted data files/population_density.csv", row.names = FALSE)


### codebook ---------------------------------------------------------------------------------------
# TBD
