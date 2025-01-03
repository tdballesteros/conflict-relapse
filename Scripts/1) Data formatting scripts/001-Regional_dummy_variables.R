# This script takes two Excel spreadsheets coding each country into a region and subregion. The subregions fit
# within regions, save for WEOG, which is split across Americas, Asia, and Europe.

# country_regions.xlsx codes countries into 5 Continent-Region levels
## Levels: Americas, Asia, Europe, MENA (Middle East & North Africa), and SSA (Sub-Saharan Africa)

# country_regions2.xlsx codes countries into 15 subregional levels
## Levels: WEOG (Western Europe & Others Group), Central America, Caribbean, South America, Eastern Europe,
## Middle East, North Africa, East Africa, West Africa, Central Africa, Southern Africa, Central Asia,
## South Asia, East Asia, and Southeast Asia

### load libraries ----------------------------------------------------------------------
library(readxl)
library(countrycode)
library(dplyr)

### load data files ----------------------------------------------------------------------
country_regions1 <- readxl::read_xlsx("Data files/Raw data files/country_regions.xlsx") 
country_regions2 <- readxl::read_xlsx("Data files/Raw data files/country_regions2.xlsx")

### data formatting ----------------------------------------------------------------------
country_regions1 <- country_regions1 %>%
  # fills in 0s for blank cells
  dplyr::mutate(across(everything(), .fns = ~replace_na(.,0))) %>%
  # using the countrycode package, add country name based on iso3c value
  dplyr::mutate(country = countrycode::countrycode(iso3c,origin="iso3c",destination="country.name")) %>%
  dplyr::relocate(country, .after = iso3c)

country_regions2 <- country_regions2 %>%
  # fills in 0s for blank cells
  dplyr::mutate(across(everything(), .fns = ~replace_na(.,0))) %>%
  # using the countrycode package, add country name based on iso3c value
  dplyr::mutate(country = countrycode::countrycode(iso3c,origin="iso3c",destination="country.name")) %>%
  dplyr::relocate(country, .after = iso3c)

# manually add country names to missing iso3c codes: ANT, BRD, DDR, KSV, RVN, SOV, YAR, YPR, YUG
country_regions1$country[country_regions1$iso3c=="ANT"] <- "Netherlands Antilles"
country_regions1$country[country_regions1$iso3c=="BRD"] <- "Germany West"
country_regions1$country[country_regions1$iso3c=="DDR"] <- "Germany East"
country_regions1$country[country_regions1$iso3c=="KSV"] <- "Kosovo"
country_regions1$country[country_regions1$iso3c=="RVN"] <- "South Vietnam"
country_regions1$country[country_regions1$iso3c=="SOV"] <- "Soviet Union"
country_regions1$country[country_regions1$iso3c=="YAR"] <- "Yemen North"
country_regions1$country[country_regions1$iso3c=="YPR"] <- "Yemen South"
country_regions1$country[country_regions1$iso3c=="YUG"] <- "Yugoslavia"

country_regions2$country[country_regions2$iso3c=="ANT"] <- "Netherlands Antilles"
country_regions2$country[country_regions2$iso3c=="BRD"] <- "Germany West"
country_regions2$country[country_regions2$iso3c=="DDR"] <- "Germany East"
country_regions2$country[country_regions2$iso3c=="KSV"] <- "Kosovo"
country_regions2$country[country_regions2$iso3c=="RVN"] <- "South Vietnam"
country_regions2$country[country_regions2$iso3c=="SOV"] <- "Soveit Union"
country_regions2$country[country_regions2$iso3c=="YAR"] <- "Yemen North"
country_regions2$country[country_regions2$iso3c=="YPR"] <- "Yemen South"
country_regions2$country[country_regions2$iso3c=="YUG"] <- "Yugoslavia"

### create version with factor variables ----------------------------------------------------------------------
# formats region1
country_regions3_1 <- country_regions1 %>%
  tidyr::pivot_longer(3:7, names_to = "region1", values_to = "region1_binary") %>%
  dplyr::filter(region1_binary == 1) %>%
  dplyr::select(-region1_binary)

# converts to factor
country_regions3_1$region1 <- factor(country_regions3_1$region1,
                                     levels = c("Americas", "Asia", "Europe", "MENA", "SSA"))

# formats region2
country_regions3_2 <- country_regions2 %>%
  tidyr::pivot_longer(3:17, names_to = "region2", values_to = "region2_binary") %>%
  dplyr::filter(region2_binary == 1) %>%
  dplyr::select(-region2_binary)

# converts to factor
country_regions3_2$region2 <- factor(country_regions3_2$region2,
                                     levels = c("WEOG", "Central America", "Caribbean", "South America", "Eastern Europe",
                                                "Middle East", "North Africa", "East Africa", "West Africa", "Central Africa",
                                                "Southern Africa", "Central Asia", "South Asia", "East Asia", "Southeast Asia"))

# merges into one dataset
country_regions3 <- dplyr::full_join(country_regions3_1,country_regions3_2,by=c("iso3c","country"))

### write data ----------------------------------------------------------------------
# writes formatted dataframes as csv files
write.csv(country_regions1,"Data files/Formatted data files/country_regions1.csv",row.names = FALSE)
write.csv(country_regions2,"Data files/Formatted data files/country_regions2.csv",row.names = FALSE)
write.csv(country_regions3,"Data files/Formatted data files/country_regions3.csv",row.names = FALSE)

### codebook ----------------------------------------------------------------------
#### country_regions1 ----------------------------------------------------------------------
# iso3c
### A country's standardized iso3c code, with non-standard codes for West Germany, East Germany, North Yemen, South Yemen,
### South Vietnam, the Netherlands Antilles, the Soviet Union, and Yugoslavia.
# country
### A country's commonly used English-language name.
# SSA
### A binary variable indicating whether the country is part of the sub-Saharan Africa region.
# MENA
### A binary variable indicating whether the country is part of the Middle East & North Africa region.
# Americas
### A binary variable indicating whether the country is part of the Americas region.
# Asia
### A binary variable indicating whether the country is part of the Asia-Pacific region.
# Europe
### A binary variable indicating whether the country is part of the Europe region.

#### country_regions2 ----------------------------------------------------------------------
# iso3c
### A country's standardized iso3c code, with non-standard codes for West Germany, East Germany, North Yemen, South Yemen,
### South Vietnam, the Netherlands Antilles, the Soviet Union, and Yugoslavia.
# country
### A country's commonly used English-language name.
# WEOG
### A binary variable indicating whether the country is part of the Western Europe & Others Group sub-region.
# Central America
### A binary variable indicating whether the country is part of the Central America sub-region.
# Caribbean
### A binary variable indicating whether the country is part of the Caribbean sub-region.
# South America
### A binary variable indicating whether the country is part of the South America sub-region.
# Eastern Europe
### A binary variable indicating whether the country is part of the Eastern Europe sub-region.
# Middle East
### A binary variable indicating whether the country is part of the Middle East sub-region.
# North Africa
### A binary variable indicating whether the country is part of the North Africa sub-region.
# East Africa
### A binary variable indicating whether the country is part of the East Africa sub-region.
# West Africa
### A binary variable indicating whether the country is part of the West Africa sub-region.
# Central Africa
### A binary variable indicating whether the country is part of the Central Africa sub-region.
# Southern Africa
### A binary variable indicating whether the country is part of the Southern Africa sub-region.
# Central Asia
### A binary variable indicating whether the country is part of the Central Asia sub-region.
# South Asia
### A binary variable indicating whether the country is part of the South Asia sub-region.
# East Asia
### A binary variable indicating whether the country is part of the East Asia sub-region.
# Southeast Asia
### A binary variable indicating whether the country is part of the Southeast Asia & Pacific sub-region.

#### country_regions3 ----------------------------------------------------------------------
# iso3c
### A country's standardized iso3c code, with non-standard codes for West Germany, East Germany, North Yemen, South Yemen,
### South Vietnam, the Netherlands Antilles, the Soviet Union, and Yugoslavia.
# country
### A country's commonly used English-language name.
# region1
### A variable containing which region a country is located in: Americas, Asia, Europe, MENA (Middle East & North Africa),
### and SSA (Sub-Saharan Africa).
# region2
### A variable containing which sub-region a country is located in: WEOG (Western Europe & Others Group), Central America,
### Caribbean, South America, Eastern Europe, Middle East, North Africa, East Africa, West Africa, Central Africa,
### Southern Africa, Central Asia, South Asia, East Asia, and Southeast Asia.
