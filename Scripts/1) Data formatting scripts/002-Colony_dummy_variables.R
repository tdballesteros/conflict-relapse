# This script takes an Excel spreadsheet coding each country into dummy variables relating to colonialism.

# The main "colony" dummy variable codes countries that were previously colonies (not parts of empries, such
# as parts of Ottoman or Russian/Soviet countries).

# Additional dummy variables are coded for whether a country was a colony of a given empire:
# the United Kingdom (GBR), France (FRA), Spain (ESP), Portugal (PRT), Netherlands (NLD), Italy (ITA),
# Belgium (BEL), the United States (USA), or other countries (e.g., Australia, New Zealand). The code is
# for the last colonial power to control the territory; for example, the Philippines is coded as an
# American colony, as the United States was the last country to administer the territory prior to its
# independence, despite Spain also controling the territory prior to the United States.

### load libraries ----------------------------------------------------------------------
library(readxl)
library(countrycode)
library(dplyr)

### load data file ----------------------------------------------------------------------
colonialism <- readxl::read_xlsx("Data files/Raw data files/colonialism.xlsx") 

### data formatting ----------------------------------------------------------------------
colonialism <- colonialism %>%
  # fills in 0s for blank cells
  dplyr::mutate(across(everything(), .fns = ~replace_na(.,0))) %>%
  # using the countrycode package, add country name based on iso3c value
  dplyr::mutate(country = countrycode::countrycode(iso3c,origin="iso3c",destination="country.name")) %>%
  dplyr::relocate(country, .after = iso3c)

# manually add country names to missing iso3c codes: BRD, DDR, KSV, RVN, SOV, YAR, YPR, YUG 
colonialism$country[colonialism$iso3c=="BRD"] <- "West Germany"
colonialism$country[colonialism$iso3c=="DDR"] <- "East Germany"
colonialism$country[colonialism$iso3c=="KSV"] <- "Kosovo"
colonialism$country[colonialism$iso3c=="RVN"] <- "South Vietnam"
colonialism$country[colonialism$iso3c=="SOV"] <- "Soviet Union"
colonialism$country[colonialism$iso3c=="YAR"] <- "North Yemen"
colonialism$country[colonialism$iso3c=="YPR"] <- "South Yemen"
colonialism$country[colonialism$iso3c=="YUG"] <- "Yugoslavia"
colonialism$country[colonialism$iso3c=="ZAN"] <- "Zanzibar"

# create a factor variable with colonizer names
colonialism_factor <- colonialism %>%
  dplyr::select(-c(country,colony)) %>%
  tidyr::pivot_longer(2:10, names_to = "colonizer", values_to = "binary") %>%
  dplyr::filter(binary == 1) %>%
  dplyr::select(-binary) %>%
  dplyr::mutate(
    colonizer = dplyr::case_match(
      colonizer,
      "colony_gbr" ~ "Colony of GBR",
      "colony_fra" ~ "Colony of FRA",
      "colony_esp" ~ "Colony of ESP",
      "colony_prt" ~ "Colony of PRT",
      # Collapsing BEL, ITA, NLD, and USA into other
      "colony_nld" ~ "Colony of another country",
      "colony_ita" ~ "Colony of another country",
      "colony_bel" ~ "Colony of another country",
      "colony_usa" ~ "Colony of another country",
      "colony_other" ~ "Colony of another country"
    )
  )

# merge factor variable into main dataframe
colonialism <- colonialism %>%
  dplyr::left_join(colonialism_factor,by="iso3c") %>%
  # repalce NA for non-colonized countries
  dplyr::mutate(colonizer = dplyr::case_when(
    is.na(colonizer) ~ "Not colonized",
    .default = colonizer
  ))

### write data ----------------------------------------------------------------------
# writes formatted dataframe as csv files
write.csv(colonialism,"Data files/Formatted data files/colonialism.csv",row.names = FALSE)

### codebook ----------------------------------------------------------------------
# iso3c
### A country's standardized iso3c code, with non-standard codes for West Germany, East Germany, North Yemen, South Yemen,
### South Vietnam, the Netherlands Antilles, the Soviet Union, Yugoslavia, and Zanzibar.
# country
### A country's commonly used English-language name.
## colony
### A dummy variable coding whether the country was a colony of a European/other Western country in its history.
## colony_gbr
### A dummy variable coding whether the last colonial power prior to independence was the United Kingdom.
## colony_fra
### A dummy variable coding whether the last colonial power prior to independence was France.
## colony_esp
### A dummy variable coding whether the last colonial power prior to independence was Spain.
## colony_prt
### A dummy variable coding whether the last colonial power prior to independence was Portugal.
## colony_nld
### A dummy variable coding whether the last colonial power prior to independence was the Netherlands.
## colony_ita
### A dummy variable coding whether the last colonial power prior to independence was Italy.
## colony_bel
### A dummy variable coding whether the last colonial power prior to independence was Belgium.
## colony_usa
### A dummy variable coding whether the last colonial power prior to independence was the United States
## colony_other
### A dummy variable coding whether the last colonial power prior to independence was another Western country
### or a special arrangement of colonial powers.
## colonizer
### A variable specifying if the country was a colony of the United Kingdom, France, Spain, Portugal, another country,
### or not colonized.
