
# This script formats two peacekeeping operation variables, indicating whether a mission to a
# country is ongoing and whether troops are deployed in a mission to a country.

# https://psdata.un.org/dataset/DPPADPOSS-PKO

### load libraries ---------------------------------------------------------------------------------
library(readxl)
library(countrycode)
library(tibble)
library(dplyr)

### load data file ---------------------------------------------------------------------------------
pko <- read.csv("Data files/Raw data files/DPPADPOSS-PKO.csv")
pko_crosswalk <- readxl::read_xlsx("Data files/Other/unpko_country_crosswalk.xlsx")

# load formatted data output by script 003-Country_years
country_years <- read.csv("Data files/Formatted data files/country_years.csv")

### format data ------------------------------------------------------------------------------------
# add country crosswalk to pko dataset
# this results in each pko-country having one entry in the dataset
pko <- pko %>%
  dplyr::full_join(pko_crosswalk, by = c("mission_acronym", "mission_name")) %>%
  dplyr::mutate(
    start_year = as.numeric(stringr::str_sub(start_date, start = 1, end = 4)),
    end_year = as.numeric(stringr::str_sub(end_date, start = 1, end = 4))
    ) %>%
  dplyr::select(iso3c, mission_acronym, mission_name, start_year, end_year)

# add missing missions: UNTAG, UNTMIH, UNCI, UNAMET
pko <- pko %>%
  # add UNTAG - mission in NAM prior to independence; code for both NAM and ZAF
  tibble::add_row(iso3c = "ZAF",
                  mission_acronym = "UNTAG",
                  mission_name = "United Nations Transition Assistance Group",
                  start_year = 1989,
                  end_year = 1990) %>%
  tibble::add_row(iso3c = "NAM",
                  mission_acronym = "UNTAG",
                  mission_name = "United Nations Transition Assistance Group",
                  start_year = 1989,
                  end_year = 1990) %>%
  # add UNTMIH - United Nations Transition Mission in Haiti
  tibble::add_row(iso3c = "HTI",
                  mission_acronym = "UNTMIH",
                  mission_name = "United Nations Transition Mission in Haiti",
                  start_year = 1997,
                  end_year = 1997) %>%
  # add UNCI - mission overseeing transfer of Dutch East Indies to Indonesia
  tibble::add_row(iso3c = "IDN",
                  mission_acronym = "UNCI",
                  mission_name = "United Nations Commission for Indonesia",
                  start_year = 1947,
                  end_year = 1950) %>%
  # add UNAMET - United Nations Mission in East Timor; TLS coded as independent starting in 2002
  tibble::add_row(iso3c = "IDN",
                  mission_acronym = "UNAMET",
                  mission_name = "United Nations Mission in East Timor",
                  start_year = 1947,
                  end_year = 1950)

# add start year for UNSCOB
pko$start_year[pko$mission_acronym == "UNSCOB"] <- 1947

# ongoing pko (not concluded within 2019 or prior)
ongoing_pko <- c("UNAMID", "MINUSMA", "MINURSO", "MONUSCO", "UNISFA", "UNMISS", "MINUSCA",
                 "UNMOGIP", "UNFICYP", "UNMIK", "UNTSO", "UNDOF", "UNIFIL")

# code "end_year" as 2019 for ongoing missions to construct list of active years
pko <- pko %>%
  dplyr::mutate(end_year = ifelse(mission_acronym %in% ongoing_pko, 2019, end_year)) %>%
  # drop pkos without an iso3c code associated; this is mostly due to extra coding of missions with
  # multiple parts having an overall mission entry (e.g., UNOSOM I and UNOSOM II also coded as just
  # UNOSOM encompassing both missions)
  tidyr::drop_na(iso3c) %>%
  # drop missions with no start or end date
  tidyr::drop_na(start_year, end_year)

pko2 <- data.frame()

for(r in 1:nrow(pko)){
  
  years <- c(pko$start_year[r]:pko$end_year[r])
  
  tmp <- data.frame(iso3c = rep(pko$iso3c[r], length(years)),
                    year = years)
  
  pko2 <- rbind(pko2, tmp)
  
}

pko2 <- pko2 %>%
  unique() %>%
  dplyr::mutate(pko_mission = 1,
                # using the countrycode package, add country name based on iso3c code
                country = countrycode::countrycode(iso3c, "iso3c", "country.name")) %>%
  dplyr::relocate(country, .after = iso3c)

# add country names for missing iso3c codes
pko2$country[pko2$iso3c == "KSV"] <- "Kosovo"
pko2$country[pko2$iso3c == "YAR"] <- "North Yemen"

### merge all countries ----------------------------------------------------------------------------
country_years <- country_years %>%
  # filter so only countries that are sovereign in a given year are in the dataset
  dplyr::filter(cn == 1) %>%
  dplyr::select(-country)

pko2 <- pko2 %>%
  dplyr::full_join(country_years, by = c("iso3c","year")) %>%
  dplyr::select(-c(cn, country, yrs_since_indep)) %>%
  # code countries who did not have a peacekeeping mission
  dplyr::mutate(pko_mission = ifelse(is.na(pko_mission), 0, pko_mission)) %>%
  dplyr::arrange(iso3c, year)

### write data ----------------------------------------------------------------------
# writes formatted dataframe as csv files
write.csv(pko2, "Data files/Formatted data files/peacekeeping_operations.csv", row.names = FALSE)

### codebook ----------------------------------------------------------------------
# iso3c
### A country's standardized iso3c code, with non-standard codes for West Germany, East Germany,
### North Yemen, South Yemen, South Vietnam, the Soviet Union, and Yugoslavia.
# country
### A country's commonly used English-language name.
# year
### The calendar year the specific variable is measured during.
# pko_mission
### A binary variable representing if a United Nations or joint United Nations peacekeeping mission
# operated within the country in a given year.
