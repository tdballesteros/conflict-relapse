
# This script assesses possible data discrepencies and inconsistencies with missing population
# data.


### load libraries ---------------------------------------------------------------------------------
library(readxl)
library(countrycode)
library(tibble)
library(readr)
library(dplyr)
library(tidyr)


### not in function --------------------------------------------------------------------------------
'%!in%' <- function(x,y)!('%in%'(x,y))


### load formatted data ----------------------------------------------------------------------------
pd_data <- readr::read_csv("Data files/Formatted data files/population.csv")


### Population Estimate Differences ----------------------------------------------------------------
pd_differences <- pd_data %>%
  dplyr::mutate(
    pop_differences = abs(un.pop - cow.pop),
    pop_differences_relative_un = pop_differences / un.pop,
    pop_differences_relative_cow = pop_differences / cow.pop,
    pop_growth_differences = abs(pop.growth.rate.un - pop.growth.rate.cow)
  )

# pull large proportional population differences
pd_differences_large <- pd_differences %>%
  dplyr::filter(
    pop_differences_relative_un > 0.1 | pop_differences_relative_cow > 0.1,
    # filter out non-estimated country-years
    un.pop.estimated == 1 | cow.pop.estimated == 1)


### Test 1 - SE Asia -------------------------------------------------------------------------------
# Due to the Vietnam War, its spillover, and coterminous wars in the region, make sure the
# population reflects the regional dynamics accurately. Use Thailand as a control.

pd_vnm <- pd_data %>% dplyr::filter(iso3c %in% c("VNM","RVN"))

# group Vietnam (combined) by year to show united Vietnam's population during the war
pd_vnm_combined <- pd_vnm %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(
    un.pop = sum(un.pop, na.rm = TRUE),
    cow.pop = sum(cow.pop, na.rm = TRUE)
    ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    iso3c = "VNM",
    country = "Vietnam (total)"
    )

pd_lao <- pd_data %>% dplyr::filter(iso3c == "LAO")
pd_khm <- pd_data %>% dplyr::filter(iso3c == "KHM")
pd_tha <- pd_data %>% dplyr::filter(iso3c == "THA")


