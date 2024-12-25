
### load libraries ----------------------------------------------------------------------
library(readxl)
library(countrycode)
library(tibble)
library(readr)
library(dplyr)
library(tidyr)

### not in function ----------------------------------------------------------------------
'%!in%' <- function(x,y)!('%in%'(x,y))

### load formatted data ----------------------------------------------------------------------
pd_data <- readr::read_csv("Data files/Formatted data files/population.csv")


### Test 1 - SE Asia ----------------------------------------------------------------------
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


