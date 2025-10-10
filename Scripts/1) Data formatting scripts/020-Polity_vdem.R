
# Uses V-Dem data to calculate polity


### load libraries ---------------------------------------------------------------------------------
library(readxl)
library(countrycode)
library(dplyr)
library(tidyverse)


### not in function --------------------------------------------------------------------------------

'%!in%' <- function(x,y)!('%in%'(x,y))


### load data file ---------------------------------------------------------------------------------

# core VDEM dataset
v_dem <- read.csv("Data files/Raw data files/V-Dem-CY-Core-v14.csv")

country_years <- read.csv("Data files/Formatted data files/country_years.csv")


### data formatting --------------------------------------------------------------------------------

# 5 high-level democracy indices
vdem <- v_dem %>%
  dplyr::select(country_name, year, v2x_polyarchy, v2x_libdem, v2x_delibdem, v2x_partipdem,
                v2x_egaldem) %>%
  dplyr::filter(year >= 1945) %>%
  
  # convert to standardized iso3c codes
  dplyr::mutate(
    iso3c = dplyr::case_when(
      country_name == "German Democratic Republic" ~ "DDR",
      country_name == "Germany" & year < 1990 ~ "BRD",
      country_name == "Germany" & year >= 1990 ~ "DEU",
      country_name == "Kosovo" ~ "KSV",
      country_name == "Republic of Vietnam" ~ "RVN",
      country_name == "Somaliland" ~ "SLD",
      country_name == "Yemen" & year < 1991 ~ "YAR",
      country_name == "Yemen" & year >= 1991 ~ "YEM",
      country_name == "South Yemen" ~ "YPR",
      country_name == "Zanzibar" ~ "ZAN",
      country_name == "Serbia" & year < 1992 ~ "YUG",
      country_name == "Serbia" & year >= 1992 ~ "SRB",
      country_name == "Russia" & year < 1991 ~ "SOV",
      country_name == "Russia" & year >= 1991 ~ "RUS",
      .default = countrycode::countrycode(country_name, "country.name", "iso3c")
    ),
    
    # code 0 for BHR's and CAF's (1964-65) libdem indicator
    v2x_libdem = dplyr::case_when(
      is.na(v2x_libdem) & iso3c %in% c("BHR", "CAF") ~ 0,
      .default = v2x_libdem
    )) %>%
  
  # join country-year data to remove pre-independent countries
  dplyr::left_join(country_years, by = c("iso3c", "year")) %>%
  dplyr::filter(cn == 1) %>%
  dplyr::select(-cn) %>%
  
  # reformatting
  dplyr::relocate(iso3c, .before = country_name) %>%
  dplyr::select(-country_name)

# missing data count
na_count <- sapply(vdem, function(y) sum(length(which(is.na(y)))))
na_count
# no missing values remaining


### PCA --------------------------------------------------------------------------------------------

# hl = high-level
vdem_hl_pca_vars <- c("v2x_polyarchy", "v2x_libdem", "v2x_delibdem", "v2x_partipdem", "v2x_egaldem")

vdem_hl_pca_df <- vdem %>%
  dplyr::select(iso3c, year, dplyr::all_of(vdem_hl_pca_vars)) %>%
  dplyr::filter(
    # filter out the same small countries filtered for other index values for consistency
    iso3c %!in% c(
      "DMA", "GRD", "LCA", "VCT", "ATG", "KNA", "MCO", "LIE", "AND", "SMR", "ISL", "MDV",
      "VUT", "SLB", "KIR", "TUV", "TON", "NRU", "MHL", "PLW", "FSM", "WSM", "PSE",
      "BHS", "BRB", "BRN", "BTN", "CPV", "MUS", "SYC", "STP", "ZAN" ###
    )
  ) %>%
  tidyr::drop_na(dplyr::all_of(vdem_hl_pca_vars))

# correlation matrix between high-level variables
vdem_hl_pca_cor <- cor(vdem_hl_pca_df %>%
                         dplyr::select(dplyr::all_of(vdem_hl_pca_vars)))

vdem_hl_pca <- stats::prcomp(
  ~ v2x_polyarchy + v2x_libdem + v2x_delibdem + v2x_partipdem + v2x_egaldem,
  data = vdem_hl_pca_df, retx = T, center = T, scale. = T)
summary(vdem_hl_pca)

vdem_hl_index <- vdem_hl_pca_df %>%
  # combines principal component scores with PCA dataset
  cbind(vdem_hl_pca[["x"]]) %>%
  # drops 2nd-5th principal component scores
  dplyr::select(-c(PC2, PC3, PC4, PC5)) %>%
  # renames first principal component as mil.cap
  dplyr::rename(vdem.hl = PC1) %>%
  # creates vdem.hl.sq variable, the square of vdem.hl
  # this tests for extremes of vdem polity - extremely strong/weak vs. average capacity
  dplyr::mutate(vdem.hl.sq = vdem.hl^2)

# test directionality of metric
# invert direction if CHE 2017 is negative - higher value is higher capacity.
if(vdem_hl_index$vdem.hl[vdem_hl_index$iso3c=="CHE"&vdem_hl_index$year==2017]<1){
  vdem_hl_index <- vdem_hl_index %>%
    dplyr::mutate(vdem.hl = -1 * vdem.hl)
}

### write data -------------------------------------------------------------------------------------
# writes formatted dataframe as csv files
write.csv(vdem_hl_index, "Data files/Formatted data files/vdem_hl_index.csv", row.names = FALSE)

