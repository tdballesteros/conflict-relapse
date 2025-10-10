
# This script formats the Ethnic Fractionalization Index, which measures ethnic diversity within a
# country.

# Ethnic Fractionalization Index
## https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/4JQRCL


### load libraries ---------------------------------------------------------------------------------
library(readxl)
library(countrycode)
library(imputeTS)
library(dplyr)


### not in function --------------------------------------------------------------------------------
'%!in%' <- function(x,y)!('%in%'(x,y))


### load data file ---------------------------------------------------------------------------------
efi <- read.csv("Data files/Raw data files/HIEF_data.csv")

# load formatted population data
population <- read.csv("Data files/Formatted data files/population.csv")

### data formatting --------------------------------------------------------------------------------
efi <- efi %>%
  unique() %>%
  dplyr::mutate(
    # using the countrycode package, add iso3c code name based on country name
    iso3c = dplyr::case_when(
      Country == "Czechoslovakia" ~ "CZE",
      Country == "German Democratic Republic" ~ "DDR",
      Country == "Republic of Vietnam" ~ "RVN",
      Country == "Yemen Arab Republic" ~ "YAR",
      Country == "Yemen PDR" ~ "YPR",
      Country == "Yugoslavia" ~ "YUG",
      .default = countrycode::countrycode(Country, "country.name", "iso3c")
      ),
    # using the countrycode package, add country name based on iso3c value
    country = dplyr::case_when(
      iso3c == "DDR" ~ "East Germany",
      iso3c == "RVN" ~ "South Vietnam",
      iso3c == "YAR" ~ "North Yemen",
      iso3c == "YPR" ~ "South Yemen",
      iso3c == "YUG" ~ "Yugoslavia",
      .default = countrycode::countrycode(iso3c, "iso3c", "country.name")
    ))  %>%
  dplyr::select(iso3c, year = Year, ethf = EFindex)


### calculate missing countries / adjust values ----------------------------------------------------
#### Cameroon --------------------------------------------------------------------------------------

#### Czechoslovakia --------------------------------------------------------------------------------
# Czechoslovakia and Czechia coded as CZE; Slovakia coded as SVK beginning in 1993
# no changes required

#### Ethiopia/Eritrea(?) ---------------------------------------------------------------------------
# ERI coded beginning in 1993
# no change in Ethiopia's index score when Eritrea gains independence, so unclear on
# Eritrea's inclusion in ETH metrics before 1993

#### France ----------------------------------------------------------------------------------------

#### Germany ---------------------------------------------------------------------------------------
# West Germany and (unified) Germany coded as DEU 1949-2013; East Germany coded as DDR 1949-1990
# (unified) Germany coded as starting in 1990

# recode West Germany as BRD
efi$iso3c[efi$iso3c == "DEU" & efi$year < 1990] <- "BRD"

# remove East Germany 1990 coding, as this year is coded as having a unified Germany
efi <- efi %>%
  dplyr::filter(iso3c != "DDR" | year != 1990)

#### India -----------------------------------------------------------------------------------------
# load linguistic data from the 2011 census, using mother tongue as a stand-in for ethnic group
efi_ind <- readxl::read_xlsx("Data files/Raw data files/DDW-C16-STMT-MDDS-0000.XLSX") %>%
  dplyr::filter(`...5` == "INDIA") %>%
  dplyr::mutate(
    name_first_two = stringr::str_sub(`...7`, start = 1, end = 2),
    name_first_two = as.numeric(name_first_two),
    name_after_two = stringr::str_sub(`...7`, start = 3, end = -1),
    `...8` = as.numeric(`...8`)) %>%
  # filter out main language family groupings - these start with a number but do not include the
  # "# Other" formatted languages
  dplyr::filter(is.na(name_first_two) | name_after_two == "Others") %>%
  dplyr::select(`...7`, `...8`) %>%
  dplyr::mutate(perc = `...8` / sum(`...8`, na.rm = TRUE),
                perc_sq = perc^2)

ind_efi_est <- 1 - sum(efi_ind$perc_sq, na.rm = TRUE)

# add estimate to data
ind_efi_est_df <- data.frame(
  iso3c = rep("IND", 67),
  year = c(1947:2013),
  ethf = rep(ind_efi_est, 67)
)

efi <- efi %>%
  rbind(ind_efi_est_df)

#### Israel/Palestine ------------------------------------------------------------------------------

#### Malaysia/Singapore ----------------------------------------------------------------------------
# MYS and SGP coded separately throughout time series, though SGP data starts in 1960

#### Mozambique ------------------------------------------------------------------------------------

#### Pakistan/Bangladesh(x) ------------------------------------------------------------------------
# BGD coded beginning in 1971
# PAK data appears just to be coding (East) Pakistan

# pull 1950-1970 PAK (combined) and BGD (East Pakistan) populations (un.pop estimates)
pak_combined_pop <- population %>%
  dplyr::filter(iso3c == "PAK",
                year %in% c(1950:1970)) %>%
  dplyr::arrange(year) %>%
  dplyr::pull(un.pop)

bgd_pop <- population %>%
  dplyr::filter(iso3c == "BGD",
                year %in% c(1950:1970)) %>%
  dplyr::arrange(year) %>%
  dplyr::pull(un.pop)

bgd_pop_perc <- bgd_pop / pak_combined_pop
# assume same ratio for 1947-1949
bgd_pop_perc <- c(bgd_pop_perc[1], bgd_pop_perc[1], bgd_pop_perc[1], bgd_pop_perc)
pak_pop_perc <- 1 - bgd_pop_perc

# pull 1971 index scores
pak_1971 <- efi$ethf[efi$iso3c == "PAK" & efi$year == 1971]
bgd_1971 <- efi$ethf[efi$iso3c == "BGD" & efi$year == 1971]

# approximate 1947-1970 scores based on annual populations and 1971 index values, assuming
# proportion of Pakistani and Bengali ethnicities in each respective portion of united Pakistan
# are negligable

for(y in 1:24){
  
  est <- (pak_pop_perc[y] * pak_1971) + (bgd_pop_perc[y] * bgd_1971)
  
  # replace with new estimate
  efi$ethf[efi$iso3c == "PAK" & efi$year == y + 1946] <- est
  
}

#### Serbia/Montenegro/Kosovo ----------------------------------------------------------------------
efi_srb_formatted <- readxl::read_xlsx("Data files/Raw data files/Ethnic_Composition.xlsx",
                                       sheet = "SRB",
                                       skip = 1) %>%
  dplyr::select(ethnicity = `...1`, count = `Number...16`)

efi_mne_formatted <- readxl::read_xlsx("Data files/Raw data files/Ethnic_Composition.xlsx",
                                       sheet = "MNE") %>%
  dplyr::filter(Municipality == "Montenegro total") %>%
  dplyr::select(seq(from = 1, to = 56, by = 2)) %>%
  dplyr::mutate_if(is.character, as.numeric) %>%
  tidyr::pivot_longer(2:28, names_to = "ethnicity", values_to = "count") %>%
  dplyr::select(ethnicity, count)

efi_ksv_formatted <- readxl::read_xlsx("Data files/Raw data files/Ethnic_Composition.xlsx",
                                       sheet = "KSV",
                                       skip = 1) %>%
  dplyr::select(ethnicity = `...1`, count = `Number...14`)

## Serbia/Montenegro/Kosovo (1992-2005) estimation
srb_mne_ksv_efi <- rbind(efi_srb_formatted, efi_mne_formatted, efi_ksv_formatted) %>%
  dplyr::group_by(ethnicity) %>%
  dplyr::summarise(count = sum(count, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    perc = count / sum(count, na.rm = TRUE),
    perc_sq = perc^2)

srb_mne_ksv_efi_est = sum(srb_mne_ksv_efi$perc_sq, na.rm = TRUE)

## Serbia/Kosovo (2006-2007) estimation
srb_ksv_efi <- rbind(efi_srb_formatted, efi_ksv_formatted) %>%
  dplyr::group_by(ethnicity) %>%
  dplyr::summarise(count = sum(count, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    perc = count / sum(count, na.rm = TRUE),
    perc_sq = perc^2)

srb_ksv_efi_est = sum(srb_ksv_efi$perc_sq, na.rm = TRUE)

## Serbia
# load data from Wikipedia: https://en.wikipedia.org/wiki/Demographics_of_Serbia#Ethnic_groups
# note the data excludes Kosovo
efi_srb <- readxl::read_xlsx("Data files/Raw data files/Ethnic_Composition.xlsx",
                             sheet = "SRB",
                             skip = 1) %>%
  dplyr::select(`...1`, `Number...16`, `%...17`) %>%
  tidyr::drop_na(`%...17`) %>%
  dplyr::mutate(`%...17` = `%...17` / 100,
                sq = `%...17`^2)

srb_efi_est <- 1 - sum(efi_srb$sq, na.rm = TRUE)

## Montenegro
# load data from Wikipedia: https://en.wikipedia.org/wiki/Demographics_of_Montenegro
efi_mne <- readxl::read_xlsx("Data files/Raw data files/Ethnic_Composition.xlsx",
                             sheet = "MNE") %>%
  dplyr::filter(Municipality == "Montenegro total") %>%
  dplyr::select(seq(from = 1, to = 56, by = 2)) %>%
  dplyr::mutate_if(is.character, as.numeric) %>%
  tidyr::pivot_longer(2:28, names_to = "ethnic group", values_to = "population") %>%
  dplyr::mutate(perc = population / sum(population, na.rm = TRUE),
                perc_sq = perc^2)

mne_efi_est <- 1 - sum(efi_mne$perc_sq, na.rm = TRUE)

## Kosovo
# load data from Wikipedia: https://en.wikipedia.org/wiki/Demographics_of_Kosovo#Ethnic_groups
# note 2011 census was boycott by ethnic Serbs
efi_ksv <- readxl::read_xlsx("Data files/Raw data files/Ethnic_Composition.xlsx",
                             sheet = "KSV",
                             skip = 1) %>%
  dplyr::select(`...1`, `Number...14`, `%...15`) %>%
  tidyr::drop_na(`%...15`) %>%
  dplyr::mutate(`%...15` = `%...15` / 100,
                sq = `%...15`^2)

ksv_efi_est <- 1 - sum(efi_ksv$sq, na.rm = TRUE)

# add estimates
srb_mne_ksv_efi_df <- data.frame(
  iso3c = c(rep("SRB", 14), rep("SRB", 2), rep("MNE", 8), rep("SRB", 6), rep("KSV", 6)),
  year = c(1992:2005, 2006:2007, 2006:2013, 2008:2013, 2008:2013),
  ethf = c(rep(srb_mne_ksv_efi_est, 14), rep(srb_ksv_efi_est, 2), rep(mne_efi_est, 8),
           rep(srb_efi_est, 6), rep(ksv_efi_est, 6))
)

efi <- efi %>%
  dplyr::filter(iso3c != "SRB") %>%
  rbind(srb_mne_ksv_efi_df)

#### South Africa/Namibia(x) -----------------------------------------------------------------------
# NAM data starts in 1990
# ZAF data does not include NAM

#### Soviet Union/Russia(x) ------------------------------------------------------------------------
# Soviet Union coded as RUS 1945-1991; Russia coded as RUS 1991-2013
# Note there are two entries for 1991: Soviet Union and Russia
# Russia coded starting in 1991

# recode for Soviet Unio
efi$iso3c[efi$iso3c == "RUS" & efi$year <= 1990] <- "SOV"

# remove Soviet Union 1991 coding (i.e., the larger of the RUS 1991 values)
efi <- efi %>%
  dplyr::filter(iso3c != "RUS" | year != 1991 |
                  ethf != max(efi$ethf[efi$iso3c == "RUS" & efi$year == 1991], na.rm = TRUE))

#### Sudan/South Sudan -----------------------------------------------------------------------------
# Sudan value includes South Sudan for 2011-2013

## South Sudan
# load data from Wikipedia: https://en.wikipedia.org/wiki/List_of_ethnic_groups_in_South_Sudan
efi_ssd <- readxl::read_xlsx("Data files/Raw data files/Ethnic_Composition.xlsx",
                             sheet = "SSD") %>%
  dplyr::rename_with(tolower) %>%
  dplyr::mutate(perc = size.avg / sum(size.avg, na.rm = TRUE),
                perc_sq = perc^2) %>%
  tidyr::drop_na(perc)

ssd_efi_est <- 1 - sum(efi_ssd$perc_sq, na.rm = TRUE)

# add estimate to data
ssd_efi_est_df <- data.frame(
  iso3c = rep("SSD", 3),
  year = c(2011:2013),
  ethf = rep(ssd_efi_est, 3)
)

efi <- efi %>%
  rbind(ssd_efi_est_df)

#### Tanzania/Zanzibar -----------------------------------------------------------------------------
# Zanzibar data is missing
# TZA appears to be coded as Tanganyika+Zanzibar throughout

# adding ZAN data unlikely to impact data results as TZA is not coded as experiencing conflict

#### Yemen -----------------------------------------------------------------------------------------
# (unified) Yemen and North Yemen coded as YAR 1945-2013; South Yemen coded as YPR 1967-1990
# (unified) Yemen coded as starting in 1991

# recoded unified Yemen as YEM
efi$iso3c[efi$iso3c == "YAR" & efi$year >= 1991] <- "YEM"

#### Yugoslavia ------------------------------------------------------------------------------------
# HRV, MKD, SRB, SVN coded starting in 1991; BIH coded starting in 1992; YUG coded 1945-1990
# YUG coded as ending in 1991 (inclusive), with republics starting independence in 1992

# estimate YUG 1991 as YUG 1990
efi <- efi %>%
  tibble::add_row(
    iso3c = "YUG",
    year = 1991,
    ethf = efi$ethf[efi$iso3c == "YUG" & efi$year == 1990]
    ) %>%
  
  # filter out HRV, MKD, SRB, and SVN 1991 codes
  dplyr::filter(iso3c %!in% c("HRV", "MKD", "SRB", "SVN") | year != 1991)

#### Vietnam/South Vietnam(?) ----------------------------------------------------------------------
# RVN coded 1954-1975
# unclear whether VNM includes RVN during this time or not, though RVN has a lower index score than
# VNM would expect a drop in index score after reunification


### estimate values outside dataset ----------------------------------------------------------------
# 2014-2019: use 2013 estimates
efi <- efi %>%
  rbind(
    efi %>%
      dplyr::filter(year == 2013) %>%
      dplyr::mutate(year = 2014),
    efi %>%
      dplyr::filter(year == 2013) %>%
      dplyr::mutate(year = 2015),
    efi %>%
      dplyr::filter(year == 2013) %>%
      dplyr::mutate(year = 2016),
    efi %>%
      dplyr::filter(year == 2013) %>%
      dplyr::mutate(year = 2017),
    efi %>%
      dplyr::filter(year == 2013) %>%
      dplyr::mutate(year = 2018),
    efi %>%
      dplyr::filter(year == 2013) %>%
      dplyr::mutate(year = 2019)
  )


### write data -------------------------------------------------------------------------------------
# writes formatted dataframe as csv files
write.csv(efi, "Data files/Formatted data files/ethnic_fractionalization.csv", row.names = FALSE)

# countries missing EFI data:
## Africa: CMR, GNQ, MOZ, SSD, STP, SYC, ZAN
## Asia: IND, ISP*/PSE, MDV
## Caribbean countries: ATG, BHS, BLZ, BRB, DMA, GRD, KNA, LCA, SUR, VCT
## Europe: AND, FRA, ISL, KSV, LIE, LUX, MCO, MLT, MNE, SMR
## Pacific countries: BRN, FSM, KIR, MHL, NRU, PLW, PNG, TON, TUV, VUT, WSM

# Critical missing: MOZ, ISP*/PSE, FRA

# efi <- efi %>%
#   dplyr::group_by(iso3c) %>%
#   dplyr::arrange(year) %>%
#   dplyr::mutate(ethf = na_interpolation(ethf, option = "linear")) %>%
#   dplyr::ungroup() %>%
#   dplyr::select(iso3c, year, ethf)

