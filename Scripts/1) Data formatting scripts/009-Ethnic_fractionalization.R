# This script formats the Ethnic Fractionalization Index, which measures ethnic diversity within a country.

# Ethnic Fractionalization Index
## https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/4JQRCL

### load libraries ----------------------------------------------------------------------
library(readxl)
library(countrycode)
library(imputeTS)
library(dplyr)

### not in function ----------------------------------------------------------------------
'%!in%' <- function(x,y)!('%in%'(x,y))

### load data file ----------------------------------------------------------------------
efi <- read.csv("Data files/Raw data files/HIEF_data.csv")

### data formatting ----------------------------------------------------------------------
efi <- efi %>%
  # using the countrycode package, add iso3c code name based on country name
  dplyr::mutate(iso3c = countrycode::countrycode(Country,"country.name","iso3c"))

efi$iso3c[efi$Country=="Czechoslovakia"] <- "CZE"
efi$iso3c[efi$Country=="German Democratic Republic"] <- "DDR"
efi$iso3c[efi$Country=="Republic of Vietnam"] <- "RVN"
efi$iso3c[efi$Country=="Yemen Arab Republic"] <- "YAR"
efi$iso3c[efi$Country=="Yemen PDR"] <- "YPR"
efi$iso3c[efi$Country=="Yugoslavia"] <- "YUG"

efi <- efi %>%
  dplyr::select(-Country) %>%
  #dplyr::full_join(expand.grid(iso3c = unique(efi$iso3c), Year = c(2014:2019))) %>%
  # using the countrycode package, add country name based on iso3c value
  dplyr::mutate(country = countrycode::countrycode(iso3c,"iso3c","country.name")) %>%
  dplyr::rename(year = Year,
                ethf = EFindex) %>%
  dplyr::relocate(iso3c, .before = year) %>%
  dplyr::relocate(country, .after = iso3c)

efi$country[efi$iso3c=="DDR"] <- "East Germany"
efi$country[efi$iso3c=="RVN"] <- "South Vietnam"
efi$country[efi$iso3c=="YAR"] <- "North Yemen"
efi$country[efi$iso3c=="YPR"] <- "South Yemen"
efi$country[efi$iso3c=="YUG"] <- "Yugoslavia"

### calculate missing countries ----------------------------------------------------------------------
#### India ----------------------------------------------------------------------
# load linguistic data from the 2011 census, using mother tongue as a stand-in for ethnic group
efi.ind <- readxl::read_xlsx("Data files/Raw data files/DDW-C16-STMT-MDDS-0000.XLSX") %>%
  dplyr::filter(`...5` == "INDIA") %>%
  dplyr::mutate(name_first_two = stringr::str_sub(`...7`, start = 1, end = 2),
                name_first_two = as.numeric(name_first_two),
                name_after_two = stringr::str_sub(`...7`, start = 3, end = -1),
                `...8` = as.numeric(`...8`)) %>%
  # filter out main language family groupings - these start with a number but do not include the
  # "# Other" formatted languages
  dplyr::filter(is.na(name_first_two) | name_after_two == "Others") %>%
  dplyr::select(`...7`,`...8`) %>%
  dplyr::mutate(perc = `...8` / sum(`...8`,na.rm=TRUE),
                perc_sq = perc^2)

ind.efi.est <- 1 - sum(efi.ind$perc_sq,na.rm=TRUE)

#### Kosovo ----------------------------------------------------------------------
# load data from Wikipedia: https://en.wikipedia.org/wiki/Demographics_of_Kosovo#Ethnic_groups
# note 2011 census was boycott by ethnic Serbs
efi.ksv <- readxl::read_xlsx("Data files/Raw data files/Ethnic_Composition.xlsx",
                             sheet = "KSV",
                             skip = 1) %>%
  dplyr::select(`...1`,`Number...14`,`%...15`) %>%
  tidyr::drop_na(`%...15`) %>%
  dplyr::mutate(`%...15` = `%...15` / 100,
                sq = `%...15`^2)

ksv.efi.est <- 1 - sum(efi.ksv$sq,na.rm=TRUE)

#### Montenegro ----------------------------------------------------------------------
# load data from Wikipedia: https://en.wikipedia.org/wiki/Demographics_of_Montenegro
efi.mne <- readxl::read_xlsx("Data files/Raw data files/Ethnic_Composition.xlsx",
                             sheet = "MNE") %>%
  dplyr::filter(Municipality == "Montenegro total") %>%
  dplyr::select(seq(from = 1, to = 56, by = 2)) %>%
  dplyr::mutate_if(is.character,as.numeric) %>%
  tidyr::pivot_longer(2:28, names_to = "ethnic group", values_to = "population") %>%
  dplyr::mutate(perc = population / sum(population,na.rm=TRUE),
                perc_sq = perc^2)

mne.eft.est <- 1 - sum(efi.mne$perc_sq,na.rm=TRUE)

#### Serbia ----------------------------------------------------------------------
# load data from Wikipedia: https://en.wikipedia.org/wiki/Demographics_of_Serbia#Ethnic_groups
# note the data excludes Kosovo
efi.srb <- readxl::read_xlsx("Data files/Raw data files/Ethnic_Composition.xlsx",
                             sheet = "SRB",
                             skip = 1) %>%
  dplyr::select(`...1`,`Number...16`,`%...17`) %>%
  tidyr::drop_na(`%...17`) %>%
  dplyr::mutate(`%...17` = `%...17` / 100,
                sq = `%...17`^2)

srb.efi.est <- 1 - sum(efi.srb$sq,na.rm=TRUE)

#### South Sudan ----------------------------------------------------------------------
# load data from Wikipedia: https://en.wikipedia.org/wiki/List_of_ethnic_groups_in_South_Sudan
efi.ssd <- readxl::read_xlsx("Data files/Raw data files/Ethnic_Composition.xlsx",
                            sheet = "SSD") %>%
  dplyr::rename_with(tolower) %>%
  dplyr::mutate(perc = size.avg / sum(size.avg,na.rm=TRUE),
                perc_sq = perc^2) %>%
  tidyr::drop_na(perc)

ssd.efi.est <- 1 - sum(efi.ssd$perc_sq,na.rm=TRUE)

### adjust data values ----------------------------------------------------------------------
#### Czechoslovakia ----------------------------------------------------------------------
# Czechoslovakia and Czechia coded as CZE; Slovakia coded as SVK beginning in 1993
# no changes required

#### Ethiopia/Eritrea(?) ----------------------------------------------------------------------
# ERI coded beginning in 1993
# no change in Ethiopia's index score when Eritrea gains independence, so unclear on
# Eritrea's inclusion in ETH metrics before 1993

#### Germany ----------------------------------------------------------------------
# West Germany and (unified) Germany coded as DEU 1949-2013; East Germany coded as DDR 1949-1990
# (unified) Germany coded as starting in 1990

# recoded West Germany as BRD
efi$iso3c[efi$iso3c=="DEU"&efi$year<1990] <- "BRD"

# remove East Germany 1990 coding, as this year is coded as having a unified Germany
efi <- efi %>%
  dplyr::filter(iso3c != "DDR" | year != 1990)

#### Malaysia/Singapore ----------------------------------------------------------------------
# MYS and SGP coded separately throughout time series, though SGP data starts in 1960

#### Pakistan/Bangladesh(x) ----------------------------------------------------------------------
# BGD coded beginning in 1971
# PAK data appears just to be coding (East) Pakistan

#### Serbia/Kosovo(x) ----------------------------------------------------------------------
# Kosovo data missing

#### Serbia/Montenegro(x) ----------------------------------------------------------------------
# Montenegro data missing

#### South Africa/Namibia(x) ----------------------------------------------------------------------
# NAM data starts in 1990
# ZAF data does not include NAM

#### Soviet Union/Russia(x) ----------------------------------------------------------------------
# Soviet Union coded as RUS 1945-1991; Russia coded as RUS 1991-2013
# Note there are two entries for 1991: Soviet Union and Russia
# Russia coded starting in 1991

#### Sudan/South Sudan(x) ----------------------------------------------------------------------
# South Sudan data missing

#### Tanzania/Zanzibar ----------------------------------------------------------------------
# Zanzibar data is missing
# TZA appears to be coded as Tanganyika+Zanzibar throughout

# adding ZAN data unlikely to impact data results as TZA is not coded as experiencing conflict

#### Yemen ----------------------------------------------------------------------
# (unified) Yemen and North Yemen coded as YAR 1945-2013; South Yemen coded as YPR 1967-1990
# (unified) Yemen coded as starting in 1991

# recoded unified Yemen as YEM
efi$iso3c[efi$iso3c=="YAR"&efi$year>=1991] <- "YEM"

#### Yugoslavia ----------------------------------------------------------------------
# HRV, MKD, SRB, SVN coded starting in 1991; BIH coded starting in 1992; YUG coded 1945-1990
# YUG coded as ending in 1991 (inclusive), with republics starting independence in 1992

# estimate YUG 1991 as YUG 1990
efi <- efi %>%
  tibble::add_row(iso3c = "YUG",
                  country = "Yugoslavia",
                  year = 1991,
                  ethf = efi$ethf[efi$iso3c=="YUG"&efi$year==1990]) %>%
  
  # filter out HRV, MKD, SRB, and SVN 1991 codes
  dplyr::filter(iso3c %!in% c("HRV","MKD","SRB","SVN") | year != 1991)

#### Vietnam/South Vietnam(?) ----------------------------------------------------------------------
# RVN coded 1954-1975
# unclear whether VNM includes RVN during this time or not, though RVN has a lower index score than VNM
# would expect a drop in index score after reunification


# countries missing EFI data:
## Africa: CMR, GNQ, MOZ, SSD, STP, SYC, ZAN
## Asia: IND, ISP*/PSE, MDV
## Caribbean countries: ATG, BHS, BLZ, BRB, DMA, GRD, KNA, LCA, SUR, VCT
## Europe: AND, FRA, ISL, KSV, LIE, LUX, MCO, MLT, MNE, SMR
## Pacific countries: BRN, FSM, KIR, MHL, NRU, PLW, PNG, TON, TUV, VUT, WSM

# Critical missing: MOZ, ISP*/PSE, FRA

efi <- efi %>%
  group_by(iso3c) %>%
  arrange(year) %>%
  mutate(ethf2 = na_interpolation(ethf, option = "linear")) %>%
  ungroup() %>%
  select(iso3c,year,ethf2) %>%
  rename(ethf = ethf2)

