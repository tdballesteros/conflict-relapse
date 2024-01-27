# This script formats a population estimate variable.

### load libraries ----------------------------------------------------------------------
library(readxl)
library(countrycode)
library(dplyr)
library(tidyr)

### not in function ----------------------------------------------------------------------
'%!in%' <- function(x,y)!('%in%'(x,y))

### load data files ----------------------------------------------------------------------
# population data from United Nations Department of Economic and Social Affairs, Population Division
# 1950 - 1984
pd1 <- readxl::read_excel("Data files/Raw data files/AnnualTotPopMidYear-20200708071736.xlsx", sheet = 2, skip = 1)
# 1985 - 2019
pd2 <- readxl::read_excel("Data files/Raw data files/AnnualTotPopMidYear-20200708071835.xlsx", sheet = 2, skip = 1)

### format data ----------------------------------------------------------------------
# merge datasets
pd <- dplyr::full_join(pd1,pd2,by=c("ISO 3166-1 numeric code","Location")) %>%
  # drop ISO 3166-1 code and data notes columns
  select(-c(`ISO 3166-1 numeric code`,Note.x,Note.y)) %>%
  # filter out non-countries (regions and economic groups)
  dplyr::filter(Location %!in% c("World","More developed regions","Less developed regions",
                                 "Least developed countries","Less developed regions, excluding least developed countries",
                                 "Less developed regions, excluding China","High-income countries",
                                 "Middle-income countries","Lower-middle-income countries",
                                 "Upper-middle-income countries","Low-income countries","Sub-Saharan Africa",
                                 "Africa","Eastern Africa","Middle Africa","Northern Africa","Southern Africa",
                                 "Western Africa","Asia","Eastern Asia","Central Asia","Southern Asia",
                                 "South-Eastern Asia","Western Asia","Europe","Eastern Europe","Northern Europe",
                                 "Southern Europe","Western Europe","Latin America and the Caribbean","Caribbean",
                                 "Central America","South America","North America","Oceania","Australia/New Zealand",
                                 "Melanesia","Polynesia","South-Central Asia","Northern America","Micronesia"),
                # filter out country sub-units + Holy See
                Location %!in% c("Mayotte","Réunion","Saint Helena","China, Hong Kong SAR","China, Macao SAR","Maldives",
                                 "Channel Islands","Faeroe Islands","Isle of Man","Gibraltar","Holy See","Anguilla","Aruba",
                                 "British Virgin Islands","Caribbean Netherlands","Cayman Islands","Curaçao","Guadeloupe",
                                 "Martinique","Montserrat","Sint Maarten (Dutch part)","Turks and Caicos Islands",
                                 "United States Virgin Islands","Belize","Falkland Islands (Malvinas)","French Guiana","Bermuda",
                                 "Greenland","Saint Pierre and Miquelon","New Caledonia","Guam","Northern Mariana Islands",
                                 "American Samoa","Cook Islands","French Polynesia","Niue","Tokelau","Wallis and Futuna Islands",
                                 "Western Sahara")) %>%
  # convert to long data
  tidyr::pivot_longer(2:71, names_to = "year", values_to = "value")

# Puerto Rico added to USA pop
pd$Location[pd$Location=="Puerto Rico"] <- "United States of America"
pd <- pd %>%
  dplyr::group_by(Location,year) %>%
  dplyr::summarise(value = sum(value)) %>%
  dplyr::ungroup() %>%
  # convert to full value
  dplyr::mutate(value = 1000 * value,
                # using the countrycode package, add iso3c based on country name
                iso3c = countrycode::countrycode(Location,"country.name","iso3c")) %>%
  # move iso3c variable first
  dplyr::relocate(iso3c, .before = Location) %>%
  dplyr::rename(country = Location)

### calculate unified/divided country data ----------------------------------------------------------------------
# YEM is combined population of YPR and YAR, DEU is combined DDR and BRD, VNM is combined with RVN
# SRB includes KSV, 6 YUG republics have component populations individually

# load COW populations to determine ratio of pop between YPR and YAR / DDR and BRD / VNM and RVN / SRB and KSV
cow.pop <- read.csv("Data files/Raw data files/NMC_5_0.csv") %>%
  as.data.frame() %>%
  dplyr::select(stateabb,year,tpop)

#### Yemen ----------------------------------------------------------------------
cow.pop.yem <- cow.pop %>%
  # select North Yemen, South Yemen, and unified Yemen
  dplyr::filter(stateabb %in% c("YAR","YPR","YEM")) %>%
  # convert each country to having a column, with each year being a row
  tidyr::pivot_wider(names_from = "stateabb", values_from = "tpop") %>%
  # calculate a combined North and South Yemen population and the proportion of the population in each country
  dplyr::mutate(total = YAR + YPR,
                yar.p = YAR / total,
                ypr.p = YPR / total)

# pull Yemen population data from the UN data source
yem <- pd %>%
  dplyr::filter(iso3c == "YEM") %>%
  dplyr::mutate(year = as.numeric(year)) %>%
  # merge UN and COW population data
  dplyr::full_join(cow.pop.yem,by="year")

# use proportions for first year of data available (1967; the year South Yemen gained independence) for both North
# and South Yemen for prior years missing data
yem$yar.p[is.na(yem$yar.p)&yem$year<1990] <- 4634/5954
yem$ypr.p[is.na(yem$ypr.p)&yem$year<1990] <- 1320/5954

# calculate estimates for 1950 - 1966 based on UN population data and 1967 proportions of populations between
# North and South Yemen
yem <- yem %>%
  dplyr::mutate(yar.e = value * yar.p,
                ypr.e = value * ypr.p)

# create dataframe for South Yemen
ypr <- yem %>%
  dplyr::select(iso3c,year,ypr.e) %>%
  # no data available from either source before 1950
  dplyr::filter(year >= 1950,
                # North and South Yemen unified starting in 1991 for purposes of this dataset
                year < 1991) %>%
  dplyr::mutate(iso3c = "YPR",
                country = "South Yemen") %>%
  dplyr::rename(value = ypr.e)

# use unified YEM population value for 1990
ypr$value[ypr$year==1990] <- 12057000

# creates dataframe for North Yemen
yar <- yem %>%
  dplyr::select(iso3c,year,yar.e) %>%
  # no data available from either source before 1950
  dplyr::filter(year >= 1950,
                # North and South Yemen unified starting in 1991 for purposes of this dataset
                year < 1991) %>%
  dplyr::mutate(iso3c = "YAR",
                country = "North Yemen") %>%
  dplyr::rename(value = yar.e)

# use unified YEM population value for 1990
yar$value[yar$year==1990] <- 12057000

# creates dataframe for unified Yemen
yem <- yem %>%
  dplyr::select(iso3c,year,value) %>%
  # removes years prior to unification
  dplyr::filter(year > 1990) %>%
  dplyr::mutate(country = "Yemen") %>%
  # merges YPR and YAR dataframes
  rbind(yar,ypr)

# removes Yemen from the main population dataframe and joins Yemen dataframe
pd <- pd %>%
  dplyr::filter(iso3c != "YEM") %>%
  rbind(yem)

#### Germany ----------------------------------------------------------------------
cow.pop.deu <- cow.pop %>%
  # select East Germany, West Germany, and unified Germany
  dplyr::filter(stateabb %in% c("GDR","GFR","GMY"),
                year > 1945) %>%
  # convert each country to having a column, with each year being a row
  tidyr::pivot_wider(names_from = "stateabb", values_from = "tpop") %>%
  # calculate a combined East and West Germany population and the proportion of the population in each country
  dplyr::mutate(total = GDR + GFR,
                gfr.p = GFR / total,
                gdr.p = GDR / total)

# pull Germany population data from the UN data source
deu <- pd %>%
  dplyr::filter(iso3c == "DEU") %>%
  dplyr::mutate(year = as.numeric(year)) %>%
  # merge UN and COW population data
  dplyr::full_join(cow.pop.deu,by="year")

# use proportions for first year of data available (1955; the year Allied forces ended the occupation of West Germany) for both East
# and West Germany for prior years missing data
deu$gdr.p[is.na(deu$gdr.p)&deu$year<1990] <- 17944/70308
deu$gfr.p[is.na(deu$gfr.p)&deu$year<1990] <- 52364/70308

# calculate estimates for 1950 - 1954 based on UN population data and 1955 proportions of populations between
# East and West Germany
deu <- deu %>%
  dplyr::mutate(gfr.e = value * gfr.p,
                gdr.e = value * gdr.p)

# create dataframe for West Germany
gfr <- deu %>%
  dplyr::select(iso3c,year,gfr.e) %>%
  # East and West Germany unified starting in 1990 for purposes of this dataset
  dplyr::filter(year < 1990) %>%
  dplyr::mutate(iso3c = "BRD",
                country = "West Germany") %>%
  dplyr::rename(value = gfr.e)

# create dataframe for East Germany
gdr <- deu %>%
  dplyr::select(iso3c,year,gdr.e) %>%
  # East and West Germany unified starting in 1990 for purposes of this dataset
  dplyr::filter(year < 1990) %>%
  dplyr::mutate(iso3c = "DDR",
                country = "East Germany") %>%
  dplyr::rename(value = gdr.e)

# creates dataframe for unified Germany
deu <- deu %>%
  dplyr::select(iso3c,year,value) %>%
  # removes years prior to unification
  dplyr::filter(year > 1989) %>%
  dplyr::mutate(country = "Germany") %>%
  # merges GFR and GDR dataframes
  rbind(gfr,gdr)

# removes Germany from the main population dataframe and joins Germany dataframe
pd <- pd %>%
  dplyr::filter(iso3c != "DEU") %>%
  rbind(deu)

#### Vietnam ----------------------------------------------------------------------
cow.pop.vnm <- cow.pop %>%
  # select North Vietnam (1954-2012) and South Vietnam (1954-1975); Vietnamese War ended in 1975
  # and reunification happened in 1976
  dplyr::filter(stateabb %in% c("DRV","RVN")) %>%
  # convert both countries to having a column, with each year being a row
  tidyr::pivot_wider(names_from = "stateabb", values_from = "tpop") %>%
  # calculate a combined North and South Vietnam population and the proportion of the population in each country
  dplyr::mutate(total = DRV + RVN,
                vnm.p = DRV / total,
                rvn.p = RVN / total)

# pull Vietnam population data from the UN data source
vnm <- pd %>%
  dplyr::filter(iso3c == "VNM") %>%
  dplyr::mutate(year = as.numeric(year)) %>%
  # merge UN and COW population data
  dplyr::full_join(cow.pop.vnm,by="year") %>%
  # uses the COW proportions on the UN data for 1954 - 1975
  dplyr::mutate(vnm.e = value * vnm.p,
                rvn.e = value * rvn.p)

# create dataframe for Vietnam (including North Vietnam)
vnm2 <- vnm %>%
  dplyr::select(iso3c,year,vnm.e,value) %>%
  # Vietnam gained independence from France in 1954
  dplyr::filter(year >= 1954) %>%
  dplyr::mutate(vnm.e = dplyr::coalesce(vnm.e,value),
                iso3c = "VNM",
                country = "Viet Nam") %>%
  dplyr::select(-value) %>%
  dplyr::rename(value = vnm.e)

# create dataframe for South Vietnam
rvn <- vnm %>%
  dplyr::select(iso3c,year,rvn.e) %>%
  # Vietnam gained independence from France in 1954
  dplyr::filter(year >= 1954,
                # Vietnam reunified starting in 1976
                year <= 1975) %>%
  dplyr::mutate(iso3c = "RVN",
                country = "South Vietnam") %>%
  dplyr::rename(value = rvn.e)

# merges VNM and RVN dataframes
vnm <- vnm2 %>%
  rbind(rvn)

# removes Vietnam from the main population dataframe and joins Vietnam dataframe
pd <- pd %>%
  dplyr::filter(iso3c != "VNM") %>%
  rbind(vnm)

#### USSR ----------------------------------------------------------------------
ussr <- pd %>%
  # select the 15 USSR successor states
  dplyr::filter(iso3c %in% c("EST","LVA","LTU","MDA","BLR","UKR","RUS","GEO","ARM","AZE",
                             "KAZ","KGZ","TJK","TKM","UZB")) %>%
  dplyr::group_by(year) %>%
  # sums the populations of the 15 SSRs
  dplyr::summarise(value = sum(value)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(iso3c = "SOV",
                country = "Soviet Union") %>%
  # the Soviet Union broke apart starting in 1992 for purposes of this dataset
  dplyr::filter(year <= 1991)

# filter out the 15 SSRs during the period of the Soviet Union
pd <- pd %>%
  dplyr::filter(iso3c %!in% c("EST","LVA","LTU","BLR","UKR","RUS","GEO","ARM","AZE",
                              "KAZ","KGZ","TJK","TKM","UZB") | year >= 1992) %>%
  # merge USSR dataset
  rbind(ussr)
                
#### Yugoslavia ----------------------------------------------------------------------
yug <- pd %>%
  # select the 6 fully recognized successor states
  dplyr::filter(iso3c %in% c("SVN","HRV","MKD","BIH","SRB","MNE")) %>%
  dplyr::group_by(year) %>%
  # sums the populations of the 6 fully recognized successor states
  dplyr::summarise(value = sum(value)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(iso3c = "YUG",
                country = "Yugoslavia") %>%
  # Yugoslavia broke apart starting in 1992 for purposes of this dataset
  dplyr::filter(year <= 1991)

# filter out the 6 fully recognized successor states during the period of Yugoslavia
pd <- pd %>%
  dplyr::filter(iso3c %!in% c("SVN","HRV","MKD","BIH","SRB","MNE") | year >= 1992) %>%
  # merge Yugoslavia dataset
  rbind(yug)

#### Serbia and Montenegro ----------------------------------------------------------------------
# coded as SRB / Serbia, but includes Montenegro's population 1992 - 2005, with Montenegro
# being a separate country in this dataset beginning in 2006
srb_mne <- pd %>%
  dplyr::select(-country) %>%
  # select Serbia and Montenegro
  dplyr::filter(iso3c %in% c("SRB","MNE")) %>%
  dplyr::group_by(year) %>%
  # sums the populations of Serbia and Montenegro
  dplyr::summarise(value = sum(value,na.rm=TRUE)) %>%
  dplyr::ungroup() %>%
  # Montenegro regained independence starting in 2006 for purposes of this dataset
  dplyr::filter(year < 2006) %>%
  dplyr::mutate(iso3c = "SRB",
                country = "Serbia and Montenegro")

# filter out Serbia and Montenegro from the main dataset during their period of unification
pd <- pd %>%
  dplyr::filter(iso3c %!in% c("SRB","MNE") | year >= 2006) %>%
  # merge Serbia and Montenegro dataset
  rbind(srb_mne)

#### Kosovo ----------------------------------------------------------------------
cow.pop.srb <- cow.pop %>%
  # select Yugoslavia (Serbia shares YUG as its coding) and Kosovo
  dplyr::filter(stateabb %in% c("YUG","KOS"),
                # Kosovo declared and gained (de facto) independence in 2008 for purposes of this dataset
                year >= 2008) %>%
  # convert both countries to having a column, with each year being a row
  tidyr::pivot_wider(names_from = "stateabb", values_from = "tpop") %>%
  # Serbia appears to include both Serbia and Kosovo, while Kosovo includes just Kosovo
  # calculate a Serbia without Kosovo population and the proportion of the population in each part relative to the total population
  dplyr::mutate(SRB = YUG - KOS,
                srb.p = SRB / YUG,
                ksv.p = KOS / YUG)

# pull Serbia population data from the UN data source (Serbia + Kosovo)
srb <- pd %>%
  dplyr::select(-country) %>%
  dplyr::filter(iso3c == "SRB",
                # Kosovo declared and gained (de facto) independence in 2008 for purposes of this dataset
                year >= 2008) %>%
  dplyr::mutate(year = as.numeric(year)) %>%
  # merge UN and COW population data
  dplyr::full_join(cow.pop.srb,by="year")

# use proportions for last year of data available (2012) for both Serbia and Kosovo for subsequent years missing data
srb$srb.p[is.na(srb$srb.p)&srb$year>2012] <- 7748/9553
srb$ksv.p[is.na(srb$ksv.p)&srb$year>2012] <- 1805/9553
  
# calculate estimates for 2013 - 2019 based on UN population data and 2012 proportions of populations between
# Serbia and Kosovo
srb <- srb %>%
  dplyr::mutate(srb.e = value * srb.p,
                ksv.e = value * ksv.p)

# create dataframe for Serbia (2013 - 2019)
srb2 <- srb %>%
  dplyr::select(iso3c,year,srb.e) %>%
  dplyr::rename(value = srb.e) %>%
  dplyr::mutate(country = "Serbia")

# create dataframe for Kosovo
ksv <- srb %>%
  dplyr::select(iso3c,year,ksv.e) %>%
  dplyr::mutate(iso3c = "KSV",
                country = "Kosovo") %>%
  dplyr::rename(value = ksv.e)

# removes Serbia (2013 - 2019) from the main population dataframe and joins the
# new Serbia and Kosovo dataframes
pd <- pd %>%
  dplyr::filter(iso3c != "SRB" | year < 2008) %>%
  rbind(srb2) %>%
  rbind(ksv)

#### Czechoslovakia ----------------------------------------------------------------------
cs <- pd %>%
  # select Czechia and Slovakia
  dplyr::filter(iso3c %in% c("CZE","SVK")) %>%
  dplyr::group_by(year) %>%
  # sums the populations Czechia and Slovakia
  dplyr::summarise(value = sum(value)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(iso3c = "CZE",
                country = "Czechoslovakia") %>%
  # Czechoslovakia broke apart starting in 1993 for purposes of this dataset
  dplyr::filter(year <= 1992)

# filter out Czechia and Slovakia during the period of Czechoslovakia
pd <- pd %>%
  dplyr::filter(iso3c %!in% c("CZE","SVK") | year >= 1993) %>%
  # merge Czechoslovakia dataset
  rbind(cs)

### add workbook estimates ----------------------------------------------------------------------
# adds population estimates for countries in the 1940s (file notes sources of estimates)
pop_40s <- readxl::read_excel("Data files/Workbooks/pop_estimates.xlsx", sheet = 1) %>%
  dplyr::select(-method) %>%
  dplyr::rename(value = pop.pd) %>%
  # using the countrycode package, add country name based on iso3c value
  dplyr::mutate(country = countrycode::countrycode(iso3c,"iso3c","country.name"))

# codes country name values missing from the countrycode package: BRD, DDR, SOV, YAR, YUG
pop_40s$country[pop_40s$iso3c=="BRD"] <- "West Germany"
pop_40s$country[pop_40s$iso3c=="DDR"] <- "East Germany"
pop_40s$country[pop_40s$iso3c=="SOV"] <- "Soviet Union"
pop_40s$country[pop_40s$iso3c=="YAR"] <- "North Yemen"
pop_40s$country[pop_40s$iso3c=="YUG"] <- "Yugoslavia"

pd <- pd %>%
  rbind(pop_40s)

### write data ----------------------------------------------------------------------
# writes formatted dataframe as csv files
write.csv(pd,"Data files/Formatted data files/population.csv")
