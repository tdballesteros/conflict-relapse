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
vnm <- vnm %>%
  rbind(rvn)

# removes Vietnam from the main population dataframe and joins Vietnam dataframe
pd <- pd %>%
  dplyr::filter(iso3c != "VNM") %>%
  rbind(vnm)

#### USSR ----------------------------------------------------------------------
ussr <- pd %>%
  dplyr::filter(iso3c %in% c("EST","LVA","LTU","MDA","BLR","UKR","RUS","GEO","ARM","AZE",
                             "KAZ","KGZ","TJK","TKM","UZB")) %>%
  tidyr::pivot_wider(names_from = iso3c, values_from = value) %>%
  dplyr::mutate(sov = EST + LVA + LTU + MDA + BLR + UKR + RUS + GEO + ARM + AZE + KAZ + KGZ + TJK + TKM + UZB) %>%
  dplyr::select(sov,year) %>%
  dplyr::mutate(iso3c = "RUS") %>%
  dplyr::filter(year <= 1991) %>%
  dplyr::rename(value = sov)

pd <- pd %>%
  dplyr::filter(iso3c != "EST" | year >= 1992) %>%
  dplyr::filter(iso3c != "LVA" | year >= 1992) %>%
  dplyr::filter(iso3c != "LTU" | year >= 1992) %>%
  dplyr::filter(iso3c != "BLR" | year >= 1992) %>%
  dplyr::filter(iso3c != "MDA" | year >= 1992) %>%
  dplyr::filter(iso3c != "UKR" | year >= 1992) %>%
  dplyr::filter(iso3c != "GEO" | year >= 1992) %>%
  dplyr::filter(iso3c != "ARM" | year >= 1992) %>%
  dplyr::filter(iso3c != "AZE" | year >= 1992) %>%
  dplyr::filter(iso3c != "TJK" | year >= 1992) %>%
  dplyr::filter(iso3c != "TKM" | year >= 1992) %>%
  dplyr::filter(iso3c != "KAZ" | year >= 1992) %>%
  dplyr::filter(iso3c != "KGZ" | year >= 1992) %>%
  dplyr::filter(iso3c != "UZB" | year >= 1992) %>%
  dplyr::filter(iso3c != "RUS" | year >= 1992) %>%
  rbind(ussr)

#### Yugoslavia ----------------------------------------------------------------------
yug <- pd %>%
  dplyr::filter(iso3c %in% c("SVN","HRV","MKD","BIH","SRB","MNE")) %>%
  tidyr::pivot_wider(names_from = iso3c, values_from = value) %>%
  dplyr::mutate(yug = SVN + HRV + MKD + BIH + SRB + MNE,
                smn = SRB + MNE) %>%
  dplyr::select(year,yug,smn) %>%
  dplyr::mutate(iso3c = "YUG",
                value = NA)

yug$value[yug$year<=1991] <- yug$yug[yug$year<=1991]
yug$value[yug$year<=2006&yug$year>1991] <- yug$smn[yug$year<=2006&yug$year>1991]

yug <- yug %>%
  dplyr::select(iso3c,year,value) %>%
  dplyr::filter(year <= 2006)

srb <- yug %>%
  dplyr::filter(year <= 2006,
                year > 1991) %>%
  dplyr::mutate(iso3c = "SRB")

yug <- yug %>%
  dplyr::filter(year <= 1991)

pd <- pd %>%
  dplyr::filter(iso3c != "SRB" | year > 2006) %>%
  dplyr::filter(iso3c != "SVN" | year >= 1992) %>%
  dplyr::filter(iso3c != "HRV" | year >= 1992) %>%
  dplyr::filter(iso3c != "MKD" | year >= 1992) %>%
  dplyr::filter(iso3c != "BIH" | year >= 1992) %>%
  dplyr::filter(iso3c != "MNE") %>%
  rbind(yug,srb)

#### Kosovo ----------------------------------------------------------------------
cow.pop.srb <- cow.pop %>%
  dplyr::filter(stateabb %in% c("KOS","YUG")) %>%
  tidyr::pivot_wider(names_from = "stateabb", values_from = "tpop") %>%
  dplyr::mutate(total = KOS + YUG,
                srb.p = YUG / total,
                ksv.p = KOS / total)

srb <- pd %>%
  dplyr::filter(iso3c == "SRB") 
srb$year <- as.numeric(srb$year)

srb <- srb %>%
  dplyr::full_join(cow.pop.srb,by="year")

srb$srb.p[srb$year>=2013] <- 0.8410812
srb$ksv.p[srb$year>=2013] <- 0.1589188

srb <- srb %>%
  dplyr::mutate(srb.e = value * srb.p,
                ksv.e = value * ksv.p)

srb2 <- srb %>%
  dplyr::select(iso3c,year,srb.e,value)

for(i in 1:nrow(srb2)){
  if(is.na(srb2$srb.e[i])){
    srb2$srb.e[i] <- srb2$value[i]
  }
}

srb2 <- srb2 %>%
  dplyr::select(iso3c,year,srb.e) %>%
  dplyr::mutate(iso3c = "SRB") %>%
  dplyr::rename(value = srb.e)

ksv <- srb %>%
  dplyr::select(iso3c,year,ksv.e,value)

for(i in 1:nrow(ksv)){
  if(is.na(ksv$ksv.e[i])){
    ksv$ksv.e[i] <- ksv$value[i]
  }
}

ksv <- ksv %>%
  dplyr::select(iso3c,year,ksv.e) %>%
  dplyr::mutate(iso3c = "KSV") %>%
  dplyr::rename(value = ksv.e) %>%
  dplyr::filter(year >= 2008)

srb <- rbind(srb2,ksv)

pd <- pd %>%
  dplyr::filter(iso3c != "SRB") %>%
  rbind(srb) %>%
  dplyr::rename(pop.pd = value)

pd$iso3c[pd$iso3c=="RUS" & pd$year<= 1991] <- "SOV"

#### Czechoslovakia ----------------------------------------------------------------------
cs <- pd %>%
  dplyr::filter(iso3c %in% c("CZE","SVK")) %>%
  tidyr::pivot_wider(names_from = iso3c, values_from = pop.pd) %>%
  dplyr::mutate(cs = CZE + SVK) %>%
  dplyr::filter(year <= 1992) %>%
  dplyr::select(year, cs) %>%
  dplyr::rename(pop.pd = cs) %>%
  dplyr::mutate(iso3c = "CZE")

pd <- pd %>%
  dplyr::filter(iso3c %!in% c("CZE","SVK") | year >= 1993) %>%
  rbind(cs)

pd$year <- as.numeric(pd$year)

### add workbook estimates ----------------------------------------------------------------------
# adds population estimates for countries in the 1940s (file notes sources of estimates)
pop_40s <- readxl::read_excel("Data files/Workbooks/pop_estimates.xlsx", sheet = 1) %>%
  dplyr::select(-method)

pd <- pd %>%
  rbind(pop_40s)

### write data ----------------------------------------------------------------------
# writes formatted dataframe as csv files
write.csv(pd,"Data files/Formatted data files/population.csv")
