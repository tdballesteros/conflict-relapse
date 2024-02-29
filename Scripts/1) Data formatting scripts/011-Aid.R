# This script creates an aid estimate variable.

# TODO: is the aid in constant $?

### load libraries ----------------------------------------------------------------------
library(countrycode)
library(dplyr)

### not in function ----------------------------------------------------------------------
'%!in%' <- function(x,y)!('%in%'(x,y))

### load data file ----------------------------------------------------------------------
# aid from aiddata.org through Quality of Government dataset
#qog <- read.csv("Data files/Raw data files/qog_std_ts_jan20.csv")
#aiddata <- read.csv("Data files/Raw data files/AidDataCoreThin_ResearchRelease_Level1_v3.1.csv")
aiddata_full <- read.csv("~/Downloads/AidDataCore_ResearchRelease_Level1_v3/AidDataCoreFull_ResearchRelease_Level1_v3.1.csv")
# received_amount_usd_nominal - very minimal data

### data formatting ----------------------------------------------------------------------
aid <- aiddata_full %>%
  dplyr::select(recipient,year,commitment_amount_usd_current) %>%
  # filter out multi-country aid packages
  dplyr::filter(recipient %!in% c("Africa, North of Sahara, Regional Programs","Africa, Regional Programs Multi-Country",
                                  "Africa, Regional Programs, Regional Programs","Africa, South of Sahara Multi-Country",
                                  "Africa, South of Sahara, Regional Programs","Africa, South of Sahara, Regional Programs Multi-Country",
                                  "America, Regional Programs, Regional Programs","Arab Countries, Regional Programs, Regional Programs",
                                  "Asia, Regional Programs Multi-Country","Asia, Regional Programs, Regional Programs",
                                  "Bilateral, unspecified","Bilateral, unspecified Multi-Country",
                                  "Central Asia, Regional Programs, Regional Programs","Europe, Regional Programs","European Commission",
                                  "Ex-Yugoslavian States, Unspecified","Far East Asia, Regional Programs","Global",
                                  "Latin America, Regional Programs, Regional Programs","MADCT Unspecified",
                                  "Mediterranean, Regional Programs, Regional Programs","Middle East, Regional Programs","no value",
                                  "North & Central America, Regional Programs","North & Central America, regional, Regional Programs",
                                  "Oceania, Regional Programs","South & Central Asia, Regional Programs","South America, Regional Programs",
                                  "South Asia, Regional Programs, Regional Programs","West Indies, Regional Programs, Regional Programs"),
                # filter out non-sovereign entities
                recipient %!in% c("Anguilla","Aruba","Bermuda","Cayman Islands","Cook Islands","Falkland Islands","French Guiana",
                                  "French Polynesia","Gibraltar","Guadeloupe","Hong Kong, China","Macao","Martinique","Mayotte",
                                  "Montserrat","Netherlands Antilles","New Caledonia","Niue","Northern Marianas","Reunion","St. Helena",
                                  "St. Pierre & Miquelon","Tokelau","Turks and Caicos Islands","Virgin Islands (UK)","Wallis & Futuna"),
                # filter unknown year
                year != 9999) %>%
  # using the countrycode package, add iso3c code based on country name
  dplyr::mutate(iso3c = countrycode::countrycode(recipient,"country.name","iso3c"),
                # using the countrycode package, add country name based on iso3c code,
                country = countrycode::countrycode(iso3c,"iso3c","country.name"))

# add missing iso3c codes
aid$iso3c[aid$recipient=="Kosovo"] <- "KSV"
aid$iso3c[aid$recipient=="Serbia and Montenegro"] <- "SRB"
aid$iso3c[aid$recipient=="Yugoslavia"] <- "YUG"

# add missing country names
aid$country[aid$recipient=="Kosovo"] <- "Kosovo"
aid$country[aid$recipient=="Serbia and Montenegro"] <- "Serbia and Montenegro"
aid$country[aid$recipient=="Yugoslavia"] <- "Yugoslavia"

# collapse by country-year
aid <- aid %>%
  dplyr::group_by(iso3c,country,year) %>%
  dplyr::summarise(commitment_amount_usd_current = sum(commitment_amount_usd_current,na.rm=TRUE)) %>%
  dplyr::ungroup()

### extend data ----------------------------------------------------------------------
# use 2013 data as estimates for 2014-2019 data

for(y in 2014:2019){
  
  tmp <- aid %>%
    dplyr::filter(year == 2013) %>%
    dplyr::mutate(year = y)
  
  aid <- rbind(aid,tmp)
  
}

### apply inflation ----------------------------------------------------------------------
# inflation from https://www.bls.gov/data/inflation_calculator.htm
# July to July, converting to 2019 dollars
inflation_table <- data.frame(year = c(1946:2019),
                              multiplier = c(1+11.96,1+10.56,1+9.52,1+9.83, #40s
                                             1+9.65,1+8.91,1+8.61,1+8.57,1+8.54,1+8.57,1+8.36,1+8.07,1+7.85,1+7.79, #50s
                                             1+7.67,1+7.55,1+7.47,1+7.36,1+7.25,1+7.12,1+6.89,1+6.68,1+6.35,1+5.97, #60s
                                             1+5.58,1+5.30,1+5.12,1+4.79,1+4.19,1+3.73,1+3.49,1+3.21,1+2.91,1+2.51, #70s
                                             1+2.10,1+1.80,1+1.63,1+1.57,1+1.46,1+1.38,1+1.34,1+1.25,1+1.17,1+1.06, #80s
                                             1+0.97,1+0.88,1+0.83,1+0.78,1+0.73,1+0.68,1+0.63,1+0.60,1+0.57,1+0.54, #90s
                                             1+0.48,1+0.45,1+0.42,1+0.40,1+0.35,1+0.31,1+0.26,1+0.23,1+0.17,1+0.19, #00s
                                             1+0.18,1+0.14,1+0.12,1+0.10,1+0.08,1+0.08,1+0.07,1+0.05,1+0.02,1+0.00)) #10s

# merge inflation multiplier table into main dataset
aid <- aid %>%
  dplyr::left_join(inflation_table,by="year") %>%
  # apply inflation adjustments
  dplyr::mutate(aid_value = commitment_amount_usd_current * multiplier) %>%
  dplyr::select(-c(commitment_amount_usd_current,multiplier))

### expand dataset ----------------------------------------------------------------------
aid <- aid %>%
  # expand dataframe to have all iso3c-year combos from 1946 - 2019
  dplyr::full_join(expand.grid(iso3c = unique(aid$iso3c), year = c(1946:2019))) %>%
  # using the countrycode package, add country name based on iso3c code
  dplyr::mutate(country = countrycode::countrycode(iso3c,"iso3c","country.name"),
                aid_value = ifelse(is.na(aid_value),0,aid_value))

# add missing country names
aid$country[aid$iso3c=="KSV"] <- "Kosovo"
aid$country[aid$iso3c=="YUG"] <- "Yugoslavia"

### countries that dissolved/unified ----------------------------------------------------------------------
# ERI, BIH, HRV, KSV, LVA, MKD, MNE, NAM, RUS, SGP, TLS, YEM, YUG

#### PAK/BGD ----------------------------------------------------------------------
# merge BGD -1970 data into PAK data

for(y in 1946:1970){
  
  aid$aid_value[aid$iso3c=="PAK"] <- aid$aid_value[aid$iso3c=="PAK"] + aid$aid_value[aid$iso3c=="BGD"]
  aid$aid_value[aid$iso3c=="BGD"] <- 0
  
}

### filter out non-sovereign country-years ----------------------------------------------------------------------
# load formatted country-years table output by the 003-Country_years script
cyears <- utils::read.csv("Data files/Formatted data files/country_years.csv") %>%
  dplyr::select(iso3c,year,cn)

aid <- aid %>%
  # sort
  dplyr::arrange(iso3c,year) %>%
  # merge in country-years data
  dplyr::full_join(cyears,by=c("iso3c","year")) %>%
  # filter non-sovereign country-years that did not have aid;
  # leave non-sovereign country-years if aid data is present
  dplyr::filter(cn == 1 | aid_value > 0) %>%
  dplyr::select(-cn)
  
### merge GDP and population data ----------------------------------------------------------------------
# load formatted data output by script 004-GDP
gdp <- read.csv("Data files/Formatted data files/gdp.csv") %>%
  dplyr::select(iso3c,year,gdp.pwt.est,gdp.gl.est)

# load formatted data output by script 005-Population
population <- read.csv("Data files/Formatted data files/population.csv") %>%
  dplyr::select(iso3c,year,un.pop,cow.pop)

# merges GDP and population datasets to calculate aid as % of GDP and per capita
aid <- aid %>%
  dplyr::full_join(gdp,by=c("iso3c","year")) %>%
  dplyr::full_join(population,by=c("iso3c","year")) %>%
  dplyr::mutate(aid.perc.gdp.pwt = 100*aid_value / gdp.pwt.est,
                aid.perc.gdp.gl = 100*aid_value / gdp.gl.est,
                aid.per.capita.un = aid_value / un.pop,
                aid.per.capita.cow = aid_value / cow.pop) %>%
  dplyr::select(-c(gdp.pwt.est,gdp.gl.est,un.pop,cow.pop))

### write data ----------------------------------------------------------------------
# writes formatted dataframe as csv files
write.csv(aid,"Data files/Formatted data files/aid.csv",row.names = FALSE)







  
# ### data formatting ----------------------------------------------------------------------
# aid2 <- qog %>%
#   dplyr::select(cname,year,aid_crsc,aid_crsio) %>%
#   # replace all Nas with 0s
#   dplyr::mutate_all(~ifelse(is.na(.), 0, .)) %>%
#   dplyr::mutate(aid2 = aid_crsc + aid_crsio,
#                 # using the countrycode package, add iso3c based on country name
#                 iso3c = countrycode(cname,"country.name","iso3c"))
# 
# # manually add country codes to missing country names: Czechoslovakia; Germany, East; Germany, West; Micronesia;
# # Yemen, South; Yemen, North; Yugoslavia; Serbia and Montenegro; Tibet; and Vietnam, South
# aid2$iso3c[aid2$cname=="Czechoslovakia"] <- "CZE"
# aid2$iso3c[aid2$cname=="Germany, East"] <- "DDR"
# aid2$iso3c[aid2$cname=="Germany, West"] <- "BRD"
# aid2$iso3c[aid2$cname=="Micronesia"] <- "FSM"
# aid2$iso3c[aid2$cname=="Yemen, South"] <- "YPR"
# aid2$iso3c[aid2$cname=="Yemen, North"] <- "YAR"
# aid2$iso3c[aid2$cname=="Yugoslavia"] <- "YUG"
# aid2$iso3c[aid2$cname=="Serbia and Montenegro"] <- "SRB"
# aid2$iso3c[aid2$cname=="Tibet"] <- "TBT"
# aid2$iso3c[aid2$cname=="Vietnam, South"] <- "RVN"
# 
# aid2 <- aid2 %>%
#   # filter countries with limited or no aid data
#   dplyr::filter(iso3c != "TBT",
#                 # Cyprus is split into Cyprus (-1974) and Cyprus (1975-) but contains all years for both
#                 cname != "Cyprus (1975-)" | year >= 1975,
#                 cname != "Cyprus (-1974)" | year <= 1974,
#                 # Czechoslovakia and Czechia have entries for all years
#                 cname != "Czech Republic" | year >= 1993,
#                 cname != "Czechoslovakia" | year <= 1992,
#                 # Ethiopia is split into Ethiopia (-1992) and Ethiopia (1993-) but contains all years for both
#                 cname != "Ethiopia (1993-)" | year >= 1993,
#                 cname != "Ethiopia (-1992)" | year <= 1992,
#                 # France is split into France (-1962) and France (1963-) but contains all years for both
#                 cname != "France (1963-)" | year >= 1963,
#                 cname != "France (-1962)" | year <= 1962,
#                 # Malaysia is split into Malaysia (-1965) and Malaysia (1966-) but contains all years for both
#                 cname != "Malaysia (1966-)" | year >= 1966,
#                 cname != "Malaysia (-1965)" | year <= 1965,
#                 # Pakistan is split into Pakistan (-1970) and Pakistan (1971-) but contains all years for both
#                 cname != "Pakistan (1971-)" | year >= 1971,
#                 cname != "Pakistan (-1970)" | year <= 1970,
#                 # Russia and the Soviet Union have entries for all years
#                 cname != "Russia" | year >= 1992,
#                 cname != "USSR" | year <= 1991,
#                 # Sudan is split into Sudan (-2011) and Sudan (2012-) but contains all years for both
#                 cname != "Sudan (2012-)" | year >= 2012,
#                 cname != "Sudan (-2011)" | year <= 2011,
#                 # Serbia and Serbia and Montenegro have entries for all years
#                 cname != "Serbia" | year >= 2006,
#                 cname != "Serbia and Montenegro" | year <= 2005,
#                 # Vietnam and Vietnam, North have entries for all years (Vietnam, South coded separately as well)
#                 # Vietnam, North data goes through 1976 and Vietnam data begins 1977
#                 cname != "Vietnam" | year >= 1977,
#                 cname != "Vietnam, North" | year <= 1976,
#                 # filter Vietnam, South to only include through 1975, as that is when the country-years datasets
#                 # codes it as existing through
#                 cname != "Vietnam, South" | year <= 1975) %>%
#   # recode iso3c for RUS 1946-1991 as SOV
#   dplyr::mutate(iso3c = ifelse(iso3c=="RUS"&year<=1991,"SOV",iso3c)) %>%
#   dplyr::select(iso3c,year,aid2) %>%
#   # dataset does not include any data beyond 2013 - filter out 2014 - 2019 entries
#   dplyr::filter(year < 2014)
# 
# # duplicate 2013 aid data and code as same amount of aid for 2014 - 2019
# for(y in 2014:2019){
#   
#   aid.x <- aid2 %>%
#     dplyr::filter(year == 2013) %>%
#     dplyr::mutate(year = y)
#   
#   aid2 <- aid2 %>%
#     rbind(aid.x)
#   
# }
# 
# ### merge GDP and population data ----------------------------------------------------------------------
# # load formatted data output by script 004-GDP
# gdp <- read.csv("Data files/Formatted data files/gdp.csv")
# 
# # load formatted data output by script 005-Population
# population <- read.csv("Data files/Formatted data files/population.csv")
# 
# # merges GDP and population datasets to calculate aid as % of GDP and per capita
# aid2 <- aid2 %>%
#   dplyr::full_join(gdp,by=c("iso3c","year")) %>%
#   dplyr::full_join(population,by=c("iso3c","country","year")) %>%
#   # drop growth rate variables
#   dplyr::select(-c(gdp.pwt.original,gdp.gl.original,gdp.growth.rate.pwt.original,gdp.growth.rate.gl.original,
#                    gdp.growth.rate.pwt.est,gdp.growth.rate.gl.est,imf.growth.rate.modern,
#                    imf.growth.rate.historical,wb.growth.rate,pop.growth.rate.un,pop.growth.rate.cow))
#   
# ### calculate % of gdp and per capita metrics ----------------------------------------------------------------------
# aid2 <- aid2 %>%
#   dplyr::mutate(aid.perc.gdp.pwt = aid2 / gdp.pwt.est,
#                 aid.perc.gdp.gl = aid2 / gdp.gl.est,
#                 aid.per.capita.un = aid2 / un.pop,
#                 aid.per.capita.cow = aid2 / cow.pop)
# 
# ### interpolate missing data ----------------------------------------------------------------------
# # check how many years each country has data for
# aid2.num.years <- aid2 %>%
#   na.omit() %>%
#   dplyr::group_by(iso3c) %>%
#   dplyr::tally() %>%
#   dplyr::ungroup()
# 
# # list countries with data for no years
# aid2.no.years <- unique(aid2$iso3c)[unique(aid2$iso3c) %!in% aid2.num.years$iso3c]
# 
# aid2 <- aid2 %>%
#   # filter out countries with no data
#   dplyr::filter(iso3c %!in% aid2.no.years) %>%
#   dplyr::group_by(iso3c) %>%
#   dplyr::arrange(year) %>%
#   # interpolate missing values
#   dplyr::mutate(aidgdp = imputeTS::na_interpolation(aidgdp, option = "spline")) %>%
#   dplyr::ungroup()
# 
# ### remove if countries did not exist that year ----------------------------------------------------------------------
# # load formatted data output by script 003-Country_years
# cyears <- read.csv("Data files/Formatted data files/country_years.csv")
# 
# # merge datasets and filter out if the country-year is not included in the cyears dataset
# aid2 <- aid2 %>%
#   dplyr::left_join(cyears,by=c("iso3c","year")) %>%
#   dplyr::filter(cn == 1) %>%
#   dplyr::select(-c(cn,gdp)) %>%
#   # using the countrycode package, add country name based on iso3c
#   dplyr::mutate(country = countrycode::countrycode(iso3c,"iso3c","country.name")) %>%
#   dplyr::relocate(country,.after=iso3c)
# 
# # adds country names for iso3c codes not in the countrycode package: BRD, DDR, RVN, SOV, YAR, YPR, YUG
# aid2$country[aid2$iso3c=="BRD"] <- "West Germany"
# aid2$country[aid2$iso3c=="DDR"] <- "East Germany"
# aid2$country[aid2$iso3c=="RVN"] <- "South Vietnam"
# aid2$country[aid2$iso3c=="SOV"] <- "Soviet Union"
# aid2$country[aid2$iso3c=="YAR"] <- "North Yemen"
# aid2$country[aid2$iso3c=="YPR"] <- "South Yemen"
# aid2$country[aid2$iso3c=="YUG"] <- "Yugoslavia"
# 
# ### write data ----------------------------------------------------------------------
# # writes formatted dataframe as csv files
# write.csv(aid2,"Data files/Formatted data files/aid.csv",row.names = FALSE)
