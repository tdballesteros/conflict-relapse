# This script formats two GDP estimates and creates a composite estimate score.

# TODO
## gl but no pwt countries: AFG, AND, CUB, ERI, FSM, GUY, KIR, LBY, LIE, MCO, MHL, NRU, PLW,
## PRK, SLB, SMR, SOM, SYC, TLS, TON, TUV
###
## ISR/PSE (/JOR?)
## RUS/SOV
## YUG
## NAM/ZAF
## MYS/SGP

### load libraries ----------------------------------------------------------------------
library(readxl)
library(utils)
library(countrycode)
library(tibble)
library(dplyr)

### not in function ----------------------------------------------------------------------
'%!in%' <- function(x,y)!('%in%'(x,y))

### load and format data ----------------------------------------------------------------------
#### Penn World Tables ----------------------------------------------------------------------
# IMF GDP growth rates from https://www.imf.org/external/datamapper/NGDP_RPCH@WEO/SSD?zoom=SSD&highlight=SSD
# Real GDP at constant 2011 national prices (in mil. 2011US$)
pwt <- readxl::read_excel("Data files/Raw data files/pwt91.xlsx", sheet = 3) %>%
  dplyr::select(country,year,rgdpna) %>%
  # convert real gdp to 2019$
  dplyr::mutate(rgdpna = rgdpna * 1000000 * (1 + 0.14),
                # using the countrycode package, add iso3c based on country name
                iso3c = countrycode::countrycode(country,"country.name","iso3c"),
                # using the countrycode package, add iso3c based on country name
                country = countrycode::countrycode(iso3c,"iso3c","country.name"))  %>%
  # move iso3c variable first
  dplyr::relocate(iso3c, .before = country) %>%
  # filter non-sovereign entities
  dplyr::filter(iso3c %!in% c("ABW","AIA","BMU","CUW","CYM","HKG","MAC","MSR","SXM","SYC","TCA","VGB")) %>%
  dplyr::rename(gdp.pwt = rgdpna)

# Note: PSE and ISR are coded separately in this dataset

#### Gleditsch ----------------------------------------------------------------------
# realgdp	- total real GDP, 2005 
gdpgl <- utils::read.delim("Data files/Raw data files/gdpv6.txt") %>%
  # using the countrycode package, add iso3c based on country name
  dplyr::mutate(iso3c = countrycode::countrycode(stateid,"gwc","iso3c")) %>%
  # move iso3c variable first
  dplyr::relocate(iso3c, .before = statenum)

# codes iso3c values missing from the countrycode package
gdpgl$iso3c[gdpgl$stateid=="AAB"] <- "ATG"
gdpgl$iso3c[gdpgl$stateid=="ABK"] <- "ABK" # Abkhazia
gdpgl$iso3c[gdpgl$stateid=="AND"] <- "AND"
gdpgl$iso3c[gdpgl$stateid=="CZE"] <- "CZE" # Czechoslovakia
gdpgl$iso3c[gdpgl$stateid=="DMA"] <- "DMA"
gdpgl$iso3c[gdpgl$stateid=="DRV"] <- "VNM" # North Vietnam and unified Vietnam
gdpgl$iso3c[gdpgl$stateid=="FSM"] <- "FSM"
gdpgl$iso3c[gdpgl$stateid=="GDR"] <- "DDR"
gdpgl$iso3c[gdpgl$stateid=="GRN"] <- "GRD"
gdpgl$iso3c[gdpgl$stateid=="KBI"] <- "KIR"
gdpgl$iso3c[gdpgl$stateid=="KOS"] <- "KSV"
gdpgl$iso3c[gdpgl$stateid=="LIE"] <- "LIE"
gdpgl$iso3c[gdpgl$stateid=="MNC"] <- "MCO"
gdpgl$iso3c[gdpgl$stateid=="MSI"] <- "MHL"
gdpgl$iso3c[gdpgl$stateid=="NAU"] <- "NRU"
gdpgl$iso3c[gdpgl$stateid=="PAL"] <- "PLW"
gdpgl$iso3c[gdpgl$stateid=="SEY"] <- "SYC"
gdpgl$iso3c[gdpgl$stateid=="SKN"] <- "KNA"
gdpgl$iso3c[gdpgl$stateid=="SLU"] <- "LCA"
gdpgl$iso3c[gdpgl$stateid=="SMN"] <- "SLB"
gdpgl$iso3c[gdpgl$stateid=="SOT"] <- "SOT" # South Osseita
gdpgl$iso3c[gdpgl$stateid=="STP"] <- "STP"
gdpgl$iso3c[gdpgl$stateid=="SVG"] <- "VCT"
gdpgl$iso3c[gdpgl$stateid=="TBT"] <- "TBT" # Tibet
gdpgl$iso3c[gdpgl$stateid=="TON"] <- "TON"
gdpgl$iso3c[gdpgl$stateid=="TUV"] <- "TUV"
gdpgl$iso3c[gdpgl$stateid=="VAN"] <- "VUT"
gdpgl$iso3c[gdpgl$stateid=="WSM"] <- "WSM"
gdpgl$iso3c[gdpgl$stateid=="YEM"] <- "YEM" # North Yemen and unified Yemen
gdpgl$iso3c[gdpgl$stateid=="YPR"] <- "YPR"
gdpgl$iso3c[gdpgl$stateid=="YUG"] <- "YUG"
gdpgl$iso3c[gdpgl$stateid=="ZAN"] <- "ZAN"
gdpgl$iso3c[gdpgl$stateid=="RVN"] <- "RVN"
gdpgl$iso3c[gdpgl$stateid=="SNM"] <- "SMR"

gdpgl$iso3c[gdpgl$iso3c=="DEU"&gdpgl$year<1991] <- "BRD" # recodes Germany before 1991 as West Germany
gdpgl$iso3c[gdpgl$iso3c=="YEM"&gdpgl$year<1991] <- "YAR" # recodes Yemen before 1991 as North Yemen

gdpgl <- gdpgl %>%
  # using the countrycode package, add country name based on iso3c value
  dplyr::mutate(country = countrycode::countrycode(iso3c,"iso3c","country.name")) %>%
  dplyr::select(iso3c,country,year,realgdp) %>%
  # convert real gdp to 2019$
  dplyr::mutate(realgdp = realgdp * (1 + 0.31) * 1000000) %>%
  dplyr::rename(gdp.gl = realgdp) %>%
  # filter non-sovereign entities
  dplyr::filter(iso3c %!in% c("ABK","SOT","TBT"))

# codes country name values missing from the countrycode package
gdpgl$country[gdpgl$iso3c=="BRD"] <- "West Germany"
gdpgl$country[gdpgl$iso3c=="DDR"] <- "East Germany"
gdpgl$country[gdpgl$iso3c=="KSV"] <- "Kosovo"
gdpgl$country[gdpgl$iso3c=="YUG"] <- "Yugoslavia"
gdpgl$country[gdpgl$iso3c=="YAR"] <- "North Yemen"
gdpgl$country[gdpgl$iso3c=="YPR"] <- "South Yemen"
gdpgl$country[gdpgl$iso3c=="RVN"] <- "South Vietnam"

# gdpgl$country[gdpgl$iso3c=="CZE"&gdpgl$year<1993] <- "Czechoslovakia" # recodes Czechia before 1993 as Czechoslovakia

### merge data ----------------------------------------------------------------------
gdp <- dplyr::full_join(pwt,gdpgl,by=c("iso3c","country","year")) %>%
  # use pwt estimates as a baseline for the estimated gdp.pwt.est variable, adjusted below
  dplyr::mutate(gdp.pwt.est = gdp.pwt,
                # use gl estimates as a baseline for the estimated gdp.gl.est variable, adjusted below
                gdp.gl.est = gdp.gl)

### gdp growth estimator functions ----------------------------------------------------------------------
# this function is used to estimate pwt gdp data based on the relative difference in the size of the economy
# between two years within the gl gdp data and applying the proportion to the pwt gdp data
gdp_growth_estimator_pwt_func <- function(df = gdp, iso, yr){
  
  # the gdp.gl baseline to estimate the proportions from
  baseline <- df$gdp.gl[df$iso3c==iso&df$year==yr]
  
  if(is.na(baseline)){
    # if baseline based on gdp.gl is NA, use gdp.gl.est
    baseline <- df$gdp.gl.est[df$iso3c==iso&df$year==yr]
  }
  
  # the gdp.pwt relative gdp to apply the proportions to
  relative <- df$gdp.pwt[df$iso3c==iso&df$year==yr]
  
  if(is.na(relative)){
    # if relative based on gdp.pwt is NA, use gdp.pwt.est
    relative <- df$gdp.pwt.est[df$iso3c==iso&df$year==yr]
  }
  
  df <- df %>%
    dplyr::mutate(prop = relative * gdp.gl / baseline,
                  gdp.pwt.est = ifelse(iso3c==iso&is.na(gdp.pwt.est),prop,gdp.pwt.est)) %>%
    dplyr::select(-prop)
  
  return(df)
  
}

# this function is used to estimate gl gdp data based on the relative difference in the size of the economy
# between two years within the pwt gdp data and applying the proportion to the gl gdp data
gdp_growth_estimator_gl_func <- function(df = gdp, iso, yr){
  
  # the gdp.pwt baseline to estimate the proportions from
  baseline <- df$gdp.pwt[df$iso3c==iso&df$year==yr]
  
  if(is.na(baseline)){
    # if baseline based on gdp.pwt is NA, use gdp.pwt.est
    baseline <- df$gdp.pwt.est[df$iso3c==iso&df$year==yr]
  }
  
  # the gdp.gl relative gdp to apply the proportions to
  relative <- df$gdp.gl[df$iso3c==iso&df$year==yr]
  
  if(is.na(relative)){
    # if baseline based on gdp.gl is NA, use gdp.gl.est
    relative <- df$gdp.gl.est[df$iso3c==iso&df$year==yr]
  }
  
  df <- df %>%
    dplyr::mutate(prop = relative * gdp.pwt / baseline,
                  gdp.gl.est = ifelse(iso3c==iso&is.na(gdp.gl.est),prop,gdp.gl.est)) %>%
    dplyr::select(-prop)
  
  return(df)
  
}

### calculate estimates ----------------------------------------------------------------------

#### AFG(x) ----------------------------------------------------------------------
# no gdp.pwt data, so use gdp.gl data as an estimate
gdp$gdp.pwt.est[gdp$iso3c=="AFG"] <- gdp$gdp.gl[gdp$iso3c=="AFG"]

# # calculate the average differences (absolute differences and % differences) between
# # pwt and gl data for countries most comparable to AFG: PAK, IRN, TJK, TKM, KGZ, UZB
# gdp_afg_compare <- gdp %>%
#   dplyr::filter(iso3c %in% c("PAK","IRN","TJK","TKM","KGZ","UZB","BGD")) %>%
#   # only comparing original data (after converting to 2019$), not estimates
#   dplyr::select(-c(gdp.pwt.est,gdp.gl.est)) %>%
#   # for purposes of this comparison, recode BGD 1950-1970 as PAK for pwt estimates, as
#   # gl estimates for PAK include BGD before BGD independence, while pwt estimates do not
#   dplyr::mutate(gdp.gl = ifelse(iso3c=="BGD",NA,gdp.gl),
#                 iso3c = ifelse(iso3c=="BGD"&year<1971,"PAK",iso3c)) %>%
#   dplyr::group_by(iso3c,year) %>%
#   dplyr::summarise(gdp.pwt = sum(gdp.pwt,na.rm=TRUE),
#                   gdp.gl = sum(gdp.gl,na.rm=TRUE)) %>%
#   dplyr::ungroup() %>%
#   dplyr::filter(iso3c != "BGD") %>%
#   # converts gdp values of 0 back into NAs and calculate comparison metrics
#   dplyr::mutate(gdp.pwt = ifelse(gdp.pwt==0,NA,gdp.pwt),
#                 gdp.gl = ifelse(gdp.gl==0,NA,gdp.gl),
#                 # BGD pwt estimates start in 1959, so prior years' estimates are omitting East Pakistan;
#                 # replace estimates with NA
#                 gdp.pwt = ifelse(iso3c=="PAK"&year<1959,NA,gdp.pwt),
#                 pwt.minus.gl = gdp.pwt - gdp.gl,
#                 pwt.perc.gl = gdp.pwt/gdp.gl)

# # average differences by year (collapse countries)
# gdp_afg_compare_year <- gdp_afg_compare %>%
#   dplyr::select(iso3c,year,pwt.minus.gl,pwt.perc.gl) %>%
#   na.omit() %>%
#   dplyr::group_by(year) %>%
#   dplyr::summarise(pwt.minus.gl.avg = mean(pwt.minus.gl,na.rm=TRUE),
#                    pwt.perc.gl.avg = mean(pwt.perc.gl,na.rm=TRUE),
#                    n = n()) %>%
#   dplyr::ungroup()

# # average differences by country (collapse years)
# gdp_afg_compare_country <- gdp_afg_compare %>%
#   dplyr::select(iso3c,year,pwt.minus.gl,pwt.perc.gl) %>%
#   na.omit() %>%
#   dplyr::group_by(iso3c) %>%
#   dplyr::summarise(pwt.minus.gl.avg = mean(pwt.minus.gl,na.rm=TRUE),
#                    pwt.perc.gl.avg = mean(pwt.perc.gl,na.rm=TRUE),
#                    n = n()) %>%
#   dplyr::ungroup()
# # 5/6 countries (PAK, TJK, TKM, KGZ, UZB) match closesly between pwt and gl metrics, while
# # IRN is slightly over twice as large in the pwt estimates versus the gl estimates.

# # glm
# gdp_afg_compare_glm <- stats::glm(pwt.perc.gl ~ iso3c + year + log(gdp.gl), data = gdp_afg_compare)
# summary(gdp_afg_compare_glm)

#### AGO ----------------------------------------------------------------------
# 1970-1974: AGO coded as gaining independence in 1975

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "AGO", 2011)

#### ALB ----------------------------------------------------------------------
# 1950-69: apply gdp.gl proportion to 1970 gdp.gl estimate and use that ratio
# on gdp.pwt estimate
gdp <- gdp_growth_estimator_pwt_func(gdp, "ALB", 1970)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "ALB", 2011)

#### AND(x) ----------------------------------------------------------------------
# no gdp.pwt data, so use gdp.gl data as an estimate
gdp$gdp.pwt.est[gdp$iso3c=="AND"] <- gdp$gdp.gl[gdp$iso3c=="AND"]

#### ARE ----------------------------------------------------------------------
# 1970: ARE coded as gaining independence in 1971

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "ARE", 2011)

#### ARG ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "ARG", 2011)

# # plot gdp.pwt.est and gdp.gl.est line graphs
# gdp.arg <- gdp %>%
#   dplyr::filter(iso3c == "ARG") %>%
#   dplyr::arrange(year)

# plot(gdp.arg$year,gdp.arg$gdp.pwt.est, type = 'l')
# lines(gdp.arg$year,gdp.arg$gdp.gl.est, type = 'l')

#### ARM ----------------------------------------------------------------------
# 1990: ARM coded as gaining independence in 1991

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "ARM", 2011)

#### ATG ----------------------------------------------------------------------
# 1970-1980: ATG coded as gaining independence in 1981

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "ATG", 2011)

#### AUS ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "AUS", 2011)

#### AUT ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "AUT", 2011)

#### AZE ----------------------------------------------------------------------
# 1990: AZE coded as gaining independence in 1991

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "AZE", 2011)

#### BDI ----------------------------------------------------------------------
# 1960-1961: BDI coded as gaining independence in 1962

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "BDI", 2011)

#### BEL ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "BEL", 2011)

#### BEN ----------------------------------------------------------------------
# 1959: BEN coded as gaining independence in 1960

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "BEN", 2011)

#### BFA ----------------------------------------------------------------------
# 1959: BFA coded as gaining independence in 1960

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "BFA", 2011)

#### BGD/PAK(x) ----------------------------------------------------------------------
# pwt codes PAK and BGD separate prior to 1971; gl codes PAK as unified until 1971,
# with PAK not including East Pakistan/Bangladesh starting that year

# BGD coded as gaining independence in 1971

# pwt estimates code PAK and BGD separate through 1971, though BGD estimates only start in 1959
# (Method 1) extend BGD estimates back based on PAK (pwt) growth rates
# (Method 2) combine PAK and BGD, then extend back using PAK (gl) growth rates, removing 1950-1958 PAK pwt estimates

# Method 1
# extend BGD pwt estimates back to 1950 using PAK pwt growth rates, then combine BGD and PAK 1950-1970

# pull dataset of just PAK to calculate growth rates
gdp.bgd.method1.growth.rates <- gdp %>%
  dplyr::filter(iso3c == "PAK")

# pull reference year to calculate the growth rates relative to 1959
gdp.bgd.method1.ref.gdp <- gdp.bgd.method1.growth.rates$gdp.pwt.est[gdp.bgd.method1.growth.rates$year==1959]

# calculate growth rates relative to 1959
gdp.bgd.method1.growth.rates <- gdp.bgd.method1.growth.rates %>%
  dplyr::mutate(multiplier = gdp.pwt.est / gdp.bgd.method1.ref.gdp) %>%
  dplyr::select(year,multiplier)

# pull dataset of just BGD to apply growth rates to
gdp.bgd.method1.estimates <- gdp %>%
  dplyr::filter(iso3c == "BGD") %>%
  dplyr::left_join(gdp.bgd.method1.growth.rates,by="year")

gdp.bgd.method1.ref.gdp <- gdp.bgd.method1.estimates$gdp.pwt.est[gdp.bgd.method1.estimates$year==1959]

gdp.bgd.method1.estimates <- gdp.bgd.method1.estimates %>%
  dplyr::mutate(gdp.pwt.est = ifelse(is.na(gdp.pwt.est),gdp.bgd.method1.ref.gdp*multiplier,gdp.pwt.est)) %>%
  dplyr::select(iso3c,year,gdp.pwt.est)

gdp.bgd.method1.combined <- gdp %>%
  dplyr::filter(iso3c == "PAK",
                year < 1971) %>%
  dplyr::select(iso3c,year,gdp.pwt.est) %>%
  rbind(gdp.bgd.method1.estimates) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(gdp.pwt.est = sum(gdp.pwt.est,na.rm=TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::rename(gdp.pwt.est.method1 = gdp.pwt.est)

# Method 2
# remove pwt PAK 1950-1958 estimates, combine pwt PAK and BGD estimates, and use PAK gl growth rates to extend back to 1950

# pull PAK and BGD gdp data
gdp.bgd.method2 <- gdp %>%
  dplyr::filter(iso3c %in% c("PAK","BGD"))

# remove PAK pwt estimates from 1950-1958
gdp.bgd.method2$gdp.pwt.est[gdp.bgd.method2$iso3c=="PAK"&gdp.bgd.method2$year %in% c(1950:1958)] <- NA

gdp.bgd.method2.combined <- gdp.bgd.method2 %>%
  # filter only years prior to BGD independence
  dplyr::filter(year %in% c(1950:1970)) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(gdp.pwt.est = sum(gdp.pwt.est,na.rm=TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(gdp.pwt.est = ifelse(gdp.pwt.est==0,NA,gdp.pwt.est))

gdp.bgd.method2.growth.rates <- gdp %>%
  dplyr::filter(iso3c == "PAK")

gdp.bgd.method2.ref.gdp <- gdp.bgd.method2.growth.rates$gdp.gl.est[gdp.bgd.method2.growth.rates$year==1959]

gdp.bgd.method2.growth.rates <- gdp.bgd.method2.growth.rates %>%
  dplyr::mutate(multiplier = gdp.gl.est / gdp.bgd.method2.ref.gdp) %>%
  dplyr::select(year,multiplier)

gdp.bgd.method2.ref.gdp2 <- gdp.bgd.method2.combined$gdp.pwt.est[gdp.bgd.method2.combined$year==1959]

gdp.bgd.method2.combined <- gdp.bgd.method2.combined %>%
  dplyr::left_join(gdp.bgd.method2.growth.rates,by="year") %>%
  dplyr::mutate(gdp.pwt.est = ifelse(is.na(gdp.pwt.est),gdp.bgd.method2.ref.gdp2*multiplier,gdp.pwt.est)) %>%
  dplyr::select(-multiplier) %>%
  dplyr::rename(gdp.pwt.est.method2 = gdp.pwt.est)

# combine methods
gdp.bgd.combined.methods <- dplyr::full_join(gdp.bgd.method1.combined,gdp.bgd.method2.combined,by="year") %>%
  dplyr::filter(year < 1971) %>%
  dplyr::mutate(iso3c = "PAK",
                # use the average between the two methods as the pwt estimate
                gdp.pwt.est.avg = (gdp.pwt.est.method1 + gdp.pwt.est.method2) / 2) %>%
  dplyr::select(iso3c,year,gdp.pwt.est.avg)

gdp <- gdp %>%
  dplyr::left_join(gdp.bgd.combined.methods,by=c("iso3c","year")) %>%
  dplyr::mutate(gdp.pwt.est = ifelse(iso3c=="PAK"&year %in% c(1950:1970),gdp.pwt.est.avg,gdp.pwt.est)) %>%
  dplyr::select(-gdp.pwt.est.avg)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "BGD", 2011)
gdp <- gdp_growth_estimator_gl_func(gdp, "PAK", 2011)

#### BGR ----------------------------------------------------------------------
# 1950-69: apply gdp.gl proportion to 1970 gdp.gl estimate and use that ratio
# on gdp.pwt estimate
gdp <- gdp_growth_estimator_pwt_func(gdp, "BGR", 1970)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "BGR", 2011)

#### BHR ----------------------------------------------------------------------
# 1970: BHR coded as gaining independence in 1971

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "BHR", 2011)

#### BHS ----------------------------------------------------------------------
# 1970-1972: BHS coded as gaining independence in 1973

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "BHS", 2011)

#### BIH ----------------------------------------------------------------------
# 1990-1991: BIH coded as gaining independence in 1992

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "BIH", 2011)

#### BLR ----------------------------------------------------------------------
# 1990: BLR coded as gaining independence in 1991

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "BLR", 2011)

#### BLZ ----------------------------------------------------------------------
# 1970-1980: BLZ coded as gaining independence in 1981

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "BLZ", 2011)

#### BOL ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "BOL", 2011)

#### BRA ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "BRA", 2011)

#### BRB ----------------------------------------------------------------------
# 1960-1965: BRB coded as gaining independence in 1966

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "BRB", 2011)

#### BRN ----------------------------------------------------------------------
# 1970-1983: BRB coded as gaining independence in 1984

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "BRN", 2011)

#### BTN ----------------------------------------------------------------------
# 1950-1969: BTN coded as gaining independence in 1971 (both pwt and gl already have
# values for 1970)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "BTN", 2011)

#### BWA ----------------------------------------------------------------------
# 1960-1965: BRB coded as gaining independence in 1966

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "BWA", 2011)

#### CAF ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "CAF", 2011)

#### CAN ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "CAN", 2011)

#### CHE ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "CHE", 2011)

#### CHL ----------------------------------------------------------------------
# 1950: apply gdp.gl proportion to 1951 gdp.gl estimate and use that ratio
# on gdp.pwt estimate
gdp <- gdp_growth_estimator_pwt_func(gdp, "CHL", 1951)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "CHL", 2011)

#### CHN ----------------------------------------------------------------------
# 1950-51: apply gdp.gl proportion to 1952 gdp.gl estimate and use that ratio
# on gdp.pwt estimate
gdp <- gdp_growth_estimator_pwt_func(gdp, "CHN", 1952)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "CHN", 2011)

#### CIV ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "CHN", 2011)

#### CMR ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "CHN", 2011)

#### COD ----------------------------------------------------------------------
# 1950-1959: COD coded as gaining independence in 1960

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "COD", 2011)

#### COG ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "COG", 2011)

#### COL ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "COL", 2011)

#### COM ----------------------------------------------------------------------
# 1960-1974: COM coded as gaining independence in 1975

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "COM", 2011)

#### CPV ----------------------------------------------------------------------
# 1960-1974: CPV coded as gaining independence in 1975

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "CPV", 2011)

#### CRI ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "CRI", 2011)

#### CUB(x) ----------------------------------------------------------------------
# no gdp.pwt data, so use gdp.gl data as an estimate
gdp$gdp.pwt.est[gdp$iso3c=="CUB"] <- gdp$gdp.gl[gdp$iso3c=="CUB"]

#### CYP ----------------------------------------------------------------------
# 1950-1959: CYP coded as gaining independence in 1960

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "CYP", 2011)

#### CZE ----------------------------------------------------------------------
# 1950-89: apply gdp.gl proportion to 1990 gdp.gl estimate and use that ratio
# on gdp.pwt estimate
gdp <- gdp_growth_estimator_pwt_func(gdp, "CZE", 1990)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "CZE", 2011)

#### DEU/BRD/DDR ----------------------------------------------------------------------
# pwt data combines East and West Germany, while gl separates East and West Germany through
# to 1990 (inclusive)
# East and West Germany are coded as existing through 1989 (inclusive), with a unified
# Germany coded as beginning starting in 1990

# calculate ratio between gl's East and West Germany GDPs
gdp_split_germany <- gdp %>%
  dplyr::select(iso3c,year,gdp.gl) %>%
  dplyr::filter(iso3c %in% c("BRD","DDR"),
                # filter out 1990, which is coded as a unified Germany
                year < 1990) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(multiplier = gdp.gl / sum(gdp.gl,na.rm=TRUE)) %>%
  dplyr::ungroup()

# pull pwt's unified German GDP data for before reunification
gdp_combined_germany <- gdp %>%
  dplyr::filter(iso3c == "DEU",
                # filter out 1990, which is coded as a unified Germany
                year < 1990) %>%
  dplyr::select(-c(iso3c,gdp.gl)) %>%
  # merge split East and West Germany ratios to combined Germany datasest
  dplyr::full_join(gdp_split_germany,by="year") %>%
  dplyr::mutate(gdp.pwt.est = gdp.pwt * multiplier,
                country = ifelse(iso3c=="BRD","West Germany","East Germany"),
                gdp.gl.est = gdp.gl) %>%
  dplyr::select(-multiplier)

# calculate 1990 estimates for pwt and gl
deu.1990.pwt <- gdp$gdp.pwt[gdp$iso3c=="DEU"&gdp$year==1990]
deu.1990.gl <- gdp$gdp.gl[gdp$iso3c=="BRD"&gdp$year==1990] + gdp$gdp.gl[gdp$iso3c=="DDR"&gdp$year==1990]

gdp <- gdp %>%
  # filter out entries for Germany 1950-1990
  dplyr::filter(iso3c %!in% c("BRD","DDR","DEU") | year > 1990) %>%
  rbind(gdp_combined_germany) %>%
  tibble::add_row(iso3c = "DEU", country = "Germany", year = 1990,
                  gdp.pwt = deu.1990.pwt, gdp.gl = NA,
                  gdp.pwt.est = deu.1990.pwt, gdp.gl.est = deu.1990.gl)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "DEU", 2011)

#### DJI ----------------------------------------------------------------------
# 1970-1976: DJI coded as gaining independence in 1977

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "DJI", 2011)

#### DMA ----------------------------------------------------------------------
# 1970-1977: DMA coded as gaining independence in 1978

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "DMA", 2011)

#### DNK ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "DNK", 2011)

#### DOM ----------------------------------------------------------------------
# 1950: apply gdp.gl proportion to 1951 gdp.gl estimate and use that ratio
# on gdp.pwt estimate
gdp <- gdp_growth_estimator_pwt_func(gdp, "DOM", 1951)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "DOM", 2011)

#### DZA ----------------------------------------------------------------------
# 1960-1961: DZA coded as gaining independence in 1962

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "DZA", 2011)

#### ECU ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "ECU", 2011)

#### EGY ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "EGY", 2011)

#### ERI(x) ----------------------------------------------------------------------
# no gdp.pwt data, so use gdp.gl data as an estimate
gdp$gdp.pwt.est[gdp$iso3c=="ERI"] <- gdp$gdp.gl[gdp$iso3c=="ERI"]

#### ESP ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "ESP", 2011)

#### ETH ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "ETH", 2011)

#### FIN ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "FIN", 2011)

#### FJI ----------------------------------------------------------------------
# 1960-1969: FJI coded as gaining independence in 1970

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "FIN", 2011)

#### FRA ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "FRA", 2011)

#### FSM(x) ----------------------------------------------------------------------
# no gdp.pwt data, so use gdp.gl data as an estimate
gdp$gdp.pwt.est[gdp$iso3c=="FSM"] <- gdp$gdp.gl[gdp$iso3c=="FSM"]

#### GAB ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "GAB", 2011)

#### GBR ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "GBR", 2011)

#### GEO ----------------------------------------------------------------------
# 1990: GEO coded as gaining independence in 1991

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "GEO", 2011)

#### GHA ----------------------------------------------------------------------
# 1955-1956: GHA coded as gaining independence in 1957

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "GHA", 2011)

#### GIN ----------------------------------------------------------------------
# 1958: apply gdp.gl proportion to 1959 gdp.gl estimate and use that ratio
# on gdp.pwt estimate
gdp <- gdp_growth_estimator_pwt_func(gdp, "GIN", 1959)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "GIN", 2011)

#### GMB ----------------------------------------------------------------------
# 1960-1964: GHA coded as gaining independence in 1965

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "GMB", 2011)

#### GNB ----------------------------------------------------------------------
# 1960-1973: GNB coded as gaining independence in 1974

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "GNB", 2011)

#### GNQ ----------------------------------------------------------------------
# 1960-1967: GNQ coded as gaining independence in 1968

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "GNQ", 2011)

#### GRC ----------------------------------------------------------------------
# 1950: apply gdp.gl proportion to 1951 gdp.gl estimate and use that ratio
# on gdp.pwt estimate
gdp <- gdp_growth_estimator_pwt_func(gdp, "GRC", 1951)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "GRC", 2011)

#### GRD ----------------------------------------------------------------------
# 1970-1993: GRD coded as gaining independence in 1974

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "GRD", 2011)

#### GTM ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "GTM", 2011)

#### GUY(x) ----------------------------------------------------------------------
# no gdp.pwt data, so use gdp.gl data as an estimate
gdp$gdp.pwt.est[gdp$iso3c=="GUY"] <- gdp$gdp.gl[gdp$iso3c=="GUY"]

#### HND ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "HND", 2011)

#### HRV ----------------------------------------------------------------------
# 1990: HRV coded as gaining independence in 1992 (both pwt and gl already have
# values for 1991)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "HND", 2011)

#### HTI ----------------------------------------------------------------------
# 1950-1959: apply gdp.gl proportion to 1960 gdp.gl estimate and use that ratio
# on gdp.pwt estimate
gdp <- gdp_growth_estimator_pwt_func(gdp, "HTI", 1960)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "HND", 2011)

#### HUN ----------------------------------------------------------------------
# 1950-1969: apply gdp.gl proportion to 1970 gdp.gl estimate and use that ratio
# on gdp.pwt estimate
gdp <- gdp_growth_estimator_pwt_func(gdp, "HUN", 1970)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "HUN", 2011)

#### IDN ----------------------------------------------------------------------
# 1950-1959: apply gdp.gl proportion to 1960 gdp.gl estimate and use that ratio
# on gdp.pwt estimate
gdp <- gdp_growth_estimator_pwt_func(gdp, "IDN", 1960)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "IDN", 2011)

#### IND ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "IND", 2011)

#### IRL ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "IRL", 2011)

#### IRN ----------------------------------------------------------------------
# 1950-1954: apply gdp.gl proportion to 1955 gdp.gl estimate and use that ratio
# on gdp.pwt estimate
gdp <- gdp_growth_estimator_pwt_func(gdp, "IRN", 1955)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "IRN", 2011)

#### IRQ ----------------------------------------------------------------------
# 1950-1969: apply gdp.gl proportion to 1970 gdp.gl estimate and use that ratio
# on gdp.pwt estimate
gdp <- gdp_growth_estimator_pwt_func(gdp, "IRQ", 1970)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "IRQ", 2011)

#### ISL ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "ISL", 2011)

#### ISR/PSE(x) ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "ISR", 2011)


gdp.isr.pse <- gdp %>%
  dplyr::filter(iso3c %in% c("ISR","PSE"))

#### ITA ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "ITA", 2011)

#### JAM ----------------------------------------------------------------------
# 1953-1961: JAM coded as gaining independence in 1962

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "ITA", 2011)

#### JOR ----------------------------------------------------------------------
# 1950-1953: apply gdp.gl proportion to 1954 gdp.gl estimate and use that ratio
# on gdp.pwt estimate
gdp <- gdp_growth_estimator_pwt_func(gdp, "JOR", 1970)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "JOR", 2011)

#### JPN ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "JPN", 2011)

#### KAZ ----------------------------------------------------------------------
# 1990: KAZ coded as gaining independence in 1991

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "JPN", 2011)

#### KEN ----------------------------------------------------------------------
# 1950-1962: KEN coded as gaining independence in 1963

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "KEN", 2011)

#### KGZ ----------------------------------------------------------------------
# 1990: KGZ coded as gaining independence in 1991

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "KGZ", 2011)

#### KHM ----------------------------------------------------------------------
# 1953-1969: apply gdp.gl proportion to 1970 gdp.gl estimate and use that ratio
# on gdp.pwt estimate
gdp <- gdp_growth_estimator_pwt_func(gdp, "KHM", 1970)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "KHM", 2011)

#### KIR(x) ----------------------------------------------------------------------
# no gdp.pwt data, so use gdp.gl data as an estimate
gdp$gdp.pwt.est[gdp$iso3c=="KIR"] <- gdp$gdp.gl[gdp$iso3c=="KIR"]

#### KNA ----------------------------------------------------------------------
# 1970-1982: KGZ coded as gaining independence in 1983

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "KNA", 2011)

#### KOR ----------------------------------------------------------------------
# 1950-1952: apply gdp.gl proportion to 1953 gdp.gl estimate and use that ratio
# on gdp.pwt estimate
gdp <- gdp_growth_estimator_pwt_func(gdp, "KOR", 1970)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "KOR", 2011)

#### KSV/MNE/SRB/YUG(x) ----------------------------------------------------------------------
# Data time frames (note all gl data extends only through 2011, inclusive):
## BIH - pwt 1990-; gl 1992-; coded as starting 1992
## HRV - pwt 1990-; gl 1991-; coded as starting 1992
## MKD - pwt 1990-; gl 1991-; coded as starting 1992
## SLN - pwt 1990-; gl 1992-; coded as starting 1992
## KSV - pwt no data; gl 2008-; coded as starting 2008
## MNE - pwt 1990-; gl 2006-; coded as starting in 2006
## SRB - pwt 1990-; gl 2006-; coded as starting in 1992
## YUG - pwt no data; gl 1950-2006; coded as ending 1991

## YUG 1992-2005 gl data is for Serbia and Montenegro
## SRB 2008- pwt data is for both Serbia and Kosovo
gdp.srb.ksv <- gdp %>%
  dplyr::filter(iso3c %in% c("SRB","KSV"),
                year >= 2008) %>%
  # calculate gl ratio between SRB and KSV GDP for 2008-2011
  dplyr::group_by(year) %>%
  dplyr::mutate(multiplier = gdp.gl / sum(gdp.gl,na.rm=TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::select(iso3c,year,multiplier) %>%
  dplyr::full_join(expand.grid(iso3c = c("SRB","KSV"), year = c(2008:2017))) %>%
  dplyr::group_by(iso3c) %>%
  dplyr::arrange(year) %>%
  # extends 2011 ratios through 2017
  dplyr::mutate(multiplier = imputeTS::na_interpolation(multiplier, option = "linear")) %>%
  dplyr::ungroup()
  
# SRB pwt data 2008-2017
srb.pwt.08.17 <- gdp %>%
  dplyr::filter(iso3c == "SRB",
                year %in% c(2008:2017)) %>%
  dplyr::select(year,gdp.pwt)

# merge SRB pwt estimates with ratio dataset
gdp.srb.ksv <- gdp.srb.ksv %>%
  dplyr::full_join(srb.pwt.08.17,by="year") %>%
  dplyr::mutate(gdp.pwt.est2 = gdp.pwt * multiplier) %>%
  dplyr::select(-c(multiplier,gdp.pwt))

# merge gdp.pwt.est2 estimates with main gdp dataset
gdp <- gdp %>%
  dplyr::full_join(gdp.srb.ksv,by=c("iso3c","year")) %>%
  dplyr::mutate(gdp.pwt.est = ifelse(iso3c %in% c("SRB","KSV")&year %in% c(2008:2017),gdp.pwt.est2,gdp.pwt.est)) %>%
  dplyr::select(-gdp.pwt.est2)
  
## SRB and MNE coded separately gdp.pwt 1991-2005 and together (as YUG) for gl ???-2006
## note gl has YUG, SRB, and MNE values for 2006, though SRB alone is larger than YUG
gdp.srb.mne <- gdp %>%
  dplyr::filter(iso3c %in% c("YUG","SRB","MNE")) %>%
  dplyr::filter(year %in% c(1991:2005)) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(gdp.pwt.est = sum(gdp.pwt,na.rm=TRUE),
                   gdp.gl.est = sum(gdp.gl,na.rm=TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(iso3c = "SRB",
                country = "Serbia and Montenegro",
                gdp.pwt = NA,
                gdp.gl = gdp.gl.est)

gdp <- gdp %>%
  # filter out YUG/SRB/MNE for 1991-2005
  dplyr::filter(iso3c %!in% c("YUG","SRB","MNE") | year %!in% c(1991:2005)) %>%
  rbind(gdp.srb.mne)




gdp.yug.full <- gdp %>%
  dplyr::filter(iso3c %in% c("YUG","KSV","SRB","MNE","MKD","HRV","BIH","SVN"))

gdp.srb.ksv.mne <- gdp %>%
  dplyr::filter(iso3c %in% c("YUG","KSV","SRB","MNE")) %>%
  dplyr::filter(year > 1990)



# KSV and SRB
gdp$rgdpna[gdp$iso3c=="SRB"&gdp$year==2008] <- gdp$rgdpna[gdp$iso3c=="SRB"&gdp$year==2008]*0.823933441
gdp$rgdpna[gdp$iso3c=="SRB"&gdp$year==2009] <- gdp$rgdpna[gdp$iso3c=="SRB"&gdp$year==2009]*0.822333889
gdp$rgdpna[gdp$iso3c=="SRB"&gdp$year==2010] <- gdp$rgdpna[gdp$iso3c=="SRB"&gdp$year==2010]*0.841862199
gdp$rgdpna[gdp$iso3c=="SRB"&gdp$year==2011] <- gdp$rgdpna[gdp$iso3c=="SRB"&gdp$year==2011]*0.831400579
gdp$rgdpna[gdp$iso3c=="SRB"&gdp$year==2012] <- gdp$rgdpna[gdp$iso3c=="SRB"&gdp$year==2012]*0.831400579
gdp$rgdpna[gdp$iso3c=="SRB"&gdp$year==2013] <- gdp$rgdpna[gdp$iso3c=="SRB"&gdp$year==2013]*0.831400579
gdp$rgdpna[gdp$iso3c=="SRB"&gdp$year==2014] <- gdp$rgdpna[gdp$iso3c=="SRB"&gdp$year==2014]*0.831400579
gdp$rgdpna[gdp$iso3c=="SRB"&gdp$year==2015] <- gdp$rgdpna[gdp$iso3c=="SRB"&gdp$year==2015]*0.831400579
gdp$rgdpna[gdp$iso3c=="SRB"&gdp$year==2016] <- gdp$rgdpna[gdp$iso3c=="SRB"&gdp$year==2016]*0.831400579
gdp$rgdpna[gdp$iso3c=="SRB"&gdp$year==2017] <- gdp$rgdpna[gdp$iso3c=="SRB"&gdp$year==2017]*0.831400579

gdp$rgdpna[gdp$iso3c=="KSV"&gdp$year==2008] <- gdp$rgdpna[gdp$iso3c=="SRB"&gdp$year==2008]*0.176066559
gdp$rgdpna[gdp$iso3c=="KSV"&gdp$year==2009] <- gdp$rgdpna[gdp$iso3c=="SRB"&gdp$year==2009]*0.177666111
gdp$rgdpna[gdp$iso3c=="KSV"&gdp$year==2010] <- gdp$rgdpna[gdp$iso3c=="SRB"&gdp$year==2010]*0.158137801
gdp$rgdpna[gdp$iso3c=="KSV"&gdp$year==2011] <- gdp$rgdpna[gdp$iso3c=="SRB"&gdp$year==2011]*0.168599421
gdp$rgdpna[gdp$iso3c=="KSV"&gdp$year==2012] <- gdp$rgdpna[gdp$iso3c=="SRB"&gdp$year==2012]*0.168599421
gdp$rgdpna[gdp$iso3c=="KSV"&gdp$year==2013] <- gdp$rgdpna[gdp$iso3c=="SRB"&gdp$year==2013]*0.168599421
gdp$rgdpna[gdp$iso3c=="KSV"&gdp$year==2014] <- gdp$rgdpna[gdp$iso3c=="SRB"&gdp$year==2014]*0.168599421
gdp$rgdpna[gdp$iso3c=="KSV"&gdp$year==2015] <- gdp$rgdpna[gdp$iso3c=="SRB"&gdp$year==2015]*0.168599421
gdp$rgdpna[gdp$iso3c=="KSV"&gdp$year==2016] <- gdp$rgdpna[gdp$iso3c=="SRB"&gdp$year==2016]*0.168599421
gdp$rgdpna[gdp$iso3c=="KSV"&gdp$year==2017] <- gdp$rgdpna[gdp$iso3c=="SRB"&gdp$year==2017]*0.168599421

# YUG
# use gl
gdp$rgdpna[gdp$iso3c=="YUG"&gdp$year<=1991] <- gdp$gdp[gdp$iso3c=="YUG"&gdp$year<=1991]

# KSV
# pwt codes SRB and KSV together, even after 2008; gl codes KSV as separate beginning in 2008,
# with SRB not including Kosovo starting that year

# no gdp.pwt data, so use gdp.gl data as an estimate
gdp$gdp.pwt.est[gdp$iso3c=="KSV"] <- gdp$gdp.gl[gdp$iso3c=="KSV"]

#### KWT ----------------------------------------------------------------------
# 1961-1969: apply gdp.gl proportion to 1970 gdp.gl estimate and use that ratio
# on gdp.pwt estimate
gdp <- gdp_growth_estimator_pwt_func(gdp, "KWT", 1970)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "KWT", 2011)

#### LAO ----------------------------------------------------------------------
# 1953: LAO coded as gaining independence in 1953, though neither pwt nor gl have
# estimates for that year
# estimate 1953 by assuming 1953-1954 GDP growth is the same as 1954-1955 growth
lao.54.55.growth <- gdp$gdp.gl.est[gdp$iso3c=="LAO"&gdp$year==1955]/gdp$gdp.gl.est[gdp$iso3c=="LAO"&gdp$year==1954]

# estimate 1953 value
gdp$gdp.gl.est[gdp$iso3c=="LAO"&gdp$year==1953] <- gdp$gdp.gl.est[gdp$iso3c=="LAO"&gdp$year==1954]/lao.54.55.growth

# 1953-1969: apply gdp.gl proportion to 1970 gdp.gl estimate and use that ratio
# on gdp.pwt estimate
gdp <- gdp_growth_estimator_pwt_func(gdp, "LAO", 1970)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "LAO", 2011)

#### LBN ----------------------------------------------------------------------
# 1950-1969: apply gdp.gl proportion to 1970 gdp.gl estimate and use that ratio
# on gdp.pwt estimate
gdp <- gdp_growth_estimator_pwt_func(gdp, "LBN", 1970)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "LBN", 2011)

#### LBR ----------------------------------------------------------------------
# 1950-1963: apply gdp.gl proportion to 1964 gdp.gl estimate and use that ratio
# on gdp.pwt estimate
gdp <- gdp_growth_estimator_pwt_func(gdp, "LBR", 1964)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "LBR", 2011)

#### LBY(x) ----------------------------------------------------------------------
# no gdp.pwt data, so use gdp.gl data as an estimate
gdp$gdp.pwt.est[gdp$iso3c=="LBY"] <- gdp$gdp.gl[gdp$iso3c=="LBY"]

#### LCA ----------------------------------------------------------------------
# 1970-1978: LCA coded as gaining independence in 1979

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "LCA", 2011)

#### LIE(x) ----------------------------------------------------------------------
# no gdp.pwt data, so use gdp.gl data as an estimate
gdp$gdp.pwt.est[gdp$iso3c=="LIE"] <- gdp$gdp.gl[gdp$iso3c=="LIE"]

#### LKA ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "LKA", 2011)

#### LSO ----------------------------------------------------------------------
# 1960-1966: LSO coded as gaining independence in 1966

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "LSO", 2011)

#### LTU ----------------------------------------------------------------------
# 1990: LTU coded as gaining independence in 1991

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "LTU", 2011)

#### LUX ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "LUX", 2011)

#### LVA ----------------------------------------------------------------------
# 1990: LVA coded as gaining independence in 1991

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "LVA", 2011)

#### MAR ----------------------------------------------------------------------
# 1950-1955: MAR coded as gaining independence in 1956

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "MAR", 2011)

#### MCO(x) ----------------------------------------------------------------------
# no gdp.pwt data, so use gdp.gl data as an estimate
gdp$gdp.pwt.est[gdp$iso3c=="MCO"] <- gdp$gdp.gl[gdp$iso3c=="MCO"]

#### MDA ----------------------------------------------------------------------
# 1990: MDA coded as gaining independence in 1991

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "MDA", 2011)

#### MDG ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "MDG", 2011)

#### MDV ----------------------------------------------------------------------
# 1965-1969: apply gdp.gl proportion to 1970 gdp.gl estimate and use that ratio
# on gdp.pwt estimate
gdp <- gdp_growth_estimator_pwt_func(gdp, "MDV", 1970)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "MDV", 2011)

#### MEX ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "MEX", 2011)

#### MHL(x) ----------------------------------------------------------------------
# no gdp.pwt data, so use gdp.gl data as an estimate
# gl data starts in 1986 - Compact of Free Association with US
gdp$gdp.pwt.est[gdp$iso3c=="MHL"] <- gdp$gdp.gl[gdp$iso3c=="MHL"]

#### MKD ----------------------------------------------------------------------
# 1990-1992: MKD coded as gaining independence in 1993 (pwt has values for 1990-1992,
# gl has values for 1991-1992)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "MKD", 2011)

#### MLI ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "MLI", 2011)

#### MLT ----------------------------------------------------------------------
# 1954-1963: MDA coded as gaining independence in 1964

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "MLT", 2011)

#### MMR ----------------------------------------------------------------------
# 1950-1961: apply gdp.gl proportion to 1962 gdp.gl estimate and use that ratio
# on gdp.pwt estimate
gdp <- gdp_growth_estimator_pwt_func(gdp, "MMR", 1962)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "MLT", 2011)

#### MNG ----------------------------------------------------------------------
# 1950-1969: apply gdp.gl proportion to 1970 gdp.gl estimate and use that ratio
# on gdp.pwt estimate
gdp <- gdp_growth_estimator_pwt_func(gdp, "MNG", 1970)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "MNG", 2011)

#### MOZ ----------------------------------------------------------------------
# 1960-1974: MOZ coded as gaining independence in 1975

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "MNG", 2011)

#### MRT ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "MRT", 2011)

#### MUS ----------------------------------------------------------------------
# 1950-1967: MUS coded as gaining independence in 1968

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "MUS", 2011)

#### MWI ----------------------------------------------------------------------
# 1954-1963: MWI coded as gaining independence in 1964

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "MWI", 2011)

#### MYS(x) ----------------------------------------------------------------------
# 1955-1956: MYS coded as gaining independence in 1957

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "MYS", 2011)

#### NAM/ZAF(x) ----------------------------------------------------------------------
# NAM
# gl data starts in 1990 (year of independence), pwt data starts in 1960

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "NAM", 2011)

# ZAF
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "ZAF", 2011)

#### NER ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "NER", 2011)

#### NGA ----------------------------------------------------------------------
# 1950-1959: NGA coded as gaining independence in 1960

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "NGA", 2011)

#### NIC ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "NIC", 2011)

#### NLD ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "NLD", 2011)

#### NOR ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "NOR", 2011)

#### NPL ----------------------------------------------------------------------
# 1950-1959: apply gdp.gl proportion to 1960 gdp.gl estimate and use that ratio
# on gdp.pwt estimate
gdp <- gdp_growth_estimator_pwt_func(gdp, "NPL", 1960)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "NPL", 2011)

#### NRU(x) ----------------------------------------------------------------------
# no gdp.pwt data, so use gdp.gl data as an estimate
# gl data starts in 1968 - end of UN trusteeship
gdp$gdp.pwt.est[gdp$iso3c=="MHL"] <- gdp$gdp.gl[gdp$iso3c=="MHL"]

#### NZL ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "NZL", 2011)

#### OMN ----------------------------------------------------------------------
# OMN coded as gaining independence in 1971, but due to Dhofar War,
# extending GDP data back to 1950, as gl contains data from this point forward

# 1950-1969: apply gdp.gl proportion to 1970 gdp.gl estimate and use that ratio
# on gdp.pwt estimate
gdp <- gdp_growth_estimator_pwt_func(gdp, "OMN", 1970)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "OMN", 2011)

#### PAN ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "PAN", 2011)

#### PER ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "PER", 2011)

#### PHL ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "PHL", 2011)

#### PLW(x) ----------------------------------------------------------------------
# no gdp.pwt data, so use gdp.gl data as an estimate
gdp$gdp.pwt.est[gdp$iso3c=="PLW"] <- gdp$gdp.gl[gdp$iso3c=="PLW"]

#### PNG ----------------------------------------------------------------------
# no gdp.pwt data, so use gdp.gl data as an estimate
gdp$gdp.pwt.est[gdp$iso3c=="PNG"] <- gdp$gdp.gl[gdp$iso3c=="PNG"]

#### POL ----------------------------------------------------------------------
# 1950-1969: apply gdp.gl proportion to 1970 gdp.gl estimate and use that ratio
# on gdp.pwt estimate
gdp <- gdp_growth_estimator_pwt_func(gdp, "POL", 1970)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "POL", 2011)

#### PRK(x) ----------------------------------------------------------------------
# no gdp.pwt data, so use gdp.gl data as an estimate
gdp$gdp.pwt.est[gdp$iso3c=="PRK"] <- gdp$gdp.gl[gdp$iso3c=="PRK"]

#### PRT ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "PRT", 2011)

#### PRY ----------------------------------------------------------------------
# 1950: apply gdp.gl proportion to 1951 gdp.gl estimate and use that ratio
# on gdp.pwt estimate
gdp <- gdp_growth_estimator_pwt_func(gdp, "PRY", 1951)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "PRY", 2011)

#### QAT ----------------------------------------------------------------------
# 1970: QAT coded as gaining independence in 1971

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "QAT", 2011)

#### ROU ----------------------------------------------------------------------
# 1950-1959: apply gdp.gl proportion to 1960 gdp.gl estimate and use that ratio
# on gdp.pwt estimate
# gl values constant 1950-1960
gdp <- gdp_growth_estimator_pwt_func(gdp, "ROU", 1960)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "ROU", 2011)

#### RUS/SOV(x) ----------------------------------------------------------------------

gdp.sov.all <- gdp %>%
  dplyr::filter(iso3c %in% c("RUS","EST","LVA","LTU","UKR","BLR","MDA","GEO","ARM","AZE","TJK","KAZ","KGZ","TKM","UZB")) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(gl.est = sum(gdp.gl.est,na.rm=TRUE)) %>%
  dplyr::ungroup()


gdp.rus <- gdp %>%
  dplyr::filter(iso3c == "RUS") %>%
  dplyr::full_join(gdp.sov.all,by="year")

# 1950-89
# apply proportion of year to 1990 gl gdp, calculate based on 1990 pwt gdp
gdp$rgdpna[gdp$iso3c=="RUS"&gdp$year==1950] <- 3620811000000*0.2436211
gdp$rgdpna[gdp$iso3c=="RUS"&gdp$year==1951] <- 3620811000000*0.2436211
gdp$rgdpna[gdp$iso3c=="RUS"&gdp$year==1952] <- 3620811000000*0.2447821
gdp$rgdpna[gdp$iso3c=="RUS"&gdp$year==1953] <- 3620811000000*0.2606672
gdp$rgdpna[gdp$iso3c=="RUS"&gdp$year==1954] <- 3620811000000*0.2850321
gdp$rgdpna[gdp$iso3c=="RUS"&gdp$year==1955] <- 3620811000000*0.3094709
gdp$rgdpna[gdp$iso3c=="RUS"&gdp$year==1956] <- 3620811000000*0.3390457
gdp$rgdpna[gdp$iso3c=="RUS"&gdp$year==1957] <- 3620811000000*1.317092
gdp$rgdpna[gdp$iso3c=="RUS"&gdp$year==1958] <- 3620811000000*0.3719076
gdp$rgdpna[gdp$iso3c=="RUS"&gdp$year==1959] <- 3620811000000*0.367778
gdp$rgdpna[gdp$iso3c=="RUS"&gdp$year==1960] <- 3620811000000*0.2869558
gdp$rgdpna[gdp$iso3c=="RUS"&gdp$year==1961] <- 3620811000000*0.3092766
gdp$rgdpna[gdp$iso3c=="RUS"&gdp$year==1962] <- 3620811000000*0.3284158
gdp$rgdpna[gdp$iso3c=="RUS"&gdp$year==1963] <- 3620811000000*0.3369125
gdp$rgdpna[gdp$iso3c=="RUS"&gdp$year==1964] <- 3620811000000*0.3626544
gdp$rgdpna[gdp$iso3c=="RUS"&gdp$year==1965] <- 3620811000000*0.3917537
gdp$rgdpna[gdp$iso3c=="RUS"&gdp$year==1966] <- 3620811000000*0.421848
gdp$rgdpna[gdp$iso3c=="RUS"&gdp$year==1967] <- 3620811000000*0.453748
gdp$rgdpna[gdp$iso3c=="RUS"&gdp$year==1968] <- 3620811000000*0.4954707
gdp$rgdpna[gdp$iso3c=="RUS"&gdp$year==1969] <- 3620811000000*0.5114751
gdp$rgdpna[gdp$iso3c=="RUS"&gdp$year==1970] <- 3620811000000*0.5541508
gdp$rgdpna[gdp$iso3c=="RUS"&gdp$year==1971] <- 3620811000000*0.5855383
gdp$rgdpna[gdp$iso3c=="RUS"&gdp$year==1972] <- 3620811000000*0.6112367
gdp$rgdpna[gdp$iso3c=="RUS"&gdp$year==1973] <- 3620811000000*0.6646439
gdp$rgdpna[gdp$iso3c=="RUS"&gdp$year==1974] <- 3620811000000*0.7011193
gdp$rgdpna[gdp$iso3c=="RUS"&gdp$year==1975] <- 3620811000000*0.736354
gdp$rgdpna[gdp$iso3c=="RUS"&gdp$year==1976] <- 3620811000000*0.7781734
gdp$rgdpna[gdp$iso3c=="RUS"&gdp$year==1977] <- 3620811000000*0.8167904
gdp$rgdpna[gdp$iso3c=="RUS"&gdp$year==1978] <- 3620811000000*0.8554579
gdp$rgdpna[gdp$iso3c=="RUS"&gdp$year==1979] <- 3620811000000*0.8737423
gdp$rgdpna[gdp$iso3c=="RUS"&gdp$year==1980] <- 3620811000000*0.9072615
gdp$rgdpna[gdp$iso3c=="RUS"&gdp$year==1981] <- 3620811000000*0.941767
gdp$rgdpna[gdp$iso3c=="RUS"&gdp$year==1982] <- 3620811000000*0.9803039
gdp$rgdpna[gdp$iso3c=="RUS"&gdp$year==1983] <- 3620811000000*1.025397
gdp$rgdpna[gdp$iso3c=="RUS"&gdp$year==1984] <- 3620811000000*1.066772
gdp$rgdpna[gdp$iso3c=="RUS"&gdp$year==1985] <- 3620811000000*1.092384
gdp$rgdpna[gdp$iso3c=="RUS"&gdp$year==1986] <- 3620811000000*1.12282
gdp$rgdpna[gdp$iso3c=="RUS"&gdp$year==1987] <- 3620811000000*1.147718
gdp$rgdpna[gdp$iso3c=="RUS"&gdp$year==1988] <- 3620811000000*1.21229
gdp$rgdpna[gdp$iso3c=="RUS"&gdp$year==1989] <- 3620811000000*1.243235

#### RWA ----------------------------------------------------------------------
# 1960-1961: QAT coded as gaining independence in 1962

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "RWA", 2011)

#### SAU ----------------------------------------------------------------------
# 1950-1969: apply gdp.gl proportion to 1970 gdp.gl estimate and use that ratio
# on gdp.pwt estimate
gdp <- gdp_growth_estimator_pwt_func(gdp, "SAU", 1970)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "SAU", 2011)

#### SDN ----------------------------------------------------------------------
# 1956-1969: apply gdp.gl proportion to 1970 gdp.gl estimate and use that ratio
# on gdp.pwt estimate
gdp <- gdp_growth_estimator_pwt_func(gdp, "SDN", 1970)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "SDN", 2011)

#### SEN ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "SEN", 2011)

#### SGP ----------------------------------------------------------------------
# 1960-1964: SGP coded as gaining independence in 1965

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "SGP", 2011)

#### SLB(x) ----------------------------------------------------------------------
# no gdp.pwt data, so use gdp.gl data as an estimate
gdp$gdp.pwt.est[gdp$iso3c=="SLB"] <- gdp$gdp.gl[gdp$iso3c=="SLB"]

#### SLE ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "SLE", 2011)

#### SLV ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "SLV", 2011)

#### SMR(x) ----------------------------------------------------------------------
# no gdp.pwt data, so use gdp.gl data as an estimate
gdp$gdp.pwt.est[gdp$iso3c=="SMR"] <- gdp$gdp.gl[gdp$iso3c=="SMR"]

#### SOM(x) ----------------------------------------------------------------------
# no gdp.pwt data, so use gdp.gl data as an estimate
gdp$gdp.pwt.est[gdp$iso3c=="SOM"] <- gdp$gdp.gl[gdp$iso3c=="SOM"]

#### SSD(x) ----------------------------------------------------------------------
# no gdp.pwt data, so use gdp.gl data as an estimate
# only have 2011 gl data
gdp$gdp.pwt.est[gdp$iso3c=="SSD"] <- gdp$gdp.gl[gdp$iso3c=="SSD"]

#### STP ----------------------------------------------------------------------
# 1970-1974: STP coded as gaining independence in 1975

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "STP", 2011)

#### SUR ----------------------------------------------------------------------
# 1970-1974: SUR coded as gaining independence in 1975

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "SUR", 2011)

#### SVK ----------------------------------------------------------------------
# 1990-1992: SVK coded as gaining independence in 1993

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "SVK", 2011)

#### SVN ----------------------------------------------------------------------
# 1990-1991: SVN coded as gaining independence in 1992

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "SVN", 2011)

#### SWE ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "SWE", 2011)

#### SWZ ----------------------------------------------------------------------
# 1968-1969: apply gdp.gl proportion to 1970 gdp.gl estimate and use that ratio
# on gdp.pwt estimate
gdp <- gdp_growth_estimator_pwt_func(gdp, "SWZ", 1970)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "SWZ", 2011)

#### SYC(x) ----------------------------------------------------------------------
# no gdp.pwt data, so use gdp.gl data as an estimate
gdp$gdp.pwt.est[gdp$iso3c=="SYC"] <- gdp$gdp.gl[gdp$iso3c=="SYC"]

#### SYR ----------------------------------------------------------------------
# 1950-1959: apply gdp.gl proportion to 1960 gdp.gl estimate and use that ratio
# on gdp.pwt estimate
gdp <- gdp_growth_estimator_pwt_func(gdp, "SYR", 1960)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "SYR", 2011)

#### TCD ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "TCD", 2011)

#### TGO ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "TGO", 2011)

#### THA ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "THA", 2011)

#### TJK ----------------------------------------------------------------------
# 1990: TJK coded as gaining independence in 1991

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "TJK", 2011)

#### TKM ----------------------------------------------------------------------
# 1990: TKM coded as gaining independence in 1991

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "TKM", 2011)

#### TLS(x) ----------------------------------------------------------------------
# no gdp.pwt data, so use gdp.gl data as an estimate
gdp$gdp.pwt.est[gdp$iso3c=="TLS"] <- gdp$gdp.gl[gdp$iso3c=="TLS"]

#### TON(x) ----------------------------------------------------------------------
# no gdp.pwt data, so use gdp.gl data as an estimate
# gl data starts in 1970 - end of protection status
gdp$gdp.pwt.est[gdp$iso3c=="TON"] <- gdp$gdp.gl[gdp$iso3c=="TON"]

#### TTO ----------------------------------------------------------------------
# 1950-1961: TTO coded as gaining independence in 1962

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "TTO", 2011)

#### TUN ----------------------------------------------------------------------
# 1950-1959: apply gdp.gl proportion to 1960 gdp.gl estimate and use that ratio
# on gdp.pwt estimate
gdp <- gdp_growth_estimator_pwt_func(gdp, "TUN", 1960)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "TTO", 2011)

#### TUR ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "TUR", 2011)

#### TUV(x) ----------------------------------------------------------------------
# no gdp.pwt data, so use gdp.gl data as an estimate
# gl data starts in 1978 - independence from UK
gdp$gdp.pwt.est[gdp$iso3c=="TUV"] <- gdp$gdp.gl[gdp$iso3c=="TUV"]

#### TWN ----------------------------------------------------------------------
# 1950: apply gdp.gl proportion to 1951 gdp.gl estimate and use that ratio
# on gdp.pwt estimate
gdp <- gdp_growth_estimator_pwt_func(gdp, "TWN", 1951)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "TWN", 2011)

#### TZA/ZAN(x) ----------------------------------------------------------------------


# no gdp.pwt data, so use gdp.gl data as an estimate
gdp$gdp.pwt.est[gdp$iso3c=="ZAN"] <- gdp$gdp.gl[gdp$iso3c=="ZAN"]

#### UGA ----------------------------------------------------------------------
# 1950-1961: UGA coded as gaining independence in 1962

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "UGA", 2011)

#### UKR ----------------------------------------------------------------------
# 1990: UKR coded as gaining independence in 1991

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "UKR", 2011)

#### URY ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "URY", 2011)

#### USA ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "USA", 2011)

#### VCT ----------------------------------------------------------------------
# 1970-1978: VCT coded as gaining independence in 1979

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "USA", 2011)

#### VEN ----------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "VEN", 2011)

#### VNM/RVN ----------------------------------------------------------------------
# VNM pwt estimates 1970-1975 contain both North and South Vietnam - use ratio between
# economies from gl estimates in those years to break up pwt estimates
gdp.vnm.multiplier <- gdp %>%
  dplyr::filter(iso3c %in% c("VNM","RVN")) %>%
  dplyr::select(iso3c,year,gdp.gl) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(multiplier = gdp.gl / sum(gdp.gl,na.rm=TRUE)) %>%
  dplyr::ungroup()

# pull pwt estimates for 1970-1975
gdp.vnm.70.75 <- gdp %>%
  dplyr::filter(iso3c == "VNM",
                year %in% c(1970:1975)) %>%
  dplyr::select(year,gdp.pwt)

# merge pwt 1970-1975 estimates with ratios
gdp.vnm.multiplier <- gdp.vnm.multiplier %>%
  dplyr::left_join(gdp.vnm.70.75,by="year") %>%
  dplyr::mutate(gdp.pwt.est2 = gdp.pwt * multiplier) %>%
  dplyr::select(iso3c,year,gdp.pwt.est2)
  
# merge gdp.pwt.est 1970-1975 with main gdp dataset
gdp <- gdp %>%
  dplyr::left_join(gdp.vnm.multiplier) %>%
  dplyr::mutate(gdp.pwt.est = ifelse(year %in% c(1970:1975)&iso3c %in% c("VNM","RVN"),gdp.pwt.est2,gdp.pwt.est)) %>%
  dplyr::select(-gdp.pwt.est2)

# RVN 1954-1969: apply gdp.gl proportion to 1970 gdp.gl estimate and use that ratio
# on gdp.pwt estimate
gdp <- gdp_growth_estimator_pwt_func(gdp, "RVN", 1970)

# calculate VNM gdp.pwt.est using separate dataset with gdp.pwt (North and South Vietnam combined) as NAs
gdp.vnm <- gdp %>%
  dplyr::filter(iso3c == "VNM") %>%
  dplyr::mutate(gdp.pwt = NA)

gdp.vnm <- gdp_growth_estimator_pwt_func(gdp.vnm, "VNM", 1970) %>%
  dplyr::rename(gdp.pwt.est2 = gdp.pwt.est) %>%
  dplyr::select(iso3c,year,gdp.pwt.est2)

gdp <- gdp %>%
  dplyr::full_join(gdp.vnm,by=c("iso3c","year")) %>%
  dplyr::mutate(gdp.pwt.est = ifelse(year %in% c(1954:1969)&iso3c=="VNM",gdp.pwt.est2,gdp.pwt.est)) %>%
  dplyr::select(-gdp.pwt.est2)

#### VUT ----------------------------------------------------------------------
# no gdp.pwt data, so use gdp.gl data as an estimate
# gl data starts in 1980
gdp$gdp.pwt.est[gdp$iso3c=="VUT"] <- gdp$gdp.gl[gdp$iso3c=="VUT"]

#### WSM ----------------------------------------------------------------------
# no gdp.pwt data, so use gdp.gl data as an estimate
# gl data starts in 1962- Western Samoa Act of 1961 enters effect
gdp$gdp.pwt.est[gdp$iso3c=="WSM"] <- gdp$gdp.gl[gdp$iso3c=="WSM"]

#### YEM/YAR/YPR ----------------------------------------------------------------------
# pwt data contains combined Yemen beginning in 1989, while gl separates North and South Yemen from
# 1950 / 1967 (respectively) through to 1990 (inclusive)
# Yemen coded as beginning starting in 1991

# calculate the proportions of Yemen's 1989 and 1990 economies
gdp.north.yemen.prop.89 <- gdp$gdp.gl[gdp$iso3c=="YAR"&gdp$year==1989]/(gdp$gdp.gl[gdp$iso3c=="YAR"&gdp$year==1989]+
                                                                      gdp$gdp.gl[gdp$iso3c=="YPR"&gdp$year==1989])
gdp.south.yemen.prop.89 <- gdp$gdp.gl[gdp$iso3c=="YPR"&gdp$year==1989]/(gdp$gdp.gl[gdp$iso3c=="YAR"&gdp$year==1989]+
                                                                      gdp$gdp.gl[gdp$iso3c=="YPR"&gdp$year==1989])
gdp.north.yemen.prop.90 <- gdp$gdp.gl[gdp$iso3c=="YAR"&gdp$year==1990]/(gdp$gdp.gl[gdp$iso3c=="YAR"&gdp$year==1990]+
                                                                         gdp$gdp.gl[gdp$iso3c=="YPR"&gdp$year==1990])
gdp.south.yemen.prop.90 <- gdp$gdp.gl[gdp$iso3c=="YPR"&gdp$year==1990]/(gdp$gdp.gl[gdp$iso3c=="YAR"&gdp$year==1990]+
                                                                         gdp$gdp.gl[gdp$iso3c=="YPR"&gdp$year==1990])

# calculate gdp.gl proportions for YAR (1950-1988) and YPR (1967-1988) based on 1989 gl GDPs
yar.multiplier <- gdp %>%
  dplyr::filter(iso3c == "YAR",
                year < 1989) %>%
  dplyr::mutate(yar.multiplier = gdp.gl / gdp$gdp.gl[gdp$iso3c=="YAR"&gdp$year==1989]) %>%
  dplyr::select(year,yar.multiplier)
ypr.multiplier <- gdp %>%
  dplyr::filter(iso3c == "YPR",
                year < 1989) %>%
  dplyr::mutate(ypr.multiplier = gdp.gl / gdp$gdp.gl[gdp$iso3c=="YPR"&gdp$year==1989]) %>%
  dplyr::select(year,ypr.multiplier)


gdp_split_yemen <- data.frame(iso3c = c(rep("YAR",41),rep("YPR",24)), year = c(1950:1990,1967:1990),
                              # calculate 1989 and 1990 pwt estimates based on gl ratios between North and South Yemen
                              gdp.pwt.est2 = c(rep(NA,39),
                                              gdp.north.yemen.prop.89*gdp$gdp.pwt[gdp$iso3c=="YEM"&gdp$year==1989],
                                              gdp.north.yemen.prop.90*gdp$gdp.pwt[gdp$iso3c=="YEM"&gdp$year==1990],
                                              rep(NA,22),
                                              gdp.south.yemen.prop.89*gdp$gdp.pwt[gdp$iso3c=="YEM"&gdp$year==1989],
                                              gdp.south.yemen.prop.90*gdp$gdp.pwt[gdp$iso3c=="YEM"&gdp$year==1990])) %>%
  dplyr::full_join(yar.multiplier,by="year") %>%
  dplyr::full_join(ypr.multiplier,by="year")

gdp_split_yemen <- gdp_split_yemen %>%
  dplyr::mutate(gdp.pwt.est2 = ifelse(iso3c=="YAR"&is.na(gdp.pwt.est2),
                                     gdp_split_yemen$gdp.pwt.est2[gdp_split_yemen$iso3c=="YAR"&gdp_split_yemen$year==1989]*yar.multiplier,
                                     gdp.pwt.est2),
                gdp.pwt.est2 = ifelse(iso3c=="YPR"&is.na(gdp.pwt.est2),
                                     gdp_split_yemen$gdp.pwt.est2[gdp_split_yemen$iso3c=="YPR"&gdp_split_yemen$year==1989]*ypr.multiplier,
                                     gdp.pwt.est2)) %>%
  dplyr::select(-c(yar.multiplier,ypr.multiplier))

# merge pwt split estimates into the main dataset
gdp <- gdp %>%
  dplyr::full_join(gdp_split_yemen,by=c("iso3c","year")) %>%
  dplyr::mutate(gdp.pwt.est = dplyr::coalesce(gdp.pwt.est,gdp.pwt.est2)) %>%
  # filter out YEM entries for 1950-1990
  dplyr::filter(iso3c != "YEM" | year > 1990) %>%
  dplyr::select(-gdp.pwt.est2)

# original estimates - appear to be different methodology for calculating than what is done above
# # YEM
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1950] <- 36403208086*0.766175876
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1951] <- 36403208086*0.782980248
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1952] <- 36403208086*0.802124909
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1953] <- 36403208086*0.823335607
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1954] <- 36403208086*0.845934022
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1955] <- 36403208086*0.86899501
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1956] <- 36403208086*0.89063537
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1957] <- 36403208086*0.913630538
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1958] <- 36403208086*0.936079029
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1959] <- 36403208086*0.959410613
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1960] <- 36403208086*0.985146478
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1961] <- 36403208086*1.012566255
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1962] <- 36403208086*1.042024643
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1963] <- 36403208086*1.075295142
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1964] <- 36403208086*1.103530363
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1965] <- 36403208086*1.132005097
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1966] <- 36403208086*1.163361313
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1967] <- 36403208086*2.159944126*0.553142898
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1968] <- 36403208086*2.239144624*0.545840546
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1969] <- 36403208086*2.234323263*0.508364272
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1970] <- 36403208086*2.578976526*0.531671379
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1971] <- 36403208086*2.069952701*0.717879913
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1972] <- 36403208086*2.306435246*0.740464405
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1973] <- 36403208086*2.423371077*0.740840973
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1974] <- 36403208086*2.791522303*0.765063879
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1975] <- 36403208086*3.13662534*0.783567826
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1976] <- 36403208086*3.339016457*0.785461425
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1977] <- 36403208086*3.440034666*0.785064651
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1978] <- 36403208086*3.539224523*0.777036978
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1979] <- 36403208086*3.756606288*0.781373105
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1980] <- 36403208086*4.08193754*0.78433014
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1981] <- 36403208086*4.425800955*0.789147717
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1982] <- 36403208086*4.609332634*0.786674748
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1983] <- 36403208086*4.728460657*0.778670468
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1984] <- 36403208086*4.873117941*0.772559119
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1985] <- 36403208086*5.699428275*0.792284933
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1986] <- 36403208086*5.872827234*0.79340554
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1987] <- 36403208086*6.301204335*0.795408931
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1988] <- 36403208086*7.246468998*0.809809327
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1989] <- 36403208086*0.409221266
# 
# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1967] <- 36403208086*2.159944126*0.446857102
# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1968] <- 36403208086*2.239144624*0.454159454
# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1969] <- 36403208086*2.234323263*0.491635728
# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1970] <- 36403208086*2.578976526*0.468328621
# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1971] <- 36403208086*2.069952701*0.282120087
# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1972] <- 36403208086*2.306435246*0.259535595
# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1973] <- 36403208086*2.423371077*0.259159027
# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1974] <- 36403208086*2.791522303*0.234936121
# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1975] <- 36403208086*3.13662534*0.216432174
# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1976] <- 36403208086*3.339016457*0.214538575
# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1977] <- 36403208086*3.440034666*0.214935349
# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1978] <- 36403208086*3.539224523*0.222963022
# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1979] <- 36403208086*3.756606288*0.218626895
# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1980] <- 36403208086*4.08193754*0.21566986
# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1981] <- 36403208086*4.425800955*0.210852283
# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1982] <- 36403208086*4.609332634*0.213325252
# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1983] <- 36403208086*4.728460657*0.221329532
# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1984] <- 36403208086*4.873117941*0.227440881
# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1985] <- 36403208086*5.699428275*0.207715067
# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1986] <- 36403208086*5.872827234*0.20659446
# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1987] <- 36403208086*6.301204335*0.204591069
# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1988] <- 36403208086*7.246468998*0.190190673
# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1989] <- 36403208086*0.590778734
# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1990] <- 36403208086*0.590778734 # rough estimate - double check

# gdp$iso3c[gdp$iso3c=="YEM"&gdp$year<=1990] <- "YAR"

#### ZMB ----------------------------------------------------------------------
# 1955-1963: ZMB coded as gaining independence in 1964

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "ZMB", 2011)

#### ZWE ----------------------------------------------------------------------
# 1955-1964: ZWE coded as gaining independence in 1965

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_gl_func(gdp, "ZMB", 2011)

#### formatting ----------------------------------------------------------------------
gdp <- gdp %>%
  dplyr::rename(gdp.pwt.original = gdp.pwt,
                gdp.gl.original = gdp.gl) %>%
  # temp
  dplyr::select(-rgdpna)

### add workbook estimates ----------------------------------------------------------------------
# adds additional gdp estimates (file notes sources of estimates)
gdp.add <- readxl::read_excel("Data files/Workbooks/gdp_add.xlsx") %>%
  dplyr::select(iso3c,year,gdp) %>%
  dplyr::rename(gdp.pwt.est = gdp) %>%
  # using the countrycode package, add country name based on iso3c code
  dplyr::mutate(country = countrycode::countrycode(iso3c,"iso3c","country.name"),
                gdp.pwt.original = NA,
                gdp.gl.original = NA,
                gdp.gl.est = NA)

gdp.add$country[gdp.add$iso3c=="KSV"] <- "Kosovo"

# adds gdp estimates for countries in the 1940s (file notes sources of estimates)
gdp_40s <- readxl::read_excel("Data files/Workbooks/gdp_estimates_40s.xlsx", sheet = 1) %>%
  dplyr::select(-method) %>%
  dplyr::rename(gdp.pwt.est = gdp) %>%
  # using the countrycode package, add country name based on iso3c code
  dplyr::mutate(country = countrycode::countrycode(iso3c,"iso3c","country.name"),
                gdp.pwt.original = NA,
                gdp.gl.original = NA,
                gdp.gl.est = NA)

gdp_40s$country[gdp_40s$iso3c=="BRD"] <- "West Germany"
gdp_40s$country[gdp_40s$iso3c=="DDR"] <- "East Germany"
gdp_40s$country[gdp_40s$iso3c=="SOV"] <- "Soviet Union"
gdp_40s$country[gdp_40s$iso3c=="YAR"] <- "North Yemen"
gdp_40s$country[gdp_40s$iso3c=="YUG"] <- "Yugoslavia"

gdp <- gdp %>%
  rbind(gdp.add,gdp_40s) %>%
  # using the countrycode package, add country name based on iso3c code
  dplyr::mutate(country = countrycode::countrycode(iso3c,"iso3c","country.name"))  %>%
  dplyr::relocate(country,.after = iso3c)

### calculate growth rates ----------------------------------------------------------------------
gdp_year_prior <- gdp %>%
  dplyr::mutate(year = year + 1) %>%
  dplyr::rename(gdp.pwt.original.plus1 = gdp.pwt.original,
                gdp.gl.original.plus1 = gdp.gl.original,
                gdp.pwt.est.plus1 = gdp.pwt.est,
                gdp.gl.est.plus1 = gdp.gl.est)

gdp2 <- gdp %>%
  dplyr::left_join(gdp_year_prior,by=c("iso3c","country","year")) %>%
  dplyr::mutate(gdp.pwt.original.growth.rate = (gdp.pwt.original / gdp.pwt.original.plus1)-1,
                gdp.gl.original.growth.rate = (gdp.gl.original / gdp.gl.original.plus1)-1,
                gdp.pwt.est.growth.rate = (gdp.pwt.est / gdp.pwt.est.plus1)-1,
                gdp.gl.est.growth.rate = (gdp.gl.est / gdp.gl.est.plus1)-1)

### adjust growth estimates for countries uniting/dissolving ----------------------------------------------------------------------
# CZE
# YEM
# post-Soviet
# post-Yugoslav
# SRB 2006
# SRB 2008
# DEU
# VNM
# BGD / PAK

### write data ----------------------------------------------------------------------
# writes formatted dataframe as csv files
write.csv(gdp,"Data files/Formatted data files/gdp.csv",row.names = FALSE)
