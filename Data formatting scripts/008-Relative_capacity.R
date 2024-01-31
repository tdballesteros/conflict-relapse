# This script creates a relative capacity metric for each country-year based on Artbetman and Kugler's (2013) RPC.

### load libraries ----------------------------------------------------------------------
library(readxl)
library(countrycode)
library(zoo)
library(imputeTS)
library(dplyr)

### not in function ----------------------------------------------------------------------
'%!in%' <- function(x,y)!('%in%'(x,y))

### load data file ----------------------------------------------------------------------
# need to fix estimates to finalised estimates
rpc <- readxl::read_excel("Data files/Raw data files/RPC2015_components.xlsx")

### format dataset ----------------------------------------------------------------------
rpc <- rpc %>%
  dplyr::select(country,year,rpe_gdp) %>%
  # fixes spelling of Guinea-Bissau
  dplyr::mutate(country = ifelse(country=="Guinea-Bissaau","Guinea-Bissau",country),
                # using the countrycode package, add iso3c based on country name
                iso3c = countrycode::countrycode(country,"country.name","iso3c")) %>%
  # remove non-sovereign entities, countries without data, and countries with very limited
  # data (no data prior to 2000)
  dplyr::filter(iso3c %!in% c("ASM","DMA","KIR","LCA","LIE","MCO","NRU","PRK","ABW","AIA",
                              "BRN","CYM","HKG","MAC","PYF","GRL","GUM","MSR","NCL","BMU",
                              "CUB","TKM"),
                # remove non-sovereign entities that are missing from the countrycode dataset
                country %!in% c("Faeore Islands","Netherlands Antilles"))

### approximate missing countries ----------------------------------------------------------------------
# CUB approx
# Take mean of Dominican Republic, Haiti, and Jamaica
rpc.cub <- rpc %>%
  dplyr::filter(iso3c %in% c("HTI","DOM","JAM")) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(rpe_gdp = mean(rpe_gdp)) %>%
  dplyr::mutate(iso3c = "CUB",
                country = "Cuba")

# TKM approx
# Take mean of Kazakhstan, Kyrgyzstan, Tajikistan, and Uzbekistan
rpc.tkm <- rpc %>%
  dplyr::filter(iso3c %in% c("KAZ","KGZ","TJK","UZB")) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(rpe_gdp = mean(rpe_gdp)) %>%
  dplyr::mutate(iso3c = "TKM",
                country = "Turkmenistan")

# merge estimates with main dataset
rpc <- rpc %>%
  rbind(rpc.cub,rpc.tkm) %>%
  # reorders variables
  dplyr::relocate(iso3c,.before=country)

### expand to all country-years ----------------------------------------------------------------------
rpc <- rpc %>%
  dplyr::full_join(expand.grid(iso3c = unique(rpc$iso3c), year = c(1946:2019)))

### calculate unified/divided country data ----------------------------------------------------------------------
# "Germany", "Yemen, Republic of" exist in dataset from 1960-2013
# "Vietnam" begins in 1979 (post reunification)
# post-Soviet countries begin at earliest in 1991; no USSR
# "Serbia, Republic of" begins in 1997; no Yugoslavia
# Czechia and Slovakia begins in 1991; no Czechoslovakia

### remove if countries did not exist that year ----------------------------------------------------------------------
# load formatted data output by script 005-Country_years
cyears <- read.csv("Data files/Formatted data files/country_years.csv")

# merge datasets and filter out if the country-year is not included in the cyears dataset
rpc <- rpc %>%
  dplyr::left_join(cyears,by=c("iso3c","year")) %>%
  dplyr::filter(cn == 1) %>%
  dplyr::select(-cn)

### write data ----------------------------------------------------------------------
# writes formatted dataframe as csv files
write.csv(rpc,"Data files/Formatted data files/relative_capacity.csv",row.names = FALSE)


# rpc.cor <- rpc %>%
#   dplyr::full_join(p5all,by=c("iso3c","year")) %>%
#   dplyr::full_join(pol,by=c("iso3c","year")) %>%
#   dplyr::full_join(gdp,by=c("iso3c","year")) %>%
#   dplyr::full_join(pd,by=c("iso3c","year")) %>%
#   dplyr::full_join(se,by=c("iso3c","year")) %>%
#   dplyr::full_join(cow,by=c("iso3c","year")) %>%
#   dplyr::full_join(cowpca1,by=c("iso3c","year")) %>%
#   dplyr::full_join(reg,by=c("iso3c")) %>%
#   #dplyr::rename(gdp = gdp.x) %>%
#   #dplyr::select(-gdp.y) %>%
#   dplyr::mutate(gdppc = gdp / pop.pd,
#                 lnpop = log(pop.pd),
#                 lngdppc = log(gdppc)) %>%
#   unique()
# 
# rpc.test <- glm(rpe_gdp ~ shec, data = rpc.cor)
# summary(rpc.test)
# 
# rpc.glm <- glm(rpe_gdp ~ parcomp + parreg + xconst + xrcomp + xropen + gdppc + lnpop + shec +
#                  lnmilexpgdp + lntroops + lnmilexpc + lnmilexpt + SSA + MENA + Americas + Europe, data = rpc.cor)
# summary(rpc.glm)
# car::vif(rpc.glm)
# 
# rpc.bih.glm <- glm(rpe_gdp ~ gdppc + lnpop + shec + lnmilexpgdp + lntroops + lnmilexpc + lnmilexpt + SSA + MENA +
#                      Americas + Europe, data = rpc.cor)
# summary(rpc.bih.glm)
# 
# rpc.cor <- rpc.cor %>%
#   dplyr::mutate(est = rpc.glm[["coefficients"]][1] + rpc.glm[["coefficients"]][2]*parcomp +
#                   rpc.glm[["coefficients"]][3]*parreg + rpc.glm[["coefficients"]][4]*xconst +
#                   rpc.glm[["coefficients"]][5]*xrcomp + rpc.glm[["coefficients"]][6]*xropen +
#                   rpc.glm[["coefficients"]][7]*gdppc + rpc.glm[["coefficients"]][8]*lnpop +
#                   rpc.glm[["coefficients"]][9]*shec + rpc.glm[["coefficients"]][10]*lnmilexpgdp +
#                   rpc.glm[["coefficients"]][11]*lntroops + rpc.glm[["coefficients"]][12]*lnmilexpc +
#                   rpc.glm[["coefficients"]][13]*lnmilexpt + rpc.glm[["coefficients"]][14]*SSA +
#                   rpc.glm[["coefficients"]][5]*MENA + rpc.glm[["coefficients"]][16]*Americas +
#                   rpc.glm[["coefficients"]][17]*Europe,
#                 bih = rpc.bih.glm[["coefficients"]][1] + rpc.bih.glm[["coefficients"]][2]*gdppc +
#                   rpc.bih.glm[["coefficients"]][3]*lnpop + rpc.bih.glm[["coefficients"]][4]*shec +
#                   rpc.bih.glm[["coefficients"]][5]*lnmilexpgdp + rpc.bih.glm[["coefficients"]][6]*lntroops +
#                   rpc.bih.glm[["coefficients"]][7]*lnmilexpc + rpc.bih.glm[["coefficients"]][8]*lnmilexpt +
#                   rpc.bih.glm[["coefficients"]][9]*SSA + rpc.bih.glm[["coefficients"]][10]*MENA +
#                   rpc.bih.glm[["coefficients"]][11]*Americas + rpc.bih.glm[["coefficients"]][12]*Europe)
# 
# plot(rpc.cor$rpe_gdp,rpc.cor$est)
# 
# rpc.cor$estused[is.na(rpc.cor$rpe_gdp)&(!is.na(rpc.cor$est))] <- 1
# rpc.cor$rpe_gdp[is.na(rpc.cor$rpe_gdp)&(!is.na(rpc.cor$est))] <- rpc.cor$est[is.na(rpc.cor$rpe_gdp)&(!is.na(rpc.cor$est))]
# rpc.cor <- rpc.cor %>%
#   dplyr::filter(year > 1945)
# 
# rpc <- rpc %>%
#   dplyr::filter(year < 2014)
# 
# # add estimates
# rpc_est <- read_excel("~/Documents/rpc_estimates.xlsx", sheet = 2) %>%
#   dplyr::select(-method)
# 
# rpc_remove <- rpc_est %>%
#   dplyr::select(iso3c,year) %>%
#   dplyr::mutate(countryyear = paste(iso3c,year)) %>%
#   dplyr::filter(iso3c != "YEM" | year >= 1990 )
# 
# rpc2 <- rpc %>%
#   dplyr::mutate(countryyear = paste(iso3c,year)) %>%
#   dplyr::filter(countryyear %!in% rpc_remove$countryyear) %>%
#   dplyr::select(-countryyear)
# 
# rpc2 <- rbind(rpc2,rpc_est) %>%
#   dplyr::rename(rpc = rpe_gdp) %>%
#   dplyr::filter(year >= 1946) %>%
#   unique() %>%
#   na.omit() %>%
#   dplyr::filter(iso3c != "YEM" | year >= 1990)
# 
# # remove post-Soviet and post-Yugoslav states for 1991
# rpc2 <- rpc2 %>%
#   dplyr::filter(iso3c %!in% c("ARM","AZE","BLR","EST","GEO","KAZ","KGZ","LTU","LVA","MDA","RUS",
#                               "TJK","TKM","UKR","UZB") | year >= 1992,
#            iso3c %!in% c("BIH","HRV","MKD","SRB","SVN") | year >= 1992)
# 
# # rpc2 <- rpc.cor %>%
# #   dplyr::select(iso3c,year,rpe_gdp,estused) %>%
# #   unique() %>%
# #   dplyr::filter(year >= 1946)
# # 
# # rpc2 <- rpc2 %>%
# #   dplyr::group_by(iso3c) %>%
# #   dplyr::arrange(year) %>%
# #   dplyr::mutate(rpc_est = na_interpolation(rpe_gdp), option = "linear") %>%
# #   dplyr::ungroup() %>%
# #   select(-c(rpe_gdp,option)) %>%
# #   dplyr::rename(rpc = rpc_est)
# # 
# # rpc2 <- rpc2 %>%
# #   dplyr::filter(iso3c %!in% c("ARM","AZE","BLR","EST","GEO","KAZ","KGZ","LTU","LVA","MDA","RUS",
# #                               "TJK","TKM","UKR","UZB") | year >= 1992,
# #                 iso3c %!in% c("BIH","HRV","MKD","SRB","SVN") | year >= 1992)
# # 
# # rpc.yar <- rpc2 %>%
# #   dplyr::filter(iso3c == "YEM") %>%
# #   dplyr::mutate(iso3c = "YAR")
# # 
# # rpc.ypr <- rpc2 %>%
# #   dplyr::filter(iso3c == "YEM") %>%
# #   dplyr::mutate(iso3c = "YPR")
# # 
# # rpc2 <- rpc2 %>%
# #   rbind(rpc.yar,rpc.ypr)
