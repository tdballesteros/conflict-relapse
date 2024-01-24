# This script creates a fiscal capacity metric for each country-year based on Artbetman and Kugler's (2013) RPC.

# TODO: aid data for KSV and PSE messed up

# load libraries
library(readxl)
library(countrycode)
library(zoo)
library(imputeTS)
library(dplyr)

# not in function
'%!in%' <- function(x,y)!('%in%'(x,y))

#### RPC ####
# need to fix estimates to finalised estimates
rpc <- readxl::read_excel("~/Downloads/RPC2015_components.xlsx")

rpc <- rpc %>%
  dplyr::select(country,year,rpe_gdp) %>%
  dplyr::mutate(iso3c = countrycode::countrycode(country,"country.name","iso3c")) %>%
  dplyr::filter(iso3c %!in% c("ASM","AND","ATG","BHS","BRB","BTN","COM","DMA","FSM","GRD","KIR",
                               "KNA","LCA","LIE","MCO","MDV","MHL","NRU","PLW","PRK","SLB","SMR",
                               "STP","SUR","TON","TUV","VCT","VUT","WSM","ZAN","ABW","AIA","BMW",
                               "BRN","CYM","HKG","MAC","PYF","GRL","GUM","MNE","MSR","NCL","BMU",
                               "DJI","CUB","TKM"),
                country %!in% c("Faeore Islands","Netherlands Antilles"))

rpc$iso3c[rpc$country=="Guinea-Bissaau"] <- "GNB"

rpc <- rpc %>%
  dplyr::select(-country)

# CUB approx
rpc.cub <- rpc %>%
  dplyr::filter(iso3c %in% c("HTI","DOM","JAM")) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(rpe_gdp = mean(rpe_gdp)) %>%
  dplyr::mutate(iso3c = "CUB")

# TKM approx
rpc.tkm <- rpc %>%
  dplyr::filter(iso3c %in% c("KAZ","KGZ","TJK","UZB")) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(rpe_gdp = mean(rpe_gdp)) %>%
  dplyr::mutate(iso3c = "TKM")

rpc <- rpc %>%
  rbind(rpc.cub,rpc.tkm)

rpc <- rpc %>%
  dplyr::full_join(expand.grid(iso3c = unique(rpc$iso3c), year = c(1950:2019)))

rpc.cor <- rpc %>%
  dplyr::full_join(p5all,by=c("iso3c","year")) %>%
  dplyr::full_join(pol,by=c("iso3c","year")) %>%
  dplyr::full_join(gdp,by=c("iso3c","year")) %>%
  dplyr::full_join(pd,by=c("iso3c","year")) %>%
  dplyr::full_join(se,by=c("iso3c","year")) %>%
  dplyr::full_join(cow,by=c("iso3c","year")) %>%
  dplyr::full_join(cowpca1,by=c("iso3c","year")) %>%
  dplyr::full_join(reg,by=c("iso3c")) %>%
  #dplyr::rename(gdp = gdp.x) %>%
  #dplyr::select(-gdp.y) %>%
  dplyr::mutate(gdppc = gdp / pop.pd,
                lnpop = log(pop.pd),
                lngdppc = log(gdppc)) %>%
  unique()

rpc.test <- glm(rpe_gdp ~ shec, data = rpc.cor)
summary(rpc.test)

rpc.glm <- glm(rpe_gdp ~ parcomp + parreg + xconst + xrcomp + xropen + gdppc + lnpop + shec +
                 lnmilexpgdp + lntroops + lnmilexpc + lnmilexpt + SSA + MENA + Americas + Europe, data = rpc.cor)
summary(rpc.glm)
car::vif(rpc.glm)

rpc.bih.glm <- glm(rpe_gdp ~ gdppc + lnpop + shec + lnmilexpgdp + lntroops + lnmilexpc + lnmilexpt + SSA + MENA +
                     Americas + Europe, data = rpc.cor)
summary(rpc.bih.glm)

rpc.cor <- rpc.cor %>%
  dplyr::mutate(est = rpc.glm[["coefficients"]][1] + rpc.glm[["coefficients"]][2]*parcomp +
                  rpc.glm[["coefficients"]][3]*parreg + rpc.glm[["coefficients"]][4]*xconst +
                  rpc.glm[["coefficients"]][5]*xrcomp + rpc.glm[["coefficients"]][6]*xropen +
                  rpc.glm[["coefficients"]][7]*gdppc + rpc.glm[["coefficients"]][8]*lnpop +
                  rpc.glm[["coefficients"]][9]*shec + rpc.glm[["coefficients"]][10]*lnmilexpgdp +
                  rpc.glm[["coefficients"]][11]*lntroops + rpc.glm[["coefficients"]][12]*lnmilexpc +
                  rpc.glm[["coefficients"]][13]*lnmilexpt + rpc.glm[["coefficients"]][14]*SSA +
                  rpc.glm[["coefficients"]][5]*MENA + rpc.glm[["coefficients"]][16]*Americas +
                  rpc.glm[["coefficients"]][17]*Europe,
                bih = rpc.bih.glm[["coefficients"]][1] + rpc.bih.glm[["coefficients"]][2]*gdppc +
                  rpc.bih.glm[["coefficients"]][3]*lnpop + rpc.bih.glm[["coefficients"]][4]*shec +
                  rpc.bih.glm[["coefficients"]][5]*lnmilexpgdp + rpc.bih.glm[["coefficients"]][6]*lntroops +
                  rpc.bih.glm[["coefficients"]][7]*lnmilexpc + rpc.bih.glm[["coefficients"]][8]*lnmilexpt +
                  rpc.bih.glm[["coefficients"]][9]*SSA + rpc.bih.glm[["coefficients"]][10]*MENA +
                  rpc.bih.glm[["coefficients"]][11]*Americas + rpc.bih.glm[["coefficients"]][12]*Europe)

plot(rpc.cor$rpe_gdp,rpc.cor$est)

rpc.cor$estused[is.na(rpc.cor$rpe_gdp)&(!is.na(rpc.cor$est))] <- 1
rpc.cor$rpe_gdp[is.na(rpc.cor$rpe_gdp)&(!is.na(rpc.cor$est))] <- rpc.cor$est[is.na(rpc.cor$rpe_gdp)&(!is.na(rpc.cor$est))]
rpc.cor <- rpc.cor %>%
  dplyr::filter(year > 1945)

rpc <- rpc %>%
  dplyr::filter(year < 2014)

# add estimates
rpc_est <- read_excel("~/Documents/rpc_estimates.xlsx", sheet = 2) %>%
  dplyr::select(-method)

rpc_remove <- rpc_est %>%
  dplyr::select(iso3c,year) %>%
  dplyr::mutate(countryyear = paste(iso3c,year)) %>%
  dplyr::filter(iso3c != "YEM" | year >= 1990 )

rpc2 <- rpc %>%
  dplyr::mutate(countryyear = paste(iso3c,year)) %>%
  dplyr::filter(countryyear %!in% rpc_remove$countryyear) %>%
  dplyr::select(-countryyear)

rpc2 <- rbind(rpc2,rpc_est) %>%
  dplyr::rename(rpc = rpe_gdp) %>%
  dplyr::filter(year >= 1946) %>%
  unique() %>%
  na.omit() %>%
  dplyr::filter(iso3c != "YEM" | year >= 1990)

# remove post-Soviet and post-Yugoslav states for 1991
rpc2 <- rpc2 %>%
  dplyr::filter(iso3c %!in% c("ARM","AZE","BLR","EST","GEO","KAZ","KGZ","LTU","LVA","MDA","RUS",
                              "TJK","TKM","UKR","UZB") | year >= 1992,
           iso3c %!in% c("BIH","HRV","MKD","SRB","SVN") | year >= 1992)

# rpc2 <- rpc.cor %>%
#   dplyr::select(iso3c,year,rpe_gdp,estused) %>%
#   unique() %>%
#   dplyr::filter(year >= 1946)
# 
# rpc2 <- rpc2 %>%
#   dplyr::group_by(iso3c) %>%
#   dplyr::arrange(year) %>%
#   dplyr::mutate(rpc_est = na_interpolation(rpe_gdp), option = "linear") %>%
#   dplyr::ungroup() %>%
#   select(-c(rpe_gdp,option)) %>%
#   dplyr::rename(rpc = rpc_est)
# 
# rpc2 <- rpc2 %>%
#   dplyr::filter(iso3c %!in% c("ARM","AZE","BLR","EST","GEO","KAZ","KGZ","LTU","LVA","MDA","RUS",
#                               "TJK","TKM","UKR","UZB") | year >= 1992,
#                 iso3c %!in% c("BIH","HRV","MKD","SRB","SVN") | year >= 1992)
# 
# rpc.yar <- rpc2 %>%
#   dplyr::filter(iso3c == "YEM") %>%
#   dplyr::mutate(iso3c = "YAR")
# 
# rpc.ypr <- rpc2 %>%
#   dplyr::filter(iso3c == "YEM") %>%
#   dplyr::mutate(iso3c = "YPR")
# 
# rpc2 <- rpc2 %>%
#   rbind(rpc.yar,rpc.ypr)

# writes formatted dataframe as csv files
write.csv(rpc2,"Data files/Formatted data files/rpc.csv")
