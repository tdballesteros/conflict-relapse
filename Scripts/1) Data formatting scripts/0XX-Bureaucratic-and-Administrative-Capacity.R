
# ICRG bureaucratic and administrative quality


### load libraries ---------------------------------------------------------------------------------
library(readxl)
library(countrycode)
library(dplyr)
library(tidyverse)


### not in function --------------------------------------------------------------------------------
'%!in%' <- function(x,y)!('%in%'(x,y))


### load data files --------------------------------------------------------------------------------
# ICRG - Law and Order
icrg_law_and_order <- readxl::read_xls("Data files/Raw data files/Re__ICRG_data_access/3BResearchersDataset2020.xls",
                                       sheet = 10,
                                       skip = 7)

# ICRG - Bureaucratic Quality
icrg_bureau <- readxl::read_xls("Data files/Raw data files/Re__ICRG_data_access/3BResearchersDataset2020.xls",
                                sheet = 13,
                                skip = 7)

# ICRG - Corruption
icrg_corrupt <- read_excel("Data files/Raw data files/Re__ICRG_data_access/3BResearchersDataset2020.xls",
                           sheet = 7,
                           skip = 7)

# country-years dataset
cyears <- read.csv("Data files/Formatted data files/country_years.csv")


### format data ------------------------------------------------------------------------------------

## Law and Order
icrg_law_and_order <- icrg_law_and_order %>%
  tidyr::pivot_longer(2:37, names_to = "year", values_to = "lo") %>%
  dplyr::mutate(
    iso3c = dplyr::case_when(
      Country == "East Germany" ~ "DDR",
      Country == "West Germany" ~ "BRD",
      Country == "Czechoslovakia" ~ "CZE",
      Country == "Serbia & Montenegro *" ~ "YUG",
      Country == "USSR" ~ "SOV",
      .default = countrycode(Country, "country.name", "iso3c")
      ),
    year = as.numeric(year)
    ) %>%
  dplyr::filter(
    # filter out non-sovereign entities
    iso3c != "HKG",
    # filter for Czechoslovakia/Czechia
    Country != "Czechoslovakia" | year <= 1992,
    Country != "Czech Republic" | year > 1992
    ) %>%
  dplyr::select(iso3c, year, lo)

## Bureaucratic Quality
icrg_bureau <- icrg_bureau %>%
  tidyr::pivot_longer(2:37, names_to = "year", values_to = "bq") %>%
  dplyr::mutate(iso3c = dplyr::case_when(
    Country == "East Germany" ~ "DDR",
    Country == "West Germany" ~ "BRD",
    Country == "Czechoslovakia" ~ "CZE",
    Country == "Serbia & Montenegro *" ~ "YUG",
    Country == "USSR" ~ "SOV",
    .default = countrycode(Country, "country.name", "iso3c")
  ),
  year = as.numeric(year)
  ) %>%
  dplyr::filter(
    # filter out non-sovereign entities
    iso3c != "HKG",
    # filter for Czechoslovakia/Czechia
    Country != "Czechoslovakia" | year <= 1992,
    Country != "Czech Republic" | year > 1992
  ) %>%
  dplyr::select(iso3c, year, bq)


## Corruption
icrg_corrupt <- icrg_corrupt %>%
  pivot_longer(2:37, names_to = "year", values_to = "cr") %>%
  dplyr::mutate(iso3c = dplyr::case_when(
    Country == "East Germany" ~ "DDR",
    Country == "West Germany" ~ "BRD",
    Country == "Czechoslovakia" ~ "CZE",
    Country == "Serbia & Montenegro *" ~ "YUG",
    Country == "USSR" ~ "SOV",
    .default = countrycode(Country, "country.name", "iso3c")
  ),
  year = as.numeric(year)
  ) %>%
  dplyr::filter(
    # filter out non-sovereign entities
    iso3c != "HKG",
    # filter for Czechoslovakia/Czechia
    Country != "Czechoslovakia" | year <= 1992,
    Country != "Czech Republic" | year > 1992
  ) %>%
  dplyr::select(iso3c, year, cr)


### combine data -----------------------------------------------------------------------------------
icrg <- icrg_law_and_order %>%
  dplyr::full_join(icrg_bureau, by = c("iso3c", "year")) %>%
  dplyr::full_join(icrg_corrupt, by = c("iso3c","year")) %>%
  dplyr::full_join(cyears, by = c("iso3c", "year")) %>%
  dplyr::filter(cn == 1 | !is.na(lo) | !is.na(bq) | !is.na(cr),
                # dataset only contains data for 1984 onwards
                year >= 1984) %>%
  dplyr::select(iso3c, year, lo, bq, cr) %>%
  dplyr::arrange(year, iso3c)


### write data -------------------------------------------------------------------------------------
# writes formatted dataframe as csv files
write.csv(icrg, "Data files/Formatted data files/icrg_bureaucratic_capacity.csv", row.names = FALSE)



# bacpca <- prcomp(~  lo + bq + cr,
#                  data = icrg, retx = T, center = T, scale. = T)
# bacpca[["rotation"]]
# 
# bacpca1 <- bacpca$x[,1] %>%
#   as.data.frame()
# bacpca1 <- bacpca1 %>%
#   mutate(countryyear = row.names(bacpca1))
# names(bacpca1) <- c("pca1","countryyear")
# 
# bacpca1 <- bacpca1 %>%
#   mutate(iso3c = str_sub(countryyear,start=1,end=3)) %>%
#   mutate(year = str_sub(countryyear,start=4,end=8))
# bacpca1$year <- as.numeric(bacpca1$year)
# 
# bacpca1 <- bacpca1[,c("iso3c","year","pca1")] %>%
#   rename(ba.cap = pca1) %>%
#   mutate(ba.cap = -ba.cap)
# 
# 
# #### linear approximation ####
# ba <- full_join(laword,bq,by=c("iso3c","year")) %>%
#   full_join(cr,by=c("iso3c","year")) %>%
#   full_join(bacpca1,by=c("iso3c","year")) %>%
#   full_join(se,by=c("iso3c","year")) %>%
#   left_join(p5all,by=c("iso3c","year")) %>%
#   left_join(pol,by=c("iso3c","year")) %>%
#   left_join(cow,by=c("iso3c","year")) %>%
#   left_join(gdp,by=c("iso3c","year")) %>%
#   left_join(fiscap,by=c("iso3c","year")) %>%
#   left_join(reg2,by=c("iso3c")) %>%
#   mutate(lnpop = log(pop.pd))
# 
# ba2 <- ba %>%
#   select(lo,bq,cr,ba.cap,iso3c,shec,polity2,polity,politysq,lnmilexpgdp,lntroops,lnmilexpc,lnmilexpt,rpc,taxgdp,
#          lnaid2gdp,year,gdppc,lnpop,`Central America`,Carribean,`South America`,`Eastern Europe`,`Middle East`,
#          `North Africa`,
#          `East Africa`,`West Africa`,`Central Africa`,`Southern Africa`,`Central Asia`,`South Asia`,`East Asia`,
#          `Southeast Asia`,WEOG) %>%
#   na.omit()
# 
# ba.glm <- glm(ba.cap ~ shec + polity2 + polity + politysq + lnmilexpgdp + lntroops + lnmilexpc + lnmilexpt +
#                 rpc + taxgdp + lnaid2gdp + gdppc + lnpop + year + `Central America` + Carribean + `South America` +
#                 `Eastern Europe` + `Middle East` + `North Africa` + `East Africa` + `West Africa` + `Central Africa` +
#                 `Southern Africa` + `Central Asia` + `South Asia` + `East Asia` + `Southeast Asia` + WEOG, data = ba2)
# summary(ba.glm)
# 
# ba.fitted <- ba2 %>%
#   cbind(fitted(ba.glm))
# plot(ba.fitted$`fitted(ba.glm)`,ba.fitted$ba.cap)
# 
# 
# lo.glm <- glm(lo ~ shec + polity2 + polity + politysq + lnmilexpgdp + lntroops + lnmilexpc + lnmilexpt +
#                 rpc + taxgdp + lnaid2gdp + gdppc + lnpop + year + `Central America` + Carribean + `South America` +
#                 `Eastern Europe` + `Middle East` + `North Africa` + `East Africa` + `West Africa` + `Central Africa` +
#                 `Southern Africa` + `Central Asia` + `South Asia` + `East Asia` + `Southeast Asia` + WEOG, data = ba2)
# summary(lo.glm)
# 
# lo.fitted <- ba2 %>%
#   cbind(fitted(lo.glm))
# plot(lo.fitted$`fitted(lo.glm)`,lo.fitted$lo)
# 
# 
# bq.glm <- glm(bq ~ shec + polity2 + polity + politysq + lnmilexpgdp + lntroops + lnmilexpc + lnmilexpt +
#                 rpc + taxgdp + lnaid2gdp + year + `Central America` + Carribean + `South America` + `Eastern Europe` +
#                 `Middle East` + `North Africa` + `East Africa` + `West Africa` + `Central Africa` + `Southern Africa` +
#                 `Central Asia` + `South Asia` + `East Asia` + `Southeast Asia` + WEOG, data = ba2)
# summary(bq.glm)
# 
# bq.fitted <- ba2 %>%
#   cbind(fitted(bq.glm))
# plot(bq.fitted$`fitted(bq.glm)`,bq.fitted$bq)
# 
# 
# cr.glm <- glm(cr ~ shec + polity2 + polity + politysq + lnmilexpgdp + lntroops + lnmilexpc + lnmilexpt +
#                 rpc + taxgdp + lnaid2gdp + year + `Central America` + Carribean + `South America` + `Eastern Europe` +
#                 `Middle East` + `North Africa` + `East Africa` + `West Africa` + `Central Africa` + `Southern Africa` +
#                 `Central Asia` + `South Asia` + `East Asia` + `Southeast Asia` + WEOG, data = ba2)
# summary(cr.glm)
# 
# cr.fitted <- ba2 %>%
#   cbind(fitted(cr.glm))
# plot(cr.fitted$`fitted(cr.glm)`,cr.fitted$cr)
# 
# 
# #### plot country ts ####
# plot.bac <- function(iso = "USA"){
#   tmp <- bacpca1 %>%
#     filter(iso3c == iso) %>%
#     arrange(year)
#   
#   plot(tmp$year,tmp$ba.cap,type='l')
# }
