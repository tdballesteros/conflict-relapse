# This script creates a second(?) tax revenue estimate.

# load libraries
library(readxl)
library(countrycode)
library(imputeTS)
library(dplyr)
library(tidyr)

### load datasets ------------------------------------------------------------
# World Bank - Tax Revenue (% of GDP) - GC.TAX.TOTL.GD.ZS
tax.wb <- read.csv("Data files/Raw data files/API_GC.TAX.TOTL.GD.ZS_DS2_en_csv_v2_1124880.csv",skip=4)
# IMF - General_government_Percent_of_GDP [imf2]
tax.imf2 <- readxl::read_excel("Data files/Raw data files/General_government_Percent_of_GDP_T.xlsx", sheet = 2)
# OECD - Total Tax Revenue (TOTALTAX) - Tax revenue as % of GDP (TAXGDP)
tax.oecd <- read.csv("Data files/Raw data files/RS_GBL_29062020081531984.csv")
# IMF World Revenue Longitudinal Data (WoRLD)
tax.imf <- readxl::read_excel("Data files/Raw data files/Tax_Revenue_in_Percent_of_GDP.xlsx", sheet = 1, skip = 1)
# ICTD / UNU-WIDER
tax.ictd <- readxl::read_excel("~/Downloads/ICTDWIDERGRD_2020.xlsx", sheet = 2)

### format datasets ------------------------------------------------------------
# format World Bank (wb) data
tax.wb <- tax.wb %>%
  dplyr::select(-c(Indicator.Name,Indicator.Code,X)) %>%
  tidyr::pivot_longer(3:62, names_to = "year", values_to = "value") %>%
  dplyr::mutate(year = as.numeric(str_sub(year,start=2,end=5))) %>%
  dplyr::filter(Country.Code %!in% c("ABW","AND","ARB","ASM","ATG","BHS","BMU","BRB","BRN","BTN","CEB",
                                     "CHI","COM","CPV","CSS","CUB","CUW","CYM","DJI","DMA","EAP","EAR",
                                     "EAS","ECA","ECS","EMU","ERI","EUU","FCS","FRO","FSM","GIB","GRD",
                                     "GRL","GUM","GUY","HIC","HKG","HPC","HTI","IBD","IBT","IDA","IDB",
                                     "IDX","IMN","INX","KIR","KNA","LAC","LBY","LCA","LCN","LDC","LIC",
                                     "LIE","LMC","LMY","LTE","MAC","MAF","MEA","MHL","MIC","MNA","MNE",
                                     "MNP","NAC","NCL","NRU","OED","OMN","OSS","PLW","PRE","PRI","PRK",
                                     "PSS","PST","PYF","QAT","SAS","SLB","SLE","SMR","SSA","SSD","SSF",
                                     "SST","STP","SUR","SWZ","SXM","SYR","TCA","TCD","TEA","TEC","TKM",
                                     "TLA","TMN","TON","TSA","TSS","TUV","UMC","VCT","VEN","VGB","VIR",
                                     "VNM","VUT","WLD","WSM","XKX","YEM")) %>%
  dplyr::select(-Country.Name) %>%
  dplyr::rename(iso3c = Country.Code,
                wb.value = value)

# format IMF (imf2) data
tax.imf2 <- tax.imf2 %>%
  tidyr::pivot_longer(2:49, names_to = "year", values_to = "value") %>%
  dplyr::rename(country = 1) %>%
  dplyr::filter(country != "NA" & country != "Scale: Units") %>%
  dplyr::rename(imf2.value = value) %>%
  dplyr::mutate(year = as.numeric(year),
                iso3c = countrycode::countrycode(country, "country.name", "iso3c"))

tax.imf2$iso3c[tax.imf2$country=="Kosovo, Rep. of"] <- "KSV"

tax.imf2 <- tax.imf2 %>%
  dplyr::select(-country)

# format OECD data
tax.oecd <- tax.oecd %>%
  dplyr::select(COU,Country,Year,Value) %>%
  dplyr::mutate(iso3c = countrycode::countrycode(Country, "country.name","iso3c")) %>%
  dplyr::filter(iso3c %!in% c("BHS","BRB","CPV","NA","COK","WSM","TKL","SLB","VUT","LIE","LCA")) %>%
  dplyr::select(iso3c,Year,Value) %>%
  dplyr::rename(year = Year,
                oecd.value = Value)

# format IMF WoRLD (imf) data
tax.imf <- tax.imf[,-seq(3,57,2)] %>%
  tidyr::pivot_longer(2:29, names_to = "year", values_to = "imf.value") %>%
  dplyr::rename(country = 1) %>%
  dplyr::mutate(year = as.numeric(year),
                iso3c = countrycode::countrycode(country,"country.name","iso3c")) %>%
  dplyr::select(iso3c,year,imf.value)

# format ICTD / UNU-WIDER (ictd) data
tax.ictd <- tax.ictd[-c(1:2),] %>%
  dplyr::select(Country,ISO,Year,`...24`) %>%
  dplyr::rename_with(tolower) %>%
  dplyr::rename(ictd.value = 4) %>%
  dplyr::mutate(year = as.numeric(year),
                ictd.value = as.numeric(ictd.value),
                iso3c = countrycode::countrycode(country,"country.name","iso3c"))

tax.ictd$iso3c[tax.ictd$country=="Kosovo"] <- "KSV"

tax.ictd <- tax.ictd %>%
  dplyr::select(iso3c,year,ictd.value) %>%
  dplyr::filter(iso3c %!in% c("ABW","AIA","ATG","BHS","BRB","BRN","BTN","DMA","FSM","GRD",
                              "HKG","KIR","KNA","LCA","MAC","MHL","MNE","MSR","NRU","PLW",
                              "SLB","SMR","TON","TUV","VCT","VUT","WSM"))

### merge datasets ------------------------------------------------------------
taxrev <- dplyr::full_join(tax.ictd,tax.oecd,by=c("iso3c","year")) %>%
  dplyr::full_join(tax.imf,by=c("iso3c","year")) %>%
  dplyr::full_join(tax.wb,by=c("iso3c","year")) %>%
  dplyr::full_join(tax.imf2,by=c("iso3c","year")) %>%
  dplyr::mutate(ictd.value = 100*ictd.value) %>%
  dplyr::filter(iso3c %!in% c("BHS","BTN",NA)) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(max = max(ictd.value,oecd.value,imf.value,wb.value,na.rm=TRUE),
                min = min(ictd.value,oecd.value,imf.value,wb.value,na.rm=TRUE))

taxrev$max[taxrev$max==-Inf] <- NA
taxrev$min[taxrev$min==Inf] <- NA

for(i in 1:nrow(taxrev)){
  taxrev$points[i] <- sum(!is.na(taxrev$ictd.value[i]),!is.na(taxrev$oecd.value[i]),!is.na(taxrev$imf.value[i]),
                          !is.na(taxrev$wb.value[i]))
}

taxrev <- taxrev %>%
  dplyr::filter(points > 0) %>%
  dplyr::mutate(diff = max - min) #%>%
# dplyr::filter(diff > 5)

hist(taxrev$diff,breaks=100)

# for(i in 1:nrow(taxrev)){
#   if(is.na(taxrev$oecd.value[i])){
#     if(!is.na(taxrev$imf.value[i])){
#       taxrev$oecd.value[i] <- taxrev$imf.value[i]
#     }
#     else{
#       if(!is.na(taxrev$ictd.value[i])){
#         taxrev$oecd.value[i] <- taxrev$ictd.value[i]
#       }
#       else{
#         if(!is.na(taxrev$wb.value[i])){
#           taxrev$oecd.value[i] <- taxrev$wb.value[i]
#         }
#       }
#     }
#   }
# }
# 
# taxrev$oecd.value[taxrev$year>=2018] <- NA
# 
# taxrev <- taxrev %>%
#   dplyr::select(iso3c,year,oecd.value) %>%
#   dplyr::group_by(iso3c) %>%
#   dplyr::arrange(year) %>%
#   dplyr::mutate(taxrev = na.interpolation(oecd.value), option = "spline") %>%
#   dplyr::ungroup() %>%
#   dplyr::select(-c(oecd.value,option))

# Tax revenue data - Artbetman and Kugler 2013
# https://www.researchgate.net/publication/271588312_RPC_v21_2013_components old version
tax.ak <- readxl::read_excel("Data files/Raw data files/RPC2015_components.xlsx")

tax.ak <- tax.ak %>%
  dplyr::select(country,year,tax) %>%
  dplyr::mutate(iso3c = countrycode::countrycode(country,"country.name","iso3c")) %>%
  dplyr::filter(country %!in% c("Faeore Islands","Netherlands Antilles"),
                iso3c %!in% c("ASM","AIA","ATG","ABW","BHS","BRB","BMU","BTN","BRN","CYM","HKG",
                              "MAC","DMA","PYF","GRL","GRD","GUM","KIR","MHL","FSM","MLT","MNE",
                              "MSR","NRU","NCL","SLB","KNA","LCA","VCT","TON","VUT","PLW","WSM",
                              "SMR"))

tax.ak$iso3c[tax.ak$country=="Guinea-Bissaau"] <- "GNB"

tax.ak <- tax.ak %>%
  dplyr::select(iso3c,year,tax) %>%
  dplyr::mutate(ak.value = tax * 100)

taxrev2 <- full_join(tax.ak,taxrev,by=c("iso3c","year")) %>%
  dplyr::mutate(est = NA)

### modeling values ------------------------------------------------------------
# load datasets
#dataset.shadow_economy <- "???" # shadow economy
#dataset.rpc <- read.csv("Data files/Formatted data files/rpc.csv")
dataset.polity_full <- read.csv("Data files/Formatted data files/polity_full.csv")
dataset.polity <- read.csv("Data files/Formatted data files/polity.csv")
dataset.military_capacity <- read.csv("Data files/Formatted data files/military_capacity.csv")
dataset.gdp <- read.csv("Data files/Formatted data files/gdp.csv")
dataset.population <- read.csv("Data files/Formatted data files/population.csv")

taxrev3 <- taxrev2 %>%
  #dplyr::full_join(dataset.shadow_economy,by=c("iso3c","year")) %>%
  #dplyr::full_join(dataset.rpc,by=c("iso3c","year")) %>%
  dplyr::left_join(dataset.polity_full,by=c("iso3c","year")) %>%
  dplyr::left_join(dataset.polity,by=c("iso3c","year")) %>%
  dplyr::left_join(dataset.military_capacity,by=c("iso3c","year")) %>%
  dplyr::left_join(dataset.gdp,by=c("iso3c","year")) %>%
  dplyr::left_join(dataset.population,by=c("iso3c","year")) %>%
  dplyr::select(iso3c,year,tax,polity,politysq,polity2,lnmilexpgdp,lntroops,lnmilexpc,
                lnmilexpt,gdp,pop.pd,ictd.value,oecd.value,imf.value,wb.value) %>%
  #dplyr::select(iso3c,year,tax,shec,rpc,polity,politysq,polity2,lnmilexpgdp,lntroops,lnmilexpc,
  #              lnmilexpt,gdp,pop.pd,ictd.value,oecd.value,imf.value,wb.value) %>%
  dplyr::mutate(gdppc = gdp / pop.pd,
                lnpop = log(pop.pd))

taxmodel.ictd <- (glm(tax ~ shec + rpc + year + polity2 + polity + politysq + lnmilexpgdp + lntroops + lnmilexpc +
                        lnmilexpt + gdppc + lnpop + ictd.value, data = taxrev3))
summary(taxmodel.ictd)

taxmodel.oecd <- (glm(tax ~ shec + rpc + year + polity2 + polity + politysq + lnmilexpgdp + lntroops + lnmilexpc +
                        lnmilexpt + gdppc + lnpop + oecd.value, data = taxrev3))
summary(taxmodel.oecd)

taxmodel.imf <- (glm(tax ~ shec + rpc + year + polity2 + polity + politysq + lnmilexpgdp + lntroops + lnmilexpc +
                       lnmilexpt + gdppc + lnpop + imf.value, data = taxrev3))
summary(taxmodel.imf)

taxmodel.wb <- (glm(tax ~ shec + rpc + year + polity2 + polity + politysq + lnmilexpgdp + lntroops + lnmilexpc +
                      lnmilexpt + gdppc + lnpop + wb.value, data = taxrev3))
summary(taxmodel.wb)

taxmodel.none <- (glm(tax ~ shec + rpc + year + polity2 + polity + politysq + lnmilexpgdp + lntroops + lnmilexpc +
                        lnmilexpt + gdppc + lnpop, data = taxrev3))
summary(taxmodel.none)

taxmodel.ictd.p <- (glm(tax ~ shec + rpc + year + lnmilexpgdp + lntroops + lnmilexpc +
                          lnmilexpt + gdppc + lnpop + ictd.value, data = taxrev3))
summary(taxmodel.ictd.p)

taxmodel.imf.p <- (glm(tax ~ shec + rpc + year + lnmilexpgdp + lntroops + lnmilexpc +
                         lnmilexpt + gdppc + lnpop + imf.value, data = taxrev3))
summary(taxmodel.imf.p)

taxmodel.none.p <- (glm(tax ~ shec + rpc + year + lnmilexpgdp + lntroops + lnmilexpc +
                          lnmilexpt + gdppc + lnpop, data = taxrev3))
summary(taxmodel.none.p)

taxrev3 <- taxrev3 %>%
  mutate(est.ictd = taxmodel.ictd[["coefficients"]][1] + taxmodel.ictd[["coefficients"]][2]*shec +
           taxmodel.ictd[["coefficients"]][3]*rpc + taxmodel.ictd[["coefficients"]][4]*year +
           taxmodel.ictd[["coefficients"]][5]*polity2 + taxmodel.ictd[["coefficients"]][6]*polity +
           taxmodel.ictd[["coefficients"]][7]*politysq + taxmodel.ictd[["coefficients"]][8]*lnmilexpgdp +
           taxmodel.ictd[["coefficients"]][9]*lntroops + taxmodel.ictd[["coefficients"]][10]*lnmilexpc +
           taxmodel.ictd[["coefficients"]][11]*lnmilexpt + taxmodel.ictd[["coefficients"]][12]*gdppc +
           taxmodel.ictd[["coefficients"]][13]*lnpop + taxmodel.ictd[["coefficients"]][14]*ictd.value) %>%
  mutate(est.oecd = taxmodel.oecd[["coefficients"]][1] + taxmodel.oecd[["coefficients"]][2]*shec +
           taxmodel.oecd[["coefficients"]][3]*rpc + taxmodel.oecd[["coefficients"]][4]*year +
           taxmodel.oecd[["coefficients"]][5]*polity2 + taxmodel.oecd[["coefficients"]][6]*polity +
           taxmodel.oecd[["coefficients"]][7]*politysq + taxmodel.oecd[["coefficients"]][8]*lnmilexpgdp +
           taxmodel.oecd[["coefficients"]][9]*lntroops + taxmodel.oecd[["coefficients"]][10]*lnmilexpc +
           taxmodel.oecd[["coefficients"]][11]*lnmilexpt + taxmodel.oecd[["coefficients"]][12]*gdppc +
           taxmodel.oecd[["coefficients"]][13]*lnpop + taxmodel.oecd[["coefficients"]][14]*oecd.value) %>%
  mutate(est.imf = taxmodel.imf[["coefficients"]][1] + taxmodel.imf[["coefficients"]][2]*shec +
           taxmodel.imf[["coefficients"]][3]*rpc + taxmodel.imf[["coefficients"]][4]*year +
           taxmodel.imf[["coefficients"]][5]*polity2 + taxmodel.imf[["coefficients"]][6]*polity +
           taxmodel.imf[["coefficients"]][7]*politysq + taxmodel.imf[["coefficients"]][8]*lnmilexpgdp +
           taxmodel.imf[["coefficients"]][9]*lntroops + taxmodel.imf[["coefficients"]][10]*lnmilexpc +
           taxmodel.imf[["coefficients"]][11]*lnmilexpt + taxmodel.imf[["coefficients"]][12]*gdppc +
           taxmodel.imf[["coefficients"]][13]*lnpop + taxmodel.imf[["coefficients"]][14]*imf.value) %>%
  mutate(est.wb = taxmodel.wb[["coefficients"]][1] + taxmodel.wb[["coefficients"]][2]*shec +
           taxmodel.wb[["coefficients"]][3]*rpc + taxmodel.wb[["coefficients"]][4]*year +
           taxmodel.wb[["coefficients"]][5]*polity2 + taxmodel.wb[["coefficients"]][6]*polity +
           taxmodel.wb[["coefficients"]][7]*politysq + taxmodel.wb[["coefficients"]][8]*lnmilexpgdp +
           taxmodel.wb[["coefficients"]][9]*lntroops + taxmodel.wb[["coefficients"]][10]*lnmilexpc +
           taxmodel.wb[["coefficients"]][11]*lnmilexpt + taxmodel.wb[["coefficients"]][12]*gdppc +
           taxmodel.wb[["coefficients"]][13]*lnpop + taxmodel.wb[["coefficients"]][14]*wb.value) %>%
  mutate(est.none = taxmodel.none[["coefficients"]][1] + taxmodel.none[["coefficients"]][2]*shec +
           taxmodel.none[["coefficients"]][3]*rpc + taxmodel.none[["coefficients"]][4]*year +
           taxmodel.none[["coefficients"]][5]*polity2 + taxmodel.none[["coefficients"]][6]*polity +
           taxmodel.none[["coefficients"]][7]*politysq + taxmodel.none[["coefficients"]][8]*lnmilexpgdp +
           taxmodel.none[["coefficients"]][9]*lntroops + taxmodel.none[["coefficients"]][10]*lnmilexpc +
           taxmodel.none[["coefficients"]][11]*lnmilexpt + taxmodel.none[["coefficients"]][12]*gdppc +
           taxmodel.none[["coefficients"]][13]*lnpop) %>%
  mutate(est.ictd.p = taxmodel.ictd.p[["coefficients"]][1] + taxmodel.ictd.p[["coefficients"]][2]*shec +
           taxmodel.ictd.p[["coefficients"]][3]*rpc + taxmodel.ictd.p[["coefficients"]][4]*year +
           taxmodel.ictd.p[["coefficients"]][5]*lnmilexpgdp + taxmodel.ictd.p[["coefficients"]][6]*lntroops +
           taxmodel.ictd.p[["coefficients"]][7]*lnmilexpc + taxmodel.ictd.p[["coefficients"]][8]*lnmilexpt +
           taxmodel.ictd.p[["coefficients"]][9]*gdppc + taxmodel.ictd.p[["coefficients"]][10]*lnpop +
           taxmodel.ictd.p[["coefficients"]][11]*ictd.value) %>%
  mutate(est.imf.p = taxmodel.imf.p[["coefficients"]][1] + taxmodel.imf.p[["coefficients"]][2]*shec +
           taxmodel.imf.p[["coefficients"]][3]*rpc + taxmodel.imf.p[["coefficients"]][4]*year +
           taxmodel.imf.p[["coefficients"]][5]*lnmilexpgdp + taxmodel.imf.p[["coefficients"]][6]*lntroops +
           taxmodel.imf.p[["coefficients"]][7]*lnmilexpc + taxmodel.imf.p[["coefficients"]][8]*lnmilexpt +
           taxmodel.imf.p[["coefficients"]][9]*gdppc + taxmodel.imf.p[["coefficients"]][10]*lnpop +
           taxmodel.imf.p[["coefficients"]][11]*imf.value) %>%
  mutate(est.none.p = taxmodel.none.p[["coefficients"]][1] + taxmodel.none.p[["coefficients"]][2]*shec +
           taxmodel.none.p[["coefficients"]][3]*rpc + taxmodel.none.p[["coefficients"]][4]*year +
           taxmodel.none.p[["coefficients"]][5]*lnmilexpgdp + taxmodel.none.p[["coefficients"]][6]*lntroops +
           taxmodel.none.p[["coefficients"]][7]*lnmilexpc + taxmodel.none.p[["coefficients"]][8]*lnmilexpt +
           taxmodel.none.p[["coefficients"]][9]*gdppc + taxmodel.none.p[["coefficients"]][10]*lnpop)

taxrevest <- read_excel("~/Documents/tax_rev_est.xlsx") %>%
  select(-method) %>%
  rename(tax = est) %>%
  mutate(countryyear = paste(iso3c,year))

taxrev4 <- taxrev2 %>%
  select(iso3c,year,tax) %>%
  mutate(countryyear = paste(iso3c,year)) %>%
  filter(countryyear %!in% taxrevest$countryyear) %>%
  rbind(taxrevest) %>%
  unique() %>%
  mutate(taxgdp = 100*tax) %>%
  select(-c(tax,countryyear))
