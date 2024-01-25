# This script creates an estimated tax revenue variable for each country-year.

### load libraries ----------------------------------------------------------------------
library(readxl)
library(countrycode)
library(imputeTS)
library(dplyr)
library(tidyr)

### load datasets ----------------------------------------------------------------------
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

### format datasets ----------------------------------------------------------------------
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

### merge datasets ----------------------------------------------------------------------
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

# countries to fill in 2014- with ICTD estimates
use.ictd <- c("AFG","AGO","ALB","ARG","AUT","BEL","BEN","BFA","BGR","BLR","BLZ","CAF","CAN","CHE","CHN","CIV","COM",
              "CPV","CZE","DEU","EGY","ESP","EST","ETH","FIN","FJI","FRA","GEO","GIN","GNB","GRC","GTM","GUY","HND",
              "HRV","HUN","IRL","ITA","JPN","KHM","KOR","KSV","LBN","LBY","LKA","LTU","LUX","LVA","MAR","MDG","MNG",
              "MOZ","NAM","NOR","NPL","NZL","PER","PHL","POL","PRT","SDN","SGP","SLE","SOM","SRB","SUR","SVN","SWZ",
              "SYC","TUN","UKR","UZB","VNM","ZAF","ZMB")

# with OECD estimates
use.oecd <- c("AUS","BOL","CHL","DNK","KAZ","KEN","MLI","MUS","MYS","NER","NGA","RWA","TGO","THA","UGA")

# with IMF estimates
use.imf <- c("BDI","BRA","COL","DJI","DZA","GHA","IDN","IND","ISL","ISR","JOR","PAK","PAN","ROU","SLV","TLS",
             "URY","USA","YEM")

# with WB estimates
use.wb <- c("ARM","AZE","BIH","COG","CRI","MKD","MMR","NIC","PSE","SVK","TTO","TUR")

for(i in 1:nrow(taxrev2)){
  if(taxrev2$iso3c[i] == "CUB" | taxrev2$iso3c[i] == "TKM"){
    taxrev2$est[i] <- taxrev2$ictd.value[i]
  }
  else if(taxrev2$year[i] >= 2012){
    if(taxrev2$iso3c[i] %in% use.ictd){
      taxrev2$est[i] <- taxrev2$ictd.value[i]
    }
    else if(taxrev2$iso3c[i] %in% use.oecd){
      taxrev2$est[i] <- taxrev2$oecd.value[i]
    }
    else if(taxrev2$iso3c[i] %in% use.imf){
      taxrev2$est[i] <- taxrev2$imf.value[i]
    }
    else if(taxrev2$iso3c[i] %in% use.wb){
      taxrev2$est[i] <- taxrev2$wb.value[i]
    }
  }
  else if(taxrev2$year[i] <= 2011){
    if(taxrev2$year[i] < 1995 & taxrev2$iso3c[i] == "ALB"){
      taxrev2$est[i] <- taxrev2$ictd.value[i]
    }
    else if(taxrev2$year[i] < 1997 & taxrev2$iso3c[i] == "GEO"){
      taxrev2$est[i] <- taxrev2$ictd.value[i]
    }
    else if(taxrev2$year[i] >= 1974 & taxrev2$year[i] <= 1997 & taxrev2$iso3c[i] == "KHM"){
      taxrev2$est[i] <- taxrev2$ictd.value[i]
    }
    else{
      taxrev2$est[i] <- taxrev2$ak.value[i]
    }
  }
}

### add estimates from spreadsheet ----------------------------------------------------------------------
# ARE
taxrev2$est[taxrev2$iso3c=="ARE"&taxrev2$year==2014] <- 18.26893705
taxrev2$est[taxrev2$iso3c=="ARE"&taxrev2$year==2015] <- 12.08599045
taxrev2$est[taxrev2$iso3c=="ARE"&taxrev2$year==2016] <- 8.607598368
taxrev2$est[taxrev2$iso3c=="ARE"&taxrev2$year==2017] <- 11.60619014
taxrev2$est[taxrev2$iso3c=="ARE"&taxrev2$year==2018] <- 13.53393468

# BGD
taxrev2$est[taxrev2$iso3c=="BGD"&taxrev2$year==2014] <- 9.968096903
taxrev2$est[taxrev2$iso3c=="BGD"&taxrev2$year==2015] <- 9.809919884
taxrev2$est[taxrev2$iso3c=="BGD"&taxrev2$year==2016] <- 10.1177561

# BHR
taxrev2$est[taxrev2$iso3c=="BHR"&taxrev2$year==2013] <- 4.442540232

# BWA
taxrev2$est[taxrev2$iso3c=="BWA"&taxrev2$year==2014] <- 24.5921955
taxrev2$est[taxrev2$iso3c=="BWA"&taxrev2$year==2015] <- 23.53972275
taxrev2$est[taxrev2$iso3c=="BWA"&taxrev2$year==2016] <- 19.88133131
taxrev2$est[taxrev2$iso3c=="BWA"&taxrev2$year==2017] <- 21.11585737
taxrev2$est[taxrev2$iso3c=="BWA"&taxrev2$year==2018] <- 18.68843541

# CMR
taxrev2$est[taxrev2$iso3c=="CMR"&taxrev2$year==2014] <- 20.49946545
taxrev2$est[taxrev2$iso3c=="CMR"&taxrev2$year==2015] <- 20.94121527
taxrev2$est[taxrev2$iso3c=="CMR"&taxrev2$year==2016] <- 20.20659197
taxrev2$est[taxrev2$iso3c=="CMR"&taxrev2$year==2017] <- 20.56936133

# COD
taxrev2$est[taxrev2$iso3c=="COD"&taxrev2$year==2014] <- 15.57179207
taxrev2$est[taxrev2$iso3c=="COD"&taxrev2$year==2015] <- 15.04740613
taxrev2$est[taxrev2$iso3c=="COD"&taxrev2$year==2016] <- 10.43251831
taxrev2$est[taxrev2$iso3c=="COD"&taxrev2$year==2017] <- 9.583485706

# COG
taxrev2$est[taxrev2$iso3c=="COG"&taxrev2$year==2014] <- 13.63729637
taxrev2$est[taxrev2$iso3c=="COG"&taxrev2$year==2015] <- 19.98480287
taxrev2$est[taxrev2$iso3c=="COG"&taxrev2$year==2016] <- 17.60418966
taxrev2$est[taxrev2$iso3c=="COG"&taxrev2$year==2017] <- 16.13646983
taxrev2$est[taxrev2$iso3c=="COG"&taxrev2$year==2017] <- 10.46882617

# CYP
taxrev2$est[taxrev2$iso3c=="CYP"&taxrev2$year==2014] <- 26.27601328
taxrev2$est[taxrev2$iso3c=="CYP"&taxrev2$year==2015] <- 25.61560807
taxrev2$est[taxrev2$iso3c=="CYP"&taxrev2$year==2016] <- 25.0147127
taxrev2$est[taxrev2$iso3c=="CYP"&taxrev2$year==2017] <- 25.58764499
taxrev2$est[taxrev2$iso3c=="CYP"&taxrev2$year==2018] <- 25.8847409

# DOM
taxrev2$est[taxrev2$iso3c=="DOM"&taxrev2$year==2014] <- 14.96886277
taxrev2$est[taxrev2$iso3c=="DOM"&taxrev2$year==2015] <- 14.35961342
taxrev2$est[taxrev2$iso3c=="DOM"&taxrev2$year==2016] <- 14.40302294
taxrev2$est[taxrev2$iso3c=="DOM"&taxrev2$year==2017] <- 14.51685803
taxrev2$est[taxrev2$iso3c=="DOM"&taxrev2$year==2018] <- 14.65658069

# ECU
taxrev2$est[taxrev2$iso3c=="ECU"&taxrev2$year==2014] <- 18.64979519
taxrev2$est[taxrev2$iso3c=="ECU"&taxrev2$year==2015] <- 20.63929291
taxrev2$est[taxrev2$iso3c=="ECU"&taxrev2$year==2016] <- 19.2802591
taxrev2$est[taxrev2$iso3c=="ECU"&taxrev2$year==2017] <- 19.27580903
taxrev2$est[taxrev2$iso3c=="ECU"&taxrev2$year==2018] <- 19.91608112

# ERI
taxrev2$est[taxrev2$iso3c=="ERI"&taxrev2$year==2014] <- 10.15451125
taxrev2$est[taxrev2$iso3c=="ERI"&taxrev2$year==2015] <- 9.928846778
taxrev2$est[taxrev2$iso3c=="ERI"&taxrev2$year==2016] <- 9.718149224
taxrev2$est[taxrev2$iso3c=="ERI"&taxrev2$year==2017] <- 9.663373161

# GAB
taxrev2$est[taxrev2$iso3c=="GAB"&taxrev2$year==2014] <- 20.90788186
taxrev2$est[taxrev2$iso3c=="GAB"&taxrev2$year==2015] <- 17.7587225
taxrev2$est[taxrev2$iso3c=="GAB"&taxrev2$year==2016] <- 15.87349836
taxrev2$est[taxrev2$iso3c=="GAB"&taxrev2$year==2017] <- 15.3432449
taxrev2$est[taxrev2$iso3c=="GAB"&taxrev2$year==2018] <- 14.14376947

# GBR
taxrev2$est[taxrev2$iso3c=="GBR"&taxrev2$year==2014] <- 28.00893549
taxrev2$est[taxrev2$iso3c=="GBR"&taxrev2$year==2015] <- 28.17257789
taxrev2$est[taxrev2$iso3c=="GBR"&taxrev2$year==2016] <- 28.57959746
taxrev2$est[taxrev2$iso3c=="GBR"&taxrev2$year==2017] <- 28.78515983
taxrev2$est[taxrev2$iso3c=="GBR"&taxrev2$year==2018] <- 28.81873415

# GMB
taxrev2$est[taxrev2$iso3c=="GMB"&taxrev2$year==2014] <- 16.75664759
taxrev2$est[taxrev2$iso3c=="GMB"&taxrev2$year==2015] <- 18.07161193
taxrev2$est[taxrev2$iso3c=="GMB"&taxrev2$year==2016] <- 17.03052275
taxrev2$est[taxrev2$iso3c=="GMB"&taxrev2$year==2017] <- 15.7314802
taxrev2$est[taxrev2$iso3c=="GMB"&taxrev2$year==2018] <- 16.09739348

# GNQ
taxrev2$est[taxrev2$iso3c=="GNQ"&taxrev2$year==2014] <- 16.85350059
taxrev2$est[taxrev2$iso3c=="GNQ"&taxrev2$year==2015] <- 20.55893911
taxrev2$est[taxrev2$iso3c=="GNQ"&taxrev2$year==2016] <- 11.53511179
taxrev2$est[taxrev2$iso3c=="GNQ"&taxrev2$year==2017] <- 10.8528141
taxrev2$est[taxrev2$iso3c=="GNQ"&taxrev2$year==2018] <- 10.11452592

# HTI
taxrev2$est[taxrev2$iso3c=="HTI"&taxrev2$year==2014] <- 8.113830202
taxrev2$est[taxrev2$iso3c=="HTI"&taxrev2$year==2015] <- 9.043231293
taxrev2$est[taxrev2$iso3c=="HTI"&taxrev2$year==2016] <- 9.262757144
taxrev2$est[taxrev2$iso3c=="HTI"&taxrev2$year==2017] <- 9.179423867
taxrev2$est[taxrev2$iso3c=="HTI"&taxrev2$year==2018] <- 8.476042628

# IRN
taxrev2$est[taxrev2$iso3c=="IRN"&taxrev2$year==2014] <- 8.272063273
taxrev2$est[taxrev2$iso3c=="IRN"&taxrev2$year==2015] <- 9.340435275
taxrev2$est[taxrev2$iso3c=="IRN"&taxrev2$year==2016] <- 10.46696695
taxrev2$est[taxrev2$iso3c=="IRN"&taxrev2$year==2017] <- 10.26830845

# IRQ
taxrev2$est[taxrev2$iso3c=="IRQ"&taxrev2$year==2014] <- 6.840683588
taxrev2$est[taxrev2$iso3c=="IRQ"&taxrev2$year==2015] <- 5.154794725
taxrev2$est[taxrev2$iso3c=="IRQ"&taxrev2$year==2016] <- 19.10519447
taxrev2$est[taxrev2$iso3c=="IRQ"&taxrev2$year==2017] <- 21.87642809

# JAM
taxrev2$est[taxrev2$iso3c=="JAM"&taxrev2$year==2014] <- 25.83939257
taxrev2$est[taxrev2$iso3c=="JAM"&taxrev2$year==2015] <- 26.88766844
taxrev2$est[taxrev2$iso3c=="JAM"&taxrev2$year==2016] <- 27.69407865
taxrev2$est[taxrev2$iso3c=="JAM"&taxrev2$year==2017] <- 28.47674052
taxrev2$est[taxrev2$iso3c=="JAM"&taxrev2$year==2018] <- 28.95764419

# KGZ
taxrev2$est[taxrev2$iso3c=="KGZ"&taxrev2$year==2014] <- 17.40541216
taxrev2$est[taxrev2$iso3c=="KGZ"&taxrev2$year==2015] <- 16.59598843
taxrev2$est[taxrev2$iso3c=="KGZ"&taxrev2$year==2016] <- 16.46507632
taxrev2$est[taxrev2$iso3c=="KGZ"&taxrev2$year==2017] <- 16.25658071
taxrev2$est[taxrev2$iso3c=="KGZ"&taxrev2$year==2018] <- 17.45063896

# KWT
taxrev2$est[taxrev2$iso3c=="KWT"&taxrev2$year==2014] <- 65.8773429
taxrev2$est[taxrev2$iso3c=="KWT"&taxrev2$year==2015] <- 88.44988251
taxrev2$est[taxrev2$iso3c=="KWT"&taxrev2$year==2016] <- 115.3017908
taxrev2$est[taxrev2$iso3c=="KWT"&taxrev2$year==2017] <- 127.7602715
taxrev2$est[taxrev2$iso3c=="KWT"&taxrev2$year==2018] <- 108.2611076

# LAO
taxrev2$est[taxrev2$iso3c=="LAO"&taxrev2$year==1982] <- 2.820894379
taxrev2$est[taxrev2$iso3c=="LAO"&taxrev2$year==1983] <- 2.10301157
taxrev2$est[taxrev2$iso3c=="LAO"&taxrev2$year==1984] <- 2.319173016
taxrev2$est[taxrev2$iso3c=="LAO"&taxrev2$year==1985] <- 1.200691015
taxrev2$est[taxrev2$iso3c=="LAO"&taxrev2$year==1986] <- 1.021805706
taxrev2$est[taxrev2$iso3c=="LAO"&taxrev2$year==1987] <- 0.883392099
taxrev2$est[taxrev2$iso3c=="LAO"&taxrev2$year==1988] <- 6.611291486
taxrev2$est[taxrev2$iso3c=="LAO"&taxrev2$year==1989] <- 4.637067287
taxrev2$est[taxrev2$iso3c=="LAO"&taxrev2$year==1990] <- 4.430524572
taxrev2$est[taxrev2$iso3c=="LAO"&taxrev2$year==1991] <- 5.439693795
taxrev2$est[taxrev2$iso3c=="LAO"&taxrev2$year==1992] <- 5.199001518
taxrev2$est[taxrev2$iso3c=="LAO"&taxrev2$year==1993] <- 4.72198086
taxrev2$est[taxrev2$iso3c=="LAO"&taxrev2$year==1994] <- 5.584127751
taxrev2$est[taxrev2$iso3c=="LAO"&taxrev2$year==1995] <- 5.49483224
taxrev2$est[taxrev2$iso3c=="LAO"&taxrev2$year==2014] <- 17.92200111
taxrev2$est[taxrev2$iso3c=="LAO"&taxrev2$year==2015] <- 17.00123913
taxrev2$est[taxrev2$iso3c=="LAO"&taxrev2$year==2016] <- 15.3478826
taxrev2$est[taxrev2$iso3c=="LAO"&taxrev2$year==2017] <- 15.11312099

# LBR
taxrev2$est[taxrev2$iso3c=="LBR"&taxrev2$year==2014] <- 19.4638304
taxrev2$est[taxrev2$iso3c=="LBR"&taxrev2$year==2015] <- 18.45366793
taxrev2$est[taxrev2$iso3c=="LBR"&taxrev2$year==2016] <- 19.35634928
taxrev2$est[taxrev2$iso3c=="LBR"&taxrev2$year==2017] <- 18.54441345
taxrev2$est[taxrev2$iso3c=="LBR"&taxrev2$year==2018] <- 18.31164472

# LSO
taxrev2$est[taxrev2$iso3c=="LSO"&taxrev2$year==2014] <- 49.14935508
taxrev2$est[taxrev2$iso3c=="LSO"&taxrev2$year==2015] <- 35.04338609
taxrev2$est[taxrev2$iso3c=="LSO"&taxrev2$year==2016] <- 29.35588649
taxrev2$est[taxrev2$iso3c=="LSO"&taxrev2$year==2017] <- 32.47998009
taxrev2$est[taxrev2$iso3c=="LSO"&taxrev2$year==2018] <- 32.70933656

# MDA
taxrev2$est[taxrev2$iso3c=="MDA"&taxrev2$year==2014] <- 23.03338547
taxrev2$est[taxrev2$iso3c=="MDA"&taxrev2$year==2015] <- 22.02524228
taxrev2$est[taxrev2$iso3c=="MDA"&taxrev2$year==2016] <- 21.91760073
taxrev2$est[taxrev2$iso3c=="MDA"&taxrev2$year==2017] <- 23.23938392
taxrev2$est[taxrev2$iso3c=="MDA"&taxrev2$year==2018] <- 23.89815934

# MDG
taxrev2$est[taxrev2$iso3c=="MDG"&taxrev2$year==2014] <- 9.666741129
taxrev2$est[taxrev2$iso3c=="MDG"&taxrev2$year==2015] <- 10.18595989
taxrev2$est[taxrev2$iso3c=="MDG"&taxrev2$year==2016] <- 10.575362
taxrev2$est[taxrev2$iso3c=="MDG"&taxrev2$year==2017] <- 11.23475891
taxrev2$est[taxrev2$iso3c=="MDG"&taxrev2$year==2018] <- 11.54925893

# MEX
taxrev2$est[taxrev2$iso3c=="MEX"&taxrev2$year==2014] <- 18.44943649
taxrev2$est[taxrev2$iso3c=="MEX"&taxrev2$year==2015] <- 21.43643156
taxrev2$est[taxrev2$iso3c=="MEX"&taxrev2$year==2016] <- 22.37778897
taxrev2$est[taxrev2$iso3c=="MEX"&taxrev2$year==2017] <- 21.66467165
taxrev2$est[taxrev2$iso3c=="MEX"&taxrev2$year==2018] <- 21.72162732

# MRT
taxrev2$est[taxrev2$iso3c=="MRT"&taxrev2$year==2014] <- 25.90611918
taxrev2$est[taxrev2$iso3c=="MRT"&taxrev2$year==2015] <- 25.88299522
taxrev2$est[taxrev2$iso3c=="MRT"&taxrev2$year==2016] <- 26.01568376
taxrev2$est[taxrev2$iso3c=="MRT"&taxrev2$year==2017] <- 27.57314763

# MWI
taxrev2$est[taxrev2$iso3c=="MWI"&taxrev2$year==2014] <- 24.25078595
taxrev2$est[taxrev2$iso3c=="MWI"&taxrev2$year==2015] <- 23.19553217
taxrev2$est[taxrev2$iso3c=="MWI"&taxrev2$year==2016] <- 24.06400238
taxrev2$est[taxrev2$iso3c=="MWI"&taxrev2$year==2017] <- 25.87187619

# NLD
taxrev2$est[taxrev2$iso3c=="NLD"&taxrev2$year==2014] <- 23.73288147
taxrev2$est[taxrev2$iso3c=="NLD"&taxrev2$year==2015] <- 24.4439034
taxrev2$est[taxrev2$iso3c=="NLD"&taxrev2$year==2016] <- 25.18743313
taxrev2$est[taxrev2$iso3c=="NLD"&taxrev2$year==2017] <- 26.42057514
taxrev2$est[taxrev2$iso3c=="NLD"&taxrev2$year==2018] <- 26.3213961

# OMN
taxrev2$est[taxrev2$iso3c=="OMN"&taxrev2$year==2014] <- 52.00659921
taxrev2$est[taxrev2$iso3c=="OMN"&taxrev2$year==2015] <- 60.64007934
taxrev2$est[taxrev2$iso3c=="OMN"&taxrev2$year==2016] <- 66.42171531
taxrev2$est[taxrev2$iso3c=="OMN"&taxrev2$year==2017] <- 61.03426764

# PNG
taxrev2$est[taxrev2$iso3c=="PNG"&taxrev2$year==2014] <- 26.85020916
taxrev2$est[taxrev2$iso3c=="PNG"&taxrev2$year==2015] <- 22.82736728
taxrev2$est[taxrev2$iso3c=="PNG"&taxrev2$year==2016] <- 19.41363438
taxrev2$est[taxrev2$iso3c=="PNG"&taxrev2$year==2017] <- 19.08475541
taxrev2$est[taxrev2$iso3c=="PNG"&taxrev2$year==2018] <- 20.3565697

# PRY
taxrev2$est[taxrev2$iso3c=="PRY"&taxrev2$year==2014] <- 14.09354317
taxrev2$est[taxrev2$iso3c=="PRY"&taxrev2$year==2015] <- 13.49518352
taxrev2$est[taxrev2$iso3c=="PRY"&taxrev2$year==2016] <- 13.19866206
taxrev2$est[taxrev2$iso3c=="PRY"&taxrev2$year==2017] <- 13.77517703
taxrev2$est[taxrev2$iso3c=="PRY"&taxrev2$year==2018] <- 13.73463291

# QAT
taxrev2$est[taxrev2$iso3c=="QAT"&taxrev2$year==2014] <- 27.54467835
taxrev2$est[taxrev2$iso3c=="QAT"&taxrev2$year==2015] <- 28.90019769
taxrev2$est[taxrev2$iso3c=="QAT"&taxrev2$year==2016] <- 25.97195253
taxrev2$est[taxrev2$iso3c=="QAT"&taxrev2$year==2017] <- 20.13513164

# ROU
taxrev2$est[taxrev2$iso3c=="ROU"&taxrev2$year==1981] <- 6.37360272
taxrev2$est[taxrev2$iso3c=="ROU"&taxrev2$year==1982] <- 5.918216119
taxrev2$est[taxrev2$iso3c=="ROU"&taxrev2$year==1983] <- 5.84587213
taxrev2$est[taxrev2$iso3c=="ROU"&taxrev2$year==1984] <- 5.885409333
taxrev2$est[taxrev2$iso3c=="ROU"&taxrev2$year==1985] <- 6.102740493
taxrev2$est[taxrev2$iso3c=="ROU"&taxrev2$year==1986] <- 6.484356849
taxrev2$est[taxrev2$iso3c=="ROU"&taxrev2$year==1987] <- 6.482603804
taxrev2$est[taxrev2$iso3c=="ROU"&taxrev2$year==1988] <- 6.573926289
taxrev2$est[taxrev2$iso3c=="ROU"&taxrev2$year==1989] <- 7.432651324

# RUS
taxrev2$est[taxrev2$iso3c=="RUS"&taxrev2$year==2014] <- 23.81842805
taxrev2$est[taxrev2$iso3c=="RUS"&taxrev2$year==2015] <- 21.30073173
taxrev2$est[taxrev2$iso3c=="RUS"&taxrev2$year==2016] <- 20.52785468
taxrev2$est[taxrev2$iso3c=="RUS"&taxrev2$year==2017] <- 22.54420133
taxrev2$est[taxrev2$iso3c=="RUS"&taxrev2$year==2018] <- 24.80533245

# SAU
taxrev2$est[taxrev2$iso3c=="SAU"&taxrev2$year==2014] <- 16.62225941
taxrev2$est[taxrev2$iso3c=="SAU"&taxrev2$year==2015] <- 19.99893493
taxrev2$est[taxrev2$iso3c=="SAU"&taxrev2$year==2016] <- 20.3322781
taxrev2$est[taxrev2$iso3c=="SAU"&taxrev2$year==2017] <- 20.26214037

# SEN
taxrev2$est[taxrev2$iso3c=="SEN"&taxrev2$year==2014] <- 19.74357801
taxrev2$est[taxrev2$iso3c=="SEN"&taxrev2$year==2015] <- 19.9062761
taxrev2$est[taxrev2$iso3c=="SEN"&taxrev2$year==2016] <- 20.66725315
taxrev2$est[taxrev2$iso3c=="SEN"&taxrev2$year==2017] <- 19.70402567

# SWE
taxrev2$est[taxrev2$iso3c=="SWE"&taxrev2$year==2014] <- 36.70641524
taxrev2$est[taxrev2$iso3c=="SWE"&taxrev2$year==2015] <- 37.51339353
taxrev2$est[taxrev2$iso3c=="SWE"&taxrev2$year==2016] <- 38.54029383
taxrev2$est[taxrev2$iso3c=="SWE"&taxrev2$year==2017] <- 39.02225685
taxrev2$est[taxrev2$iso3c=="SWE"&taxrev2$year==2018] <- 38.54955275

# TCD
taxrev2$est[taxrev2$iso3c=="TCD"&taxrev2$year==2014] <- 9.134050483
taxrev2$est[taxrev2$iso3c=="TCD"&taxrev2$year==2015] <- 9.529747025
taxrev2$est[taxrev2$iso3c=="TCD"&taxrev2$year==2016] <- 7.931808208
taxrev2$est[taxrev2$iso3c=="TCD"&taxrev2$year==2017] <- 10.13326709

# TJK
taxrev2$est[taxrev2$iso3c=="TJK"&taxrev2$year==1995] <- 9.105722901
taxrev2$est[taxrev2$iso3c=="TJK"&taxrev2$year==1996] <- 11.13954021
taxrev2$est[taxrev2$iso3c=="TJK"&taxrev2$year==1997] <- 10.78686219
taxrev2$est[taxrev2$iso3c=="TJK"&taxrev2$year==2014] <- 17.62817658
taxrev2$est[taxrev2$iso3c=="TJK"&taxrev2$year==2015] <- 17.05200584
taxrev2$est[taxrev2$iso3c=="TJK"&taxrev2$year==2016] <- 16.17621992
taxrev2$est[taxrev2$iso3c=="TJK"&taxrev2$year==2017] <- 16.89659805

# TZA
taxrev2$est[taxrev2$iso3c=="TZA"&taxrev2$year==2014] <- 16.05003342
taxrev2$est[taxrev2$iso3c=="TZA"&taxrev2$year==2015] <- 14.81942131
taxrev2$est[taxrev2$iso3c=="TZA"&taxrev2$year==2016] <- 16.0875223
taxrev2$est[taxrev2$iso3c=="TZA"&taxrev2$year==2017] <- 16.70363907
taxrev2$est[taxrev2$iso3c=="TZA"&taxrev2$year==2018] <- 16.23386637

# VEN
taxrev2$est[taxrev2$iso3c=="VEN"&taxrev2$year==2014] <- 15.26505077
taxrev2$est[taxrev2$iso3c=="VEN"&taxrev2$year==2015] <- 17.19703376
taxrev2$est[taxrev2$iso3c=="VEN"&taxrev2$year==2016] <- 12.09959716

# ZWE
taxrev2$est[taxrev2$iso3c=="ZWE"&taxrev2$year==2014] <- 27.86654185
taxrev2$est[taxrev2$iso3c=="ZWE"&taxrev2$year==2015] <- 27.43892828
taxrev2$est[taxrev2$iso3c=="ZWE"&taxrev2$year==2016] <- 24.01947098
taxrev2$est[taxrev2$iso3c=="ZWE"&taxrev2$year==2017] <- 20.4138143

# linear interpolation
taxrev2 <- taxrev2 %>%
  dplyr::filter(iso3c %!in% c("MDV","CPF","COM","LUX","MLT")) %>%
  dplyr::select(iso3c,year,est)

taxrev2 <- taxrev2 %>%
  dplyr::full_join(expand.grid(iso3c = unique(taxrev2$iso3c),year = c(1946:2019)), by=c("iso3c","year"))

taxrev2 <- taxrev2 %>%
  dplyr::group_by(iso3c) %>%
  dplyr::arrange(year) %>%
  dplyr::mutate(taxgdp = imputeTS::na_interpolation(est), option = "spline") %>%
  dplyr::ungroup() %>%
  dplyr::select(-c(est,option))

# writes formatted dataframe as csv files
write.csv(taxrev2,"Data files/Formatted data files/tax_revenue.csv")

# Need to fill in missing data for BIH,KHM,CUB,ERI,ETH,GEO,LAO,ROU,SRB,TJK,TLS,TKM,VNM
## ???
