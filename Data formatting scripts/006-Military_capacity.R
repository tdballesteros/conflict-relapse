# This script creates a military capacity index score for each country-year.

#TODO: weight for PCA
#      Issue with GDPs in 2018-19
#      Weirdness with population b/t 1991-92 (might be how YUG and SOV are coded)
#      Add PSE
#      Issue with UKR 1991 (2x, v diff values)

# load libraries
library(readxl)
library(countrycode)
library(imputeTS)
library(mice)
library(mtsdi)
library(dplyr)
library(tidyr)

# not in function
'%!in%' <- function(x,y)!('%in%'(x,y))

#### countries without standardised iso3c codes
# Republic of Vietnam (South Vietnam) - RVN                  COW: RVN
# People's Democratic Republic of Yemen (South Yemen) - YPR  COW: YPR
# Yemen Arab Republic (North Yemen) -  YAR                   COW: YAR
# Zanzibar -  ZAN                                            COW: ZAN
# Kosovo -  KSV                                              COW: KOS*
# Yugoslavia - YUG                                           COW: YUG
# Czechoslovakia -  CZE                                      COW: CZE
# German Democratic Republic - DDR                           COW: GDR*
# Federal Republic of Germany - BRD                          COW: GFR*

# YAR, YPR -1989
# YEM 1990-
# BRD, DDR -1990
# DEU 1991-
# YUG -1991
# BIH, HRV, MKD, SRB, SVN 1992-
# KSV 2008-
# SOV -1991
# post-Soviet 1992-

#### upload COW files & updated data files ####
cow <- read_csv("Data files/Raw data files/NMC_5_0.csv")
new.milper <- read_excel("Data files/Raw data files/iiss_milper.xlsx")
new.milex <- read_excel("Data files/Raw data files/iiss_milex.xlsx")

#### munge cow ####
cow <- cow %>%
  dplyr::filter(stateabb != "SRB" | year != 2012) %>%
  dplyr::select(stateabb,year,milex,milper) %>%
  dplyr::filter(year >= 1946) %>%
  dplyr::mutate(iso3c = countrycode::countrycode(stateabb,"cowc","iso3c"))

cow$iso3c[cow$stateabb=="RVN"] <- "RVN"
cow$iso3c[cow$stateabb=="YPR"] <- "YPR"
cow$iso3c[cow$stateabb=="YAR"] <- "YAR"
cow$iso3c[cow$stateabb=="ZAN"] <- "ZAN"
cow$iso3c[cow$stateabb=="KOS"] <- "KSV"
cow$iso3c[cow$stateabb=="YUG"] <- "YUG"
cow$iso3c[cow$stateabb=="CZE"] <- "CZE"
cow$iso3c[cow$stateabb=="GDR"] <- "DDR"
cow$iso3c[cow$stateabb=="GFR"] <- "BRD"

cow <- cow %>%
  dplyr::select(iso3c,year,milex,milper)

cow$milex[cow$milex==-9] <- NA
cow$milper[cow$milper==-9] <- NA

cow <- cow %>%
  dplyr::mutate(milexpgdp = NA,
                milper = 1000 * milper,
                milex = 1000 * milex, # double check the multiplier
                milexpc = NA)

#### munge new files and add onto cow ####
new.milper <- new.milper %>%
  dplyr::mutate(milex = NA,
                milexpc = NA,
                milexpgdp = NA)

new.milex <- new.milex %>%
  dplyr::mutate(milper = NA) %>%
  dplyr::select(-`...6`)

cow <- cow %>%
  rbind(new.milper,new.milex)

#### merge so country-year is on a single line ####
cow <- cow %>%
  tidyr::pivot_longer(3:6, names_to = "variable", values_to = "value") %>%
  dplyr::group_by(iso3c,year,variable) %>%
  dplyr::summarise(value = mean(value,na.rm=T)) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(names_from = "variable", values_from = "value")

cow$milex[is.nan(cow$milex)] <- NA
cow$milexpc[is.nan(cow$milexpc)] <- NA
cow$milexpgdp[is.nan(cow$milexpgdp)] <- NA
cow$milper[is.nan(cow$milper)] <- NA

#### estimates entered from US WMEAT data
# ZWE
# 2008 and 2011 value is same (in constant 2014$), so applying deflated 2011 value to 2008
# same relationship between 2009 and 2010
cow$milex[cow$iso3c=="ZWE"&cow$year==2007] <- 219065654 # WMEAT 2019
cow$milex[cow$iso3c=="ZWE"&cow$year==2008] <- 192778000
cow$milex[cow$iso3c=="ZWE"&cow$year==2009] <- 96800000

# ZMB
cow$milex[cow$iso3c=="ZMB"&cow$year==1992] <- (69631057+95294117)/2

# YEM
# Estimates between IISS and WMEAT are pretty similar, so applying % change for 2013 and 2014 from 2012 value
# remembering to account for inflation (current year average)
cow$milex[cow$iso3c=="YEM"&cow$year==1990] <- 1403460990
cow$milex[cow$iso3c=="YEM"&cow$year==2013] <- 1704977800
cow$milex[cow$iso3c=="YEM"&cow$year==2014] <- 1879896330

# VNM
# WMEAT 1998
cow$milex[cow$iso3c=="VNM"&cow$year==1987] <- 1621512070

# UZB
# same as with YEM for the 2014 value
# same idea with 2008 and 2009, except average of values for base year of 2007 and for base year of 2010
# current year average exchange
cow$milex[cow$iso3c=="UZB"&cow$year==2008] <- (143184216 + 1105336326)/2
cow$milex[cow$iso3c=="UZB"&cow$year==2009] <- (167397797 + 1182549110)/2
cow$milex[cow$iso3c=="UZB"&cow$year==1992] <- 679637838
cow$milex[cow$iso3c=="UZB"&cow$year==1993] <- 828225260

cow$milex[cow$iso3c=="UZB"&cow$year==2014] <- 1642609920 # WMEAT 2019
cow$milex[cow$iso3c=="UZB"&cow$year==2015] <- 1936716510
cow$milex[cow$iso3c=="UZB"&cow$year==2016] <- 1942890120
cow$milex[cow$iso3c=="UZB"&cow$year==2017] <- 1632585014

# ARE
# YEM method again
# current year average exchange
# WMEAT 2019
cow$milex[cow$iso3c=="ARE"&cow$year==2014] <- 13536091400
cow$milex[cow$iso3c=="ARE"&cow$year==2015] <- 14394973100
cow$milex[cow$iso3c=="ARE"&cow$year==2016] <- 12256934900
cow$milex[cow$iso3c=="ARE"&cow$year==2017] <- 11386074130

# TTO
cow$milex[cow$iso3c=="TTO"&cow$year==1993] <- (75040040+6600493.68)/2

# TGO
cow$milex[cow$iso3c=="TGO"&cow$year==1992] <- (46218975+35645550)/2

# TKM
# UZB method for 2009 and 2011, YEM for 2014
# didn't go well...
# current yeare xchange rate
cow$milex[cow$iso3c=="TKM"&cow$year==2009] <- (70889487 + 143753504)/2      # yikes
cow$milex[cow$iso3c=="TKM"&cow$year==2011] <- (258326203 + 430526268)/2     # yikes 2
# WMEAT 2019
cow$milex[cow$iso3c=="TKM"&cow$year==2014] <- 736547836
cow$milex[cow$iso3c=="TKM"&cow$year==2015] <- 662766616
cow$milex[cow$iso3c=="TKM"&cow$year==2016] <- 788029028
cow$milex[cow$iso3c=="TKM"&cow$year==2017] <- 852951344

# TLS
# doesn't look promising...
# current year exchange rate
# am gonna have to use the 2004 estimate as a base for WMEAT 1995-2005 data
cow$milex[cow$iso3c=="TLS"&cow$year==2009] <- 1152761.02
cow$milex[cow$iso3c=="TLS"&cow$year==2008] <- 1344941.60
cow$milex[cow$iso3c=="TLS"&cow$year==2007] <- 955451.79
cow$milex[cow$iso3c=="TLS"&cow$year==2006] <- 777554.84
cow$milex[cow$iso3c=="TLS"&cow$year==2005] <- 388091.46
cow$milex[cow$iso3c=="TLS"&cow$year==2004] <- 173752.74
cow$milex[cow$iso3c=="TLS"&cow$year==2003] <- 130821.42
cow$milex[cow$iso3c=="TLS"&cow$year==2002] <- 128118.20

cow$milper[cow$iso3c=="TLS"&cow$year==2002] <- 1000
cow$milper[cow$iso3c=="TLS"&cow$year==2003] <- 1000
cow$milper[cow$iso3c=="TLS"&cow$year==2004] <- 1000

# SYR
# current year exchange rate
# WMEAT 2019
cow$milex[cow$iso3c=="SYR"&cow$year==2011] <- 2569660550
cow$milex[cow$iso3c=="SYR"&cow$year==2012] <- 2509339880
cow$milex[cow$iso3c=="SYR"&cow$year==2013] <- 2066513110
cow$milex[cow$iso3c=="SYR"&cow$year==2014] <- 1605855120
cow$milex[cow$iso3c=="SYR"&cow$year==2015] <- 1332103790
cow$milex[cow$iso3c=="SYR"&cow$year==2016] <- 836283600
cow$milex[cow$iso3c=="SYR"&cow$year==2017] <- 1005413378

# SWZ
# current year exchange rates
# also troops
# all from WMEAT 2004-2014 or WMEAT 1995-2005
cow$milex[cow$iso3c=="SWZ"&cow$year==1992] <- (12885174+17124654)/2
cow$milex[cow$iso3c=="SWZ"&cow$year==2002] <- 35950000
cow$milex[cow$iso3c=="SWZ"&cow$year==2003] <- 43290000
cow$milex[cow$iso3c=="SWZ"&cow$year==2004] <- 47700000
cow$milex[cow$iso3c=="SWZ"&cow$year==2005] <- 61510000
cow$milex[cow$iso3c=="SWZ"&cow$year==2006] <- 64060000
cow$milex[cow$iso3c=="SWZ"&cow$year==2007] <- 69940000
cow$milex[cow$iso3c=="SWZ"&cow$year==2008] <- 78480000
cow$milex[cow$iso3c=="SWZ"&cow$year==2009] <- 81350000
cow$milex[cow$iso3c=="SWZ"&cow$year==2010] <- 100660000
cow$milex[cow$iso3c=="SWZ"&cow$year==2011] <- 85340000
cow$milex[cow$iso3c=="SWZ"&cow$year==2012] <- 86550000
cow$milex[cow$iso3c=="SWZ"&cow$year==2013] <- 83340000
cow$milex[cow$iso3c=="SWZ"&cow$year==2014] <- 90000000

cow$milper[cow$iso3c=="SWZ"&cow$year==2001] <- 3000 # WMEAT 2005
cow$milper[cow$iso3c=="SWZ"&cow$year==2002] <- 3000 # WMEAT 2005
cow$milper[cow$iso3c=="SWZ"&cow$year==2003] <- 3000 # WMEAT 2005
cow$milper[cow$iso3c=="SWZ"&cow$year==2004] <- 3000
cow$milper[cow$iso3c=="SWZ"&cow$year==2005] <- 3000
cow$milper[cow$iso3c=="SWZ"&cow$year==2006] <- 3000
cow$milper[cow$iso3c=="SWZ"&cow$year==2007] <- 3000
cow$milper[cow$iso3c=="SWZ"&cow$year==2008] <- 3000
cow$milper[cow$iso3c=="SWZ"&cow$year==2009] <- 3000
cow$milper[cow$iso3c=="SWZ"&cow$year==2010] <- 3000
cow$milper[cow$iso3c=="SWZ"&cow$year==2011] <- 3000
cow$milper[cow$iso3c=="SWZ"&cow$year==2012] <- 3000
cow$milper[cow$iso3c=="SWZ"&cow$year==2013] <- 3000
cow$milper[cow$iso3c=="SWZ"&cow$year==2014] <- 3000

# SVN
cow$milex[cow$iso3c=="SVN"&cow$year==1993] <- (291354249+300709738)/2

# SOM
# current year exchange rates
# all based on 2009
# 2015-17 based on 2014 estimate using WMEAT 2019
# milper 2005-17 from WMEAT
cow$milex[cow$iso3c=="SOM"&cow$year==2005] <- 24553066
cow$milex[cow$iso3c=="SOM"&cow$year==2006] <- 31973988
cow$milex[cow$iso3c=="SOM"&cow$year==2007] <- 39282123
cow$milex[cow$iso3c=="SOM"&cow$year==2008] <- 51863698
cow$milex[cow$iso3c=="SOM"&cow$year==2010] <- 27394260
cow$milex[cow$iso3c=="SOM"&cow$year==2011] <- 28388320
cow$milex[cow$iso3c=="SOM"&cow$year==2012] <- 39601268
cow$milex[cow$iso3c=="SOM"&cow$year==2013] <- 55077887
cow$milex[cow$iso3c=="SOM"&cow$year==2014] <- 59923480
cow$milex[cow$iso3c=="SOM"&cow$year==2015] <- 63566573
cow$milex[cow$iso3c=="SOM"&cow$year==2016] <- 64092357
cow$milex[cow$iso3c=="SOM"&cow$year==2017] <- 68832334

cow$milper[cow$iso3c=="SOM"&cow$year==2005] <- 10000
cow$milper[cow$iso3c=="SOM"&cow$year==2006] <- 12000
cow$milper[cow$iso3c=="SOM"&cow$year==2007] <- 15000
cow$milper[cow$iso3c=="SOM"&cow$year==2008] <- 18000
cow$milper[cow$iso3c=="SOM"&cow$year==2009] <- 20000
cow$milper[cow$iso3c=="SOM"&cow$year==2010] <- 25000
cow$milper[cow$iso3c=="SOM"&cow$year==2011] <- 26000
cow$milper[cow$iso3c=="SOM"&cow$year==2012] <- 26000
cow$milper[cow$iso3c=="SOM"&cow$year==2013] <- 26000
cow$milper[cow$iso3c=="SOM"&cow$year==2014] <- 26000
cow$milper[cow$iso3c=="SOM"&cow$year==2015] <- 31000
cow$milper[cow$iso3c=="SOM"&cow$year==2016] <- 31000
cow$milper[cow$iso3c=="SOM"&cow$year==2017] <- 31000

# SLE
cow$milex[cow$iso3c=="SLE"&cow$year==1992] <- (16711454+13135388)/2

# SDN
# current year exchange rate
# oh boi...
cow$milex[cow$iso3c=="SDN"&cow$year==2006] <- (1564434450+523527874)/2
cow$milex[cow$iso3c=="SDN"&cow$year==2007] <- (1910377390+639547960)/2
cow$milex[cow$iso3c=="SDN"&cow$year==2008] <- (2209490480+740056140)/2
cow$milex[cow$iso3c=="SDN"&cow$year==2012] <- (1136922580+2128393150)/2

cow$milex[cow$iso3c=="SDN"&cow$year==2014] <- 1970218460 # WMEAT 2019
cow$milex[cow$iso3c=="SDN"&cow$year==2015] <- 1876911000
cow$milex[cow$iso3c=="SDN"&cow$year==2016] <- 2239349720
cow$milex[cow$iso3c=="SDN"&cow$year==2017] <- 3564773954
cow$milex[cow$iso3c=="SDN"&cow$year==2018] <- 1869674951 # SIPRI
cow$milex[cow$iso3c=="SDN"&cow$year==2019] <- 1728178030

# PAN
cow$milper[cow$iso3c=="PAN"&cow$year==2000] <- 7000
cow$milper[cow$iso3c=="PAN"&cow$year==2000] <- 6000

# QAT
cow$milex[cow$iso3c=="QAT"&cow$year==1982] <- (1249520760+269814967)/2
cow$milex[cow$iso3c=="QAT"&cow$year==1989] <- (119084979+869239270)/2
cow$milex[cow$iso3c=="QAT"&cow$year==1990] <- (112955020+824494470)/2
cow$milex[cow$iso3c=="QAT"&cow$year==1992] <- (811511751+802983472)/2
cow$milex[cow$iso3c=="QAT"&cow$year==2015] <- 4790830050 # WMEAT 2019
cow$milex[cow$iso3c=="QAT"&cow$year==2016] <- 4367966320 # WMEAT 2019
cow$milex[cow$iso3c=="QAT"&cow$year==2017] <- 3816134115 # WMEAT 2019

cow$milper[cow$iso3c=="QAT"&cow$year==2015] <- 12000 # WMEAT 2019

# NER
cow$milex[cow$iso3c=="NER"&cow$year==2014] <- (104056653+176159287)/2

# MUS
cow$milper[cow$iso3c=="MUS"&cow$year==2000] <- 500
cow$milper[cow$iso3c=="MUS"&cow$year==2001] <- 500
cow$milper[cow$iso3c=="MUS"&cow$year==2002] <- 500
cow$milper[cow$iso3c=="MUS"&cow$year==2003] <- 500
cow$milper[cow$iso3c=="MUS"&cow$year==2004] <- 0

# MRT
cow$milex[cow$iso3c=="MRT"&cow$year==1990] <- (38703932+37913656)/2

# MMR
# estimates from WMEAT 2019
cow$milex[cow$iso3c=="MRT"&cow$year==2008] <- (11102268900+1242671400)/2
cow$milex[cow$iso3c=="MRT"&cow$year==2009] <- (12108552200+1355850700)/2

# MLI
# from WMEAT 2019
cow$milper[cow$iso3c=="MLI"&cow$year==2017] <- 6858

# MDG
cow$milex[cow$iso3c=="MDG"&cow$year==1999] <- (43436994+227536707)/2
cow$milex[cow$iso3c=="MDG"&cow$year==2000] <- (47647058+249356829)/2
cow$milex[cow$iso3c=="MDG"&cow$year==2001] <- (61423483+321401152)/2

cow$milper[cow$iso3c=="MDG"&cow$year==2000] <- 15833
cow$milper[cow$iso3c=="MDG"&cow$year==2001] <- 15833

# LBY
# current year exchange rate
cow$milex[cow$iso3c=="LBY"&cow$year==2011] <- (3911403300+2047815400)/2
cow$milex[cow$iso3c=="LBY"&cow$year==1993] <- (1626880590+1532047120)/2

# WMEAT 2019
cow$milex[cow$iso3c=="LBY"&cow$year==2014] <- 4653787560
cow$milex[cow$iso3c=="LBY"&cow$year==2015] <- 3329770700
cow$milex[cow$iso3c=="LBY"&cow$year==2016] <- 3453235740
cow$milex[cow$iso3c=="LBY"&cow$year==2017] <- 3171374297

cow$milper[cow$iso3c=="LBY"&cow$year==2012] <- cow$milper[cow$iso3c=="LBY"&cow$year==2014]
cow$milper[cow$iso3c=="LBY"&cow$year==2013] <- cow$milper[cow$iso3c=="LBY"&cow$year==2014]
cow$milper[cow$iso3c=="LBY"&cow$year==2015] <- cow$milper[cow$iso3c=="LBY"&cow$year==2014]
cow$milper[cow$iso3c=="LBY"&cow$year==2016] <- cow$milper[cow$iso3c=="LBY"&cow$year==2014]
cow$milper[cow$iso3c=="LBY"&cow$year==2017] <- cow$milper[cow$iso3c=="LBY"&cow$year==2014]

# LBR
# current year exchange rate
cow$milex[cow$iso3c=="LBR"&cow$year==2008] <- 4085684.73
cow$milex[cow$iso3c=="LBR"&cow$year==2007] <- 3869015.13
cow$milex[cow$iso3c=="LBR"&cow$year==2006] <- 4724846.34
cow$milex[cow$iso3c=="LBR"&cow$year==2005] <- 8166206.01
cow$milex[cow$iso3c=="LBR"&cow$year==2004] <- 3517978.80

# LBN
cow$milex[cow$iso3c=="LBN"&cow$year==1987] <- (25869182+36360236)/2 # SIPRI

# LAO
# current year exchange rate
cow$milex[cow$iso3c=="LAO"&cow$year==2004] <- 11631529
# WMEAT 2019
cow$milex[cow$iso3c=="LAO"&cow$year==2015] <- 28848836
cow$milex[cow$iso3c=="LAO"&cow$year==2016] <- 29087456
cow$milex[cow$iso3c=="LAO"&cow$year==2017] <- 34521761

# KHM
# from SIPRI
cow$milex[cow$iso3c=="KHM"&cow$year==1986] <- 2525388.34
cow$milex[cow$iso3c=="KHM"&cow$year==1987] <- 2380925.17
cow$milex[cow$iso3c=="KHM"&cow$year==1988] <- 3738665.92
cow$milex[cow$iso3c=="KHM"&cow$year==1989] <- 7200664
cow$milex[cow$iso3c=="KHM"&cow$year==1990] <- 17549074

# KGZ
# from SIPRI
cow$milex[cow$iso3c=="KGZ"&cow$year==2015] <- 94016083
cow$milex[cow$iso3c=="KGZ"&cow$year==2016] <- 101223696
cow$milex[cow$iso3c=="KGZ"&cow$year==2017] <- 102972820
cow$milex[cow$iso3c=="KGZ"&cow$year==2018] <- 109543626
cow$milex[cow$iso3c=="KGZ"&cow$year==2019] <- 108891166

# KSV
# completely from WMEAT 2019, 2018-19 from SIPRI
cow$milex[cow$iso3c=="KSV"&cow$year==2008] <- 25000000
cow$milex[cow$iso3c=="KSV"&cow$year==2009] <- 30000000
cow$milex[cow$iso3c=="KSV"&cow$year==2010] <- 40000000
cow$milex[cow$iso3c=="KSV"&cow$year==2011] <- 50000000
cow$milex[cow$iso3c=="KSV"&cow$year==2012] <- 45000000
cow$milex[cow$iso3c=="KSV"&cow$year==2013] <- 50000000
cow$milex[cow$iso3c=="KSV"&cow$year==2014] <- 55000000
cow$milex[cow$iso3c=="KSV"&cow$year==2015] <- 50000000
cow$milex[cow$iso3c=="KSV"&cow$year==2016] <- 50000000
cow$milex[cow$iso3c=="KSV"&cow$year==2017] <- 55000000
cow$milex[cow$iso3c=="KSV"&cow$year==2018] <- 60488650
cow$milex[cow$iso3c=="KSV"&cow$year==2019] <- 64469293

cow$milper[cow$iso3c=="KSV"&cow$year==2008] <- 3000
cow$milper[cow$iso3c=="KSV"&cow$year==2009] <- 1000
cow$milper[cow$iso3c=="KSV"&cow$year==2010] <- 2000
cow$milper[cow$iso3c=="KSV"&cow$year==2011] <- 2000
cow$milper[cow$iso3c=="KSV"&cow$year==2012] <- 2000
cow$milper[cow$iso3c=="KSV"&cow$year==2013] <- 2000
cow$milper[cow$iso3c=="KSV"&cow$year==2014] <- 2000
cow$milper[cow$iso3c=="KSV"&cow$year==2015] <- 2000
cow$milper[cow$iso3c=="KSV"&cow$year==2016] <- 2000
cow$milper[cow$iso3c=="KSV"&cow$year==2017] <- 2000

# PRK
# current year exchange rate
# all from WMEAT, 2004-2014 (2015-17 from WMEAT 2019)
# WMEAT 1995-2005 estimates only use 2001 estimate as base, not estimated 2004 base
cow$milex[cow$iso3c=="PRK"&cow$year==2002] <- 2133067870
cow$milex[cow$iso3c=="PRK"&cow$year==2003] <- 2335167380
cow$milex[cow$iso3c=="PRK"&cow$year==2004] <- 2591580000
cow$milex[cow$iso3c=="PRK"&cow$year==2005] <- 3001740000
cow$milex[cow$iso3c=="PRK"&cow$year==2006] <- 3220130000
cow$milex[cow$iso3c=="PRK"&cow$year==2007] <- 3392240000
cow$milex[cow$iso3c=="PRK"&cow$year==2008] <- 3249840000
cow$milex[cow$iso3c=="PRK"&cow$year==2009] <- 2576080000
cow$milex[cow$iso3c=="PRK"&cow$year==2010] <- 3001370000
cow$milex[cow$iso3c=="PRK"&cow$year==2011] <- 3451650000
cow$milex[cow$iso3c=="PRK"&cow$year==2012] <- 3461800000
cow$milex[cow$iso3c=="PRK"&cow$year==2013] <- 3931670000
cow$milex[cow$iso3c=="PRK"&cow$year==2014] <- 4170000000
cow$milex[cow$iso3c=="PRK"&cow$year==2015] <- 3950000000
cow$milex[cow$iso3c=="PRK"&cow$year==2016] <- 3990000000
cow$milex[cow$iso3c=="PRK"&cow$year==2017] <- 4170000000

# IRQ
# current year exchange rate
cow$milex[cow$iso3c=="IRQ"&cow$year==2008] <- 3768765190
cow$milex[cow$iso3c=="IRQ"&cow$year==2007] <- 2326159520
cow$milex[cow$iso3c=="IRQ"&cow$year==2006] <- 1494290720
cow$milex[cow$iso3c=="IRQ"&cow$year==2005] <- 571683230
cow$milex[cow$iso3c=="IRQ"&cow$year==2004] <- 452719760
cow$milex[cow$iso3c=="IRQ"&cow$year==2003] <- (1362862870+541987061)/2
cow$milex[cow$iso3c=="IRQ"&cow$year==2002] <- (734899627+291876604)/2

# IRN
# WMEAT 1987
cow$milex[cow$iso3c=="IRN"&cow$year==1981] <- (3774584210+13815099200)/2

# HTI
# milex from SIPRI, milper from WMEAT
cow$milex[cow$iso3c=="HTI"&cow$year==1993] <- 53939521 # SIPRI
cow$milex[cow$iso3c=="HTI"&cow$year==1994] <- (45260643+47069933)/2 # SIPRI

cow$milper[cow$iso3c=="HTI"&cow$year==2000] <- 0
cow$milper[cow$iso3c=="HTI"&cow$year==2001] <- 0
cow$milper[cow$iso3c=="HTI"&cow$year==2002] <- 0
cow$milper[cow$iso3c=="HTI"&cow$year==2003] <- 0
cow$milper[cow$iso3c=="HTI"&cow$year==2004] <- 0
cow$milper[cow$iso3c=="HTI"&cow$year==2007] <- 0
cow$milper[cow$iso3c=="HTI"&cow$year==2008] <- 0

# GNQ
# current year exchange rate
# looks like a bad estimate
# 2015-16 from SIPRI, based on 2014 estimate
cow$milex[cow$iso3c=="GNQ"&cow$year==2014] <- 3761533
cow$milex[cow$iso3c=="GNQ"&cow$year==2015] <- 3653752.15 # estimate corroberated by using 2009 as base
cow$milex[cow$iso3c=="GNQ"&cow$year==2016] <- 3694566.86

# GNB
# current year exchange rate
cow$milex[cow$iso3c=="GNB"&cow$year==2014] <- 26518006 # WMEAT 2019
cow$milex[cow$iso3c=="GNB"&cow$year==2015] <- 21250378 # WMEAT 2019
cow$milex[cow$iso3c=="GNB"&cow$year==2016] <- 26782685 # WMEAT 2019
cow$milex[cow$iso3c=="GNB"&cow$year==2017] <- 32694581 # WMEAT 2019

cow$milex[cow$iso3c=="GNB"&cow$year==1992] <- (8755837+5890985.48)/2
cow$milex[cow$iso3c=="GNB"&cow$year==1993] <- (11569992+7784366.58)/2

# GMB
# milex from SIPRI
cow$milex[cow$iso3c=="GMB"&cow$year==1984] <- (10387762+1223912.98)/2
cow$milex[cow$iso3c=="GMB"&cow$year==1986] <- (1660180.35+874869.40)/2
cow$milex[cow$iso3c=="GMB"&cow$year==1992] <- (1235467.63+1185132.88)/2
cow$milex[cow$iso3c=="GMB"&cow$year==1991] <- (1518429.54+4117008.00)/2
cow$milex[cow$iso3c=="GMB"&cow$year==1990] <- (3609252.02+3462206.10)/2

cow$milex[cow$iso3c=="GMB"&cow$year==2015] <- 13973655 # WMEAT 2017
cow$milex[cow$iso3c=="GMB"&cow$year==2016] <- 11059294 # WMEAT 2017
cow$milex[cow$iso3c=="GMB"&cow$year==2017] <- 10325706 # WMEAT 2017
cow$milex[cow$iso3c=="GMB"&cow$year==2018] <- 10919009 # SIPRI based on 2014
cow$milex[cow$iso3c=="GMB"&cow$year==2019] <- 13686750 # SIPRI based on 2014

# GIN
# current year exchange rate
cow$milex[cow$iso3c=="GIN"&cow$year==2014] <- 48956316
cow$milex[cow$iso3c=="GIN"&cow$year==1992] <- (44748849+3844632.05)/2

# GEO
cow$milex[cow$iso3c=="GEO"&cow$year==1993] <- (327089949+50505452)/2

# GAB
cow$milex[cow$iso3c=="GAB"&cow$year==1993] <- 137338468
cow$milex[cow$iso3c=="GAB"&cow$year==1992] <- 124634916

# ERI
# current year exchange rate
cow$milex[cow$iso3c=="ERI"&cow$year==2006] <- (67694469.3+68032088.8)/2
cow$milex[cow$iso3c=="ERI"&cow$year==2007] <- (69290861.3+69636442.6)/2
cow$milex[cow$iso3c=="ERI"&cow$year==2008] <- (67098022.6+67401515.7)/2

cow$milex[cow$iso3c=="ERI"&cow$year==2014] <- 88304959 # WMEAT 2019
cow$milex[cow$iso3c=="ERI"&cow$year==2015] <- 100408034 # WMEAT 2019
cow$milex[cow$iso3c=="ERI"&cow$year==2016] <- 110076834 # WMEAT 2019
cow$milex[cow$iso3c=="ERI"&cow$year==2017] <- 129143593 # WMEAT 2019

# ECU
cow$milex[cow$iso3c=="ECU"&cow$year==1992] <- (278590655+580866235)/2

# DJI
# current year exchange rate
cow$milex[cow$iso3c=="DJI"&cow$year==2014] <- 12240104

# CRI
cow$milper[cow$iso3c=="CRI"&cow$year==2000] <- 5000
cow$milper[cow$iso3c=="CRI"&cow$year==2001] <- 4000

# CUB
# current year exchange rate
cow$milex[cow$iso3c=="CUB"&cow$year==2012] <- 128504790 # WMEAT 2017
cow$milex[cow$iso3c=="CUB"&cow$year==2013] <- 120105662 # WMEAT 2017
cow$milex[cow$iso3c=="CUB"&cow$year==2014] <- 122498562 # WMEAT 2017
cow$milex[cow$iso3c=="CUB"&cow$year==2015] <- 115607571 # WMEAT 2017
cow$milex[cow$iso3c=="CUB"&cow$year==2016] <- 116563807 # WMEAT 2017
cow$milex[cow$iso3c=="CUB"&cow$year==2017] <- 113377214 # WMEAT 2017
cow$milex[cow$iso3c=="CUB"&cow$year==1993] <- 690782377
cow$milex[cow$iso3c=="CUB"&cow$year==1994] <- 696081148 # number from COW looked wack so I replaced it
cow$milex[cow$iso3c=="CUB"&cow$year==2018] <- 111709264 # SIPRI based on 2011

# COG
# yikes [2002]
cow$milex[cow$iso3c=="COG"&cow$year==2002] <- (120411260+47896107)/2
cow$milex[cow$iso3c=="COG"&cow$year==1991] <- (144123264+140521265)/2
cow$milex[cow$iso3c=="COG"&cow$year==1993] <- (107045916+74721810)/2

# COD
cow$milex[cow$iso3c=="COD"&cow$year==2002] <- (723094959+96146337)/2
cow$milex[cow$iso3c=="COD"&cow$year==1992] <- 159321275

# CIV
# based on WMEAT 2018
cow$milper[cow$iso3c=="CIV"&cow$year==2013] <- 26308.33333
cow$milper[cow$iso3c=="CIV"&cow$year==2014] <- 26308.33333
cow$milper[cow$iso3c=="CIV"&cow$year==2015] <- 28700

# CAF
# only based on 1990, not 1992 (1992 was a less certain estimate)
cow$milex[cow$iso3c=="CAF"&cow$year==1991] <- 23453685

# BWA
cow$milex[cow$iso3c=="BWA"&cow$year==1992] <- (63309566+141006574)/2

# BEN
# current year exchange rate
cow$milex[cow$iso3c=="BEN"&cow$year==2005] <- (77169550+47049614)/2
cow$milex[cow$iso3c=="BEN"&cow$year==1993] <- (39606764+12510588)/2

# BLR
cow$milper[cow$iso3c=="BLR"&cow$year==1997] <- 78000
cow$milper[cow$iso3c=="BLR"&cow$year==1998] <- 78000

# BDI
cow$milex[cow$iso3c=="BDI"&cow$year==1992] <- (30834695+26270776)/2

# ARM
cow$milex[cow$iso3c=="ARM"&cow$year==1994] <- 71264675

# AGO
cow$milex[cow$iso3c=="AGO"&cow$year==1992] <- (690047211+1800969870)/2

# AFG
# current year exchange rate
cow$milex[cow$iso3c=="AFG"&cow$year==2004] <- 112438100
cow$milex[cow$iso3c=="AFG"&cow$year==2002] <- 69068457
cow$milex[cow$iso3c=="AFG"&cow$year==2003] <- 133300236

cow$milper[cow$iso3c=="AFG"&cow$year==2001] <- 45000 # WMEAT 2007(?)

# remove YUG values for 2012
cow <- cow %>%
  filter(iso3c != "YUG" | year != 2012)

#### ~inflation~ ####
# inflation from https://www.bls.gov/data/inflation_calculator.htm
# July to July, converting to 2019 dollars
for(i in 1:nrow(cow)){
  if(cow$year[i]==1946){
    cow$milex[i] = cow$milex[i]*(1+11.96)
    cow$milexpc[i] = cow$milexpc[i]*(1+11.96)
  }
  else if(cow$year[i]==1947){
    cow$milex[i] = cow$milex[i]*(1+10.56)
    cow$milexpc[i] = cow$milexpc[i]*(1+10.56)
  }
  else if(cow$year[i]==1948){
    cow$milex[i] = cow$milex[i]*(1+9.52)
    cow$milexpc[i] = cow$milexpc[i]*(1+9.52)
  }
  else if(cow$year[i]==1949){
    cow$milex[i] = cow$milex[i]*(1+9.83)
    cow$milexpc[i] = cow$milexpc[i]*(1+9.83)
  }
  else if(cow$year[i]==1950){
    cow$milex[i] = cow$milex[i]*(1+9.65)
    cow$milexpc[i] = cow$milexpc[i]*(1+9.65)
  }
  else if(cow$year[i]==1951){
    cow$milex[i] = cow$milex[i]*(1+8.91)
    cow$milexpc[i] = cow$milexpc[i]*(1+8.91)
  }
  else if(cow$year[i]==1952){
    cow$milex[i] = cow$milex[i]*(1+8.61)
    cow$milexpc[i] = cow$milexpc[i]*(1+8.61)
  }
  else if(cow$year[i]==1953){
    cow$milex[i] = cow$milex[i]*(1+8.57)
    cow$milexpc[i] = cow$milexpc[i]*(1+8.57)
  }
  else if(cow$year[i]==1954){
    cow$milex[i] = cow$milex[i]*(1+8.54)
    cow$milexpc[i] = cow$milexpc[i]*(1+8.54)
  }
  else if(cow$year[i]==1955){
    cow$milex[i] = cow$milex[i]*(1+8.57)
    cow$milexpc[i] = cow$milexpc[i]*(1+8.57)
  }
  else if(cow$year[i]==1956){
    cow$milex[i] = cow$milex[i]*(1+8.36)
    cow$milexpc[i] = cow$milexpc[i]*(1+8.36)
  }
  else if(cow$year[i]==1957){
    cow$milex[i] = cow$milex[i]*(1+8.07)
    cow$milexpc[i] = cow$milexpc[i]*(1+8.07)
  }
  else if(cow$year[i]==1958){
    cow$milex[i] = cow$milex[i]*(1+7.85)
    cow$milexpc[i] = cow$milexpc[i]*(1+7.85)
  }
  else if(cow$year[i]==1959){
    cow$milex[i] = cow$milex[i]*(1+7.79)
    cow$milexpc[i] = cow$milexpc[i]*(1+7.79)
  }
  else if(cow$year[i]==1960){
    cow$milex[i] = cow$milex[i]*(1+7.67)
    cow$milexpc[i] = cow$milexpc[i]*(1+7.67)
  }
  else if(cow$year[i]==1961){
    cow$milex[i] = cow$milex[i]*(1+7.55)
    cow$milexpc[i] = cow$milexpc[i]*(1+7.55)
  }
  else if(cow$year[i]==1962){
    cow$milex[i] = cow$milex[i]*(1+7.47)
    cow$milexpc[i] = cow$milexpc[i]*(1+7.47)
  }
  else if(cow$year[i]==1963){
    cow$milex[i] = cow$milex[i]*(1+7.36)
    cow$milexpc[i] = cow$milexpc[i]*(1+7.36)
  }
  else if(cow$year[i]==1964){
    cow$milex[i] = cow$milex[i]*(1+7.25)
    cow$milexpc[i] = cow$milexpc[i]*(1+7.25)
  }
  else if(cow$year[i]==1965){
    cow$milex[i] = cow$milex[i]*(1+7.12)
    cow$milexpc[i] = cow$milexpc[i]*(1+7.12)
  }
  else if(cow$year[i]==1966){
    cow$milex[i] = cow$milex[i]*(1+6.89)
    cow$milexpc[i] = cow$milexpc[i]*(1+6.89)
  }
  else if(cow$year[i]==1967){
    cow$milex[i] = cow$milex[i]*(1+6.68)
    cow$milexpc[i] = cow$milexpc[i]*(1+6.68)
  }
  else if(cow$year[i]==1968){
    cow$milex[i] = cow$milex[i]*(1+6.35)
    cow$milexpc[i] = cow$milexpc[i]*(1+6.35)
  }
  else if(cow$year[i]==1969){
    cow$milex[i] = cow$milex[i]*(1+5.97)
    cow$milexpc[i] = cow$milexpc[i]*(1+5.97)
  }
  else if(cow$year[i]==1970){
    cow$milex[i] = cow$milex[i]*(1+5.58)
    cow$milexpc[i] = cow$milexpc[i]*(1+5.58)
  }
  else if(cow$year[i]==1971){
    cow$milex[i] = cow$milex[i]*(1+5.30)
    cow$milexpc[i] = cow$milexpc[i]*(1+5.30)
  }
  else if(cow$year[i]==1972){
    cow$milex[i] = cow$milex[i]*(1+5.12)
    cow$milexpc[i] = cow$milexpc[i]*(1+5.12)
  }
  else if(cow$year[i]==1973){
    cow$milex[i] = cow$milex[i]*(1+4.79)
    cow$milexpc[i] = cow$milexpc[i]*(1+4.79)
  }
  else if(cow$year[i]==1974){
    cow$milex[i] = cow$milex[i]*(1+4.19)
    cow$milexpc[i] = cow$milexpc[i]*(1+4.19)
  }
  else if(cow$year[i]==1975){
    cow$milex[i] = cow$milex[i]*(1+3.73)
    cow$milexpc[i] = cow$milexpc[i]*(1+3.73)
  }
  else if(cow$year[i]==1976){
    cow$milex[i] = cow$milex[i]*(1+3.49)
    cow$milexpc[i] = cow$milexpc[i]*(1+3.49)
  }
  else if(cow$year[i]==1977){
    cow$milex[i] = cow$milex[i]*(1+3.21)
    cow$milexpc[i] = cow$milexpc[i]*(1+3.21)
  }
  else if(cow$year[i]==1978){
    cow$milex[i] = cow$milex[i]*(1+2.91)
    cow$milexpc[i] = cow$milexpc[i]*(1+2.91)
  }
  else if(cow$year[i]==1979){
    cow$milex[i] = cow$milex[i]*(1+2.51)
    cow$milexpc[i] = cow$milexpc[i]*(1+2.51)
  }
  else if(cow$year[i]==1980){
    cow$milex[i] = cow$milex[i]*(1+2.10)
    cow$milexpc[i] = cow$milexpc[i]*(1+2.10)
  }
  else if(cow$year[i]==1981){
    cow$milex[i] = cow$milex[i]*(1+1.80)
    cow$milexpc[i] = cow$milexpc[i]*(1+1.80)
  }
  else if(cow$year[i]==1982){
    cow$milex[i] = cow$milex[i]*(1+1.63)
    cow$milexpc[i] = cow$milexpc[i]*(1+1.63)
  }
  else if(cow$year[i]==1983){
    cow$milex[i] = cow$milex[i]*(1+1.57)
    cow$milexpc[i] = cow$milexpc[i]*(1+1.57)
  }
  else if(cow$year[i]==1984){
    cow$milex[i] = cow$milex[i]*(1+1.46)
    cow$milexpc[i] = cow$milexpc[i]*(1+1.46)
  }
  else if(cow$year[i]==1985){
    cow$milex[i] = cow$milex[i]*(1+1.38)
    cow$milexpc[i] = cow$milexpc[i]*(1+1.38)
  }
  else if(cow$year[i]==1986){
    cow$milex[i] = cow$milex[i]*(1+1.34)
    cow$milexpc[i] = cow$milexpc[i]*(1+1.34)
  }
  else if(cow$year[i]==1987){
    cow$milex[i] = cow$milex[i]*(1+1.25)
    cow$milexpc[i] = cow$milexpc[i]*(1+1.25)
  }
  else if(cow$year[i]==1988){
    cow$milex[i] = cow$milex[i]*(1+1.17)
    cow$milexpc[i] = cow$milexpc[i]*(1+1.17)
  }
  else if(cow$year[i]==1989){
    cow$milex[i] = cow$milex[i]*(1+1.06)
    cow$milexpc[i] = cow$milexpc[i]*(1+1.06)
  }
  else if(cow$year[i]==1990){
    cow$milex[i] = cow$milex[i]*(1+0.97)
    cow$milexpc[i] = cow$milexpc[i]*(1+0.97)
  }
  else if(cow$year[i]==1991){
    cow$milex[i] = cow$milex[i]*(1+0.88)
    cow$milexpc[i] = cow$milexpc[i]*(1+0.88)
  }
  else if(cow$year[i]==1992){
    cow$milex[i] = cow$milex[i]*(1+0.83)
    cow$milexpc[i] = cow$milexpc[i]*(1+0.83)
  }
  else if(cow$year[i]==1993){
    cow$milex[i] = cow$milex[i]*(1+0.78)
    cow$milexpc[i] = cow$milexpc[i]*(1+0.78)
  }
  else if(cow$year[i]==1994){
    cow$milex[i] = cow$milex[i]*(1+0.73)
    cow$milexpc[i] = cow$milexpc[i]*(1+0.73)
  }
  else if(cow$year[i]==1995){
    cow$milex[i] = cow$milex[i]*(1+0.68)
    cow$milexpc[i] = cow$milexpc[i]*(1+0.68)
  }
  else if(cow$year[i]==1996){
    cow$milex[i] = cow$milex[i]*(1+0.63)
    cow$milexpc[i] = cow$milexpc[i]*(1+0.63)
  }
  else if(cow$year[i]==1997){
    cow$milex[i] = cow$milex[i]*(1+0.60)
    cow$milexpc[i] = cow$milexpc[i]*(1+0.60)
  }
  else if(cow$year[i]==1998){
    cow$milex[i] = cow$milex[i]*(1+0.57)
    cow$milexpc[i] = cow$milexpc[i]*(1+0.57)
  }
  else if(cow$year[i]==1999){
    cow$milex[i] = cow$milex[i]*(1+0.54)
    cow$milexpc[i] = cow$milexpc[i]*(1+0.54)
  }
  else if(cow$year[i]==2000){
    cow$milex[i] = cow$milex[i]*(1+0.48)
    cow$milexpc[i] = cow$milexpc[i]*(1+0.48)
  }
  else if(cow$year[i]==2001){
    cow$milex[i] = cow$milex[i]*(1+0.45)
    cow$milexpc[i] = cow$milexpc[i]*(1+0.45)
  }
  else if(cow$year[i]==2002){
    cow$milex[i] = cow$milex[i]*(1+0.42)
    cow$milexpc[i] = cow$milexpc[i]*(1+0.42)
  }
  else if(cow$year[i]==2003){
    cow$milex[i] = cow$milex[i]*(1+0.40)
    cow$milexpc[i] = cow$milexpc[i]*(1+0.40)
  }
  else if(cow$year[i]==2004){
    cow$milex[i] = cow$milex[i]*(1+0.35)
    cow$milexpc[i] = cow$milexpc[i]*(1+0.35)
  }
  else if(cow$year[i]==2005){
    cow$milex[i] = cow$milex[i]*(1+0.31)
    cow$milexpc[i] = cow$milexpc[i]*(1+0.31)
  }
  else if(cow$year[i]==2006){
    cow$milex[i] = cow$milex[i]*(1+0.26)
    cow$milexpc[i] = cow$milexpc[i]*(1+0.26)
  }
  else if(cow$year[i]==2007){
    cow$milex[i] = cow$milex[i]*(1+0.23)
    cow$milexpc[i] = cow$milexpc[i]*(1+0.23)
  }
  else if(cow$year[i]==2008){
    cow$milex[i] = cow$milex[i]*(1+0.17)
    cow$milexpc[i] = cow$milexpc[i]*(1+0.17)
  }
  else if(cow$year[i]==2009){
    cow$milex[i] = cow$milex[i]*(1+0.19)
    cow$milexpc[i] = cow$milexpc[i]*(1+0.19)
  }
  else if(cow$year[i]==2010){
    cow$milex[i] = cow$milex[i]*(1+0.18)
    cow$milexpc[i] = cow$milexpc[i]*(1+0.18)
  }
  else if(cow$year[i]==2011){
    cow$milex[i] = cow$milex[i]*(1+0.14)
    cow$milexpc[i] = cow$milexpc[i]*(1+0.14)
  }
  else if(cow$year[i]==2012){
    cow$milex[i] = cow$milex[i]*(1+0.12)
    cow$milexpc[i] = cow$milexpc[i]*(1+0.12)
  }
  else if(cow$year[i]==2013){
    cow$milex[i] = cow$milex[i]*(1+0.10)
    cow$milexpc[i] = cow$milexpc[i]*(1+0.10)
  }
  else if(cow$year[i]==2014){
    cow$milex[i] = cow$milex[i]*(1+0.08)
    cow$milexpc[i] = cow$milexpc[i]*(1+0.08)
  }
  else if(cow$year[i]==2015){
    cow$milex[i] = cow$milex[i]*(1+0.08)
    cow$milexpc[i] = cow$milexpc[i]*(1+0.08)
  }
  else if(cow$year[i]==2016){
    cow$milex[i] = cow$milex[i]*(1+0.07)
    cow$milexpc[i] = cow$milexpc[i]*(1+0.07)
  }
  else if(cow$year[i]==2017){
    cow$milex[i] = cow$milex[i]*(1+0.05)
    cow$milexpc[i] = cow$milexpc[i]*(1+0.05)
  }
  else if(cow$year[i]==2018){
    cow$milex[i] = cow$milex[i]*(1+0.02)
    cow$milexpc[i] = cow$milexpc[i]*(1+0.02)
  }
}

srb2 <- cow %>%
  dplyr::filter(iso3c == "YUG",year > 1991) %>%
  dplyr::mutate(iso3c = "SRB")

cow <- cow %>%
  dplyr::filter(iso3c != "YUG" | year <= 1991) %>%
  rbind(srb2)

#### Fixing merge/split countries ####
cow <- cow %>%
  dplyr::full_join(expand.grid(iso3c = unique(cow$iso3c), year = c(1946:2019)))

cow$iso3c[cow$iso3c=="RUS"&cow$year<=1991] <- "SOV"

cow <- cow %>%
  dplyr::filter(iso3c != "YAR" | year <= 1989) %>%
  dplyr::filter(iso3c != "YPR" | year <= 1989) %>%
  dplyr::filter(iso3c != "YEM" | year >= 1990) %>%
  dplyr::filter(iso3c != "BRD" | year <= 1990) %>%
  dplyr::filter(iso3c != "DDR" | year <= 1990) %>%
  dplyr::filter(iso3c != "DEU" | year >= 1991) %>%
  dplyr::filter(iso3c %!in% c("ARM","AZE","BLR","EST","GEO","KAZ","KGZ","LTU","LVA","MDA",
                              "RUS","TJK","TKM","UZB") | year >= 1992,
                iso3c %!in% c("BIH","HRV","MKD","SRB","SVN") | year >= 1992,
                iso3c != "KSV" | year >= 2008)

#### linearly interpolate missing values ####
cow <- cow %>%
  dplyr::filter(iso3c %!in% c("BHS","BRB","DMA","GRD","LCA","VCT","ATG","KNA","BLZ","GUY","SUR","LUX",
                              "MCO","LIE","AND","SMR","MLT","MNE","ISL","CPV","STP","ZAN","COM","SYC",
                              "BTN","MDV","BRN","VUT","SLB","KIR","TUV","FJI","TON","NRU","MHL","PLW",
                              "FSM","WSM","DJI"))

cow.milex <- cow %>%
  dplyr::full_join(expand.grid(iso3c = unique(cow$iso3c), year = c(1946:2019))) %>%
  dplyr::filter(iso3c != "PSE") %>%
  dplyr::select(iso3c,year,milex) %>%
  dplyr::group_by(iso3c) %>%
  dplyr::arrange(year) %>%
  dplyr::mutate(milex.approx = imputeTS::na_interpolation(milex, option = "linear")) %>%
  dplyr::ungroup() %>%
  dplyr::select(iso3c,year,milex.approx) %>%
  dplyr::rename(milex = milex.approx)

cow.milper <- cow %>%
  dplyr::full_join(expand.grid(iso3c = unique(cow$iso3c), year = c(1946:2019))) %>%
  dplyr::filter(iso3c != "PSE") %>%
  dplyr::select(iso3c,year,milper) %>%
  dplyr::group_by(iso3c) %>%
  dplyr::arrange(year) %>%
  dplyr::mutate(milper.approx = imputeTS::na_interpolation(milper, option = "linear")) %>%
  dplyr::ungroup() %>%
  dplyr::select(iso3c,year,milper.approx) %>%
  dplyr::rename(milper = milper.approx)

cow <- cow %>%
  dplyr::select(-c(milex,milper)) %>%
  dplyr::full_join(cow.milex,by=c("iso3c","year")) %>%
  dplyr::full_join(cow.milper,by=c("iso3c","year"))

#### merge population and GDP data ####
cow <- cow %>%
  dplyr::left_join(gdp,by=c("iso3c","year")) %>%
  dplyr::left_join(pd,by=c("iso3c","year")) %>%
  dplyr::filter(year < 2020)

cow$milex[cow$iso3c=="ARE"&cow$year==2014] <- cow$milexpgdp[cow$iso3c=="ARE"&cow$year==2014] * 
  cow$gdp[cow$iso3c=="ARE"&cow$year==2014]
cow$milex[cow$iso3c=="GIN"&cow$year==2014] <- cow$milexpgdp[cow$iso3c=="GIN"&cow$year==2014] * 
  cow$gdp[cow$iso3c=="GIN"&cow$year==2014]
cow$milex[cow$iso3c=="GNB"&cow$year==2014] <- cow$milexpgdp[cow$iso3c=="GNB"&cow$year==2014] * 
  cow$gdp[cow$iso3c=="GNB"&cow$year==2014]
cow$milex[cow$iso3c=="NER"&cow$year==2014] <- cow$milexpgdp[cow$iso3c=="NER"&cow$year==2014] * 
  cow$gdp[cow$iso3c=="NER"&cow$year==2014]
cow$milex[cow$iso3c=="SDN"&cow$year==2014] <- cow$milexpgdp[cow$iso3c=="SDN"&cow$year==2014] * 
  cow$gdp[cow$iso3c=="SDN"&cow$year==2014]

cow <- cow %>%
  dplyr::mutate(milexpt = milex / milper,
                miltppc = milper / pop.pd)

for(i in 1:nrow(cow)){
  if(is.na(cow$milexpgdp[i])){
    cow$milexpgdp[i] = cow$milex[i] / cow$gdp[i]
  }
  if(is.na(cow$milexpc[i])){
    cow$milexpc[i] = cow$milex[i] / cow$pop.pd[i]
  }
  if(is.na(cow$miltppc[i])){
    cow$miltppc[i] = cow$milper[i] / cow$pop.pd[i]
  }
}

#### remove small countries ####
cow <- cow %>%
  dplyr::filter(iso3c %!in% c("BHS","BRB","DMA","GRD","LCA","VCT","ATG","KNA","BLZ","GUY","SUR","LUX",
                              "MCO","LIE","AND","SMR","MLT","MNE","ISL","CPV","STP","ZAN","COM","SYC",
                              "BTN","MDV","BRN","VUT","SLB","KIR","TUV","FJI","TON","NRU","MHL","PLW",
                              "FSM","WSM","DJI")) %>%
  dplyr::select(c(iso3c,year,milexpgdp,milexpc,milexpt,miltppc))

#### gather data ####
cow <- cow %>%
  tidyr::pivot_longer(3:6, names_to = "variable", values_to = "value")

cow.names <- cow %>%
  na.omit() %>%
  dplyr::group_by(iso3c,variable) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(iso3c) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::ungroup()

#### linear interpolation for missing values ####
cow <- cow %>%
  dplyr::filter(iso3c %!in% c("PSE")) %>%
  dplyr::group_by(iso3c,variable) %>%
  dplyr::arrange(year) %>%
  dplyr::mutate(approx = imputeTS::na_interpolation(value), option = "linear") %>%
  dplyr::ungroup() %>%
  dplyr::select(-c(value,option)) %>%
  dplyr::rename(value = approx)

cow$value[cow$value==Inf] <- 0
cow$value[is.nan(cow$value)] <- 0

cow.dup <- cow %>%
  dplyr::group_by(iso3c,year,variable) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::ungroup()

cow <- cow %>%
  unique()

#### spread values ####
cow <- cow %>%
  as.data.frame() %>%
  tidyr::pivot_wider(names_from = variable, values_from = value) %>%
  dplyr::mutate(lnmilexpgdp = log(milexpgdp),
                lntroops = log(miltppc * 1000000),
                lnmilexpc = log(milexpc),
                lnmilexpt = log(milexpt))

cow$lnmilexpgdp[cow$lnmilexpgdp==-Inf] <- 0
cow$lntroops[cow$lntroops==-Inf] <- 0
cow$lnmilexpc[cow$lnmilexpc==-Inf] <- 0
cow$lnmilexpt[cow$lnmilexpt==-Inf] <- 0

cow <- cow %>%
  dplyr::left_join(cyears2,by=c("iso3c","year")) %>%
  dplyr::filter(cn == 1) %>%
  dplyr::select(-cn)

cow <- cow %>%
  dplyr::mutate(countryyear = paste(iso3c,year)) %>%
  as.data.frame()

#cow <- cow %>%
#  filter(year >= 1983)

row.names(cow) <- cow$countryyear


#### PCA ####
cowpca <- prcomp(~  lnmilexpgdp + lntroops + lnmilexpc + lnmilexpt,
                 data = cow, retx = T, center = T, scale. = T)

cowpca[["rotation"]]


cowpca1 <- cowpca$x[,1] %>%
  as.data.frame()
cowpca1 <- cowpca1 %>%
  dplyr::mutate(countryyear = row.names(cowpca1))
names(cowpca1) <- c("pca1","countryyear")

cowpca1 <- cowpca1 %>%
  dplyr::mutate(iso3c = str_sub(countryyear,start=1,end=3),
                year = str_sub(countryyear,start=4,end=8))
cowpca1$year <- as.numeric(cowpca1$year)

cowpca1 <- cowpca1[,c("iso3c","year","pca1")] %>%
  dplyr::rename(mil.cap = pca1)

#cowpca.old <- cowpca1
#names(cowpca.old) <- c("iso3c","year","mc.old")

#cowcomp <- full_join(cowpca1,cowpca.old,by=c("iso3c","year"))
#plot(cowcomp$mc.old,cowcomp$mil.cap)
#cowcomp <- cowcomp %>%
#  mutate(diff = mil.cap - mc.old)

#### yearly avg ####
plot(cowpca1$year,cowpca1$mil.cap)

mc.year <- cowpca1 %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(avg = mean(mil.cap)) %>%
  dplyr::ungroup()

plot(mc.year$year,mc.year$avg,type='l')

#### ts for USSR/YUG/YEM/DEU/CZE/VNM ####
# Soviet successor states
arm.ts <- cowpca1 %>%
  dplyr::filter(iso3c %in% c("ARM","SOV")) %>%
  dplyr::mutate(iso3c = "ARM")

aze.ts <- cowpca1 %>%
  dplyr::filter(iso3c %in% c("AZE","SOV")) %>%
  dplyr::mutate(iso3c = "AZE")

blr.ts <- cowpca1 %>%
  dplyr::filter(iso3c %in% c("BLR","SOV")) %>%
  dplyr::mutate(iso3c = "BLR")

est.ts <- cowpca1 %>%
  dplyr::filter(iso3c %in% c("EST","SOV")) %>%
  dplyr::mutate(iso3c = "EST")

geo.ts <- cowpca1 %>%
  dplyr::filter(iso3c %in% c("GEO","SOV")) %>%
  dplyr::mutate(iso3c = "GEO")

kaz.ts <- cowpca1 %>%
  dplyr::filter(iso3c %in% c("KAZ","SOV")) %>%
  dplyr::mutate(iso3c = "KAZ")

kgz.ts <- cowpca1 %>%
  dplyr::filter(iso3c %in% c("KGZ","SOV")) %>%
  dplyr::mutate(iso3c = "KGZ")

ltu.ts <- cowpca1 %>%
  dplyr::filter(iso3c %in% c("LTU","SOV")) %>%
  dplyr::mutate(iso3c = "LTU")

lva.ts <- cowpca1 %>%
  dplyr::filter(iso3c %in% c("LVA","SOV")) %>%
  dplyr::mutate(iso3c = "LVA")

mda.ts <- cowpca1 %>%
  dplyr::filter(iso3c %in% c("MDA","SOV")) %>%
  dplyr::mutate(iso3c = "MDA")

rus.ts <- cowpca1 %>%
  dplyr::filter(iso3c %in% c("RUS","SOV")) %>%
  dplyr::mutate(iso3c = "RUS")

tjk.ts <- cowpca1 %>%
  dplyr::filter(iso3c %in% c("TJK","SOV")) %>%
  dplyr::mutate(iso3c = "TJK")

tkm.ts <- cowpca1 %>%
  dplyr::filter(iso3c %in% c("TKM","SOV")) %>%
  dplyr::mutate(iso3c = "TKM")

ukr.ts <- cowpca1 %>%
  dplyr::filter(iso3c %in% c("UKR","SOV")) %>%
  dplyr::mutate(iso3c = "UKR")

uzb.ts <- cowpca1 %>%
  dplyr::filter(iso3c %in% c("UZB","SOV")) %>%
  dplyr::mutate(iso3c = "UZB")

# Yugoslav successor states
bih.ts <- cowpca1 %>%
  dplyr::filter(iso3c %in% c("BIH","YUG")) %>%
  dplyr::mutate(iso3c = "BIH")

hrv.ts <- cowpca1 %>%
  dplyr::filter(iso3c %in% c("HRV","YUG")) %>%
  dplyr::mutate(iso3c = "HRV")

mkd.ts <- cowpca1 %>%
  dplyr::filter(iso3c %in% c("MKD","YUG")) %>%
  dplyr::mutate(iso3c = "MKD")

srb.ts <- cowpca1 %>%
  dplyr::filter(iso3c %in% c("SRB","YUG")) %>%
  dplyr::mutate(iso3c = "SRB")

svn.ts <- cowpca1 %>%
  dplyr::filter(iso3c %in% c("SVN","YUG")) %>%
  dplyr::mutate(iso3c = "SVN")

ksv.ts <- cowpca1 %>%
  dplyr::filter(iso3c %in% c("KSV","SRB","YUG"),
                iso3c != "SRB" | year %in% c(1992:2007)) %>%
  dplyr::mutate(iso3c = "KSV")

# YEM
yar.ts <- cowpca1 %>%
  dplyr::filter(iso3c %in% c("YAR","YEM")) %>%
  dplyr::mutate(iso3c = "YAR")

ypr.ts <- cowpca1 %>%
  dplyr::filter(iso3c %in% c("YPR","YEM")) %>%
  dplyr::mutate(iso3c = "YPR")

# DEU
brd.ts <- cowpca1 %>%
  dplyr::filter(iso3c %in% c("BRD","DEU")) %>%
  dplyr::mutate(iso3c = "BRD")

ddr.ts <- cowpca1 %>%
  dplyr::filter(iso3c %in% c("DDR","DEU")) %>%
  dplyr::mutate(iso3c = "DDR")

# CZE
svk.ts <- cowpca1 %>%
  dplyr::filter(iso3c %in% c("SVK","CZE"),
                iso3c != "CZE" | year <= 1992) %>%
  dplyr::mutate(iso3c = "SVK")

# VNM
rvn.ts <- cowpca1 %>%
  dplyr::filter(iso3c %in% c("RVN","VNM"),
                iso3c != "VNM" | year >= 1976) %>%
  dplyr::mutate(iso3c = "RVN")

cowpca1 <- cowpca1 %>%
  dplyr::filter(iso3c %!in% c("ARM","AZE","BLR","EST","GEO","KAZ","KGZ","LTU","LVA","MDA",
                              "RUS","TJK","TKM","UKR","UZB","BIH","HRV","MKD","SRB","SVN",
                              "KSV","YAR","YPR","BRD","DDR","SVK","RVN")) %>%
  rbind(arm.ts,aze.ts,blr.ts,est.ts,geo.ts,kaz.ts,kgz.ts,ltu.ts,lva.ts,mda.ts,rus.ts,tjk.ts,
        tkm.ts,ukr.ts,uzb.ts,bih.ts,hrv.ts,mkd.ts,srb.ts,svn.ts,ksv.ts,yar.ts,ypr.ts,brd.ts,
        ddr.ts,svk.ts,rvn.ts)

#### plot mc country ####
plot.mc <- function(iso = "USA"){
  tmp <- cowpca1 %>%
    dplyr::filter(iso3c == iso) %>%
    dplyr::arrange(year)
  
  plot(tmp$year,tmp$mil.cap,type='l')
}

# #### logistf against conflict by component ####
# miltest <- ucdp4 %>%
#   dplyr::select(confid,iso3c,year,conflict) %>%
#   dplyr::full_join(cow,by=c("iso3c","year"))
# 
# milglm1 <- logistf(conflict ~ lnmilexpgdp, data = miltest, pl = T)
# summary(milglm1)
# 
# milglm2 <- logistf(conflict ~ lntroops, data = miltest, pl = T)
# summary(milglm2)
# 
# milglm3 <- logistf(conflict ~ lnmilexpc, data = miltest, pl = T)
# summary(milglm3)
# 
# milglm4 <- logistf(conflict ~ lnmilexpt, data = miltest, pl = T)
# summary(milglm4)

# writes formatted dataframe as csv files
write.csv(cowpca1,"Data files/Formatted data files/military_capacity.csv")
