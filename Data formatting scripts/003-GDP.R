# This script formats two GDP estimates and creates a composite estimate score.

### load libraries ----------------------------------------------------------------------
library(readxl)
library(utils)
library(countrycode)
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
                iso3c = countrycode::countrycode(country,"country.name","iso3c"))  %>%
  # move iso3c variable first
  dplyr::relocate(iso3c, .before = country)

#### Gleditsch ----------------------------------------------------------------------
# realgdp	- total real GDP, 2005 
gdpgl <- utils::read.delim("Data files/Raw data files/gdpv6.txt") %>%
  # using the countrycode package, add iso3c based on country name
  dplyr::mutate(iso3c = countrycode::countrycode(stateid,"gwc","iso3c")) %>%
  # move iso3c variable first
  dplyr::relocate(iso3c, .before = stateid)

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
gdpgl$iso3c[gdpgl$stateid=="SOT"] <- "SOT" # South Osseita?
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
gdpgl$iso3c[gdpgl$stateid=="DEU"&gdpgl$year<1991] <- "BRD" # recodes Germany before 1991 as West Germany

gdpgl <- gdpgl %>%
  dplyr::select(iso3c,year,realgdp) %>%
  # convert real gdp to 2019$
  dplyr::mutate(realgdp = realgdp * (1 + 0.31) * 1000000) %>%
  dplyr::rename(gdp = realgdp)

### merge data ----------------------------------------------------------------------
gdp <- full_join(pwt,gdpgl,by=c("iso3c","year")) %>%
  dplyr::filter(iso3c %!in% c("ABW","AIA","ATG","BHS","BLZ","BMU","BRB","BRN","BTN","COM","CPV","CUW","DMA",
                              "GRD","HKG","ISL","KNA","LCA","LUX","MAC","MDV","MLT","MSR","STP","SUR","SXM",
                              "SYC","TCA","VCT","VGB","GUY","MCO","LIE","AND","ZAN","TBT","VUT","SLB","KIR",
                              "NRU","TON","TUV","MHL","PLW","FSM","WSM",NA,"ABK","CYM"))

### modify data ----------------------------------------------------------------------
# modifications calculated on Excel workbook unless otherwise denoted

# AFG
# No pwt data, use gl data
gdp$rgdpna[gdp$iso3c=="AFG"] <- gdp$gdp[gdp$iso3c=="AFG"]

# ALB
# 1950-69
# apply proportion of year to 1970 gl gdp, calculate based on 1970 pwt gdp
gdp$rgdpna[gdp$iso3c=="ALB"&gdp$year==1950] <- 9917750625*0.2104304
gdp$rgdpna[gdp$iso3c=="ALB"&gdp$year==1951] <- 9917750625*0.2245623
gdp$rgdpna[gdp$iso3c=="ALB"&gdp$year==1952] <- 9917750625*0.2298448
gdp$rgdpna[gdp$iso3c=="ALB"&gdp$year==1953] <- 9917750625*0.2453233
gdp$rgdpna[gdp$iso3c=="ALB"&gdp$year==1954] <- 9917750625*0.2596703
gdp$rgdpna[gdp$iso3c=="ALB"&gdp$year==1955] <- 9917750625*0.2817816
gdp$rgdpna[gdp$iso3c=="ALB"&gdp$year==1956] <- 9917750625*0.2931079
gdp$rgdpna[gdp$iso3c=="ALB"&gdp$year==1957] <- 9917750625*0.3210018
gdp$rgdpna[gdp$iso3c=="ALB"&gdp$year==1958] <- 9917750625*0.3457449
gdp$rgdpna[gdp$iso3c=="ALB"&gdp$year==1959] <- 9917750625*0.3717933
gdp$rgdpna[gdp$iso3c=="ALB"&gdp$year==1960] <- 9917750625*0.404447
gdp$rgdpna[gdp$iso3c=="ALB"&gdp$year==1961] <- 9917750625*0.4201956
gdp$rgdpna[gdp$iso3c=="ALB"&gdp$year==1962] <- 9917750625*0.4475768
gdp$rgdpna[gdp$iso3c=="ALB"&gdp$year==1963] <- 9917750625*0.4770423
gdp$rgdpna[gdp$iso3c=="ALB"&gdp$year==1964] <- 9917750625*0.5080368
gdp$rgdpna[gdp$iso3c=="ALB"&gdp$year==1965] <- 9917750625*0.5419445
gdp$rgdpna[gdp$iso3c=="ALB"&gdp$year==1966] <- 9917750625*0.5782666
gdp$rgdpna[gdp$iso3c=="ALB"&gdp$year==1967] <- 9917750625*0.6133358
gdp$rgdpna[gdp$iso3c=="ALB"&gdp$year==1968] <- 9917750625*0.6528973
gdp$rgdpna[gdp$iso3c=="ALB"&gdp$year==1969] <- 9917750625*0.6952957

# BGR
# 1950-69
# apply proportion of year to 1970 gl gdp, calculate based on 1970 pwt gdp
gdp$rgdpna[gdp$iso3c=="BGR"&gdp$year==1950] <- 39972737344*0.644406
gdp$rgdpna[gdp$iso3c=="BGR"&gdp$year==1951] <- 39972737344*0.7770812
gdp$rgdpna[gdp$iso3c=="BGR"&gdp$year==1952] <- 39972737344*0.7413073
gdp$rgdpna[gdp$iso3c=="BGR"&gdp$year==1953] <- 39972737344*0.8244637
gdp$rgdpna[gdp$iso3c=="BGR"&gdp$year==1954] <- 39972737344*0.8091314
gdp$rgdpna[gdp$iso3c=="BGR"&gdp$year==1955] <- 39972737344*0.8670661
gdp$rgdpna[gdp$iso3c=="BGR"&gdp$year==1956] <- 39972737344*0.867813
gdp$rgdpna[gdp$iso3c=="BGR"&gdp$year==1957] <- 39972737344*0.9595969
gdp$rgdpna[gdp$iso3c=="BGR"&gdp$year==1958] <- 39972737344*1.0433
gdp$rgdpna[gdp$iso3c=="BGR"&gdp$year==1959] <- 39972737344*1.126628
gdp$rgdpna[gdp$iso3c=="BGR"&gdp$year==1960] <- 39972737344*1.233148
gdp$rgdpna[gdp$iso3c=="BGR"&gdp$year==1961] <- 39972737344*1.313471
gdp$rgdpna[gdp$iso3c=="BGR"&gdp$year==1962] <- 39972737344*1.421233
gdp$rgdpna[gdp$iso3c=="BGR"&gdp$year==1963] <- 39972737344*1.486245
gdp$rgdpna[gdp$iso3c=="BGR"&gdp$year==1964] <- 39972737344*1.603162
gdp$rgdpna[gdp$iso3c=="BGR"&gdp$year==1965] <- 39972737344*1.699582
gdp$rgdpna[gdp$iso3c=="BGR"&gdp$year==1966] <- 39972737344*1.833637
gdp$rgdpna[gdp$iso3c=="BGR"&gdp$year==1967] <- 39972737344*1.93241
gdp$rgdpna[gdp$iso3c=="BGR"&gdp$year==1968] <- 39972737344*1.967989
gdp$rgdpna[gdp$iso3c=="BGR"&gdp$year==1969] <- 39972737344*2.063848

# CHN
# 1950-51
# apply proportion of year to 1952 gl gdp, calculate based on 1952 pwt gdp
gdp$rgdpna[gdp$iso3c=="CHN"&gdp$year==1950] <- 512758100000*1.021341
gdp$rgdpna[gdp$iso3c=="CHN"&gdp$year==1951] <- 512758100000*1.129943

# CUB
# No pwt data, use gl data
# 2012- using ratios from WB data
gdp$rgdpna[gdp$iso3c=="CUB"] <- gdp$gdp[gdp$iso3c=="CUB"]

# CZE
# 1950-89
# apply proportion of year to 1990 gl gdp, calculate based on 1990 pwt gdp
gdp$rgdpna[gdp$iso3c=="CZE"&gdp$year==1950] <- 222166600000*0.5767603
gdp$rgdpna[gdp$iso3c=="CZE"&gdp$year==1951] <- 222166600000*0.5872503
gdp$rgdpna[gdp$iso3c=="CZE"&gdp$year==1952] <- 222166600000*0.6068063
gdp$rgdpna[gdp$iso3c=="CZE"&gdp$year==1953] <- 222166600000*0.6041555
gdp$rgdpna[gdp$iso3c=="CZE"&gdp$year==1954] <- 222166600000*0.6289767
gdp$rgdpna[gdp$iso3c=="CZE"&gdp$year==1955] <- 222166600000*0.6828317
gdp$rgdpna[gdp$iso3c=="CZE"&gdp$year==1956] <- 222166600000*0.7229959
gdp$rgdpna[gdp$iso3c=="CZE"&gdp$year==1957] <- 222166600000*0.7673477
gdp$rgdpna[gdp$iso3c=="CZE"&gdp$year==1958] <- 222166600000*0.8259704
gdp$rgdpna[gdp$iso3c=="CZE"&gdp$year==1959] <- 222166600000*0.8622132
gdp$rgdpna[gdp$iso3c=="CZE"&gdp$year==1960] <- 222166600000*0.3412654
gdp$rgdpna[gdp$iso3c=="CZE"&gdp$year==1961] <- 222166600000*0.3670866
gdp$rgdpna[gdp$iso3c=="CZE"&gdp$year==1962] <- 222166600000*0.3796636
gdp$rgdpna[gdp$iso3c=="CZE"&gdp$year==1963] <- 222166600000*0.3710668
gdp$rgdpna[gdp$iso3c=="CZE"&gdp$year==1964] <- 222166600000*0.3613048
gdp$rgdpna[gdp$iso3c=="CZE"&gdp$year==1965] <- 222166600000*0.3958626
gdp$rgdpna[gdp$iso3c=="CZE"&gdp$year==1966] <- 222166600000*0.469701
gdp$rgdpna[gdp$iso3c=="CZE"&gdp$year==1967] <- 222166600000*0.4712054
gdp$rgdpna[gdp$iso3c=="CZE"&gdp$year==1968] <- 222166600000*0.5037783
gdp$rgdpna[gdp$iso3c=="CZE"&gdp$year==1969] <- 222166600000*0.5384801
gdp$rgdpna[gdp$iso3c=="CZE"&gdp$year==1970] <- 222166600000*0.5632054
gdp$rgdpna[gdp$iso3c=="CZE"&gdp$year==1971] <- 222166600000*0.5898618
gdp$rgdpna[gdp$iso3c=="CZE"&gdp$year==1972] <- 222166600000*0.6204515
gdp$rgdpna[gdp$iso3c=="CZE"&gdp$year==1973] <- 222166600000*0.6597143
gdp$rgdpna[gdp$iso3c=="CZE"&gdp$year==1974] <- 222166600000*0.7098459
gdp$rgdpna[gdp$iso3c=="CZE"&gdp$year==1975] <- 222166600000*0.7509946
gdp$rgdpna[gdp$iso3c=="CZE"&gdp$year==1976] <- 222166600000*0.7794422
gdp$rgdpna[gdp$iso3c=="CZE"&gdp$year==1977] <- 222166600000*0.8069075
gdp$rgdpna[gdp$iso3c=="CZE"&gdp$year==1978] <- 222166600000*0.8492363
gdp$rgdpna[gdp$iso3c=="CZE"&gdp$year==1979] <- 222166600000*0.869459
gdp$rgdpna[gdp$iso3c=="CZE"&gdp$year==1980] <- 222166600000*0.8878418
gdp$rgdpna[gdp$iso3c=="CZE"&gdp$year==1981] <- 222166600000*0.8825097
gdp$rgdpna[gdp$iso3c=="CZE"&gdp$year==1982] <- 222166600000*0.8907772
gdp$rgdpna[gdp$iso3c=="CZE"&gdp$year==1983] <- 222166600000*0.9145281
gdp$rgdpna[gdp$iso3c=="CZE"&gdp$year==1984] <- 222166600000*0.9311186
gdp$rgdpna[gdp$iso3c=="CZE"&gdp$year==1985] <- 222166600000*0.9473635
gdp$rgdpna[gdp$iso3c=="CZE"&gdp$year==1986] <- 222166600000*0.9690602
gdp$rgdpna[gdp$iso3c=="CZE"&gdp$year==1987] <- 222166600000*0.9753781
gdp$rgdpna[gdp$iso3c=="CZE"&gdp$year==1988] <- 222166600000*1.000138
gdp$rgdpna[gdp$iso3c=="CZE"&gdp$year==1989] <- 222166600000*1.021631

# DOM
# 1950
# apply proportion of year to 1951 gl gdp, calculate based on 1951 pwt gdp
gdp$rgdpna[gdp$iso3c=="DOM"&gdp$year==1950] <- 5882531924*0.7140365

# ERI
# No pwt data, use gl data
gdp$rgdpna[gdp$iso3c=="ERI"] <- gdp$gdp[gdp$iso3c=="ERI"]

# GIN
# 1958
# apply proportion of year to 1959 gl gdp, calculate based on 1959 pwt gdp
gdp$rgdpna[gdp$iso3c=="GIN"&gdp$year==1959] <- 5350714131*0.2627502

# GRC
# 1950
# apply proportion of year to 1950 gl gdp, calculate based on 1950 pwt gdp
gdp$rgdpna[gdp$iso3c=="GRC"&gdp$year==1950] <- 38260101094*0.7977532

# HTI
# 1950-59
# apply proportion of year to 1960 gl gdp, calculate based on 1960 pwt gdp
gdp$rgdpna[gdp$iso3c=="HTI"&gdp$year==1950] <- 10961301504*0.778916
gdp$rgdpna[gdp$iso3c=="HTI"&gdp$year==1951] <- 10961301504*0.7912403
gdp$rgdpna[gdp$iso3c=="HTI"&gdp$year==1952] <- 10961301504*0.8354519
gdp$rgdpna[gdp$iso3c=="HTI"&gdp$year==1953] <- 10961301504*0.8083114
gdp$rgdpna[gdp$iso3c=="HTI"&gdp$year==1954] <- 10961301504*0.8731443
gdp$rgdpna[gdp$iso3c=="HTI"&gdp$year==1955] <- 10961301504*0.8363287
gdp$rgdpna[gdp$iso3c=="HTI"&gdp$year==1956] <- 10961301504*0.9074318
gdp$rgdpna[gdp$iso3c=="HTI"&gdp$year==1957] <- 10961301504*0.8511691
gdp$rgdpna[gdp$iso3c=="HTI"&gdp$year==1958] <- 10961301504*0.9149073
gdp$rgdpna[gdp$iso3c=="HTI"&gdp$year==1959] <- 10961301504*0.8683456

# HUN
# 1950-69
# apply proportion of year to 1970 gl gdp, calculate based on 1970 pwt gdp
gdp$rgdpna[gdp$iso3c=="HUN"&gdp$year==1950] <- 106103451563*0.6078862
gdp$rgdpna[gdp$iso3c=="HUN"&gdp$year==1951] <- 106103451563*0.6667386
gdp$rgdpna[gdp$iso3c=="HUN"&gdp$year==1952] <- 106103451563*0.6893299
gdp$rgdpna[gdp$iso3c=="HUN"&gdp$year==1953] <- 106103451563*0.7021187
gdp$rgdpna[gdp$iso3c=="HUN"&gdp$year==1954] <- 106103451563*0.7266986
gdp$rgdpna[gdp$iso3c=="HUN"&gdp$year==1955] <- 106103451563*0.791495
gdp$rgdpna[gdp$iso3c=="HUN"&gdp$year==1956] <- 106103451563*0.7557711
gdp$rgdpna[gdp$iso3c=="HUN"&gdp$year==1957] <- 106103451563*0.8182661
gdp$rgdpna[gdp$iso3c=="HUN"&gdp$year==1958] <- 106103451563*0.8731024
gdp$rgdpna[gdp$iso3c=="HUN"&gdp$year==1959] <- 106103451563*0.9084702
gdp$rgdpna[gdp$iso3c=="HUN"&gdp$year==1960] <- 106103451563*0.9559951
gdp$rgdpna[gdp$iso3c=="HUN"&gdp$year==1961] <- 106103451563*1.004253
gdp$rgdpna[gdp$iso3c=="HUN"&gdp$year==1962] <- 106103451563*1.046211
gdp$rgdpna[gdp$iso3c=="HUN"&gdp$year==1963] <- 106103451563*1.10367
gdp$rgdpna[gdp$iso3c=="HUN"&gdp$year==1964] <- 106103451563*1.165725
gdp$rgdpna[gdp$iso3c=="HUN"&gdp$year==1965] <- 106103451563*1.174925
gdp$rgdpna[gdp$iso3c=="HUN"&gdp$year==1966] <- 106103451563*1.241702
gdp$rgdpna[gdp$iso3c=="HUN"&gdp$year==1967] <- 106103451563*1.312863
gdp$rgdpna[gdp$iso3c=="HUN"&gdp$year==1968] <- 106103451563*1.328902
gdp$rgdpna[gdp$iso3c=="HUN"&gdp$year==1969] <- 106103451563*1.368558

# IDN
# 1950-59
# apply proportion of year to 1960 gl gdp, calculate based on 1960 pwt gdp
gdp$rgdpna[gdp$iso3c=="IDN"&gdp$year==1950] <- 176168000000*0.6034447
gdp$rgdpna[gdp$iso3c=="IDN"&gdp$year==1951] <- 176168000000*0.6473939
gdp$rgdpna[gdp$iso3c=="IDN"&gdp$year==1952] <- 176168000000*0.6857607
gdp$rgdpna[gdp$iso3c=="IDN"&gdp$year==1953] <- 176168000000*0.706772
gdp$rgdpna[gdp$iso3c=="IDN"&gdp$year==1954] <- 176168000000*0.7583901
gdp$rgdpna[gdp$iso3c=="IDN"&gdp$year==1955] <- 176168000000*0.7619492
gdp$rgdpna[gdp$iso3c=="IDN"&gdp$year==1956] <- 176168000000*0.7833078
gdp$rgdpna[gdp$iso3c=="IDN"&gdp$year==1957] <- 176168000000*0.818346
gdp$rgdpna[gdp$iso3c=="IDN"&gdp$year==1958] <- 176168000000*0.8103749
gdp$rgdpna[gdp$iso3c=="IDN"&gdp$year==1959] <- 176168000000*0.8629899

# IRN
# 1950-54
# apply proportion of year to 1955 gl gdp, calculate based on 1955 pwt gdp
gdp$rgdpna[gdp$iso3c=="IRN"&gdp$year==1950] <- 204256400000*0.4427302
gdp$rgdpna[gdp$iso3c=="IRN"&gdp$year==1951] <- 204256400000*0.4412411
gdp$rgdpna[gdp$iso3c=="IRN"&gdp$year==1952] <- 204256400000*0.4396846
gdp$rgdpna[gdp$iso3c=="IRN"&gdp$year==1953] <- 204256400000*0.4388899
gdp$rgdpna[gdp$iso3c=="IRN"&gdp$year==1954] <- 204256400000*0.4375508

# IRQ
# 1950-69
# apply proportion of year to 1970 gl gdp, calculate based on 1970 pwt gdp
gdp$rgdpna[gdp$iso3c=="IRQ"&gdp$year==1950] <- 77700250000*0.2858802
gdp$rgdpna[gdp$iso3c=="IRQ"&gdp$year==1951] <- 77700250000*0.311014
gdp$rgdpna[gdp$iso3c=="IRQ"&gdp$year==1952] <- 77700250000*0.3442234
gdp$rgdpna[gdp$iso3c=="IRQ"&gdp$year==1953] <- 77700250000*0.6965029
gdp$rgdpna[gdp$iso3c=="IRQ"&gdp$year==1954] <- 77700250000*0.8311367
gdp$rgdpna[gdp$iso3c=="IRQ"&gdp$year==1955] <- 77700250000*0.7886484
gdp$rgdpna[gdp$iso3c=="IRQ"&gdp$year==1956] <- 77700250000*0.8210938
gdp$rgdpna[gdp$iso3c=="IRQ"&gdp$year==1957] <- 77700250000*0.8245818
gdp$rgdpna[gdp$iso3c=="IRQ"&gdp$year==1958] <- 77700250000*0.9070875
gdp$rgdpna[gdp$iso3c=="IRQ"&gdp$year==1959] <- 77700250000*0.9260505
gdp$rgdpna[gdp$iso3c=="IRQ"&gdp$year==1960] <- 77700250000*1.109379
gdp$rgdpna[gdp$iso3c=="IRQ"&gdp$year==1961] <- 77700250000*1.215816
gdp$rgdpna[gdp$iso3c=="IRQ"&gdp$year==1962] <- 77700250000*1.276934
gdp$rgdpna[gdp$iso3c=="IRQ"&gdp$year==1963] <- 77700250000*1.251306
gdp$rgdpna[gdp$iso3c=="IRQ"&gdp$year==1964] <- 77700250000*1.470672
gdp$rgdpna[gdp$iso3c=="IRQ"&gdp$year==1965] <- 77700250000*1.663742
gdp$rgdpna[gdp$iso3c=="IRQ"&gdp$year==1966] <- 77700250000*1.732173
gdp$rgdpna[gdp$iso3c=="IRQ"&gdp$year==1967] <- 77700250000*1.574491
gdp$rgdpna[gdp$iso3c=="IRQ"&gdp$year==1968] <- 77700250000*1.868587
gdp$rgdpna[gdp$iso3c=="IRQ"&gdp$year==1969] <- 77700250000*1.920694

# JOR
# 1950-53
# apply proportion of year to 1954 gl gdp, calculate based on 1954 pwt gdp
gdp$rgdpna[gdp$iso3c=="JOR"&gdp$year==1950] <- 4314272666*1.843644
gdp$rgdpna[gdp$iso3c=="JOR"&gdp$year==1951] <- 4314272666*1.942918
gdp$rgdpna[gdp$iso3c=="JOR"&gdp$year==1952] <- 4314272666*2.041876
gdp$rgdpna[gdp$iso3c=="JOR"&gdp$year==1953] <- 4314272666*2.141532

# KHM
# 1953-1969
# apply proportion of year to 1970 gl gdp, calculate based on 1970 pwt gdp
gdp$rgdpna[gdp$iso3c=="KHM"&gdp$year==1953] <- 11489818418*0.3282207
gdp$rgdpna[gdp$iso3c=="KHM"&gdp$year==1954] <- 11489818418*0.3650988
gdp$rgdpna[gdp$iso3c=="KHM"&gdp$year==1955] <- 11489818418*0.3561436
gdp$rgdpna[gdp$iso3c=="KHM"&gdp$year==1956] <- 11489818418*0.4018013
gdp$rgdpna[gdp$iso3c=="KHM"&gdp$year==1957] <- 11489818418*0.4267904
gdp$rgdpna[gdp$iso3c=="KHM"&gdp$year==1958] <- 11489818418*0.4472999
gdp$rgdpna[gdp$iso3c=="KHM"&gdp$year==1959] <- 11489818418*0.4897406
gdp$rgdpna[gdp$iso3c=="KHM"&gdp$year==1960] <- 11489818418*0.7482947
gdp$rgdpna[gdp$iso3c=="KHM"&gdp$year==1961] <- 11489818418*0.7719022
gdp$rgdpna[gdp$iso3c=="KHM"&gdp$year==1962] <- 11489818418*0.7778169
gdp$rgdpna[gdp$iso3c=="KHM"&gdp$year==1963] <- 11489818418*0.8180711
gdp$rgdpna[gdp$iso3c=="KHM"&gdp$year==1964] <- 11489818418*0.8480101
gdp$rgdpna[gdp$iso3c=="KHM"&gdp$year==1965] <- 11489818418*0.8952199
gdp$rgdpna[gdp$iso3c=="KHM"&gdp$year==1966] <- 11489818418*0.9298109
gdp$rgdpna[gdp$iso3c=="KHM"&gdp$year==1967] <- 11489818418*0.8494523
gdp$rgdpna[gdp$iso3c=="KHM"&gdp$year==1968] <- 11489818418*0.8918663
gdp$rgdpna[gdp$iso3c=="KHM"&gdp$year==1969] <- 11489818418*0.9521496

# KOR
# 1950-52
# apply proportion of year to 1953 gl gdp, calculate based on 1953 pwt gdp
gdp$rgdpna[gdp$iso3c=="KOR"&gdp$year==1950] <- 33755490000*1.221218
gdp$rgdpna[gdp$iso3c=="KOR"&gdp$year==1951] <- 33755490000*1.118221
gdp$rgdpna[gdp$iso3c=="KOR"&gdp$year==1952] <- 33755490000*1.190059

# KWT
# 1961-69
# apply proportion of year to 1970 gl gdp, calculate based on 1970 pwt gdp
gdp$rgdpna[gdp$iso3c=="KWT"&gdp$year==1961] <- 152089800000*0.02466352
gdp$rgdpna[gdp$iso3c=="KWT"&gdp$year==1962] <- 152089800000*0.02743534
gdp$rgdpna[gdp$iso3c=="KWT"&gdp$year==1963] <- 152089800000*0.02893185
gdp$rgdpna[gdp$iso3c=="KWT"&gdp$year==1964] <- 152089800000*0.03176783
gdp$rgdpna[gdp$iso3c=="KWT"&gdp$year==1965] <- 152089800000*0.03248645
gdp$rgdpna[gdp$iso3c=="KWT"&gdp$year==1966] <- 152089800000*0.03634545
gdp$rgdpna[gdp$iso3c=="KWT"&gdp$year==1967] <- 152089800000*0.03712179
gdp$rgdpna[gdp$iso3c=="KWT"&gdp$year==1968] <- 152089800000*0.04050572
gdp$rgdpna[gdp$iso3c=="KWT"&gdp$year==1969] <- 152089800000*0.0417326

# LAO
# 1954-69
# apply proportion of year to 1970 gl gdp, calculate based on 1970 pwt gdp
gdp$rgdpna[gdp$iso3c=="LAO"&gdp$year==1954] <- 3489446206*0.9100848
gdp$rgdpna[gdp$iso3c=="LAO"&gdp$year==1955] <- 3489446206*0.9403258
gdp$rgdpna[gdp$iso3c=="LAO"&gdp$year==1956] <- 3489446206*1.021758
gdp$rgdpna[gdp$iso3c=="LAO"&gdp$year==1957] <- 3489446206*1.002047
gdp$rgdpna[gdp$iso3c=="LAO"&gdp$year==1958] <- 3489446206*1.103611
gdp$rgdpna[gdp$iso3c=="LAO"&gdp$year==1959] <- 3489446206*1.144634
gdp$rgdpna[gdp$iso3c=="LAO"&gdp$year==1960] <- 3489446206*1.182674
gdp$rgdpna[gdp$iso3c=="LAO"&gdp$year==1961] <- 3489446206*1.223498
gdp$rgdpna[gdp$iso3c=="LAO"&gdp$year==1962] <- 3489446206*1.263595
gdp$rgdpna[gdp$iso3c=="LAO"&gdp$year==1963] <- 3489446206*1.305248
gdp$rgdpna[gdp$iso3c=="LAO"&gdp$year==1964] <- 3489446206*1.349864
gdp$rgdpna[gdp$iso3c=="LAO"&gdp$year==1965] <- 3489446206*1.396165
gdp$rgdpna[gdp$iso3c=="LAO"&gdp$year==1966] <- 3489446206*1.443652
gdp$rgdpna[gdp$iso3c=="LAO"&gdp$year==1967] <- 3489446206*1.492883
gdp$rgdpna[gdp$iso3c=="LAO"&gdp$year==1968] <- 3489446206*1.543331
gdp$rgdpna[gdp$iso3c=="LAO"&gdp$year==1969] <- 3489446206*1.595573

# LBN
# 1950-69
# apply proportion of year to 1970 gl gdp, calculate based on 1970 pwt gdp
gdp$rgdpna[gdp$iso3c=="LBN"&gdp$year==1950] <- 40751477578*0.3085933
gdp$rgdpna[gdp$iso3c=="LBN"&gdp$year==1951] <- 40751477578*0.274505
gdp$rgdpna[gdp$iso3c=="LBN"&gdp$year==1952] <- 40751477578*0.2898091
gdp$rgdpna[gdp$iso3c=="LBN"&gdp$year==1953] <- 40751477578*0.3320524
gdp$rgdpna[gdp$iso3c=="LBN"&gdp$year==1954] <- 40751477578*0.3801578
gdp$rgdpna[gdp$iso3c=="LBN"&gdp$year==1955] <- 40751477578*0.4098486
gdp$rgdpna[gdp$iso3c=="LBN"&gdp$year==1956] <- 40751477578*0.4003124
gdp$rgdpna[gdp$iso3c=="LBN"&gdp$year==1957] <- 40751477578*0.4073775
gdp$rgdpna[gdp$iso3c=="LBN"&gdp$year==1958] <- 40751477578*0.3499949
gdp$rgdpna[gdp$iso3c=="LBN"&gdp$year==1959] <- 40751477578*0.3803951
gdp$rgdpna[gdp$iso3c=="LBN"&gdp$year==1960] <- 40751477578*0.3912438
gdp$rgdpna[gdp$iso3c=="LBN"&gdp$year==1961] <- 40751477578*0.4180322
gdp$rgdpna[gdp$iso3c=="LBN"&gdp$year==1962] <- 40751477578*0.4348238
gdp$rgdpna[gdp$iso3c=="LBN"&gdp$year==1963] <- 40751477578*0.4392715
gdp$rgdpna[gdp$iso3c=="LBN"&gdp$year==1964] <- 40751477578*0.4662788
gdp$rgdpna[gdp$iso3c=="LBN"&gdp$year==1965] <- 40751477578*0.5124609
gdp$rgdpna[gdp$iso3c=="LBN"&gdp$year==1966] <- 40751477578*0.5463265
gdp$rgdpna[gdp$iso3c=="LBN"&gdp$year==1967] <- 40751477578*0.5191697
gdp$rgdpna[gdp$iso3c=="LBN"&gdp$year==1968] <- 40751477578*0.582494
gdp$rgdpna[gdp$iso3c=="LBN"&gdp$year==1969] <- 40751477578*0.5942537

# LBR
# 1950-63
# apply proportion of year to 1964 gl gdp, calculate based on 1964 pwt gdp
gdp$rgdpna[gdp$iso3c=="LBR"&gdp$year==1950] <- 1871580249*1.113692
gdp$rgdpna[gdp$iso3c=="LBR"&gdp$year==1951] <- 1871580249*1.171589
gdp$rgdpna[gdp$iso3c=="LBR"&gdp$year==1952] <- 1871580249*1.202999
gdp$rgdpna[gdp$iso3c=="LBR"&gdp$year==1953] <- 1871580249*1.246214
gdp$rgdpna[gdp$iso3c=="LBR"&gdp$year==1954] <- 1871580249*1.312201
gdp$rgdpna[gdp$iso3c=="LBR"&gdp$year==1955] <- 1871580249*1.353601
gdp$rgdpna[gdp$iso3c=="LBR"&gdp$year==1956] <- 1871580249*1.404462
gdp$rgdpna[gdp$iso3c=="LBR"&gdp$year==1957] <- 1871580249*1.454925
gdp$rgdpna[gdp$iso3c=="LBR"&gdp$year==1958] <- 1871580249*1.497862
gdp$rgdpna[gdp$iso3c=="LBR"&gdp$year==1959] <- 1871580249*1.585595
gdp$rgdpna[gdp$iso3c=="LBR"&gdp$year==1960] <- 1871580249*1.130902
gdp$rgdpna[gdp$iso3c=="LBR"&gdp$year==1961] <- 1871580249*1.198306
gdp$rgdpna[gdp$iso3c=="LBR"&gdp$year==1962] <- 1871580249*1.22057
gdp$rgdpna[gdp$iso3c=="LBR"&gdp$year==1963] <- 1871580249*1.374912

# LBY
# No pwt data, use gl data
gdp$rgdpna[gdp$iso3c=="LBY"] <- gdp$gdp[gdp$iso3c=="LBY"]

# MMR
# 1950-61
# apply proportion of year to 1962 gl gdp, calculate based on 1962 pwt gdp
gdp$rgdpna[gdp$iso3c=="MMR"&gdp$year==1950] <- 15433540430*0.4941486
gdp$rgdpna[gdp$iso3c=="MMR"&gdp$year==1951] <- 15433540430*0.5170026
gdp$rgdpna[gdp$iso3c=="MMR"&gdp$year==1952] <- 15433540430*0.5718837
gdp$rgdpna[gdp$iso3c=="MMR"&gdp$year==1953] <- 15433540430*0.5894567
gdp$rgdpna[gdp$iso3c=="MMR"&gdp$year==1954] <- 15433540430*0.5885971
gdp$rgdpna[gdp$iso3c=="MMR"&gdp$year==1955] <- 15433540430*0.6063387
gdp$rgdpna[gdp$iso3c=="MMR"&gdp$year==1956] <- 15433540430*0.5849287
gdp$rgdpna[gdp$iso3c=="MMR"&gdp$year==1957] <- 15433540430*0.668622
gdp$rgdpna[gdp$iso3c=="MMR"&gdp$year==1958] <- 15433540430*0.6566431
gdp$rgdpna[gdp$iso3c=="MMR"&gdp$year==1959] <- 15433540430*0.7570025
gdp$rgdpna[gdp$iso3c=="MMR"&gdp$year==1960] <- 15433540430*0.8115624
gdp$rgdpna[gdp$iso3c=="MMR"&gdp$year==1961] <- 15433540430*0.8213154

# MNG
# 1950-69
# apply proportion of year to 1970 gl gdp, calculate based on 1970 pwt gdp
gdp$rgdpna[gdp$iso3c=="MNG"&gdp$year==1950] <- 4659460547*0.3719799
gdp$rgdpna[gdp$iso3c=="MNG"&gdp$year==1951] <- 4659460547*0.3887352
gdp$rgdpna[gdp$iso3c=="MNG"&gdp$year==1952] <- 4659460547*0.4089996
gdp$rgdpna[gdp$iso3c=="MNG"&gdp$year==1953] <- 4659460547*0.4288284
gdp$rgdpna[gdp$iso3c=="MNG"&gdp$year==1954] <- 4659460547*0.4485524
gdp$rgdpna[gdp$iso3c=="MNG"&gdp$year==1955] <- 4659460547*0.4713095
gdp$rgdpna[gdp$iso3c=="MNG"&gdp$year==1956] <- 4659460547*0.4957527
gdp$rgdpna[gdp$iso3c=="MNG"&gdp$year==1957] <- 4659460547*0.522374
gdp$rgdpna[gdp$iso3c=="MNG"&gdp$year==1958] <- 4659460547*0.5527626
gdp$rgdpna[gdp$iso3c=="MNG"&gdp$year==1959] <- 4659460547*0.5862973
gdp$rgdpna[gdp$iso3c=="MNG"&gdp$year==1960] <- 4659460547*0.6227846
gdp$rgdpna[gdp$iso3c=="MNG"&gdp$year==1961] <- 4659460547*0.6617243
gdp$rgdpna[gdp$iso3c=="MNG"&gdp$year==1962] <- 4659460547*0.6988085
gdp$rgdpna[gdp$iso3c=="MNG"&gdp$year==1963] <- 4659460547*0.7366188
gdp$rgdpna[gdp$iso3c=="MNG"&gdp$year==1964] <- 4659460547*0.78056
gdp$rgdpna[gdp$iso3c=="MNG"&gdp$year==1965] <- 4659460547*0.8262276
gdp$rgdpna[gdp$iso3c=="MNG"&gdp$year==1966] <- 4659460547*0.873194
gdp$rgdpna[gdp$iso3c=="MNG"&gdp$year==1967] <- 4659460547*0.9235405
gdp$rgdpna[gdp$iso3c=="MNG"&gdp$year==1968] <- 4659460547*0.9782674
gdp$rgdpna[gdp$iso3c=="MNG"&gdp$year==1969] <- 4659460547*1.035422

# NPL
# 1950-59
# apply proportion of year to 1960 gl gdp, calculate based on 1960 pwt gdp
gdp$rgdpna[gdp$iso3c=="NPL"&gdp$year==1950] <- 10863631113*0.767245
gdp$rgdpna[gdp$iso3c=="NPL"&gdp$year==1951] <- 10863631113*0.7915004
gdp$rgdpna[gdp$iso3c=="NPL"&gdp$year==1952] <- 10863631113*0.8220059
gdp$rgdpna[gdp$iso3c=="NPL"&gdp$year==1953] <- 10863631113*0.8766985
gdp$rgdpna[gdp$iso3c=="NPL"&gdp$year==1954] <- 10863631113*0.9006439
gdp$rgdpna[gdp$iso3c=="NPL"&gdp$year==1955] <- 10863631113*0.9239979
gdp$rgdpna[gdp$iso3c=="NPL"&gdp$year==1956] <- 10863631113*0.9704514
gdp$rgdpna[gdp$iso3c=="NPL"&gdp$year==1957] <- 10863631113*0.9769759
gdp$rgdpna[gdp$iso3c=="NPL"&gdp$year==1958] <- 10863631113*1.040141
gdp$rgdpna[gdp$iso3c=="NPL"&gdp$year==1959] <- 10863631113*1.075229

# OMN
# 1950-69
# apply proportion of year to 1970 gl gdp, calculate based on 1970 pwt gdp
gdp$rgdpna[gdp$iso3c=="OMN"&gdp$year==1950] <- 12137187012*0.01692675
gdp$rgdpna[gdp$iso3c=="OMN"&gdp$year==1951] <- 12137187012*0.01800256
gdp$rgdpna[gdp$iso3c=="OMN"&gdp$year==1952] <- 12137187012*0.01910689
gdp$rgdpna[gdp$iso3c=="OMN"&gdp$year==1953] <- 12137187012*0.02032532
gdp$rgdpna[gdp$iso3c=="OMN"&gdp$year==1954] <- 12137187012*0.02159509
gdp$rgdpna[gdp$iso3c=="OMN"&gdp$year==1955] <- 12137187012*0.02292856
gdp$rgdpna[gdp$iso3c=="OMN"&gdp$year==1956] <- 12137187012*0.02438943
gdp$rgdpna[gdp$iso3c=="OMN"&gdp$year==1957] <- 12137187012*0.02591306
gdp$rgdpna[gdp$iso3c=="OMN"&gdp$year==1958] <- 12137187012*0.0275175
gdp$rgdpna[gdp$iso3c=="OMN"&gdp$year==1959] <- 12137187012*0.02924887
gdp$rgdpna[gdp$iso3c=="OMN"&gdp$year==1960] <- 12137187012*0.03106296
gdp$rgdpna[gdp$iso3c=="OMN"&gdp$year==1961] <- 12137187012*0.03139288
gdp$rgdpna[gdp$iso3c=="OMN"&gdp$year==1962] <- 12137187012*0.03772461
gdp$rgdpna[gdp$iso3c=="OMN"&gdp$year==1963] <- 12137187012*0.03940179
gdp$rgdpna[gdp$iso3c=="OMN"&gdp$year==1964] <- 12137187012*0.0393918
gdp$rgdpna[gdp$iso3c=="OMN"&gdp$year==1965] <- 12137187012*0.03955534
gdp$rgdpna[gdp$iso3c=="OMN"&gdp$year==1966] <- 12137187012*0.04163517
gdp$rgdpna[gdp$iso3c=="OMN"&gdp$year==1967] <- 12137187012*0.08210659
gdp$rgdpna[gdp$iso3c=="OMN"&gdp$year==1968] <- 12137187012*0.2458258
gdp$rgdpna[gdp$iso3c=="OMN"&gdp$year==1969] <- 12137187012*0.3122574

# PNG
# No pwt data, use gl data
gdp$rgdpna[gdp$iso3c=="PNG"] <- gdp$gdp[gdp$iso3c=="PNG"]

# POL
# 1950-69
# apply proportion of year to 1970 gl gdp, calculate based on 1970 pwt gdp
gdp$rgdpna[gdp$iso3c=="POL"&gdp$year==1950] <- 287641700000*0.546325
gdp$rgdpna[gdp$iso3c=="POL"&gdp$year==1951] <- 287641700000*0.5704815
gdp$rgdpna[gdp$iso3c=="POL"&gdp$year==1952] <- 287641700000*0.5839102
gdp$rgdpna[gdp$iso3c=="POL"&gdp$year==1953] <- 287641700000*0.6181972
gdp$rgdpna[gdp$iso3c=="POL"&gdp$year==1954] <- 287641700000*0.6534578
gdp$rgdpna[gdp$iso3c=="POL"&gdp$year==1955] <- 287641700000*0.6855388
gdp$rgdpna[gdp$iso3c=="POL"&gdp$year==1956] <- 287641700000*0.7164691
gdp$rgdpna[gdp$iso3c=="POL"&gdp$year==1957] <- 287641700000*0.7541718
gdp$rgdpna[gdp$iso3c=="POL"&gdp$year==1958] <- 287641700000*0.7910077
gdp$rgdpna[gdp$iso3c=="POL"&gdp$year==1959] <- 287641700000*0.8141861
gdp$rgdpna[gdp$iso3c=="POL"&gdp$year==1960] <- 287641700000*0.8547625
gdp$rgdpna[gdp$iso3c=="POL"&gdp$year==1961] <- 287641700000*0.9233089
gdp$rgdpna[gdp$iso3c=="POL"&gdp$year==1962] <- 287641700000*0.9111888
gdp$rgdpna[gdp$iso3c=="POL"&gdp$year==1963] <- 287641700000*0.9666574
gdp$rgdpna[gdp$iso3c=="POL"&gdp$year==1964] <- 287641700000*1.015092
gdp$rgdpna[gdp$iso3c=="POL"&gdp$year==1965] <- 287641700000*1.072744
gdp$rgdpna[gdp$iso3c=="POL"&gdp$year==1966] <- 287641700000*1.137782
gdp$rgdpna[gdp$iso3c=="POL"&gdp$year==1967] <- 287641700000*1.178789
gdp$rgdpna[gdp$iso3c=="POL"&gdp$year==1968] <- 287641700000*1.254288
gdp$rgdpna[gdp$iso3c=="POL"&gdp$year==1969] <- 287641700000*1.241742

# PRK
# No pwt data, use gl data
gdp$rgdpna[gdp$iso3c=="PRK"] <- gdp$gdp[gdp$iso3c=="PRK"]

# PRY
# 1950
# apply proportion of year to 1951 gl gdp, calculate based on 1951 pwt gdp
gdp$rgdpna[gdp$iso3c=="PRY"&gdp$year==1950] <- 4348922988*1.147959

# ROU
# 1950-59
# apply proportion of year to 1960 gl gdp, calculate based on 1960 pwt gdp
# gl proportion is 1, looks interpolated out
gdp$rgdpna[gdp$iso3c=="ROU"&gdp$year==1950] <- 48098296641
gdp$rgdpna[gdp$iso3c=="ROU"&gdp$year==1951] <- 48098296641
gdp$rgdpna[gdp$iso3c=="ROU"&gdp$year==1952] <- 48098296641
gdp$rgdpna[gdp$iso3c=="ROU"&gdp$year==1953] <- 48098296641
gdp$rgdpna[gdp$iso3c=="ROU"&gdp$year==1954] <- 48098296641
gdp$rgdpna[gdp$iso3c=="ROU"&gdp$year==1955] <- 48098296641
gdp$rgdpna[gdp$iso3c=="ROU"&gdp$year==1956] <- 48098296641
gdp$rgdpna[gdp$iso3c=="ROU"&gdp$year==1957] <- 48098296641
gdp$rgdpna[gdp$iso3c=="ROU"&gdp$year==1958] <- 48098296641
gdp$rgdpna[gdp$iso3c=="ROU"&gdp$year==1959] <- 48098296641

# RUS
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

# SAU
# 1950-69
# apply proportion of year to 1970 gl gdp, calculate based on 1970 pwt gdp
gdp$rgdpna[gdp$iso3c=="SAU"&gdp$year==1950] <- 365519100000*0.01983352
gdp$rgdpna[gdp$iso3c=="SAU"&gdp$year==1951] <- 365519100000*0.02156631
gdp$rgdpna[gdp$iso3c=="SAU"&gdp$year==1952] <- 365519100000*0.02295976
gdp$rgdpna[gdp$iso3c=="SAU"&gdp$year==1953] <- 365519100000*0.02535497
gdp$rgdpna[gdp$iso3c=="SAU"&gdp$year==1954] <- 365519100000*0.0283785
gdp$rgdpna[gdp$iso3c=="SAU"&gdp$year==1955] <- 365519100000*0.02915761
gdp$rgdpna[gdp$iso3c=="SAU"&gdp$year==1956] <- 365519100000*0.0314188
gdp$rgdpna[gdp$iso3c=="SAU"&gdp$year==1957] <- 365519100000*0.03263065
gdp$rgdpna[gdp$iso3c=="SAU"&gdp$year==1958] <- 365519100000*0.03435636
gdp$rgdpna[gdp$iso3c=="SAU"&gdp$year==1959] <- 365519100000*0.03805956
gdp$rgdpna[gdp$iso3c=="SAU"&gdp$year==1960] <- 365519100000*0.05154108
gdp$rgdpna[gdp$iso3c=="SAU"&gdp$year==1961] <- 365519100000*0.05832101
gdp$rgdpna[gdp$iso3c=="SAU"&gdp$year==1962] <- 365519100000*0.06139393
gdp$rgdpna[gdp$iso3c=="SAU"&gdp$year==1963] <- 365519100000*0.07077048
gdp$rgdpna[gdp$iso3c=="SAU"&gdp$year==1964] <- 365519100000*0.0815638
gdp$rgdpna[gdp$iso3c=="SAU"&gdp$year==1965] <- 365519100000*0.09350898
gdp$rgdpna[gdp$iso3c=="SAU"&gdp$year==1966] <- 365519100000*0.1045689
gdp$rgdpna[gdp$iso3c=="SAU"&gdp$year==1967] <- 365519100000*0.1109867
gdp$rgdpna[gdp$iso3c=="SAU"&gdp$year==1968] <- 365519100000*0.1204553
gdp$rgdpna[gdp$iso3c=="SAU"&gdp$year==1969] <- 365519100000*0.1290332

# SDN
# 1956-69
# apply proportion of year to 1970 gl gdp, calculate based on 1970 pwt gdp
gdp$rgdpna[gdp$iso3c=="SDN"&gdp$year==1956] <- 27497740000*1.109839
gdp$rgdpna[gdp$iso3c=="SDN"&gdp$year==1957] <- 27497740000*1.084723
gdp$rgdpna[gdp$iso3c=="SDN"&gdp$year==1958] <- 27497740000*1.120858
gdp$rgdpna[gdp$iso3c=="SDN"&gdp$year==1959] <- 27497740000*1.243459
gdp$rgdpna[gdp$iso3c=="SDN"&gdp$year==1960] <- 27497740000*1.256598
gdp$rgdpna[gdp$iso3c=="SDN"&gdp$year==1961] <- 27497740000*1.246431
gdp$rgdpna[gdp$iso3c=="SDN"&gdp$year==1962] <- 27497740000*1.323955
gdp$rgdpna[gdp$iso3c=="SDN"&gdp$year==1963] <- 27497740000*1.276925
gdp$rgdpna[gdp$iso3c=="SDN"&gdp$year==1964] <- 27497740000*1.255645
gdp$rgdpna[gdp$iso3c=="SDN"&gdp$year==1965] <- 27497740000*1.336645
gdp$rgdpna[gdp$iso3c=="SDN"&gdp$year==1966] <- 27497740000*1.314072
gdp$rgdpna[gdp$iso3c=="SDN"&gdp$year==1967] <- 27497740000*1.266328
gdp$rgdpna[gdp$iso3c=="SDN"&gdp$year==1968] <- 27497740000*1.338594
gdp$rgdpna[gdp$iso3c=="SDN"&gdp$year==1969] <- 27497740000*1.416896

# SOM
# No pwt data, use gl data
gdp$rgdpna[gdp$iso3c=="SOM"] <- gdp$gdp[gdp$iso3c=="SOM"]

# SSD
# No pwt data, use gl data
gdp$rgdpna[gdp$iso3c=="SSD"] <- gdp$gdp[gdp$iso3c=="SSD"]

# SWZ
# 1968-69
# apply proportion of year to 1970 gl gdp, calculate based on 1970 pwt gdp
gdp$rgdpna[gdp$iso3c=="SWZ"&gdp$year==1968] <- 1181690991*2.002123
gdp$rgdpna[gdp$iso3c=="SWZ"&gdp$year==1969] <- 1181690991*2.436974

# SYR
# 1950-59
# apply proportion of year to 1960 gl gdp, calculate based on 1960 pwt gdp
gdp$rgdpna[gdp$iso3c=="SYR"&gdp$year==1950] <- 10113925254*0.8900549
gdp$rgdpna[gdp$iso3c=="SYR"&gdp$year==1951] <- 10113925254*0.8568255
gdp$rgdpna[gdp$iso3c=="SYR"&gdp$year==1952] <- 10113925254*1.080886
gdp$rgdpna[gdp$iso3c=="SYR"&gdp$year==1953] <- 10113925254*1.227474
gdp$rgdpna[gdp$iso3c=="SYR"&gdp$year==1954] <- 10113925254*1.410478
gdp$rgdpna[gdp$iso3c=="SYR"&gdp$year==1955] <- 10113925254*1.274459
gdp$rgdpna[gdp$iso3c=="SYR"&gdp$year==1956] <- 10113925254*1.511193
gdp$rgdpna[gdp$iso3c=="SYR"&gdp$year==1957] <- 10113925254*1.605017
gdp$rgdpna[gdp$iso3c=="SYR"&gdp$year==1958] <- 10113925254*1.383689
gdp$rgdpna[gdp$iso3c=="SYR"&gdp$year==1959] <- 10113925254*1.434623

# TLS
# No pwt data, use gl data
gdp$rgdpna[gdp$iso3c=="TLS"] <- gdp$gdp[gdp$iso3c=="TLS"]

# TUN
# 1956-59
# apply proportion of year to 1960 gl gdp, calculate based on 1960 pwt gdp
gdp$rgdpna[gdp$iso3c=="TUN"&gdp$year==1956] <- 9738018047*0.9256697
gdp$rgdpna[gdp$iso3c=="TUN"&gdp$year==1957] <- 9738018047*0.8928575
gdp$rgdpna[gdp$iso3c=="TUN"&gdp$year==1958] <- 9738018047*1.012701
gdp$rgdpna[gdp$iso3c=="TUN"&gdp$year==1959] <- 9738018047*0.9717677

# TWN
# 1950
# apply proportion of year to 1951 gl gdp, calculate based on 1951 pwt gdp
gdp$rgdpna[gdp$iso3c=="TWN"&gdp$year==1950] <- 12399070000*0.8890934

# VNM and RVN
# See excel spreadsheet for calculations
gdp$rgdpna[gdp$iso3c=="VNM"&gdp$year==1954] <- 45768960469*0.847859729*0.582806934
gdp$rgdpna[gdp$iso3c=="VNM"&gdp$year==1955] <- 45768960469*0.863093225*0.572520459
gdp$rgdpna[gdp$iso3c=="VNM"&gdp$year==1956] <- 45768960469*0.877545627*0.563091552
gdp$rgdpna[gdp$iso3c=="VNM"&gdp$year==1957] <- 45768960469*0.892581918*0.553605803
gdp$rgdpna[gdp$iso3c=="VNM"&gdp$year==1958] <- 45768960469*0.908294833*0.544028779
gdp$rgdpna[gdp$iso3c=="VNM"&gdp$year==1959] <- 45768960469*0.92390843*0.534834961
gdp$rgdpna[gdp$iso3c=="VNM"&gdp$year==1960] <- 45768960469*0.940365804*0.525474796
gdp$rgdpna[gdp$iso3c=="VNM"&gdp$year==1961] <- 45768960469*0.960169378*0.514636834
gdp$rgdpna[gdp$iso3c=="VNM"&gdp$year==1962] <- 45768960469*1.017489834*0.485644684
gdp$rgdpna[gdp$iso3c=="VNM"&gdp$year==1963] <- 45768960469*1.029340494*0.480053521
gdp$rgdpna[gdp$iso3c=="VNM"&gdp$year==1964] <- 45768960469*1.051408643*0.469977618
gdp$rgdpna[gdp$iso3c=="VNM"&gdp$year==1965] <- 45768960469*1.054095678*0.46877958
gdp$rgdpna[gdp$iso3c=="VNM"&gdp$year==1966] <- 45768960469*1.056892907*0.467538883
gdp$rgdpna[gdp$iso3c=="VNM"&gdp$year==1967] <- 45768960469*0.985486984*0.501415582
gdp$rgdpna[gdp$iso3c=="VNM"&gdp$year==1968] <- 45768960469*0.976159927*0.50620653
gdp$rgdpna[gdp$iso3c=="VNM"&gdp$year==1969] <- 45768960469*1.017207907*0.485779284
gdp$rgdpna[gdp$iso3c=="VNM"&gdp$year==1970] <- 45768960469*0.480787873
gdp$rgdpna[gdp$iso3c=="VNM"&gdp$year==1971] <- 47510435156*0.470670747
gdp$rgdpna[gdp$iso3c=="VNM"&gdp$year==1972] <- 48442033359*0.48532456
gdp$rgdpna[gdp$iso3c=="VNM"&gdp$year==1973] <- 47753593594*0.462854851
gdp$rgdpna[gdp$iso3c=="VNM"&gdp$year==1974] <- 48938752734*0.488760395
gdp$rgdpna[gdp$iso3c=="VNM"&gdp$year==1975] <- 50400250547*0.487296203

gdp$rgdpna[gdp$iso3c=="RVN"&gdp$year==1954] <- 45768960469*0.847859729*0.417193066
gdp$rgdpna[gdp$iso3c=="RVN"&gdp$year==1955] <- 45768960469*0.863093225*0.427479541
gdp$rgdpna[gdp$iso3c=="RVN"&gdp$year==1956] <- 45768960469*0.877545627*0.436908448
gdp$rgdpna[gdp$iso3c=="RVN"&gdp$year==1957] <- 45768960469*0.892581918*0.446394197
gdp$rgdpna[gdp$iso3c=="RVN"&gdp$year==1958] <- 45768960469*0.908294833*0.455971221
gdp$rgdpna[gdp$iso3c=="RVN"&gdp$year==1959] <- 45768960469*0.92390843*0.465165039
gdp$rgdpna[gdp$iso3c=="RVN"&gdp$year==1960] <- 45768960469*0.940365804*0.474525204
gdp$rgdpna[gdp$iso3c=="RVN"&gdp$year==1961] <- 45768960469*0.960169378*0.485363166
gdp$rgdpna[gdp$iso3c=="RVN"&gdp$year==1962] <- 45768960469*1.017489834*0.514355316
gdp$rgdpna[gdp$iso3c=="RVN"&gdp$year==1963] <- 45768960469*1.029340494*0.519946479
gdp$rgdpna[gdp$iso3c=="RVN"&gdp$year==1964] <- 45768960469*1.051408643*0.530022382
gdp$rgdpna[gdp$iso3c=="RVN"&gdp$year==1965] <- 45768960469*1.054095678*0.53122042
gdp$rgdpna[gdp$iso3c=="RVN"&gdp$year==1966] <- 45768960469*1.056892907*0.532461117
gdp$rgdpna[gdp$iso3c=="RVN"&gdp$year==1967] <- 45768960469*0.985486984*0.498584418
gdp$rgdpna[gdp$iso3c=="RVN"&gdp$year==1968] <- 45768960469*0.976159927*0.49379347
gdp$rgdpna[gdp$iso3c=="RVN"&gdp$year==1969] <- 45768960469*1.017207907*0.514220716
gdp$rgdpna[gdp$iso3c=="RVN"&gdp$year==1970] <- 45768960469*0.519212127
gdp$rgdpna[gdp$iso3c=="RVN"&gdp$year==1971] <- 47510435156*0.529329253
gdp$rgdpna[gdp$iso3c=="RVN"&gdp$year==1972] <- 48442033359*0.51467544
gdp$rgdpna[gdp$iso3c=="RVN"&gdp$year==1973] <- 47753593594*0.537145149
gdp$rgdpna[gdp$iso3c=="RVN"&gdp$year==1974] <- 48938752734*0.511239605
gdp$rgdpna[gdp$iso3c=="RVN"&gdp$year==1975] <- 50400250547*0.512703797


# BRD and DDR
gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1950] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1950]*0.65
gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1951] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1951]*0.67
gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1952] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1952]*0.69
gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1953] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1953]*0.71
gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1954] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1954]*0.72
gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1955] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1955]*0.75
gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1956] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1956]*0.76
gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1957] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1957]*0.77
gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1958] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1958]*0.78
gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1959] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1959]*0.79
gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1960] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1960]*0.81
gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1961] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1961]*0.81
gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1962] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1962]*0.82
gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1963] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1963]*0.82
gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1964] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1964]*0.83
gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1965] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1965]*0.84
gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1966] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1966]*0.84
gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1967] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1967]*0.84
gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1968] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1968]*0.85
gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1969] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1969]*0.86
gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1970] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1970]*0.86
gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1971] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1971]*0.86
gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1972] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1972]*0.86
gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1973] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1973]*0.86
gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1974] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1974]*0.86
gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1975] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1975]*0.85
gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1976] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1976]*0.86
gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1977] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1977]*0.85
gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1978] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1978]*0.85
gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1979] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1979]*0.85
gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1980] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1980]*0.85
gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1981] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1981]*0.84
gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1982] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1982]*0.83
gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1983] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1983]*0.83
gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1984] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1984]*0.82
gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1985] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1985]*0.82
gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1986] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1986]*0.82
gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1987] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1987]*0.82
gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1988] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1988]*0.82
gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1989] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1989]*0.83
gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1990] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1990]*0.83

gdp$rgdpna[gdp$iso3c=="DDR"&gdp$year==1950] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1950]*0.35
gdp$rgdpna[gdp$iso3c=="DDR"&gdp$year==1951] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1951]*0.33
gdp$rgdpna[gdp$iso3c=="DDR"&gdp$year==1952] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1952]*0.31
gdp$rgdpna[gdp$iso3c=="DDR"&gdp$year==1953] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1953]*0.29
gdp$rgdpna[gdp$iso3c=="DDR"&gdp$year==1954] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1954]*0.28
gdp$rgdpna[gdp$iso3c=="DDR"&gdp$year==1955] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1955]*0.25
gdp$rgdpna[gdp$iso3c=="DDR"&gdp$year==1956] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1956]*0.24
gdp$rgdpna[gdp$iso3c=="DDR"&gdp$year==1957] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1957]*0.23
gdp$rgdpna[gdp$iso3c=="DDR"&gdp$year==1958] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1958]*0.22
gdp$rgdpna[gdp$iso3c=="DDR"&gdp$year==1959] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1959]*0.21
gdp$rgdpna[gdp$iso3c=="DDR"&gdp$year==1960] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1960]*0.19
gdp$rgdpna[gdp$iso3c=="DDR"&gdp$year==1961] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1961]*0.19
gdp$rgdpna[gdp$iso3c=="DDR"&gdp$year==1962] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1962]*0.18
gdp$rgdpna[gdp$iso3c=="DDR"&gdp$year==1963] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1963]*0.18
gdp$rgdpna[gdp$iso3c=="DDR"&gdp$year==1964] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1964]*0.17
gdp$rgdpna[gdp$iso3c=="DDR"&gdp$year==1965] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1965]*0.16
gdp$rgdpna[gdp$iso3c=="DDR"&gdp$year==1966] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1966]*0.16
gdp$rgdpna[gdp$iso3c=="DDR"&gdp$year==1967] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1967]*0.16
gdp$rgdpna[gdp$iso3c=="DDR"&gdp$year==1968] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1968]*0.15
gdp$rgdpna[gdp$iso3c=="DDR"&gdp$year==1969] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1969]*0.14
gdp$rgdpna[gdp$iso3c=="DDR"&gdp$year==1970] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1970]*0.14
gdp$rgdpna[gdp$iso3c=="DDR"&gdp$year==1971] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1971]*0.14
gdp$rgdpna[gdp$iso3c=="DDR"&gdp$year==1972] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1972]*0.14
gdp$rgdpna[gdp$iso3c=="DDR"&gdp$year==1973] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1973]*0.14
gdp$rgdpna[gdp$iso3c=="DDR"&gdp$year==1974] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1974]*0.14
gdp$rgdpna[gdp$iso3c=="DDR"&gdp$year==1975] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1975]*0.15
gdp$rgdpna[gdp$iso3c=="DDR"&gdp$year==1976] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1976]*0.14
gdp$rgdpna[gdp$iso3c=="DDR"&gdp$year==1977] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1977]*0.15
gdp$rgdpna[gdp$iso3c=="DDR"&gdp$year==1978] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1978]*0.15
gdp$rgdpna[gdp$iso3c=="DDR"&gdp$year==1979] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1979]*0.15
gdp$rgdpna[gdp$iso3c=="DDR"&gdp$year==1980] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1980]*0.15
gdp$rgdpna[gdp$iso3c=="DDR"&gdp$year==1981] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1981]*0.16
gdp$rgdpna[gdp$iso3c=="DDR"&gdp$year==1982] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1982]*0.17
gdp$rgdpna[gdp$iso3c=="DDR"&gdp$year==1983] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1983]*0.17
gdp$rgdpna[gdp$iso3c=="DDR"&gdp$year==1984] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1984]*0.18
gdp$rgdpna[gdp$iso3c=="DDR"&gdp$year==1985] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1985]*0.18
gdp$rgdpna[gdp$iso3c=="DDR"&gdp$year==1986] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1986]*0.18
gdp$rgdpna[gdp$iso3c=="DDR"&gdp$year==1987] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1987]*0.18
gdp$rgdpna[gdp$iso3c=="DDR"&gdp$year==1988] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1988]*0.18
gdp$rgdpna[gdp$iso3c=="DDR"&gdp$year==1989] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1989]*0.17
gdp$rgdpna[gdp$iso3c=="DDR"&gdp$year==1990] <- gdp$rgdpna[gdp$iso3c=="DEU"&gdp$year==1990]*0.17

gdp$iso3c[gdp$iso3c=="DEU"&gdp$year<=1990] <- "BRD"

# YEM
gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1950] <- 36403208086*0.766175876
gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1951] <- 36403208086*0.782980248
gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1952] <- 36403208086*0.802124909
gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1953] <- 36403208086*0.823335607
gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1954] <- 36403208086*0.845934022
gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1955] <- 36403208086*0.86899501
gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1956] <- 36403208086*0.89063537
gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1957] <- 36403208086*0.913630538
gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1958] <- 36403208086*0.936079029
gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1959] <- 36403208086*0.959410613
gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1960] <- 36403208086*0.985146478
gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1961] <- 36403208086*1.012566255
gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1962] <- 36403208086*1.042024643
gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1963] <- 36403208086*1.075295142
gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1964] <- 36403208086*1.103530363
gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1965] <- 36403208086*1.132005097
gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1966] <- 36403208086*1.163361313
gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1967] <- 36403208086*2.159944126*0.553142898
gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1968] <- 36403208086*2.239144624*0.545840546
gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1969] <- 36403208086*2.234323263*0.508364272
gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1970] <- 36403208086*2.578976526*0.531671379
gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1971] <- 36403208086*2.069952701*0.717879913
gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1972] <- 36403208086*2.306435246*0.740464405
gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1973] <- 36403208086*2.423371077*0.740840973
gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1974] <- 36403208086*2.791522303*0.765063879
gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1975] <- 36403208086*3.13662534*0.783567826
gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1976] <- 36403208086*3.339016457*0.785461425
gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1977] <- 36403208086*3.440034666*0.785064651
gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1978] <- 36403208086*3.539224523*0.777036978
gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1979] <- 36403208086*3.756606288*0.781373105
gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1980] <- 36403208086*4.08193754*0.78433014
gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1981] <- 36403208086*4.425800955*0.789147717
gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1982] <- 36403208086*4.609332634*0.786674748
gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1983] <- 36403208086*4.728460657*0.778670468
gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1984] <- 36403208086*4.873117941*0.772559119
gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1985] <- 36403208086*5.699428275*0.792284933
gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1986] <- 36403208086*5.872827234*0.79340554
gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1987] <- 36403208086*6.301204335*0.795408931
gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1988] <- 36403208086*7.246468998*0.809809327
gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1989] <- 36403208086*0.409221266

gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1967] <- 36403208086*2.159944126*0.446857102
gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1968] <- 36403208086*2.239144624*0.454159454
gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1969] <- 36403208086*2.234323263*0.491635728
gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1970] <- 36403208086*2.578976526*0.468328621
gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1971] <- 36403208086*2.069952701*0.282120087
gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1972] <- 36403208086*2.306435246*0.259535595
gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1973] <- 36403208086*2.423371077*0.259159027
gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1974] <- 36403208086*2.791522303*0.234936121
gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1975] <- 36403208086*3.13662534*0.216432174
gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1976] <- 36403208086*3.339016457*0.214538575
gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1977] <- 36403208086*3.440034666*0.214935349
gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1978] <- 36403208086*3.539224523*0.222963022
gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1979] <- 36403208086*3.756606288*0.218626895
gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1980] <- 36403208086*4.08193754*0.21566986
gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1981] <- 36403208086*4.425800955*0.210852283
gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1982] <- 36403208086*4.609332634*0.213325252
gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1983] <- 36403208086*4.728460657*0.221329532
gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1984] <- 36403208086*4.873117941*0.227440881
gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1985] <- 36403208086*5.699428275*0.207715067
gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1986] <- 36403208086*5.872827234*0.20659446
gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1987] <- 36403208086*6.301204335*0.204591069
gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1988] <- 36403208086*7.246468998*0.190190673
gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1989] <- 36403208086*0.590778734
gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1990] <- 36403208086*0.590778734 # rough estimate - double check

gdp$iso3c[gdp$iso3c=="YEM"&gdp$year<=1990] <- "YAR"

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

gdp <- gdp %>%
  dplyr::select(iso3c,year,rgdpna) %>%
  dplyr::rename(gdp = rgdpna)

### add workbook estimates ----------------------------------------------------------------------
# adds additional gdp estimates (file notes sources of estimates)
gdp.add <- readxl::read_excel("Data files/Workbooks/gdp_add.xlsx") %>%
  dplyr::select(iso3c,year,gdp)

gdp <- gdp %>%
  rbind(gdp.add)

gdp$iso3c[gdp$iso3c=="RUS"&gdp$year<= 1991] <- "SOV"

gdp <- gdp %>%
  na.omit()

# adds gdp estimates for countries in the 1940s (file notes sources of estimates)
gdp_40s <- readxl::read_excel("Data files/Workbooks/gdp_estimates_40s.xlsx", sheet = 1) %>%
  dplyr::select(-method)

gdp <- gdp %>%
  rbind(gdp_40s)

### write data ----------------------------------------------------------------------
# writes formatted dataframe as csv files
write.csv(gdp,"Data files/Formatted data files/gdp.csv")
