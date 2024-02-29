# This script creates an estimated tax revenue variable for each country-year.

### load libraries ----------------------------------------------------------------------
library(readxl)
library(countrycode)
library(imputeTS)
library(stringr)
library(dplyr)
library(tidyr)

### not in function ----------------------------------------------------------------------
'%!in%' <- function(x,y)!('%in%'(x,y))

### load datasets ----------------------------------------------------------------------
# World Bank - Tax Revenue (% of GDP) - GC.TAX.TOTL.GD.ZS
tax.wb <- read.csv("Data files/Raw data files/API_GC.TAX.TOTL.GD.ZS_DS2_en_csv_v2_1124880.csv",skip=4)

# IMF - General_government_Percent_of_GDP [imf2]
tax.imf2 <- readxl::read_xlsx("Data files/Raw data files/General_government_Percent_of_GDP_T.xlsx", sheet = 2)

# OECD - Total Tax Revenue (TOTALTAX) - Tax revenue as % of GDP (TAXGDP)
tax.oecd <- read.csv("Data files/Raw data files/RS_GBL_29062020081531984.csv")

# IMF World Revenue Longitudinal Data (WoRLD)
tax.imf <- readxl::read_xlsx("Data files/Raw data files/Tax_Revenue_in_Percent_of_GDP.xlsx", sheet = 1, skip = 1)

# ICTD / UNU-WIDER
tax.ictd <- readxl::read_xlsx("~/Downloads/ICTDWIDERGRD_2020.xlsx", sheet = 2)

# Artbetman and Kugler [AK] 2013
# https://www.researchgate.net/publication/271588312_RPC_v21_2013_components old version
tax.ak <- readxl::read_xlsx("Data files/Raw data files/RPC2015_components.xlsx")

### format datasets ----------------------------------------------------------------------
#### World Bank [WB] data ----------------------------------------------------------------------
tax.wb <- tax.wb %>%
  dplyr::select(-c(Indicator.Name,Indicator.Code,X)) %>%
  tidyr::pivot_longer(3:62, names_to = "year", values_to = "value") %>%
  dplyr::mutate(year = as.numeric(stringr::str_sub(year, start = 2, end = 5))) %>%
  # filter out groups of countries
  dplyr::filter(Country.Code %!in% c("ARB","CSS","CEB","EAR","EAP","EAS","LDC","MEA","MNA","TEA","TMN",
                                     "ECA","ECS","EMU","EUU","FCS","HIC","HPC","IBD","IDA","IDX","INX",
                                     "LAC","LCN","LMC","LMY","LTE","MIC","NAC","OED","OSS","PRE","PSS",
                                     "PST","SAS","SSA","SSF","SST","TEC","TLA","TSA","TSS","UMC","WLD",
                                     "IDB","IBT"),
                # filter out territories / non-sovereign entities
                Country.Code %!in% c("ABW","ASM","BMU","CHI","CUW","CYM","FRO","GIB","GRL","GUM","HKG",
                                     "IMN","MAC","MAF","MNP","NCL","PYF","SXM","TCA","VGB","VIR","PRI"),
                # filter out countries with no data in the dataset
                Country.Code %!in% c("AND","ATG","BRN","COM","CUB","DJI","DMA","ERI","GRD","GUY","HTI",
                                     "LBY","LIC","LIE","MNE","OMN","PRK","QAT","SLE","SSD","STP","SUR",
                                     "SWZ","SYR","TCD","TKM","TUV","VEN","VNM","XKX","YEM")) %>%
  dplyr::select(-Country.Name) %>%
  dplyr::rename(iso3c = Country.Code,
                wb.value = value)

#### IMF WoRLD [IMF] data ----------------------------------------------------------------------
tax.imf <- tax.imf[,-seq(from = 3, to = 57, by = 2)] %>%
  tidyr::pivot_longer(2:29, names_to = "year", values_to = "imf.value") %>%
  dplyr::rename(country = 1) %>%
  dplyr::mutate(year = as.numeric(year),
                # using the countrycode package, add iso3c based on country name
                iso3c = countrycode::countrycode(country,"country.name","iso3c")) %>%
  dplyr::select(iso3c,year,imf.value)

#### IMF [IMF2] data ----------------------------------------------------------------------
tax.imf2 <- tax.imf2 %>%
  tidyr::pivot_longer(2:49, names_to = "year", values_to = "value") %>%
  dplyr::rename(country = 1) %>%
  dplyr::filter(country != "NA" & country != "Scale: Units") %>%
  dplyr::rename(imf2.value = value) %>%
  dplyr::mutate(year = as.numeric(year),
                # using the countrycode package, add iso3c based on country name
                iso3c = countrycode::countrycode(country, "country.name", "iso3c"))

tax.imf2$iso3c[tax.imf2$country=="Kosovo, Rep. of"] <- "KSV"

tax.imf2 <- tax.imf2 %>%
  dplyr::select(-country)

#### OECD data ----------------------------------------------------------------------
tax.oecd <- tax.oecd %>%
  # using the countrycode package, add iso3c based on country name
  dplyr::mutate(iso3c = countrycode::countrycode(Country, "country.name","iso3c")) %>%
  dplyr::filter(iso3c %!in% c(419,"AFRIC","COK","OAVG","TKL"),
                Country %!in% c("Latin America and the Caribbean","OECD - Average","Africa")) %>%
  dplyr::select(iso3c,Year,Value) %>%
  dplyr::rename(year = Year,
                oecd.value = Value)

#### ICTD data ----------------------------------------------------------------------
# format ICTD / UNU-WIDER (ictd) data
tax.ictd <- tax.ictd[-c(1:2),] %>%
  dplyr::select(Country,ISO,Year,`...24`) %>%
  dplyr::rename_with(tolower) %>%
  dplyr::rename(ictd.value = 4) %>%
  dplyr::mutate(year = as.numeric(year),
                ictd.value = 100*as.numeric(ictd.value),
                # using the countrycode package, add iso3c based on country name
                iso3c = countrycode::countrycode(country,"country.name","iso3c"))

tax.ictd$iso3c[tax.ictd$country=="Kosovo"] <- "KSV"

tax.ictd <- tax.ictd %>%
  dplyr::select(iso3c,year,ictd.value) %>%
  # filter out territories / non-sovereign entities
  dplyr::filter(iso3c %!in% c("ABW","AIA","HKG","MAC","MSR"))

#### AK data ----------------------------------------------------------------------
tax.ak <- tax.ak %>%
  dplyr::select(country,year,tax) %>%
  # using the countrycode package, add iso3c based on country name
  dplyr::mutate(iso3c = countrycode::countrycode(country,"country.name","iso3c")) %>%
  dplyr::filter(country %!in% c("Faeore Islands","Netherlands Antilles"),
                iso3c %!in% c("ASM","AIA","ATG","ABW","BHS","BRB","BMU","BTN","BRN","CYM","HKG",
                              "MAC","DMA","PYF","GRL","GRD","GUM","KIR","MHL","FSM","MLT","MNE",
                              "MSR","NRU","NCL","SLB","KNA","LCA","VCT","TON","VUT","PLW","WSM",
                              "SMR"))

tax.ak$iso3c[tax.ak$country=="Guinea-Bissaau"] <- "GNB"

tax.ak <- tax.ak %>%
  dplyr::mutate(ak.value = tax * 100) %>%
  dplyr::select(iso3c,year,ak.value)

### merge datasets ----------------------------------------------------------------------
taxrev <- dplyr::full_join(tax.ictd,tax.oecd,by=c("iso3c","year")) %>%
  dplyr::full_join(tax.imf,by=c("iso3c","year")) %>%
  dplyr::full_join(tax.wb,by=c("iso3c","year")) %>%
  dplyr::full_join(tax.imf2,by=c("iso3c","year")) %>%
  dplyr::full_join(tax.ak,by=c("iso3c","year")) %>%
  # using the countrycode package, add country name based on iso3c code
  dplyr::mutate(country = countrycode::countrycode(iso3c,"iso3c","country.name")) %>%
  dplyr::relocate(country, .after = iso3c) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(country = ifelse(iso3c=="KSV","Kosovo",country),
                max = max(ictd.value,oecd.value,imf.value,wb.value,imf2.value,ak.value,na.rm=TRUE),
                min = min(ictd.value,oecd.value,imf.value,wb.value,imf2.value,ak.value,na.rm=TRUE),
  # dplyr::mutate(max = max(ictd.value,oecd.value,imf.value,wb.value,na.rm=TRUE),
  #               min = min(ictd.value,oecd.value,imf.value,wb.value,na.rm=TRUE),
                diff = max - min) %>%
  # replace Infs with NAs
  dplyr::mutate_all(~ifelse(is.infinite(.), NA, .))

for(i in 1:nrow(taxrev)){
  taxrev$points[i] <- sum(!is.na(taxrev$ictd.value[i]),!is.na(taxrev$oecd.value[i]),!is.na(taxrev$imf.value[i]),
                          !is.na(taxrev$wb.value[i]),!is.na(taxrev$imf2.value[i]),!is.na(taxrev$ak.value[i]))
}

### X-to-AK comparison function ----------------------------------------------------------------------
# this function compares the ICTD, OECD, IMF, WB, and IMF2 values to AK values for the last 5 years of available
# AK data (defaults to 2013; default values compared are 2009 - 2013 and default changes compared are
# 2008-09 - 2012-13)
x.to.ak <- function(df = taxrev, iso, lastyr = 2013){
  
  df2 <- df %>%
    dplyr::filter(iso3c == iso,
                  year %in% c((lastyr-5):lastyr)) %>%
    dplyr::arrange(year)
  
  # calculate yearly change
  for(y in (lastyr-5):lastyr){
    
    df2$ictd.change[df2$year==y] <- 100*(df2$ictd.value[df2$year==y]-df2$ictd.value[df2$year==(y-1)])/df2$ictd.value[df2$year==(y-1)]
    df2$oecd.change[df2$year==y] <- 100*(df2$oecd.value[df2$year==y]-df2$oecd.value[df2$year==(y-1)])/df2$oecd.value[df2$year==(y-1)]
    df2$imf.change[df2$year==y] <- 100*(df2$imf.value[df2$year==y]-df2$imf.value[df2$year==(y-1)])/df2$imf.value[df2$year==(y-1)]
    df2$wb.change[df2$year==y] <- 100*(df2$wb.value[df2$year==y]-df2$wb.value[df2$year==(y-1)])/df2$wb.value[df2$year==(y-1)]
    df2$imf2.change[df2$year==y] <- 100*(df2$imf2.value[df2$year==y]-df2$imf2.value[df2$year==(y-1)])/df2$imf2.value[df2$year==(y-1)]
    df2$ak.change[df2$year==y] <- 100*(df2$ak.value[df2$year==y]-df2$ak.value[df2$year==(y-1)])/df2$ak.value[df2$year==(y-1)]
    
  }
  
  df2 <- df2 %>%
    dplyr::mutate(ictd.diff = ictd.value - ak.value,
                  oecd.diff = oecd.value - ak.value,
                  imf.diff = imf.value - ak.value,
                  wb.diff = wb.value - ak.value,
                  imf2.diff = imf2.value - ak.value,
                  ictd.change.diff = ictd.change - ak.change,
                  oecd.change.diff = oecd.change - ak.change,
                  imf.change.diff = imf.change - ak.change,
                  wb.change.diff = wb.change - ak.change,
                  imf2.change.diff = imf2.change - ak.change) %>%
    # filter out first year in the dataset, as it has already been used to calculate yearly change
    dplyr::filter(year != (lastyr-5))
  
  x <- glm(ak.value ~ ictd.value, data = df2)
  summary(x)
  
  mean(df2$ictd.diff)

  
  # correlations - values
  ictd.value.cor <- cor(df2$ictd.value,df2$ak.value)
  oecd.value.cor <- cor(df2$oecd.value,df2$ak.value)
  imf.value.cor <- cor(df2$imf.value,df2$ak.value)
  wb.value.cor <- cor(df2$wb.value,df2$ak.value)
  imf2.value.cor <- cor(df2$imf2.value,df2$ak.value)
  
  # correlations - change
  ictd.change.cor <- cor(df2$ictd.change,df2$ak.change)
  oecd.change.cor <- cor(df2$oecd.change,df2$ak.change)
  imf.change.cor <- cor(df2$imf.change,df2$ak.change)
  wb.change.cor <- cor(df2$wb.change,df2$ak.change)
  imf2.change.cor <- cor(df2$imf2.change,df2$ak.change)
  
  # weights - values
  value.cor.sum <- sum(ictd.value.cor,oecd.value.cor,imf.value.cor,wb.value.cor,ictd.value.cor,na.rm=TRUE)
  
  ictd.value.weight <- ictd.value.cor / value.cor.sum
  oecd.value.weight <- oecd.value.cor / value.cor.sum
  imf.value.weight <- imf.value.cor / value.cor.sum
  wb.value.weight <- wb.value.cor / value.cor.sum
  imf2.value.weight <- imf2.value.cor / value.cor.sum
  
  # if NA, replace as 0
  if(is.na(ictd.value.weight)){
    ictd.value.weight <- 0
  }
  if(is.na(oecd.value.weight)){
    oecd.value.weight <- 0
  }
  if(is.na(imf.value.weight)){
    imf.value.weight <- 0
  }
  if(is.na(wb.value.weight)){
    wb.value.weight <- 0
  }
  if(is.na(imf2.value.weight)){
    imf2.value.weight <- 0
  }
  
  
  # est 2014 weights (test)
  est.2014.value <- sum((df$ictd.value[df$iso3c==iso&df$year==2014]-mean(df2$ictd.diff))*ictd.value.weight,
                        (df$oecd.value[df$iso3c==iso&df$year==2014]-mean(df2$oecd.diff))*oecd.value.weight,
                        (df$imf.value[df$iso3c==iso&df$year==2014]-mean(df2$imf.diff))*imf.value.weight,
                        (df$wb.value[df$iso3c==iso&df$year==2014]-mean(df2$wb.diff))*wb.value.weight,
                        (df$imf2.value[df$iso3c==iso&df$year==2014]-mean(df2$imf2.diff))*imf2.value.weight,
                        na.rm=TRUE)
    
}

### estimate missing ak values ----------------------------------------------------------------------
# this section only covers 1960 onwards; 1946-1959 calculated later

#### AFG ----------------------------------------------------------------------
# ICTD: 1981-1989; 2003-2017
# OECD: N/A
# IMF: 2003-2017
# WB: 2006-2017
# IMF2: 2006-2017
# AK: 1960-2013

#### AGO ----------------------------------------------------------------------
# [Independence starting 1975]
# ICTD: 1985-2005; 2007-2017
# OECD: N/A
# IMF: 1996-2017
# WB: 1999-2017
# IMF2: N/A
# AK: 1960-2013

#### ALB ----------------------------------------------------------------------
# ICTD: 1989-2018
# OECD: N/A
# IMF: 1995-2017
# WB: 1995-1998; 2002-2004; 2011-2018
# IMF2: 2005-2018
# AK: 1995-2013

#### ARE ----------------------------------------------------------------------
# [Independence starting 1971]
# ICTD: 2012-2018
# OECD: N/A
# IMF: 1990-2017
# WB: 1997-1999; 2011-2018
# IMF2: 2011-2018
# AK: 1960-2013

#### ARG ----------------------------------------------------------------------
# ICTD: 1985-1988; 1990-2018
# OECD: 1990-2018
# IMF: 1990-2016
# WB: 1990-2018
# IMF2: 2002-2004
# AK: 1960-2013

#### ARM ----------------------------------------------------------------------
# [Independence starting 1991]
# ICTD: 1991-2012
# OECD: N/A
# IMF: 1993-2017
# WB: 2004-2018
# IMF2: 2004-2018
# AK: 1991-2013

#### ATG ----------------------------------------------------------------------
# [Independence starting 1981]
# ICTD: 1992-2017
# OECD: N/A
# IMF: N/A
# WB: N/A
# IMF2: N/A
# AK: N/A

#### AUS ----------------------------------------------------------------------
# ICTD: 1980-2017
# OECD: 1990-2017
# IMF: 1990-2016
# WB: 1972-2018
# IMF2: 1972-1994; 1999-2018
# AK: 1960-2013

#### AUT ----------------------------------------------------------------------
# ICTD: 1980-2018
# OECD: 1990-2018
# IMF: 1990-2017
# WB: 1972-2018
# IMF2: 1972-2018
# AK: 1960-2013

#### AZE ----------------------------------------------------------------------
# [Independence starting 1991]
# ICTD: 1994-2018
# OECD: N/A
# IMF: 1994-2017
# WB: 1994-1999; 2008-2018
# IMF2: 2008-2018
# AK: 1991-2013

#### BDI ----------------------------------------------------------------------
# [Independence starting 1962]
# ICTD: 1982-2014
# OECD: N/A
# IMF: 1991-2017
# WB: 1973-1981; 1991-1999
# IMF2: N/A
# AK: 1960-2013

#### BEL ----------------------------------------------------------------------
# ICTD: 1980-2018
# OECD: 1990-2018
# IMF: 1990-2017
# WB: 1995-2018
# IMF2: 1973-1989; 1995-2018
# AK: 1960-2013

#### BEN ----------------------------------------------------------------------
# [Independence starting 1960]
# ICTD: 1980-2018
# OECD: N/A
# IMF: 1990-2017
# WB: 1976-1979
# IMF2: N/A
# AK: 1960-2013

#### BFA ----------------------------------------------------------------------
# [Independence starting 1960]
# ICTD: 1980-2018
# OECD: 2000-2017
# IMF: 1990-2017
# WB: 2002-2018
# IMF2: N/A
# AK: 1960-2013

#### BGD ----------------------------------------------------------------------
# [Independence starting 1971]
# ICTD: 1984-2018
# OECD: N/A
# IMF: 2001-2016
# WB: 2001-2016
# IMF2: N/A
# AK: 1960-2013

#### BGR ----------------------------------------------------------------------
# ICTD: 1986-1988; 1990-2018
# OECD: 1995-2018
# IMF: 1990-2017
# WB: 1990-2018
# IMF2: 1990-2018
# AK: 1962-2013

#### BHR ----------------------------------------------------------------------
# [Independence starting 1971]
# ICTD: 1980-2011; 2013
# OECD: N/A
# IMF: 1990-2017
# WB: 1980-2004
# IMF2: N/A
# AK: 1960-2013

#### BHS ----------------------------------------------------------------------
# [Independence starting 1973]
# ICTD: 1980-1986; 1988-2018
# OECD: 1990-2018
# IMF: 1990-2017
# WB: 1990-2018
# IMF2: N/A
# AK: N/A

#### BIH ----------------------------------------------------------------------
# [Independence starting 1992]
# ICTD: 1999-2018
# OECD: N/A
# IMF: 1998-2017
# WB: 2005-2018
# IMF2: 2005-2018
# AK: 1999-2013

#### BLR ----------------------------------------------------------------------
# [Independence starting 1991]
# ICTD: 2003-2018
# OECD: N/A
# IMF: 1992-2017
# WB: 1992-2018
# IMF2: 2003-2018
# AK: 1991-2013

#### BLZ ----------------------------------------------------------------------
# [Independence starting 1981]
# ICTD: 1982-2018
# OECD: 1990-2018
# IMF: 1990-2017
# WB: 1990-2017
# IMF2: N/A
# AK: 1981-2013

#### BOL ----------------------------------------------------------------------
# ICTD: 2018
# OECD: 1990-2018
# IMF: 1990-2016
# WB: 1985-2007
# IMF2: 1986-2007
# AK: 1960-2013

#### BRA ----------------------------------------------------------------------
# ICTD: 1990-2018
# OECD: 1990-2018
# IMF: 1990-2016
# WB: 1980-1994; 1996-2018
# IMF2: 1980-1989; 2006-2018
# AK: 1960-2013

#### BRB ----------------------------------------------------------------------
# [Independence starting 1966]
# ICTD: 1981-2018
# OECD: 1990-2018
# IMF: N/A
# WB: 2003-2016
# IMF2: N/A
# AK: N/A

#### BRN ----------------------------------------------------------------------
# [Independence starting 1984]
# ICTD: 1990-2009; 2016-2018
# OECD: N/A
# IMF: N/A
# WB: N/A
# IMF2: N/A
# AK: N/A

#### BTN ----------------------------------------------------------------------
# [Independence starting 1971]
# ICTD: 1983-2018
# OECD: N/A
# IMF: 1990-2017
# WB: 1982-1986; 1988-2018
# IMF2: N/A
# AK: N/A

#### BWA ----------------------------------------------------------------------
# [Independence starting 1966]
# ICTD: 1980-1994; 2003-2018
# OECD: 2004-2017
# IMF: 1990-1996; 2001-2017
# WB: 1990-1996; 2006-2018
# IMF2: N/A
# AK: 1960-2013

#### CAF ----------------------------------------------------------------------
# [Independence starting 1960]
# ICTD: 1980-2012
# OECD: N/A
# IMF: 1990-2017
# WB: 2004; 2008-2012; 2014-2018
# IMF2: N/A
# AK: 1960-2013

#### CAN ----------------------------------------------------------------------
# ICTD: 1980-2018
# OECD: 1990-2018
# IMF: 1990-2017
# WB: 1990-2018
# IMF2: 1990-2019
# AK: 1960-2013

#### CHE ----------------------------------------------------------------------
# ICTD: 1980-2018
# OECD: 1990-2018
# IMF: 1990-2017
# WB: 1972-2018
# IMF2: 1972-2018
# AK: 1960-2013

#### CHL ----------------------------------------------------------------------
# ICTD: 1980-2018
# OECD: 1990-2018
# IMF: 1990-2017
# WB: 1972-2018
# IMF2: 1974-1988; 1992-2018
# AK: 1960-2013

#### CHN ----------------------------------------------------------------------
# ICTD: 1985-2017
# OECD: N/A
# IMF: 1990-2017
# WB: 2005-2017
# IMF2: 2005-2017
# AK: 1960-2013

#### CIV ----------------------------------------------------------------------
# [Independence starting 1960]
# ICTD: 1980-2012; 2016-2018
# OECD: 1990-2017
# IMF: 1990; 1994-2017
# WB: 1990; 1995; 2001-2018
# IMF2: 2018
# AK: 1960-2013

#### CMR ----------------------------------------------------------------------
# [Independence starting 1960]
# ICTD: 1982-1986; 1993-2017
# OECD: 1993-2017
# IMF: 1990-1995; 1998-2017
# WB: 1990-1995; 1998-1999; 2012-2018
# IMF2: N/A
# AK: 1960-2013

#### COD ----------------------------------------------------------------------
# [Independence starting 1960]
# ICTD: 1980-2018
# OECD: 2000-2017
# IMF: 1996-2017
# WB: 1972-1989
# IMF2: N/A
# AK: 1960-2013

#### COG ----------------------------------------------------------------------
# [Independence starting 1960]
# ICTD: 1980; 1982-1986; 1989-2018
# OECD: 1998-2017
# IMF: 1990-2017
# WB: 2001-2018
# IMF2: 2003-2014
# AK: 1960-2013

#### COL ----------------------------------------------------------------------
# ICTD: 1993-1999
# OECD: 1990-2018
# IMF: 1990-2016
# WB: 1998-2000; 2003; 2008-2018
# IMF2: 1998-2000; 2003; 2008-2018
# AK: 1960-2013

#### COM ----------------------------------------------------------------------
# [Independence starting 1975]
# ICTD: 1980-2018
# OECD: N/A
# IMF: N/A
# WB: N/A
# IMF2: N/A
# AK: 1960-2013

#### CPV ----------------------------------------------------------------------
# [Independence starting 1975]
# ICTD: 1980-2018
# OECD: 1990-2017
# IMF: N/A
# WB: 2005-2017
# IMF2: N/A
# AK: 1960-2013

#### CRI ----------------------------------------------------------------------
# ICTD: 1980-2018
# OECD: 1990-2018
# IMF: 1990-2016
# WB: 1972-2018
# IMF2: 2002-2018
# AK: 1960-2013

#### CUB ----------------------------------------------------------------------
# ICTD: 1990-2015
# OECD: 1990-2018
# IMF: N/A
# WB: N/A
# IMF2: N/A
# AK: N/A

#### CYP ----------------------------------------------------------------------
# [Independence starting 1960]
# ICTD: 1980-2018
# OECD: N/A
# IMF: 1990-2017
# WB: 1975-2018
# IMF2: 1995-2018
# AK: 1960-2013

#### CZE ----------------------------------------------------------------------
# ICTD: 1993-2018
# OECD: 1993-2018
# IMF: 1993-2017
# WB: 1993-2018
# IMF2: 1995-2018
# AK: 1991-2013

#### DEU ----------------------------------------------------------------------
# [unclear what the values before 1990 represent]
# ICTD: 1980-2018
# OECD: 1990-2018
# IMF: 1990-2017
# WB: 1972-2018
# IMF2: 1972-2018
# AK: 1960-2013

#### DJI ----------------------------------------------------------------------
# [Independence starting 1977]
# ICTD: 1981-2018
# OECD: N/A
# IMF: 1990-2017
# WB: N/A
# IMF2: N/A
# AK: 1960-2013

#### DMA ----------------------------------------------------------------------
# [Independence starting 1978]
# ICTD: 1980-2017
# OECD: N/A
# IMF: N/A
# WB: N/A
# IMF2: N/A
# AK: N/A

#### DNK ----------------------------------------------------------------------
# ICTD: 1980-2018
# OECD: 1990-2018
# IMF: 1990-2017
# WB: 1972-2018
# IMF2: 1972-2018
# AK: 1960-2013

#### DOM ----------------------------------------------------------------------
# ICTD: 1980-2018
# OECD: 1990-2018
# IMF: 1990-2017
# WB: 1972-2018
# IMF2: N/A
# AK: 1960-2013

#### DZA ----------------------------------------------------------------------
# [Independence starting 1962]
# ICTD: 2016-2017
# OECD: N/A
# IMF: 1990-2017
# WB: N/A
# IMF2: N/A
# AK: 1960-2013

#### ECU ----------------------------------------------------------------------
# ICTD: 1980-1985; 1990-2018
# OECD: 1990-2018
# IMF: 1990-2016
# WB: N/A
# IMF2: N/A
# AK: 1960-2013

#### EGY ----------------------------------------------------------------------
# ICTD: 1987-2018
# OECD: 2002-2017
# IMF: 1990-2017
# WB: 1975-1979; 1981-1997; 2002-2015
# IMF2: 2002-2015
# AK: 1960-2013

#### ERI ----------------------------------------------------------------------
# [Independence starting 1993]
# ICTD: 1992-2002
# OECD: N/A
# IMF: 1993-2017
# WB: N/A
# IMF2: N/A
# AK: 1992-2013

#### ESP ----------------------------------------------------------------------
# ICTD: 1981-2018
# OECD: 1990-2018
# IMF: 1990-2017
# WB: 1995-2018
# IMF2: 1980-2018
# AK: 1960-2013

#### EST ----------------------------------------------------------------------
# [Independence starting 1991]
# ICTD: 1993-2018
# OECD: 1995-2018
# IMF: 1995-2017
# WB: 1993-2018
# IMF2: 1995-2018
# AK: 1991-2013

#### ETH ----------------------------------------------------------------------
# ICTD: 1980-2018
# OECD: N/A
# IMF: 1990-2017
# WB: 1990-1999; 2001-2018
# IMF2: N/A
# AK: 1975-2013

#### FIN ----------------------------------------------------------------------
# ICTD: 1980-2018
# OECD: 1990-2018
# IMF: 1990-2017
# WB: 1972-2018
# IMF2: 1972-2018
# AK: 1960-2013

#### FJI ----------------------------------------------------------------------
# [Independence starting 1970]
# ICTD: 1980-2018
# OECD: 2008-2017
# IMF: 1990-1996; 2004-2006; 2010-2015
# WB: 1990-1996; 2004-2006; 2010-2018
# IMF2: N/A
# AK: 1960-2013

#### FRA ----------------------------------------------------------------------
# ICTD: 1980-2018
# OECD: 1990-2018
# IMF: 1990-2017
# WB: 1972-2018
# IMF2: 1972-2018
# AK: 1960-2013

#### FSM ----------------------------------------------------------------------
# [Independence starting 1991]
# ICTD: 1989-2018
# OECD: N/A
# IMF: N/A
# WB: 2008-2018
# IMF2: N/A
# AK: N/A

#### GAB ----------------------------------------------------------------------
# [Independence starting 1960]
# ICTD: 1980-1996
# OECD: N/A
# IMF: 1990-2017
# WB: 2012-2018
# IMF2: N/A
# AK: 1960-2013

#### GBR ----------------------------------------------------------------------
# ICTD: 1980-2018
# OECD: 1990-2018
# IMF: 1990-2017
# WB: 1972-2018
# IMF2: 1972-2018
# AK: 1960-2013

#### GEO ----------------------------------------------------------------------
# [Independence starting 1991]
# ICTD: 1995-2018
# OECD: N/A
# IMF: 1995-2017
# WB: 1997-2018
# IMF2: 2003-2018
# AK: 1997-2013

#### GHA ----------------------------------------------------------------------
# [Independence starting 1957]
# ICTD: 1983-2018
# OECD: 2000-2017
# IMF: 1990-2017
# WB: 1990-1993; 2001-2011; 2014-2018
# IMF2: N/A
# AK: 1960-2013

#### GIN ----------------------------------------------------------------------
# [Independence starting 1958]
# ICTD: 1980-2018
# OECD: N/A
# IMF: 1990-2017
# WB: 1989-1992
# IMF2: N/A
# AK: 1960-2013

#### GMB ----------------------------------------------------------------------
# [Independence starting 1965]
# ICTD: 1980-2018
# OECD: N/A
# IMF: 1990-1993; 2000-2017
# WB: 1990-1993; 2001-2009
# IMF2: N/A
# AK: 1960-2013

#### GNB ----------------------------------------------------------------------
# [Independence starting 1974]
# ICTD: 1980-2017
# OECD: N/A
# IMF: 1991-2017
# WB: 2017
# IMF2: N/A
# AK: 1960-2013

#### GNQ ----------------------------------------------------------------------
# [Independence starting 1968]
# ICTD: 1980-2018
# OECD: 2005-2017
# IMF: 1990-2017
# WB: 2006-2018
# IMF2: N/A
# AK: 1960-2013

#### GRC ----------------------------------------------------------------------
# ICTD: 1980-2018
# OECD: 1990-2018
# IMF: 1990-2018
# WB: 1972-1990; 1995-2018
# IMF2: 1995-2018
# AK: 1960-2013

#### GRD ----------------------------------------------------------------------
# [Independence starting 1974]
# ICTD: 1982-1986; 1992-2018
# OECD: N/A
# IMF: N/A
# WB: N/A
# IMF2: N/A
# AK: N/A

#### GTM ----------------------------------------------------------------------
# ICTD: 1980-2018
# OECD: 1990-2018
# IMF: 1990-2016
# WB: 1990-2018
# IMF2: N/A
# AK: 1960-2013

#### GUY ----------------------------------------------------------------------
# [Independence starting 1966]
# ICTD: 1987-2018
# OECD: 1990-2018
# IMF: 1990-2016
# WB: N/A
# IMF2: N/A
# AK: 1960-2013





### estimate missing ak values ----------------------------------------------------------------------
# ICTD: 
# OECD: 
# IMF: 
# WB: 
# IMF2: 
# AK: 



# 1960- missing values calculated here; 1946-1959 and 2014-2019 calculated later

# ALB

# ATG

# BGR

# BHS

# BIH

# BRB

# BRN

# BTN

# CUB

# CZE (Czechoslovakia)

# DMA

# ETH

# FSM

# GEO

# GRD

# HUN

# KHM

# KNA

# KSV

# LAO

# LCA

# LIE

# MCO

# MDV

# MHL

# MKD

# MLT

# MNE

# MNG

# NRU

# PLW

# PSE

# ROU

# SLB

# SMR

# SRB

# SSD

# TJK

# TKM

# TLS

# TON

# TUV

# VCT

# VNM

# VUT

# WSM

### old ----------------------------------------------------------------------

# taxrev <- taxrev %>%
#   dplyr::filter(points > 0) #%>%
# # dplyr::filter(diff > 5)

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

taxrev2 <- taxrev %>%
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

taxrev3 <- taxrev2 %>%
  dplyr::mutate(est = ifelse(iso3c %in% c("CUB","TKM"),ictd.value,
                             ifelse(year >= 2012&iso3c %in% use.ictd,ictd.value,
                                    ifelse(year >= 2012&iso3c %in% use.oecd,oecd.value,
                                           ifelse(year >= 2012&iso3c %in% use.imf,imf.value,
                                                  ifelse(year >= 2012&iso3c %in% use.wb,wb.value,
                                                         ifelse(year < 1995&iso3c == "ALB",ictd.value,
                                                                ifelse(year < 1997&iso3c == "GEO",ictd.value,
                                                                       ifelse(year > 1974&year <= 1997&iso3c == "KHM",ictd.value,est)))))))),
                est = ifelse(is.na(est),ak.value,est))

for(i in 1:nrow(taxrev2)){
  if(taxrev2$iso3c[i] %in% c("CUB","TKM")){
    taxrev2$est[i] <- taxrev2$ictd.value[i]
  }
  if(taxrev2$year[i] >= 2012){
    if(taxrev2$iso3c[i] %in% use.ictd){
      taxrev2$est[i] <- taxrev2$ictd.value[i]
      }
    if(taxrev2$iso3c[i] %in% use.oecd){
      taxrev2$est[i] <- taxrev2$oecd.value[i]
      }
    if(taxrev2$iso3c[i] %in% use.imf){
      taxrev2$est[i] <- taxrev2$imf.value[i]
      }
    if(taxrev2$iso3c[i] %in% use.wb){
      taxrev2$est[i] <- taxrev2$wb.value[i]
      }
    }
  if(taxrev2$year[i] <= 2011){
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

taxrev_missing <- taxrev2 %>%
  dplyr::group_by(iso3c,is.na(est)) %>%
  dplyr::tally() %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(names_from = `is.na(est)`, values_from = n) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(perc.missing = `TRUE` / sum(`FALSE`,`TRUE`,na.rm=TRUE)) %>%
  dplyr::filter(perc.missing == 1) %>%
  dplyr::pull(iso3c)

# linear interpolation
taxrev2 <- taxrev2 %>%
  dplyr::filter(iso3c %!in% taxrev_missing) %>%
  dplyr::select(iso3c,year,est)

taxrev2 <- taxrev2 %>%
  dplyr::full_join(expand.grid(iso3c = unique(taxrev2$iso3c),year = c(1946:2019)), by=c("iso3c","year"))

taxrev2 <- taxrev2 %>%
  dplyr::group_by(iso3c) %>%
  dplyr::arrange(year) %>%
  dplyr::mutate(taxgdp = imputeTS::na_interpolation(est), option = "spline") %>%
  dplyr::ungroup() %>%
  dplyr::select(-c(est,option)) %>%
  # using the countrycode package, add country name based on iso3c
  dplyr::mutate(country = countrycode::countrycode(iso3c,"iso3c","country.name"))

# writes formatted dataframe as csv files
write.csv(taxrev2,"Data files/Formatted data files/tax_revenue.csv",row.names = FALSE)

# Need to fill in missing data for BIH,KHM,CUB,ERI,ETH,GEO,LAO,ROU,SRB,TJK,TLS,TKM,VNM
