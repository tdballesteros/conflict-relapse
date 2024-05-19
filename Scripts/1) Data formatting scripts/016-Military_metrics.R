# This script formats multiple military-related government metrics.

### load libraries ----------------------------------------------------------------------
library(readxl)
library(countrycode)
library(ggplot2)
library(stats)
library(dplyr)
library(tidyr)

### not in function ----------------------------------------------------------------------
'%!in%' <- function(x,y)!('%in%'(x,y))

### load data files ----------------------------------------------------------------------
cow <- read.csv("Data files/Raw data files/NMC_5_0.csv")

# load all sheets in the "Military Balance.xlsx" file
military.balance <- lapply(
  readxl::excel_sheets("Data files/Raw data files/Military Balance.xlsx"),
  read_xlsx,
  na = c("n.k.","n/a","n.k","n.a.","n.a"),
  path = "Data files/Raw data files/Military Balance.xlsx"
  )

# load all sheets in the "SIPRI-Milex-data-1949-2019.xlsx"" file
sipri.sheets <- lapply(
  readxl::excel_sheets("Data files/Raw data files/SIPRI-Milex-data-1949-2019.xlsx"),
  read_xlsx,
  skip = 5,
  na = c("xxx",". ."),
  path = "Data files/Raw data files/SIPRI-Milex-data-1949-2019.xlsx"
  )

# WMEAT files
wmeat.1973 <- readxl::read_xlsx(path = "Data files/Raw data files/185674.xlsx",
                                skip = 5,
                                na = c("-","…","N.A.","xx","a","b","c","d","e","f","g","h","i","j",
                                       "k","l","m","n","o","p","q","r","s","t","u","v","w","x","y",
                                       "TO 8 (EST)","6 TO 10 (EST)"),
                                col_names = FALSE) 
wmeat.1984 <- readxl::read_xlsx(path = "Data files/Raw data files/185659.xlsx",
                                skip = 5,
                                na = c("NA", "E", "e", "b", "...", "d", "c", "E b", "d c", "f"),
                                col_names = FALSE) 
wmeat.1995 <- readxl::read_xlsx(path = "Data files/Raw data files/185685.xlsx",
                                skip = 5,
                                na = c("NA","E","R","d","E,b","b","c","E,d","c,f"),
                                col_names = FALSE)
wmeat.2005 <- lapply(readxl::excel_sheets("Data files/Raw data files/121777.xls"),
                     read_xls, skip = 7, na = c("NA","E","R"), col_names = FALSE,
                     path = "Data files/Raw data files/121777.xls")
wmeat.2012 <- lapply(readxl::excel_sheets("Data files/Raw data files/209509.xlsx"),
                     read_xlsx, skip = 6, na = "n/a",
                     path = "Data files/Raw data files/209509.xlsx")
wmeat.2016 <- lapply(readxl::excel_sheets("Data files/Raw data files/266013.xlsx"),
                     read_xlsx, skip = 6, na = "n/a",
                     path = "Data files/Raw data files/266013.xlsx")
wmeat.2019 <- lapply(readxl::excel_sheets("Data files/Raw data files/WMEAT-2019-Table-I-Military-Expenditures-and-Armed-Forces-Personnel-2007-2017.xlsx"),
                     read_xlsx, skip = 6, na = "n/a",
                     path = "Data files/Raw data files/WMEAT-2019-Table-I-Military-Expenditures-and-Armed-Forces-Personnel-2007-2017.xlsx")

# country-years dataset
cyears <- read.csv("Data files/Formatted data files/country_years.csv")

# gdp dataset
gdp <- read.csv("Data files/Formatted data files/gdp.csv")

# population dataset
population <- read.csv("Data files/Formatted data files/population.csv")

### inflation table ----------------------------------------------------------------------
# inflation from https://www.bls.gov/data/inflation_calculator.htm
# July to July, converting to 2019 dollars
inflation_table <- data.frame(
  year = c(1946:2020),
  multiplier = c(
    1+11.96,1+10.56,1+9.52,1+9.83, #40s
    1+9.65,1+8.91,1+8.61,1+8.57,1+8.54,1+8.57,1+8.36,1+8.07,1+7.85,1+7.79, #50s
    1+7.67,1+7.55,1+7.47,1+7.36,1+7.25,1+7.12,1+6.89,1+6.68,1+6.35,1+5.97, #60s
    1+5.58,1+5.30,1+5.12,1+4.79,1+4.19,1+3.73,1+3.49,1+3.21,1+2.91,1+2.51, #70s
    1+2.10,1+1.80,1+1.63,1+1.57,1+1.46,1+1.38,1+1.34,1+1.25,1+1.17,1+1.06, #80s
    1+0.97,1+0.88,1+0.83,1+0.78,1+0.73,1+0.68,1+0.63,1+0.60,1+0.57,1+0.54, #90s
    1+0.48,1+0.45,1+0.42,1+0.40,1+0.35,1+0.31,1+0.26,1+0.23,1+0.17,1+0.19, #00s
    1+0.18,1+0.14,1+0.12,1+0.10,1+0.08,1+0.08,1+0.07,1+0.05,1+0.02,1+0.00, #10s
    1-0.01 #20s
    )
  )

### format data ----------------------------------------------------------------------
#### COW ----------------------------------------------------------------------
cow <- cow %>%
  dplyr::filter(year >= 1945) %>%
  # using the countrycode package, add iso3c based on country COW abbreviation
  dplyr::mutate(iso3c = countrycode::countrycode(stateabb,"cowc","iso3c")) %>%
                
  # codes iso3c values missing from the countrycode package
  dplyr::mutate(iso3c = dplyr::case_when(
    stateabb == "RVN" ~ "RVN",
    stateabb == "YPR" ~ "YPR",
    stateabb == "YAR" ~ "YAR",
    stateabb == "ZAN" ~ "ZAN",
    stateabb == "KOS" ~ "KSV",
    stateabb == "YUG" ~ "YUG",
    stateabb == "CZE" ~ "CZE",
    stateabb == "GDR" ~ "DDR",
    stateabb == "GFR" ~ "BRD",
    .default = iso3c
  )) %>%
  
  dplyr::select(iso3c,year,milex,milper) %>%
  dplyr::mutate(
    # recodes -9 (missing) to NA for milex and milper variables
    milex = ifelse(milex==-9,NA,milex),
    milper = ifelse(milper==-9,NA,milper),
    # multiplies milex and milper to be full numbers
    milex = 1000 * milex,
    milper = 1000 * milper
    ) %>%
  
  # adjust for inflation
  dplyr::left_join(inflation_table,by="year") %>%
  
  # multiples dollar values in that year's dollar value to 2019 dollars
  dplyr::mutate(milex = milex * multiplier) %>%
  
  dplyr::select(-multiplier) %>%
  dplyr::rename(milex.cow = milex,
                milper.cow = milper)

# recode YUG 1992-2017 as SRB
cow$iso3c[cow$iso3c=="YUG"&cow$year>=1992] <- "SRB"

# recode RUS 1946-1990 as SOV
cow$iso3c[cow$iso3c=="RUS"&cow$year<=1990] <- "SOV"

#### Military Balance (IISS) ----------------------------------------------------------------------
# rename datasets as Y20XX, referring to the year the report was published
names(military.balance) <- readxl::excel_sheets("Data files/Raw data files/Military Balance.xlsx")

# create empty dataframe to compile all formatted sheets into
military.balance.data <- data.frame()

# format each dataset and combine into one complete dataset
for(m in names(military.balance)){
  
  year <- as.numeric(stringr::str_sub(m, start = 2, end = 5))
  df <- military.balance[[m]]
  
  # create variable subheader years for expenditure data
  year.a <- dplyr::case_when(
    m == "Y2012" ~ 2008,
    .default = year - 3
  )
  year.b <- dplyr::case_when(
    m == "Y2012" ~ 2009,
    .default = year - 2
  )
  year.c <- dplyr::case_when(
    m == "Y2012" ~ 2010,
    .default = year - 1
  )
  
  # remove footnotes column (column  14) if it exists
  if(ncol(df) == 14){
    
    df <- df[,-14]
    
  }
  
  names(df) <- c("country",
                 paste0("defense.spending.",year.a), # in current year USD
                 paste0("defense.spending.",year.b),
                 paste0("defense.spending.",year.c),
                 paste0("defense.spending.percapita.",year.a), # in current year USD
                 paste0("defense.spending.percapita.",year.b),
                 paste0("defense.spending.percapita.",year.c),
                 paste0("defense.spending.percgdp.",year.a), # as percentage of GDP
                 paste0("defense.spending.percgdp.",year.b),
                 paste0("defense.spending.percgdp.",year.c),
                 paste0("active.armed.forces.",year),
                 paste0("reservists.",year),
                 paste0("active.paramilitary.",year))
  
  # remove first row (secondary header row containing years)
  df <- df[-1,]
  
  # remove footnotes rows at the end, based on which year it is
  number_of_footnote_rows <- dplyr::case_when(
    year == 2020 | year == 2015 | year == 2012 ~ 4,
    year == 2019 | year == 2017                ~ 5,
    year == 2018                               ~ 6,
    year == 2016                               ~ 3,
    year == 2014                               ~ 14,
    year == 2013                               ~ 28,
    .default                                   = 0
  )
  
  df <- df[-c((nrow(df)-number_of_footnote_rows+1):nrow(df)),]
  
  df <- df %>%
    # recoding country names countrycode package could not identify
    dplyr::mutate(country = dplyr::case_when(
      country == "UAE*" ~ "United Arab Emirates",
      country == "Somali Republic" ~ "Somalia",
      .default = country
    )) %>%
    
    # filter out headers (e.g., "North America") and total rows
    dplyr::filter(country %!in% c("North America","Europe","Russia and Eurasia","Asia","Middle East and North Africa",
                                  "Latin America and the Caribbean","Sub-Saharan Africa","Summary","Total","Total*","Total**",
                                  "Global totals","US","NATO EX-US","Total NATO","Non-NATO Europe","Russia 2",
                                  "South and Central Asia","East Asia and Australasia","Latin America and Caribbean",
                                  "Total *","Total **","Nato Europe","Non-Nato Europe","Subtotal NATO Ex-US",
                                  "Latin America & The Carribean","Latin America and the Carribean")) %>%
    tidyr::pivot_longer(cols = 2:13, names_to = "metric", values_to = "value") %>%
    
    dplyr::mutate(
      # add variable denoting which year the data was published / which sheet the data is from
      data.pub.year = year,
      # add variable for the year the data is for, based on the end of the variable name
      year = stringr::str_sub(metric, start = -4, end = -1),
      # rename variables to remove the specific year the data is for
      metric = stringr::str_sub(metric, start = 1, end = -6),
      # modify values to be full values
      value = ifelse(metric == "defense.spending", # multiply by 1,000,000
                     value * 1000000,
                     ifelse(metric %in% c("active.armed.forces","reservists", # multiply by 1,000
                                          "active.paramilitary"), value * 1000,
                            value)),
      # using the countrycode package, add iso3c based on country name
      iso3c = countrycode::countrycode(country,"country.name","iso3c"))
  
  military.balance.data <- rbind(military.balance.data,df)
  
}

military.balance.data.premerge <- military.balance.data

# filter out entries that appear to contain errors
military.balance.data$value[military.balance.data$iso3c=="FJI"&
                              military.balance.data$year==2011&
                              military.balance.data$data.pub.year==2014&
                              military.balance.data$metric %in% c("defense.spending",
                                                                  "defense.spending.percapita",
                                                                  "defense.spending.percgdp")] <- NA

military.balance.data <- military.balance.data %>%
  dplyr::mutate(year = as.numeric(year)) %>%
  # adjust for inflation
  #dplyr::left_join(inflation_table,dplyr::join_by("data.pub.year"=="year")) %>%
  dplyr::left_join(inflation_table,by="year") %>%
  dplyr::mutate(
    value = ifelse(metric %in% c("defense.spending","defense.spending.percapita"),
                   value * multiplier, value),
    # using the countrycode package, add country name based on iso3c code
    country = countrycode::countrycode(iso3c,"iso3c","country.name")
    ) %>%
  dplyr::select(-multiplier) %>%
  # combine variable name with publication year, to differentiate them as separate columns
  dplyr::mutate(metric = paste0(metric,".iiss")) %>%
  dplyr::relocate(iso3c, .before = country) %>%
  dplyr::relocate(year, .after = country) %>%
  tidyr::drop_na(value) %>%
  dplyr::group_by(iso3c,country,year,metric) %>%
  # keep the most recent published version of the data (for GDP, GDP per capita, and % GDP, which are
  # published for three consecutive years)
  dplyr::filter(data.pub.year == max(data.pub.year,na.rm=TRUE)) %>%
  dplyr::select(-data.pub.year) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(names_from = "metric", values_from = "value")

#### SIPRI ----------------------------------------------------------------------
# rename datasets to have sheet names
names(sipri.sheets) <- readxl::excel_sheets("Data files/Raw data files/SIPRI-Milex-data-1949-2019.xlsx")

sipri <- sipri.sheets[["Current USD"]]

# remove footnotes
sipri <- sipri[-c(192:204),]

sipri <- sipri %>%
  # filter out region labels
  dplyr::filter(Country %!in% c("Africa","North Africa","Sub-Saharan","Americas",
                                "Central America and the Caribbean","North America","South America",
                                "Asia & Oceania","Central Asia","East Asia","South Asia",
                                "South-East Asia","Oceania","Europe","Central Europe","Eastern Europe",
                                "Western Europe","Middle East")) %>%
  # remove Notes column
  dplyr::select(-Notes) %>%
  # convert variables to numeric
  dplyr::mutate_at(vars(2:72), as.numeric) %>%
  # pivot to long data format
  tidyr::pivot_longer(cols = 2:72, names_to = "year", values_to = "mil.expenditure.current.year") %>%
  dplyr::mutate(year = as.numeric(year)) %>%
  # convert values to full amount
  dplyr::mutate(mil.expenditure.current.year = mil.expenditure.current.year * 1000000) %>%
  # convert current year $ to constant 2019 $
  dplyr::left_join(inflation_table,by="year") %>%
  dplyr::mutate(
    milexp.sipri = mil.expenditure.current.year * multiplier,
    # using the countrycode package, add iso3c code based on country name
    iso3c = countrycode::countrycode(Country,"country.name","iso3c"),
    iso3c = dplyr::case_when(
      Country == "Czechoslovakia" ~ "CZE",
      Country == "German DR" ~ "DDR",
      Country == "Germany" & year %in% c(1946:1989) ~ "BRD",
      Country == "Kosovo" ~ "KSV",
      Country == "USSR" ~ "SOV",
      Country == "Yemen, North" ~ "YAR",
      Country == "Yugoslavia" ~ "YUG",
      .default = iso3c
    )) %>%
  dplyr::filter(
    Country != "Czechoslovakia" | year <= 1992,
    Country != "Czechia" | year >= 1993
  ) %>%
  dplyr::select(-c(mil.expenditure.current.year,multiplier,Country))


#### WMEAT ----------------------------------------------------------------------
##### WMEAT 1973 ----------------------------------------------------------------------
wmeat.1973.data <- wmeat.1973

names(wmeat.1973.data) <- c(
  "country.and.year",
  "military.expenditure.current.dollars", # in millions
  "blank1",
  "military.expenditure.1972.dollars", # in millions
  "blank2",
  "gdp.current.dollars", # in millions; gnp, not gdp
  "blank3",
  "gdp.1972.dollars", # in millions; gnp, not gdp
  "blank4",
  "mil.expenditure.over.gdp", # percentage; gnp, not gdp
  "blank5",
  "population", # in millions
  "blank6",
  "mil.expenditure.per.capita", # 1972 dollars
  "blank7",
  "gdp.per.capita", # 1972 dollars; gnp, not gdp
  "blank8",
  "armed.personnel", #in thousands
  "blank9",
  "mi.expenditure.per.armed.personnel", # 1972 dollarsw
  "blank10",
  "armed.forces.per.1000.pop",
  "blank11"
)

wmeat.1973.data <- wmeat.1973.data %>%
  # filter out 10-year average annual growth rows
  dplyr::filter(country.and.year != "GROWTH RATE (PCT ANN)") %>%
  # convert "see X" entries to NAs
  dplyr::mutate(country.and.year = ifelse(country.and.year %in% c("CEYLON (SEE SRI LANKA)",
                                                                  "KHMER REPUBLIC (SEE CAMBODIA)",
                                                                  "MADAGASCAR (SEE MALAGASY REPUBLIC)",
                                                                  "TAIWAN (SEE CHINA, REPUBLIC OF)",
                                                                  "USSR (SEE SOVIET UNION)"),
                                          NA, country.and.year)) %>%
  
  # remove blank columns
  dplyr::select(-dplyr::contains("blank"))

# remove blank rows
wmeat.1973.data <- wmeat.1973.data[rowSums(is.na(wmeat.1973.data)) != ncol(wmeat.1973.data),]

# list all countries in the dataset
wmeat.1973.country.list <- wmeat.1973.data %>%
  dplyr::filter(country.and.year %!in% c(1963:1973,NA)) %>%
  dplyr::pull(country.and.year)

# add placeholder column for country name
wmeat.1973.data <- wmeat.1973.data %>%
  dplyr::mutate(country = NA)

for(i in 1:(length(wmeat.1973.country.list)-1)){
  
  # pull the row with ith country header
  i.row <- which(wmeat.1973.data$country.and.year == wmeat.1973.country.list[i])
  
  # pull the row with the (i+1)th country header
  iplus1.row <- which(wmeat.1973.data$country.and.year == wmeat.1973.country.list[i+1])
  
  wmeat.1973.data$country[c((i.row+1):(iplus1.row-1))] <- wmeat.1973.country.list[i]
  
}

# code country for the last entry on the list
i.row.zmb <- which(wmeat.1973.data$country.and.year == "ZAMBIA")
wmeat.1973.data$country[c((i.row.zmb+1):nrow(wmeat.1973.data))] <- "ZAMBIA"

wmeat.1973.data <- wmeat.1973.data %>%
  # filter out country header rows
  tidyr::drop_na(country) %>%
  dplyr::rename(year = country.and.year) %>%
  dplyr::select(country, year, military.expenditure.current.dollars, armed.personnel, gdp.current.dollars, population) %>%
  
  # convert to full values
  dplyr::mutate(military.expenditure.current.dollars = as.numeric(military.expenditure.current.dollars) * 1000000,
                armed.personnel = as.numeric(armed.personnel) * 1000,
                gdp.current.dollars = as.numeric(gdp.current.dollars) * 1000000,
                population = as.numeric(population) * 1000000) %>%
  tidyr::pivot_longer(3:6, names_to = "variable", values_to = "value") %>%
  
  dplyr::mutate(
    # add WMEAT version year
    version = 1973,
    # using the countrycode package, add iso3c code based on country name
    # add missing iso3c codes
    iso3c = dplyr::case_when(
      country == "CZECHOSLOVAKIA" ~ "CZE",
      country == "GERMAN DEMOCRATIC REPUBLIC" ~ "DDR",
      country == "GERMANY, FEDERAL REPUBLIC OF" ~ "BRD",
      country == "VIETNAM, REPUBLIC OF" ~ "RVN",
      country == "YEMEN (ADEN)" ~ "YPR",
      country == "YEMEN (SANAA)" ~ "YAR",
      country == "YUGOSLAVIA" ~ "YUG",
      .default = countrycode::countrycode(country,"country.name","iso3c")
    )) %>%
  
  dplyr::select(-country) %>%
  dplyr::mutate(
    # using the countrycode package, add country name based on iso3c code
    # add missing country names
    country = dplyr::case_when(
      iso3c=="BRD" ~ "West Germany",
      iso3c=="DDR" ~ "East Germany",
      iso3c=="RVN" ~ "South Vietnam",
      iso3c=="YAR" ~ "North Yemen",
      iso3c=="YPR" ~ "South Yemen",
      iso3c=="YUG" ~ "Yugoslavia",
      .default     = countrycode::countrycode(iso3c,"iso3c","country.name")
    ))

##### WMEAT 1984 ----------------------------------------------------------------------
wmeat.1984.data <- wmeat.1984

names(wmeat.1984.data) <- c(
  "country.and.year",
  "military.expenditure.current.dollars", # in millions
  "blank1",
  "military.expenditure.1982.dollars", # in millions
  "blank2",
  "armed.personnel", #in thousands
  "blank3",
  "gdp.current.dollars", # in millions; gnp, not gdp
  "blank4",
  "gdp.1982.dollars", # in millions; gnp, not gdp
  "blank5",
  "central.gov.expenditure.1982.dollars", # in millions
  "blank6",
  "population", # in millions
  "blank7",
  "mil.expenditure.over.gdp", # percentage; gnp, not gdp
  "blank8",
  "mil.expenditure.over.central.gov.expenditure", # percentage
  "blank9",
  "mil.expenditure.per.capita", # 1982 dollars
  "blank10",
  "armed.forces.per.1000.pop",
  "blank11",
  "gdp.per.capita", # 1982 dollars; gnp, not gdp
  "blank12"
)

# adjust China notations
wmeat.1984.data$country.and.year[wmeat.1984.data$country.and.year=="CHINA"] <- NA
wmeat.1984.data$country.and.year[wmeat.1984.data$country.and.year=="MAINLAND"] <- "CHINA"

wmeat.1984.data <- wmeat.1984.data %>%
  # convert "see X" entries to NAs
  dplyr::mutate(country.and.year = ifelse(country.and.year %in% c("UPPER VOLTA (see BURKINA FASO)"),
                                          NA, country.and.year)) %>%
  
  # remove blank columns
  dplyr::select(-dplyr::contains("blank"))

# remove blank rows
wmeat.1984.data <- wmeat.1984.data[rowSums(is.na(wmeat.1984.data)) != ncol(wmeat.1984.data),]

# list all countries in the dataset
wmeat.1984.country.list <- wmeat.1984.data %>%
  dplyr::filter(country.and.year %!in% c(1973:1983,NA)) %>%
  dplyr::pull(country.and.year)

# add placeholder column for country name
wmeat.1984.data <- wmeat.1984.data %>%
  dplyr::mutate(country = NA)

for(i in 1:(length(wmeat.1984.country.list)-1)){
  
  # pull the row with ith country header
  i.row <- which(wmeat.1984.data$country.and.year == wmeat.1984.country.list[i])
  
  # pull the row with the (i+1)th country header
  iplus1.row <- which(wmeat.1984.data$country.and.year == wmeat.1984.country.list[i+1])
  
  wmeat.1984.data$country[c((i.row+1):(iplus1.row-1))] <- wmeat.1984.country.list[i]
  
}

# code country for the last entry on the list
i.row.zwe <- which(wmeat.1984.data$country.and.year == "ZIMBABWE")
wmeat.1984.data$country[c((i.row.zwe+1):nrow(wmeat.1984.data))] <- "ZIMBABWE"

wmeat.1984.data <- wmeat.1984.data %>%
  # filter out country header rows
  tidyr::drop_na(country) %>%
  dplyr::rename(year = country.and.year) %>%
  dplyr::select(country, year, military.expenditure.current.dollars, armed.personnel, gdp.current.dollars, population) %>%
  
  # convert to full values
  dplyr::mutate(military.expenditure.current.dollars = as.numeric(military.expenditure.current.dollars) * 1000000,
                armed.personnel = as.numeric(armed.personnel) * 1000,
                gdp.current.dollars = as.numeric(gdp.current.dollars) * 1000000,
                population = as.numeric(population) * 1000000) %>%
  tidyr::pivot_longer(3:6, names_to = "variable", values_to = "value") %>%
  
  dplyr::mutate(
    # add WMEAT version year
    version = 1984,
    # using the countrycode package, add iso3c code based on country name
    iso3c = countrycode::countrycode(country,"country.name","iso3c"),
    # add missing iso3c codes
    iso3c = dplyr::case_when(
      country == "CZECHOSLOVAKIA" ~ "CZE",
      country == "GERMANY, EAST" ~ "DDR",
      country == "GERMANY, WEST" ~ "BRD",
      country == "VIETNAM, SOUTH" ~ "RVN",
      country == "YEMEN (ADEN)" ~ "YPR",
      country == "YEMEN (SANAA)" ~ "YAR",
      country == "YUGOSLAVIA" ~ "YUG",
      .default = iso3c
    )) %>%
  
  dplyr::select(-country) %>%
  dplyr::mutate(
    # using the countrycode package, add country name based on iso3c code
    country = countrycode::countrycode(iso3c,"iso3c","country.name"),
    # add missing country names
    country = dplyr::case_when(
      iso3c=="BRD" ~ "West Germany",
      iso3c=="DDR" ~ "East Germany",
      iso3c=="RVN" ~ "South Vietnam",
      iso3c=="YAR" ~ "North Yemen",
      iso3c=="YPR" ~ "South Yemen",
      iso3c=="YUG" ~ "Yugoslavia",
      .default     = country
    ))

##### WMEAT 1995 ----------------------------------------------------------------------
wmeat.1995.data <- wmeat.1995

names(wmeat.1995.data) <- c(
  "country.and.year",
  "military.expenditure.current.dollars", # in millions
  "blank1",
  "military.expenditure.1994.dollars", # in millions
  "blank2",
  "armed.personnel", #in thousands
  "blank3",
  "gdp.current.dollars", # in millions; gnp, not gdp
  "blank4",
  "gdp.1994.dollars", # in millions; gnp, not gdp
  "blank5",
  "central.gov.expenditure.1994.dollars", # in millions
  "blank6",
  "population", # in millions
  "blank7",
  "mil.expenditure.over.gdp", # percentage; gnp, not gdp
  "blank8",
  "mil.expenditure.over.central.gov.expenditure", # percentage
  "blank9",
  "mil.expenditure.per.capita", # 1994 dollars
  "blank10",
  "armed.forces.per.1000.pop",
  "blank11",
  "gdp.per.capita", # 1994 dollars; gnp, not gdp
  "blank12"
  )

wmeat.1995.data <- wmeat.1995.data %>%
  dplyr::mutate(
    # remove letters from year variable
    country.and.year = dplyr::case_when(
      country.and.year == "1989g" ~ "1989",
      country.and.year == "1990e" ~ "1990",
      country.and.year == "1990g" ~ "1990",
      country.and.year == "1991e" ~ "1991",
      .default = country.and.year
    ),
    
    # adjust China notations
    country.and.year = dplyr::case_when(
      country.and.year == "China" ~ NA,
      country.and.year == "-Mainland" ~ "China",
      country.and.year == "-Taiwan" ~ "Taiwan",
      .default = country.and.year
  ),
    
  # convert "see X" entries to NAs
  country.and.year = ifelse(country.and.year %in% c("Cote d'Ivoire (see Ivory Coast)",
                                                    "Kampuchea (see Cambodia)",
                                                    "Myanmar (see Burma)",
                                                    "Upper Volta (see Burkina Faso)"),
                            NA, country.and.year)) %>%
  
  # remove blank columns
  dplyr::select(-dplyr::contains("blank"))

# remove blank rows
wmeat.1995.data <- wmeat.1995.data[rowSums(is.na(wmeat.1995.data)) != ncol(wmeat.1995.data),]

# list all countries in the dataset
wmeat.1995.country.list <- wmeat.1995.data %>%
  dplyr::filter(country.and.year %!in% c(1984:1994,NA)) %>%
  dplyr::pull(country.and.year)

# add placeholder column for country name
wmeat.1995.data <- wmeat.1995.data %>%
  dplyr::mutate(country = NA)

for(i in 1:(length(wmeat.1995.country.list)-1)){
  
  # pull the row with ith country header
  i.row <- which(wmeat.1995.data$country.and.year == wmeat.1995.country.list[i])
  
  # pull the row with the (i+1)th country header
  iplus1.row <- which(wmeat.1995.data$country.and.year == wmeat.1995.country.list[i+1])
  
  wmeat.1995.data$country[c((i.row+1):(iplus1.row-1))] <- wmeat.1995.country.list[i]
  
}

# code country for the last entry on the list
i.row.zwe <- which(wmeat.1995.data$country.and.year == "Zimbabwe")
wmeat.1995.data$country[c((i.row.zwe+1):nrow(wmeat.1995.data))] <- "Zimbabwe"

# filter out country header rows
wmeat.1995.data <- wmeat.1995.data %>%
  tidyr::drop_na(country) %>%
  dplyr::rename(year = country.and.year) %>%
  dplyr::select(country, year, military.expenditure.current.dollars, armed.personnel, gdp.current.dollars, population) %>%
  # convert to full values
  dplyr::mutate(military.expenditure.current.dollars = as.numeric(military.expenditure.current.dollars) * 1000000,
                armed.personnel = as.numeric(armed.personnel) * 1000,
                gdp.current.dollars = as.numeric(gdp.current.dollars) * 1000000,
                population = as.numeric(population) * 1000000) %>%
  tidyr::pivot_longer(3:6, names_to = "variable", values_to = "value") %>%
  dplyr::mutate(
    # add WMEAT version year
    version = 1995,
    # using the countrycode package, add iso3c code based on country name
    iso3c = countrycode::countrycode(country,"country.name","iso3c"),
    # add missing iso3c codes
    iso3c = dplyr::case_when(
      country == "Czechoslovakia" ~ "CZE",
      country == "Germany, East" ~ "DDR",
      country == "Germany, West" & year <= 1989 ~ "BRD",
      country == "Germany, West" & year >= 1990 ~ "DEU",
      country == "Serbia and Montenegro" ~ "SRB",
      country == "Yemen (Aden)" ~ "YPR",
      country == "Yemen (Sanaa)" ~ "YAR",
      country == "Yugoslavia" ~ "YUG",
      .default = iso3c
    )) %>%

  dplyr::select(-country) %>%
  dplyr::mutate(
    # using the countrycode package, add country name based on iso3c code
    country = countrycode::countrycode(iso3c,"iso3c","country.name"),
    # add missing country names
    country = dplyr::case_when(
      iso3c == "BRD" ~ "West Germany",
      iso3c == "DDR" ~ "East Germany",
      iso3c == "YAR" ~ "North Yemen",
      iso3c == "YPR" ~ "South Yemen",
      iso3c == "YUG" ~ "Yugoslavia",
      .default = country
    ))

##### WMEAT 2005 ----------------------------------------------------------------------
# all the data needed is on Sheet 2- "By Country"
wmeat.2005.data <- wmeat.2005[[2]]

names(wmeat.2005.data) <- c(
  "country.and.year",
  "military.expenditure.current.dollars", # in millions
  "blank1",
  "military.expenditure.2005.dollars", # in millions
  "blank2",
  "armed.personnel", #in thousands
  "blank3",
  "gdp.current.dollars", # in millions
  "blank4",
  "gdp.2005.dollars", # in millions
  "blank5",
  "central.gov.expenditure.2005.dollars", # in millions
  "blank6",
  "population", # in millions
  "blank7",
  "mil.expenditure.over.armed.forces", # 2005 dollars
  "blank8",
  "mil.expenditure.over.gdp", # percentage
  "blank9",
  "mil.expenditure.over.central.gov.expenditure", # percentage
  "blank10",
  "mil.expenditure.per.capita", # 2005 dollars
  "blank11",
  "armed.forces.per.1000.pop",
  "blank12",
  "gdp.per.capita", # 2005 dollars
  "blank13"
  )

# remove blank columns
wmeat.2005.data <- wmeat.2005.data %>%
  dplyr::select(-dplyr::contains("blank"))

# remove blank rows
wmeat.2005.data <- wmeat.2005.data[rowSums(is.na(wmeat.2005.data)) != ncol(wmeat.2005.data),]

# fix country name in incorrect column
wmeat.2005.data$country.and.year[wmeat.2005.data$military.expenditure.current.dollars=="Sao Tome and Principe"] <- "Sao Tome and Principe"

# list all countries in the dataset
wmeat.2005.country.list <- wmeat.2005.data %>%
  dplyr::filter(country.and.year %!in% c(1995:2005,NA)) %>%
  dplyr::pull(country.and.year)

# add placeholder column for country name
wmeat.2005.data <- wmeat.2005.data %>%
  dplyr::mutate(country = NA)

for(i in 1:(length(wmeat.2005.country.list)-1)){
  
  # pull the row with ith country header
  i.row <- which(wmeat.2005.data$country.and.year == wmeat.2005.country.list[i])
  
  # pull the row with the (i+1)th country header
  iplus1.row <- which(wmeat.2005.data$country.and.year == wmeat.2005.country.list[i+1])
  
  wmeat.2005.data$country[c((i.row+1):(iplus1.row-1))] <- wmeat.2005.country.list[i]
 
}

# code country for the last entry on the list
i.row.zwe <- which(wmeat.2005.data$country.and.year == "Zimbabwe")
wmeat.2005.data$country[c((i.row.zwe+1):nrow(wmeat.2005.data))] <- "Zimbabwe"

# filter out country header rows
wmeat.2005.data <- wmeat.2005.data %>%
  tidyr::drop_na(country) %>%
  dplyr::rename(year = country.and.year) %>%
  dplyr::select(country, year, military.expenditure.current.dollars, armed.personnel, gdp.current.dollars, population) %>%
  # convert to full values
  dplyr::mutate(military.expenditure.current.dollars = as.numeric(military.expenditure.current.dollars) * 1000000,
                armed.personnel = as.numeric(armed.personnel) * 1000,
                gdp.current.dollars = as.numeric(gdp.current.dollars) * 1000000,
                population = as.numeric(population) * 1000000) %>%
  tidyr::pivot_longer(3:6, names_to = "variable", values_to = "value") %>%
  
  dplyr::mutate(
    # add WMEAT version year
    version = 2005,
    # using the countrycode package, add iso3c code based on country name
    iso3c = countrycode::countrycode(country,"country.name","iso3c"),
    # add missing iso3c codes
    iso3c = dplyr::case_when(
      country == "New  Zealand" ~ "NZL",
      country == "Serbia and Montenegro" ~ "SRB",
      country == "Yemen (Sanaa)" ~ "YEM", # Note data only covers time post-Yemeni unification
      .default = iso3c
    )) %>%

  dplyr::select(-country) %>%
  # using the countrycode package, add country name based on iso3c code
  dplyr::mutate(country = countrycode::countrycode(iso3c,"iso3c","country.name"))

##### WMEAT 2012 ----------------------------------------------------------------------
names(wmeat.2012) <- readxl::excel_sheets("Data files/Raw data files/209509.xlsx")

# list of country tabs
wmeat.2012.tab.list <- readxl::excel_sheets("Data files/Raw data files/209509.xlsx")
wmeat.2012.tab.list <- wmeat.2012.tab.list[wmeat.2012.tab.list %!in% c("Table of Contents","Overview","Geographic Groups","Geog. Groups 2",
                                                                       "Political Groups","Economic Groups","Group Rankings & Trends",
                                                                       "Country Rankings & Trends")]

wmeat.2012.data <- data.frame()

# format each country in the wmeat dataset list in long format and add to placeholder dataframe
for(c in wmeat.2012.tab.list){
  
  df <- wmeat.2012[[c]]
  
  # remove blank rows
  df <- df[rowSums(is.na(df)) != ncol(df),]
  
  # select variables of interest
  ## Armed forces personnel (AF) (in thousands) and Military expenditure (ME) - Current dollars (millions) are variables of interest
  ## Population (midyear, in millions) and Gross domestic product (GDP) - Current dollars (millions) are comparisons to test
  ## the validity of population and GDP data
  df <- df[c(2,3,7,12),] %>%
    dplyr::select(-Mean)
  
  # rename variables
  df$`Parameter / Year`[1] <- "armed.personnel" # in thousands
  df$`Parameter / Year`[2] <- "population" # in millions
  df$`Parameter / Year`[3] <- "military.expenditure.current.dollars" # in millions
  df$`Parameter / Year`[4] <- "gdp.current.dollars" # in millions
  
  # convert all variables besides variable name to numeric
  df <- df %>%
    dplyr::mutate(dplyr::across(c(2:ncol(df)), as.numeric))
  
  df <- df %>%
    # convert to long data
    tidyr::pivot_longer(2:12, names_to = "year", values_to = "value") %>%
    dplyr::rename(variable = "Parameter / Year") %>%
    dplyr::mutate(
      # convert to full value - multiply by 1,000 if variable is armed.personnel, otherwise multiply by 1,000,000
      value = ifelse(variable == "armed.personnel",
                     value * 1000,
                     value * 1000000),
      country = c,
      # using the countrycode package, add iso3c code based on country name
      iso3c = countrycode::countrycode(country,"country.name","iso3c")
      ) %>%

    # reorder variables
    dplyr::select(iso3c,country,year,variable,value)
  
  # manually code countries where countrycode did not identify the name used in the WMEAT file
  df$iso3c[df$country=="Cent. Afr. Rep."] <- "CAF"
  df$iso3c[df$country=="Kosovo"] <- "KSV"
  df$iso3c[df$country=="Timor l`Este"] <- "TLS"
  
  wmeat.2012.data <- rbind(wmeat.2012.data, df)
  
}

# add WMEAT version year
wmeat.2012.data <- wmeat.2012.data %>%
  dplyr::mutate(version = 2012)

##### WMEAT 2016 ----------------------------------------------------------------------
names(wmeat.2016) <- readxl::excel_sheets("Data files/Raw data files/266013.xlsx")

# list of country tabs
wmeat.2016.tab.list <- readxl::excel_sheets("Data files/Raw data files/266013.xlsx")
wmeat.2016.tab.list <- wmeat.2016.tab.list[wmeat.2016.tab.list %!in% c("Table of Contents","Overview","Geographic Groups","Geog. Groups 2",
                                                                       "Political Groups","Economic Groups","Group Rankings & Trends",
                                                                       "Country Rankings & Trends","Charts")]

wmeat.2016.data <- data.frame()

# format each country in the wmeat dataset list in long format and add to placeholder dataframe
for(c in wmeat.2016.tab.list){
  
  df <- wmeat.2016[[c]]
  
  # remove blank rows
  df <- df[rowSums(is.na(df)) != ncol(df),]
  
  # select variables of interest
  ## Armed forces personnel (AF) (in thousands) and Military expenditure (ME) - Current dollars (millions) are variables of interest
  ## Population (midyear, in millions) and Gross domestic product (GDP) - Current dollars (millions) are comparisons to test
  ## the validity of population and GDP data
  df <- df[c(2,5,29,34),] %>%
    dplyr::select(-Mean)
  
  # rename variables
  df$`Parameter / Year`[1] <- "armed.personnel" # in thousands
  df$`Parameter / Year`[2] <- "population" # in millions
  df$`Parameter / Year`[3] <- "military.expenditure.current.dollars" # in millions
  df$`Parameter / Year`[4] <- "gdp.current.dollars" # in millions
  
  # convert all variables besides variable name to numeric
  df <- df %>%
    dplyr::mutate(dplyr::across(c(2:ncol(df)), as.numeric))
  
  df <- df %>%
    # convert to long data
    tidyr::pivot_longer(2:12, names_to = "year", values_to = "value") %>%
    dplyr::rename(variable = "Parameter / Year") %>%
    dplyr::mutate(
      # convert to full value - multiply by 1,000 if variable is armed.personnel, otherwise multiply by 1,000,000
      value = ifelse(variable == "armed.personnel",
                     value * 1000,
                     value * 1000000),
      country = c,
      # using the countrycode package, add iso3c code based on country name
      iso3c = countrycode::countrycode(country,"country.name","iso3c")
      ) %>%
    
    # reorder variables
    dplyr::select(iso3c,country,year,variable,value)
  
  # manually code countries where countrycode did not identify the name used in the WMEAT file
  df$iso3c[df$country=="Cent. Afr. Rep."] <- "CAF"
  df$iso3c[df$country=="Kosovo"] <- "KSV"
  df$iso3c[df$country=="S. Sudan"] <- "SSD"
  df$iso3c[df$country=="Timor l`Este"] <- "TLS"
  
  wmeat.2016.data <- rbind(wmeat.2016.data, df)
  
}

# add WMEAT version year
wmeat.2016.data <- wmeat.2016.data %>%
  dplyr::mutate(version = 2016)

##### WMEAT 2019 ----------------------------------------------------------------------
names(wmeat.2019) <- readxl::excel_sheets("Data files/Raw data files/WMEAT-2019-Table-I-Military-Expenditures-and-Armed-Forces-Personnel-2007-2017.xlsx")

# list of country tabs
wmeat.2019.tab.list <- readxl::excel_sheets("Data files/Raw data files/WMEAT-2019-Table-I-Military-Expenditures-and-Armed-Forces-Personnel-2007-2017.xlsx")
wmeat.2019.tab.list <- wmeat.2019.tab.list[wmeat.2019.tab.list %!in% c("Table of Contents","Overview","Geographic Groups","Geog. Groups 2",
                                                                       "Political Groups","Economic Groups","Group Rankings & Trends",
                                                                       "Country Rankings & Trends","Charts")]

wmeat.2019.data <- data.frame()

# format each country in the wmeat dataset list in long format and add to placeholder dataframe
for(c in wmeat.2019.tab.list){
  
  df <- wmeat.2019[[c]]
  
  # remove blank rows
  df <- df[rowSums(is.na(df)) != ncol(df),]
  
  # select variables of interest
  ## Armed forces personnel (AF) (in thousands) and Military expenditure (ME) - Current dollars (millions) are variables of interest
  ## Population (midyear, in millions) and Gross domestic product (GDP) - Current dollars (millions) are comparisons to test
  ## the validity of population and GDP data
  df <- df[c(2,5,29,34),] %>%
    dplyr::select(-Mean)
  
  # rename variables
  df$`Parameter / Year`[1] <- "armed.personnel" # in thousands
  df$`Parameter / Year`[2] <- "population" # in millions
  df$`Parameter / Year`[3] <- "military.expenditure.current.dollars" # in millions
  df$`Parameter / Year`[4] <- "gdp.current.dollars" # in millions
  
  # convert all variables besides variable name to numeric
  df <- df %>%
    dplyr::mutate(dplyr::across(c(2:ncol(df)), as.numeric))
  
  df <- df %>%
    # convert to long data
    tidyr::pivot_longer(2:12, names_to = "year", values_to = "value") %>%
    dplyr::rename(variable = "Parameter / Year") %>%
    dplyr::mutate(
      # convert to full value - multiply by 1,000 if variable is armed.personnel, otherwise multiply by 1,000,000
      value = ifelse(variable == "armed.personnel",
                     value * 1000,
                     value * 1000000),
      country = c,
      # using the countrycode package, add iso3c code based on country name
      iso3c = countrycode::countrycode(country,"country.name","iso3c")
      ) %>%
    
    # reorder variables
    dplyr::select(iso3c,country,year,variable,value)
   
  # manually code countries where countrycode did not identify the name used in the WMEAT file
  df$iso3c[df$country=="Cent. Afr. Rep."] <- "CAF"
  df$iso3c[df$country=="Kosovo"] <- "KSV"
  df$iso3c[df$country=="S. Sudan"] <- "SSD"
  df$iso3c[df$country=="Timor l`Este"] <- "TLS"
     
  wmeat.2019.data <- rbind(wmeat.2019.data, df)
  
}

# add WMEAT version year
wmeat.2019.data <- wmeat.2019.data %>%
  dplyr::mutate(version = 2019)

##### merge WMEAT datasets ----------------------------------------------------------------------
wmeat.data <- rbind(wmeat.2019.data,wmeat.2016.data,wmeat.2012.data,wmeat.2005.data,
                    wmeat.1995.data,wmeat.1984.data,wmeat.1973.data) %>%
  dplyr::mutate(year = as.numeric(year)) %>%
  dplyr::left_join(inflation_table,by="year") %>%
  
  # adjust for inflation
  dplyr::mutate(
    value = ifelse(
      variable %in% c("gdp.current.dollars","military.expenditure.current.dollars"),
      value * multiplier,
      value)
    ) %>%
  
  dplyr::mutate(variable = case_when(
    variable == "military.expenditure.current.dollars" ~ "military.expenditure.wmeat",
    variable == "gdp.current.dollars" ~ "gdp.wmeat",
    variable == "population" ~ "population.wmeat",
    variable == "armed.personnel" ~ "armed.personnel.wmeat",
    .default = variable
  )) %>%
  tidyr::drop_na(value) %>%
  
  # keep the most recent published version of the data
  dplyr::group_by(iso3c,year,variable) %>%
  dplyr::filter(version == max(version,na.rm=TRUE)) %>%
  dplyr::ungroup() %>%
  
  dplyr::select(-c(multiplier,version,country)) %>%
  tidyr::pivot_wider(names_from = "variable", values_from = "value") %>%
  
  dplyr::mutate(iso3c = ifelse(iso3c=="RUS"&year %in% c(1946:1990),"SOV",iso3c)) %>%
  dplyr::mutate(iso3c = ifelse(iso3c=="YAR"&year>1990,"YEM",iso3c))

### merge datasets ----------------------------------------------------------------------
mildata <- dplyr::full_join(cow,military.balance.data,by=c("iso3c","year")) %>%
  dplyr::full_join(sipri,by=c("iso3c","year")) %>%
  dplyr::full_join(wmeat.data,by=c("iso3c","year")) %>%
  
  dplyr::mutate(
    # set COW data as default expenditure and personnel values for COW estimates
    mil.expenditure.cow = milex.cow,
    mil.personnel.cow = milper.cow,
    
    # for missing COW estimate data, add in IISS data
    #mil.expenditure.cow = dplyr::coalesce(mil.expenditure.cow,defense.spending.iiss),
    #mil.personnel.cow = dplyr::coalesce(mil.personnel.cow,active.armed.forces.iiss),
    
    # set WMEAT data as default expenditure and personnel values for WMEAT estimates
    mil.expenditure.wmeat = military.expenditure.wmeat,
    mil.personnel.wmeat = armed.personnel.wmeat,
    
    # set SIPRI data as default expenditure for SIPRI estimates
    mil.expenditure.sipri = milexp.sipri,
    
    # using the countrycode package, add country name based on iso3c code
    country = countrycode::countrycode(iso3c,"iso3c","country.name"),
    # add country names for missing iso3c codes
    country = dplyr::case_when(
      iso3c=="BRD" ~ "West Germany",
      iso3c=="DDR" ~ "East Germany",
      iso3c=="KSV" ~ "Kosovo",
      iso3c=="RVN" ~ "South Vietnam",
      iso3c=="SOV" ~ "Soviet Union",
      iso3c=="YAR" ~ "North Yemen",
      iso3c=="YPR" ~ "South Yemen",
      iso3c=="YUG" ~ "Yugoslavia",
      iso3c=="ZAN" ~ "Zanzibar",
      .default = country
    )) %>%
  
  dplyr::relocate(country, .after = iso3c) %>%
  dplyr::relocate(mil.expenditure.cow, .after = year) %>%
  dplyr::relocate(mil.personnel.cow, .after = mil.expenditure.cow) %>%
  dplyr::relocate(mil.expenditure.wmeat, .after = mil.personnel.cow) %>%
  dplyr::relocate(mil.personnel.wmeat, .after = mil.expenditure.wmeat) %>%
  dplyr::relocate(mil.expenditure.sipri, .after = mil.personnel.wmeat)

### military metrics estimator functions ----------------------------------------------------------------------
mil_expenditure_growth_estimator_func <- function(df = mildata, col_missing, col_ref, iso, yr, restricted = c(2013:2019)){

  # pull the baseline expenditure for the reference year for the missing data column
  baseline <- df %>%
    dplyr::filter(iso3c == iso,
                  year == yr) %>%
    dplyr::pull(.data[[col_missing]])
  
  # pull the relative expenditure for the reference year for the reference column
  relative <- df %>%
    dplyr::filter(iso3c == iso,
                  year == yr) %>%
    dplyr::pull(.data[[col_ref]])
  
  df <- df %>%
    dplyr::mutate(
      # calculate estimate for the missing column based on the proportion between the year and the relative
      # value of the reference column
      prop = baseline * .data[[col_ref]] / relative,
      # fill in the estimates in the col_missing variable, based on the restricted variable
      !!col_missing := dplyr::case_when(
        iso3c == iso & year %in% restricted ~ prop,
        .default = .data[[col_missing]]
      )) %>%
    dplyr::select(-prop)
  
  return(df)
  
}

# this function approximates a COW military expenditure gap of one year using IISS or WMEAT data, calculating the distance the gap year
# value in the reference data is from the preceding and proceding years and applying it to the COW data. This method only works when
# both data series are trending similarly.
mil_expenditure_distance_gap_estimator_func <- function(df = mildata, col_missing, col_ref = "IISS", iso, yrs, estimator = "IISS",
                                                            yr.minus.1 = NA, yr.plus.1 = NA){
  
  # sets year - 1 and year + 1 to those default years if a different reference year is not selected
  if(is.na(yr.minus.1)){
    yr.minus.1 <- min(yrs, na.rm = TRUE) - 1
  }
  if(is.na(yr.plus.1)){
    yr.plus.1 <- max(yrs, na.rm = TRUE) + 1
  }
  
  # selects which column to apply proportions to
  missing.column <- dplyr::case_when(
    col_missing == "COW" ~ "mil.expenditure.cow",
    col_missing == "COW alt" ~ "mil.expenditure.cow.alt",
    col_missing == "WMEAT" ~ "mil.expenditure.wmeat",
    col_missing == "SIPRI" ~ "mil.expenditure.sipri",
    col_missing == "COW personnel" ~ "mil.personnel.cow",
    col_missing == "WMEAT personnel" ~ "mil.personnel.wmeat",
    .default = "mil.expenditure.cow"
  )
  
  # selects which column to calculate the proportions from
  estimator.column <- dplyr::case_when(
    col_ref == "COW" ~ "milex.cow",
    col_ref == "COW est" ~ "mil.expenditure.cow",
    col_ref == "COW est alt" ~ "mil.expenditure.cow.alt",
    col_ref == "IISS" ~ "defense.spending.iiss",
    col_ref == "WMEAT" ~ "military.expenditure.wmeat",
    col_ref == "WMEAT est" ~ "mil.expenditure.wmeat",
    col_ref == "SIPRI" ~ "milexp.sipri",
    col_ref == "SIPRI est" ~ "mil.expenditure.sipri",
    col_ref == "COW personnel" ~ "mil.personnel.cow",
    col_ref == "WMEAT personnel" ~ "mil.personnel.wmeat",
    .default = "defense.spending.iiss"
  )
  
  # pulls estimates for yr.minus.1 and yr.plus.1 from the estimator data
  est.expenditure.yearminus1 <- df %>%
    dplyr::filter(iso3c == iso,
                  year == yr.minus.1) %>%
    dplyr::pull(all_of(estimator.column))
  
  est.expenditure.yearplus1 <- df %>%
    dplyr::filter(iso3c == iso,
                  year == yr.plus.1) %>%
    dplyr::pull(all_of(estimator.column))
  
  # pulls estimates for yr.minus.1 and yr.plus.1 from the missing data
  missing.expenditure.yearminus1 <- df %>%
    dplyr::filter(iso3c == iso,
                  year == yr.minus.1) %>%
    dplyr::pull(all_of(missing.column))
  
  missing.expenditure.yearplus1 <- df %>%
    dplyr::filter(iso3c == iso,
                  year == yr.plus.1) %>%
    dplyr::pull(all_of(missing.column))
  
  estimate.vector <- array()
  
  for(a in yrs){
    
    # pull estimates for year a from the estimator data
    est.expenditure.year <- df %>%
      dplyr::filter(iso3c == iso,
                    year == a) %>%
      dplyr::pull(all_of(estimator.column))
    
    # calculate estimate for year a
    est <- (
      (est.expenditure.year - est.expenditure.yearplus1)*
        (missing.expenditure.yearminus1 - missing.expenditure.yearplus1)/
        (est.expenditure.yearminus1 - est.expenditure.yearplus1)
      ) +
      missing.expenditure.yearplus1
    
    estimate.vector <- c(estimate.vector,est)
    
  }
  
  # remove first value (NA) from estimate.vector
  estimate.vector <- estimate.vector[-1]

  return(estimate.vector)
  
}

# this function approximates a COW military expenditure gap of one year using IISS or WMEAT data, calculating the percent change
# from the preceding and proceding years and applying the proportions to the COW values, returning the average of the estimates.
mil_expenditure_gap_estimator_func <- function(df = mildata, col_missing, col_ref = "IISS", iso, yrs,
                                               estimator = "IISS", yr.minus.1 = NA, yr.plus.1 = NA,
                                               wgt.minus = 1, wgt.plus = 1){
  
  # sets year - 1 and year + 1 to those default years if a different reference year is not selected
  if(is.na(yr.minus.1)){
    yr.minus.1 <- min(yrs, na.rm = TRUE) - 1
  }
  if(is.na(yr.plus.1)){
    yr.plus.1 <- max(yrs, na.rm = TRUE) + 1
  }
  
  # selects which column to apply proportions to
  missing.column <- dplyr::case_when(
    col_missing == "COW" ~ "mil.expenditure.cow",
    col_missing == "COW alt" ~ "mil.expenditure.cow.alt",
    col_missing == "WMEAT" ~ "mil.expenditure.wmeat",
    col_missing == "SIPRI" ~ "mil.expenditure.sipri",
    col_missing == "COW personnel" ~ "mil.personnel.cow",
    col_missing == "WMEAT personnel" ~ "mil.personnel.wmeat",
    .default = "mil.expenditure.cow"
  )
  
  # selects which column to calculate the proportions from
  estimator.column <- dplyr::case_when(
    col_ref == "COW" ~ "milex.cow",
    col_ref == "COW est" ~ "mil.expenditure.cow",
    col_ref == "COW est alt" ~ "mil.expenditure.cow.alt",
    col_ref == "IISS" ~ "defense.spending.iiss",
    col_ref == "WMEAT" ~ "military.expenditure.wmeat",
    col_ref == "WMEAT est" ~ "mil.expenditure.wmeat",
    col_ref == "SIPRI" ~ "milexp.sipri",
    col_ref == "SIPRI est" ~ "mil.expenditure.sipri",
    col_ref == "COW personnel" ~ "mil.personnel.cow",
    col_ref == "WMEAT personnel" ~ "mil.personnel.wmeat",
    .default = "defense.spending.iiss"
  )
  
  # pulls estimates for yr.minus.1 and yr.plus.1 from the estimator data
  est.expenditure.yearminus1 <- df %>%
    dplyr::filter(iso3c == iso,
                  year == yr.minus.1) %>%
    dplyr::pull(all_of(estimator.column))
  
  est.expenditure.yearplus1 <- df %>%
    dplyr::filter(iso3c == iso,
                  year == yr.plus.1) %>%
    dplyr::pull(all_of(estimator.column))
  
  # pulls estimates for yr.minus.1 and yr.plus.1 from the missing data
  missing.expenditure.yearminus1 <- df %>%
    dplyr::filter(iso3c == iso,
                  year == yr.minus.1) %>%
    dplyr::pull(all_of(missing.column))
  
  missing.expenditure.yearplus1 <- df %>%
    dplyr::filter(iso3c == iso,
                  year == yr.plus.1) %>%
    dplyr::pull(all_of(missing.column))

  estimate.vector <- array()
  
  for(a in yrs){
    
    # pull estimates for year a from the estimator data
    est.expenditure.year <- df %>%
      dplyr::filter(iso3c == iso,
                    year == a) %>%
      dplyr::pull(all_of(estimator.column))
    
    # calculate percentage differences in reference data
    est.ratio.yearminus1 <- est.expenditure.year / est.expenditure.yearminus1
    est.ratio.yearplus1 <- est.expenditure.year / est.expenditure.yearplus1
    
    # calculate COW estimates based on percentages
    missing.est.yearminus1 <- missing.expenditure.yearminus1 * est.ratio.yearminus1
    missing.est.yearplus1 <- missing.expenditure.yearplus1 * est.ratio.yearplus1
    
    # average estimates
    missing.est.average <- (wgt.minus * missing.est.yearminus1 + wgt.plus * missing.est.yearplus1)/(wgt.minus+wgt.plus)
    
    estimate.vector <- c(estimate.vector,missing.est.average)
    
  }
  
  # remove first value (NA) from estimate.vector
  estimate.vector <- estimate.vector[-1]
  
  return(estimate.vector)
  
}

# lm estimator
mil_estimator_lm_func <- function(df = mildata, iso, est_variable, model_variables, model_data_years, predict_years){
  
  df2 <- df %>%
    dplyr::filter(iso3c == iso) %>%
    dplyr::arrange(year)
  
  df3 <- df2 %>%
    dplyr::mutate(year = year + 1)
  
  names(df3)[4:ncol(df3)] <- paste0(names(df3)[4:ncol(df3)],".plus1")
  
  df2 <- df2 %>%
    dplyr::left_join(df3, by = c("iso3c", "country", "year")) %>%
    dplyr::mutate(
      # military expenditure
      mil.expenditure.growth.rate.cow = 100 * (mil.expenditure.cow - mil.expenditure.cow.plus1) / mil.expenditure.cow.plus1,
      mil.expenditure.growth.rate.cow.alt = 100 * (mil.expenditure.cow.alt - mil.expenditure.cow.alt.plus1) / mil.expenditure.cow.alt.plus1,
      mil.expenditure.growth.rate.wmeat = 100 * (mil.expenditure.wmeat - mil.expenditure.wmeat.plus1) / mil.expenditure.wmeat.plus1,
      mil.expenditure.growth.rate.sipri = 100 * (mil.expenditure.sipri - mil.expenditure.sipri.plus1) / mil.expenditure.sipri.plus1,
      # military personnel
      mil.personnel.growth.rate.cow = 100 * (mil.personnel.cow - mil.personnel.cow.plus1) / mil.personnel.cow.plus1,
      mil.personnel.growth.rate.wmeat = 100 * (mil.personnel.wmeat - mil.personnel.wmeat.plus1) / mil.personnel.wmeat.plus1
    )
  
  model_years_running <- model_data_years
  
  for(yr in predict_years){
    
    df_model_years <- df2 %>%
      dplyr::filter(year %in% model_years_running) %>%
      dplyr::select(all_of(est_variable),all_of(model_variables))
    # row number (for weighting)
    # dplyr::mutate(wgt = dplyr::case_when(
    #   weights == TRUE ~ row_number(),
    #   weights == FALSE ~ 1
    #   )
    
    # wgt_list <- df_model_years %>%
    #   dplyr::pull(wgt)
    # 
    # df_model_years <- df_model_years %>%
    #   dplyr::select(-c(wgt))
    
    df_model_years <- df_model_years %>%
      dplyr::rename(est_var = which(names(df_model_years)==est_variable))
    
    df_predict_year <- df2 %>%
      dplyr::filter(year == yr)
    
    df_lm <- stats::lm(
      est_var ~ . , df_model_years
    )
    
    pred_val <- predict.lm(df_lm, df_predict_year, type = "response")
    
    # add to df, df2 and model_years_running
    model_years_running <- c(model_years_running,pred_val)
    
    df2[[est_variable]][df2$year==yr] <- pred_val
    df[[est_variable]][df$iso3c==iso&df$year==yr] <- pred_val
    
  }
  
  return(df)
  
}


# arima
mil_estimator_arima_func <- function(df = mildata, iso, metric = "mil.expenditure.cow", direction = "forwards", start.est.yr = 2018, end.est.yr = 2019){
  
  original_data <- df %>%
    dplyr::arrange(year) %>%
    dplyr::filter(
      iso3c == iso,
      year %!in% c(start.est.yr:end.est.yr,1945,2020)) %>%
    dplyr::pull(metric)
  
  num_of_estimates <- abs(end.est.yr - start.est.yr) + 1
  
  data0 <- original_data
  
  if(direction == "backwards"){

    data0 <- data0 %>%
      rev()
    
  }
  
  for(i in start.est.yr:end.est.yr){
    
    arima.i <- stats::arima(data0, order = c(1,1,1))
    pred <- as.numeric(predict(arima.i, 1)$pred)
    
    data0 <- c(data0,pred)
    df <- df %>%
      dplyr::mutate(
        !!metric := ifelse(iso3c == iso&year == i,pred,.data[[metric]])
      )
  }
  
  return(df)
  
}

# this function approximates the military expenditure or personnel of a country with no comparable data based on its growth
# rates the prior/subsequent years.
# the function applies weighted growth rates of 1/2 year+1, 1/3 year+2, and 1/6 year+3
mil_estimator_no_data_func <- function(df = mildata, iso, metric, startyr = 1949, endyr = 1946, dir = NA){
  
  for(y in startyr:endyr){
    
    # calculate which direction the years with data go
    if(!is.na(dir)){
      if(dir == "lower"){
      
        year1 <- y - 1
        year2 <- y - 2
        year3 <- y - 3
        year4 <- y - 4
    
      } else if(dir == "higher"){
      
        year1 <- y + 1
        year2 <- y + 2
        year3 <- y + 3
        year4 <- y + 4
      
        }
      } else if(startyr > endyr){
      
      year1 <- y + 1
      year2 <- y + 2
      year3 <- y + 3
      year4 <- y + 4
      
    } else if(startyr < endyr){
      
      year1 <- y - 1
      year2 <- y - 2
      year3 <- y - 3
      year4 <- y - 4
      
    } else if(startyr < 2010){
      
      year1 <- y + 1
      year2 <- y + 2
      year3 <- y + 3
      year4 <- y + 4
      
    } else if(startyr >= 2010){
      
      year1 <- y - 1
      year2 <- y - 2
      year3 <- y - 3
      year4 <- y - 4
      
    }
    
    # pull values for calculating growth estimate
    yr1 <- df %>%
      dplyr::filter(
        iso3c == iso,
        year == year1
        ) %>%
      dplyr::pull(.data[[metric]])
    
    yr2 <- df %>%
      dplyr::filter(
        iso3c == iso,
        year == year2
      ) %>%
      dplyr::pull(.data[[metric]])
    
    yr3 <- df %>%
      dplyr::filter(
        iso3c == iso,
        year == year3
      ) %>%
      dplyr::pull(.data[[metric]])
    
    yr4 <- df %>%
      dplyr::filter(
        iso3c == iso,
        year == year4
      ) %>%
      dplyr::pull(.data[[metric]])
    
    growth_rate <- (1/2)*(yr1 / yr2) + (1/3)*(yr2 / yr3) + (1/6)*(yr3 / yr4)

    # add estimate
    df <- df %>%
      dplyr::mutate(!!metric := ifelse(iso3c==iso&year==y,
                                       yr1 * growth_rate,
                                       .data[[metric]]))
    
  }
  
  return(df)
  
}

### military metrics plotting functions ----------------------------------------------------------------------
mil.expenditure.country.plot <- function(data = mildata, iso){
  
  df <- data %>%
    dplyr::filter(iso3c == iso) %>%
    dplyr::arrange(year) %>%
    dplyr::select(
      year,mil.expenditure.cow,mil.expenditure.cow.alt,mil.expenditure.wmeat,mil.expenditure.sipri
      ) %>%
    tidyr::pivot_longer(2:5, names_to = "metric", values_to = "value")
  
  plot <- ggplot(data = df) +
    geom_line(aes(x = year, y = value, group = metric, color = metric))
  
}

#### pull 0s ----------------------------------------------------------------------

# Data that are 0s for non-small countries likely are 0 due to rounding.
mildata.zeros <- mildata %>%
  dplyr::select("iso3c","country","year","mil.expenditure.cow","mil.personnel.cow",
                "mil.expenditure.wmeat","mil.personnel.wmeat","mil.expenditure.sipri",
                "defense.spending.iiss","active.armed.forces.iiss") %>%
  tidyr::pivot_longer(4:10, names_to = "variable", values_to = "value") %>%
  dplyr::filter(
    value == 0,
    # remove small countries who don't have full time series
    iso3c %!in% c(
      "DMA","GRD","LCA","VCT","ATG","KNA","MCO","LIE","AND","SMR","ISL","COM","MDV","VUT",
      "SLB","KIR","TUV","TON","NRU","MHL","PLW","FSM","WSM","PSE"
      )
    ) %>%
  dplyr::left_join(cyears,by=c("iso3c","country","year")) %>%
  dplyr::filter(cn == 1) %>%
  dplyr::select(-c(cn,yrs_since_indep))

#### estimate missing values ----------------------------------------------------------------------
# replace COW estimates 2010-2012 with IISS data that is more recently released, for countries whose
# COW and IISS data broadly align outside 2012

milbal.expenditure <- military.balance.data.premerge %>%
  dplyr::filter(metric == "defense.spending") %>%
  dplyr::arrange(year)

# countries who will see substantial impacts based on replacing the data (>=10%
# difference between at least one year's [2008-2012] COW and IISS epxenditure
# data):
# AFG, ALB, ARM, ATG, BGR*, BWA, COL, CPV, CUB, DJI, ESP, FJI, GBR*, GEO, GRC, HRV,
# HUN, IDN, IND, IRL, IRN, ISR*, ITA, JAM, KGZ, KHM*, LBN, LBR, LKA*, LSO, LTU, LVA,
# MUS, NAM, NOR*, NPL, NZL, PHL, PRT, ROU, SEN, SVK, TJK, TLS, TUR, TZA, UGA*, VEN,
# YEM, ZWE

# look into COW CUB data
# look into FJI IISS data (rnd2)

# *significant, but not >=10% difference

expenditure.cow.iiss.replace <- c(
  "AFG", "AGO", "ALB", "ARE", "ARG", "ARM", "ATG", "AUS", "AUT", "AZE", "BDI", "BEL", "BEN", "BFA",
  "BGD", "BGR", "BHR", "BHS", "BIH", "BLR", "BLZ", "BOL", "BRA", "BRB", "BRN", "BWA", "CAF", "CAN",
  "CHE", "CHL", "CHN", "CIV", "CMR", "COD", "COG", "COL", "CPV", "CRI", "CUB", "CYP", "CZE", "DEU",
  "DJI", "DNK", "DOM", "DZA", "ECU", "EGY", "ERI", "ESP", "EST", "ETH", "FIN", "FJI", "FRA", "GAB",
  "GBR", "GEO", "GHA", "GIN", "GMB", "GNB", "GNQ", "GRC", "GTM", "GUY", "HND", "HRV", "HUN", "IDN",
  "IND", "IRL", "IRN", "IRQ", "ISR", "ITA", "JAM", "JOR", "JPN", "KAZ", "KEN", "KGZ", "KHM", "KOR",
  "KWT", "LAO", "LBN", "LBR", "LBY", "LKA", "LSO", "LTU", "LUX", "LVA", "MAR", "MDA", "MDG", "MEX",
  "MKD", "MLI", "MLT", "MMR", "MNE", "MNG", "MOZ", "MRT", "MUS", "MWI", "MYS", "NAM", "NER", "NGA",
  "NIC", "NLD", "NOR", "NPL", "NZL", "OMN", "PAK", "PAN", "PER", "PHL", "PNG", "POL", "PRT", "PRY",
  "QAT", "ROU", "RUS", "RWA", "SAU", "SDN", "SEN", "SGP", "SLE", "SLV", "SOM", "SRB", "SSD", "SUR",
  "SVK", "SVN", "SWE", "SYC", "SYR", "TCD", "TGO", "THA", "TJK", "TKM", "TLS", "TTO", "TUN", "TUR",
  "TWN", "TZA", "UGA", "UKR", "URY", "USA", "UZB", "VEN", "VNM", "ZAF", "ZMB", "ZWE"
  )

# Not applied to: AND, BTN, COM, DMA, FSM, GRD, HTI, ISL, KIR, KNA, KSV, LCA, LIE, MCO, MDV, MHL, NRU, PLW, PRK,
# SLB, SMR, STP, SWZ, TON, TUV, VUT, WSM

# add new columns as an alternate to the primary way that keeps all COW estiamtes
mildata <- mildata %>%
  dplyr::mutate(
    mil.expenditure.cow.alt = dplyr::case_when(
      iso3c %in% expenditure.cow.iiss.replace ~ dplyr::coalesce(defense.spending.iiss,milex.cow),
      !iso3c %in% expenditure.cow.iiss.replace ~ milex.cow,
      .default = milex.cow
    )
  ) %>%
  dplyr::relocate(mil.expenditure.cow.alt, .after = mil.expenditure.cow)

# extending COW estimates 2013-2019 based on complete IISS data (if incomplete data, done under the
# specific country). COW estimates for these metrics are sourced from IISS data, so this generally
# amounts to updating the COW dataset.

modern.mil.expenditure.data <- mildata %>%
  dplyr::filter(year >= 2007) %>%
  dplyr::select(iso3c,year,mil.expenditure.cow,mil.expenditure.wmeat,
                milex.cow,defense.spending.iiss,military.expenditure.wmeat) %>%
  dplyr::arrange(year) %>%
  dplyr::mutate(cow.iiss.diff = mil.expenditure.cow / defense.spending.iiss)

# Expenditure extended based on % growth, given that the 2012 COW and IISS estimates align (this
# accounts for small differences from converting to 2019$)
expenditure.cow.extend <- c(
  "AFG", "AGO", "ALB", "ARG", "ARM", "ATG", "AUS", "AUT", "AZE", "BDI", "BEL", "BEN", "BFA", "BGD",
  "BGR", "BHR", "BHS", "BIH", "BLR", "BLZ", "BOL", "BRA", "BRB", "BRN", "CAN", "CHE", "CHL", "CHN",
  "CIV", "CMR", "COD", "COG", "CPV", "CRI", "CYP", "CZE", "DEU", "DNK", "DOM", "DZA", "ECU", "EGY",
  "EST", "ETH", "FIN", "FJI", "FRA", "GAB", "GBR", "GEO", "GHA", "GTM", "GUY", "HND", "HRV", "HUN",
  "IDN", "IRL", "IRQ", "ISR", "JAM", "JOR", "JPN", "KAZ", "KEN", "KHM", "KOR", "KWT", "LBR", "LKA",
  "LSO", "LTU", "LUX", "MAR", "MDA", "MDG", "MEX", "MKD", "MLI", "MLT", "MMR", "MNE", "MNG", "MOZ",
  "MRT", "MUS", "MWI", "MYS", "NAM", "NGA", "NIC", "NLD", "NOR", "NZL", "OMN", "PAK", "PAN", "PER",
  "PHL", "PNG", "POL", "PRT", "PRY", "ROU", "RUS", "RWA", "SAU", "SEN", "SGP", "SLE", "SLV", "SSD",
  "SVK", "SVN", "SWE", "TCD", "TGO", "THA", "TJK", "TLS", "TTO", "TUN", "TUR", "TWN", "TZA", "UGA",
  "UKR", "URY", "USA", "VNM", "ZAF", "ZMB", "ZWE"
  )

for(c in expenditure.cow.extend){
  
  mildata <- mil_expenditure_growth_estimator_func(df = mildata, col_missing = "mil.expenditure.cow",
                                                   col_ref = "defense.spending.iiss", iso = c,
                                                   yr = 2012, restricted = c(2013:2019))
      
  # mildata <- mil_expenditure_growth_estimator_cow_func(df = mildata, iso = c, yr = 2012, restricted = c(2013:2019))

}

# Personnel extended if the 2012 COW and IISS estimates match
personnel.cow.extend <- c(
  "AFG", "AGO", "ALB", "ARE", "ARG", "ARM", "ATG", "AUS", "AUT", "AZE", "BDI", "BEL", "BEN", "BFA",
  "BGD", "BGR", "BHR", "BHS", "BIH", "BLR", "BLZ", "BOL", "BRA", "BRB", "BRN", "BWA", "CAF", "CAN",
  "CHE", "CHL", "CHN", "CMR", "COD", "COG", "COL", "CPV", "CRI", "CUB", "CYP", "CZE", "DEU", "DJI",
  "DNK", "DOM", "DZA", "ECU", "EGY", "ERI", "ESP", "EST", "ETH", "FIN", "FJI", "FRA", "GAB", "GBR",
  "GEO", "GHA", "GIN", "GMB", "GNB", "GNQ", "GRC", "GTM", "GUY", "HND", "HRV", "HTI", "HUN", "IDN",
  "IND", "IRL", "IRN", "IRQ", "ISL", "ISR", "ITA", "JAM", "JOR", "JPN", "KAZ", "KEN", "KGZ", "KHM",
  "KOR", "KWT", "LAO", "LBN", "LBR", "LKA", "LSO", "LTU", "LUX", "LVA", "MAR", "MDA", "MDG", "MEX",
  "MKD", "MLI", "MLT", "MMR", "MNE", "MNG", "MOZ", "MRT", "MUS", "MWI", "MYS", "NAM", "NER", "NGA",
  "NIC", "NLD", "NOR", "NPL", "NZL", "OMN", "PAK", "PAN", "PER", "PHL", "PNG", "POL", "PRK", "PRT",
  "PRY", "QAT", "ROU", "RUS", "RWA", "SAU", "SDN", "SEN", "SGP", "SLE", "SLV", "SOM", "SRB", "SSD",
  "SUR", "SVK", "SVN", "SWE", "SYC", "SYR", "TCD", "TGO", "THA", "TJK", "TKM", "TLS", "TTO", "TUN",
  "TUR", "TWN", "TZA", "UGA", "UKR", "URY", "USA", "UZB", "VEN", "VNM", "YEM", "ZAF", "ZMB", "ZWE"
  )

for(c in personnel.cow.extend){
  
  mildata <- mildata %>%
    dplyr::mutate(mil.personnel.cow = ifelse(is.na(mil.personnel.cow)&year %in% c(2013:2019)&iso3c==c,active.armed.forces.iiss,mil.personnel.cow))
  
}

# Add estimates flag for remaining missing values
# Note the above calculations are not included as estimate flags as they
# are extending the original data beyond its time frame and the estimates
# are sourced from the same dataset.
# Several calculations based on the IISS data will remove the flag, but
# only if they follow the same principal as above.
mildata <- mildata %>%
  dplyr::mutate(
    mil.expenditure.cow.est.flag = ifelse(is.na(mil.expenditure.cow),1,0),
    mil.expenditure.cow.alt.est.flag = ifelse(is.na(mil.expenditure.cow.alt),1,0),
    mil.personnel.cow.est.flag = ifelse(is.na(mil.personnel.cow),1,0),
    mil.expenditure.wmeat.est.flag = ifelse(is.na(mil.expenditure.wmeat),1,0),
    mil.personnel.wmeat.est.flag = ifelse(is.na(mil.personnel.wmeat),1,0),
    mil.expenditure.sipri.est.flag = ifelse(is.na(mil.expenditure.sipri),1,0)
    )

# No cow both: AND, BTN, COM, DMA, FSM, GRD, KIR, KNA, KSV, LBY, LCA, LIE, MCO, MDV, MHL, NRU, PLW, PSE, SLB, SMR, STP, SWZ, TON, TUV, VCT, VUT, WSM
# No cow expenditure: ARE, BWA, CAF, COL, CUB, DJI, ERI, ESP, GIN, GMB, GNB, GNQ, GRC, HTI, IND, IRN, ISL,
# ITA, KGZ, LAO, LBN, LVA, NER, NPL, PRK, QAT, SDN, SOM, SRB, SUR, SYC, SYR, TKM, UZB, VEN, YEM
# No cow personnel: CIV

# expenditure viewer fx
milexp.viewer <- function(iso){
  
  df <- mildata %>%
    dplyr::filter(iso3c == iso) %>%
    dplyr::select(year,mil.expenditure.cow,mil.expenditure.cow.alt,mil.expenditure.wmeat,mil.expenditure.sipri,
                  milex.cow,defense.spending.iiss,military.expenditure.wmeat,milexp.sipri) %>%
    dplyr::arrange(year)
  
  return(df)
  
}

# personnel viewer fx
milper.viewer <- function(iso){
  
  df <- mildata %>%
    dplyr::filter(iso3c == iso) %>%
    dplyr::select(year,mil.personnel.cow,mil.personnel.wmeat,milper.cow,active.armed.forces.iiss,reservists.iiss,
                  active.paramilitary.iiss,armed.personnel.wmeat) %>%
    dplyr::arrange(year) %>%
    dplyr::mutate(est.diff = mil.personnel.cow - mil.personnel.wmeat)
  
  return(df)
  
}

##### AFG ----------------------------------------------------------------------

###### cow expenditure
# 1986-1989: assume consistent growth between 1985 and 1990
afg.alpha.1 <- (mildata$mil.expenditure.cow[mildata$iso3c=="AFG"&mildata$year==1990]/mildata$mil.expenditure.cow[mildata$iso3c=="AFG"&mildata$year==1985])^(1/5)

mildata$mil.expenditure.cow[mildata$iso3c=="AFG"&mildata$year==1986] <- mildata$mil.expenditure.cow[mildata$iso3c=="AFG"&mildata$year==1985] * afg.alpha.1
mildata$mil.expenditure.cow[mildata$iso3c=="AFG"&mildata$year==1987] <- mildata$mil.expenditure.cow[mildata$iso3c=="AFG"&mildata$year==1986] * afg.alpha.1
mildata$mil.expenditure.cow[mildata$iso3c=="AFG"&mildata$year==1988] <- mildata$mil.expenditure.cow[mildata$iso3c=="AFG"&mildata$year==1987] * afg.alpha.1
mildata$mil.expenditure.cow[mildata$iso3c=="AFG"&mildata$year==1989] <- mildata$mil.expenditure.cow[mildata$iso3c=="AFG"&mildata$year==1988] * afg.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="AFG"&mildata$year==1986] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="AFG"&mildata$year==1985] * afg.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="AFG"&mildata$year==1987] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="AFG"&mildata$year==1986] * afg.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="AFG"&mildata$year==1988] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="AFG"&mildata$year==1987] * afg.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="AFG"&mildata$year==1989] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="AFG"&mildata$year==1988] * afg.alpha.1

# 1991-1994: assume consistent growth between 1990 and 1995
afg.alpha.2 <- (mildata$mil.expenditure.cow[mildata$iso3c=="AFG"&mildata$year==1995]/mildata$mil.expenditure.cow[mildata$iso3c=="AFG"&mildata$year==1990])^(1/5)

mildata$mil.expenditure.cow[mildata$iso3c=="AFG"&mildata$year==1991] <- mildata$mil.expenditure.cow[mildata$iso3c=="AFG"&mildata$year==1990] * afg.alpha.2
mildata$mil.expenditure.cow[mildata$iso3c=="AFG"&mildata$year==1992] <- mildata$mil.expenditure.cow[mildata$iso3c=="AFG"&mildata$year==1991] * afg.alpha.2
mildata$mil.expenditure.cow[mildata$iso3c=="AFG"&mildata$year==1993] <- mildata$mil.expenditure.cow[mildata$iso3c=="AFG"&mildata$year==1992] * afg.alpha.2
mildata$mil.expenditure.cow[mildata$iso3c=="AFG"&mildata$year==1994] <- mildata$mil.expenditure.cow[mildata$iso3c=="AFG"&mildata$year==1993] * afg.alpha.2
mildata$mil.expenditure.cow.alt[mildata$iso3c=="AFG"&mildata$year==1991] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="AFG"&mildata$year==1990] * afg.alpha.2
mildata$mil.expenditure.cow.alt[mildata$iso3c=="AFG"&mildata$year==1992] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="AFG"&mildata$year==1991] * afg.alpha.2
mildata$mil.expenditure.cow.alt[mildata$iso3c=="AFG"&mildata$year==1993] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="AFG"&mildata$year==1992] * afg.alpha.2
mildata$mil.expenditure.cow.alt[mildata$iso3c=="AFG"&mildata$year==1994] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="AFG"&mildata$year==1993] * afg.alpha.2

# 2002-2004
mildata$mil.expenditure.cow[mildata$iso3c=="AFG"&mildata$year==2002] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "AFG", 2002, yr.minus.1 = 2001, yr.plus.1 = 2005)
mildata$mil.expenditure.cow[mildata$iso3c=="AFG"&mildata$year==2003] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "AFG", 2003, yr.minus.1 = 2001, yr.plus.1 = 2005)
mildata$mil.expenditure.cow[mildata$iso3c=="AFG"&mildata$year==2004] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "AFG", 2004, yr.minus.1 = 2001, yr.plus.1 = 2005)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="AFG"&mildata$year==2002] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "AFG", 2002, yr.minus.1 = 2001, yr.plus.1 = 2005)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="AFG"&mildata$year==2003] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "AFG", 2003, yr.minus.1 = 2001, yr.plus.1 = 2005)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="AFG"&mildata$year==2004] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "AFG", 2004, yr.minus.1 = 2001, yr.plus.1 = 2005)

# 1946-1949
mildata <- mil_estimator_arima_func(
  mildata, "AFG",
  metric = "mil.expenditure.cow",
  direction = "backwards",
  1949,1946
)
mildata <- mil_estimator_arima_func(
  mildata, "AFG",
  metric = "mil.expenditure.cow.alt",
  direction = "backwards",
  1949,1946
)

###### wmeat expenditure
# 1950-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "AFG", 1963, restricted = c(1950:1962)
)

# 1985: calculate based on ratio between COW 1984 and 1985
afg.cow.expenditure.1984 <- mildata$mil.expenditure.cow[mildata$iso3c=="AFG"&mildata$year==1984]
afg.cow.expenditure.1985 <- mildata$mil.expenditure.cow[mildata$iso3c=="AFG"&mildata$year==1985]

afg.wmeat.expenditure.1984 <- mildata$mil.expenditure.wmeat[mildata$iso3c=="AFG"&mildata$year==1984]

mildata$mil.expenditure.wmeat[mildata$iso3c=="AFG"&mildata$year==1985] <- afg.wmeat.expenditure.1984 *
  afg.cow.expenditure.1985 / afg.cow.expenditure.1984

# 1986-1989: assume consistent growth between 1985 and 1990
afg.alpha.3 <- (mildata$mil.expenditure.wmeat[mildata$iso3c=="AFG"&mildata$year==1990]/mildata$mil.expenditure.wmeat[mildata$iso3c=="AFG"&mildata$year==1985])^(1/5)

mildata$mil.expenditure.wmeat[mildata$iso3c=="AFG"&mildata$year==1986] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="AFG"&mildata$year==1985] * afg.alpha.3
mildata$mil.expenditure.wmeat[mildata$iso3c=="AFG"&mildata$year==1987] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="AFG"&mildata$year==1986] * afg.alpha.3
mildata$mil.expenditure.wmeat[mildata$iso3c=="AFG"&mildata$year==1988] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="AFG"&mildata$year==1987] * afg.alpha.3
mildata$mil.expenditure.wmeat[mildata$iso3c=="AFG"&mildata$year==1989] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="AFG"&mildata$year==1988] * afg.alpha.3

# 1991-1994: assume consistent growth between 1990 and 1995
afg.alpha.4 <- (mildata$mil.expenditure.wmeat[mildata$iso3c=="AFG"&mildata$year==1995]/mildata$mil.expenditure.wmeat[mildata$iso3c=="AFG"&mildata$year==1990])^(1/5)

mildata$mil.expenditure.wmeat[mildata$iso3c=="AFG"&mildata$year==1991] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="AFG"&mildata$year==1990] * afg.alpha.4
mildata$mil.expenditure.wmeat[mildata$iso3c=="AFG"&mildata$year==1992] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="AFG"&mildata$year==1991] * afg.alpha.4
mildata$mil.expenditure.wmeat[mildata$iso3c=="AFG"&mildata$year==1993] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="AFG"&mildata$year==1992] * afg.alpha.4
mildata$mil.expenditure.wmeat[mildata$iso3c=="AFG"&mildata$year==1994] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="AFG"&mildata$year==1993] * afg.alpha.4

# 1995-2000
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "AFG", 2001, restricted = c(1995:2000)
)

# 2018-2019
mildata <- mil_estimator_lm_func(
  mildata, "AFG",
  est_variable = "mil.expenditure.wmeat",
  model_variables = c(
    "mil.expenditure.cow.alt","mil.expenditure.sipri","mil.expenditure.growth.rate.cow.alt",
    "mil.expenditure.growth.rate.sipri","mil.expenditure.cow.alt.plus1","mil.expenditure.wmeat.plus1",
    "mil.expenditure.sipri.plus1"
  ),
  model_data_years = c(2006:2017),
  predict_years = c(2018:2019)
)

# 1946-1949
mildata <- mil_estimator_arima_func(
  mildata, "AFG",
  metric = "mil.expenditure.wmeat",
  direction = "backwards",
  1949,1946
)

###### sipri expenditure
# 1950-1969
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "AFG", 1970, restricted = c(1950:1969)
)

# 1971-1972
mildata$mil.expenditure.sipri[mildata$iso3c=="AFG"&mildata$year==1971] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "AFG", 1971, yr.minus.1 = 1970, yr.plus.1 = 1973)
mildata$mil.expenditure.sipri[mildata$iso3c=="AFG"&mildata$year==1972] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "AFG", 1972, yr.minus.1 = 1970, yr.plus.1 = 1973)

# 1978-1985
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "AFG", 1977, restricted = c(1978:1985)
)

# 2001-2003
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.wmeat",
  "AFG", 2004, restricted = c(2001:2003)
)

# 1995-2000
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "AFG", 2001, restricted = c(1995:2000)
)

# 1990
afg.cow.1985 <- mildata$mil.expenditure.cow[mildata$iso3c=="AFG"&mildata$year==1985]
afg.cow.1990 <- mildata$mil.expenditure.cow[mildata$iso3c=="AFG"&mildata$year==1990]
afg.cow.1995 <- mildata$mil.expenditure.cow[mildata$iso3c=="AFG"&mildata$year==1995]
afg.sipri.1985 <- mildata$mil.expenditure.sipri[mildata$iso3c=="AFG"&mildata$year==1985]
afg.sipri.1995 <- mildata$mil.expenditure.sipri[mildata$iso3c=="AFG"&mildata$year==1995]

afg.sipri.1990.a <- afg.sipri.1985 * afg.cow.1990 / afg.cow.1985
afg.sipri.1990.b <- afg.sipri.1995 * afg.cow.1990 / afg.cow.1995

mildata$mil.expenditure.sipri[mildata$iso3c=="AFG"&mildata$year==1990] <- mean(afg.sipri.1990.a,afg.sipri.1990.b)

# 1986-1989: assume consistent growth between 1985 and 1990
afg.alpha.5 <- (mildata$mil.expenditure.sipri[mildata$iso3c=="AFG"&mildata$year==1990]/mildata$mil.expenditure.sipri[mildata$iso3c=="AFG"&mildata$year==1985])^(1/5)

mildata$mil.expenditure.sipri[mildata$iso3c=="AFG"&mildata$year==1986] <- mildata$mil.expenditure.sipri[mildata$iso3c=="AFG"&mildata$year==1985] * afg.alpha.5
mildata$mil.expenditure.sipri[mildata$iso3c=="AFG"&mildata$year==1987] <- mildata$mil.expenditure.sipri[mildata$iso3c=="AFG"&mildata$year==1986] * afg.alpha.5
mildata$mil.expenditure.sipri[mildata$iso3c=="AFG"&mildata$year==1988] <- mildata$mil.expenditure.sipri[mildata$iso3c=="AFG"&mildata$year==1987] * afg.alpha.5
mildata$mil.expenditure.sipri[mildata$iso3c=="AFG"&mildata$year==1989] <- mildata$mil.expenditure.sipri[mildata$iso3c=="AFG"&mildata$year==1988] * afg.alpha.5

# 1991-1994: assume consistent growth between 1990 and 1995
afg.alpha.6 <- (mildata$mil.expenditure.sipri[mildata$iso3c=="AFG"&mildata$year==1995]/mildata$mil.expenditure.sipri[mildata$iso3c=="AFG"&mildata$year==1990])^(1/5)

mildata$mil.expenditure.sipri[mildata$iso3c=="AFG"&mildata$year==1991] <- mildata$mil.expenditure.sipri[mildata$iso3c=="AFG"&mildata$year==1990] * afg.alpha.6
mildata$mil.expenditure.sipri[mildata$iso3c=="AFG"&mildata$year==1992] <- mildata$mil.expenditure.sipri[mildata$iso3c=="AFG"&mildata$year==1991] * afg.alpha.6
mildata$mil.expenditure.sipri[mildata$iso3c=="AFG"&mildata$year==1993] <- mildata$mil.expenditure.sipri[mildata$iso3c=="AFG"&mildata$year==1992] * afg.alpha.6
mildata$mil.expenditure.sipri[mildata$iso3c=="AFG"&mildata$year==1994] <- mildata$mil.expenditure.sipri[mildata$iso3c=="AFG"&mildata$year==1993] * afg.alpha.6

# 1946-1949
mildata <- mil_estimator_arima_func(
  mildata, "AFG",
  metric = "mil.expenditure.sipri",
  direction = "backwards",
  1949,1946
)

###### cow personnel
# 1995-1998: assume consistent growth between 1994 and 1999
afg.alpha.7 <- (mildata$mil.personnel.cow[mildata$iso3c=="AFG"&mildata$year==1999]/mildata$mil.personnel.cow[mildata$iso3c=="AFG"&mildata$year==1994])^(1/5)

mildata$mil.personnel.cow[mildata$iso3c=="AFG"&mildata$year==1995] <- mildata$mil.personnel.cow[mildata$iso3c=="AFG"&mildata$year==1994] * afg.alpha.7
mildata$mil.personnel.cow[mildata$iso3c=="AFG"&mildata$year==1996] <- mildata$mil.personnel.cow[mildata$iso3c=="AFG"&mildata$year==1995] * afg.alpha.7
mildata$mil.personnel.cow[mildata$iso3c=="AFG"&mildata$year==1997] <- mildata$mil.personnel.cow[mildata$iso3c=="AFG"&mildata$year==1996] * afg.alpha.7
mildata$mil.personnel.cow[mildata$iso3c=="AFG"&mildata$year==1998] <- mildata$mil.personnel.cow[mildata$iso3c=="AFG"&mildata$year==1997] * afg.alpha.7

# 2001: calculate based on ratio between WMEAT 2001 and 2002 (2000 WMEAT is NA)
afg.wmeat.personnel.2001 <- mildata$armed.personnel.wmeat[mildata$iso3c=="AFG"&mildata$year==2001]
afg.wmeat.personnel.2002 <- mildata$armed.personnel.wmeat[mildata$iso3c=="AFG"&mildata$year==2002]

afg.cow.personnel.2002 <- mildata$mil.personnel.cow[mildata$iso3c=="AFG"&mildata$year==2002]

mildata$mil.personnel.cow[mildata$iso3c=="AFG"&mildata$year==2001] <- afg.cow.personnel.2002 *
  afg.wmeat.personnel.2001 / afg.wmeat.personnel.2002

###### wmeat personnel
# 1945-1962: COW and WMEAT personnel match 1963-1994, so use COW personnel estimates for 1945-1962
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1945:1962)&iso3c=="AFG",mil.personnel.cow,mil.personnel.wmeat))

# 1995-1998: assume consistent growth between 1994 and 1999
afg.alpha.8 <- (mildata$mil.personnel.wmeat[mildata$iso3c=="AFG"&mildata$year==1999]/mildata$mil.personnel.wmeat[mildata$iso3c=="AFG"&mildata$year==1994])^(1/5)

mildata$mil.personnel.wmeat[mildata$iso3c=="AFG"&mildata$year==1995] <- mildata$mil.personnel.wmeat[mildata$iso3c=="AFG"&mildata$year==1994] * afg.alpha.8
mildata$mil.personnel.wmeat[mildata$iso3c=="AFG"&mildata$year==1996] <- mildata$mil.personnel.wmeat[mildata$iso3c=="AFG"&mildata$year==1995] * afg.alpha.8
mildata$mil.personnel.wmeat[mildata$iso3c=="AFG"&mildata$year==1997] <- mildata$mil.personnel.wmeat[mildata$iso3c=="AFG"&mildata$year==1996] * afg.alpha.8
mildata$mil.personnel.wmeat[mildata$iso3c=="AFG"&mildata$year==1998] <- mildata$mil.personnel.wmeat[mildata$iso3c=="AFG"&mildata$year==1997] * afg.alpha.8

# 1999-2000
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "AFG", 2002, restricted = c(1999:2000)
)

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "AFG", 2017, restricted = c(2018:2019)
)

x <- milexp.viewer("AFG")
y <- milper.viewer("AFG")

##### AGO ----------------------------------------------------------------------

###### cow expenditure
# 1978
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "mil.expenditure.sipri",
  "AGO", 1979, restricted = 1978
)
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow.alt",
  col_ref = "mil.expenditure.sipri",
  "AGO", 1979, restricted = 1978
)

# 1976-1977: assume consistent growth between 1975 and 1978
ago.alpha.1 <- (mildata$mil.expenditure.cow[mildata$iso3c=="AGO"&mildata$year==1978]/mildata$mil.expenditure.cow[mildata$iso3c=="AGO"&mildata$year==1975])^(1/3)

mildata$mil.expenditure.cow[mildata$iso3c=="AGO"&mildata$year==1976] <- mildata$mil.expenditure.cow[mildata$iso3c=="AGO"&mildata$year==1975] * ago.alpha.1
mildata$mil.expenditure.cow[mildata$iso3c=="AGO"&mildata$year==1977] <- mildata$mil.expenditure.cow[mildata$iso3c=="AGO"&mildata$year==1976] * ago.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="AGO"&mildata$year==1976] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="AGO"&mildata$year==1975] * ago.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="AGO"&mildata$year==1977] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="AGO"&mildata$year==1976] * ago.alpha.1

# 1984
mildata$mil.expenditure.cow[mildata$iso3c=="AGO"&mildata$year==1984] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "AGO", 1984)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="AGO"&mildata$year==1984] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "AGO", 1984)

# 1992
mildata$mil.expenditure.cow[mildata$iso3c=="AGO"&mildata$year==1992] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "AGO", 1992)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="AGO"&mildata$year==1992] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "AGO", 1992)

###### wmeat expenditure
# 1978-1982
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "AGO", 1983, restricted = c(1978:1982)
)

# 1976-1977: assume consistent growth between 1975 and 1978
ago.alpha.2 <- (mildata$mil.expenditure.wmeat[mildata$iso3c=="AGO"&mildata$year==1978]/mildata$mil.expenditure.wmeat[mildata$iso3c=="AGO"&mildata$year==1975])^(1/3)

mildata$mil.expenditure.wmeat[mildata$iso3c=="AGO"&mildata$year==1976] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="AGO"&mildata$year==1975] * ago.alpha.2
mildata$mil.expenditure.wmeat[mildata$iso3c=="AGO"&mildata$year==1977] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="AGO"&mildata$year==1976] * ago.alpha.2

# 1984-1985
mildata$mil.expenditure.wmeat[mildata$iso3c=="AGO"&mildata$year==1984] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "AGO", 1984, yr.minus.1 = 1983, yr.plus.1 = 1986)
mildata$mil.expenditure.wmeat[mildata$iso3c=="AGO"&mildata$year==1985] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "AGO", 1985, yr.minus.1 = 1983, yr.plus.1 = 1986)

# 1987-1994
mildata$mil.expenditure.wmeat[mildata$iso3c=="AGO"&mildata$year==1987] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "AGO", 1987, yr.minus.1 = 1986, yr.plus.1 = 1995)
mildata$mil.expenditure.wmeat[mildata$iso3c=="AGO"&mildata$year==1988] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "AGO", 1988, yr.minus.1 = 1986, yr.plus.1 = 1995)
mildata$mil.expenditure.wmeat[mildata$iso3c=="AGO"&mildata$year==1989] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "AGO", 1989, yr.minus.1 = 1986, yr.plus.1 = 1995)
mildata$mil.expenditure.wmeat[mildata$iso3c=="AGO"&mildata$year==1990] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "AGO", 1990, yr.minus.1 = 1986, yr.plus.1 = 1995)
mildata$mil.expenditure.wmeat[mildata$iso3c=="AGO"&mildata$year==1991] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "AGO", 1991, yr.minus.1 = 1986, yr.plus.1 = 1995)
mildata$mil.expenditure.wmeat[mildata$iso3c=="AGO"&mildata$year==1992] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "AGO", 1992, yr.minus.1 = 1986, yr.plus.1 = 1995)
mildata$mil.expenditure.wmeat[mildata$iso3c=="AGO"&mildata$year==1993] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "AGO", 1993, yr.minus.1 = 1986, yr.plus.1 = 1995)
mildata$mil.expenditure.wmeat[mildata$iso3c=="AGO"&mildata$year==1994] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "AGO", 1994, yr.minus.1 = 1986, yr.plus.1 = 1995)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "AGO", 2017, restricted = c(2018:2019)
)

###### sipri expenditure
# 1975: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "AGO", 1979, restricted = 1975
)

# 1976-1977: assume consistent growth between 1975 and 1978
ago.alpha.3 <- (mildata$mil.expenditure.sipri[mildata$iso3c=="AGO"&mildata$year==1978]/mildata$mil.expenditure.sipri[mildata$iso3c=="AGO"&mildata$year==1975])^(1/3)

mildata$mil.expenditure.sipri[mildata$iso3c=="AGO"&mildata$year==1976] <- mildata$mil.expenditure.sipri[mildata$iso3c=="AGO"&mildata$year==1975] * ago.alpha.3
mildata$mil.expenditure.sipri[mildata$iso3c=="AGO"&mildata$year==1977] <- mildata$mil.expenditure.sipri[mildata$iso3c=="AGO"&mildata$year==1976] * ago.alpha.3

###### cow personnel
# good

###### wmeat personnel
# 2018-2019: 2009-2010 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="AGO"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="AGO"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="AGO"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="AGO"&mildata$year==2018]

# x <- milexp.viewer("AGO")
# y <- milper.viewer("AGO")

##### ALB ----------------------------------------------------------------------

###### cow expenditure
# good

###### wmeat expenditure
# 1946-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "ALB", 1963, restricted = c(1946:1962)
)

# 1980: estimate WMEAT 1990 as equidistant point based on COW 1979-1981 values
mildata$mil.expenditure.wmeat[mildata$iso3c=="ALB"&mildata$year==1980] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "ALB", 1980)

# 1982-1983
mildata$mil.expenditure.wmeat[mildata$iso3c=="ALB"&mildata$year==1982] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "ALB", 1982, yr.minus.1 = 1981, yr.plus.1 = 1984)
mildata$mil.expenditure.wmeat[mildata$iso3c=="ALB"&mildata$year==1983] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "ALB", 1983, yr.minus.1 = 1981, yr.plus.1 = 1984)

# 1986: estimate WMEAT 1986 as equidistant point based on COW 1985-1987 values
mildata$mil.expenditure.wmeat[mildata$iso3c=="ALB"&mildata$year==1986] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "ALB", 1986)

# 1990-1994
mildata$mil.expenditure.wmeat[mildata$iso3c=="ALB"&mildata$year==1990] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "ALB", 1990, yr.minus.1 = 1989, yr.plus.1 = 1995)
mildata$mil.expenditure.wmeat[mildata$iso3c=="ALB"&mildata$year==1991] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "ALB", 1991, yr.minus.1 = 1989, yr.plus.1 = 1995)
mildata$mil.expenditure.wmeat[mildata$iso3c=="ALB"&mildata$year==1992] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "ALB", 1992, yr.minus.1 = 1989, yr.plus.1 = 1995)
mildata$mil.expenditure.wmeat[mildata$iso3c=="ALB"&mildata$year==1993] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "ALB", 1993, yr.minus.1 = 1989, yr.plus.1 = 1995)
mildata$mil.expenditure.wmeat[mildata$iso3c=="ALB"&mildata$year==1994] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "ALB", 1994, yr.minus.1 = 1989, yr.plus.1 = 1995)


# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "ALB", 2017, restricted = c(2018:2019)
)

###### sipri expenditure
# 1946-1991: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "ALB", 1992, restricted = c(1946:1991)
)

###### cow personnel
# good

###### wmeat personnel
# 1946-1962: COW and WMEAT personnel match 1963-1983, so use COW personnel estimates for 1945-1962
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1945:1962)&iso3c=="ALB",mil.personnel.cow,mil.personnel.wmeat))

# 1984-1991: COW and WMEAT personnel match 1983 and 1992-1993, so use COW personnel estimates for 1984-1991
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1984:1991)&iso3c=="ALB",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "ALB", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("ALB")
# y <- milper.viewer("ALB")

##### AND(x) ----------------------------------------------------------------------

###### cow expenditure
# 1946-1992
# 2013-2019

###### wmeat expenditure
# N/A

###### sipri expenditure
# N/A

###### cow personnel
# 1946-1992
# 2013-2019

###### wmeat personnel
# N/A

x <- milexp.viewer("AND")
y <- milper.viewer("AND")

##### ARE ----------------------------------------------------------------------

###### cow expenditure
# 2012-2013: use % change of IISS estimates
mildata <- mil_expenditure_growth_estimator_func(
  df = mildata, col_missing = "mil.expenditure.cow",
  col_ref = "defense.spending.iiss", iso = "ARE",
  yr = 2011, restricted = c(2012:2013))

## remove estimate flag for 2012-2013
mildata <- mildata %>%
  dplyr::mutate(mil.expenditure.cow.est.flag  = dplyr::case_when(
    iso3c == "ARE" & year %in% c(2012:2013) ~ 0,
    .default = mil.expenditure.cow.est.flag
  ))

# 2014-2017: use % change of WMEAT estimates
mildata <- mil_expenditure_growth_estimator_func(
  df = mildata, col_missing = "mil.expenditure.cow",
  col_ref = "military.expenditure.wmeat", iso = "ARE",
  yr = 2013, restricted = c(2014:2017))

# (same for alt)
mildata <- mil_expenditure_growth_estimator_func(
  df = mildata, col_missing = "mil.expenditure.cow.alt",
  col_ref = "military.expenditure.wmeat", iso = "ARE",
  yr = 2013, restricted = c(2014:2017))

# 2018-2019
mildata <- mil_estimator_no_data_func(
  mildata,"ARE",
  metric = "mil.expenditure.cow",
  2018,2019
)
mildata <- mil_estimator_no_data_func(
  mildata,"ARE",
  metric = "mil.expenditure.cow.alt",
  2018,2019
)

###### wmeat expenditure
# 1971-1972: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "ARE", 1973, restricted = c(1971:1972)
)

# 2018-2019
mildata <- mil_estimator_no_data_func(
  mildata,"ARE",
  metric = "mil.expenditure.wmeat",
  2018,2019
)

###### sipri expenditure
# 1971-1996: use % change of COW estimates
mildata <- mil_expenditure_growth_estimator_func(
  df = mildata, col_missing = "mil.expenditure.sipri",
  col_ref = "milex.cow", iso = "ARE",
  yr = 1997, restricted = c(1971:1996))

# 2015-2017: use % change of WMEAT estimates
mildata <- mil_expenditure_growth_estimator_func(
  df = mildata, col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.wmeat", iso = "ARE",
  yr = 2014, restricted = c(2015:2017))

# 2018-2019
mildata <- mil_estimator_no_data_func(
  mildata,"ARE",
  metric = "mil.expenditure.sipri",
  2018,2019
)

###### cow personnel
# good

###### wmeat personnel
# 1971: COW and WMEAT personnel match 1972-1982, so use COW personnel estimates for 1971
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year==1971&iso3c=="ARE",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2015-16 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="ARE"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="ARE"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="ARE"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="ARE"&mildata$year==2018]

# x <- milexp.viewer("ARE")
# y <- milper.viewer("ARE")

##### ARG ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1946-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "ARG", 1963, restricted = c(1946:1962)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "ARG", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1961: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "ARG", 1962, restricted = c(1946:1961)
)

##### cow personnel
# good

##### wmeat personnel
# 1946-1962: COW and WMEAT personnel match 1963-1996, so use COW personnel estimates for 1945-1962
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1945:1962)&iso3c=="ARG",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2015-16 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="ARG"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="ARG"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="ARG"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="ARG"&mildata$year==2018]

# x <- milexp.viewer("ARG")
# y <- milper.viewer("ARG")

arg.test.cow.wmeat <- mildata %>%
  dplyr::filter(iso3c == "ARG") %>%
  dplyr::arrange(year) %>%
  dplyr::select(year,milex.cow,military.expenditure.wmeat) %>%
  na.omit()

arg.cow <- mildata %>%
  dplyr::filter(iso3c == "ARG") %>%
  dplyr::arrange(year) %>%
  dplyr::pull(milex.cow)

arg.wmeat <- mildata %>%
  dplyr::filter(iso3c == "ARG") %>%
  dplyr::arrange(year) %>%
  dplyr::pull(military.expenditure.wmeat)

arg.sipri <- mildata %>%
  dplyr::filter(iso3c == "ARG") %>%
  dplyr::arrange(year) %>%
  dplyr::pull(milexp.sipri)

ccf(arg.test.cow.wmeat[,2],arg.test.cow.wmeat[,3], lag.max = 2, type = "correlation",
    plot = FALSE, na.action = na.fail)

##### ARM ----------------------------------------------------------------------

##### cow expenditure
# 1994: apply WMEAT growth rates to COW
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "military.expenditure.wmeat",
  "ARM", 1995, restricted = 1994
)

##### wmeat expenditure
# 1991-1993: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "ARM", 1994, restricted = c(1991:1993)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "ARM", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1991-1992: apply WMEAT growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.wmeat",
  "ARM", 1993, restricted = c(1991:1992)
)

# 1994: apply WMEAT growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "military.expenditure.wmeat",
  "ARM", 1995, restricted = 1994
)

##### cow personnel
# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"ARM",
  metric = "mil.personnel.cow",
  1991,1991
)

##### wmeat personnel
# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"ARM",
  metric = "mil.personnel.wmeat",
  1991,1991
)

# 2018-2019: COW and WMEAT personnel match 2014-2017, so use COW personnel estimates for 2018-2019
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(2018:2019)&iso3c=="ARM",mil.personnel.cow,mil.personnel.wmeat))

# x <- milexp.viewer("ARM")
# y <- milper.viewer("ARM")

##### AUS ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1946-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "AUS", 1963, restricted = c(1946:1962)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "AUS", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1949: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "AUS", 1950, restricted = c(1946:1949)
)

##### cow personnel
# good

##### wmeat personnel
# 1945-1962: COW and WMEAT personnel match 1963-1993, so use COW personnel estimates for 1945-1962
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1945:1962)&iso3c=="AUS",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "AUS", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("AUS")
# y <- milper.viewer("AUS")

##### AUT ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1955-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "AUT", 1963, restricted = c(1955:1962)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "AUT", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1955-1956: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "AUT", 1957, restricted = c(1955:1956)
)

##### cow personnel
# good

##### wmeat personnel
# 1955-1962: COW and WMEAT personnel match 1963-1992, so use COW personnel estimates for 1955-1962
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1955:1962)&iso3c=="AUT",mil.personnel.cow,mil.personnel.wmeat))

# 1993: COW and WMEAT personnel match 1963-1997, so use COW personnel estimates for 1993
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year==1993&iso3c=="AUT",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "AUT", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("AUT")
# y <- milper.viewer("AUT")

##### AZE ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1991: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "AZE", 1992, restricted = 1991
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "AZE", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1991: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "AZE", 1992, restricted = 1991
)

##### cow personnel
# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"AZE",
  metric = "mil.personnel.cow",
  1991,1991
)

##### wmeat personnel
# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"AZE",
  metric = "mil.personnel.wmeat",
  1991,1991
)

# 2018-2019: 2006-07 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="AZE"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="AZE"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="AZE"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="AZE"&mildata$year==2018]

# x <- milexp.viewer("AZE")
# y <- milper.viewer("AZE")

##### BDI ----------------------------------------------------------------------

##### cow expenditure
# 1992: use average of SIPRI proportions for 1991 and 1993
mildata$mil.expenditure.cow[mildata$iso3c=="BDI"&mildata$year==1992] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "BDI", 1992)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="BDI"&mildata$year==1992] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "BDI", 1992)

##### wmeat expenditure
# 1962: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "BDI", 1963, restricted = 1962
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "BDI", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1970: estimate SIPRI 1970 as equidistant point based on COW 1969-1971 values
mildata$mil.expenditure.sipri[mildata$iso3c=="BDI"&mildata$year==1970] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "BDI", 1970)

# 2009-2011
mildata$mil.expenditure.sipri[mildata$iso3c=="BDI"&mildata$year==2009] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "BDI", 2009, yr.minus.1 = 2008, yr.plus.1 = 2012)
mildata$mil.expenditure.sipri[mildata$iso3c=="BDI"&mildata$year==2010] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "BDI", 2010, yr.minus.1 = 2008, yr.plus.1 = 2012)
mildata$mil.expenditure.sipri[mildata$iso3c=="BDI"&mildata$year==2011] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "BDI", 2011, yr.minus.1 = 2008, yr.plus.1 = 2012)

##### cow personnel
# good

##### wmeat personnel
# 1962: COW and WMEAT personnel match 1963-1991, so use COW personnel estimates for 1962
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year==1962&iso3c=="BDI",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "BDI", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("BDI")
# y <- milper.viewer("BDI")

##### BEL ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1946-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "BEL", 1963, restricted = c(1946:1962)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "BEL", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1948: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "BEL", 1949, restricted = c(1946:1948)
)

##### cow personnel
# good

##### wmeat personnel
# 1945-1962: COW and WMEAT personnel match 1963-1975, so use COW personnel estimates for 1945-1962
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1945:1962)&iso3c=="BEL",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "BEL", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("BEL")
# y <- milper.viewer("BEL")

##### BEN ----------------------------------------------------------------------

##### cow expenditure
# 1960: replace 0 estimate with proportion from SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "mil.expenditure.sipri",
  "BEN", 1961, restricted = 1960
)
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow.alt",
  col_ref = "mil.expenditure.sipri",
  "BEN", 1961, restricted = 1960
)

# 1993: use average of WMEAT proportions for 1992 and 1994
mildata$mil.expenditure.cow[mildata$iso3c=="BEN"&mildata$year==1993] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "BEN", 1993)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="BEN"&mildata$year==1993] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "BEN", 1993)

# 2005: use average of WMEAT proportions for 2004 and 2006
mildata$mil.expenditure.cow[mildata$iso3c=="BEN"&mildata$year==2005] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "BEN", 2005)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="BEN"&mildata$year==2005] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "BEN", 2005)

##### wmeat expenditure
# 1960-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "BEN", 1963, restricted = c(1960:1962)
)

# 1982
mildata$mil.expenditure.wmeat[mildata$iso3c=="BEN"&mildata$year==1982] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "BEN", 1982)

# 1985
mildata$mil.expenditure.wmeat[mildata$iso3c=="BEN"&mildata$year==1985] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "BEN", 1985)

# 1987
mildata$mil.expenditure.wmeat[mildata$iso3c=="BEN"&mildata$year==1987] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "BEN", 1987)

# 1991
mildata$mil.expenditure.wmeat[mildata$iso3c=="BEN"&mildata$year==1991] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "BEN", 1991)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "BEN", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1991-1998
mildata$mil.expenditure.sipri[mildata$iso3c=="BEN"&mildata$year==1991] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW est", "BEN", 1991, yr.minus.1 = 1990, yr.plus.1 = 1999)
mildata$mil.expenditure.sipri[mildata$iso3c=="BEN"&mildata$year==1992] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW est", "BEN", 1992, yr.minus.1 = 1990, yr.plus.1 = 1999)
mildata$mil.expenditure.sipri[mildata$iso3c=="BEN"&mildata$year==1993] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW est", "BEN", 1993, yr.minus.1 = 1990, yr.plus.1 = 1999)
mildata$mil.expenditure.sipri[mildata$iso3c=="BEN"&mildata$year==1994] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW est", "BEN", 1994, yr.minus.1 = 1990, yr.plus.1 = 1999)
mildata$mil.expenditure.sipri[mildata$iso3c=="BEN"&mildata$year==1995] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW est", "BEN", 1995, yr.minus.1 = 1990, yr.plus.1 = 1999)
mildata$mil.expenditure.sipri[mildata$iso3c=="BEN"&mildata$year==1996] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW est", "BEN", 1996, yr.minus.1 = 1990, yr.plus.1 = 1999)
mildata$mil.expenditure.sipri[mildata$iso3c=="BEN"&mildata$year==1997] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW est", "BEN", 1997, yr.minus.1 = 1990, yr.plus.1 = 1999)
mildata$mil.expenditure.sipri[mildata$iso3c=="BEN"&mildata$year==1998] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW est", "BEN", 1998, yr.minus.1 = 1990, yr.plus.1 = 1999)

# 2007
mildata$mil.expenditure.sipri[mildata$iso3c=="BEN"&mildata$year==2007] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "BEN", 2007)

# 2009-2011
mildata$mil.expenditure.sipri[mildata$iso3c=="BEN"&mildata$year==2009] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "BEN", 2009, yr.minus.1 = 2008, yr.plus.1 = 2012)
mildata$mil.expenditure.sipri[mildata$iso3c=="BEN"&mildata$year==2010] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "BEN", 2010, yr.minus.1 = 2008, yr.plus.1 = 2012)
mildata$mil.expenditure.sipri[mildata$iso3c=="BEN"&mildata$year==2011] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "BEN", 2011, yr.minus.1 = 2008, yr.plus.1 = 2012)

##### cow personnel
# good

##### wmeat personnel
# 1960-1962: COW and WMEAT personnel match 1963-1971, so use COW personnel estimates for 1960-1962
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1960:1962)&iso3c=="BEN",mil.personnel.cow,mil.personnel.wmeat))

# 1977: COW and WMEAT personnel match 1973-1976 and 1978-1980, so use COW personnel estimates for 1977
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year==1977&iso3c=="BEN",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2013-14 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="BEN"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="BEN"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="BEN"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="BEN"&mildata$year==2018]

# x <- milexp.viewer("BEN")
# y <- milper.viewer("BEN")

##### BFA ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1960-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "BFA", 1963, restricted = c(1960:1962)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "BFA", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# good

##### cow personnel
# good

##### wmeat personnel
# 1960-1962: COW and WMEAT personnel match 1963-1998, so use COW personnel estimates for 1960-1962
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1960:1962)&iso3c=="BFA",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2012-13 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="BFA"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="BFA"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="BFA"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="BFA"&mildata$year==2018]

x <- milexp.viewer("BFA")
y <- milper.viewer("BFA")

##### BGD ----------------------------------------------------------------------

##### cow expenditure
# 1972: apply WMEAT growth rates to COW
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "mil.expenditure.wmeat",
  "BGD", 1973, restricted = 1972
)
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow.alt",
  col_ref = "mil.expenditure.wmeat",
  "BGD", 1973, restricted = 1972
)

# 1971
mildata <- mil_estimator_no_data_func(
  mildata,"BGD",
  metric = "mil.expenditure.cow",
  1971,1971
)
mildata <- mil_estimator_no_data_func(
  mildata,"BGD",
  metric = "mil.expenditure.cow.alt",
  1971,1971
)

##### wmeat expenditure
# 1971
mildata <- mil_estimator_no_data_func(
  mildata,"BGD",
  metric = "mil.expenditure.wmeat",
  1971,1971
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "BGD", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1972: apply WMEAT growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.wmeat",
  "BGD", 1973, restricted = 1972
)

# 1971
mildata <- mil_estimator_no_data_func(
  mildata,"BGD",
  metric = "mil.expenditure.sipri",
  1971,1971
)

##### cow personnel
# 1971
mildata <- mil_estimator_no_data_func(
  mildata,"BGD",
  metric = "mil.personnel.cow",
  1971,1971
)

##### wmeat personnel
# 1971
mildata <- mil_estimator_no_data_func(
  mildata,"BGD",
  metric = "mil.personnel.wmeat",
  1971,1971
)

# 2018-2019: 2010-11 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="BGD"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="BGD"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="BGD"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="BGD"&mildata$year==2018]

# x <- milexp.viewer("BGD")
# y <- milper.viewer("BGD")

##### BGR ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1946-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "BGR", 1963, restricted = c(1946:1962)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "BGR", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1963-1988: apply WMEAT growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.wmeat",
  "BGR", 1989, restricted = c(1963:1988)
)

# 1946-1962: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "BGR", 1963, restricted = c(1946:1962)
)

##### cow personnel
# good

##### wmeat personnel
# 1946-1962: COW and WMEAT personnel match 1963-1976, so use COW personnel estimates for 1945-1962
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1945:1962)&iso3c=="BGR",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2014-15 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="BGR"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="BGR"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="BGR"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="BGR"&mildata$year==2018]

# x <- milexp.viewer("BGR")
# y <- milper.viewer("BGR")

##### BHR ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "BHR", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# good

##### cow personnel
# good

##### wmeat personnel
# 1971: COW and WMEAT personnel match 1972-1973, so use COW personnel estimates for 1971
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year==1971&iso3c=="BHR",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2009-10 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="BHR"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="BHR"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="BHR"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="BHR"&mildata$year==2018]

# x <- milexp.viewer("BHR")
# y <- milper.viewer("BHR")

##### BHS ----------------------------------------------------------------------

##### cow expenditure
# 1993: use average of 1992 and 1994
mildata$mil.expenditure.cow[mildata$iso3c=="BHS"&mildata$year==1993] <- mean(c(
  mildata$mil.expenditure.cow[mildata$iso3c=="BHS"&mildata$year==1992],
  mildata$mil.expenditure.cow[mildata$iso3c=="BHS"&mildata$year==1994]
  ))
mildata$mil.expenditure.cow.alt[mildata$iso3c=="BHS"&mildata$year==1993] <- mean(c(
  mildata$mil.expenditure.cow.alt[mildata$iso3c=="BHS"&mildata$year==1992],
  mildata$mil.expenditure.cow.alt[mildata$iso3c=="BHS"&mildata$year==1994]
))

##### wmeat expenditure
# N/A

##### sipri expenditure
# N/A

##### cow personnel
# good

##### wmeat personnel
# N/A

# x <- milexp.viewer("BHS")
# y <- milper.viewer("BHS")

##### BIH ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1992-1998: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "BIH", 1999, restricted = c(1992:1998)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "BIH", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1992-2001: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "BIH", 2002, restricted = c(1992:2001)
)

##### cow personnel
# good

##### wmeat personnel
# 1993: COW and WMEAT personnel match 1992 and 1994-1996, so use COW personnel estimates for 1993
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year==1993&iso3c=="BIH",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2013-14 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="BIH"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="BIH"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="BIH"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="BIH"&mildata$year==2018]

# x <- milexp.viewer("BIH")
# y <- milper.viewer("BIH")

##### BLR ----------------------------------------------------------------------

##### cow expenditure
# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"BLR",
  metric = "mil.expenditure.cow",
  1991,1991
)
mildata <- mil_estimator_no_data_func(
  mildata,"BLR",
  metric = "mil.expenditure.cow.alt",
  1991,1991
)

##### wmeat expenditure
# 1992: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "BLR", 1993, restricted = 1992
)

# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"BLR",
  metric = "mil.expenditure.wmeat",
  1991,1991
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "BLR", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1992-1994: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "BLR", 1995, restricted = c(1992:1994)
)

# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"BLR",
  metric = "mil.expenditure.sipri",
  1991,1991
)

##### cow personnel
# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"BLR",
  metric = "mil.personnel.cow",
  1991,1991
)

# 1997-1998: wmeat shows no change, while cow has different values for 1996 and 1999; assume consistent growth between 1996 and 1999
blr.alpha <- (mildata$mil.personnel.cow[mildata$iso3c=="BLR"&mildata$year==1999]/mildata$mil.personnel.cow[mildata$iso3c=="BLR"&mildata$year==1996])^(1/3)

mildata$mil.personnel.cow[mildata$iso3c=="BLR"&mildata$year==1997] <- mildata$mil.personnel.cow[mildata$iso3c=="BLR"&mildata$year==1996] * blr.alpha
mildata$mil.personnel.cow[mildata$iso3c=="BLR"&mildata$year==1998] <- mildata$mil.personnel.cow[mildata$iso3c=="BLR"&mildata$year==1997] * blr.alpha

##### wmeat personnel
# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"BLR",
  metric = "mil.personnel.wmeat",
  1991,1991
)

# 2018-2019: 2013-14 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="BLR"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="BLR"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="BLR"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="BLR"&mildata$year==2018]

# x <- milexp.viewer("BLR")
# y <- milper.viewer("BLR")

##### BLZ ----------------------------------------------------------------------

##### cow expenditure
# 1992: use average of WMEAT proportions for 1991 and 1993
mildata$mil.expenditure.cow[mildata$iso3c=="BLZ"&mildata$year==1992] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "BLZ", 1992)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="BLZ"&mildata$year==1992] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "BLZ", 1992)

# 2008: use average of WMEAT proportions for 2007 and 2009
mildata$mil.expenditure.cow[mildata$iso3c=="BLZ"&mildata$year==2008] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "BLZ", 2008)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="BLZ"&mildata$year==2008] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "BLZ", 2008)

##### wmeat expenditure
# 1981-1983: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "BLZ", 1984, restricted = c(1981:1983)
)

# 1986-1987
mildata$mil.expenditure.wmeat[mildata$iso3c=="BLZ"&mildata$year==1986] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "BLZ", 1986, yr.minus.1 = 1985, yr.plus.1 = 1988)
mildata$mil.expenditure.wmeat[mildata$iso3c=="BLZ"&mildata$year==1987] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "BLZ", 1987, yr.minus.1 = 1985, yr.plus.1 = 1988)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "BLZ", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1986-1988

# 1986-1987
mildata$mil.expenditure.sipri[mildata$iso3c=="BLZ"&mildata$year==1986] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "BLZ", 1986, yr.minus.1 = 1985, yr.plus.1 = 1989)
mildata$mil.expenditure.sipri[mildata$iso3c=="BLZ"&mildata$year==1987] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "BLZ", 1987, yr.minus.1 = 1985, yr.plus.1 = 1989)
mildata$mil.expenditure.sipri[mildata$iso3c=="BLZ"&mildata$year==1988] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "BLZ", 1988, yr.minus.1 = 1985, yr.plus.1 = 1989)

# 1998-1999
mildata$mil.expenditure.sipri[mildata$iso3c=="BLZ"&mildata$year==1998] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "BLZ", 1998, yr.minus.1 = 1997, yr.plus.1 = 2000)
mildata$mil.expenditure.sipri[mildata$iso3c=="BLZ"&mildata$year==1999] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "BLZ", 1999, yr.minus.1 = 1997, yr.plus.1 = 2000)

##### cow personnel
# good

##### wmeat personnel
# 1981-1983: COW and WMEAT personnel match 1963-2016, so use COW personnel estimates for 1981-1983
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1981:1983)&iso3c=="BLZ",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "BLZ", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("BLZ")
# y <- milper.viewer("BLZ")

##### BOL ----------------------------------------------------------------------

##### cow expenditure
# 1949: assume consistent growth between 1948 and 1950
bol.alpha.1 <- (mildata$mil.expenditure.cow[mildata$iso3c=="BOL"&mildata$year==1950]/mildata$mil.expenditure.cow[mildata$iso3c=="BOL"&mildata$year==1948])^(1/2)

mildata$mil.expenditure.cow[mildata$iso3c=="BOL"&mildata$year==1949] <- mildata$mil.expenditure.cow[mildata$iso3c=="BOL"&mildata$year==1948] * bol.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="BOL"&mildata$year==1949] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="BOL"&mildata$year==1948] * bol.alpha.1

# 1951: assume consistent growth between 1950 and 1952
bol.alpha.2 <- (mildata$mil.expenditure.cow[mildata$iso3c=="BOL"&mildata$year==1952]/mildata$mil.expenditure.cow[mildata$iso3c=="BOL"&mildata$year==1950])^(1/2)

mildata$mil.expenditure.cow[mildata$iso3c=="BOL"&mildata$year==1951] <- mildata$mil.expenditure.cow[mildata$iso3c=="BOL"&mildata$year==1950] * bol.alpha.2
mildata$mil.expenditure.cow.alt[mildata$iso3c=="BOL"&mildata$year==1951] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="BOL"&mildata$year==1950] * bol.alpha.2

##### wmeat expenditure
# 1952-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "BOL", 1963, restricted = c(1952:1962)
)

# 1946-1948; 1950: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "BOL", 1952, restricted = c(1946:1948, 1950)
)

# 1949: assume consistent growth between 1948 and 1950
bol.alpha.3 <- (mildata$mil.expenditure.wmeat[mildata$iso3c=="BOL"&mildata$year==1950]/mildata$mil.expenditure.wmeat[mildata$iso3c=="BOL"&mildata$year==1948])^(1/2)

mildata$mil.expenditure.wmeat[mildata$iso3c=="BOL"&mildata$year==1949] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="BOL"&mildata$year==1948] * bol.alpha.3

# 1951: assume consistent growth between 1950 and 1952
bol.alpha.4 <- (mildata$mil.expenditure.wmeat[mildata$iso3c=="BOL"&mildata$year==1952]/mildata$mil.expenditure.wmeat[mildata$iso3c=="BOL"&mildata$year==1950])^(1/2)

mildata$mil.expenditure.wmeat[mildata$iso3c=="BOL"&mildata$year==1951] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="BOL"&mildata$year==1950] * bol.alpha.4

# 1983
mildata$mil.expenditure.wmeat[mildata$iso3c=="BOL"&mildata$year==1983] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "BOL", 1983)

# 1985
mildata$mil.expenditure.wmeat[mildata$iso3c=="BOL"&mildata$year==1985] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "BOL", 1985)

# 2018-2019: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "BOL", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1952: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "BOL", 1953, restricted = c(1946:1952)
)

# 1954
mildata$mil.expenditure.sipri[mildata$iso3c=="BOL"&mildata$year==1954] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "BOL", 1954)

# 1958-1960
mildata$mil.expenditure.sipri[mildata$iso3c=="BOL"&mildata$year==1958] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "BOL", 1958, yr.minus.1 = 1957, yr.plus.1 = 1961)
mildata$mil.expenditure.sipri[mildata$iso3c=="BOL"&mildata$year==1959] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "BOL", 1959, yr.minus.1 = 1957, yr.plus.1 = 1961)
mildata$mil.expenditure.sipri[mildata$iso3c=="BOL"&mildata$year==1960] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "BOL", 1960, yr.minus.1 = 1957, yr.plus.1 = 1961)

# 1964-1967
mildata$mil.expenditure.sipri[mildata$iso3c=="BOL"&mildata$year==1964] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "BOL", 1964, yr.minus.1 = 1963, yr.plus.1 = 1968)
mildata$mil.expenditure.sipri[mildata$iso3c=="BOL"&mildata$year==1965] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "BOL", 1965, yr.minus.1 = 1963, yr.plus.1 = 1968)
mildata$mil.expenditure.sipri[mildata$iso3c=="BOL"&mildata$year==1966] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "BOL", 1966, yr.minus.1 = 1963, yr.plus.1 = 1968)
mildata$mil.expenditure.sipri[mildata$iso3c=="BOL"&mildata$year==1967] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "BOL", 1967, yr.minus.1 = 1963, yr.plus.1 = 1968)

##### cow personnel
# good

##### wmeat personnel
# 1946-1962: COW and WMEAT personnel match 1963-1999, so use COW personnel estimates for 1945-1962
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1945:1962)&iso3c=="BOL",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="BOL"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="BOL"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="BOL"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="BOL"&mildata$year==2018]

# x <- milexp.viewer("BOL")
# y <- milper.viewer("BOL")

##### BRA ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1946-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "BRA", 1963, restricted = c(1946:1962)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "BRA", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1952: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "BRA", 2017, restricted = c(2018:2019)
)

# 1954-1956
mildata$mil.expenditure.sipri[mildata$iso3c=="BRA"&mildata$year==1954] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "BRA", 1954, yr.minus.1 = 1953, yr.plus.1 = 1957)
mildata$mil.expenditure.sipri[mildata$iso3c=="BRA"&mildata$year==1955] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "BRA", 1955, yr.minus.1 = 1953, yr.plus.1 = 1957)
mildata$mil.expenditure.wmeat[mildata$iso3c=="BRA"&mildata$year==1956] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "BRA", 1956, yr.minus.1 = 1953, yr.plus.1 = 1957)

##### cow personnel
# good

##### wmeat personnel
# 1946-1962: COW and WMEAT personnel match 1963-1993, so use COW personnel estimates for 1945-1962
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1945:1962)&iso3c=="BRA",mil.personnel.cow,mil.personnel.wmeat))

# WMEAT 1996 lists Brazil's 1994 armed forces at 296,000, as opposed to the WMEAT 1995 report's 196,000
mildata$mil.personnel.wmeat[mildata$iso3c=="BRA"&mildata$year==1994] <- 296000

# 2018-2019: 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="BRA"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="BRA"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="BRA"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="BRA"&mildata$year==2018]

# x <- milexp.viewer("BRA")
# y <- milper.viewer("BRA")

##### BRB ----------------------------------------------------------------------

##### cow expenditure
# 1991-1992: assume consistent growth between 1990 and 1993
brb.alpha.1 <- (mildata$mil.expenditure.cow[mildata$iso3c=="BRB"&mildata$year==1993]/mildata$mil.expenditure.cow[mildata$iso3c=="BRB"&mildata$year==1990])^(1/3)

mildata$mil.expenditure.cow[mildata$iso3c=="BRB"&mildata$year==1991] <- mildata$mil.expenditure.cow[mildata$iso3c=="BRB"&mildata$year==1990] * brb.alpha.1
mildata$mil.expenditure.cow[mildata$iso3c=="BRB"&mildata$year==1992] <- mildata$mil.expenditure.cow[mildata$iso3c=="BRB"&mildata$year==1991] * brb.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="BRB"&mildata$year==1991] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="BRB"&mildata$year==1990] * brb.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="BRB"&mildata$year==1992] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="BRB"&mildata$year==1991] * brb.alpha.1

##### wmeat expenditure
# 1966-1972: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "BRB", 1973, restricted = c(1966:1972)
)

# 1975-1976 expenditure is listed as $1,000,000 current US$ in the WMEAT 1987 report
mildata$mil.expenditure.wmeat[mildata$iso3c=="BRB"&mildata$year==1975] <- 1000000 * inflation_table$multiplier[inflation_table$year==1975]
mildata$mil.expenditure.wmeat[mildata$iso3c=="BRB"&mildata$year==1976] <- 1000000 * inflation_table$multiplier[inflation_table$year==1976]

# 1983
mildata$mil.expenditure.wmeat[mildata$iso3c=="BRB"&mildata$year==1983] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "BRB", 1983)

# 1990
mildata$mil.expenditure.wmeat[mildata$iso3c=="BRB"&mildata$year==1990] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "BRB", 1990, yr.minus.1 = 1989, yr.plus.1 = 1993)

# 1991-1992: assume consistent growth between 1990 and 1993
brb.alpha.2 <- (mildata$mil.expenditure.wmeat[mildata$iso3c=="BRB"&mildata$year==1993]/mildata$mil.expenditure.wmeat[mildata$iso3c=="BRB"&mildata$year==1990])^(1/3)

mildata$mil.expenditure.wmeat[mildata$iso3c=="BRB"&mildata$year==1991] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="BRB"&mildata$year==1990] * brb.alpha.2
mildata$mil.expenditure.wmeat[mildata$iso3c=="BRB"&mildata$year==1992] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="BRB"&mildata$year==1991] * brb.alpha.2

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "BRB", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# N/A

##### cow personnel
# good

##### wmeat personnel
# 1966-1972: COW and WMEAT personnel match 1963-1998, so use COW personnel estimates for 1966-1972
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1966:1972)&iso3c=="BRB",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: COW and WMEAT personnel match 2004-2017, so use COW personnel estimates for 2018-2019
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(2018:2019)&iso3c=="BRB",mil.personnel.cow,mil.personnel.wmeat))

# x <- milexp.viewer("BRB")
# y <- milper.viewer("BRB")

##### BRD ----------------------------------------------------------------------

##### cow est
# 1953-1954: apply SIPRI growth rates to COW
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "mil.expenditure.sipri",
  "BRD", 1955, restricted = c(1953:1954)
)
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow.alt",
  col_ref = "mil.expenditure.sipri",
  "BRD", 1955, restricted = c(1953:1954)
)

# 1949-1952
mildata <- mil_estimator_no_data_func(
  mildata,"BRD",
  metric = "mil.expenditure.cow",
  1952,1949
)
mildata <- mil_estimator_no_data_func(
  mildata,"BRD",
  metric = "mil.expenditure.cow.alt",
  1952,1949
)

##### wmeat expenditure
# 1955-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "BRD", 1963, restricted = c(1955:1962)
)

# 1953-1954: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "BRD", 1955, restricted = c(1953:1954)
)

# 1949-1952
mildata <- mil_estimator_no_data_func(
  mildata,"BRD",
  metric = "mil.expenditure.wmeat",
  1952,1949
)

##### sipri expenditure
# 1949-1952
mildata <- mil_estimator_no_data_func(
  mildata,"BRD",
  metric = "mil.expenditure.sipri",
  1952,1949
)

##### cow personnel
# 1949-1954
mildata <- mil_estimator_no_data_func(
  mildata,"BRD",
  metric = "mil.personnel.cow",
  1954,1949
)

##### wmeat personnel
# 1955-1962: COW and WMEAT personnel match 1963-1981, so use COW personnel estimates for 1955-1962
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1955:1962)&iso3c=="BRD",mil.personnel.cow,mil.personnel.wmeat))

# 1949-1954
mildata <- mil_estimator_no_data_func(
  mildata,"BRD",
  metric = "mil.personnel.wmeat",
  1954,1949
)

# x <- milexp.viewer("BRD")
# y <- milper.viewer("BRD")

##### BRN ----------------------------------------------------------------------

##### cow expenditure
# 1990-1991
mildata$mil.expenditure.cow[mildata$iso3c=="BRN"&mildata$year==1990] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "BRN", 1990, yr.minus.1 = 1989, yr.plus.1 = 1992)
mildata$mil.expenditure.cow[mildata$iso3c=="BRN"&mildata$year==1991] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "BRN", 1991, yr.minus.1 = 1989, yr.plus.1 = 1992)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="BRN"&mildata$year==1990] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "BRN", 1990, yr.minus.1 = 1989, yr.plus.1 = 1992)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="BRN"&mildata$year==1991] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "BRN", 1991, yr.minus.1 = 1989, yr.plus.1 = 1992)

##### wmeat expenditure
# 1991
mildata$mil.expenditure.wmeat[mildata$iso3c=="BRN"&mildata$year==1991] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "BRN", 1991)

# 1993
mildata$mil.expenditure.wmeat[mildata$iso3c=="BRN"&mildata$year==1993] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "BRN", 1993)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "BRN", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# good

##### cow personnel
# good

##### wmeat personnel
# 1984-1985: COW and WMEAT personnel match 1986-1993, so use COW personnel estimates for 1984-1985
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(mildata$year %in% c(1984:1985)&mildata$iso3c=="BRN",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2006-07 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="BRN"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="BRN"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="BRN"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="BRN"&mildata$year==2018]

# x <- milexp.viewer("BRN")
# y <- milper.viewer("BRN")

##### BTN(x) ----------------------------------------------------------------------
# Note: coded as becoming a country in 1971

##### cow expenditure
# 1971-1997
mildata <- mil_estimator_no_data_func(
  mildata,"BTN",
  metric = "mil.expenditure.cow",
  1997,1971
)

# 2002-2019

##### wmeat expenditure
# 1971-1998
# 2006-2019

##### sipri expenditure
# N/A

##### cow personnel
# 2001-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.cow",
  col_ref = "mil.personnel.wmeat",
  "BTN", 2000, restricted = c(2001:2019)
)

##### wmeat personnel
# 1971-1991: COW and WMEAT personnel match 1992-1998 so use COW personnel estimates for 1971-1991
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1971:1991)&iso3c=="BTN",mil.personnel.cow,mil.personnel.wmeat))

x <- milexp.viewer("BTN")
# y <- milper.viewer("BTN")

##### BWA ----------------------------------------------------------------------

##### cow expenditure
# 1992: estimate COW 1992 as equidistant point based on WMEAT 1991-1993 values
mildata$mil.expenditure.cow[mildata$iso3c=="BWA"&mildata$year==1992] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "BWA", 1992)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="BWA"&mildata$year==1992] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "BWA", 1992)

# 2013-2019 (non-alt): use % change of IISS estimates
mildata <- mil_expenditure_growth_estimator_func(
  df = mildata, col_missing = "mil.expenditure.cow",
  col_ref = "defense.spending.iiss", iso = "BWA",
  yr = 2012, restricted = c(2013:2019))

## remove estimate flag for 2013-2019
mildata <- mildata %>%
  dplyr::mutate(mil.expenditure.cow.est.flag  = dplyr::case_when(
    iso3c == "BWA" & year %in% c(2012:2013) ~ 0,
    .default = mil.expenditure.cow.est.flag
  ))

##### wmeat expenditure
# 1966-1972: apply COW estimates to WMEAT
mildata <- mildata %>%
  dplyr::mutate(mil.expenditure.wmeat = ifelse(year %in% c(1966:1972)&iso3c=="BWA",mil.expenditure.cow,mil.expenditure.wmeat))

# 2018-2019: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "BWA", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1966-1976: apply COW estimates to SIPRI
mildata <- mildata %>%
  dplyr::mutate(mil.expenditure.sipri = ifelse(year %in% c(1966:1976)&iso3c=="BWA",mil.expenditure.cow,mil.expenditure.sipri))

##### cow personnel
# good

##### wmeat personnel
# 1966-1972: COW and WMEAT personnel match 1972-1991, so use COW personnel estimates for 1966-1972
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1966:1972)&iso3c=="BWA",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: COW and WMEAT personnel match 2002-2017, so use COW personnel estimates for 2018-2019
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(2018:2019)&iso3c=="BWA",mil.personnel.cow,mil.personnel.wmeat))

# x <- milexp.viewer("BWA")
# y <- milper.viewer("BWA")

##### CAF ----------------------------------------------------------------------

##### cow expenditure
# 1991: apply SIPRI growth rates to COW
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "mil.expenditure.sipri",
  "CAF", 1992, restricted = 1991
)
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow.alt",
  col_ref = "mil.expenditure.sipri",
  "CAF", 1992, restricted = 1991
)

# 2012-2019: use IISS growth rates
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "defense.spending.iiss",
  "CAF", 2011, restricted = c(2012:2019)
)

## remove estimate flag for 2012-2019
mildata <- mildata %>%
  dplyr::mutate(mil.expenditure.cow.est.flag  = dplyr::case_when(
    iso3c == "CAF" & year %in% c(2012:2019) ~ 0,
    .default = mil.expenditure.cow.est.flag
  ))

##### wmeat expenditure
# 1960-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "CAF", 1963, restricted = c(1960:1962)
)

# 1984-1986
mildata$mil.expenditure.wmeat[mildata$iso3c=="CAF"&mildata$year==1984] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "CAF", 1984, yr.minus.1 = 1983, yr.plus.1 = 1987)
mildata$mil.expenditure.wmeat[mildata$iso3c=="CAF"&mildata$year==1985] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "CAF", 1985, yr.minus.1 = 1983, yr.plus.1 = 1987)
mildata$mil.expenditure.wmeat[mildata$iso3c=="CAF"&mildata$year==1986] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "CAF", 1986, yr.minus.1 = 1983, yr.plus.1 = 1987)

# 1988
mildata$mil.expenditure.wmeat[mildata$iso3c=="CAF"&mildata$year==1988] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "CAF", 1988)

# 1991: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "CAF", 1992, restricted = 1991
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "CAF", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1960-1990: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "CAF", 1991, restricted = c(1960:1990)
)

# 1997-2001
mildata$mil.expenditure.sipri[mildata$iso3c=="CAF"&mildata$year==1997] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "CAF", 1997, yr.minus.1 = 1996, yr.plus.1 = 2002)
mildata$mil.expenditure.sipri[mildata$iso3c=="CAF"&mildata$year==1998] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "CAF", 1998, yr.minus.1 = 1996, yr.plus.1 = 2002)
mildata$mil.expenditure.sipri[mildata$iso3c=="CAF"&mildata$year==1999] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "CAF", 1999, yr.minus.1 = 1996, yr.plus.1 = 2002)
mildata$mil.expenditure.sipri[mildata$iso3c=="CAF"&mildata$year==2000] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "CAF", 2000, yr.minus.1 = 1996, yr.plus.1 = 2002)
mildata$mil.expenditure.sipri[mildata$iso3c=="CAF"&mildata$year==2001] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "CAF", 2001, yr.minus.1 = 1996, yr.plus.1 = 2002)

# 2006
mildata$mil.expenditure.sipri[mildata$iso3c=="CAF"&mildata$year==2006] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "CAF", 2006)

##### cow personnel
# good

##### wmeat personnel
# 1960-1963: COW and WMEAT personnel match 1964-1991, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1960:1963)&iso3c=="CAF",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2014-15 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="CAF"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="CAF"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="CAF"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="CAF"&mildata$year==2018]

# x <- milexp.viewer("CAF")
# y <- milper.viewer("CAF")

##### CAN ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1946-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "CAN", 1963, restricted = c(1946:1963)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "CAN", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1949: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "CAN", 1950, restricted = c(1946:1949)
)

##### cow personnel
# good

##### wmeat personnel
# 1945-1962: COW and WMEAT personnel match 1963-1976, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1945:1962)&iso3c=="CAN",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "CAN", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("CAN")
# y <- milper.viewer("CAN")

##### CHE ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1946-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "CHE", 1963, restricted = c(1946:1962)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "CHE", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1956: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "CHE", 1957, restricted = c(1946:1956)
)

##### cow personnel
# good

##### wmeat personnel
# 1945-1962: COW and WMEAT personnel match 1963-1976, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1945:1962)&iso3c=="CHE",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2016-2017 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="CHE"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="CHE"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="CHE"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="CHE"&mildata$year==2018]

# x <- milexp.viewer("CHE")
# y <- milper.viewer("CHE")

##### CHL ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1946-1962: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "CHL", 1963, restricted = c(1946:1962)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "CHL", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1949: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "CHL", 1950, restricted = c(1946:1949)
)

##### cow personnel
# good

##### wmeat personnel
# 1946-1962: COW and WMEAT personnel match 1963-1996, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1946:1962)&iso3c=="CHL",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: COW and WMEAT personnel match 2016-2017, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(2018:2019)&iso3c=="CHL",mil.personnel.cow,mil.personnel.wmeat))

x <- milexp.viewer("CHL")
# y <- milper.viewer("CHL")

##### CHN ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1946-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "CHN", 1963, restricted = c(1946:1962)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "CHN", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1988: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "CHN", 1989, restricted = c(1946:1988)
)

##### cow personnel
# good

##### wmeat personnel
# 1946-1962: COW and WMEAT personnel match 1963-1964, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1946:1962)&iso3c=="CHN",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "CHN", 2017, restricted = c(2018:2019)
)

x <- milexp.viewer("CHN")
# y <- milper.viewer("CHN")

##### CIV ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1960-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "CIV", 1963, restricted = c(1960:1962)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "CIV", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1960-1961: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "CIV", 1962, restricted = c(1960:1961)
)

# 1995
mildata$mil.expenditure.sipri[mildata$iso3c=="CIV"&mildata$year==1995] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "CIV", 1995)

# 1998-2002
mildata$mil.expenditure.sipri[mildata$iso3c=="CIV"&mildata$year==1998] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "CIV", 1998, yr.minus.1 = 1997, yr.plus.1 = 2003)
mildata$mil.expenditure.sipri[mildata$iso3c=="CIV"&mildata$year==1999] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "CIV", 1999, yr.minus.1 = 1997, yr.plus.1 = 2003)
mildata$mil.expenditure.sipri[mildata$iso3c=="CIV"&mildata$year==2000] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "CIV", 2000, yr.minus.1 = 1997, yr.plus.1 = 2003)
mildata$mil.expenditure.sipri[mildata$iso3c=="CIV"&mildata$year==2001] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "CIV", 2001, yr.minus.1 = 1997, yr.plus.1 = 2003)
mildata$mil.expenditure.sipri[mildata$iso3c=="CIV"&mildata$year==2002] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "CIV", 2002, yr.minus.1 = 1997, yr.plus.1 = 2003)

##### cow personnel
# 2016-2020
# apply IISS estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.cow = ifelse(year %in% c(2016:2020)&iso3c=="CIV",active.armed.forces.iiss,mil.personnel.cow))

# 2012-2015
# WMEAT estimates largely match COW estimates before and after the gap, so apply
# WMEAT pattern to COW estimates
civ.wmeat.mil.personnel.2011 <- mildata$armed.personnel.wmeat[mildata$iso3c=="CIV"&mildata$year==2011] # min
civ.wmeat.mil.personnel.2016 <- mildata$armed.personnel.wmeat[mildata$iso3c=="CIV"&mildata$year==2016] # max
civ.cow.mil.personnel.2011 <- mildata$mil.personnel.cow[mildata$iso3c=="CIV"&mildata$year==2011] # min
civ.cow.mil.personnel.2016 <- mildata$mil.personnel.cow[mildata$iso3c=="CIV"&mildata$year==2016] # max
civ.cow.mil.personnel.diff <- civ.cow.mil.personnel.2016 - civ.cow.mil.personnel.2011 # diff

for(y in 2012:2015){
  
  mildata$mil.personnel.cow[mildata$iso3c=="CIV"&mildata$year==y] <- civ.cow.mil.personnel.2011 +
    (civ.cow.mil.personnel.diff*(mildata$armed.personnel.wmeat[mildata$iso3c=="CIV"&mildata$year==y]-civ.wmeat.mil.personnel.2011)/
       (civ.wmeat.mil.personnel.2016-civ.wmeat.mil.personnel.2011))
  
  }

##### wmeat personnel
# 1960-1962: COW and WMEAT personnel match 1963-1980, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1960:1962)&iso3c=="CIV",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "CIV", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("CIV")
# y <- milper.viewer("CIV")

##### CMR ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1960-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "CMR", 1963, restricted = c(1960:1962)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "CMR", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1960-1969: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "CMR", 1970, restricted = c(1960:1969)
)

##### cow personnel
# good

##### wmeat personnel
# 1960-1962: COW and WMEAT personnel match 1963-1994, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1960:1962)&iso3c=="CMR",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "CMR", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("CMR")
# y <- milper.viewer("CMR")

##### COD ----------------------------------------------------------------------

##### cow expenditure
# 1992
mildata$mil.expenditure.cow[mildata$iso3c=="COD"&mildata$year==1992] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "COD", 1992)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="COD"&mildata$year==1992] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "COD", 1992)

# 2002
mildata$mil.expenditure.cow[mildata$iso3c=="COD"&mildata$year==2002] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "COD", 2002)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="COD"&mildata$year==2002] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "COD", 2002)

##### wmeat expenditure
# 1960-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "COD", 1963, restricted = c(1960:1962)
)

# 1987
mildata$mil.expenditure.wmeat[mildata$iso3c=="COD"&mildata$year==1987] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "COD", 1987)

# 1989-1991
mildata$mil.expenditure.wmeat[mildata$iso3c=="COD"&mildata$year==1989] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "COD", 1989, yr.minus.1 = 1988, yr.plus.1 = 1992)
mildata$mil.expenditure.wmeat[mildata$iso3c=="COD"&mildata$year==1990] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "COD", 1990, yr.minus.1 = 1988, yr.plus.1 = 1992)
mildata$mil.expenditure.wmeat[mildata$iso3c=="COD"&mildata$year==1991] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "COD", 1991, yr.minus.1 = 1988, yr.plus.1 = 1992)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "COD", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1960-1962: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "COD", 1963, restricted = c(1960:1962)
)

# 1994-1995
mildata$mil.expenditure.sipri[mildata$iso3c=="COD"&mildata$year==1994] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "COD", 1994, yr.minus.1 = 1993, yr.plus.1 = 1996)
mildata$mil.expenditure.sipri[mildata$iso3c=="COD"&mildata$year==1995] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "COD", 1995, yr.minus.1 = 1993, yr.plus.1 = 1996)

# 2001-2002
mildata$mil.expenditure.sipri[mildata$iso3c=="COD"&mildata$year==2001] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "COD", 2001, yr.minus.1 = 2000, yr.plus.1 = 2003)
mildata$mil.expenditure.sipri[mildata$iso3c=="COD"&mildata$year==2002] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "COD", 2002, yr.minus.1 = 2000, yr.plus.1 = 2003)

##### cow personnel
# good

##### wmeat personnel
# 1960-1962: COW and WMEAT personnel match 1963-1991, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1960:1962)&iso3c=="COD",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2013-2014 - 2016-2017 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="COD"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="COD"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="COD"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="COD"&mildata$year==2018]

# x <- milexp.viewer("COD")
# y <- milper.viewer("COD")

##### COG ----------------------------------------------------------------------

##### cow expenditure
# 1991: use average of WMEAT proportions for 1990 and 1992
mildata$mil.expenditure.cow[mildata$iso3c=="COG"&mildata$year==1991] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "COG", 1991)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="COG"&mildata$year==1991] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "COG", 1991)

# 1993: estimate COW 1993 as equidistant point based on WMEAT 1992-1994 values
mildata$mil.expenditure.cow[mildata$iso3c=="COG"&mildata$year==1993] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "COG", 1993)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="COG"&mildata$year==1993] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "COG", 1993)

# 2002: use average of WMEAT proportions for 2001 and 2003
mildata$mil.expenditure.cow[mildata$iso3c=="COG"&mildata$year==2002] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "COG", 2002)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="COG"&mildata$year==2002] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "COG", 2002)

##### wmeat expenditure
# 1960-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "COG", 1963, restricted = c(1960:1962)
)

# 1979
mildata$mil.expenditure.wmeat[mildata$iso3c=="COG"&mildata$year==1979] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "COG", 1979)

# 1984
mildata$mil.expenditure.wmeat[mildata$iso3c=="COG"&mildata$year==1984] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "COG", 1984)

# 1988-1989
mildata$mil.expenditure.wmeat[mildata$iso3c=="COG"&mildata$year==1988] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "COG", 1988, yr.minus.1 = 1987, yr.plus.1 = 1990)
mildata$mil.expenditure.wmeat[mildata$iso3c=="COG"&mildata$year==1989] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "COG", 1989, yr.minus.1 = 1987, yr.plus.1 = 1990)

# 1996
mildata$mil.expenditure.wmeat[mildata$iso3c=="COG"&mildata$year==1996] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "COG", 1996)

# 2018-2019: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "COG", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1960-1969: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "COG", 1970, restricted = c(1960:1969)
)

# 1983
mildata$mil.expenditure.sipri[mildata$iso3c=="COG"&mildata$year==1983] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "COG", 1983)

# 1986
mildata$mil.expenditure.sipri[mildata$iso3c=="COG"&mildata$year==1986] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "COG", 1986)

# 1988-1991
mildata$mil.expenditure.sipri[mildata$iso3c=="COG"&mildata$year==1988] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW est", "COG", 1988, yr.minus.1 = 1987, yr.plus.1 = 1992)
mildata$mil.expenditure.sipri[mildata$iso3c=="COG"&mildata$year==1989] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW est", "COG", 1989, yr.minus.1 = 1987, yr.plus.1 = 1992)
mildata$mil.expenditure.sipri[mildata$iso3c=="COG"&mildata$year==1990] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW est", "COG", 1990, yr.minus.1 = 1987, yr.plus.1 = 1992)
mildata$mil.expenditure.sipri[mildata$iso3c=="COG"&mildata$year==1991] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW est", "COG", 1991, yr.minus.1 = 1987, yr.plus.1 = 1992)

# 1994-2000
mildata$mil.expenditure.sipri[mildata$iso3c=="COG"&mildata$year==1994] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "COG", 1994, yr.minus.1 = 1992, yr.plus.1 = 2001)
mildata$mil.expenditure.sipri[mildata$iso3c=="COG"&mildata$year==1995] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "COG", 1995, yr.minus.1 = 1992, yr.plus.1 = 2001)
mildata$mil.expenditure.sipri[mildata$iso3c=="COG"&mildata$year==1996] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "COG", 1996, yr.minus.1 = 1992, yr.plus.1 = 2001)
mildata$mil.expenditure.sipri[mildata$iso3c=="COG"&mildata$year==1997] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "COG", 1997, yr.minus.1 = 1992, yr.plus.1 = 2001)
mildata$mil.expenditure.sipri[mildata$iso3c=="COG"&mildata$year==1998] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "COG", 1998, yr.minus.1 = 1992, yr.plus.1 = 2001)
mildata$mil.expenditure.sipri[mildata$iso3c=="COG"&mildata$year==1999] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "COG", 1999, yr.minus.1 = 1992, yr.plus.1 = 2001)
mildata$mil.expenditure.sipri[mildata$iso3c=="COG"&mildata$year==2000] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "COG", 2000, yr.minus.1 = 1992, yr.plus.1 = 2001)

# 2009
mildata$mil.expenditure.sipri[mildata$iso3c=="COG"&mildata$year==2009] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "COG", 2009)

# 2011-2012
mildata$mil.expenditure.sipri[mildata$iso3c=="COG"&mildata$year==2011] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "IISS", "COG", 2011, yr.minus.1 = 2010, yr.plus.1 = 2013)
mildata$mil.expenditure.sipri[mildata$iso3c=="COG"&mildata$year==2012] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "IISS", "COG", 2012, yr.minus.1 = 2010, yr.plus.1 = 2013)

# 2015
mildata$mil.expenditure.sipri[mildata$iso3c=="COG"&mildata$year==2015] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "IISS", "COG", 2015)

##### cow personnel
# good

##### wmeat personnel
# 1960-1962: COW and WMEAT personnel match 1963-1976, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1960:1962)&iso3c=="COG",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 1999-2000 - 2016-2017 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="COG"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="COG"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="COG"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="COG"&mildata$year==2018]

# x <- milexp.viewer("COG")
# y <- milper.viewer("COG")

##### COL ----------------------------------------------------------------------

##### cow expenditure
# 2013-2019 (non-alt): use IISS growth rates
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "defense.spending.iiss",
  "COL", 2012, restricted = c(2013:2019)
)

## remove estimate flag for 2013-2019
mildata <- mildata %>%
  dplyr::mutate(mil.expenditure.cow.est.flag  = dplyr::case_when(
    iso3c == "COL" & year %in% c(2013:2019) ~ 0,
    .default = mil.expenditure.cow.est.flag
  ))

##### wmeat expenditure
# 1946-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "COL", 1963, restricted = c(1946:1962)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "COL", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1957: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "COL", 1958, restricted = c(1946:1957)
)

##### cow personnel
# good

##### wmeat personnel
# 1945-1962: COW and WMEAT personnel match 1963-1998, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1945:1962)&iso3c=="COL",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "COL", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("COL")
# y <- milper.viewer("COL")

##### COM(x) ----------------------------------------------------------------------

##### cow expenditure
# 1976-1979
# 1983
# 1985-1986
# 1988-2019

##### wmeat expenditure
# N/A

##### sipri expenditure
# N/A

##### cow personnel
# 1977-1978
# 1993-2019

##### wmeat personnel
# N/A

x <- milexp.viewer("COM")
y <- milper.viewer("COM")

##### CPV ----------------------------------------------------------------------

# WMEAT 1992 provides estimates for CPV from 1982-1990
mildata$mil.expenditure.wmeat[mildata$iso3c=="CPV"&mildata$year==1982] <- 0
mildata$mil.expenditure.wmeat[mildata$iso3c=="CPV"&mildata$year==1983] <- 0
mildata$mil.expenditure.wmeat[mildata$iso3c=="CPV"&mildata$year==1984] <- 0
mildata$mil.expenditure.wmeat[mildata$iso3c=="CPV"&mildata$year==1985] <- 10000000 * inflation_table$multiplier[inflation_table$year==1985]
mildata$mil.expenditure.wmeat[mildata$iso3c=="CPV"&mildata$year==1986] <- 5000000 * inflation_table$multiplier[inflation_table$year==1986]
mildata$mil.expenditure.wmeat[mildata$iso3c=="CPV"&mildata$year==1987] <- 5000000 * inflation_table$multiplier[inflation_table$year==1987]
mildata$mil.expenditure.wmeat[mildata$iso3c=="CPV"&mildata$year==1988] <- 5000000 * inflation_table$multiplier[inflation_table$year==1988]
mildata$mil.expenditure.wmeat[mildata$iso3c=="CPV"&mildata$year==1989] <- 5000000 * inflation_table$multiplier[inflation_table$year==1989]
mildata$mil.expenditure.wmeat[mildata$iso3c=="CPV"&mildata$year==1990] <- 5000000 * inflation_table$multiplier[inflation_table$year==1990]

##### cow expenditure
# 1988-1990
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "mil.expenditure.wmeat",
  "CPV", 1991, restricted = c(1988:1990)
)
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow.alt",
  col_ref = "mil.expenditure.wmeat",
  "CPV", 1991, restricted = c(1988:1990)
)

# 1984-1987
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "mil.expenditure.sipri",
  "CPV", 1988, restricted = c(1984:1987)
)
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow.alt",
  col_ref = "mil.expenditure.sipri",
  "CPV", 1988, restricted = c(1984:1987)
)

# 1982-1983: assume consistent growth between 1947 and 1950
cpv.alpha.1 <- (mildata$mil.expenditure.cow[mildata$iso3c=="CPV"&mildata$year==1984]/mildata$mil.expenditure.cow[mildata$iso3c=="CPV"&mildata$year==1981])^(1/3)

mildata$mil.expenditure.cow[mildata$iso3c=="CPV"&mildata$year==1982] <- mildata$mil.expenditure.cow[mildata$iso3c=="CPV"&mildata$year==1981] * cpv.alpha.1
mildata$mil.expenditure.cow[mildata$iso3c=="CPV"&mildata$year==1983] <- mildata$mil.expenditure.cow[mildata$iso3c=="CPV"&mildata$year==1982] * cpv.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="CPV"&mildata$year==1982] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="CPV"&mildata$year==1981] * cpv.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="CPV"&mildata$year==1983] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="CPV"&mildata$year==1982] * cpv.alpha.1

# 1975: recalculate
mildata <- mil_estimator_no_data_func(
  mildata,"CPV",
  metric = "mil.expenditure.cow",
  1975,1975
)
mildata <- mil_estimator_no_data_func(
  mildata,"CPV",
  metric = "mil.expenditure.cow.alt",
  1975,1975
)

##### wmeat expenditure
# 1975
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "CPV", 1976, restricted = 1975
)


# 1982-1984: recalculate assuming consistent growth between 1981 and 1985
cpv.alpha.2 <- (mildata$mil.expenditure.wmeat[mildata$iso3c=="CPV"&mildata$year==1985]/mildata$mil.expenditure.wmeat[mildata$iso3c=="CPV"&mildata$year==1981])^(1/4)

mildata$mil.expenditure.wmeat[mildata$iso3c=="CPV"&mildata$year==1982] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="CPV"&mildata$year==1981] * cpv.alpha.2
mildata$mil.expenditure.wmeat[mildata$iso3c=="CPV"&mildata$year==1983] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="CPV"&mildata$year==1982] * cpv.alpha.2
mildata$mil.expenditure.wmeat[mildata$iso3c=="CPV"&mildata$year==1984] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="CPV"&mildata$year==1983] * cpv.alpha.2

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "CPV", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1975-1983: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "CPV", 1984, restricted = c(1975:1983)
)

# 1989-1991
mildata$mil.expenditure.sipri[mildata$iso3c=="CPV"&mildata$year==1989] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT est", "CPV", 1989, yr.minus.1 = 1988, yr.plus.1 = 1992)
mildata$mil.expenditure.sipri[mildata$iso3c=="CPV"&mildata$year==1990] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT est", "CPV", 1990, yr.minus.1 = 1988, yr.plus.1 = 1992)
mildata$mil.expenditure.sipri[mildata$iso3c=="CPV"&mildata$year==1991] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT est", "CPV", 1991, yr.minus.1 = 1988, yr.plus.1 = 1992)

##### cow personnel
# good

##### wmeat personnel
# 1975
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "CPV", 1976, restricted = 1975
)

# 2018-2019: COW and WMEAT personnel match 2004-2017, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(2018:2019)&iso3c=="CPV",mil.personnel.cow,mil.personnel.wmeat))

# x <- milexp.viewer("CPV")
# y <- milper.viewer("CPV")

##### CRI ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1946-1972: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "CRI", 1973, restricted = c(1946:1972)
)

# 1994
mildata$mil.expenditure.wmeat[mildata$iso3c=="CRI"&mildata$year==1994] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "CRI", 1994)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "CRI", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1948: the military was abolished in 1948, so assume difference in spending from 1948-1949 COW and WMEAT is SIPRI value
cri.cow.diff <- mildata$mil.expenditure.cow[mildata$iso3c=="CRI"&mildata$year==1948] - mildata$mil.expenditure.cow[mildata$iso3c=="CRI"&mildata$year==1947]
cri.wmeat.diff <- mildata$mil.expenditure.wmeat[mildata$iso3c=="CRI"&mildata$year==1948] - mildata$mil.expenditure.wmeat[mildata$iso3c=="CRI"&mildata$year==1947]

mildata$mil.expenditure.sipri[mildata$iso3c=="CRI"&mildata$year==1948] <- mean(cri.cow.diff,cri.wmeat.diff)

# 1946-1947: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "CRI", 1948, restricted = c(1946:1947)
)

##### cow personnel
# 2000-2001: assume consistent decline
cri.alpha <- (mildata$mil.personnel.cow[mildata$iso3c=="CRI"&mildata$year==2002] - mildata$mil.personnel.cow[mildata$iso3c=="CRI"&mildata$year==1999]) / 3

mildata$mil.personnel.cow[mildata$iso3c=="CRI"&mildata$year==2000] <- mildata$mil.personnel.cow[mildata$iso3c=="CRI"&mildata$year==1999] + cri.alpha
mildata$mil.personnel.cow[mildata$iso3c=="CRI"&mildata$year==2001] <- mildata$mil.personnel.cow[mildata$iso3c=="CRI"&mildata$year==2000] + cri.alpha

##### wmeat personnel
# 1945-1962: COW and WMEAT personnel match 1963-1976, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1945:1962)&iso3c=="CRI",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2008-2009 - 2016-2017 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="CRI"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="CRI"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="CRI"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="CRI"&mildata$year==2018]

# x <- milexp.viewer("CRI")
# y <- milper.viewer("CRI")

##### CUB ----------------------------------------------------------------------

##### cow expenditure
# 1994: multiply by 1000 - this lines with WMEAT estimate
mildata$mil.expenditure.cow[mildata$iso3c=="CUB"&mildata$year==1994] <- 1000 * mildata$mil.expenditure.cow[mildata$iso3c=="CUB"&mildata$year==1994]
mildata$mil.expenditure.cow.alt[mildata$iso3c=="CUB"&mildata$year==1994] <- 1000 * mildata$mil.expenditure.cow.alt[mildata$iso3c=="CUB"&mildata$year==1994]

# 2009: remove apparent extra 0 - IISS value for 2009 is ~1/10 the value of the COW value
mildata$mil.expenditure.cow[mildata$iso3c=="CUB"&mildata$year==2009] <- mildata$mil.expenditure.cow[mildata$iso3c=="CUB"&mildata$year==2009] / 10

# 1946-1947
mildata <- mil_estimator_no_data_func(
  mildata,"CUB",
  metric = "mil.expenditure.cow",
  1947,1946
)
mildata <- mil_estimator_no_data_func(
  mildata,"CUB",
  metric = "mil.expenditure.cow.alt",
  1947,1946
)

# 1949: assume consistent growth between 1948 and 1950
cub.alpha.1 <- (mildata$mil.expenditure.cow[mildata$iso3c=="CUB"&mildata$year==1950]/mildata$mil.expenditure.cow[mildata$iso3c=="CUB"&mildata$year==1948])^(1/3)

mildata$mil.expenditure.cow[mildata$iso3c=="CUB"&mildata$year==1949] <- mildata$mil.expenditure.cow[mildata$iso3c=="CUB"&mildata$year==1948] * cub.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="CUB"&mildata$year==1949] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="CUB"&mildata$year==1948] * cub.alpha.1

# 1951: assume consistent growth between 1950 and 1952
cub.alpha.2 <- (mildata$mil.expenditure.cow[mildata$iso3c=="CUB"&mildata$year==1952]/mildata$mil.expenditure.cow[mildata$iso3c=="CUB"&mildata$year==1950])^(1/3)

mildata$mil.expenditure.cow[mildata$iso3c=="CUB"&mildata$year==1951] <- mildata$mil.expenditure.cow[mildata$iso3c=="CUB"&mildata$year==1950] * cub.alpha.2
mildata$mil.expenditure.cow.alt[mildata$iso3c=="CUB"&mildata$year==1951] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="CUB"&mildata$year==1950] * cub.alpha.2

# 1954: assume consistent growth between 1953 and 1955
cub.alpha.3 <- (mildata$mil.expenditure.cow[mildata$iso3c=="CUB"&mildata$year==1955]/mildata$mil.expenditure.cow[mildata$iso3c=="CUB"&mildata$year==1953])^(1/3)

mildata$mil.expenditure.cow[mildata$iso3c=="CUB"&mildata$year==1954] <- mildata$mil.expenditure.cow[mildata$iso3c=="CUB"&mildata$year==1953] * cub.alpha.3
mildata$mil.expenditure.cow.alt[mildata$iso3c=="CUB"&mildata$year==1954] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="CUB"&mildata$year==1953] * cub.alpha.3

# 1958-1959: assume consistent growth between 1957 and 1960
cub.alpha.4 <- (mildata$mil.expenditure.cow[mildata$iso3c=="CUB"&mildata$year==1960]/mildata$mil.expenditure.cow[mildata$iso3c=="CUB"&mildata$year==1957])^(1/4)

mildata$mil.expenditure.cow[mildata$iso3c=="CUB"&mildata$year==1958] <- mildata$mil.expenditure.cow[mildata$iso3c=="CUB"&mildata$year==1957] * cub.alpha.4
mildata$mil.expenditure.cow[mildata$iso3c=="CUB"&mildata$year==1959] <- mildata$mil.expenditure.cow[mildata$iso3c=="CUB"&mildata$year==1958] * cub.alpha.4
mildata$mil.expenditure.cow.alt[mildata$iso3c=="CUB"&mildata$year==1958] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="CUB"&mildata$year==1957] * cub.alpha.4
mildata$mil.expenditure.cow.alt[mildata$iso3c=="CUB"&mildata$year==1959] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="CUB"&mildata$year==1958] * cub.alpha.4

# 1976: assume consistent growth between 1975 and 1977
cub.alpha.5 <- (mildata$mil.expenditure.cow[mildata$iso3c=="CUB"&mildata$year==1977]/mildata$mil.expenditure.cow[mildata$iso3c=="CUB"&mildata$year==1975])^(1/3)

mildata$mil.expenditure.cow[mildata$iso3c=="CUB"&mildata$year==1976] <- mildata$mil.expenditure.cow[mildata$iso3c=="CUB"&mildata$year==1975] * cub.alpha.5
mildata$mil.expenditure.cow.alt[mildata$iso3c=="CUB"&mildata$year==1976] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="CUB"&mildata$year==1975] * cub.alpha.5

# 1992-1993
mildata$mil.expenditure.cow[mildata$iso3c=="CUB"&mildata$year==1992] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "CUB", 1992, yr.minus.1 = 1991, yr.plus.1 = 1994)
mildata$mil.expenditure.cow[mildata$iso3c=="CUB"&mildata$year==1993] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "CUB", 1993, yr.minus.1 = 1991, yr.plus.1 = 1994)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="CUB"&mildata$year==1992] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "CUB", 1992, yr.minus.1 = 1991, yr.plus.1 = 1994)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="CUB"&mildata$year==1993] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "CUB", 1993, yr.minus.1 = 1991, yr.plus.1 = 1994)

# 2010
mildata$mil.expenditure.cow[mildata$iso3c=="CUB"&mildata$year==2010] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "IISS", "CUB", 2010)

# 2012: apply IISS proportions from 2011
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "defense.spending.iiss",
  "CUB", 2011, restricted = 2012
)

## remove estimate flag for 2012
mildata <- mildata %>%
  dplyr::mutate(mil.expenditure.cow.est.flag  = dplyr::case_when(
    iso3c == "CUB" & year == 2012 ~ 0,
    .default = mil.expenditure.cow.est.flag
  ))

# 2013-2017: apply WMEAT proportions from 2012
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "mil.expenditure.wmeat",
  "CUB", 2012, restricted = c(2013:2017)
)
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow.alt",
  col_ref = "mil.expenditure.wmeat",
  "CUB", 2012, restricted = c(2013:2017)
)

# 2018: apply SIPRI proportions to COW
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "mil.expenditure.sipri",
  "CUB", 2017, restricted = 2018
)
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow.alt",
  col_ref = "mil.expenditure.sipri",
  "CUB", 2017, restricted = 2018
)

# 2019
mildata <- mil_estimator_no_data_func(
  mildata,"CUB",
  metric = "mil.expenditure.cow",
  2019,2019
)
mildata <- mil_estimator_no_data_func(
  mildata,"CUB",
  metric = "mil.expenditure.cow.alt",
  2019,2019
)

##### wmeat expenditure
# 1946-1962: apply COW proportions from 1963
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "CUB", 1963, restricted = c(1946:1962)
)

# 1977: apply COW proportions from 1978
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "CUB", 1978, restricted = 1977
)

# 1976: assume consistent growth between 1975 and 1977
cub.alpha.6 <- (mildata$mil.expenditure.wmeat[mildata$iso3c=="CUB"&mildata$year==1977]/mildata$mil.expenditure.wmeat[mildata$iso3c=="CUB"&mildata$year==1975])^(1/3)

mildata$mil.expenditure.wmeat[mildata$iso3c=="CUB"&mildata$year==1976] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="CUB"&mildata$year==1975] * cub.alpha.6

# 2018: apply SIPRI proportions to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "CUB", 2017, restricted = 2018
)

# 2019
mildata <- mil_estimator_no_data_func(
  mildata,"CUB",
  metric = "mil.expenditure.wmeat",
  2019,2019
)

##### sipri expenditure
# 1946-2002: apply COW proportions to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "CUB", 2003, restricted = c(1946:2002)
)

# 2019
mildata <- mil_estimator_no_data_func(
  mildata,"CUB",
  metric = "mil.expenditure.sipri",
  2019,2019
)

##### cow personnel
# good

##### wmeat personnel
# 1945-1962: COW and WMEAT personnel match 1963-1995, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1945:1962)&iso3c=="CUB",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2007-2008 - 2016-2017 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="CUB"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="CUB"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="CUB"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="CUB"&mildata$year==2018]

# x <- milexp.viewer("CUB")
# y <- milper.viewer("CUB")

##### CYP ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1960-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "CYP", 1963, restricted = c(1960:1962)
)

# 1992
mildata$mil.expenditure.wmeat[mildata$iso3c=="CYP"&mildata$year==1992] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "CYP", 1992)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "CYP", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1960-1984: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "CYP", 1985, restricted = c(1960:1984)
)

##### cow personnel
# good

##### wmeat personnel
# 1960-1964: COW and WMEAT personnel match 1965-1976, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1960:1964)&iso3c=="CYP",mil.personnel.cow,mil.personnel.wmeat))

# 1977: COW and WMEAT personnel match 1960-1992, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year==1977&iso3c=="CYP",mil.personnel.cow,mil.personnel.wmeat))

# 1993-1994: COW and WMEAT personnel match 1978-1998, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1993:1994)&iso3c=="CYP",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: COW and WMEAT personnel match 2012-2017, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(2018:2019)&iso3c=="CYP",mil.personnel.cow,mil.personnel.wmeat))

# x <- milexp.viewer("CYP")
# y <- milper.viewer("CYP")

##### CZE ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1946-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "CZE", 1963, restricted = c(1946:1962)
)

# 1992
mildata$mil.expenditure.wmeat[mildata$iso3c=="CZE"&mildata$year==1992] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "CZE", 1992)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "CZE", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1989: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "CZE", 1990, restricted = c(1946:1989)
)

# 1991-1992
mildata$mil.expenditure.sipri[mildata$iso3c=="CZE"&mildata$year==1991] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT est", "CZE", 1991, yr.minus.1 = 1990, yr.plus.1 = 1993)
mildata$mil.expenditure.sipri[mildata$iso3c=="CZE"&mildata$year==1992] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT est", "CZE", 1992, yr.minus.1 = 1990, yr.plus.1 = 1993)

##### cow personnel
# good

##### wmeat personnel
# 1945-1962: COW and WMEAT personnel match 1963-1976, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1945:1962)&iso3c=="CZE",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "CZE", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("CZE")
# y <- milper.viewer("CZE")

##### DDR ----------------------------------------------------------------------

##### cow expenditure
# 1949-1956
mildata <- mil_estimator_no_data_func(
  mildata,"DDR",
  metric = "mil.expenditure.cow",
  1956,1949
)
mildata <- mil_estimator_no_data_func(
  mildata,"DDR",
  metric = "mil.expenditure.cow.alt",
  1956,1949
)

##### wmeat expenditure
# 1949-1956
mildata <- mil_estimator_no_data_func(
  mildata,"DDR",
  metric = "mil.expenditure.wmeat",
  1956,1949
)

# 1957-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "DDR", 1963, restricted = c(1957:1962)
)

##### sipri expenditure
# N/A

##### cow personnel
# 1949-1953
mildata <- mil_estimator_no_data_func(
  mildata,"DDR",
  metric = "mil.personnel.cow",
  1953,1949
)

##### wmeat personnel
# 1954-1962: COW and WMEAT personnel match 1963-1976, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1954:1962)&iso3c=="DDR",mil.personnel.cow,mil.personnel.wmeat))

# 1949-1953
mildata <- mil_estimator_no_data_func(
  mildata,"DDR",
  metric = "mil.personnel.wmeat",
  1953,1949
)

# x <- milexp.viewer("DDR")
# y <- milper.viewer("DDR")

##### DEU ----------------------------------------------------------------------
# note: COW 1945 Germany (pre-partition/occupation) military personnel: 5,300,000

##### cow expenditure
# good

##### wmeat expenditure
# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "DEU", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# good

##### cow personnel
# good

##### wmeat personnel
# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
mildata,col_missing = "mil.personnel.wmeat",
col_ref = "mil.personnel.cow",
"DEU", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("DEU")
# y <- milper.viewer("DEU")

##### DJI ----------------------------------------------------------------------

##### cow expenditure
# 1978: apply SIPRI growth rates to COW
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "mil.expenditure.sipri",
  "DJI", 1979, restricted = 1978
)
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow.alt",
  col_ref = "mil.expenditure.sipri",
  "DJI", 1979, restricted = 1978
)

# 2012-2013: apply IISS proportions from 2011
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "defense.spending.iiss",
  "DJI", 2011, restricted = c(2012:2013)
)

## remove estimate flag for 2012-2013
mildata <- mildata %>%
  dplyr::mutate(mil.expenditure.cow.est.flag  = dplyr::case_when(
    iso3c == "DJI" & year %in% c(2012:2013) ~ 0,
    .default = mil.expenditure.cow.est.flag
  ))

# 2014-2017: apply WMEAT proportions from 2013
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "mil.expenditure.wmeat",
  "DJI", 2013, restricted = c(2014:2017)
)
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow.alt",
  col_ref = "mil.expenditure.wmeat",
  "DJI", 2013, restricted = c(2014:2017)
)

# 2018-2019
mildata <- mil_estimator_no_data_func(
  mildata,"DJI",
  metric = "mil.expenditure.cow",
  2018,2019
)
mildata <- mil_estimator_no_data_func(
  mildata,"DJI",
  metric = "mil.expenditure.cow.alt",
  2018,2019
)

##### wmeat expenditure
# 1977-1985: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "DJI", 1986, restricted = c(1977:1985)
)

# 2018-2019
mildata <- mil_estimator_no_data_func(
  mildata,"DJI",
  metric = "mil.expenditure.wmeat",
  2018,2019
)

##### sipri expenditure
# 1977
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "DJI", 1978, restricted = 1977
)

# 2009-2017: apply WMEAT growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.wmeat",
  "DJI", 2008, restricted = c(2009:2017)
)

# 2018-2019
mildata <- mil_estimator_no_data_func(
  mildata,"DJI",
  metric = "mil.expenditure.sipri",
  2018,2019
)

##### cow personnel
# good

##### wmeat personnel
# 1977-1983: COW and WMEAT personnel match 1963-1998, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1977:1983)&iso3c=="DJI",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2009-2010- 2016-2017 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="DJI"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="DJI"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="DJI"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="DJI"&mildata$year==2018]

# x <- milexp.viewer("DJI")
# y <- milper.viewer("DJI")

##### DMA(x) ----------------------------------------------------------------------

##### cow expenditure
# 1980-1985
# 2012-2019

##### wmeat expenditure
# N/A

##### sipri expenditure
# N/A

##### cow personnel
# 2013-2019

##### wmeat personnel
# N/A

x <- milexp.viewer("DMA")
y <- milper.viewer("DMA")

##### DNK ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1946-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "DNK", 1963, restricted = c(1946:1962)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "DNK", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1948: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "DNK", 1949, restricted = c(1946:1948)
)

##### cow personnel
# good

##### wmeat personnel
# 1945-1962: COW and WMEAT personnel match 1963-1981, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1945:1962)&iso3c=="DNK",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: COW and WMEAT personnel match 2014-2017, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(2018:2019)&iso3c=="DNK",mil.personnel.cow,mil.personnel.wmeat))

# x <- milexp.viewer("DNK")
# y <- milper.viewer("DNK")

##### DOM ----------------------------------------------------------------------

##### cow expenditure
# 1948-1949: assume consistent growth between 1947 and 1950
dom.alpha.1 <- (mildata$mil.expenditure.cow[mildata$iso3c=="DOM"&mildata$year==1950]/mildata$mil.expenditure.cow[mildata$iso3c=="DOM"&mildata$year==1947])^(1/3)

mildata$mil.expenditure.cow[mildata$iso3c=="DOM"&mildata$year==1948] <- mildata$mil.expenditure.cow[mildata$iso3c=="DOM"&mildata$year==1947] * dom.alpha.1
mildata$mil.expenditure.cow[mildata$iso3c=="DOM"&mildata$year==1949] <- mildata$mil.expenditure.cow[mildata$iso3c=="DOM"&mildata$year==1948] * dom.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="DOM"&mildata$year==1948] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="DOM"&mildata$year==1947] * dom.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="DOM"&mildata$year==1949] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="DOM"&mildata$year==1948] * dom.alpha.1

# 1951-1952: assume consistent growth between 1950 and 1953
dom.alpha.2 <- (mildata$mil.expenditure.cow[mildata$iso3c=="DOM"&mildata$year==1953]/mildata$mil.expenditure.cow[mildata$iso3c=="DOM"&mildata$year==1950])^(1/3)

mildata$mil.expenditure.cow[mildata$iso3c=="DOM"&mildata$year==1951] <- mildata$mil.expenditure.cow[mildata$iso3c=="DOM"&mildata$year==1950] * dom.alpha.2
mildata$mil.expenditure.cow[mildata$iso3c=="DOM"&mildata$year==1952] <- mildata$mil.expenditure.cow[mildata$iso3c=="DOM"&mildata$year==1951] * dom.alpha.2
mildata$mil.expenditure.cow.alt[mildata$iso3c=="DOM"&mildata$year==1951] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="DOM"&mildata$year==1950] * dom.alpha.2
mildata$mil.expenditure.cow.alt[mildata$iso3c=="DOM"&mildata$year==1952] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="DOM"&mildata$year==1951] * dom.alpha.2

# 1956-1957: assume consistent growth between 1955 and 1958
dom.alpha.3 <- (mildata$mil.expenditure.cow[mildata$iso3c=="DOM"&mildata$year==1958]/mildata$mil.expenditure.cow[mildata$iso3c=="DOM"&mildata$year==1955])^(1/3)

mildata$mil.expenditure.cow[mildata$iso3c=="DOM"&mildata$year==1956] <- mildata$mil.expenditure.cow[mildata$iso3c=="DOM"&mildata$year==1955] * dom.alpha.3
mildata$mil.expenditure.cow[mildata$iso3c=="DOM"&mildata$year==1957] <- mildata$mil.expenditure.cow[mildata$iso3c=="DOM"&mildata$year==1956] * dom.alpha.3
mildata$mil.expenditure.cow.alt[mildata$iso3c=="DOM"&mildata$year==1956] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="DOM"&mildata$year==1955] * dom.alpha.3
mildata$mil.expenditure.cow.alt[mildata$iso3c=="DOM"&mildata$year==1957] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="DOM"&mildata$year==1956] * dom.alpha.3

##### wmeat expenditure
# 1946-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "DOM", 1963, restricted = c(1946:1962)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "DOM", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1957: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "DOM", 1958, restricted = c(1946:1957)
)

# 1959
mildata$mil.expenditure.sipri[mildata$iso3c=="DOM"&mildata$year==1959] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "DOM", 1959)

##### cow personnel
# good

##### wmeat personnel
# 1945-1962: COW and WMEAT personnel match 1963-1975, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1945:1962)&iso3c=="DOM",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2016-2017 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="DOM"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="DOM"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="DOM"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="DOM"&mildata$year==2018]

# x <- milexp.viewer("DOM")
# y <- milper.viewer("DOM")

##### DZA ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "DZA", 1963, restricted = 1962
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "DZA", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1962: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "DZA", 1963, restricted = 1962
)

##### cow personnel
# good

##### wmeat personnel
# 1962: COW and WMEAT personnel match 1963-1991, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year==1962&iso3c=="DZA",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2014-2015 - 2016-2017 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="DZA"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="DZA"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="DZA"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="DZA"&mildata$year==2018]

# x <- milexp.viewer("DZA")
# y <- milper.viewer("DZA")

##### ECU ----------------------------------------------------------------------

##### cow expenditure
# 1948: assume consistent growth between 1947 and 1949
ecu.alpha <- (mildata$mil.expenditure.cow[mildata$iso3c=="ECU"&mildata$year==1949]/mildata$mil.expenditure.cow[mildata$iso3c=="ECU"&mildata$year==1947])^(1/2)

mildata$mil.expenditure.cow[mildata$iso3c=="ECU"&mildata$year==1948] <- mildata$mil.expenditure.cow[mildata$iso3c=="ECU"&mildata$year==1947] * ecu.alpha
mildata$mil.expenditure.cow.alt[mildata$iso3c=="ECU"&mildata$year==1948] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="ECU"&mildata$year==1947] * ecu.alpha

# 1992
mildata$mil.expenditure.cow[mildata$iso3c=="ECU"&mildata$year==1992] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "ECU", 1992)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="ECU"&mildata$year==1992] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "ECU", 1992)

##### wmeat expenditure
# 1946-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "ECU", 1963, restricted = c(1946:1962)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "ECU", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1952: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "ECU", 1953, restricted = c(1946:1952)
)

# 1954-1956
mildata$mil.expenditure.sipri[mildata$iso3c=="ECU"&mildata$year==1954] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "ECU", 1954, yr.minus.1 = 1953, yr.plus.1 = 1957)
mildata$mil.expenditure.sipri[mildata$iso3c=="ECU"&mildata$year==1955] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "ECU", 1955, yr.minus.1 = 1953, yr.plus.1 = 1957)
mildata$mil.expenditure.sipri[mildata$iso3c=="ECU"&mildata$year==1956] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "ECU", 1956, yr.minus.1 = 1953, yr.plus.1 = 1957)

##### cow personnel
# good

##### wmeat personnel
# 1946-1962: COW and WMEAT personnel match 1963-2000, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1946:1962)&iso3c=="ECU",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: COW and WMEAT personnel match 2016-2017 so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(2018:2019)&iso3c=="ECU",mil.personnel.cow,mil.personnel.wmeat))

# x <- milexp.viewer("ECU")
# y <- milper.viewer("ECU")

##### EGY ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1946-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "EGY", 1963, restricted = c(1946:1962)
)

# 1981
mildata$mil.expenditure.wmeat[mildata$iso3c=="EGY"&mildata$year==1981] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "EGY", 1981)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "EGY", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1961: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "EGY", 1963, restricted = c(1946:1961)
)

##### cow personnel
# good

##### wmeat personnel
# 1946-1962: COW and WMEAT personnel match 1963-1999, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1946:1962)&iso3c=="EGY",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2013-2014 - 2016-2017 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="EGY"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="EGY"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="EGY"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="EGY"&mildata$year==2018]

# x <- milexp.viewer("EGY")
# y <- milper.viewer("EGY")

##### ERI ----------------------------------------------------------------------

##### cow expenditure
# 2006-2008
mildata$mil.expenditure.cow[mildata$iso3c=="ERI"&mildata$year==2006] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "ERI", 2006, yr.minus.1 = 2005, yr.plus.1 = 2009)
mildata$mil.expenditure.cow[mildata$iso3c=="ERI"&mildata$year==2007] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "ERI", 2007, yr.minus.1 = 2005, yr.plus.1 = 2009)
mildata$mil.expenditure.cow[mildata$iso3c=="ERI"&mildata$year==2008] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "ERI", 2008, yr.minus.1 = 2005, yr.plus.1 = 2009)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="ERI"&mildata$year==2006] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "ERI", 2006, yr.minus.1 = 2005, yr.plus.1 = 2009)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="ERI"&mildata$year==2007] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "ERI", 2007, yr.minus.1 = 2005, yr.plus.1 = 2009)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="ERI"&mildata$year==2008] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "ERI", 2008, yr.minus.1 = 2005, yr.plus.1 = 2009)

# 2010: apply IISS proportions from 2011
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "defense.spending.iiss",
  "ERI", 2011, restricted = 2010
)

# 2012-2013: apply IISS proportions from 2011
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "defense.spending.iiss",
  "ERI", 2011, restricted = c(2012:2013)
)

## remove estimate flag for 2012-2013
mildata <- mildata %>%
  dplyr::mutate(mil.expenditure.cow.est.flag  = dplyr::case_when(
    iso3c == "ERI" & year %in% c(2012:2013) ~ 0,
    .default = mil.expenditure.cow.est.flag
  ))

# 2014-2017: apply WMEAT proportions from 2013
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "mil.expenditure.wmeat",
  "ERI", 2013, restricted = c(2014:2017)
)
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow.alt",
  col_ref = "mil.expenditure.wmeat",
  "ERI", 2013, restricted = c(2014:2017)
)

# 2018-2019
mildata <- mil_estimator_no_data_func(
  mildata,"ERI",
  metric = "mil.expenditure.cow",
  2018,2019
)
mildata <- mil_estimator_no_data_func(
  mildata,"ERI",
  metric = "mil.expenditure.cow.alt",
  2018,2019
)

##### wmeat expenditure
# 1993-1994: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "ERI", 1995, restricted = c(1993:1994)
)

# 2018-2019
mildata <- mil_estimator_no_data_func(
  mildata,"ERI",
  metric = "mil.expenditure.wmeat",
  2018,2019
)

##### sipri expenditure
# 2004-2017: apply WMEAT growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.wmeat",
  "ERI", 2003, restricted = c(2004:2017)
)

# 2018-2019
mildata <- mil_estimator_no_data_func(
  mildata,"ERI",
  metric = "mil.expenditure.sipri",
  2018,2019
)

##### cow personnel
# good

##### wmeat personnel
# 1993-1994: COW and WMEAT personnel match 1995-1998, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1993:1994)&iso3c=="ERI",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2011-2012 - 2016-2017 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="ERI"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="ERI"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="ERI"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="ERI"&mildata$year==2018]

# x <- milexp.viewer("ERI")
# y <- milper.viewer("ERI")

##### ESP ----------------------------------------------------------------------

##### cow expenditure
# 2013-2019 (non-alt): apply IISS growth rates to COW
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "defense.spending.iiss",
  "ESP", 2012, restricted = c(2013:2019)
)

##### wmeat expenditure
# 1949-1962: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "ESP", 1963, restricted = c(1949:1962)
)

# 1946-1948: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "ESP", 1949, restricted = c(1946:1948)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "ESP", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1948: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "ESP", 1949, restricted = c(1946:1948)
)

##### cow personnel
# good

##### wmeat personnel
# 1945-1962: COW and WMEAT personnel match 1963-1976, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1945:1962)&iso3c=="ESP",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "ESP", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("ESP")
# y <- milper.viewer("ESP")

##### EST ----------------------------------------------------------------------

##### cow expenditure
# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"EST",
  metric = "mil.expenditure.cow",
  1991,1991
)
mildata <- mil_estimator_no_data_func(
  mildata,"EST",
  metric = "mil.expenditure.cow.alt",
  1991,1991
)

##### wmeat expenditure
# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"EST",
  metric = "mil.expenditure.wmeat",
  1991,1991
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "EST", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1992: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "EST", 1993, restricted = 1992
)

# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"EST",
  metric = "mil.expenditure.sipri",
  1991,1991
)

##### cow personnel
# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"EST",
  metric = "mil.personnel.cow",
  1991,1991
)

##### wmeat personnel
# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"EST",
  metric = "mil.personnel.wmeat",
  1991,1991
)

# 2018-2019: COW and WMEAT personnel match 2009-2017, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(2018:2019)&iso3c=="EST",mil.personnel.cow,mil.personnel.wmeat))

# TODO: check EST WMEAT armed personnel 1994

# x <- milexp.viewer("EST")
# y <- milper.viewer("EST")

##### ETH ----------------------------------------------------------------------

##### cow expenditure
# 1957-1958: apply SIPRI growth rates to COW
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "mil.expenditure.sipri",
  "ETH", 1959, restricted = c(1957:1958)
)
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow.alt",
  col_ref = "mil.expenditure.sipri",
  "ETH", 1959, restricted = c(1957:1958)
)

# 1946-1956
mildata <- mil_estimator_no_data_func(
  mildata,"ETH",
  metric = "mil.expenditure.cow",
  1956,1946
)
mildata <- mil_estimator_no_data_func(
  mildata,"ETH",
  metric = "mil.expenditure.cow.alt",
  1956,1946
)

##### wmeat expenditure
# 1957-1962: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "ETH", 1963, restricted = c(1957:1962)
)

# 1946-1956
mildata <- mil_estimator_no_data_func(
  mildata,"ETH",
  metric = "mil.expenditure.wmeat",
  1956,1946
)

# 1981
mildata$mil.expenditure.wmeat[mildata$iso3c=="ETH"&mildata$year==1981] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "ETH", 1981)

# 1983
mildata$mil.expenditure.wmeat[mildata$iso3c=="ETH"&mildata$year==1983] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "ETH", 1983)

# 1992
mildata$mil.expenditure.wmeat[mildata$iso3c=="ETH"&mildata$year==1992] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "ETH", 1992)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "ETH", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1956
mildata <- mil_estimator_no_data_func(
  mildata,"ETH",
  metric = "mil.expenditure.sipri",
  1956,1946
)

##### cow personnel
# good

##### wmeat personnel
# 1946-1962
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "ETH", 1963, restricted = c(1946:1962)
)

# 2018-2019: 2009-2010 - 2016-2017 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="ETH"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="ETH"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="ETH"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="ETH"&mildata$year==2018]

# x <- milexp.viewer("ETH")
# y <- milper.viewer("ETH")

##### FIN ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1946-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "FIN", 1963, restricted = c(1946:1962)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "FIN", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1957: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "FIN", 1958, restricted = c(1946:1957)
)

##### cow personnel
# good

##### wmeat personnel
# 1945-1962: COW and WMEAT personnel match 1963-1976, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1945:1962)&iso3c=="FIN",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2014-2015 - 2016-2017 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="FIN"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="FIN"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="FIN"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="FIN"&mildata$year==2018]

# x <- milexp.viewer("FIN")
# y <- milper.viewer("FIN")

##### FJI ----------------------------------------------------------------------

##### cow expenditure
# 2011 (non-alt)
mildata$mil.expenditure.cow[mildata$iso3c=="FJI"&mildata$year==2011] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "IISS", "FJI", 2011)

##### wmeat expenditure
# 1970-1972: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "FJI", 1973, restricted = c(1970:1972)
)

# 1983
mildata$mil.expenditure.wmeat[mildata$iso3c=="FJI"&mildata$year==1983] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "FJI", 1983)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "FJI", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# good

##### cow personnel
# good

##### wmeat personnel
# 1970-1972: COW and WMEAT personnel match 1973-1982, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1970:1972)&iso3c=="FJI",mil.personnel.cow,mil.personnel.wmeat))

# 1983: COW and WMEAT personnel match 1973-1998, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year==1983&iso3c=="FJI",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: COW and WMEAT personnel match 2013-2017, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(2018:2019)&iso3c=="FJI",mil.personnel.cow,mil.personnel.wmeat))

# x <- milexp.viewer("FJI")
# y <- milper.viewer("FJI")

##### FRA ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1946-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "FRA", 1963, restricted = c(1946:1962)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "FRA", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1948: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "FRA", 1949, restricted = c(1946:1948)
)

##### cow personnel
# good

##### wmeat personnel
# 1945-1962: COW and WMEAT personnel match 1963-1981, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1945:1962)&iso3c=="FRA",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "FRA", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("FRA")
# y <- milper.viewer("FRA")

##### FSM(x) ----------------------------------------------------------------------

##### cow expenditure
# 2013-2019

##### wmeat expenditure
# N/A

##### sipri expenditure
# N/A

##### cow personnel
# 2013-2019

##### wmeat personnel
# N/A

x <- milexp.viewer("FSM")
y <- milper.viewer("FSM")

##### GAB ----------------------------------------------------------------------

##### cow expenditure
# 1992-1993: estimate COW 1992-1993 as equidistant point based on WMEAT 1990-1994 values (WMEAT 1991 is NA)
mildata$mil.expenditure.cow[mildata$iso3c=="GAB"&mildata$year==1992] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "GAB", 1992, yr.minus.1 = 1990, yr.plus.1 = 1994)
mildata$mil.expenditure.cow[mildata$iso3c=="GAB"&mildata$year==1993] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "GAB", 1993, yr.minus.1 = 1990, yr.plus.1 = 1994)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="GAB"&mildata$year==1992] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "GAB", 1992, yr.minus.1 = 1990, yr.plus.1 = 1994)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="GAB"&mildata$year==1993] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "GAB", 1993, yr.minus.1 = 1990, yr.plus.1 = 1994)

# 1991: assume consistent growth between 1990 and 1992
gab.alpha.1 <- (mildata$mil.expenditure.cow[mildata$iso3c=="GAB"&mildata$year==1992]/mildata$mil.expenditure.cow[mildata$iso3c=="GAB"&mildata$year==1990])^(1/2)

mildata$mil.expenditure.cow[mildata$iso3c=="GAB"&mildata$year==1991] <- mildata$mil.expenditure.cow[mildata$iso3c=="GAB"&mildata$year==1990] * gab.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="GAB"&mildata$year==1991] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="GAB"&mildata$year==1990] * gab.alpha.1

##### wmeat expenditure
# 1960-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "GAB", 1963, restricted = c(1960:1962)
)

# 1991: assume consistent growth between 1990 and 1992
gab.alpha.2 <- (mildata$mil.expenditure.wmeat[mildata$iso3c=="GAB"&mildata$year==1992]/mildata$mil.expenditure.wmeat[mildata$iso3c=="GAB"&mildata$year==1990])^(1/2)

mildata$mil.expenditure.wmeat[mildata$iso3c=="GAB"&mildata$year==1991] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="GAB"&mildata$year==1990] * gab.alpha.2

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "GAB", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1960-1966: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "GAB", 1967, restricted = c(1960:1966)
)

# 1978: estimate SIPRI 1978 as equidistant points based on COW 1977-1979 values
mildata$mil.expenditure.sipri[mildata$iso3c=="GAB"&mildata$year==1978] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "GAB", 1978)

# 1987-1990: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "GAB", 1986, restricted = c(1987:1990)
)

# 1991: assume consistent growth between 1990 and 1992
gab.alpha.3 <- (mildata$mil.expenditure.sipri[mildata$iso3c=="GAB"&mildata$year==1992]/mildata$mil.expenditure.sipri[mildata$iso3c=="GAB"&mildata$year==1990])^(1/2)

mildata$mil.expenditure.sipri[mildata$iso3c=="GAB"&mildata$year==1991] <- mildata$mil.expenditure.sipri[mildata$iso3c=="GAB"&mildata$year==1990] * gab.alpha.3

# 1992-1999: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "GAB", 2000, restricted = c(1992:1999)
)

# 2007-2009: estimate SIPRI 2007-2009 as equidistant points based on COW 2006-2010 values
mildata$mil.expenditure.sipri[mildata$iso3c=="GAB"&mildata$year==2007] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "GAB", 2007, yr.minus.1 = 2006, yr.plus.1 = 2010)
mildata$mil.expenditure.sipri[mildata$iso3c=="GAB"&mildata$year==2008] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "GAB", 2008, yr.minus.1 = 2006, yr.plus.1 = 2010)
mildata$mil.expenditure.sipri[mildata$iso3c=="GAB"&mildata$year==2009] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "GAB", 2009, yr.minus.1 = 2006, yr.plus.1 = 2010)

##### cow personnel
# good

##### wmeat personnel
# 1960-1962: COW and WMEAT personnel match 1963-1981, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1960:1962)&iso3c=="GAB",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2000-2001 - 2016-2017 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="GAB"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="GAB"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="GAB"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="GAB"&mildata$year==2018]

# x <- milexp.viewer("GAB")
# y <- milper.viewer("GAB")

##### GBR ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1946-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "GBR", 1963, restricted = c(1946:1962)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "GBR", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1949: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "GBR", 1950, restricted = c(1946:1949)
)

##### cow personnel
# good

##### wmeat personnel
# 1945-1962: COW and WMEAT personnel match 1963-1969, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1945:1962)&iso3c=="GBR",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "GBR", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("GBR")
# y <- milper.viewer("GBR")

##### GEO ----------------------------------------------------------------------
# TODO: check COW 1992 expenditure value - looks like there is an extra 0

# 1991 personnel estimates source: https://on.ge/story/10303-ქართული-ჯარის-დიდი-დღე

##### cow expenditure
# 1993
mildata$mil.expenditure.cow[mildata$iso3c=="GEO"&mildata$year==1993] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "GEO", 1993, yr.minus.1 = 1992, yr.plus.1 = 1995)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="GEO"&mildata$year==1993] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "GEO", 1993, yr.minus.1 = 1992, yr.plus.1 = 1995)

# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"GEO",
  metric = "mil.expenditure.cow",
  1991,1991
)
# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"GEO",
  metric = "mil.expenditure.cow.alt",
  1991,1991
)

##### wmeat expenditure
# 1994
mildata$mil.expenditure.wmeat[mildata$iso3c=="GEO"&mildata$year==1994] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "GEO", 1994, yr.minus.1 = 1992, yr.plus.1 = 1995)

# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"GEO",
  metric = "mil.expenditure.wmeat",
  1991,1991
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "GEO", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1994-1995: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "GEO", 1996, restricted = c(1994:1995)
)

# 1992-1993: apply WMEAT growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.wmeat",
  "GEO", 1995, restricted = c(1992:1993)
)

# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"GEO",
  metric = "mil.expenditure.sipri",
  1991,1991
)

##### cow personnel
# 1991: estimate via news article source
mildata$mil.personnel.cow[mildata$iso3c=="GEO"&mildata$year==1991] <- 10000

##### wmeat personnel
# 1991: estimate via news article source
mildata$mil.personnel.wmeat[mildata$iso3c=="GEO"&mildata$year==1991] <- 10000

# 1992: assume consistent growth between 1991 and 1993
geo.alpha <- (mildata$mil.personnel.wmeat[mildata$iso3c=="GEO"&mildata$year==1993]/mildata$mil.personnel.wmeat[mildata$iso3c=="GEO"&mildata$year==1991])^(1/2)

mildata$mil.personnel.wmeat[mildata$iso3c=="GEO"&mildata$year==1992] <- mildata$mil.personnel.wmeat[mildata$iso3c=="GEO"&mildata$year==1991] * geo.alpha

# 2018-2019: 2008-2009 - 2016-2017 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="GEO"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="GEO"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="GEO"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="GEO"&mildata$year==2018]

# x <- milexp.viewer("GEO")
# y <- milper.viewer("GEO")

##### GHA ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1957-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "GHA", 1963, restricted = c(1957:1962)
)

# 1983
mildata$mil.expenditure.wmeat[mildata$iso3c=="GHA"&mildata$year==1983] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "GHA", 1983)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "GHA", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1957: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "GHA", 1958, restricted = 1957
)

# 1967
mildata$mil.expenditure.sipri[mildata$iso3c=="GHA"&mildata$year==1967] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "GHA", 1967)

##### cow personnel
# good

##### wmeat personnel
# 1957-1962: COW and WMEAT personnel match 1963-1998, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1957:1962)&iso3c=="GHA",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2012-2013 - 2016-2017 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="GHA"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="GHA"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="GHA"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="GHA"&mildata$year==2018]

# x <- milexp.viewer("GHA")
# y <- milper.viewer("GHA")

##### GIN ----------------------------------------------------------------------

##### cow expenditure
# 2012-2013; 2015-2019: apply IISS growth estimates based on 2011
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "defense.spending.iiss",
  "GIN", 2011, restricted = c(2012:2013,2015:2019)
)

## remove estimate flag for 2012-2013, but not 2015-2019 due to the 2014 gap
mildata <- mildata %>%
  dplyr::mutate(mil.expenditure.cow.est.flag  = dplyr::case_when(
    iso3c == "GIN" & year %in% c(2012:2013) ~ 0,
    .default = mil.expenditure.cow.est.flag
  ))

# 2010: calculate IISS 2010 as a percent of 2009 and 2011 values, apply proportions
# to COW estimates, and average the two estimates
mildata$mil.expenditure.cow[mildata$iso3c=="GIN"&mildata$year==2010] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "IISS", "GIN", 2010)

# 2014: calculate WMEAT 2014 as a percent of 2013 and 2015 values, apply proportions
# to COW estimates, and average the two estimates
mildata$mil.expenditure.cow[mildata$iso3c=="GIN"&mildata$year==2014] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "GIN", 2014)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="GIN"&mildata$year==2014] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "GIN", 2014)

# 1958-1959
mildata <- mil_estimator_no_data_func(
  mildata,"GIN",
  metric = "mil.expenditure.cow",
  1959,1958
)
mildata <- mil_estimator_no_data_func(
  mildata,"GIN",
  metric = "mil.expenditure.cow.alt",
  1959,1958
)

# 1976: assume consistent growth between 1975 and 1977
gin.alpha.1 <- (mildata$mil.expenditure.cow[mildata$iso3c=="GIN"&mildata$year==1977]/mildata$mil.expenditure.cow[mildata$iso3c=="GIN"&mildata$year==1975])^(1/2)

mildata$mil.expenditure.cow[mildata$iso3c=="GIN"&mildata$year==1976] <- mildata$mil.expenditure.cow[mildata$iso3c=="GIN"&mildata$year==1975] * gin.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="GIN"&mildata$year==1976] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="GIN"&mildata$year==1975] * gin.alpha.1

# 1978-1980: apply SIPRI growth rates to COW
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "mil.expenditure.sipri",
  "GIN", 1981, restricted = c(1978:1980)
)
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow.alt",
  col_ref = "mil.expenditure.sipri",
  "GIN", 1981, restricted = c(1978:1980)
)

# 1985-1987: assume consistent growth between 1984 and 1988
gin.alpha.2 <- (mildata$mil.expenditure.cow[mildata$iso3c=="GIN"&mildata$year==1988]/mildata$mil.expenditure.cow[mildata$iso3c=="GIN"&mildata$year==1984])^(1/4)

mildata$mil.expenditure.cow[mildata$iso3c=="GIN"&mildata$year==1985] <- mildata$mil.expenditure.cow[mildata$iso3c=="GIN"&mildata$year==1984] * gin.alpha.2
mildata$mil.expenditure.cow[mildata$iso3c=="GIN"&mildata$year==1986] <- mildata$mil.expenditure.cow[mildata$iso3c=="GIN"&mildata$year==1985] * gin.alpha.2
mildata$mil.expenditure.cow[mildata$iso3c=="GIN"&mildata$year==1987] <- mildata$mil.expenditure.cow[mildata$iso3c=="GIN"&mildata$year==1986] * gin.alpha.2
mildata$mil.expenditure.cow.alt[mildata$iso3c=="GIN"&mildata$year==1985] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="GIN"&mildata$year==1984] * gin.alpha.2
mildata$mil.expenditure.cow.alt[mildata$iso3c=="GIN"&mildata$year==1986] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="GIN"&mildata$year==1985] * gin.alpha.2
mildata$mil.expenditure.cow.alt[mildata$iso3c=="GIN"&mildata$year==1987] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="GIN"&mildata$year==1986] * gin.alpha.2

# 1992: calculate WMEAT 1992 as a percent of 1991 and 1994 values (WMEAT 1993 is NA),
# apply proportions to COW estimates, and average the two estimates
mildata$mil.expenditure.cow[mildata$iso3c=="GIN"&mildata$year==1992] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "GIN", 1992, yr.plus.1 = 1994)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="GIN"&mildata$year==1992] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "GIN", 1992, yr.plus.1 = 1994)

##### wmeat expenditure
# 1960-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "GIN", 1963, restricted = c(1960:1962)
)

# 1958-1959
mildata <- mil_estimator_no_data_func(
  mildata,"GIN",
  metric = "mil.expenditure.wmeat",
  1959,1958
)

# 1977-1980: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "GIN", 1981, restricted = c(1977:1980)
)

# 1976: assume consistent growth between 1975 and 1977
gin.alpha.3 <- (mildata$mil.expenditure.wmeat[mildata$iso3c=="GIN"&mildata$year==1977]/mildata$mil.expenditure.wmeat[mildata$iso3c=="GIN"&mildata$year==1975])^(1/2)

mildata$mil.expenditure.wmeat[mildata$iso3c=="GIN"&mildata$year==1976] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="GIN"&mildata$year==1975] * gin.alpha.3

# 1982-1983
mildata$mil.expenditure.wmeat[mildata$iso3c=="GIN"&mildata$year==1982] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "GIN", 1982, yr.minus.1 = 1981, yr.plus.1 = 1984)
mildata$mil.expenditure.wmeat[mildata$iso3c=="GIN"&mildata$year==1983] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "GIN", 1983, yr.minus.1 = 1981, yr.plus.1 = 1984)

# 1985-1987: assume consistent growth between 1984 and 1988
gin.alpha.4 <- (mildata$mil.expenditure.wmeat[mildata$iso3c=="GIN"&mildata$year==1988]/mildata$mil.expenditure.wmeat[mildata$iso3c=="GIN"&mildata$year==1984])^(1/4)

mildata$mil.expenditure.wmeat[mildata$iso3c=="GIN"&mildata$year==1985] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="GIN"&mildata$year==1984] * gin.alpha.4
mildata$mil.expenditure.wmeat[mildata$iso3c=="GIN"&mildata$year==1986] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="GIN"&mildata$year==1985] * gin.alpha.4
mildata$mil.expenditure.wmeat[mildata$iso3c=="GIN"&mildata$year==1987] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="GIN"&mildata$year==1986] * gin.alpha.4

# 1989
mildata$mil.expenditure.wmeat[mildata$iso3c=="GIN"&mildata$year==1989] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "GIN", 1989)

# 1993
mildata$mil.expenditure.wmeat[mildata$iso3c=="GIN"&mildata$year==1993] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "GIN", 1993, yr.minus.1 = 1991)

# 1995-1996
mildata$mil.expenditure.wmeat[mildata$iso3c=="GIN"&mildata$year==1995] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "GIN", 1995, yr.minus.1 = 1994, yr.plus.1 = 1997)
mildata$mil.expenditure.wmeat[mildata$iso3c=="GIN"&mildata$year==1996] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "GIN", 1996, yr.minus.1 = 1994, yr.plus.1 = 1997)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "GIN", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1960-1964: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "GIN", 1965, restricted = c(1960:1964)
)

# 1958-1959
mildata <- mil_estimator_no_data_func(
  mildata,"GIN",
  metric = "mil.expenditure.sipri",
  1959,1958
)

# 1967
mildata$mil.expenditure.sipri[mildata$iso3c=="GIN"&mildata$year==1967] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "GIN", 1967)

# 1974-1975: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "GIN", 1973, restricted = c(1974:1975)
)

# 1977: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "GIN", 1981, restricted = 1977
)

# 1976: assume consistent growth between 1975 and 1977
gin.alpha.5 <- (mildata$mil.expenditure.sipri[mildata$iso3c=="GIN"&mildata$year==1977]/mildata$mil.expenditure.sipri[mildata$iso3c=="GIN"&mildata$year==1975])^(1/2)

mildata$mil.expenditure.sipri[mildata$iso3c=="GIN"&mildata$year==1976] <- mildata$mil.expenditure.sipri[mildata$iso3c=="GIN"&mildata$year==1975] * gin.alpha.5

# 1983-1984: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "GIN", 1982, restricted = c(1983:1984)
)

# 1985-1987: assume consistent growth between 1984 and 1988
gin.alpha.6 <- (mildata$mil.expenditure.sipri[mildata$iso3c=="GIN"&mildata$year==1988]/mildata$mil.expenditure.sipri[mildata$iso3c=="GIN"&mildata$year==1984])^(1/4)

mildata$mil.expenditure.sipri[mildata$iso3c=="GIN"&mildata$year==1985] <- mildata$mil.expenditure.sipri[mildata$iso3c=="GIN"&mildata$year==1984] * gin.alpha.6
mildata$mil.expenditure.sipri[mildata$iso3c=="GIN"&mildata$year==1986] <- mildata$mil.expenditure.sipri[mildata$iso3c=="GIN"&mildata$year==1985] * gin.alpha.6
mildata$mil.expenditure.sipri[mildata$iso3c=="GIN"&mildata$year==1987] <- mildata$mil.expenditure.sipri[mildata$iso3c=="GIN"&mildata$year==1986] * gin.alpha.6

# 1988-1990: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "GIN", 1991, restricted = c(1988:1990)
)

# 1995-1996
mildata$mil.expenditure.sipri[mildata$iso3c=="GIN"&mildata$year==1995] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "GIN", 1995, yr.minus.1 = 1994, yr.plus.1 = 1997)
mildata$mil.expenditure.sipri[mildata$iso3c=="GIN"&mildata$year==1996] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "GIN", 1996, yr.minus.1 = 1994, yr.plus.1 = 1997)

# 2005-2011
mildata$mil.expenditure.sipri[mildata$iso3c=="GIN"&mildata$year==2005] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT est", "GIN", 2005, yr.minus.1 = 2004, yr.plus.1 = 2012)
mildata$mil.expenditure.sipri[mildata$iso3c=="GIN"&mildata$year==2006] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT est", "GIN", 2006, yr.minus.1 = 2004, yr.plus.1 = 2012)
mildata$mil.expenditure.sipri[mildata$iso3c=="GIN"&mildata$year==2007] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT est", "GIN", 2007, yr.minus.1 = 2004, yr.plus.1 = 2012)
mildata$mil.expenditure.sipri[mildata$iso3c=="GIN"&mildata$year==2008] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT est", "GIN", 2008, yr.minus.1 = 2004, yr.plus.1 = 2012)
mildata$mil.expenditure.sipri[mildata$iso3c=="GIN"&mildata$year==2009] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT est", "GIN", 2009, yr.minus.1 = 2004, yr.plus.1 = 2012)
mildata$mil.expenditure.sipri[mildata$iso3c=="GIN"&mildata$year==2010] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT est", "GIN", 2010, yr.minus.1 = 2004, yr.plus.1 = 2012)
mildata$mil.expenditure.sipri[mildata$iso3c=="GIN"&mildata$year==2011] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT est", "GIN", 2011, yr.minus.1 = 2004, yr.plus.1 = 2012)

##### cow personnel
# good

##### wmeat personnel
# 1958-1962: COW and WMEAT personnel match 1963-1966, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1958:1962)&iso3c=="GIN",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2013-14 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="GIN"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="GIN"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="GIN"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="GIN"&mildata$year==2018]

# x <- milexp.viewer("GIN")
# y <- milper.viewer("GIN")

##### GMB ----------------------------------------------------------------------

##### cow expenditure
# 2012-2015: apply IISS growth estimates based on 2011
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "defense.spending.iiss",
  "GMB", 2011, restricted = c(2012:2015)
)

## remove estimate flag for 2012-2015
mildata <- mildata %>%
  dplyr::mutate(mil.expenditure.cow.est.flag  = dplyr::case_when(
    iso3c == "GMB" & year %in% c(2012:2015) ~ 0,
    .default = mil.expenditure.cow.est.flag
  ))

# 2016-2017: apply WMEAT growth estimates based on 2015 COW estimate
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "mil.expenditure.wmeat",
  "GMB", 2015, restricted = c(2016:2017)
)
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow.alt",
  col_ref = "mil.expenditure.wmeat",
  "GMB", 2015, restricted = c(2016:2017)
)

# 2018-2019: apply SIPRI growth estimates based on 2015 COW estimate
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "mil.expenditure.sipri",
  "GMB", 2015, restricted = c(2018:2019)
)
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow.alt",
  col_ref = "mil.expenditure.sipri",
  "GMB", 2015, restricted = c(2018:2019)
)

# 2010: estimate COW 2010 as equidistant point based on IISS 2009-2011 values
mildata$mil.expenditure.cow[mildata$iso3c=="GMB"&mildata$year==2010] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "IISS", "GMB", 2010)

# 1990-1992
mildata$mil.expenditure.cow[mildata$iso3c=="GMB"&mildata$year==1990] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "GMB", 1990, yr.minus.1 = 1989, yr.plus.1 = 1993)
mildata$mil.expenditure.cow[mildata$iso3c=="GMB"&mildata$year==1991] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "GMB", 1991, yr.minus.1 = 1989, yr.plus.1 = 1993)
mildata$mil.expenditure.cow[mildata$iso3c=="GMB"&mildata$year==1992] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "GMB", 1992, yr.minus.1 = 1989, yr.plus.1 = 1993)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="GMB"&mildata$year==1990] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "GMB", 1990, yr.minus.1 = 1989, yr.plus.1 = 1993)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="GMB"&mildata$year==1991] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "GMB", 1991, yr.minus.1 = 1989, yr.plus.1 = 1993)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="GMB"&mildata$year==1992] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "GMB", 1992, yr.minus.1 = 1989, yr.plus.1 = 1993)

# 1986
mildata$mil.expenditure.cow[mildata$iso3c=="GMB"&mildata$year==1986] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "GMB", 1986)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="GMB"&mildata$year==1986] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "GMB", 1986)

# 1984
mildata$mil.expenditure.cow[mildata$iso3c=="GMB"&mildata$year==1984] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "GMB", 1984)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="GMB"&mildata$year==1984] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "GMB", 1984)

##### wmeat expenditure
# 1965-1972: extend WMEAT series using COW estimates
mildata <- mildata %>%
  dplyr::mutate(mil.expenditure.wmeat = ifelse(year %in% c(1965:1972)&iso3c=="GMB",mil.expenditure.cow,mil.expenditure.wmeat))

# 1984-1987
mildata$mil.expenditure.wmeat[mildata$iso3c=="GMB"&mildata$year==1984] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "GMB", 1984, yr.minus.1 = 1983, yr.plus.1 = 1988)
mildata$mil.expenditure.wmeat[mildata$iso3c=="GMB"&mildata$year==1985] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "GMB", 1985, yr.minus.1 = 1983, yr.plus.1 = 1988)
mildata$mil.expenditure.wmeat[mildata$iso3c=="GMB"&mildata$year==1986] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "GMB", 1986, yr.minus.1 = 1983, yr.plus.1 = 1988)
mildata$mil.expenditure.wmeat[mildata$iso3c=="GMB"&mildata$year==1987] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "GMB", 1987, yr.minus.1 = 1983, yr.plus.1 = 1988)

# 1989
mildata$mil.expenditure.wmeat[mildata$iso3c=="GMB"&mildata$year==1989] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "GMB", 1989)

# 1991
mildata$mil.expenditure.wmeat[mildata$iso3c=="GMB"&mildata$year==1991] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "GMB", 1991)

# 2018-2019: apply SIPRI growth estimates based on 2015 WMEAT estimate
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "GMB", 2015, restricted = c(2018:2019)
)

##### sipri expenditure
# 1981
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "GMB", 1983, restricted = 1981
)

# 1982: assume consistant growth between 1981 and 1983
gmb.alpha.1 <- (mildata$mil.expenditure.sipri[mildata$iso3c=="GMB"&mildata$year==1983]/mildata$mil.expenditure.sipri[mildata$iso3c=="GMB"&mildata$year==1981])^(1/2)

mildata$mil.expenditure.sipri[mildata$iso3c=="GMB"&mildata$year==1982] <- mildata$mil.expenditure.sipri[mildata$iso3c=="GMB"&mildata$year==1981] * gmb.alpha.1

# 1965-1980: extend SIPRI series using COW estimates
mildata <- mildata %>%
  dplyr::mutate(mil.expenditure.sipri = ifelse(year %in% c(1965:1980)&iso3c=="GMB",mil.expenditure.cow,mil.expenditure.sipri))

# 2010-2011
mildata$mil.expenditure.sipri[mildata$iso3c=="GMB"&mildata$year==2010] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "GMB", 2010, yr.minus.1 = 2009, yr.plus.1 = 2012)
mildata$mil.expenditure.sipri[mildata$iso3c=="GMB"&mildata$year==2011] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "GMB", 2011, yr.minus.1 = 2009, yr.plus.1 = 2012)

# 2016-2017
mildata$mil.expenditure.sipri[mildata$iso3c=="GMB"&mildata$year==2016] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW est", "GMB", 2016, yr.minus.1 = 2015, yr.plus.1 = 2018)
mildata$mil.expenditure.sipri[mildata$iso3c=="GMB"&mildata$year==2017] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW est", "GMB", 2017, yr.minus.1 = 2015, yr.plus.1 = 2018)

##### cow personnel
# good

##### wmeat personnel
# 1965-1966: 1967-68 - 1972-73 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="GMB"&mildata$year==1966] <- mildata$mil.personnel.wmeat[mildata$iso3c=="GMB"&mildata$year==1967]
mildata$mil.personnel.wmeat[mildata$iso3c=="GMB"&mildata$year==1965] <- mildata$mil.personnel.wmeat[mildata$iso3c=="GMB"&mildata$year==1966]

# 1974: assume consistent decline
gmb.alpha.2 <- (mildata$mil.personnel.wmeat[mildata$iso3c=="GMB"&mildata$year==1975] - mildata$mil.personnel.wmeat[mildata$iso3c=="GMB"&mildata$year==1973]) / 2

mildata$mil.personnel.wmeat[mildata$iso3c=="GMB"&mildata$year==1974] <- mildata$mil.personnel.wmeat[mildata$iso3c=="GMB"&mildata$year==1973] + gmb.alpha.2

# 2018-2019: 2009-10 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="GMB"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="GMB"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="GMB"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="GMB"&mildata$year==2018]

# x <- milexp.viewer("GMB")
# y <- milper.viewer("GMB")

##### GNB ----------------------------------------------------------------------

##### cow expenditure
# 1975: assume consistent growth between 1974 and 1976
gnb.alpha.1 <- (mildata$mil.expenditure.cow[mildata$iso3c=="GNB"&mildata$year==1976]/mildata$mil.expenditure.cow[mildata$iso3c=="GNB"&mildata$year==1974])^(1/2)

mildata$mil.expenditure.cow[mildata$iso3c=="GNB"&mildata$year==1975] <- mildata$mil.expenditure.cow[mildata$iso3c=="GNB"&mildata$year==1974] * gnb.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="GNB"&mildata$year==1975] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="GNB"&mildata$year==1974] * gnb.alpha.1

# 1988: assume consistent growth between 1987 and 1989
gnb.alpha.2 <- (mildata$mil.expenditure.cow[mildata$iso3c=="GNB"&mildata$year==1989]/mildata$mil.expenditure.cow[mildata$iso3c=="GNB"&mildata$year==1987])^(1/2)

mildata$mil.expenditure.cow[mildata$iso3c=="GNB"&mildata$year==1988] <- mildata$mil.expenditure.cow[mildata$iso3c=="GNB"&mildata$year==1987] * gnb.alpha.2
mildata$mil.expenditure.cow.alt[mildata$iso3c=="GNB"&mildata$year==1988] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="GNB"&mildata$year==1987] * gnb.alpha.2

# 1992-1993: estimate COW 1992-1993 as equidistant point based on WMEAT 1989-1994 values (WMEAT 1989-1990 is NA)
mildata$mil.expenditure.cow[mildata$iso3c=="GNB"&mildata$year==1992] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "GNB", 1992, yr.minus.1 = 1989, yr.plus.1 = 1994)
mildata$mil.expenditure.cow[mildata$iso3c=="GNB"&mildata$year==1993] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "GNB", 1993, yr.minus.1 = 1989, yr.plus.1 = 1994)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="GNB"&mildata$year==1992] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "GNB", 1992, yr.minus.1 = 1989, yr.plus.1 = 1994)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="GNB"&mildata$year==1993] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "GNB", 1993, yr.minus.1 = 1989, yr.plus.1 = 1994)

# 1991: assume consistent growth between 1990 and 1992
gnb.alpha.3 <- (mildata$mil.expenditure.cow[mildata$iso3c=="GNB"&mildata$year==1992]/mildata$mil.expenditure.cow[mildata$iso3c=="GNB"&mildata$year==1990])^(1/2)

mildata$mil.expenditure.cow[mildata$iso3c=="GNB"&mildata$year==1991] <- mildata$mil.expenditure.cow[mildata$iso3c=="GNB"&mildata$year==1990] * gnb.alpha.3
mildata$mil.expenditure.cow.alt[mildata$iso3c=="GNB"&mildata$year==1991] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="GNB"&mildata$year==1990] * gnb.alpha.3

# 2013: apply IISS growth estimates based on 2012
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "defense.spending.iiss",
  "GNB", 2012, restricted = 2013
)

## remove estimate flag for 2013
mildata <- mildata %>%
  dplyr::mutate(mil.expenditure.cow.est.flag  = dplyr::case_when(
    iso3c == "GNB" & year == 2013 ~ 0,
    .default = mil.expenditure.cow.est.flag
  ))

# 2014-2017: apply SIPRI growth rates to COW
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "mil.expenditure.sipri",
  "GNB", 2013, restricted = c(2014:2017)
)
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow.alt",
  col_ref = "mil.expenditure.sipri",
  "GNB", 2013, restricted = c(2014:2017)
)

# 2018-2019
mildata <- mil_estimator_no_data_func(
  mildata,"GNB",
  metric = "mil.expenditure.cow",
  2018,2019
)
mildata <- mil_estimator_no_data_func(
  mildata,"GNB",
  metric = "mil.expenditure.cow.alt",
  2018,2019
)

##### wmeat expenditure
# 1974: recalculate by applying COW growth rate to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "GNB", 1976, restricted = 1974
)

# 1975: assume consistent growth between 1990 and 1992
gnb.alpha.4 <- (mildata$mil.expenditure.wmeat[mildata$iso3c=="GNB"&mildata$year==1976]/mildata$mil.expenditure.wmeat[mildata$iso3c=="GNB"&mildata$year==1974])^(1/2)

mildata$mil.expenditure.wmeat[mildata$iso3c=="GNB"&mildata$year==1975] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="GNB"&mildata$year==1974] * gnb.alpha.4

# 1983
mildata$mil.expenditure.wmeat[mildata$iso3c=="GNB"&mildata$year==1983] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "GNB", 1983)

# 1988: assume consistent growth between 1987 and 1989
gnb.alpha.5 <- (mildata$mil.expenditure.wmeat[mildata$iso3c=="GNB"&mildata$year==1989]/mildata$mil.expenditure.wmeat[mildata$iso3c=="GNB"&mildata$year==1987])^(1/2)

mildata$mil.expenditure.wmeat[mildata$iso3c=="GNB"&mildata$year==1988] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="GNB"&mildata$year==1987] * gnb.alpha.5

# 1990: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "GNB", 1989, restricted = 1990
)

# 1991: assume consistent growth between 1990 and 1992
gnb.alpha.6 <- (mildata$mil.expenditure.wmeat[mildata$iso3c=="GNB"&mildata$year==1992]/mildata$mil.expenditure.wmeat[mildata$iso3c=="GNB"&mildata$year==1990])^(1/2)

mildata$mil.expenditure.wmeat[mildata$iso3c=="GNB"&mildata$year==1991] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="GNB"&mildata$year==1990] * gnb.alpha.6

# 2018-2019
mildata <- mil_estimator_no_data_func(
  mildata,"GNB",
  metric = "mil.expenditure.wmeat",
  2018,2019
)

##### sipri expenditure
# 1974-1981: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "GNB", 1982, restricted = c(1974:1981)
)

# 1992-1993: apply WMEAT growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.wmeat",
  "GNB", 1994, restricted = c(1992:1993)
)

# 1990: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "GNB", 1989, restricted = 1990
)

# 1988: assume consistent growth between 1987 and 1989
gnb.alpha.7 <- (mildata$mil.expenditure.sipri[mildata$iso3c=="GNB"&mildata$year==1989]/mildata$mil.expenditure.sipri[mildata$iso3c=="GNB"&mildata$year==1987])^(1/2)

mildata$mil.expenditure.sipri[mildata$iso3c=="GNB"&mildata$year==1988] <- mildata$mil.expenditure.sipri[mildata$iso3c=="GNB"&mildata$year==1987] * gnb.alpha.7

# 1991: assume consistent growth between 1990 and 1992
gnb.alpha.8 <- (mildata$mil.expenditure.sipri[mildata$iso3c=="GNB"&mildata$year==1992]/mildata$mil.expenditure.sipri[mildata$iso3c=="GNB"&mildata$year==1990])^(1/2)

mildata$mil.expenditure.sipri[mildata$iso3c=="GNB"&mildata$year==1991] <- mildata$mil.expenditure.sipri[mildata$iso3c=="GNB"&mildata$year==1990] * gnb.alpha.8

# 1999
mildata$mil.expenditure.sipri[mildata$iso3c=="GNB"&mildata$year==1999] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "GNB", 1999)

# 2004
mildata$mil.expenditure.sipri[mildata$iso3c=="GNB"&mildata$year==2004] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "GNB", 2004)

# 2006-2008
mildata$mil.expenditure.sipri[mildata$iso3c=="GNB"&mildata$year==2006] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "GNB", 2006, yr.minus.1 = 2005, yr.plus.1 = 2009)
mildata$mil.expenditure.sipri[mildata$iso3c=="GNB"&mildata$year==2007] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "GNB", 2007, yr.minus.1 = 2005, yr.plus.1 = 2009)
mildata$mil.expenditure.sipri[mildata$iso3c=="GNB"&mildata$year==2008] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "GNB", 2008, yr.minus.1 = 2005, yr.plus.1 = 2009)

# 2018-2019
mildata <- mil_estimator_no_data_func(
  mildata,"GNB",
  metric = "mil.expenditure.sipri",
  2018,2019
)

##### cow personnel
# good

##### wmeat personnel
# 2018-2019: 2011-12 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="GNB"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="GNB"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="GNB"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="GNB"&mildata$year==2018]

# x <- milexp.viewer("GNB")
# y <- milper.viewer("GNB")

##### GNQ ----------------------------------------------------------------------

##### cow expenditure
# 1968: reestimate
mildata <- mil_estimator_no_data_func(
  mildata,"GNQ",
  metric = "mil.expenditure.cow",
  1968,1968
)
mildata <- mil_estimator_no_data_func(
  mildata,"GNQ",
  metric = "mil.expenditure.cow.alt",
  1968,1968
)

# 1977-1979: recalculate assuming consistent growth between 1976 and 1980
gnq.alpha.1 <- (mildata$mil.expenditure.cow[mildata$iso3c=="GNQ"&mildata$year==1980]/mildata$mil.expenditure.cow[mildata$iso3c=="GNQ"&mildata$year==1976])^(1/4)

mildata$mil.expenditure.cow[mildata$iso3c=="GNQ"&mildata$year==1977] <- mildata$mil.expenditure.cow[mildata$iso3c=="GNQ"&mildata$year==1976] * gnq.alpha.1
mildata$mil.expenditure.cow[mildata$iso3c=="GNQ"&mildata$year==1978] <- mildata$mil.expenditure.cow[mildata$iso3c=="GNQ"&mildata$year==1977] * gnq.alpha.1
mildata$mil.expenditure.cow[mildata$iso3c=="GNQ"&mildata$year==1979] <- mildata$mil.expenditure.cow[mildata$iso3c=="GNQ"&mildata$year==1978] * gnq.alpha.1

mildata$mil.expenditure.cow.alt[mildata$iso3c=="GNQ"&mildata$year==1977] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="GNQ"&mildata$year==1976] * gnq.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="GNQ"&mildata$year==1978] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="GNQ"&mildata$year==1977] * gnq.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="GNQ"&mildata$year==1979] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="GNQ"&mildata$year==1978] * gnq.alpha.1

# 1983-1993: recalculate assuming consistent growth between 1982 and 1994
gnq.alpha.2 <- (mildata$mil.expenditure.cow[mildata$iso3c=="GNQ"&mildata$year==1994]/mildata$mil.expenditure.cow[mildata$iso3c=="GNQ"&mildata$year==1982])^(1/12)

mildata$mil.expenditure.cow[mildata$iso3c=="GNQ"&mildata$year==1983] <- mildata$mil.expenditure.cow[mildata$iso3c=="GNQ"&mildata$year==1982] * gnq.alpha.2
mildata$mil.expenditure.cow[mildata$iso3c=="GNQ"&mildata$year==1984] <- mildata$mil.expenditure.cow[mildata$iso3c=="GNQ"&mildata$year==1983] * gnq.alpha.2
mildata$mil.expenditure.cow[mildata$iso3c=="GNQ"&mildata$year==1985] <- mildata$mil.expenditure.cow[mildata$iso3c=="GNQ"&mildata$year==1984] * gnq.alpha.2
mildata$mil.expenditure.cow[mildata$iso3c=="GNQ"&mildata$year==1986] <- mildata$mil.expenditure.cow[mildata$iso3c=="GNQ"&mildata$year==1985] * gnq.alpha.2
mildata$mil.expenditure.cow[mildata$iso3c=="GNQ"&mildata$year==1987] <- mildata$mil.expenditure.cow[mildata$iso3c=="GNQ"&mildata$year==1986] * gnq.alpha.2
mildata$mil.expenditure.cow[mildata$iso3c=="GNQ"&mildata$year==1988] <- mildata$mil.expenditure.cow[mildata$iso3c=="GNQ"&mildata$year==1987] * gnq.alpha.2
mildata$mil.expenditure.cow[mildata$iso3c=="GNQ"&mildata$year==1989] <- mildata$mil.expenditure.cow[mildata$iso3c=="GNQ"&mildata$year==1988] * gnq.alpha.2
mildata$mil.expenditure.cow[mildata$iso3c=="GNQ"&mildata$year==1990] <- mildata$mil.expenditure.cow[mildata$iso3c=="GNQ"&mildata$year==1989] * gnq.alpha.2
mildata$mil.expenditure.cow[mildata$iso3c=="GNQ"&mildata$year==1991] <- mildata$mil.expenditure.cow[mildata$iso3c=="GNQ"&mildata$year==1990] * gnq.alpha.2
mildata$mil.expenditure.cow[mildata$iso3c=="GNQ"&mildata$year==1992] <- mildata$mil.expenditure.cow[mildata$iso3c=="GNQ"&mildata$year==1991] * gnq.alpha.2
mildata$mil.expenditure.cow[mildata$iso3c=="GNQ"&mildata$year==1993] <- mildata$mil.expenditure.cow[mildata$iso3c=="GNQ"&mildata$year==1992] * gnq.alpha.2

mildata$mil.expenditure.cow.alt[mildata$iso3c=="GNQ"&mildata$year==1983] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="GNQ"&mildata$year==1982] * gnq.alpha.2
mildata$mil.expenditure.cow.alt[mildata$iso3c=="GNQ"&mildata$year==1984] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="GNQ"&mildata$year==1983] * gnq.alpha.2
mildata$mil.expenditure.cow.alt[mildata$iso3c=="GNQ"&mildata$year==1985] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="GNQ"&mildata$year==1984] * gnq.alpha.2
mildata$mil.expenditure.cow.alt[mildata$iso3c=="GNQ"&mildata$year==1986] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="GNQ"&mildata$year==1985] * gnq.alpha.2
mildata$mil.expenditure.cow.alt[mildata$iso3c=="GNQ"&mildata$year==1987] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="GNQ"&mildata$year==1986] * gnq.alpha.2
mildata$mil.expenditure.cow.alt[mildata$iso3c=="GNQ"&mildata$year==1988] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="GNQ"&mildata$year==1987] * gnq.alpha.2
mildata$mil.expenditure.cow.alt[mildata$iso3c=="GNQ"&mildata$year==1989] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="GNQ"&mildata$year==1988] * gnq.alpha.2
mildata$mil.expenditure.cow.alt[mildata$iso3c=="GNQ"&mildata$year==1990] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="GNQ"&mildata$year==1989] * gnq.alpha.2
mildata$mil.expenditure.cow.alt[mildata$iso3c=="GNQ"&mildata$year==1991] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="GNQ"&mildata$year==1990] * gnq.alpha.2
mildata$mil.expenditure.cow.alt[mildata$iso3c=="GNQ"&mildata$year==1992] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="GNQ"&mildata$year==1991] * gnq.alpha.2
mildata$mil.expenditure.cow.alt[mildata$iso3c=="GNQ"&mildata$year==1993] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="GNQ"&mildata$year==1992] * gnq.alpha.2

# 2010 (non-alt): estimate COW 2010 as equidistant point based on IISS 2009-2011 values
mildata$mil.expenditure.cow[mildata$iso3c=="GNQ"&mildata$year==2010] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "IISS", "GNQ", 2010)

# 2012-2013: apply IISS growth estimates based on 2011 COW estimate
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "defense.spending.iiss",
  "GNQ", 2011, restricted = c(2012:2013)
)

## remove estimate flag for 2012-2013
mildata <- mildata %>%
  dplyr::mutate(mil.expenditure.cow.est.flag  = dplyr::case_when(
    iso3c == "GNQ" & year %in% c(2012:2013) ~ 0,
    .default = mil.expenditure.cow.est.flag
  ))

# 2014-2019
mildata <- mil_estimator_no_data_func(
  mildata,"GNQ",
  metric = "mil.expenditure.cow",
  2014,2019
)
mildata <- mil_estimator_no_data_func(
  mildata,"GNQ",
  metric = "mil.expenditure.cow.alt",
  2014,2019
)

##### wmeat expenditure
# 1968: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "GNQ", 1969, restricted = 1968
)

# 1976: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "GNQ", 1975, restricted = 1976
)

# 1980; 1982: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "GNQ", 1981, restricted = c(1980, 1982)
)

# 1977-1979: recalculate assuming consistent growth between 1976 and 1980
gnq.alpha.3 <- (mildata$mil.expenditure.wmeat[mildata$iso3c=="GNQ"&mildata$year==1980]/mildata$mil.expenditure.wmeat[mildata$iso3c=="GNQ"&mildata$year==1976])^(1/4)

mildata$mil.expenditure.wmeat[mildata$iso3c=="GNQ"&mildata$year==1977] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="GNQ"&mildata$year==1976] * gnq.alpha.3
mildata$mil.expenditure.wmeat[mildata$iso3c=="GNQ"&mildata$year==1978] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="GNQ"&mildata$year==1977] * gnq.alpha.3
mildata$mil.expenditure.wmeat[mildata$iso3c=="GNQ"&mildata$year==1979] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="GNQ"&mildata$year==1978] * gnq.alpha.3

# 1983-1993: recalculate assuming consistent growth between 1982 and 1994
gnq.alpha.4 <- (mildata$mil.expenditure.cow[mildata$iso3c=="GNQ"&mildata$year==1994]/mildata$mil.expenditure.cow[mildata$iso3c=="GNQ"&mildata$year==1982])^(1/12)

mildata$mil.expenditure.wmeat[mildata$iso3c=="GNQ"&mildata$year==1983] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="GNQ"&mildata$year==1982] * gnq.alpha.4
mildata$mil.expenditure.wmeat[mildata$iso3c=="GNQ"&mildata$year==1984] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="GNQ"&mildata$year==1983] * gnq.alpha.4
mildata$mil.expenditure.wmeat[mildata$iso3c=="GNQ"&mildata$year==1985] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="GNQ"&mildata$year==1984] * gnq.alpha.4
mildata$mil.expenditure.wmeat[mildata$iso3c=="GNQ"&mildata$year==1986] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="GNQ"&mildata$year==1985] * gnq.alpha.4
mildata$mil.expenditure.wmeat[mildata$iso3c=="GNQ"&mildata$year==1987] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="GNQ"&mildata$year==1986] * gnq.alpha.4
mildata$mil.expenditure.wmeat[mildata$iso3c=="GNQ"&mildata$year==1988] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="GNQ"&mildata$year==1987] * gnq.alpha.4
mildata$mil.expenditure.wmeat[mildata$iso3c=="GNQ"&mildata$year==1989] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="GNQ"&mildata$year==1988] * gnq.alpha.4
mildata$mil.expenditure.wmeat[mildata$iso3c=="GNQ"&mildata$year==1990] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="GNQ"&mildata$year==1989] * gnq.alpha.4
mildata$mil.expenditure.wmeat[mildata$iso3c=="GNQ"&mildata$year==1991] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="GNQ"&mildata$year==1990] * gnq.alpha.4
mildata$mil.expenditure.wmeat[mildata$iso3c=="GNQ"&mildata$year==1992] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="GNQ"&mildata$year==1991] * gnq.alpha.4
mildata$mil.expenditure.wmeat[mildata$iso3c=="GNQ"&mildata$year==1993] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="GNQ"&mildata$year==1992] * gnq.alpha.4

# 2010-2013
mildata$mil.expenditure.wmeat[mildata$iso3c=="GNQ"&mildata$year==2010] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW est", "GNQ", 2010, yr.minus.1 = 2009, yr.plus.1 = 2014)
mildata$mil.expenditure.wmeat[mildata$iso3c=="GNQ"&mildata$year==2011] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW est", "GNQ", 2011, yr.minus.1 = 2009, yr.plus.1 = 2014)
mildata$mil.expenditure.wmeat[mildata$iso3c=="GNQ"&mildata$year==2012] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW est", "GNQ", 2012, yr.minus.1 = 2009, yr.plus.1 = 2014)
mildata$mil.expenditure.wmeat[mildata$iso3c=="GNQ"&mildata$year==2013] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW est", "GNQ", 2013, yr.minus.1 = 2009, yr.plus.1 = 2014)

# 2015-2018: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "GNQ", 2014, restricted = c(2015:2018)
)

# 2019
mildata <- mil_estimator_no_data_func(
  mildata,"GNQ",
  metric = "mil.expenditure.wmeat",
  2019,2019
)

##### sipri expenditure
# 1968-1993: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "GNQ", 1994, restricted = c(1968:1993)
)

# 1996-2006
mildata$mil.expenditure.sipri[mildata$iso3c=="GNQ"&mildata$year==1996] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "GNQ", 1996, yr.minus.1 = 1995, yr.plus.1 = 2007)
mildata$mil.expenditure.sipri[mildata$iso3c=="GNQ"&mildata$year==1997] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "GNQ", 1997, yr.minus.1 = 1995, yr.plus.1 = 2007)
mildata$mil.expenditure.sipri[mildata$iso3c=="GNQ"&mildata$year==1998] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "GNQ", 1998, yr.minus.1 = 1995, yr.plus.1 = 2007)
mildata$mil.expenditure.sipri[mildata$iso3c=="GNQ"&mildata$year==1999] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "GNQ", 1999, yr.minus.1 = 1995, yr.plus.1 = 2007)
mildata$mil.expenditure.sipri[mildata$iso3c=="GNQ"&mildata$year==2000] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "GNQ", 2000, yr.minus.1 = 1995, yr.plus.1 = 2007)
mildata$mil.expenditure.sipri[mildata$iso3c=="GNQ"&mildata$year==2001] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "GNQ", 2001, yr.minus.1 = 1995, yr.plus.1 = 2007)
mildata$mil.expenditure.sipri[mildata$iso3c=="GNQ"&mildata$year==2002] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "GNQ", 2002, yr.minus.1 = 1995, yr.plus.1 = 2007)
mildata$mil.expenditure.sipri[mildata$iso3c=="GNQ"&mildata$year==2003] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "GNQ", 2003, yr.minus.1 = 1995, yr.plus.1 = 2007)
mildata$mil.expenditure.sipri[mildata$iso3c=="GNQ"&mildata$year==2004] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "GNQ", 2004, yr.minus.1 = 1995, yr.plus.1 = 2007)
mildata$mil.expenditure.sipri[mildata$iso3c=="GNQ"&mildata$year==2005] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "GNQ", 2005, yr.minus.1 = 1995, yr.plus.1 = 2007)
mildata$mil.expenditure.sipri[mildata$iso3c=="GNQ"&mildata$year==2006] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "GNQ", 2006, yr.minus.1 = 1995, yr.plus.1 = 2007)

# 2010-2013
mildata$mil.expenditure.sipri[mildata$iso3c=="GNQ"&mildata$year==2010] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW est", "GNQ", 2010, yr.minus.1 = 2009, yr.plus.1 = 2014)
mildata$mil.expenditure.sipri[mildata$iso3c=="GNQ"&mildata$year==2011] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW est", "GNQ", 2011, yr.minus.1 = 2009, yr.plus.1 = 2014)
mildata$mil.expenditure.sipri[mildata$iso3c=="GNQ"&mildata$year==2012] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW est", "GNQ", 2012, yr.minus.1 = 2009, yr.plus.1 = 2014)
mildata$mil.expenditure.sipri[mildata$iso3c=="GNQ"&mildata$year==2013] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW est", "GNQ", 2013, yr.minus.1 = 2009, yr.plus.1 = 2014)

# 2019
mildata <- mil_estimator_no_data_func(
  mildata,"GNQ",
  metric = "mil.expenditure.sipri",
  2019,2019
)

##### cow personnel
# good

##### wmeat personnel
# 1968: COW and WMEAT personnel match 1969-1998, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year==1968&iso3c=="GNQ",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2004-05 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="GNQ"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="GNQ"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="GNQ"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="GNQ"&mildata$year==2018]

# x <- milexp.viewer("GNQ")
# y <- milper.viewer("GNQ")

##### GRC ----------------------------------------------------------------------

##### cow expenditure
# 2013-2019 (non-alt): apply IISS growth estimates based on 2012 COW estimate
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "defense.spending.iiss",
  "GRC", 2012, restricted = c(2013:2019)
)

## remove estimate flag for 2013-2019
mildata <- mildata %>%
  dplyr::mutate(mil.expenditure.cow.est.flag  = dplyr::case_when(
    iso3c == "GRC" & year %in% c(2013:2019) ~ 0,
    .default = mil.expenditure.cow.est.flag
  ))

##### wmeat expenditure
# 1946-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "GRC", 1963, restricted = c(1946:1962)
)

# 2018-2019: apply IISS growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "defense.spending.iiss",
  "GRC", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1948: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "GRC", 1949, restricted = c(1946:1948)
)

##### cow personnel
# good

##### wmeat personnel
# 1945-1962: COW and WMEAT personnel match 1963-1976, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1945:1962)&iso3c=="GRC",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "GRC", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("GRC")
# y <- milper.viewer("GRC")

##### GRD(x) ----------------------------------------------------------------------

##### cow expenditure
# 1978-1981
# 1984-1993

##### wmeat expenditure
# N/A

##### sipri expenditure
# N/A

##### cow personnel
# good

##### wmeat personnel
# N/A

x <- milexp.viewer("GRD")
y <- milper.viewer("GRD")

##### GTM ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1946-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "GTM", 1963, restricted = c(1946:1962)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "GTM", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1954: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "GTM", 1955, restricted = c(1946:1954)
)

##### cow personnel
# good

##### wmeat personnel
# 1946-1962: COW and WMEAT personnel match 1963-1978, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1946:1962)&iso3c=="GTM",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="GTM"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="GTM"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="GTM"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="GTM"&mildata$year==2018]

# x <- milexp.viewer("GTM")
# y <- milper.viewer("GTM")

##### GUY ----------------------------------------------------------------------

##### cow expenditure
# 1992
mildata$mil.expenditure.cow[mildata$iso3c=="GUY"&mildata$year==1992] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "GUY", 1992)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="GUY"&mildata$year==1992] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "GUY", 1992)

# 2005-2008
mildata$mil.expenditure.cow[mildata$iso3c=="GUY"&mildata$year==2005] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "GUY", 2005, yr.minus.1 = 2004, yr.plus.1 = 2009)
mildata$mil.expenditure.cow[mildata$iso3c=="GUY"&mildata$year==2006] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "GUY", 2006, yr.minus.1 = 2004, yr.plus.1 = 2009)
mildata$mil.expenditure.cow[mildata$iso3c=="GUY"&mildata$year==2007] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "GUY", 2007, yr.minus.1 = 2004, yr.plus.1 = 2009)
mildata$mil.expenditure.cow[mildata$iso3c=="GUY"&mildata$year==2008] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "GUY", 2008, yr.minus.1 = 2004, yr.plus.1 = 2009)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="GUY"&mildata$year==2005] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "GUY", 2005, yr.minus.1 = 2004, yr.plus.1 = 2009)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="GUY"&mildata$year==2006] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "GUY", 2006, yr.minus.1 = 2004, yr.plus.1 = 2009)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="GUY"&mildata$year==2007] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "GUY", 2007, yr.minus.1 = 2004, yr.plus.1 = 2009)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="GUY"&mildata$year==2008] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "GUY", 2008, yr.minus.1 = 2004, yr.plus.1 = 2009)

##### wmeat expenditure
# 1983
mildata$mil.expenditure.wmeat[mildata$iso3c=="GUY"&mildata$year==1983] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "GUY", 1983)

# 1986-1987
mildata$mil.expenditure.wmeat[mildata$iso3c=="GUY"&mildata$year==1986] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "GUY", 1986, yr.minus.1 = 1985, yr.plus.1 = 1988)
mildata$mil.expenditure.wmeat[mildata$iso3c=="GUY"&mildata$year==1987] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "GUY", 1987, yr.minus.1 = 1985, yr.plus.1 = 1988)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "GUY", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1966-1972: apply WMEAT growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.wmeat",
  "GUY", 1973, restricted = c(1966:1972)
)

# 1997-1999
mildata$mil.expenditure.sipri[mildata$iso3c=="GUY"&mildata$year==1997] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "GUY", 1997, yr.minus.1 = 1996, yr.plus.1 = 2000)
mildata$mil.expenditure.sipri[mildata$iso3c=="GUY"&mildata$year==1998] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "GUY", 1998, yr.minus.1 = 1996, yr.plus.1 = 2000)
mildata$mil.expenditure.sipri[mildata$iso3c=="GUY"&mildata$year==1999] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "GUY", 1999, yr.minus.1 = 1996, yr.plus.1 = 2000)

##### cow personnel
# good

##### wmeat personnel
# 2018-2019: 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="GUY"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="GUY"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="GUY"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="GUY"&mildata$year==2018]

# x <- milexp.viewer("GUY")
# y <- milper.viewer("GUY")

##### HND ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1951-1962: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "HND", 1963, restricted = c(1951:1962)
)

# 1946-1950: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "HND", 1951, restricted = c(1946:1950)
)

# 1980-1983
mildata$mil.expenditure.wmeat[mildata$iso3c=="HND"&mildata$year==1980] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "HND", 1980, yr.minus.1 = 1979, yr.plus.1 = 1984)
mildata$mil.expenditure.wmeat[mildata$iso3c=="HND"&mildata$year==1981] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "HND", 1981, yr.minus.1 = 1979, yr.plus.1 = 1984)
mildata$mil.expenditure.wmeat[mildata$iso3c=="HND"&mildata$year==1982] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "HND", 1982, yr.minus.1 = 1979, yr.plus.1 = 1984)
mildata$mil.expenditure.wmeat[mildata$iso3c=="HND"&mildata$year==1983] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "HND", 1983, yr.minus.1 = 1979, yr.plus.1 = 1984)

# 2018-2019: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "HND", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1950: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "HND", 1951, restricted = c(1946:1950)
)

# 1992
mildata$mil.expenditure.sipri[mildata$iso3c=="HND"&mildata$year==1992] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "HND", 1992)

# 1994-1999
mildata$mil.expenditure.sipri[mildata$iso3c=="HND"&mildata$year==1994] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "HND", 1994, yr.minus.1 = 1993, yr.plus.1 = 2000)
mildata$mil.expenditure.sipri[mildata$iso3c=="HND"&mildata$year==1995] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "HND", 1995, yr.minus.1 = 1993, yr.plus.1 = 2000)
mildata$mil.expenditure.sipri[mildata$iso3c=="HND"&mildata$year==1996] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "HND", 1996, yr.minus.1 = 1993, yr.plus.1 = 2000)
mildata$mil.expenditure.sipri[mildata$iso3c=="HND"&mildata$year==1997] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "HND", 1997, yr.minus.1 = 1993, yr.plus.1 = 2000)
mildata$mil.expenditure.sipri[mildata$iso3c=="HND"&mildata$year==1998] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "HND", 1998, yr.minus.1 = 1993, yr.plus.1 = 2000)
mildata$mil.expenditure.sipri[mildata$iso3c=="HND"&mildata$year==1999] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "HND", 1999, yr.minus.1 = 1993, yr.plus.1 = 2000)

##### cow personnel
# good

##### wmeat personnel
# 1946-1962: COW and WMEAT personnel match 1963-1977, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1946:1962)&iso3c=="HND",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "HND", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("HND")
# y <- milper.viewer("HND")

##### HRV ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1992-1994: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "HRV", 1995, restricted = c(1992:1994)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "HRV", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# good

##### cow personnel
# good

##### wmeat personnel
# 1993
mildata$mil.personnel.wmeat[mildata$iso3c=="HRV"&mildata$year==1993] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT personnel", col_ref = "COW personnel", "HRV", 1993)

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "HRV", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("HRV")
# y <- milper.viewer("HRV")

##### HTI ----------------------------------------------------------------------

##### cow expenditure
# 2010-2012; 2016-2019: apply IISS estimates to COW
mildata <- mildata %>%
  dplyr::mutate(
    mil.expenditure.cow = ifelse(year %in% c(2010:2012,2016:2019)&iso3c=="HTI",defense.spending.iiss,mil.expenditure.cow),
    mil.expenditure.cow.alt = ifelse(year %in% c(2010:2012,2016:2019)&iso3c=="HTI",defense.spending.iiss,mil.expenditure.cow.alt)
    )

# 2004-2009: apply WMEAT growth rates to COW
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "mil.expenditure.wmeat",
  "HTI", 2003, restricted = c(2004:2009)
)
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow.alt",
  col_ref = "mil.expenditure.wmeat",
  "HTI", 2003, restricted = c(2004:2009)
)

# 2013-2015: apply SIPRI growth rates to COW
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "mil.expenditure.sipri",
  "HTI", 2016, restricted = c(2013:2015)
)
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow.alt",
  col_ref = "mil.expenditure.sipri",
  "HTI", 2016, restricted = c(2013:2015)
)

# 2012: recalculate assuming consistent growth between 2011 and 2013
hti.alpha.1 <- (mildata$mil.expenditure.cow[mildata$iso3c=="HTI"&mildata$year==2013]/mildata$mil.expenditure.cow[mildata$iso3c=="HTI"&mildata$year==2011])^(1/2)

mildata$mil.expenditure.cow[mildata$iso3c=="HTI"&mildata$year==2012] <- mildata$mil.expenditure.cow[mildata$iso3c=="HTI"&mildata$year==2011] * hti.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="HTI"&mildata$year==2012] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="HTI"&mildata$year==2011] * hti.alpha.1

# 1986: assume consistent growth between 1985 and 1987
hti.alpha.2 <- (mildata$mil.expenditure.cow[mildata$iso3c=="HTI"&mildata$year==1987]/mildata$mil.expenditure.cow[mildata$iso3c=="HTI"&mildata$year==1985])^(1/2)

mildata$mil.expenditure.cow[mildata$iso3c=="HTI"&mildata$year==1986] <- mildata$mil.expenditure.cow[mildata$iso3c=="HTI"&mildata$year==1985] * hti.alpha.2
mildata$mil.expenditure.cow.alt[mildata$iso3c=="HTI"&mildata$year==1986] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="HTI"&mildata$year==1985] * hti.alpha.2

# 1993-1994
mildata$mil.expenditure.cow[mildata$iso3c=="HTI"&mildata$year==1993] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "HTI", 1993, yr.minus.1 = 1992, yr.plus.1 = 1995)
mildata$mil.expenditure.cow[mildata$iso3c=="HTI"&mildata$year==1994] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "HTI", 1994, yr.minus.1 = 1992, yr.plus.1 = 1995)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="HTI"&mildata$year==1993] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "HTI", 1993, yr.minus.1 = 1992, yr.plus.1 = 1995)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="HTI"&mildata$year==1994] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "HTI", 1994, yr.minus.1 = 1992, yr.plus.1 = 1995)

##### wmeat expenditure
# 1946-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "HTI", 1963, restricted = c(1946:1962)
)

# 1986: assume consistent growth between 1985 and 1987
hti.alpha.3 <- (mildata$mil.expenditure.wmeat[mildata$iso3c=="HTI"&mildata$year==1987]/mildata$mil.expenditure.wmeat[mildata$iso3c=="HTI"&mildata$year==1985])^(1/2)

mildata$mil.expenditure.wmeat[mildata$iso3c=="HTI"&mildata$year==1986] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="HTI"&mildata$year==1985] * hti.alpha.3

# 1995-1998
mildata$mil.expenditure.wmeat[mildata$iso3c=="HTI"&mildata$year==1995] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW est", "HTI", 1995, yr.minus.1 = 1994, yr.plus.1 = 1999)
mildata$mil.expenditure.wmeat[mildata$iso3c=="HTI"&mildata$year==1996] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW est", "HTI", 1996, yr.minus.1 = 1994, yr.plus.1 = 1999)
mildata$mil.expenditure.wmeat[mildata$iso3c=="HTI"&mildata$year==1997] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW est", "HTI", 1997, yr.minus.1 = 1994, yr.plus.1 = 1999)
mildata$mil.expenditure.wmeat[mildata$iso3c=="HTI"&mildata$year==1998] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW est", "HTI", 1998, yr.minus.1 = 1994, yr.plus.1 = 1999)

# 2010-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "HTI", 2009, restricted = c(2010:2019)
)

##### sipri expenditure
# 1946-1987: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "HTI", 1988, restricted = c(1946:1987)
)

# 1996-2012
mildata$mil.expenditure.sipri[mildata$iso3c=="HTI"&mildata$year==1996] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW est", "HTI", 1996, yr.minus.1 = 1995, yr.plus.1 = 2013)
mildata$mil.expenditure.sipri[mildata$iso3c=="HTI"&mildata$year==1997] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW est", "HTI", 1997, yr.minus.1 = 1995, yr.plus.1 = 2013)
mildata$mil.expenditure.sipri[mildata$iso3c=="HTI"&mildata$year==1998] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW est", "HTI", 1998, yr.minus.1 = 1995, yr.plus.1 = 2013)
mildata$mil.expenditure.sipri[mildata$iso3c=="HTI"&mildata$year==1999] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW est", "HTI", 1999, yr.minus.1 = 1995, yr.plus.1 = 2013)
mildata$mil.expenditure.sipri[mildata$iso3c=="HTI"&mildata$year==2000] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW est", "HTI", 2000, yr.minus.1 = 1995, yr.plus.1 = 2013)
mildata$mil.expenditure.sipri[mildata$iso3c=="HTI"&mildata$year==2001] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW est", "HTI", 2001, yr.minus.1 = 1995, yr.plus.1 = 2013)
mildata$mil.expenditure.sipri[mildata$iso3c=="HTI"&mildata$year==2002] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW est", "HTI", 2002, yr.minus.1 = 1995, yr.plus.1 = 2013)
mildata$mil.expenditure.sipri[mildata$iso3c=="HTI"&mildata$year==2003] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW est", "HTI", 2003, yr.minus.1 = 1995, yr.plus.1 = 2013)
mildata$mil.expenditure.sipri[mildata$iso3c=="HTI"&mildata$year==2004] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW est", "HTI", 2004, yr.minus.1 = 1995, yr.plus.1 = 2013)
mildata$mil.expenditure.sipri[mildata$iso3c=="HTI"&mildata$year==2005] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW est", "HTI", 2005, yr.minus.1 = 1995, yr.plus.1 = 2013)
mildata$mil.expenditure.sipri[mildata$iso3c=="HTI"&mildata$year==2006] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW est", "HTI", 2006, yr.minus.1 = 1995, yr.plus.1 = 2013)
mildata$mil.expenditure.sipri[mildata$iso3c=="HTI"&mildata$year==2007] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW est", "HTI", 2007, yr.minus.1 = 1995, yr.plus.1 = 2013)
mildata$mil.expenditure.sipri[mildata$iso3c=="HTI"&mildata$year==2008] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW est", "HTI", 2008, yr.minus.1 = 1995, yr.plus.1 = 2013)
mildata$mil.expenditure.sipri[mildata$iso3c=="HTI"&mildata$year==2009] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW est", "HTI", 2009, yr.minus.1 = 1995, yr.plus.1 = 2013)
mildata$mil.expenditure.sipri[mildata$iso3c=="HTI"&mildata$year==2010] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW est", "HTI", 2010, yr.minus.1 = 1995, yr.plus.1 = 2013)
mildata$mil.expenditure.sipri[mildata$iso3c=="HTI"&mildata$year==2011] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW est", "HTI", 2011, yr.minus.1 = 1995, yr.plus.1 = 2013)
mildata$mil.expenditure.sipri[mildata$iso3c=="HTI"&mildata$year==2012] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW est", "HTI", 2012, yr.minus.1 = 1995, yr.plus.1 = 2013)

##### cow personnel
# 2000: assume consistent growth
hti.alpha.4 <- (mildata$mil.personnel.cow[mildata$iso3c=="HTI"&mildata$year==2001] - mildata$mil.personnel.cow[mildata$iso3c=="HTI"&mildata$year==1999]) / 2

mildata$mil.personnel.cow[mildata$iso3c=="HTI"&mildata$year==2000] <- mildata$mil.personnel.cow[mildata$iso3c=="HTI"&mildata$year==1999] + hti.alpha.4

# 2002-2004: assume consistent decline
hti.alpha.5 <- (mildata$mil.personnel.cow[mildata$iso3c=="HTI"&mildata$year==2005] - mildata$mil.personnel.cow[mildata$iso3c=="HTI"&mildata$year==2001]) / 4

mildata$mil.personnel.cow[mildata$iso3c=="HTI"&mildata$year==2002] <- mildata$mil.personnel.cow[mildata$iso3c=="HTI"&mildata$year==2001] + hti.alpha.5
mildata$mil.personnel.cow[mildata$iso3c=="HTI"&mildata$year==2003] <- mildata$mil.personnel.cow[mildata$iso3c=="HTI"&mildata$year==2002] + hti.alpha.5
mildata$mil.personnel.cow[mildata$iso3c=="HTI"&mildata$year==2004] <- mildata$mil.personnel.cow[mildata$iso3c=="HTI"&mildata$year==2003] + hti.alpha.5

# 2007: assume consistent growth
hti.alpha.6 <- (mildata$mil.personnel.cow[mildata$iso3c=="HTI"&mildata$year==2008] - mildata$mil.personnel.cow[mildata$iso3c=="HTI"&mildata$year==2006]) / 2

mildata$mil.personnel.cow[mildata$iso3c=="HTI"&mildata$year==2007] <- mildata$mil.personnel.cow[mildata$iso3c=="HTI"&mildata$year==2006] + hti.alpha.6

##### wmeat personnel
# 1945-1962: COW and WMEAT personnel match 1963-1999, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1945:1962)&iso3c=="HTI",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2014-15 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="HTI"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="HTI"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="HTI"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="HTI"&mildata$year==2018]

# x <- milexp.viewer("HTI")
# y <- milper.viewer("HTI")

##### HUN ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1946-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "HUN", 1963, restricted = c(1946:1962)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "HUN", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1967: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "HUN", 1968, restricted = c(1946:1967)
)

##### cow personnel
# good

##### wmeat personnel
# 1946-1962: COW and WMEAT personnel match 1963-1976, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1946:1962)&iso3c=="HUN",mil.personnel.cow,mil.personnel.wmeat))

# 1993: COW and WMEAT personnel match 1984-1992 and 1995-1999, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year==1993&iso3c=="HUN",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "HUN", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("HUN")
# y <- milper.viewer("HUN")

##### IDN ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1949-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "IDN", 1963, restricted = c(1949:1962)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "IDN", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1973: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "IDN", 1974, restricted = c(1949:1973)
)

##### cow personnel
# good

##### wmeat personnel
# 1949-1962: COW and WMEAT personnel match 1963-1982, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1949:1962)&iso3c=="IDN",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2014-15 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="IDN"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="IDN"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="IDN"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="IDN"&mildata$year==2018]

# x <- milexp.viewer("IDN")
# y <- milper.viewer("IDN")

##### IND ----------------------------------------------------------------------

##### cow expenditure (non-alt)
# 2013-2019 (non-alt): apply IISS growth estimates based on 2012 COW estimate
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "defense.spending.iiss",
  "IND", 2012, restricted = c(2013:2019)
)

## remove estimate flag for 2013-2019
mildata <- mildata %>%
  dplyr::mutate(mil.expenditure.cow.est.flag  = dplyr::case_when(
    iso3c == "IND" & year %in% c(2013:2019) ~ 0,
    .default = mil.expenditure.cow.est.flag
  ))

##### wmeat expenditure
# 1947-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "IND", 1963, restricted = c(1947:1962)
)

# 2018-2019: apply IISS growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "defense.spending.iiss",
  "IND", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1947-1955: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "IND", 1956, restricted = c(1947:1955)
)

##### cow personnel
# good

##### wmeat personnel
# 1947-1962: COW and WMEAT personnel match 1963-1964, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1947:1962)&iso3c=="IND",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "IND", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("IND")
# y <- milper.viewer("IND")

##### IRL ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1946-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "IRL", 1963, restricted = c(1946:1963)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "IRL", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1957: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "IRL", 1958, restricted = c(1946:1957)
)

##### cow personnel
# good

##### wmeat personnel
# 1945-1962: COW and WMEAT personnel match 1963-1976, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1945:1962)&iso3c=="IRL",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: COW and WMEAT personnel match 2009-2017, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(2018:2019)&iso3c=="IRL",mil.personnel.cow,mil.personnel.wmeat))

# x <- milexp.viewer("IRL")
# y <- milper.viewer("IRL")

##### IRN ----------------------------------------------------------------------

##### cow expenditure
# 1981: calculate WMEAT 1981 as a percent of 1980 and 1982 values, apply proportions
# to COW estimates, and average the two estimates
mildata$mil.expenditure.cow[mildata$iso3c=="IRN"&mildata$year==1981] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "IRN", 1981)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="IRN"&mildata$year==1981] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "IRN", 1981)

# 2013-2019: apply IISS growth estimates based on 2012 COW estimate
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "defense.spending.iiss",
  "IRN", 2012, restricted = c(2013:2019)
)

## remove estimate flag for 2013-2019
mildata <- mildata %>%
  dplyr::mutate(mil.expenditure.cow.est.flag  = dplyr::case_when(
    iso3c == "IRN" & year %in% c(2013:2019) ~ 0,
    .default = mil.expenditure.cow.est.flag
  ))

##### wmeat expenditure
# 1946-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "IRN", 1963, restricted = c(1946:1962)
)

# 2018-2019: apply IISS growth estimates based on 2017 WMEAT estimate
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "defense.spending.iiss",
  "IRN", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1959: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "IRN", 1960, restricted = c(1946:1959)
)

##### cow personnel
# good

##### wmeat personnel
# 1945-1962: COW and WMEAT personnel match 1963-1980, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1945:1962)&iso3c=="IRN",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2009-10 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="IRN"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="IRN"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="IRN"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="IRN"&mildata$year==2018]

# x <- milexp.viewer("IRN")
# y <- milper.viewer("IRN")

##### IRQ ----------------------------------------------------------------------

##### cow expenditure
# 2002-2004: apply WMEAT growth rates to COW
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "mil.expenditure.wmeat",
  "IRQ", 2001, restricted = c(2002:2004)
)
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow.alt",
  col_ref = "mil.expenditure.wmeat",
  "IRQ", 2001, restricted = c(2002:2004)
)

# 2005-2008
mildata$mil.expenditure.cow[mildata$iso3c=="IRQ"&mildata$year==2005] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "IRQ", 2005, yr.minus.1 = 2004, yr.plus.1 = 2009)
mildata$mil.expenditure.cow[mildata$iso3c=="IRQ"&mildata$year==2006] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "IRQ", 2006, yr.minus.1 = 2004, yr.plus.1 = 2009)
mildata$mil.expenditure.cow[mildata$iso3c=="IRQ"&mildata$year==2007] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "IRQ", 2007, yr.minus.1 = 2004, yr.plus.1 = 2009)
mildata$mil.expenditure.cow[mildata$iso3c=="IRQ"&mildata$year==2008] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "IRQ", 2008, yr.minus.1 = 2004, yr.plus.1 = 2009)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="IRQ"&mildata$year==2005] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "IRQ", 2005, yr.minus.1 = 2004, yr.plus.1 = 2009)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="IRQ"&mildata$year==2006] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "IRQ", 2006, yr.minus.1 = 2004, yr.plus.1 = 2009)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="IRQ"&mildata$year==2007] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "IRQ", 2007, yr.minus.1 = 2004, yr.plus.1 = 2009)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="IRQ"&mildata$year==2008] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "IRQ", 2008, yr.minus.1 = 2004, yr.plus.1 = 2009)

##### wmeat expenditure
# 1946-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "IRQ", 1963, restricted = c(1946:1962)
)

# 1992-1994
mildata$mil.expenditure.wmeat[mildata$iso3c=="IRQ"&mildata$year==1992] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "IRQ", 1992, yr.minus.1 = 1991, yr.plus.1 = 1995)
mildata$mil.expenditure.wmeat[mildata$iso3c=="IRQ"&mildata$year==1993] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "IRQ", 1993, yr.minus.1 = 1991, yr.plus.1 = 1995)
mildata$mil.expenditure.wmeat[mildata$iso3c=="IRQ"&mildata$year==1994] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "IRQ", 1994, yr.minus.1 = 1991, yr.plus.1 = 1995)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "IRQ", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1956: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "IRQ", 1957, restricted = c(1946:1956)
)

# 1982-2001
mildata$mil.expenditure.sipri[mildata$iso3c=="IRQ"&mildata$year==1982] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "IRQ", 1982, yr.minus.1 = 1980, yr.plus.1 = 2009)
mildata$mil.expenditure.sipri[mildata$iso3c=="IRQ"&mildata$year==1983] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "IRQ", 1983, yr.minus.1 = 1980, yr.plus.1 = 2009)
mildata$mil.expenditure.sipri[mildata$iso3c=="IRQ"&mildata$year==1984] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "IRQ", 1984, yr.minus.1 = 1980, yr.plus.1 = 2009)
mildata$mil.expenditure.sipri[mildata$iso3c=="IRQ"&mildata$year==1985] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "IRQ", 1985, yr.minus.1 = 1980, yr.plus.1 = 2009)
mildata$mil.expenditure.sipri[mildata$iso3c=="IRQ"&mildata$year==1986] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "IRQ", 1986, yr.minus.1 = 1980, yr.plus.1 = 2009)
mildata$mil.expenditure.sipri[mildata$iso3c=="IRQ"&mildata$year==1987] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "IRQ", 1987, yr.minus.1 = 1980, yr.plus.1 = 2009)
mildata$mil.expenditure.sipri[mildata$iso3c=="IRQ"&mildata$year==1988] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "IRQ", 1988, yr.minus.1 = 1980, yr.plus.1 = 2009)
mildata$mil.expenditure.sipri[mildata$iso3c=="IRQ"&mildata$year==1989] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "IRQ", 1989, yr.minus.1 = 1980, yr.plus.1 = 2009)
mildata$mil.expenditure.sipri[mildata$iso3c=="IRQ"&mildata$year==1990] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "IRQ", 1990, yr.minus.1 = 1980, yr.plus.1 = 2009)
mildata$mil.expenditure.sipri[mildata$iso3c=="IRQ"&mildata$year==1991] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "IRQ", 1991, yr.minus.1 = 1980, yr.plus.1 = 2009)
mildata$mil.expenditure.sipri[mildata$iso3c=="IRQ"&mildata$year==1992] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "IRQ", 1992, yr.minus.1 = 1980, yr.plus.1 = 2009)
mildata$mil.expenditure.sipri[mildata$iso3c=="IRQ"&mildata$year==1993] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "IRQ", 1993, yr.minus.1 = 1980, yr.plus.1 = 2009)
mildata$mil.expenditure.sipri[mildata$iso3c=="IRQ"&mildata$year==1994] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "IRQ", 1994, yr.minus.1 = 1980, yr.plus.1 = 2009)
mildata$mil.expenditure.sipri[mildata$iso3c=="IRQ"&mildata$year==1995] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "IRQ", 1995, yr.minus.1 = 1980, yr.plus.1 = 2009)
mildata$mil.expenditure.sipri[mildata$iso3c=="IRQ"&mildata$year==1996] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "IRQ", 1996, yr.minus.1 = 1980, yr.plus.1 = 2009)
mildata$mil.expenditure.sipri[mildata$iso3c=="IRQ"&mildata$year==1997] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "IRQ", 1997, yr.minus.1 = 1980, yr.plus.1 = 2009)
mildata$mil.expenditure.sipri[mildata$iso3c=="IRQ"&mildata$year==1998] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "IRQ", 1998, yr.minus.1 = 1980, yr.plus.1 = 2009)
mildata$mil.expenditure.sipri[mildata$iso3c=="IRQ"&mildata$year==1999] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "IRQ", 1999, yr.minus.1 = 1980, yr.plus.1 = 2009)
mildata$mil.expenditure.sipri[mildata$iso3c=="IRQ"&mildata$year==2000] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "IRQ", 2000, yr.minus.1 = 1980, yr.plus.1 = 2009)
mildata$mil.expenditure.sipri[mildata$iso3c=="IRQ"&mildata$year==2001] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "IRQ", 2001, yr.minus.1 = 1980, yr.plus.1 = 2009)

# 2002-2003
mildata$mil.expenditure.sipri[mildata$iso3c=="IRQ"&mildata$year==2002] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "IRQ", 2002, yr.minus.1 = 2001, yr.plus.1 = 2004)
mildata$mil.expenditure.sipri[mildata$iso3c=="IRQ"&mildata$year==2003] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "IRQ", 2003, yr.minus.1 = 2001, yr.plus.1 = 2004)

##### cow personnel
# good

##### wmeat personnel
# 1946-1962: COW and WMEAT personnel match 1963-1977, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1946:1962)&iso3c=="IRQ",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="IRQ"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="IRQ"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="IRQ"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="IRQ"&mildata$year==2018]

# x <- milexp.viewer("IRQ")
# y <- milper.viewer("IRQ")

##### ISL(x) ----------------------------------------------------------------------

##### cow expenditure
# 2002-2004
# 2007-2019

##### wmeat expenditure
# 1946-1972
# 2018-2019

##### sipri expenditure
# 1946-1948

##### cow personnel
# 2000-2001: COW and WMEAT personnel match 1973-2017, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.cow = ifelse(year %in% c(2000:2001)&iso3c=="ISL",mil.personnel.wmeat,mil.personnel.cow))

##### wmeat personnel
# 1945-1972: COW and WMEAT personnel match 1973-2017, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1945:1972)&iso3c=="ISL",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: COW and WMEAT personnel match 1973-2017, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(2018:2019)&iso3c=="ISL",mil.personnel.cow,mil.personnel.wmeat))

x <- milexp.viewer("ISL")
# y <- milper.viewer("ISL")

##### ISR ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1948-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "ISR", 1963, restricted = c(1948:1962)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "ISR", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1948-1950: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "ISR", 1951, restricted = c(1948:1950)
)

##### cow personnel
# good

##### wmeat personnel
# 1948-1962: COW and WMEAT personnel match 1963-1973, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1948:1962)&iso3c=="ISR",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2008-09 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="ISR"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="ISR"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="ISR"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="ISR"&mildata$year==2018]

# x <- milexp.viewer("ISR")
# y <- milper.viewer("ISR")

##### ITA ----------------------------------------------------------------------

##### cow expenditure
# 2013-2019 (non-alt): apply IISS growth estimates based on 2012 COW estimate
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "defense.spending.iiss",
  "ITA", 2012, restricted = c(2013:2019)
)

## remove estimate flag for 2013-2019
mildata <- mildata %>%
  dplyr::mutate(mil.expenditure.cow.est.flag  = dplyr::case_when(
    iso3c == "ITA" & year %in% c(2013:2019) ~ 0,
    .default = mil.expenditure.cow.est.flag
  ))

##### wmeat expenditure
# 1946-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "ITA", 1963, restricted = c(1946:1962)
)

# 2018-2019: apply IISS growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "defense.spending.iiss",
  "ITA", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1948: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "ITA", 1949, restricted = c(1946:1948)
)

##### cow personnel
# good

##### wmeat personnel
# 1945-1962: COW and WMEAT personnel match 1963-1965, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1945:1962)&iso3c=="ITA",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "ITA", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("ITA")
# y <- milper.viewer("ITA")

##### JAM ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "JAM", 1963, restricted = 1962
)

# 1986
mildata$mil.expenditure.wmeat[mildata$iso3c=="JAM"&mildata$year==1986] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "JAM", 1986)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "JAM", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1962-1975: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "JAM", 1976, restricted = c(1962:1975)
)

# 1978-1980
mildata$mil.expenditure.sipri[mildata$iso3c=="JAM"&mildata$year==1978] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "JAM", 1978, yr.minus.1 = 1977, yr.plus.1 = 1981)
mildata$mil.expenditure.sipri[mildata$iso3c=="JAM"&mildata$year==1979] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "JAM", 1979, yr.minus.1 = 1977, yr.plus.1 = 1981)
mildata$mil.expenditure.sipri[mildata$iso3c=="JAM"&mildata$year==1980] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "JAM", 1980, yr.minus.1 = 1977, yr.plus.1 = 1981)

##### cow personnel
# good

##### wmeat personnel
# 1962: COW and WMEAT personnel match 1963-1977, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year==1962&iso3c=="JAM",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: COW and WMEAT personnel match 2004-2017, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(2018:2019)&iso3c=="JAM",mil.personnel.cow,mil.personnel.wmeat))

# x <- milexp.viewer("JAM")
# y <- milper.viewer("JAM")

##### JOR ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1946-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "JOR", 1963, restricted = c(1946:1962)
)

# 1988-1989
mildata$mil.expenditure.wmeat[mildata$iso3c=="JOR"&mildata$year==1988] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "JOR", 1988, yr.minus.1 = 1987, yr.plus.1 = 1990)
mildata$mil.expenditure.wmeat[mildata$iso3c=="JOR"&mildata$year==1989] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "JOR", 1989, yr.minus.1 = 1987, yr.plus.1 = 1990)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "JOR", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1956: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "JOR", 1957, restricted = c(1946:1956)
)

##### cow personnel
# 1960: assume consistent growth between 1959 and 1961
jor.alpha.1 <- (mildata$mil.personnel.cow[mildata$iso3c=="JOR"&mildata$year==1961]/mildata$mil.personnel.cow[mildata$iso3c=="JOR"&mildata$year==1959])^(1/3)

mildata$mil.personnel.cow[mildata$iso3c=="JOR"&mildata$year==1960] <- mildata$mil.personnel.cow[mildata$iso3c=="JOR"&mildata$year==1959] * jor.alpha.1

##### wmeat personnel
# 1946-1959; 1961-1962: COW and WMEAT personnel match 1963-1980, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1946:1962)&iso3c=="JOR",mil.personnel.cow,mil.personnel.wmeat))

# 1960: assume consistent growth between 1959 and 1961
jor.alpha.2 <- (mildata$mil.personnel.wmeat[mildata$iso3c=="JOR"&mildata$year==1961]/mildata$mil.personnel.wmeat[mildata$iso3c=="JOR"&mildata$year==1959])^(1/3)

mildata$mil.personnel.wmeat[mildata$iso3c=="JOR"&mildata$year==1960] <- mildata$mil.personnel.wmeat[mildata$iso3c=="JOR"&mildata$year==1959] * jor.alpha.2

# 2018-2019: 2013-14 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="JOR"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="JOR"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="JOR"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="JOR"&mildata$year==2018]

# x <- milexp.viewer("JOR")
# y <- milper.viewer("JOR")

##### JPN ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1952-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "JPN", 1963, restricted = c(1952:1962)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "JPN", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# good

##### cow personnel
# good

##### wmeat personnel
# 1952-1962: COW and WMEAT personnel match 1963-1965, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1945:1962)&iso3c=="JPN",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2013-14 - 2017-18 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="JPN"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="JPN"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="JPN"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="JPN"&mildata$year==2018]

x <- milexp.viewer("JPN")
y <- milper.viewer("JPN")

##### KAZ ----------------------------------------------------------------------

##### cow expenditure
# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"KAZ",
  metric = "mil.expenditure.cow",
  1991,1991
)
mildata <- mil_estimator_no_data_func(
  mildata,"KAZ",
  metric = "mil.expenditure.cow.alt",
  1991,1991
)

##### wmeat expenditure
# 1992-1993: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "KAZ", 1994, restricted = c(1992:1993)
)

# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"KAZ",
  metric = "mil.expenditure.wmeat",
  1991,1991
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "KAZ", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1992-1993: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "KAZ", 1994, restricted = c(1992:1993)
)

# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"KAZ",
  metric = "mil.expenditure.sipri",
  1991,1991
)

##### cow personnel
# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"KAZ",
  metric = "mil.personnel.cow",
  1991,1991
)

##### wmeat personnel
# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"KAZ",
  metric = "mil.personnel.wmeat",
  1991,1991
)

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "KAZ", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("KAZ")
# y <- milper.viewer("KAZ")

##### KEN ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1963: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "KEN", 1964, restricted = 1963
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "KEN", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# good

##### cow personnel
# good

##### wmeat personnel
# 1963: COW and WMEAT personnel match 1964-1995, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year==1963&iso3c=="KEN",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2001-02 - 2017-18 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="KEN"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="KEN"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="KEN"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="KEN"&mildata$year==2018]

# x <- milexp.viewer("KEN")
# y <- milper.viewer("KEN")

##### KGZ ----------------------------------------------------------------------

##### cow expenditure
# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"KGZ",
  metric = "mil.expenditure.cow",
  1991,1991
)
mildata <- mil_estimator_no_data_func(
  mildata,"KGZ",
  metric = "mil.expenditure.cow.alt",
  1991,1991
)

# 2013-2015 (non-alt): apply IISS growth rates to COW
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "defense.spending.iiss",
  "KGZ", 2012, restricted = c(2013:2015)
)

## remove estimate flag for 2013-2015
mildata <- mildata %>%
  dplyr::mutate(mil.expenditure.cow.est.flag  = dplyr::case_when(
    iso3c == "KGZ" & year %in% c(2013:2015) ~ 0,
    .default = mil.expenditure.cow.est.flag
  ))

# 2016-2019: apply SIPRI growth rates to COW
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "mil.expenditure.sipri",
  "KGZ", 2015, restricted = c(2016:2019)
)
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow.alt",
  col_ref = "mil.expenditure.sipri",
  "KGZ", 2015, restricted = c(2016:2019)
)

##### wmeat expenditure
# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"KGZ",
  metric = "mil.expenditure.wmeat",
  1991,1991
)

# 2018-2019: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "KGZ", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"KGZ",
  metric = "mil.expenditure.sipri",
  1991,1991
)

# 1992-1993: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "KGZ", 1994, restricted = c(1992:1993)
)

##### cow personnel
# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"KGZ",
  metric = "mil.personnel.cow",
  1991,1991
)

##### wmeat personnel
# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"KGZ",
  metric = "mil.personnel.wmeat",
  1991,1991
)

# 2018-2019: 2008-09 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="KGZ"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="KGZ"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="KGZ"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="KGZ"&mildata$year==2018]

# x <- milexp.viewer("KGZ")
# y <- milper.viewer("KGZ")

##### KHM ----------------------------------------------------------------------

##### cow expenditure
# 1990: apply SIPRI growth rates to COW
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "mil.expenditure.sipri",
  "KHM", 1991, restricted = 1990
)
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow.alt",
  col_ref = "mil.expenditure.sipri",
  "KHM", 1991, restricted = 1990
)

# 1976-1989: assume consistent growth between 1975 and 1990
khm.alpha.1 <- (mildata$mil.expenditure.cow[mildata$iso3c=="KHM"&mildata$year==1990]/mildata$mil.expenditure.cow[mildata$iso3c=="KHM"&mildata$year==1975])^(1/15)

for(y in 1976:1989){
  mildata$mil.expenditure.cow[mildata$iso3c=="KHM"&mildata$year==y] <- mildata$mil.expenditure.cow[mildata$iso3c=="KHM"&mildata$year==(y-1)] * khm.alpha.1
  mildata$mil.expenditure.cow.alt[mildata$iso3c=="KHM"&mildata$year==y] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="KHM"&mildata$year==(y-1)] * khm.alpha.1
}

##### wmeat expenditure
# 1953-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "KHM", 1963, restricted = c(1953:1962)
)

# 1990: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "KHM", 1991, restricted = 1990
)

# 1976-1989: assume consistent growth between 1975 and 1990
khm.alpha.2 <- (mildata$mil.expenditure.wmeat[mildata$iso3c=="KHM"&mildata$year==1990]/mildata$mil.expenditure.wmeat[mildata$iso3c=="KHM"&mildata$year==1975])^(1/15)

for(y in 1976:1989){
  mildata$mil.expenditure.wmeat[mildata$iso3c=="KHM"&mildata$year==y] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="KHM"&mildata$year==(y-1)] * khm.alpha.2
}

# 2018-2019: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "KHM", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1953-1975: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "KHM", 1990, restricted = c(1953:1975)
)

# 1976-1989: assume consistent growth between 1975 and 1990
khm.alpha.3 <- (mildata$mil.expenditure.sipri[mildata$iso3c=="KHM"&mildata$year==1990]/mildata$mil.expenditure.sipri[mildata$iso3c=="KHM"&mildata$year==1975])^(1/15)

for(y in 1976:1989){
  mildata$mil.expenditure.sipri[mildata$iso3c=="KHM"&mildata$year==y] <- mildata$mil.expenditure.sipri[mildata$iso3c=="KHM"&mildata$year==(y-1)] * khm.alpha.3
}

##### cow personnel
# good

##### wmeat personnel
# 1953-1962: COW and WMEAT personnel match 1963-1978, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1953:1962)&iso3c=="KHM",mil.personnel.cow,mil.personnel.wmeat))

# 1979: COW and WMEAT personnel match 1963-1994, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year==1979&iso3c=="KHM",mil.personnel.cow,mil.personnel.wmeat))

# 1995-1996: COW 1994 and 1995 values match, 1996 and 1997 values match; apply pattern to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="KHM"&mildata$year==1995] <- mildata$mil.personnel.wmeat[mildata$iso3c=="KHM"&mildata$year==1994]
mildata$mil.personnel.wmeat[mildata$iso3c=="KHM"&mildata$year==1996] <- mildata$mil.personnel.wmeat[mildata$iso3c=="KHM"&mildata$year==1997]

# 2018-2019: 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="KHM"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="KHM"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="KHM"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="KHM"&mildata$year==2018]

# x <- milexp.viewer("KHM")
# y <- milper.viewer("KHM")

##### KIR(x) ----------------------------------------------------------------------

##### cow expenditure
# 2013-2019

##### wmeat expenditure
# N/A

##### sipri expenditure
# N/A

##### cow personnel
# 2013-2019

##### wmeat personnel
# N/A

x <- milexp.viewer("KIR")
y <- milper.viewer("KIR")

##### KNA(x) ----------------------------------------------------------------------

##### cow expenditure
# 1988-2001
# 2013-2019

##### wmeat expenditure
# N/A

##### sipri expenditure
# N/A

##### cow personnel
# 2013-2019

##### wmeat personnel
# N/A

x <- milexp.viewer("KNA")
y <- milper.viewer("KNA")

##### KOR ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1949-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "KOR", 1963, restricted = c(1949:1962)
)

# 2018-2019: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "KOR", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1949: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "KOR", 1950, restricted = 1949
)

# 1951
mildata$mil.expenditure.sipri[mildata$iso3c=="KOR"&mildata$year==1951] <- mil_expenditure_gap_estimator_func(
df = mildata, col_missing = "SIPRI", col_ref = "COW", "KOR", 1951)

##### cow personnel
# good

##### wmeat personnel
# 1949-1962: COW and WMEAT personnel match 1963-1995, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1949:1962)&iso3c=="KOR",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "KOR", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("KOR")
# y <- milper.viewer("KOR")

##### KSV(ce,we,se,cp,wp) ----------------------------------------------------------------------

##### cow expenditure
# appears to not be counting the de facto government of Kosovo's expenditure
# 2013-2019

##### wmeat expenditure
# 2018-2019

##### sipri expenditure
# good

##### cow personnel
# appears to not be counting the de facto government of Kosovo's expenditure
# 2013-2019

##### wmeat personnel
# 2018-2019

x <- milexp.viewer("KSV")
y <- milper.viewer("KSV")

##### KWT ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1961-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "KWT", 1963, restricted = c(1961:1962)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "KWT", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1961-1969: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "KWT", 1970, restricted = c(1961:1969)
)

# 1975-1976
mildata$mil.expenditure.sipri[mildata$iso3c=="KWT"&mildata$year==1975] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "KWT", 1975, yr.minus.1 = 1974, yr.plus.1 = 1977)
mildata$mil.expenditure.sipri[mildata$iso3c=="KWT"&mildata$year==1976] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "KWT", 1976, yr.minus.1 = 1974, yr.plus.1 = 1977)

##### cow personnel
# good

##### wmeat personnel
# 1961-1962: COW and WMEAT personnel match 1963-1995, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1961:1962)&iso3c=="KWT",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2004-05 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="KWT"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="KWT"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="KWT"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="KWT"&mildata$year==2018]

# x <- milexp.viewer("KWT")
# y <- milper.viewer("KWT")

##### LAO ----------------------------------------------------------------------

##### cow expenditure
# 1953
mildata <- mil_estimator_no_data_func(
  mildata,"LAO",
  metric = "mil.expenditure.cow",
  1953,1953
)
mildata <- mil_estimator_no_data_func(
  mildata,"LAO",
  metric = "mil.expenditure.cow.alt",
  1953,1953
)

# 1982-1983: assume consistent growth between 1981 and 1984
lao.alpha.1 <- (mildata$mil.expenditure.cow[mildata$iso3c=="LAO"&mildata$year==1984]/mildata$mil.expenditure.cow[mildata$iso3c=="LAO"&mildata$year==1981])^(1/3)

mildata$mil.expenditure.cow[mildata$iso3c=="LAO"&mildata$year==1982] <- mildata$mil.expenditure.cow[mildata$iso3c=="LAO"&mildata$year==1981] * lao.alpha.1
mildata$mil.expenditure.cow[mildata$iso3c=="LAO"&mildata$year==1983] <- mildata$mil.expenditure.cow[mildata$iso3c=="LAO"&mildata$year==1982] * lao.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="LAO"&mildata$year==1982] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="LAO"&mildata$year==1981] * lao.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="LAO"&mildata$year==1983] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="LAO"&mildata$year==1982] * lao.alpha.1

# 1986-1988: assume consistent growth between 1985 and 1989
lao.alpha.2 <- (mildata$mil.expenditure.cow[mildata$iso3c=="LAO"&mildata$year==1989]/mildata$mil.expenditure.cow[mildata$iso3c=="LAO"&mildata$year==1985])^(1/4)

mildata$mil.expenditure.cow[mildata$iso3c=="LAO"&mildata$year==1986] <- mildata$mil.expenditure.cow[mildata$iso3c=="LAO"&mildata$year==1985] * lao.alpha.2
mildata$mil.expenditure.cow[mildata$iso3c=="LAO"&mildata$year==1987] <- mildata$mil.expenditure.cow[mildata$iso3c=="LAO"&mildata$year==1986] * lao.alpha.2
mildata$mil.expenditure.cow[mildata$iso3c=="LAO"&mildata$year==1988] <- mildata$mil.expenditure.cow[mildata$iso3c=="LAO"&mildata$year==1987] * lao.alpha.2
mildata$mil.expenditure.cow.alt[mildata$iso3c=="LAO"&mildata$year==1986] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="LAO"&mildata$year==1985] * lao.alpha.2
mildata$mil.expenditure.cow.alt[mildata$iso3c=="LAO"&mildata$year==1987] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="LAO"&mildata$year==1986] * lao.alpha.2
mildata$mil.expenditure.cow.alt[mildata$iso3c=="LAO"&mildata$year==1988] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="LAO"&mildata$year==1987] * lao.alpha.2

# 1990-1991 assume consistent growth between 1989 and 1992
lao.alpha.3 <- (mildata$mil.expenditure.cow[mildata$iso3c=="LAO"&mildata$year==1992]/mildata$mil.expenditure.cow[mildata$iso3c=="LAO"&mildata$year==1989])^(1/3)

mildata$mil.expenditure.cow[mildata$iso3c=="LAO"&mildata$year==1990] <- mildata$mil.expenditure.cow[mildata$iso3c=="LAO"&mildata$year==1989] * lao.alpha.3
mildata$mil.expenditure.cow[mildata$iso3c=="LAO"&mildata$year==1991] <- mildata$mil.expenditure.cow[mildata$iso3c=="LAO"&mildata$year==1990] * lao.alpha.3
mildata$mil.expenditure.cow.alt[mildata$iso3c=="LAO"&mildata$year==1990] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="LAO"&mildata$year==1989] * lao.alpha.3
mildata$mil.expenditure.cow.alt[mildata$iso3c=="LAO"&mildata$year==1991] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="LAO"&mildata$year==1990] * lao.alpha.3

# 2004
mildata$mil.expenditure.cow[mildata$iso3c=="LAO"&mildata$year==2004] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "LAO", 2004)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="LAO"&mildata$year==2004] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "LAO", 2004)

# 2010 (non-alt)
mildata$mil.expenditure.cow[mildata$iso3c=="LAO"&mildata$year==2010] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "IISS", "LAO", 2010)

# 2013-2014 (non-alt): apply IISS growth rates to COW
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "defense.spending.iiss",
  "LAO", 2012, restricted = c(2013:2014)
)

## remove estimate flag for 2013-2014
mildata <- mildata %>%
  dplyr::mutate(mil.expenditure.cow.est.flag  = dplyr::case_when(
    iso3c == "LAO" & year %in% c(2013:2014) ~ 0,
    .default = mil.expenditure.cow.est.flag
  ))

# 2015-2017: apply WMEAT growth estimates based on 2014 COW estimate
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "mil.expenditure.wmeat",
  "LAO", 2014, restricted = c(2015:2017)
)
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow.alt",
  col_ref = "mil.expenditure.wmeat",
  "LAO", 2014, restricted = c(2015:2017)
)

# 2018-2019
mildata <- mil_estimator_no_data_func(
  mildata,"LAO",
  metric = "mil.expenditure.cow",
  2018,2019
)
mildata <- mil_estimator_no_data_func(
  mildata,"LAO",
  metric = "mil.expenditure.cow.alt",
  2018,2019
)

##### wmeat expenditure
# 1954-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "LAO", 1963, restricted = c(1954:1962)
)

# 1953
mildata <- mil_estimator_no_data_func(
  mildata,"LAO",
  metric = "mil.expenditure.wmeat",
  1953,1953
)

# 1974-1978
mildata$mil.expenditure.wmeat[mildata$iso3c=="LAO"&mildata$year==1974] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "LAO", 1974, yr.minus.1 = 1973, yr.plus.1 = 1979)
mildata$mil.expenditure.wmeat[mildata$iso3c=="LAO"&mildata$year==1975] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "LAO", 1975, yr.minus.1 = 1973, yr.plus.1 = 1979)
mildata$mil.expenditure.wmeat[mildata$iso3c=="LAO"&mildata$year==1976] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "LAO", 1976, yr.minus.1 = 1973, yr.plus.1 = 1979)
mildata$mil.expenditure.wmeat[mildata$iso3c=="LAO"&mildata$year==1977] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "LAO", 1977, yr.minus.1 = 1973, yr.plus.1 = 1979)
mildata$mil.expenditure.wmeat[mildata$iso3c=="LAO"&mildata$year==1978] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "LAO", 1978, yr.minus.1 = 1973, yr.plus.1 = 1979)

# 1980-1981: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "LAO", 1979, restricted = c(1980:1981)
)

# 1989: average estimates from COW 1985 and 1992
lao.cow.1985 <- mildata$mil.expenditure.cow[mildata$iso3c=="LAO"&mildata$year==1985]
lao.cow.1989 <- mildata$mil.expenditure.cow[mildata$iso3c=="LAO"&mildata$year==1989]
lao.cow.1992 <- mildata$mil.expenditure.cow[mildata$iso3c=="LAO"&mildata$year==1992]

lao.wmeat.1985 <- mildata$mil.expenditure.wmeat[mildata$iso3c=="LAO"&mildata$year==1985]
lao.wmeat.1992 <- mildata$mil.expenditure.wmeat[mildata$iso3c=="LAO"&mildata$year==1992]

lao.wmeat.1989.est.1 <- lao.wmeat.1985 * lao.cow.1989 / lao.cow.1985
lao.wmeat.1989.est.2 <- lao.wmeat.1992 * lao.cow.1989 / lao.cow.1992

mildata$mil.expenditure.wmeat[mildata$iso3c=="LAO"&mildata$year==1989] <- mean(c(lao.wmeat.1989.est.1,lao.wmeat.1989.est.2))

# 1982-1983: assume consistent growth between 1981 and 1984
lao.alpha.4 <- (mildata$mil.expenditure.wmeat[mildata$iso3c=="LAO"&mildata$year==1984]/mildata$mil.expenditure.wmeat[mildata$iso3c=="LAO"&mildata$year==1981])^(1/3)

mildata$mil.expenditure.wmeat[mildata$iso3c=="LAO"&mildata$year==1982] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="LAO"&mildata$year==1981] * lao.alpha.4
mildata$mil.expenditure.wmeat[mildata$iso3c=="LAO"&mildata$year==1983] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="LAO"&mildata$year==1982] * lao.alpha.4

# 1986-1988: assume consistent growth between 1985 and 1989
lao.alpha.5 <- (mildata$mil.expenditure.wmeat[mildata$iso3c=="LAO"&mildata$year==1989]/mildata$mil.expenditure.wmeat[mildata$iso3c=="LAO"&mildata$year==1985])^(1/4)

mildata$mil.expenditure.wmeat[mildata$iso3c=="LAO"&mildata$year==1986] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="LAO"&mildata$year==1985] * lao.alpha.5
mildata$mil.expenditure.wmeat[mildata$iso3c=="LAO"&mildata$year==1987] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="LAO"&mildata$year==1986] * lao.alpha.5
mildata$mil.expenditure.wmeat[mildata$iso3c=="LAO"&mildata$year==1988] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="LAO"&mildata$year==1987] * lao.alpha.5

# 1990-1991 assume consistent growth between 1989 and 1992
lao.alpha.6 <- (mildata$mil.expenditure.wmeat[mildata$iso3c=="LAO"&mildata$year==1992]/mildata$mil.expenditure.wmeat[mildata$iso3c=="LAO"&mildata$year==1989])^(1/3)

mildata$mil.expenditure.wmeat[mildata$iso3c=="LAO"&mildata$year==1990] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="LAO"&mildata$year==1989] * lao.alpha.6
mildata$mil.expenditure.wmeat[mildata$iso3c=="LAO"&mildata$year==1991] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="LAO"&mildata$year==1990] * lao.alpha.6

# 2018-2019
mildata <- mil_estimator_no_data_func(
  mildata,"LAO",
  metric = "mil.expenditure.wmeat",
  2018,2019
)

##### sipri expenditure
# 1954-1966: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "LAO", 1967, restricted = c(1954:1966)
)

# 1953
mildata <- mil_estimator_no_data_func(
  mildata,"LAO",
  metric = "mil.expenditure.sipri",
  1953,1953
)

# 1974-1981: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "LAO", 1973, restricted = c(1974:1981)
)

# 1984-1985: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "LAO", 1981, restricted = c(1984:1985)
)

# 1989: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "LAO", 1992, restricted = 1989
)

# 1982-1983: assume consistent growth between 1981 and 1984
lao.alpha.7 <- (mildata$mil.expenditure.sipri[mildata$iso3c=="LAO"&mildata$year==1984]/mildata$mil.expenditure.sipri[mildata$iso3c=="LAO"&mildata$year==1981])^(1/3)

mildata$mil.expenditure.sipri[mildata$iso3c=="LAO"&mildata$year==1982] <- mildata$mil.expenditure.sipri[mildata$iso3c=="LAO"&mildata$year==1981] * lao.alpha.7
mildata$mil.expenditure.sipri[mildata$iso3c=="LAO"&mildata$year==1983] <- mildata$mil.expenditure.sipri[mildata$iso3c=="LAO"&mildata$year==1982] * lao.alpha.7

# 1986-1988: assume consistent growth between 1985 and 1989
lao.alpha.8 <- (mildata$mil.expenditure.sipri[mildata$iso3c=="LAO"&mildata$year==1989]/mildata$mil.expenditure.sipri[mildata$iso3c=="LAO"&mildata$year==1985])^(1/4)

mildata$mil.expenditure.sipri[mildata$iso3c=="LAO"&mildata$year==1986] <- mildata$mil.expenditure.sipri[mildata$iso3c=="LAO"&mildata$year==1985] * lao.alpha.8
mildata$mil.expenditure.sipri[mildata$iso3c=="LAO"&mildata$year==1987] <- mildata$mil.expenditure.sipri[mildata$iso3c=="LAO"&mildata$year==1986] * lao.alpha.8
mildata$mil.expenditure.sipri[mildata$iso3c=="LAO"&mildata$year==1988] <- mildata$mil.expenditure.sipri[mildata$iso3c=="LAO"&mildata$year==1987] * lao.alpha.8

# 1990-1991 assume consistent growth between 1989 and 1992
lao.alpha.9 <- (mildata$mil.expenditure.sipri[mildata$iso3c=="LAO"&mildata$year==1992]/mildata$mil.expenditure.sipri[mildata$iso3c=="LAO"&mildata$year==1989])^(1/3)

mildata$mil.expenditure.sipri[mildata$iso3c=="LAO"&mildata$year==1990] <- mildata$mil.expenditure.sipri[mildata$iso3c=="LAO"&mildata$year==1989] * lao.alpha.9
mildata$mil.expenditure.sipri[mildata$iso3c=="LAO"&mildata$year==1991] <- mildata$mil.expenditure.sipri[mildata$iso3c=="LAO"&mildata$year==1990] * lao.alpha.9

# 2014-2017: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "LAO", 2013, restricted = c(2014:2017)
)

# 2018-2019
mildata <- mil_estimator_no_data_func(
  mildata,"LAO",
  metric = "mil.expenditure.sipri",
  2018,2019
)

##### cow personnel
# good

##### wmeat personnel
# 1953-1962: COW and WMEAT personnel match 1963-1982, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1953:1962)&iso3c=="LAO",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2002-03 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="LAO"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="LAO"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="LAO"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="LAO"&mildata$year==2018]

# x <- milexp.viewer("LAO")
# y <- milper.viewer("LAO")

##### LBN ----------------------------------------------------------------------

##### cow expenditure
# 1987
mildata$mil.expenditure.cow[mildata$iso3c=="LBN"&mildata$year==1987] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "LBN", 1987)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="LBN"&mildata$year==1987] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "LBN", 1987)

# 1989: assume consistent growth between 1988 and 1990
lbn.alpha.1 <- (mildata$mil.expenditure.cow[mildata$iso3c=="LBN"&mildata$year==1990]/mildata$mil.expenditure.cow[mildata$iso3c=="LBN"&mildata$year==1988])^(1/2)

mildata$mil.expenditure.cow[mildata$iso3c=="LBN"&mildata$year==1989] <- mildata$mil.expenditure.cow[mildata$iso3c=="LBN"&mildata$year==1988] * lbn.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="LBN"&mildata$year==1989] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="LBN"&mildata$year==1988] * lbn.alpha.1

# 2013-2019 (non-alt): apply IISS growth rates to COW
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "defense.spending.iiss",
  "LBN", 2012, restricted = c(2013:2019)
)

## remove estimate flag for 2013-2019
mildata <- mildata %>%
  dplyr::mutate(mil.expenditure.cow.est.flag  = dplyr::case_when(
    iso3c == "LBN" & year %in% c(2013:2019) ~ 0,
    .default = mil.expenditure.cow.est.flag
  ))

##### wmeat expenditure
# 1946-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "LBN", 1963, restricted = c(1946:1962)
)

# 1985-1986: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "LBN", 1984, restricted = c(1985:1986)
)

# 1987-1988: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "LBN", 1986, restricted = c(1987:1988)
)

# 1989: assume consistent growth between 1988 and 1990
lbn.alpha.2 <- (mildata$mil.expenditure.wmeat[mildata$iso3c=="LBN"&mildata$year==1990]/mildata$mil.expenditure.wmeat[mildata$iso3c=="LBN"&mildata$year==1988])^(1/2)

mildata$mil.expenditure.wmeat[mildata$iso3c=="LBN"&mildata$year==1989] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="LBN"&mildata$year==1988] * lbn.alpha.2

# 2018-2019: apply IISS growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "defense.spending.iiss",
  "LBN", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1954: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "LBN", 1955, restricted = c(1946:1954)
)

# 1989: assume consistent growth between 1988 and 1990
lbn.alpha.3 <- (mildata$mil.expenditure.sipri[mildata$iso3c=="LBN"&mildata$year==1990]/mildata$mil.expenditure.sipri[mildata$iso3c=="LBN"&mildata$year==1988])^(1/2)

mildata$mil.expenditure.sipri[mildata$iso3c=="LBN"&mildata$year==1989] <- mildata$mil.expenditure.sipri[mildata$iso3c=="LBN"&mildata$year==1988] * lbn.alpha.3

##### cow personnel
# good

##### wmeat personnel
# 1946-1962: COW and WMEAT personnel match 1963-1973, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1946:1962)&iso3c=="LBN",mil.personnel.cow,mil.personnel.wmeat))

# 1976: COW's 1976 value matches its 1975 value, so apply same logic to WMEAT estimate
mildata$mil.personnel.wmeat[mildata$iso3c=="LBN"&mildata$year==1976] <- mildata$mil.personnel.wmeat[mildata$iso3c=="LBN"&mildata$year==1975]

# 2018-2019: COW and WMEAT personnel match 2013-2017, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(2018:2019)&iso3c=="LBN",mil.personnel.cow,mil.personnel.wmeat))

# x <- milexp.viewer("LBN")
# y <- milper.viewer("LBN")

##### LBR ----------------------------------------------------------------------

##### cow expenditure
# 1946-1956
mildata <- mil_estimator_no_data_func(
  mildata,"LBR",
  metric = "mil.expenditure.cow",
  1956,1946
)
mildata <- mil_estimator_no_data_func(
  mildata,"LBR",
  metric = "mil.expenditure.cow.alt",
  1956,1946
)

# 1992
mildata$mil.expenditure.cow[mildata$iso3c=="LBR"&mildata$year==1992] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "LBR", 1992)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="LBR"&mildata$year==1992] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "LBR", 1992)

# 2004-2008
mildata$mil.expenditure.cow[mildata$iso3c=="LBR"&mildata$year==2004] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "LBR", 2004, yr.minus.1 = 2003, yr.plus.1 = 2009)
mildata$mil.expenditure.cow[mildata$iso3c=="LBR"&mildata$year==2005] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "LBR", 2005, yr.minus.1 = 2003, yr.plus.1 = 2009)
mildata$mil.expenditure.cow[mildata$iso3c=="LBR"&mildata$year==2006] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "LBR", 2006, yr.minus.1 = 2003, yr.plus.1 = 2009)
mildata$mil.expenditure.cow[mildata$iso3c=="LBR"&mildata$year==2007] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "LBR", 2007, yr.minus.1 = 2003, yr.plus.1 = 2009)
mildata$mil.expenditure.cow[mildata$iso3c=="LBR"&mildata$year==2008] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "LBR", 2008, yr.minus.1 = 2003, yr.plus.1 = 2009)

mildata$mil.expenditure.cow.alt[mildata$iso3c=="LBR"&mildata$year==2004] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "LBR", 2004, yr.minus.1 = 2003, yr.plus.1 = 2009)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="LBR"&mildata$year==2005] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "LBR", 2005, yr.minus.1 = 2003, yr.plus.1 = 2009)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="LBR"&mildata$year==2006] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "LBR", 2006, yr.minus.1 = 2003, yr.plus.1 = 2009)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="LBR"&mildata$year==2007] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "LBR", 2007, yr.minus.1 = 2003, yr.plus.1 = 2009)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="LBR"&mildata$year==2008] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "LBR", 2008, yr.minus.1 = 2003, yr.plus.1 = 2009)

# 2012-2019 (non-alt): apply IISS growth rates to COW
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "defense.spending.iiss",
  "LBR", 2011, restricted = c(2012:2019)
)

## remove estimate flag for 2012-2019
mildata <- mildata %>%
  dplyr::mutate(mil.expenditure.cow.est.flag  = dplyr::case_when(
    iso3c == "LBR" & year %in% c(2012:2019) ~ 0,
    .default = mil.expenditure.cow.est.flag
  ))

##### wmeat expenditure
# 1957-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "LBR", 1963, restricted = c(1957:1962)
)

# 1946-1956
mildata <- mil_estimator_no_data_func(
  mildata,"LBR",
  metric = "mil.expenditure.wmeat",
  1956,1946
)

# 1989-1992
mildata$mil.expenditure.wmeat[mildata$iso3c=="LBR"&mildata$year==1989] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW est", "LBR", 1989, yr.minus.1 = 1988, yr.plus.1 = 1993)
mildata$mil.expenditure.wmeat[mildata$iso3c=="LBR"&mildata$year==1990] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW est", "LBR", 1990, yr.minus.1 = 1988, yr.plus.1 = 1993)
mildata$mil.expenditure.wmeat[mildata$iso3c=="LBR"&mildata$year==1991] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW est", "LBR", 1991, yr.minus.1 = 1988, yr.plus.1 = 1993)
mildata$mil.expenditure.wmeat[mildata$iso3c=="LBR"&mildata$year==1992] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW est", "LBR", 1992, yr.minus.1 = 1988, yr.plus.1 = 1993)

# 1995-1996
mildata$mil.expenditure.wmeat[mildata$iso3c=="LBR"&mildata$year==1995] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "LBR", 1995, yr.minus.1 = 1994, yr.plus.1 = 1997)
mildata$mil.expenditure.wmeat[mildata$iso3c=="LBR"&mildata$year==1996] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "LBR", 1996, yr.minus.1 = 1994, yr.plus.1 = 1997)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "LBR", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1956
mildata <- mil_estimator_no_data_func(
  mildata,"LBR",
  metric = "mil.expenditure.sipri",
  1956,1946
)

# 1957-1964: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "LBR", 1965, restricted = c(1957:1964)
)

# 1989-1990
mildata$mil.expenditure.sipri[mildata$iso3c=="LBR"&mildata$year==1989] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "LBR", 1989, yr.minus.1 = 1988, yr.plus.1 = 1991)
mildata$mil.expenditure.sipri[mildata$iso3c=="LBR"&mildata$year==1990] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "LBR", 1990, yr.minus.1 = 1988, yr.plus.1 = 1991)

# 1995-2003
mildata$mil.expenditure.sipri[mildata$iso3c=="LBR"&mildata$year==1995] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "LBR", 1995, yr.minus.1 = 1994, yr.plus.1 = 2009)
mildata$mil.expenditure.sipri[mildata$iso3c=="LBR"&mildata$year==1996] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "LBR", 1996, yr.minus.1 = 1994, yr.plus.1 = 2009)
mildata$mil.expenditure.sipri[mildata$iso3c=="LBR"&mildata$year==1997] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "LBR", 1997, yr.minus.1 = 1994, yr.plus.1 = 2009)
mildata$mil.expenditure.sipri[mildata$iso3c=="LBR"&mildata$year==1998] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "LBR", 1998, yr.minus.1 = 1994, yr.plus.1 = 2009)
mildata$mil.expenditure.sipri[mildata$iso3c=="LBR"&mildata$year==1999] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "LBR", 1999, yr.minus.1 = 1994, yr.plus.1 = 2009)
mildata$mil.expenditure.sipri[mildata$iso3c=="LBR"&mildata$year==2000] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "LBR", 2000, yr.minus.1 = 1994, yr.plus.1 = 2009)
mildata$mil.expenditure.sipri[mildata$iso3c=="LBR"&mildata$year==2001] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "LBR", 2001, yr.minus.1 = 1994, yr.plus.1 = 2009)
mildata$mil.expenditure.sipri[mildata$iso3c=="LBR"&mildata$year==2002] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "LBR", 2002, yr.minus.1 = 1994, yr.plus.1 = 2009)
mildata$mil.expenditure.sipri[mildata$iso3c=="LBR"&mildata$year==2003] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "LBR", 2003, yr.minus.1 = 1994, yr.plus.1 = 2009)

##### cow personnel
# 1994-1998: assume consistent growth between 1993 and 1999
lbr.alpha.1 <- (mildata$mil.personnel.cow[mildata$iso3c=="LBR"&mildata$year==1999]/mildata$mil.personnel.cow[mildata$iso3c=="LBR"&mildata$year==1993])^(1/6)

mildata$mil.personnel.cow[mildata$iso3c=="LBR"&mildata$year==1994] <- mildata$mil.personnel.cow[mildata$iso3c=="LBR"&mildata$year==1993] * lbr.alpha.1
mildata$mil.personnel.cow[mildata$iso3c=="LBR"&mildata$year==1995] <- mildata$mil.personnel.cow[mildata$iso3c=="LBR"&mildata$year==1994] * lbr.alpha.1
mildata$mil.personnel.cow[mildata$iso3c=="LBR"&mildata$year==1996] <- mildata$mil.personnel.cow[mildata$iso3c=="LBR"&mildata$year==1995] * lbr.alpha.1
mildata$mil.personnel.cow[mildata$iso3c=="LBR"&mildata$year==1997] <- mildata$mil.personnel.cow[mildata$iso3c=="LBR"&mildata$year==1996] * lbr.alpha.1
mildata$mil.personnel.cow[mildata$iso3c=="LBR"&mildata$year==1998] <- mildata$mil.personnel.cow[mildata$iso3c=="LBR"&mildata$year==1997] * lbr.alpha.1

##### wmeat personnel
# 1945-1962: COW and WMEAT personnel match 1963-1992, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1945:1962)&iso3c=="LBR",mil.personnel.cow,mil.personnel.wmeat))

# 1993: COW and WMEAT personnel match 1963-1992, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year==1993&iso3c=="LBR",mil.personnel.cow,mil.personnel.wmeat))

# 2000-2001: 2002-03 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="LBR"&mildata$year==2001] <- mildata$mil.personnel.wmeat[mildata$iso3c=="LBR"&mildata$year==2002]
mildata$mil.personnel.wmeat[mildata$iso3c=="LBR"&mildata$year==2000] <- mildata$mil.personnel.wmeat[mildata$iso3c=="LBR"&mildata$year==2001]

# 1999: apply COW personnel growth rates to WMEAT personnel
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "LBR", 2000, restricted = 1999
)

# 1994-1998: assume consistent growth between 1993 and 1999
lbr.alpha.2 <- (mildata$mil.personnel.wmeat[mildata$iso3c=="LBR"&mildata$year==1999]/mildata$mil.personnel.wmeat[mildata$iso3c=="LBR"&mildata$year==1993])^(1/6)

mildata$mil.personnel.wmeat[mildata$iso3c=="LBR"&mildata$year==1994] <- mildata$mil.personnel.wmeat[mildata$iso3c=="LBR"&mildata$year==1993] * lbr.alpha.2
mildata$mil.personnel.wmeat[mildata$iso3c=="LBR"&mildata$year==1995] <- mildata$mil.personnel.wmeat[mildata$iso3c=="LBR"&mildata$year==1994] * lbr.alpha.2
mildata$mil.personnel.wmeat[mildata$iso3c=="LBR"&mildata$year==1996] <- mildata$mil.personnel.wmeat[mildata$iso3c=="LBR"&mildata$year==1995] * lbr.alpha.2
mildata$mil.personnel.wmeat[mildata$iso3c=="LBR"&mildata$year==1997] <- mildata$mil.personnel.wmeat[mildata$iso3c=="LBR"&mildata$year==1996] * lbr.alpha.2
mildata$mil.personnel.wmeat[mildata$iso3c=="LBR"&mildata$year==1998] <- mildata$mil.personnel.wmeat[mildata$iso3c=="LBR"&mildata$year==1997] * lbr.alpha.2

# 2018-2019: COW and WMEAT personnel match 2007-2017, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(2018:2019)&iso3c=="LBR",mil.personnel.cow,mil.personnel.wmeat))

# x <- milexp.viewer("LBR")
# y <- milper.viewer("LBR")

##### LBY ----------------------------------------------------------------------

##### cow expenditure
# 1951-1958
mildata <- mil_estimator_no_data_func(
  mildata,"LBY",
  metric = "mil.expenditure.cow",
  1958,1951
)
mildata <- mil_estimator_no_data_func(
  mildata,"LBY",
  metric = "mil.expenditure.cow.alt",
  1958,1951
)

# 1985: assume consistent growth between 1984 and 1986
lby.alpha.1 <- (mildata$mil.expenditure.cow[mildata$iso3c=="LBY"&mildata$year==1986]/mildata$mil.expenditure.cow[mildata$iso3c=="LBY"&mildata$year==1984])^(1/2)

mildata$mil.expenditure.cow[mildata$iso3c=="LBY"&mildata$year==1985] <- mildata$mil.expenditure.cow[mildata$iso3c=="LBY"&mildata$year==1984] * lby.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="LBY"&mildata$year==1985] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="LBY"&mildata$year==1984] * lby.alpha.1

# 1993: estimate COW 1993 as equidistant point based on WMEAT 1992-1994 values
mildata$mil.expenditure.cow[mildata$iso3c=="LBY"&mildata$year==1993] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "LBY", 1993)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="LBY"&mildata$year==1993] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "LBY", 1993)

# 2011
mildata$mil.expenditure.cow[mildata$iso3c=="LBY"&mildata$year==2011] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "LBY", 2011)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="LBY"&mildata$year==2011] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "LBY", 2011)

# 2013 (non-alt): apply IISS growth rates to COW
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "defense.spending.iiss",
  "LBY", 2012, restricted = 2013
)

## remove estimate flag for 2013
mildata <- mildata %>%
  dplyr::mutate(mil.expenditure.cow.est.flag  = dplyr::case_when(
    iso3c == "LBY" & year == 2013 ~ 0,
    .default = mil.expenditure.cow.est.flag
  ))

# 2014-2017: apply WMEAT growth estimates based on 2013 COW estimate
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "mil.expenditure.wmeat",
  "LBY", 2013, restricted = c(2014:2017)
)
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow.alt",
  col_ref = "mil.expenditure.wmeat",
  "LBY", 2013, restricted = c(2014:2017)
)

# 2018-2019
mildata <- mil_estimator_no_data_func(
  mildata,"LBY",
  metric = "mil.expenditure.cow",
  2018,2019
)
mildata <- mil_estimator_no_data_func(
  mildata,"LBY",
  metric = "mil.expenditure.cow.alt",
  2018,2019
)

##### wmeat expenditure
# 1951-1958
mildata <- mil_estimator_no_data_func(
  mildata,"LBY",
  metric = "mil.expenditure.wmeat",
  1958,1951
)

# 1959-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "LBY", 1963, restricted = c(1959:1962)
)

# 1981
mildata$mil.expenditure.wmeat[mildata$iso3c=="LBY"&mildata$year==1981] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "LBY", 1981)

# 1986: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "LBY", 1987, restricted = 1986
)

# 1985: assume consistent growth between 1984 and 1986
lby.alpha.1 <- (mildata$mil.expenditure.wmeat[mildata$iso3c=="LBY"&mildata$year==1986]/mildata$mil.expenditure.wmeat[mildata$iso3c=="LBY"&mildata$year==1984])^(1/2)

mildata$mil.expenditure.wmeat[mildata$iso3c=="LBY"&mildata$year==1985] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="LBY"&mildata$year==1984] * lby.alpha.1

# 1988
mildata$mil.expenditure.wmeat[mildata$iso3c=="LBY"&mildata$year==1988] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "LBY", 1988)

# 1990
mildata$mil.expenditure.wmeat[mildata$iso3c=="LBY"&mildata$year==1990] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "LBY", 1990)

# 2018-2019
mildata <- mil_estimator_no_data_func(
  mildata,"LBY",
  metric = "mil.expenditure.wmeat",
  2018,2019
)

##### sipri expenditure
# 1951-1958
mildata <- mil_estimator_no_data_func(
  mildata,"LBY",
  metric = "mil.expenditure.sipri",
  1958,1951
)

# 1983-1984: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "LBY", 1982, restricted = c(1983:1984)
)

# 1985: assume consistent growth between 1984 and 1986
lby.alpha.1 <- (mildata$mil.expenditure.sipri[mildata$iso3c=="LBY"&mildata$year==1986]/mildata$mil.expenditure.sipri[mildata$iso3c=="LBY"&mildata$year==1984])^(1/2)

mildata$mil.expenditure.sipri[mildata$iso3c=="LBY"&mildata$year==1985] <- mildata$mil.expenditure.sipri[mildata$iso3c=="LBY"&mildata$year==1984] * lby.alpha.1

# 1986-1996: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "LBY", 1997, restricted = c(1986:1996)
)

# 2009-2011
mildata$mil.expenditure.sipri[mildata$iso3c=="LBY"&mildata$year==2009] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "LBY", 2009, yr.minus.1 = 2008, yr.plus.1 = 2012)
mildata$mil.expenditure.sipri[mildata$iso3c=="LBY"&mildata$year==2010] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "LBY", 2010, yr.minus.1 = 2008, yr.plus.1 = 2012)
mildata$mil.expenditure.sipri[mildata$iso3c=="LBY"&mildata$year==2011] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "LBY", 2011, yr.minus.1 = 2008, yr.plus.1 = 2012)

# 2015-2017: apply WMEAT growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.wmeat",
  "LBY", 2014, restricted = c(2015:2017)
)

# 2018-2019
mildata <- mil_estimator_no_data_func(
  mildata,"LBY",
  metric = "mil.expenditure.sipri",
  2018,2019
)

##### cow personnel
# 2012-2017
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.cow",
  col_ref = "mil.personnel.wmeat",
  "LBY", 2011, restricted = c(2012:2017)
)

# 2018-2019
mildata <- mil_estimator_no_data_func(
  mildata,"LBY",
  metric = "mil.personnel.cow",
  2018,2019
)

##### wmeat personnel
# 1951-1962: COW and WMEAT personnel match 1963-1994, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1951:1962)&iso3c=="LBY",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019
mildata <- mil_estimator_no_data_func(
  mildata,"LBY",
  metric = "mil.personnel.wmeat",
  2018,2019
)

# x <- milexp.viewer("LBY")
# y <- milper.viewer("LBY")

##### LCA(x) ----------------------------------------------------------------------

##### cow expenditure
# 2013-2019

##### wmeat expenditure
# N/A

##### sipri expenditure
# N/A

##### cow personnel
# 2013-2019

##### wmeat personnel
# N/A

x <- milexp.viewer("LCA")
y <- milper.viewer("LCA")

##### LIE(x) ----------------------------------------------------------------------

##### cow expenditure
# 1946-1989
# 2013-2019

##### wmeat expenditure
# N/A

##### sipri expenditure
# N/A

##### cow personnel
# 1946-1989
# 2013-2019

##### wmeat personnel
# N/A

x <- milexp.viewer("LIE")
y <- milper.viewer("LIE")

##### LKA ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1948-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "LKA", 1963, restricted = c(1948:1962)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "LKA", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1948-1950: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "LKA", 1951, restricted = c(1948:1950)
)

##### cow personnel
# good

##### wmeat personnel
# 1948-1962: COW and WMEAT personnel match 1963-1982, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1948:1962)&iso3c=="LKA",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "LKA", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("LKA")
# y <- milper.viewer("LKA")

##### LSO ----------------------------------------------------------------------

##### cow expenditure
# 1976-1977: reestimate based on SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "mil.expenditure.sipri",
  "LSO", 1978, restricted = c(1976:1977)
)
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow.alt",
  col_ref = "mil.expenditure.sipri",
  "LSO", 1978, restricted = c(1976:1977)
)

# 1966-1975
mildata <- mil_estimator_no_data_func(
  mildata,"LSO",
  metric = "mil.expenditure.cow",
  1975,1966
)
mildata <- mil_estimator_no_data_func(
  mildata,"LSO",
  metric = "mil.expenditure.cow.alt",
  1975,1966
)

# 1980: reestimate based on SIPRI
mildata$mil.expenditure.cow[mildata$iso3c=="LSO"&mildata$year==1980] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "LSO", 1980)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="LSO"&mildata$year==1980] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "LSO", 1980)

##### wmeat expenditure
# 1976-1977: reestimate based on SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "LSO", 1978, restricted = c(1976:1977)
)

# 1966-1972
mildata <- mil_estimator_no_data_func(
  mildata,"LSO",
  metric = "mil.expenditure.wmeat",
  1975,1966
)

# 1980
mildata$mil.expenditure.wmeat[mildata$iso3c=="LSO"&mildata$year==1980] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "LSO", 1980)

# 1982
mildata$mil.expenditure.wmeat[mildata$iso3c=="LSO"&mildata$year==1982] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "LSO", 1982)

# 1985
mildata$mil.expenditure.wmeat[mildata$iso3c=="LSO"&mildata$year==1985] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "LSO", 1985)

# 1987-1988
mildata$mil.expenditure.wmeat[mildata$iso3c=="LSO"&mildata$year==1987] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "LSO", 1987, yr.minus.1 = 1986, yr.plus.1 = 1989)
mildata$mil.expenditure.wmeat[mildata$iso3c=="LSO"&mildata$year==1988] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "LSO", 1988, yr.minus.1 = 1986, yr.plus.1 = 1989)

# 1991
mildata$mil.expenditure.wmeat[mildata$iso3c=="LSO"&mildata$year==1991] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "LSO", 1991)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "LSO", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1966-1975
mildata <- mil_estimator_no_data_func(
  mildata,"LSO",
  metric = "mil.expenditure.sipri",
  1975,1966
)

##### cow personnel
# good

##### wmeat personnel
# 1966-1972: COW and WMEAT personnel match 1973-1976, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1966:1972)&iso3c=="LSO",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: COW and WMEAT personnel match 2003-1017, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(2018:2019)&iso3c=="LSO",mil.personnel.cow,mil.personnel.wmeat))

# x <- milexp.viewer("LSO")
# y <- milper.viewer("LSO")

##### LTU ----------------------------------------------------------------------

##### cow expenditure
# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"LTU",
  metric = "mil.expenditure.cow",
  1991,1991
)
mildata <- mil_estimator_no_data_func(
  mildata,"LTU",
  metric = "mil.expenditure.cow.alt",
  1991,1991
)

##### wmeat expenditure
# 1992: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "LTU", 1993, restricted = 1992
)

# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"LTU",
  metric = "mil.expenditure.wmeat",
  1991,1991
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "LTU", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1992: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "LTU", 1993, restricted = 1992
)

# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"LTU",
  metric = "mil.expenditure.sipri",
  1991,1991
)

##### cow personnel
# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"LTU",
  metric = "mil.personnel.cow",
  1991,1991
)

##### wmeat personnel
# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"LTU",
  metric = "mil.personnel.wmeat",
  1991,1991
)

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "LTU", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("LTU")
# y <- milper.viewer("LTU")

##### LUX ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1946-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "LUX", 1963, restricted = c(1946:1962)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "LUX", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1948: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "LUX", 1949, restricted = c(1946:1948)
)

##### cow personnel
# good

##### wmeat personnel
# 1946-1962: COW and WMEAT personnel match 1963-1999, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1946:1962)&iso3c=="LUX",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: COW and WMEAT personnel match 2005-1017, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(2018:2019)&iso3c=="LUX",mil.personnel.cow,mil.personnel.wmeat))

# x <- milexp.viewer("LUX")
# y <- milper.viewer("LUX")

##### LVA ----------------------------------------------------------------------

##### cow expenditure
# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"LVA",
  metric = "mil.expenditure.cow",
  1991,1991
)
mildata <- mil_estimator_no_data_func(
  mildata,"LVA",
  metric = "mil.expenditure.cow.alt",
  1991,1991
)

# 2013-2019 (non-alt): apply IISS growth rates to COW
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "defense.spending.iiss",
  "LVA", 2012, restricted = c(2013:2019)
)

## remove estimate flag for 2013-2019
mildata <- mildata %>%
  dplyr::mutate(mil.expenditure.cow.est.flag  = dplyr::case_when(
    iso3c == "LVA" & year %in% c(2013:2019) ~ 0,
    .default = mil.expenditure.cow.est.flag
  ))

##### wmeat expenditure
# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"LVA",
  metric = "mil.expenditure.wmeat",
  1991,1991
)

# 2018-2019: apply IISS growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "defense.spending.iiss",
  "LVA", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1992: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "LVA", 1993, restricted = 1992
)

# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"LVA",
  metric = "mil.expenditure.sipri",
  1991,1991
)

##### cow personnel
# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"LVA",
  metric = "mil.personnel.cow",
  1991,1991
)

##### wmeat personnel
# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"LVA",
  metric = "mil.personnel.wmeat",
  1991,1991
)

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "LVA", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("LVA")
# y <- milper.viewer("LVA")

##### MAR ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1956-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "MAR", 1963, restricted = c(1956:1962)
)

# 1984-1985
mildata$mil.expenditure.wmeat[mildata$iso3c=="MAR"&mildata$year==1984] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "MAR", 1984, yr.minus.1 = 1983, yr.plus.1 = 1986)
mildata$mil.expenditure.wmeat[mildata$iso3c=="MAR"&mildata$year==1985] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "MAR", 1985, yr.minus.1 = 1983, yr.plus.1 = 1986)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "MAR", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# good

##### cow personnel
# good

##### wmeat personnel
# 1956-1962: COW and WMEAT personnel match 1963-1999, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1956:1962)&iso3c=="MAR",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2008-09 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="MAR"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="MAR"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="MAR"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="MAR"&mildata$year==2018]

x <- milexp.viewer("MAR")
# y <- milper.viewer("MAR")

##### MCO(x) ----------------------------------------------------------------------

##### cow expenditure
# 1946-1993
# 2013-2019

##### wmeat expenditure
# N/A

##### sipri expenditure
# N/A

##### cow personnel
# 1946-1993
# 2013-2019

##### wmeat personnel
# N/A

x <- milexp.viewer("MCO")
y <- milper.viewer("MCO")

##### MDA ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1991-1993: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "MDA", 1994, restricted = c(1991:1993)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "MDA", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1991-1994: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "MDA", 1995, restricted = c(1991:1994)
)

##### cow personnel
# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"MDA",
  metric = "mil.personnel.cow",
  1991,1991
)

##### wmeat personnel
# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"MDA",
  metric = "mil.personnel.wmeat",
  1991,1991
)

# 2018-2019: 2011-12 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="MDA"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="MDA"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="MDA"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="MDA"&mildata$year==2018]

# x <- milexp.viewer("MDA")
# y <- milper.viewer("MDA")

##### MDG ----------------------------------------------------------------------

# TODO: Check 2002-04 COW expenditure values

##### cow expenditure
# 1999-2001
mildata$mil.expenditure.cow[mildata$iso3c=="MDG"&mildata$year==1999] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "MDG", 1999, yr.minus.1 = 1998, yr.plus.1 = 2002)
mildata$mil.expenditure.cow[mildata$iso3c=="MDG"&mildata$year==2000] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "MDG", 2000, yr.minus.1 = 1998, yr.plus.1 = 2002)
mildata$mil.expenditure.cow[mildata$iso3c=="MDG"&mildata$year==2001] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "MDG", 2001, yr.minus.1 = 1998, yr.plus.1 = 2002)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="MDG"&mildata$year==1999] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "MDG", 1999, yr.minus.1 = 1998, yr.plus.1 = 2002)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="MDG"&mildata$year==2000] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "MDG", 2000, yr.minus.1 = 1998, yr.plus.1 = 2002)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="MDG"&mildata$year==2001] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "MDG", 2001, yr.minus.1 = 1998, yr.plus.1 = 2002)

##### wmeat expenditure
# 1960-1962: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "MDG", 1963, restricted = c(1960:1962)
)

# 1982
mildata$mil.expenditure.wmeat[mildata$iso3c=="MDG"&mildata$year==1982] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "MDG", 1982)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "MDG", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1982-1983
mildata$mil.expenditure.sipri[mildata$iso3c=="MDG"&mildata$year==1982] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "MDG", 1982, yr.minus.1 = 1981, yr.plus.1 = 1984)
mildata$mil.expenditure.sipri[mildata$iso3c=="MDG"&mildata$year==1983] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "MDG", 1983, yr.minus.1 = 1981, yr.plus.1 = 1984)

##### cow personnel
# 2000-2001
mildata$mil.personnel.cow[mildata$iso3c=="MDG"&mildata$year==2000] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW personnel", col_ref = "WMEAT personnel", "MDG", 2000, yr.minus.1 = 1999, yr.plus.1 = 2002)
mildata$mil.personnel.cow[mildata$iso3c=="MDG"&mildata$year==2001] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW personnel", col_ref = "WMEAT personnel", "MDG", 2001, yr.minus.1 = 1999, yr.plus.1 = 2002)

##### wmeat personnel
# 1960-1962: COW and WMEAT personnel match 1963-1999, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1960:1962)&iso3c=="MDG",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2012-13 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="MDG"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="MDG"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="MDG"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="MDG"&mildata$year==2018]

# x <- milexp.viewer("MDG")
# y <- milper.viewer("MDG")

##### MDV(x) ----------------------------------------------------------------------

##### cow expenditure
# 1965-1983
# 1985
# 1994-1997
# 2008-2019

##### wmeat expenditure
# N/A

##### sipri expenditure
# N/A

##### cow personnel
# 2002-2007
# 2009
# 2011-2019

##### wmeat personnel
# N/A

x <- milexp.viewer("MDV")
y <- milper.viewer("MDV")

##### MEX ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1946-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "MEX", 1963, restricted = c(1946:1962)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "MEX", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1948: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "MEX", 1949, restricted = c(1946:1948)
)

##### cow personnel
# good

##### wmeat personnel
# 1946-1962: COW and WMEAT personnel match 1963-1980, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1946:1962)&iso3c=="MEX",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "MEX", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("MEX")
# y <- milper.viewer("MEX")

##### MHL(x) ----------------------------------------------------------------------

##### cow expenditure
# 2013-2019

##### wmeat expenditure
# N/A

##### sipri expenditure
# N/A

##### cow personnel
# 2013-2019

##### wmeat personnel
# N/A

x <- milexp.viewer("MHL")
y <- milper.viewer("MHL")

##### MKD ----------------------------------------------------------------------

##### cow expenditure
# 1992
mildata <- mil_estimator_no_data_func(
  mildata,"MKD",
  metric = "mil.expenditure.cow",
  1992,1992
)
mildata <- mil_estimator_no_data_func(
  mildata,"MKD",
  metric = "mil.expenditure.cow.alt",
  1992,1992
)

##### wmeat expenditure
# 1993-1994: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "MKD", 1995, restricted = c(1993:1994)
)

# 1992
mildata <- mil_estimator_no_data_func(
  mildata,"MKD",
  metric = "mil.expenditure.wmeat",
  1992,1992
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "MKD", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1993-1995: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "MKD", 1996, restricted = c(1993:1995)
)

# 1992
mildata <- mil_estimator_no_data_func(
  mildata,"MKD",
  metric = "mil.expenditure.sipri",
  1992,1992
)

##### cow personnel
# 1992: COW and WMEAT personnel match 1993-1994, so use WMEAT personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.cow = ifelse(year==1992&iso3c=="MKD",mil.personnel.wmeat,mil.personnel.cow))

##### wmeat personnel
# 2018-2019: 2010-11 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="MKD"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="MKD"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="MKD"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="MKD"&mildata$year==2018]

# x <- milexp.viewer("MKD")
# y <- milper.viewer("MKD")

##### MLI ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1960-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "MLI", 1963, restricted = c(1960:1962)
)

# 1990-1991
mildata$mil.expenditure.wmeat[mildata$iso3c=="MLI"&mildata$year==1990] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "MLI", 1990, yr.minus.1 = 1989, yr.plus.1 = 1992)
mildata$mil.expenditure.wmeat[mildata$iso3c=="MLI"&mildata$year==1991] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "MLI", 1991, yr.minus.1 = 1989, yr.plus.1 = 1992)

# 2018-2019: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "MLI", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1960: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "MLI", 1961, restricted = 1960
)

# 1991-1992
mildata$mil.expenditure.sipri[mildata$iso3c=="MLI"&mildata$year==1991] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "MLI", 1991, yr.minus.1 = 1990, yr.plus.1 = 1993)
mildata$mil.expenditure.sipri[mildata$iso3c=="MLI"&mildata$year==1992] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "MLI", 1992, yr.minus.1 = 1990, yr.plus.1 = 1993)

##### cow personnel
# good

##### wmeat personnel
# 1960-1962: COW and WMEAT personnel match 1963-1981, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1960:1962)&iso3c=="MLI",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "MLI", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("MLI")
# y <- milper.viewer("MLI")

##### MLT ----------------------------------------------------------------------

##### cow expenditure
# 1970: apply WMEAT growth rates to COW
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "mil.expenditure.wmeat",
  "MLT", 1971, restricted = 1970
)
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow.alt",
  col_ref = "mil.expenditure.wmeat",
  "MLT", 1971, restricted = 1970
)

# 1964-1969: reestimate
mildata <- mil_estimator_no_data_func(
  mildata,"MLT",
  metric = "mil.expenditure.cow",
  1969,1964
)
mildata <- mil_estimator_no_data_func(
  mildata,"MLT",
  metric = "mil.expenditure.cow.alt",
  1969,1964
)

##### wmeat expenditure
# 1972
mildata$mil.expenditure.wmeat[mildata$iso3c=="MLT"&mildata$year==1972] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "MLT", 1972)

# 1979
mildata$mil.expenditure.wmeat[mildata$iso3c=="MLT"&mildata$year==1979] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "MLT", 1979)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "MLT", 2017, restricted = c(2018:2019)
)

# 1964-1969: reestimate
mildata <- mil_estimator_no_data_func(
  mildata,"MLT",
  metric = "mil.expenditure.wmeat",
  1969,1964
)

##### sipri expenditure
# 1971-1984: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "MLT", 1985, restricted = c(1971:1984)
)

# 1970: apply WMEAT growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.wmeat",
  "MLT", 1971, restricted = 1970
)

# 1964-1969: reestimate
mildata <- mil_estimator_no_data_func(
  mildata,"MLT",
  metric = "mil.expenditure.sipri",
  1969,1964
)

##### cow personnel
# good

##### wmeat personnel
# 1964: COW and WMEAT personnel match 1965-1993, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year==1964&iso3c=="MLT",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: COW and WMEAT personnel match 1995-2017, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(2018:2019)&iso3c=="MLT",mil.personnel.cow,mil.personnel.wmeat))

# x <- milexp.viewer("MLT")
# y <- milper.viewer("MLT")

##### MMR ----------------------------------------------------------------------

##### cow expenditure
# 2008-2009
mildata$mil.expenditure.cow[mildata$iso3c=="MMR"&mildata$year==2008] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "MMR", 2008, yr.minus.1 = 2007, yr.plus.1 = 2010)
mildata$mil.expenditure.cow[mildata$iso3c=="MMR"&mildata$year==2009] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "MMR", 2009, yr.minus.1 = 2007, yr.plus.1 = 2010)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="MMR"&mildata$year==2008] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "MMR", 2008, yr.minus.1 = 2007, yr.plus.1 = 2010)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="MMR"&mildata$year==2009] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "MMR", 2009, yr.minus.1 = 2007, yr.plus.1 = 2010)

##### wmeat expenditure
# 1948-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "MMR", 1963, restricted = c(1946:1962)
)

# 1995-1998
mildata$mil.expenditure.wmeat[mildata$iso3c=="MMR"&mildata$year==1995] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "MMR", 1995, yr.minus.1 = 1994, yr.plus.1 = 1999)
mildata$mil.expenditure.wmeat[mildata$iso3c=="MMR"&mildata$year==1996] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "MMR", 1996, yr.minus.1 = 1994, yr.plus.1 = 1999)
mildata$mil.expenditure.wmeat[mildata$iso3c=="MMR"&mildata$year==1997] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "MMR", 1997, yr.minus.1 = 1994, yr.plus.1 = 1999)
mildata$mil.expenditure.wmeat[mildata$iso3c=="MMR"&mildata$year==1998] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "MMR", 1998, yr.minus.1 = 1994, yr.plus.1 = 1999)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "MMR", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1948-1950: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "MMR", 1951, restricted = c(1948:1950)
)

# 2006-2019: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "MMR", 2005, restricted = c(2006:2019)
)

##### cow personnel
# good

##### wmeat personnel
# 1948-1962: apply COW personnel growth rates to WMEAT personnel
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "MMR", 1963, restricted = c(1948:1962)
)

# 2018-2019: 2012-13 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="MMR"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="MMR"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="MMR"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="MMR"&mildata$year==2018]

# x <- milexp.viewer("MMR")
# y <- milper.viewer("MMR")

##### MNE ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 2018-2019: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "MNE", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# good

##### cow personnel
# 2006-2007: apply WMEAT personnel growth rates to COW personnel
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.cow",
  col_ref = "mil.personnel.wmeat",
  "MNE", 2008, restricted = c(2006:2007)
)

##### wmeat personnel
# 2018-2019: 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="MNE"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="MNE"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="MNE"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="MNE"&mildata$year==2018]

# x <- milexp.viewer("MNE")
# y <- milper.viewer("MNE")

##### MNG ----------------------------------------------------------------------

##### cow expenditure
# 1946-1947
mildata <- mil_estimator_no_data_func(
  mildata,"MNG",
  metric = "mil.expenditure.cow",
  1947,1946
)
mildata <- mil_estimator_no_data_func(
  mildata,"MNG",
  metric = "mil.expenditure.cow.alt",
  1947,1946
)

# 1949-1951: assume consistent growth between 1948 and 1952
mng.alpha.1 <- (mildata$mil.expenditure.cow[mildata$iso3c=="MNG"&mildata$year==1952]/mildata$mil.expenditure.cow[mildata$iso3c=="MNG"&mildata$year==1948])^(1/4)

mildata$mil.expenditure.cow[mildata$iso3c=="MNG"&mildata$year==1949] <- mildata$mil.expenditure.cow[mildata$iso3c=="MNG"&mildata$year==1948] * mng.alpha.1
mildata$mil.expenditure.cow[mildata$iso3c=="MNG"&mildata$year==1950] <- mildata$mil.expenditure.cow[mildata$iso3c=="MNG"&mildata$year==1949] * mng.alpha.1
mildata$mil.expenditure.cow[mildata$iso3c=="MNG"&mildata$year==1951] <- mildata$mil.expenditure.cow[mildata$iso3c=="MNG"&mildata$year==1950] * mng.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="MNG"&mildata$year==1949] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="MNG"&mildata$year==1948] * mng.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="MNG"&mildata$year==1950] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="MNG"&mildata$year==1949] * mng.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="MNG"&mildata$year==1951] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="MNG"&mildata$year==1950] * mng.alpha.1

# 1954: assume consistent growth between 1953 and 1955
mng.alpha.2 <- (mildata$mil.expenditure.cow[mildata$iso3c=="MNG"&mildata$year==1955]/mildata$mil.expenditure.cow[mildata$iso3c=="MNG"&mildata$year==1953])^(1/2)

mildata$mil.expenditure.cow[mildata$iso3c=="MNG"&mildata$year==1954] <- mildata$mil.expenditure.cow[mildata$iso3c=="MNG"&mildata$year==1953] * mng.alpha.2
mildata$mil.expenditure.cow.alt[mildata$iso3c=="MNG"&mildata$year==1954] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="MNG"&mildata$year==1953] * mng.alpha.2

# 1956: assume consistent growth between 1955 and 1957
mng.alpha.3 <- (mildata$mil.expenditure.cow[mildata$iso3c=="MNG"&mildata$year==1957]/mildata$mil.expenditure.cow[mildata$iso3c=="MNG"&mildata$year==1955])^(1/2)

mildata$mil.expenditure.cow[mildata$iso3c=="MNG"&mildata$year==1956] <- mildata$mil.expenditure.cow[mildata$iso3c=="MNG"&mildata$year==1955] * mng.alpha.3
mildata$mil.expenditure.cow.alt[mildata$iso3c=="MNG"&mildata$year==1956] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="MNG"&mildata$year==1955] * mng.alpha.3

# 1958-1959: assume consistent growth between 1957 and 1960
mng.alpha.4 <- (mildata$mil.expenditure.cow[mildata$iso3c=="MNG"&mildata$year==1960]/mildata$mil.expenditure.cow[mildata$iso3c=="MNG"&mildata$year==1957])^(1/3)

mildata$mil.expenditure.cow[mildata$iso3c=="MNG"&mildata$year==1958] <- mildata$mil.expenditure.cow[mildata$iso3c=="MNG"&mildata$year==1957] * mng.alpha.4
mildata$mil.expenditure.cow[mildata$iso3c=="MNG"&mildata$year==1959] <- mildata$mil.expenditure.cow[mildata$iso3c=="MNG"&mildata$year==1958] * mng.alpha.4
mildata$mil.expenditure.cow.alt[mildata$iso3c=="MNG"&mildata$year==1958] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="MNG"&mildata$year==1957] * mng.alpha.4
mildata$mil.expenditure.cow.alt[mildata$iso3c=="MNG"&mildata$year==1959] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="MNG"&mildata$year==1958] * mng.alpha.4

##### wmeat expenditure
# 1946-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "MNG", 1963, restricted = c(1946:1962)
)

# 1974-1976
mildata$mil.expenditure.wmeat[mildata$iso3c=="MNG"&mildata$year==1974] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "MNG", 1974, yr.minus.1 = 1973, yr.plus.1 = 1977)
mildata$mil.expenditure.wmeat[mildata$iso3c=="MNG"&mildata$year==1975] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "MNG", 1975, yr.minus.1 = 1973, yr.plus.1 = 1977)
mildata$mil.expenditure.wmeat[mildata$iso3c=="MNG"&mildata$year==1976] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "MNG", 1976, yr.minus.1 = 1973, yr.plus.1 = 1977)

# 1978-1983
mildata$mil.expenditure.wmeat[mildata$iso3c=="MNG"&mildata$year==1978] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "MNG", 1978, yr.minus.1 = 1977, yr.plus.1 = 1984, wgt.minus = 6, wgt.plus = 1)
mildata$mil.expenditure.wmeat[mildata$iso3c=="MNG"&mildata$year==1979] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "MNG", 1979, yr.minus.1 = 1977, yr.plus.1 = 1984, wgt.minus = 5, wgt.plus = 2)
mildata$mil.expenditure.wmeat[mildata$iso3c=="MNG"&mildata$year==1980] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "MNG", 1980, yr.minus.1 = 1977, yr.plus.1 = 1984, wgt.minus = 4, wgt.plus = 3)
mildata$mil.expenditure.wmeat[mildata$iso3c=="MNG"&mildata$year==1981] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "MNG", 1981, yr.minus.1 = 1977, yr.plus.1 = 1984, wgt.minus = 3, wgt.plus = 4)
mildata$mil.expenditure.wmeat[mildata$iso3c=="MNG"&mildata$year==1982] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "MNG", 1982, yr.minus.1 = 1977, yr.plus.1 = 1984, wgt.minus = 2, wgt.plus = 5)
mildata$mil.expenditure.wmeat[mildata$iso3c=="MNG"&mildata$year==1983] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "MNG", 1983, yr.minus.1 = 1977, yr.plus.1 = 1984, wgt.minus = 1, wgt.plus = 6)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "MNG", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1990: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "MNG", 1991, restricted = c(1946:1990)
)

##### cow personnel
# 1946-1948: assume consistent growth between 1945 and 1949
mng.alpha.5 <- (mildata$mil.personnel.cow[mildata$iso3c=="MNG"&mildata$year==1949]/mildata$mil.personnel.cow[mildata$iso3c=="MNG"&mildata$year==1945])^(1/4)

mildata$mil.personnel.cow[mildata$iso3c=="MNG"&mildata$year==1946] <- mildata$mil.personnel.cow[mildata$iso3c=="MNG"&mildata$year==1945] * mng.alpha.5
mildata$mil.personnel.cow[mildata$iso3c=="MNG"&mildata$year==1947] <- mildata$mil.personnel.cow[mildata$iso3c=="MNG"&mildata$year==1946] * mng.alpha.5
mildata$mil.personnel.cow[mildata$iso3c=="MNG"&mildata$year==1948] <- mildata$mil.personnel.cow[mildata$iso3c=="MNG"&mildata$year==1947] * mng.alpha.5

##### wmeat personnel
# 1945; 1949-1972: COW and WMEAT personnel match 1973-1993, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1946:1972)&iso3c=="MNG",mil.personnel.cow,mil.personnel.wmeat))

# 1945-1948: apply COW personnel growth rates to WMEAT personnel
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "MNG", 1949, restricted = c(1945:1948)
)

# 2018-2019: 2010-11 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="MNG"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="MNG"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="MNG"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="MNG"&mildata$year==2018]

# x <- milexp.viewer("MNG")
# y <- milper.viewer("MNG")

##### MOZ ----------------------------------------------------------------------

##### cow expenditure
# 2012-2019 (non-alt): apply IISS growth rates to COW
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "defense.spending.iiss",
  "MOZ", 2011, restricted = c(2012:2019)
)

## remove estimate flag for 2012-2019
mildata <- mildata %>%
  dplyr::mutate(mil.expenditure.cow.est.flag  = dplyr::case_when(
    iso3c == "MOZ" & year %in% c(2012:2019) ~ 0,
    .default = mil.expenditure.cow.est.flag
  ))

# 2010 (non-alt): calculate IISS 2010 as a percent of 2009 and 2011 values, apply proportions
# to COW estimates, and average the two estimates
mildata$mil.expenditure.cow[mildata$iso3c=="MOZ"&mildata$year==2010] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "IISS", "MOZ", 2010)

##### wmeat expenditure
# 1983-1984
mildata$mil.expenditure.wmeat[mildata$iso3c=="MOZ"&mildata$year==1983] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "MOZ", 1983, yr.minus.1 = 1982, yr.plus.1 = 1985)
mildata$mil.expenditure.wmeat[mildata$iso3c=="MOZ"&mildata$year==1984] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "MOZ", 1984, yr.minus.1 = 1982, yr.plus.1 = 1985)

# 1986
mildata$mil.expenditure.wmeat[mildata$iso3c=="MOZ"&mildata$year==1986] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "MOZ", 1986)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "MOZ", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1975-1976: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "MOZ", 1977, restricted = c(1975:1976)
)

##### cow personnel
# good

##### wmeat personnel
# 2018-2019: 2010-11 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="MOZ"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="MOZ"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="MOZ"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="MOZ"&mildata$year==2018]

# x <- milexp.viewer("MOZ")
# y <- milper.viewer("MOZ")

##### MRT ----------------------------------------------------------------------

##### cow expenditure
# 1988: estimate COW 1988 as equidistant point based on SIPRI 1987-1989 values
mildata$mil.expenditure.cow[mildata$iso3c=="MRT"&mildata$year==1988] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "MRT", 1988)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="MRT"&mildata$year==1988] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "MRT", 1988)

# 1990: estimate COW 1990 as equidistant point based on WMEAT 1989-1991 values
mildata$mil.expenditure.cow[mildata$iso3c=="MRT"&mildata$year==1990] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "MRT", 1990)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="MRT"&mildata$year==1990] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "MRT", 1990)

##### wmeat expenditure
# 1960-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "MRT", 1963, restricted = c(1960:1962)
)

# 1984: estimate WMEAT 1984 as equidistant point based on COW 1983-1985 values
mildata$mil.expenditure.wmeat[mildata$iso3c=="MRT"&mildata$year==1984] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "MRT", 1984)

# 1988: estimate WMEAT 1988 as equidistant point based on SIPRI 1987-1989 values
mildata$mil.expenditure.wmeat[mildata$iso3c=="MRT"&mildata$year==1988] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "MRT", 1988)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "MRT", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1960-1984: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "MRT", 1985, restricted = c(1960:1984)
)

# 2004
mildata$mil.expenditure.sipri[mildata$iso3c=="MRT"&mildata$year==2004] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "MRT", 2004)

# 2007
mildata$mil.expenditure.sipri[mildata$iso3c=="MRT"&mildata$year==2007] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "MRT", 2007)

# 2010-2011
mildata$mil.expenditure.sipri[mildata$iso3c=="MRT"&mildata$year==2010] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "MRT", 2010, yr.minus.1 = 2009, yr.plus.1 = 2012)
mildata$mil.expenditure.sipri[mildata$iso3c=="MRT"&mildata$year==2011] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "MRT", 2011, yr.minus.1 = 2009, yr.plus.1 = 2012)

##### cow personnel
# good

##### wmeat personnel
# 1960-1963: COW and WMEAT personnel match 1964-1993, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1960:1963)&iso3c=="MRT",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2013-14 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="MRT"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="MRT"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="MRT"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="MRT"&mildata$year==2018]

# x <- milexp.viewer("MRT")
# y <- milper.viewer("MRT")

##### MUS ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "MUS", 2017, restricted = c(2018:2019)
)

# 1973-1975: reestimate
mildata$mil.expenditure.wmeat[mildata$iso3c=="MUS"&mildata$year==1973] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "MUS", 1973, yr.minus.1 = 1972, yr.plus.1 = 1976)
mildata$mil.expenditure.wmeat[mildata$iso3c=="MUS"&mildata$year==1974] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "MUS", 1974, yr.minus.1 = 1972, yr.plus.1 = 1976)
mildata$mil.expenditure.wmeat[mildata$iso3c=="MUS"&mildata$year==1975] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "MUS", 1975, yr.minus.1 = 1972, yr.plus.1 = 1976)

##### sipri expenditure
# good

##### cow personnel
# 2000-2004
mildata$mil.personnel.cow[mildata$iso3c=="MUS"&mildata$year==2000] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW personnel", col_ref = "WMEAT personnel", "MUS", 2000, yr.minus.1 = 1999, yr.plus.1 = 2005, wgt.minus = 5, wgt.plus = 1)
mildata$mil.personnel.cow[mildata$iso3c=="MUS"&mildata$year==2001] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW personnel", col_ref = "WMEAT personnel", "MUS", 2001, yr.minus.1 = 1999, yr.plus.1 = 2005, wgt.minus = 4, wgt.plus = 2)
mildata$mil.personnel.cow[mildata$iso3c=="MUS"&mildata$year==2002] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW personnel", col_ref = "WMEAT personnel", "MUS", 2002, yr.minus.1 = 1999, yr.plus.1 = 2005, wgt.minus = 3, wgt.plus = 3)
mildata$mil.personnel.cow[mildata$iso3c=="MUS"&mildata$year==2003] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW personnel", col_ref = "WMEAT personnel", "MUS", 2003, yr.minus.1 = 1999, yr.plus.1 = 2005, wgt.minus = 2, wgt.plus = 4)
mildata$mil.personnel.cow[mildata$iso3c=="MUS"&mildata$year==2004] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW personnel", col_ref = "WMEAT personnel", "MUS", 2004, yr.minus.1 = 1999, yr.plus.1 = 2005, wgt.minus = 1, wgt.plus = 5)

##### wmeat personnel
# 1968-1970: apply COW personnel growth rates to WMEAT personnel
mildata$mil.personnel.wmeat[mildata$iso3c=="MUS"&mildata$year==1970] <- mildata$mil.personnel.wmeat[mildata$iso3c=="MUS"&mildata$year==1971]
mildata$mil.personnel.wmeat[mildata$iso3c=="MUS"&mildata$year==1969] <- mildata$mil.personnel.wmeat[mildata$iso3c=="MUS"&mildata$year==1970]
mildata$mil.personnel.wmeat[mildata$iso3c=="MUS"&mildata$year==1968] <- mildata$mil.personnel.wmeat[mildata$iso3c=="MUS"&mildata$year==1969]

# 2018-2019: 2005-06 - 2017-18 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="MUS"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="MUS"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="MUS"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="MUS"&mildata$year==2018]

# x <- milexp.viewer("MUS")
# y <- milper.viewer("MUS")

##### MWI ----------------------------------------------------------------------
# confirmed MWI's 2010 IISS value is not a typo
# IISS (2012) - n.k.; IISS (2013) - 325m

##### cow expenditure
# 2010 (non-alt): use average of WMEAT proportions for 2009 and 2011
mildata$mil.expenditure.cow[mildata$iso3c=="MWI"&mildata$year==2010] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "MWI", 2010)

##### wmeat expenditure
# 1964: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "MWI", 1965, restricted = 1964
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "MWI", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1964: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "MWI", 1965, restricted = 1964
)

##### cow personnel
# good

##### wmeat personnel
# 1964: COW and WMEAT personnel match 1965-1976, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year==1964&iso3c=="MWI",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: COW and WMEAT personnel match 2004-2017, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(2018:2019)&iso3c=="MWI",mil.personnel.cow,mil.personnel.wmeat))

x <- milexp.viewer("MWI")
# y <- milper.viewer("MWI")

##### MYS ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1957-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "MYS", 1963, restricted = c(1957:1962)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "MYS", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# good

##### cow personnel
# good

##### wmeat personnel
# 1957-1962: COW and WMEAT personnel match 1963-1982, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1957:1962)&iso3c=="MYS",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "MYS", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("MYS")
# y <- milper.viewer("MYS")

##### NAM ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "NAM", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1990: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "NAM", 1991, restricted = 1990
)

##### cow personnel
# good

##### wmeat personnel
# 1990: COW and WMEAT personnel match 1991-1997, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year==1990&iso3c=="NAM",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "NAM", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("NAM")
# y <- milper.viewer("NAM")

##### NER ----------------------------------------------------------------------

##### cow expenditure
# 2013; 2015-2019 (non-alt): apply IISS growth estimates based on 2012
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "defense.spending.iiss",
  "NER", 2012, restricted = c(2013,2015:2019)
)

## remove estimate flag for 2013, but not 2015-2019 due to the 2014 gap
mildata <- mildata %>%
  dplyr::mutate(mil.expenditure.cow.est.flag  = dplyr::case_when(
    iso3c == "NER" & year == 2013 ~ 0,
    .default = mil.expenditure.cow.est.flag
  ))

# 2014: estimate COW 2014 as equidistant point based on WMEAT 2013-2015 values
mildata$mil.expenditure.cow[mildata$iso3c=="NER"&mildata$year==2014] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "NER", 2014)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="NER"&mildata$year==2014] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "NER", 2014)

##### wmeat expenditure
# 1960-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "NER", 1963, restricted = c(1960:1962)
)

# 1987
mildata$mil.expenditure.wmeat[mildata$iso3c=="NER"&mildata$year==1987] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "NER", 1987)

# 1990
mildata$mil.expenditure.wmeat[mildata$iso3c=="NER"&mildata$year==1990] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "NER", 1990)

# 2018-2019: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "NER", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1960-1974: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "NER", 1975, restricted = c(1960:1974)
)

# 1988-1993
mildata$mil.expenditure.sipri[mildata$iso3c=="NER"&mildata$year==1988] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "NER", 1988, yr.minus.1 = 1987, yr.plus.1 = 1994)
mildata$mil.expenditure.sipri[mildata$iso3c=="NER"&mildata$year==1989] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "NER", 1989, yr.minus.1 = 1987, yr.plus.1 = 1994)
mildata$mil.expenditure.sipri[mildata$iso3c=="NER"&mildata$year==1990] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "NER", 1990, yr.minus.1 = 1987, yr.plus.1 = 1994)
mildata$mil.expenditure.sipri[mildata$iso3c=="NER"&mildata$year==1991] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "NER", 1991, yr.minus.1 = 1987, yr.plus.1 = 1994)
mildata$mil.expenditure.sipri[mildata$iso3c=="NER"&mildata$year==1992] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "NER", 1992, yr.minus.1 = 1987, yr.plus.1 = 1994)
mildata$mil.expenditure.sipri[mildata$iso3c=="NER"&mildata$year==1993] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "NER", 1993, yr.minus.1 = 1987, yr.plus.1 = 1994)

# 2006-2007
mildata$mil.expenditure.sipri[mildata$iso3c=="NER"&mildata$year==2006] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "NER", 2006, yr.minus.1 = 2005, yr.plus.1 = 2008)
mildata$mil.expenditure.sipri[mildata$iso3c=="NER"&mildata$year==2007] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "NER", 2007, yr.minus.1 = 2005, yr.plus.1 = 2008)

# 2015
mildata$mil.expenditure.sipri[mildata$iso3c=="NER"&mildata$year==2015] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW est", "NER", 2015)

##### cow personnel
# good

##### wmeat personnel
# 1960-1962: COW and WMEAT personnel match 1963-1976, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1960:1962)&iso3c=="NER",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2010-11 - 2017-18 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="NER"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="NER"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="NER"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="NER"&mildata$year==2018]

# x <- milexp.viewer("NER")
# y <- milper.viewer("NER")

##### NGA ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1960-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "NGA", 1963, restricted = c(1960:1962)
)

# 1989
mildata$mil.expenditure.wmeat[mildata$iso3c=="NGA"&mildata$year==1989] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "NGA", 1989)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "NGA", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# good

##### cow personnel
# good

##### wmeat personnel
# 1960-1962: COW and WMEAT personnel match 1963-1980, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1960:1962)&iso3c=="NGA",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "NGA", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("NGA")
# y <- milper.viewer("NGA")

##### NIC ----------------------------------------------------------------------

##### cow expenditure
# 1946-1956
mildata <- mil_estimator_no_data_func(
  mildata,"NIC",
  metric = "mil.expenditure.cow",
  1956,1946
)
mildata <- mil_estimator_no_data_func(
  mildata,"NIC",
  metric = "mil.expenditure.cow.alt",
  1956,1946
)

# 1989: assume consistent growth between 1988 and 1990
nic.alpha.1 <- (mildata$mil.expenditure.cow[mildata$iso3c=="NIC"&mildata$year==1990]/mildata$mil.expenditure.cow[mildata$iso3c=="NIC"&mildata$year==1988])^(1/2)

mildata$mil.expenditure.cow[mildata$iso3c=="NIC"&mildata$year==1989] <- mildata$mil.expenditure.cow[mildata$iso3c=="NIC"&mildata$year==1988] * nic.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="NIC"&mildata$year==1989] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="NIC"&mildata$year==1988] * nic.alpha.1

##### wmeat expenditure
# 1957-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "NIC", 1963, restricted = c(1957:1962)
)

# 1946-1956
mildata <- mil_estimator_no_data_func(
  mildata,"NIC",
  metric = "mil.expenditure.wmeat",
  1956,1946
)

# 1986-1988: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "NIC", 1985, restricted = c(1986:1988)
)

# 1989: assume consistent growth between 1988 and 1990
nic.alpha.2 <- (mildata$mil.expenditure.wmeat[mildata$iso3c=="NIC"&mildata$year==1990]/mildata$mil.expenditure.wmeat[mildata$iso3c=="NIC"&mildata$year==1988])^(1/2)

mildata$mil.expenditure.wmeat[mildata$iso3c=="NIC"&mildata$year==1989] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="NIC"&mildata$year==1988] * nic.alpha.2

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "NIC", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1957-1967: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "NIC", 1968, restricted = c(1957:1967)
)

# 1946-1956
mildata <- mil_estimator_no_data_func(
  mildata,"NIC",
  metric = "mil.expenditure.sipri",
  1956,1946
)

# 1982-1988: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "NIC", 1981, restricted = c(1982:1988)
)

# 1989: assume consistent growth between 1988 and 1990
nic.alpha.3 <- (mildata$mil.expenditure.sipri[mildata$iso3c=="NIC"&mildata$year==1990]/mildata$mil.expenditure.sipri[mildata$iso3c=="NIC"&mildata$year==1988])^(1/2)

mildata$mil.expenditure.sipri[mildata$iso3c=="NIC"&mildata$year==1989] <- mildata$mil.expenditure.sipri[mildata$iso3c=="NIC"&mildata$year==1988] * nic.alpha.3

##### cow personnel
# good

##### wmeat personnel
# 1946-1962: COW and WMEAT personnel match 1963-1977, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1946:1962)&iso3c=="NIC",mil.personnel.cow,mil.personnel.wmeat))

# 1978: assume consistent growth between 1977 and 1979
nic.alpha.4 <- (mildata$mil.personnel.wmeat[mildata$iso3c=="NIC"&mildata$year==1979]/mildata$mil.personnel.wmeat[mildata$iso3c=="NIC"&mildata$year==1977])^(1/2)

mildata$mil.personnel.wmeat[mildata$iso3c=="NIC"&mildata$year==1978] <- mildata$mil.personnel.wmeat[mildata$iso3c=="NIC"&mildata$year==1977] * nic.alpha.4

# 2018-2019: 2009-10 - 2017-18 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="NIC"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="NIC"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="NIC"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="NIC"&mildata$year==2018]

# x <- milexp.viewer("NIC")
# y <- milper.viewer("NIC")

##### NLD ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1946-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "NLD", 1963, restricted = c(1946:1962)
)

# 2018-2019: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "NLD", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1948: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "NLD", 1949, restricted = c(1946:1948)
)

##### cow personnel
# good

##### wmeat personnel
# 1945-1962: COW and WMEAT personnel match 1963-2000, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1945:1962)&iso3c=="NLD",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "NLD", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("NLD")
# y <- milper.viewer("NLD")

##### NOR ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1946-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "NOR", 1963, restricted = c(1946:1962)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "NOR", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1948: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "NOR", 1949, restricted = c(1946:1948)
)

##### cow personnel
# good

##### wmeat personnel
# 1945-1962: COW and WMEAT personnel match 1963-1981, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1945:1962)&iso3c=="NOR",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "NOR", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("NOR")
# y <- milper.viewer("NOR")

##### NPL ----------------------------------------------------------------------

##### cow expenditure
# 1953: assume consistent growth between 1952 and 1954
npl.alpha.1 <- (mildata$mil.expenditure.cow[mildata$iso3c=="NPL"&mildata$year==1954]/mildata$mil.expenditure.cow[mildata$iso3c=="NPL"&mildata$year==1952])^(1/2)

mildata$mil.expenditure.cow[mildata$iso3c=="NPL"&mildata$year==1953] <- mildata$mil.expenditure.cow[mildata$iso3c=="NPL"&mildata$year==1952] * npl.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="NPL"&mildata$year==1953] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="NPL"&mildata$year==1952] * npl.alpha.1

# 1946-1949
mildata <- mil_estimator_no_data_func(
  mildata,"NPL",
  metric = "mil.expenditure.cow",
  1949,1946
)
mildata <- mil_estimator_no_data_func(
  mildata,"NPL",
  metric = "mil.expenditure.cow.alt",
  1949,1946
)

# 2013-2019 (non-alt): apply IISS growth estimates based on 2012
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "defense.spending.iiss",
  "NPL", 2012, restricted = c(2013:2019)
)

## remove estimate flag for 2013-2019
mildata <- mildata %>%
  dplyr::mutate(mil.expenditure.cow.est.flag  = dplyr::case_when(
    iso3c == "NPL" & year %in% c(2013:2019) ~ 0,
    .default = mil.expenditure.cow.est.flag
  ))

##### wmeat expenditure
# 1950-1952; 1954-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "NPL", 1963, restricted = c(1946:1962)
)

# 2018-2019: apply IISS growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "defense.spending.iiss",
  "NPL", 2017, restricted = c(2018:2019)
)

# 1946-1949
mildata <- mil_estimator_no_data_func(
  mildata,"NPL",
  metric = "mil.expenditure.wmeat",
  1949,1946
)

# 1953: assume consistent growth between 1952 and 1954
npl.alpha.2 <- (mildata$mil.expenditure.wmeat[mildata$iso3c=="NPL"&mildata$year==1954]/mildata$mil.expenditure.wmeat[mildata$iso3c=="NPL"&mildata$year==1952])^(1/2)

mildata$mil.expenditure.wmeat[mildata$iso3c=="NPL"&mildata$year==1953] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="NPL"&mildata$year==1952] * npl.alpha.2

##### sipri expenditure
# 1963-1969: apply WMEAT growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.wmeat",
  "NPL", 1970, restricted = c(1963:1969)
)

#  1950-1952; 1954-1962: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "NPL", 1963, restricted = c(1946:1962)
)

# 1946-1949
mildata <- mil_estimator_no_data_func(
  mildata,"NPL",
  metric = "mil.expenditure.sipri",
  1949,1946
)

# 1953: assume consistent growth between 1952 and 1954
npl.alpha.3 <- (mildata$mil.expenditure.sipri[mildata$iso3c=="NPL"&mildata$year==1954]/mildata$mil.expenditure.sipri[mildata$iso3c=="NPL"&mildata$year==1952])^(1/2)

mildata$mil.expenditure.sipri[mildata$iso3c=="NPL"&mildata$year==1953] <- mildata$mil.expenditure.sipri[mildata$iso3c=="NPL"&mildata$year==1952] * npl.alpha.3

##### cow personnel
# good

##### wmeat personnel
# 1945-1962: COW and WMEAT personnel match 1963-1971, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1945:1962)&iso3c=="NPL",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "NPL", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("NPL")
# y <- milper.viewer("NPL")

##### NRU(x) ----------------------------------------------------------------------

##### cow expenditure
# 2013-2019

##### wmeat expenditure
# N/A

##### sipri expenditure
# N/A

##### cow personnel
# 2013-2019

##### wmeat personnel
# N/A

x <- milexp.viewer("NRU")
y <- milper.viewer("NRU")

##### NZL ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1946-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "NZL", 1963, restricted = c(1946:1962)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "NZL", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1955: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "NZL", 1956, restricted = c(1946:1955)
)

##### cow personnel
# 1946-1949
mildata <- mil_estimator_no_data_func(
  mildata,"NZL",
  metric = "mil.personnel.cow",
  1949,1946
)

##### wmeat personnel
# 1950-1962: COW and WMEAT personnel match 1963-1977, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1950:1962)&iso3c=="NZL",mil.personnel.cow,mil.personnel.wmeat))

# 1946-1949
mildata <- mil_estimator_no_data_func(
  mildata,"NZL",
  metric = "mil.personnel.wmeat",
  1949,1946
)

# 2018-2019: COW and WMEAT personnel match 2013-2017, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(2018:2019)&iso3c=="NZL",mil.personnel.cow,mil.personnel.wmeat))

# x <- milexp.viewer("NZL")
# y <- milper.viewer("NZL")

##### OMN ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1971-1972: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "OMN", 1973, restricted = c(1971:1972)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "OMN", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# good

##### cow personnel
# good

##### wmeat personnel
# 2018-2019: 2008-09 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="OMN"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="OMN"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="OMN"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="OMN"&mildata$year==2018]

# x <- milexp.viewer("OMN")
# y <- milper.viewer("OMN")

##### PAK ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1947-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "PAK", 1963, restricted = c(1947:1962)
)

# 2018-2019: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "PAK", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1947-1950: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "PAK", 1951, restricted = c(1947:1950)
)

##### cow personnel
# good

##### wmeat personnel
# 1947-1962: COW and WMEAT personnel match 1963-1973, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1947:1962)&iso3c=="PAK",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2014-2017 have paralleled growth between COW and WMEAT, so apply continued pattern from COW to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="PAK"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="PAK"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="PAK"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="PAK"&mildata$year==2018]

# x <- milexp.viewer("PAK")
# y <- milper.viewer("PAK")

##### PAN ----------------------------------------------------------------------

##### cow expenditure
# 1946-1959
mildata <- mil_estimator_no_data_func(
  mildata,"PAN",
  metric = "mil.expenditure.cow",
  1959,1946
)
mildata <- mil_estimator_no_data_func(
  mildata,"PAN",
  metric = "mil.expenditure.cow.alt",
  1959,1946
)

##### wmeat expenditure
# 1960-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "PAN", 1963, restricted = c(1960:1962)
)

# 1946-1959
mildata <- mil_estimator_no_data_func(
  mildata,"PAN",
  metric = "mil.expenditure.wmeat",
  1959,1946
)

# 1980
mildata$mil.expenditure.wmeat[mildata$iso3c=="PAN"&mildata$year==1980] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "PAN", 1980)

# 1983
mildata$mil.expenditure.wmeat[mildata$iso3c=="PAN"&mildata$year==1983] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "PAN", 1983)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "PAN", 2017, restricted = c(2018:2019)
)

# 1992-1994: reestimate
mildata$mil.expenditure.wmeat[mildata$iso3c=="PAN"&mildata$year==1992] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "PAN", 1992, yr.minus.1 = 1991, yr.plus.1 = 1995)
mildata$mil.expenditure.wmeat[mildata$iso3c=="PAN"&mildata$year==1993] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "PAN", 1993, yr.minus.1 = 1991, yr.plus.1 = 1995)
mildata$mil.expenditure.wmeat[mildata$iso3c=="PAN"&mildata$year==1994] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "PAN", 1994, yr.minus.1 = 1991, yr.plus.1 = 1995)

##### sipri expenditure
# 1946-1986: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "PAN", 1987, restricted = c(1946:1986)
)

##### cow personnel
# 2000-2001: assume consistent decline
pan.alpha <- (mildata$mil.personnel.cow[mildata$iso3c=="PAN"&mildata$year==2002] - mildata$mil.personnel.cow[mildata$iso3c=="PAN"&mildata$year==1999]) / 3

mildata$mil.personnel.cow[mildata$iso3c=="PAN"&mildata$year==2000] <- mildata$mil.personnel.cow[mildata$iso3c=="PAN"&mildata$year==1999] + pan.alpha
mildata$mil.personnel.cow[mildata$iso3c=="PAN"&mildata$year==2001] <- mildata$mil.personnel.cow[mildata$iso3c=="PAN"&mildata$year==2000] + pan.alpha

##### wmeat personnel
# 1945-1962: COW and WMEAT personnel match 1963-1982, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1945:1962)&iso3c=="PAN",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="PAN"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="PAN"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="PAN"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="PAN"&mildata$year==2018]

# x <- milexp.viewer("PAN")
# y <- milper.viewer("PAN")

##### PER ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1946-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "PER", 1963, restricted = c(1946:1962)
)

# 1988-1989
mildata$mil.expenditure.wmeat[mildata$iso3c=="PER"&mildata$year==1988] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "PER", 1988, yr.minus.1 = 1987, yr.plus.1 = 1990)
mildata$mil.expenditure.wmeat[mildata$iso3c=="PER"&mildata$year==1989] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "PER", 1989, yr.minus.1 = 1987, yr.plus.1 = 1990)

# 2018-2019: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "PER", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1948: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "PER", 1949, restricted = c(1946:1948)
)

##### cow personnel
# good

##### wmeat personnel
# 1945-1962: COW and WMEAT personnel match 1963-2000, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1945:1962)&iso3c=="PER",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="PER"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="PER"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="PER"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="PER"&mildata$year==2018]

# x <- milexp.viewer("PER")
# y <- milper.viewer("PER")

##### PHL ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1946-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "PHL", 1963, restricted = c(1946:1962)
)

# 2018-2019: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "PHL", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1957: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "PHL", 1958, restricted = c(1946:1957)
)

# 1975
mildata$mil.expenditure.sipri[mildata$iso3c=="PHL"&mildata$year==1975] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "PHL", 1975)

##### cow personnel
# good

##### wmeat personnel
# 1946-1962: COW and WMEAT personnel match 1963-1982, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1946:1962)&iso3c=="PHL",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2011-12 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="PHL"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="PHL"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="PHL"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="PHL"&mildata$year==2018]

# x <- milexp.viewer("PHL")
# y <- milper.viewer("PHL")

##### PLW(x) ----------------------------------------------------------------------

##### cow expenditure
# 2013-2019

##### wmeat expenditure
# N/A

##### sipri expenditure
# N/A

##### cow personnel
# 2013-2019

##### wmeat personnel
# N/A

x <- milexp.viewer("PLW")
y <- milper.viewer("PLW")

##### PNG ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1975: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "PNG", 1976, restricted = 1975
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "PNG", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1982-1984
mildata$mil.expenditure.sipri[mildata$iso3c=="PNG"&mildata$year==1982] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "PNG", 1982, yr.minus.1 = 1981, yr.plus.1 = 1985)
mildata$mil.expenditure.sipri[mildata$iso3c=="PNG"&mildata$year==1983] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "PNG", 1983, yr.minus.1 = 1981, yr.plus.1 = 1985)
mildata$mil.expenditure.sipri[mildata$iso3c=="PNG"&mildata$year==1984] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "PNG", 1984, yr.minus.1 = 1981, yr.plus.1 = 1985)

##### cow personnel
# good

##### wmeat personnel
# 1975-1976: COW and WMEAT personnel match 1977, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1975:1976)&iso3c=="PNG",mil.personnel.cow,mil.personnel.wmeat))

# 1993: COW and WMEAT personnel match 1979-2000, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year==1993&iso3c=="PNG",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: COW and WMEAT personnel match 2015-2017, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(2018:2019)&iso3c=="PNG",mil.personnel.cow,mil.personnel.wmeat))

# x <- milexp.viewer("PNG")
# y <- milper.viewer("PNG")

##### POL ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1946-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "POL", 1963, restricted = c(1946:1962)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "POL", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1950: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "POL", 1951, restricted = c(1946:1950)
)

##### cow personnel
# good

##### wmeat personnel
# 1945-1962: COW and WMEAT personnel match 1967-1976, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1945:1962)&iso3c=="POL",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "POL", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("POL")
# y <- milper.viewer("POL")

##### PRK ----------------------------------------------------------------------

##### cow expenditure
# 1959: assume consistent decline between 1958 and 1960
prk.alpha.1 <- (mildata$mil.expenditure.cow[mildata$iso3c=="PRK"&mildata$year==1960]/mildata$mil.expenditure.cow[mildata$iso3c=="PRK"&mildata$year==1958])^(1/2)

mildata$mil.expenditure.cow[mildata$iso3c=="PRK"&mildata$year==1959] <- mildata$mil.expenditure.cow[mildata$iso3c=="PRK"&mildata$year==1958] * prk.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="PRK"&mildata$year==1959] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="PRK"&mildata$year==1958] * prk.alpha.1

# 1949-1957
# mildata <- mil_estimator_no_data_func(
#   mildata,"PRK",
#   metric = "mil.expenditure.cow",
#   1957,1949
# )
# mildata <- mil_estimator_no_data_func(
#   mildata,"PRK",
#   metric = "mil.expenditure.cow.alt",
#   1957,1949
# )

# 2002-2017: apply WMEAT growth rates to COW
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "mil.expenditure.wmeat",
  "PRK", 2001, restricted = c(2002:2017)
)
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow.alt",
  col_ref = "mil.expenditure.wmeat",
  "PRK", 2001, restricted = c(2002:2017)
)

# 2018: apply SIPRI growth rates to COW
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "mil.expenditure.sipri",
  "PRK", 2017, restricted = 2018
)
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow.alt",
  col_ref = "mil.expenditure.sipri",
  "PRK", 2017, restricted = 2018
)

# 2019
mildata <- mil_estimator_no_data_func(
  mildata,"PRK",
  metric = "mil.expenditure.cow",
  2019,2019
)
mildata <- mil_estimator_no_data_func(
  mildata,"PRK",
  metric = "mil.expenditure.cow.alt",
  2019,2019
)

##### wmeat expenditure
# 1958; 1960-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "PRK", 1963, restricted = c(1958,1960:1962)
)

# 1959: assume consistent decline between 1958 and 1960
prk.alpha.2 <- (mildata$mil.expenditure.wmeat[mildata$iso3c=="PRK"&mildata$year==1960]/mildata$mil.expenditure.wmeat[mildata$iso3c=="PRK"&mildata$year==1958])^(1/2)

mildata$mil.expenditure.wmeat[mildata$iso3c=="PRK"&mildata$year==1959] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="PRK"&mildata$year==1958] * prk.alpha.2

# 1948-1957
# mildata <- mil_estimator_no_data_func(
#   mildata,"PRK",
#   metric = "mil.expenditure.wmeat",
#   1957,1948
# )

# 2018: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "PRK", 2017, restricted = 2018
)

# 2019
mildata <- mil_estimator_no_data_func(
  mildata,"PRK",
  metric = "mil.expenditure.wmeat",
  2019,2019
)

##### sipri expenditure
# 1963-2013: apply WMEAT growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.wmeat",
  "PRK", 2014, restricted = c(1963:2013)
)

# 1958; 1960-1962: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "PRK", 1963, restricted = c(1958,1960:1962)
)

# 1959: assume consistent decline between 1958 and 1960
prk.alpha.3 <- (mildata$mil.expenditure.sipri[mildata$iso3c=="PRK"&mildata$year==1960]/mildata$mil.expenditure.sipri[mildata$iso3c=="PRK"&mildata$year==1958])^(1/2)

mildata$mil.expenditure.sipri[mildata$iso3c=="PRK"&mildata$year==1959] <- mildata$mil.expenditure.sipri[mildata$iso3c=="PRK"&mildata$year==1958] * prk.alpha.3

# 1948-1957
# mildata <- mil_estimator_no_data_func(
#   mildata,"PRK",
#   metric = "mil.expenditure.sipri",
#   1957,1948
# )

# 2019
mildata <- mil_estimator_no_data_func(
  mildata,"PRK",
  metric = "mil.expenditure.sipri",
  2019,2019
)

##### cow personnel
# good

##### wmeat personnel
# 1948-1962: COW and WMEAT personnel match 1963-1980, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1948:1962)&iso3c=="PRK",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "PRK", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("PRK")
# y <- milper.viewer("PRK")

##### PRT ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1946-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "PRT", 1963, restricted = c(1946:1962)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "PRT", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1948: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "PRT", 1949, restricted = c(1946:1948)
)

##### cow personnel
# 1946-1947
mildata <- mil_estimator_no_data_func(
  mildata,"PRT",
  metric = "mil.personnel.cow",
  1947,1946
)

##### wmeat personnel
# 1948-1962: COW and WMEAT personnel match 1963, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1948:1962)&iso3c=="PRT",mil.personnel.cow,mil.personnel.wmeat))

# 1946-1947
mildata <- mil_estimator_no_data_func(
  mildata,"PRT",
  metric = "mil.personnel.wmeat",
  1947,1946
)

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "PRT", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("PRT")
# y <- milper.viewer("PRT")

##### PRY ----------------------------------------------------------------------

##### cow expenditure
# 1948-1949: assume consistent decline between 1947 and 1950
pry.alpha.1 <- (mildata$mil.expenditure.cow[mildata$iso3c=="PRY"&mildata$year==1950]/mildata$mil.expenditure.cow[mildata$iso3c=="PRY"&mildata$year==1947])^(1/3)

mildata$mil.expenditure.cow[mildata$iso3c=="PRY"&mildata$year==1948] <- mildata$mil.expenditure.cow[mildata$iso3c=="PRY"&mildata$year==1947] * pry.alpha.1
mildata$mil.expenditure.cow[mildata$iso3c=="PRY"&mildata$year==1949] <- mildata$mil.expenditure.cow[mildata$iso3c=="PRY"&mildata$year==1948] * pry.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="PRY"&mildata$year==1948] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="PRY"&mildata$year==1947] * pry.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="PRY"&mildata$year==1949] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="PRY"&mildata$year==1948] * pry.alpha.1

# 1953: assume consistent decline between 1952 and 1954
pry.alpha.2 <- (mildata$mil.expenditure.cow[mildata$iso3c=="PRY"&mildata$year==1954]/mildata$mil.expenditure.cow[mildata$iso3c=="PRY"&mildata$year==1952])^(1/2)

mildata$mil.expenditure.cow[mildata$iso3c=="PRY"&mildata$year==1953] <- mildata$mil.expenditure.cow[mildata$iso3c=="PRY"&mildata$year==1952] * pry.alpha.2
mildata$mil.expenditure.cow.alt[mildata$iso3c=="PRY"&mildata$year==1953] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="PRY"&mildata$year==1952] * pry.alpha.2

##### wmeat expenditure
# 1946-1947, 1950-1952, 1954-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "PRY", 1963, restricted = c(1946:1947, 1950:1952, 1954:1962)
)

# 1948-1949: assume consistent decline between 1947 and 1950
pry.alpha.3 <- (mildata$mil.expenditure.wmeat[mildata$iso3c=="PRY"&mildata$year==1950]/mildata$mil.expenditure.wmeat[mildata$iso3c=="PRY"&mildata$year==1947])^(1/3)

mildata$mil.expenditure.wmeat[mildata$iso3c=="PRY"&mildata$year==1948] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="PRY"&mildata$year==1947] * pry.alpha.3
mildata$mil.expenditure.wmeat[mildata$iso3c=="PRY"&mildata$year==1949] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="PRY"&mildata$year==1948] * pry.alpha.3

# 1953: assume consistent decline between 1952 and 1954
pry.alpha.4 <- (mildata$mil.expenditure.wmeat[mildata$iso3c=="PRY"&mildata$year==1954]/mildata$mil.expenditure.wmeat[mildata$iso3c=="PRY"&mildata$year==1952])^(1/2)

mildata$mil.expenditure.wmeat[mildata$iso3c=="PRY"&mildata$year==1953] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="PRY"&mildata$year==1952] * pry.alpha.4

# 1986
mildata$mil.expenditure.wmeat[mildata$iso3c=="PRY"&mildata$year==1986] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "PRY", 1986)

# 2018-2019: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "PRY", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1947, 1950-1952, 1954-1959: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "PRY", 1960, restricted = c(1946:1947, 1950:1952, 1954:1959)
)

# 1948-1949: assume consistent decline between 1947 and 1950
pry.alpha.5 <- (mildata$mil.expenditure.sipri[mildata$iso3c=="PRY"&mildata$year==1950]/mildata$mil.expenditure.sipri[mildata$iso3c=="PRY"&mildata$year==1947])^(1/3)

mildata$mil.expenditure.sipri[mildata$iso3c=="PRY"&mildata$year==1948] <- mildata$mil.expenditure.sipri[mildata$iso3c=="PRY"&mildata$year==1947] * pry.alpha.5
mildata$mil.expenditure.sipri[mildata$iso3c=="PRY"&mildata$year==1949] <- mildata$mil.expenditure.sipri[mildata$iso3c=="PRY"&mildata$year==1948] * pry.alpha.5

# 1953: assume consistent decline between 1952 and 1954
pry.alpha.6 <- (mildata$mil.expenditure.sipri[mildata$iso3c=="PRY"&mildata$year==1954]/mildata$mil.expenditure.sipri[mildata$iso3c=="PRY"&mildata$year==1952])^(1/2)

mildata$mil.expenditure.sipri[mildata$iso3c=="PRY"&mildata$year==1953] <- mildata$mil.expenditure.sipri[mildata$iso3c=="PRY"&mildata$year==1952] * pry.alpha.6

##### cow personnel
# good

##### wmeat personnel
# 1945-1962: COW and WMEAT personnel match 1963-1993, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1945:1962)&iso3c=="PRY",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: COW and WMEAT personnel match 2013-2017, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(2018:2019)&iso3c=="PRY",mil.personnel.cow,mil.personnel.wmeat))

# x <- milexp.viewer("PRY")
# y <- milper.viewer("PRY")

##### PSE(x) ----------------------------------------------------------------------

# N/A for all

# cow expenditure

# wmeat expenditure

# sipri expenditure

# cow personnel

# wmeat personnel

x <- milexp.viewer("PSE")
y <- milper.viewer("PSE")

##### QAT ----------------------------------------------------------------------

##### cow expenditure
# 1982
mildata$mil.expenditure.cow[mildata$iso3c=="QAT"&mildata$year==1982] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "QAT", 1982)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="QAT"&mildata$year==1982] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "QAT", 1982)

# 1986: assume consistent decline between 1985 and 1987
qat.alpha.1 <- (mildata$mil.expenditure.cow[mildata$iso3c=="QAT"&mildata$year==1987]/mildata$mil.expenditure.cow[mildata$iso3c=="QAT"&mildata$year==1985])^(1/3)

mildata$mil.expenditure.cow[mildata$iso3c=="QAT"&mildata$year==1986] <- mildata$mil.expenditure.cow[mildata$iso3c=="QAT"&mildata$year==1985] * qat.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="QAT"&mildata$year==1986] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="QAT"&mildata$year==1985] * qat.alpha.1

# 1989-1990
mildata$mil.expenditure.cow[mildata$iso3c=="QAT"&mildata$year==1989] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "QAT", 1989, yr.minus.1 = 1988, yr.plus.1 = 1991)
mildata$mil.expenditure.cow[mildata$iso3c=="QAT"&mildata$year==1990] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "QAT", 1990, yr.minus.1 = 1988, yr.plus.1 = 1991)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="QAT"&mildata$year==1989] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "QAT", 1989, yr.minus.1 = 1988, yr.plus.1 = 1991)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="QAT"&mildata$year==1990] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "QAT", 1990, yr.minus.1 = 1988, yr.plus.1 = 1991)

# 1992: estimate COW 1992 as equidistant point based on WMEAT 1991-1993 values
mildata$mil.expenditure.cow[mildata$iso3c=="QAT"&mildata$year==1992] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "QAT", 1992)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="QAT"&mildata$year==1992] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "QAT", 1992)

# 2012-2016 (non-alt): apply IISS growth estimates based on 2011
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "defense.spending.iiss",
  "QAT", 2011, restricted = c(2012:2016)
)

## remove estimate flag for 2012-2016
mildata <- mildata %>%
  dplyr::mutate(mil.expenditure.cow.est.flag  = dplyr::case_when(
    iso3c == "QAT" & year %in% c(2012:2016) ~ 0,
    .default = mil.expenditure.cow.est.flag
  ))

# 2017: apply WMEAT growth estimates based on 2016
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "mil.expenditure.wmeat",
  "QAT", 2016, restricted = 2017
)
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow.alt",
  col_ref = "mil.expenditure.wmeat",
  "QAT", 2016, restricted = 2017
)

# 2018-2019
mildata <- mil_estimator_no_data_func(
  mildata,"QAT",
  metric = "mil.expenditure.cow",
  2018,2019
)
mildata <- mil_estimator_no_data_func(
  mildata,"QAT",
  metric = "mil.expenditure.cow.alt",
  2018,2019
)

##### wmeat expenditure
# 1971-1972: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "QAT", 1973, restricted = c(1971:1972)
)

# 1981-1985: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "QAT", 1980, restricted = c(1981:1985)
)

# 1987-1990: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "QAT", 1991, restricted = c(1987:1990)
)

# 1986: assume consistent decline between 1985 and 1987
qat.alpha.2 <- (mildata$mil.expenditure.wmeat[mildata$iso3c=="QAT"&mildata$year==1987]/mildata$mil.expenditure.wmeat[mildata$iso3c=="QAT"&mildata$year==1985])^(1/3)

mildata$mil.expenditure.wmeat[mildata$iso3c=="QAT"&mildata$year==1986] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="QAT"&mildata$year==1985] * qat.alpha.2

# 2018-2019
mildata <- mil_estimator_no_data_func(
  mildata,"QAT",
  metric = "mil.expenditure.wmeat",
  2018,2019
)

##### sipri expenditure
# 1971-1979: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "QAT", 1980, restricted = c(1971:1979)
)

# 1986: assume consistent decline between 1985 and 1987
qat.alpha.3 <- (mildata$mil.expenditure.sipri[mildata$iso3c=="QAT"&mildata$year==1987]/mildata$mil.expenditure.sipri[mildata$iso3c=="QAT"&mildata$year==1985])^(1/3)

mildata$mil.expenditure.sipri[mildata$iso3c=="QAT"&mildata$year==1986] <- mildata$mil.expenditure.sipri[mildata$iso3c=="QAT"&mildata$year==1985] * qat.alpha.3

# 1992-2001
mildata$mil.expenditure.sipri[mildata$iso3c=="QAT"&mildata$year==1992] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "QAT", 1992, yr.minus.1 = 1991, yr.plus.1 = 2002, wgt.minus = 10, wgt.plus = 1)
mildata$mil.expenditure.sipri[mildata$iso3c=="QAT"&mildata$year==1993] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "QAT", 1993, yr.minus.1 = 1991, yr.plus.1 = 2002, wgt.minus = 9, wgt.plus = 2)
mildata$mil.expenditure.sipri[mildata$iso3c=="QAT"&mildata$year==1994] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "QAT", 1994, yr.minus.1 = 1991, yr.plus.1 = 2002, wgt.minus = 8, wgt.plus = 3)
mildata$mil.expenditure.sipri[mildata$iso3c=="QAT"&mildata$year==1995] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "QAT", 1995, yr.minus.1 = 1991, yr.plus.1 = 2002, wgt.minus = 7, wgt.plus = 4)
mildata$mil.expenditure.sipri[mildata$iso3c=="QAT"&mildata$year==1996] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "QAT", 1996, yr.minus.1 = 1991, yr.plus.1 = 2002, wgt.minus = 6, wgt.plus = 5)
mildata$mil.expenditure.sipri[mildata$iso3c=="QAT"&mildata$year==1997] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "QAT", 1997, yr.minus.1 = 1991, yr.plus.1 = 2002, wgt.minus = 5, wgt.plus = 6)
mildata$mil.expenditure.sipri[mildata$iso3c=="QAT"&mildata$year==1998] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "QAT", 1998, yr.minus.1 = 1991, yr.plus.1 = 2002, wgt.minus = 4, wgt.plus = 7)
mildata$mil.expenditure.sipri[mildata$iso3c=="QAT"&mildata$year==1999] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "QAT", 1999, yr.minus.1 = 1991, yr.plus.1 = 2002, wgt.minus = 3, wgt.plus = 8)
mildata$mil.expenditure.sipri[mildata$iso3c=="QAT"&mildata$year==2000] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "QAT", 2000, yr.minus.1 = 1991, yr.plus.1 = 2002, wgt.minus = 2, wgt.plus = 9)
mildata$mil.expenditure.sipri[mildata$iso3c=="QAT"&mildata$year==2001] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "QAT", 2001, yr.minus.1 = 1991, yr.plus.1 = 2002, wgt.minus = 1, wgt.plus = 10)


# 2011-2017: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "QAT", 2010, restricted = c(2011:2017)
)

# 2018-2019
mildata <- mil_estimator_no_data_func(
  mildata,"QAT",
  metric = "mil.expenditure.sipri",
  2018,2019
)

##### cow personnel
# good

##### wmeat personnel
# 1971
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "QAT", 1972, restricted = 1971
)

# 2018-2019: COW and WMEAT personnel match 2004-2017, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(2018:2019)&iso3c=="QAT",mil.personnel.cow,mil.personnel.wmeat))

# x <- milexp.viewer("QAT")
# y <- milper.viewer("QAT")

##### ROU ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1946-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "ROU", 1963, restricted = c(1946:1962)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "ROU", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1956: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "ROU", 1957, restricted = c(1946:1956)
)

##### cow personnel
# good

##### wmeat personnel
# 1945-1962: COW and WMEAT personnel match 1963-1965, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1945:1962)&iso3c=="ROU",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "ROU", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("ROU")
# y <- milper.viewer("ROU")

##### RUS ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 2018-2019: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "RUS", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1991-1992: apply WMEAT growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.wmeat",
  "RUS", 1993, restricted = c(1991:1992)
)

##### cow personnel
# good

##### wmeat personnel
# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "RUS", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("RUS")
# y <- milper.viewer("RUS")

##### RVN ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1954-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "RVN", 1963, restricted = c(1954:1962)
)

##### sipri expenditure
# N/A

##### cow personnel
# good

##### wmeat personnel
# 1954-1962: COW and WMEAT personnel match 1963, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1954:1962)&iso3c=="RVN",mil.personnel.cow,mil.personnel.wmeat))

# 1975: COW and WMEAT personnel match 1973-1974, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year==1975&iso3c=="RVN",mil.personnel.cow,mil.personnel.wmeat))

x <- milexp.viewer("RVN")
# y <- milper.viewer("RVN")

##### RWA ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "RWA", 1963, restricted = 1962
)

# 1982
mildata$mil.expenditure.wmeat[mildata$iso3c=="RWA"&mildata$year==1982] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "RWA", 1982)

# 1984
mildata$mil.expenditure.wmeat[mildata$iso3c=="RWA"&mildata$year==1984] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "RWA", 1984)

# 1986
mildata$mil.expenditure.wmeat[mildata$iso3c=="RWA"&mildata$year==1986] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "RWA", 1986)

# 2018-2019: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "RWA", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1963-1972: apply WMEAT growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.wmeat",
  "RWA", 1973, restricted = c(1963:1972)
)

# 1962: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "RWA", 1963, restricted = 1962
)

##### cow personnel
# good

##### wmeat personnel
# 1962: COW and WMEAT personnel match 1963-1993, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year==1962&iso3c=="RWA",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2007-08 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="RWA"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="RWA"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="RWA"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="RWA"&mildata$year==2018]

# x <- milexp.viewer("RWA")
# y <- milper.viewer("RWA")

##### SAU ----------------------------------------------------------------------

##### cow expenditure
# 1949-1950: assume consistent decline between 1948 and 1951
sau.alpha.1 <- (mildata$mil.expenditure.cow[mildata$iso3c=="SAU"&mildata$year==1951]/mildata$mil.expenditure.cow[mildata$iso3c=="SAU"&mildata$year==1948])^(1/3)

mildata$mil.expenditure.cow[mildata$iso3c=="SAU"&mildata$year==1949] <- mildata$mil.expenditure.cow[mildata$iso3c=="SAU"&mildata$year==1948] * sau.alpha.1
mildata$mil.expenditure.cow[mildata$iso3c=="SAU"&mildata$year==1950] <- mildata$mil.expenditure.cow[mildata$iso3c=="SAU"&mildata$year==1949] * sau.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="SAU"&mildata$year==1949] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="SAU"&mildata$year==1948] * sau.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="SAU"&mildata$year==1950] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="SAU"&mildata$year==1949] * sau.alpha.1

# 1956: assume consistent decline between 1955 and 1957
sau.alpha.2 <- (mildata$mil.expenditure.cow[mildata$iso3c=="SAU"&mildata$year==1957]/mildata$mil.expenditure.cow[mildata$iso3c=="SAU"&mildata$year==1955])^(1/3)

mildata$mil.expenditure.cow[mildata$iso3c=="SAU"&mildata$year==1956] <- mildata$mil.expenditure.cow[mildata$iso3c=="SAU"&mildata$year==1955] * sau.alpha.2
mildata$mil.expenditure.cow.alt[mildata$iso3c=="SAU"&mildata$year==1956] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="SAU"&mildata$year==1955] * sau.alpha.2

# 1946
mildata <- mil_estimator_no_data_func(
  mildata,"SAU",
  metric = "mil.expenditure.cow",
  1946,1946
)
mildata <- mil_estimator_no_data_func(
  mildata,"SAU",
  metric = "mil.expenditure.cow.alt",
  1946,1946
)

##### wmeat expenditure
# 1947-1948; 1951-1955; 1957-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "SAU", 1963, restricted = c(1947:1948, 1951:1955, 1957:1962)
)

# 1949-1950: assume consistent decline between 1948 and 1951
sau.alpha.3 <- (mildata$mil.expenditure.wmeat[mildata$iso3c=="SAU"&mildata$year==1951]/mildata$mil.expenditure.wmeat[mildata$iso3c=="SAU"&mildata$year==1948])^(1/3)

mildata$mil.expenditure.wmeat[mildata$iso3c=="SAU"&mildata$year==1949] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="SAU"&mildata$year==1948] * sau.alpha.3
mildata$mil.expenditure.wmeat[mildata$iso3c=="SAU"&mildata$year==1950] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="SAU"&mildata$year==1949] * sau.alpha.3

# 1956: assume consistent decline between 1955 and 1957
sau.alpha.4 <- (mildata$mil.expenditure.wmeat[mildata$iso3c=="SAU"&mildata$year==1957]/mildata$mil.expenditure.wmeat[mildata$iso3c=="SAU"&mildata$year==1955])^(1/3)

mildata$mil.expenditure.wmeat[mildata$iso3c=="SAU"&mildata$year==1956] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="SAU"&mildata$year==1955] * sau.alpha.4

# 1946
mildata <- mil_estimator_no_data_func(
  mildata,"SAU",
  metric = "mil.expenditure.wmeat",
  1946,1946
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "SAU", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1947-1948; 1951-1955; 1957: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "SAU", 1958, restricted = c(1947:1948, 1951:1955, 1957)
)

# 1949-1950: assume consistent decline between 1948 and 1951
sau.alpha.5 <- (mildata$mil.expenditure.sipri[mildata$iso3c=="SAU"&mildata$year==1951]/mildata$mil.expenditure.sipri[mildata$iso3c=="SAU"&mildata$year==1948])^(1/3)

mildata$mil.expenditure.sipri[mildata$iso3c=="SAU"&mildata$year==1949] <- mildata$mil.expenditure.sipri[mildata$iso3c=="SAU"&mildata$year==1948] * sau.alpha.5
mildata$mil.expenditure.sipri[mildata$iso3c=="SAU"&mildata$year==1950] <- mildata$mil.expenditure.sipri[mildata$iso3c=="SAU"&mildata$year==1949] * sau.alpha.5

# 1956: assume consistent decline between 1955 and 1957
sau.alpha.6 <- (mildata$mil.expenditure.sipri[mildata$iso3c=="SAU"&mildata$year==1957]/mildata$mil.expenditure.sipri[mildata$iso3c=="SAU"&mildata$year==1955])^(1/3)

mildata$mil.expenditure.sipri[mildata$iso3c=="SAU"&mildata$year==1956] <- mildata$mil.expenditure.sipri[mildata$iso3c=="SAU"&mildata$year==1955] * sau.alpha.6

# 1974-1976
mildata$mil.expenditure.sipri[mildata$iso3c=="SAU"&mildata$year==1974] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "SAU", 1974, yr.minus.1 = 1973, yr.plus.1 = 1977)
mildata$mil.expenditure.sipri[mildata$iso3c=="SAU"&mildata$year==1975] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "SAU", 1975, yr.minus.1 = 1973, yr.plus.1 = 1977)
mildata$mil.expenditure.sipri[mildata$iso3c=="SAU"&mildata$year==1976] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "SAU", 1976, yr.minus.1 = 1973, yr.plus.1 = 1977)

# 1986
mildata$mil.expenditure.sipri[mildata$iso3c=="SAU"&mildata$year==1986] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "SAU", 1986)

# 1946
mildata <- mil_estimator_no_data_func(
  mildata,"SAU",
  metric = "mil.expenditure.sipri",
  1946,1946
)

##### cow personnel
# good

##### wmeat personnel
# 1945-1962: COW and WMEAT personnel match 1963-1973, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1945:1962)&iso3c=="SAU",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2015-16 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="SAU"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="SAU"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="SAU"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="SAU"&mildata$year==2018]

# x <- milexp.viewer("SAU")
# y <- milper.viewer("SAU")

##### SDN ----------------------------------------------------------------------

##### cow expenditure
# 2006-2008
mildata$mil.expenditure.cow[mildata$iso3c=="SDN"&mildata$year==2006] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "SDN", 2006, yr.minus.1 = 2005, yr.plus.1 = 2009, wgt.minus = 3, wgt.plus = 1)
mildata$mil.expenditure.cow[mildata$iso3c=="SDN"&mildata$year==2007] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "SDN", 2007, yr.minus.1 = 2005, yr.plus.1 = 2009, wgt.minus = 2, wgt.plus = 2)
mildata$mil.expenditure.cow[mildata$iso3c=="SDN"&mildata$year==2008] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "SDN", 2008, yr.minus.1 = 2005, yr.plus.1 = 2009, wgt.minus = 1, wgt.plus = 3)

mildata$mil.expenditure.cow.alt[mildata$iso3c=="SDN"&mildata$year==2006] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "SDN", 2006, yr.minus.1 = 2005, yr.plus.1 = 2009, wgt.minus = 3, wgt.plus = 1)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="SDN"&mildata$year==2007] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "SDN", 2007, yr.minus.1 = 2005, yr.plus.1 = 2009, wgt.minus = 2, wgt.plus = 2)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="SDN"&mildata$year==2008] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "SDN", 2008, yr.minus.1 = 2005, yr.plus.1 = 2009, wgt.minus = 1, wgt.plus = 3)

# 2013: apply IISS growth estimates based on 2011
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "defense.spending.iiss",
  "SDN", 2011, restricted = 2013
)

## remove estimate flag for 2013
mildata <- mildata %>%
  dplyr::mutate(mil.expenditure.cow.est.flag  = dplyr::case_when(
    iso3c == "SDN" & year == 2013 ~ 0,
    .default = mil.expenditure.cow.est.flag
  ))

# 2012: calculate WMEAT 2012 as a percent of 2011 and 2013 values, apply proportions
# to COW estimates, and average the two estimates
mildata$mil.expenditure.cow[mildata$iso3c=="SDN"&mildata$year==2012] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "SDN", 2012)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="SDN"&mildata$year==2012] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "SDN", 2012)

# 2014-2017: apply WMEAT growth estimates based on 2013
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "military.expenditure.wmeat",
  "SDN", 2013, restricted = c(2014:2017)
)
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow.alt",
  col_ref = "military.expenditure.wmeat",
  "SDN", 2013, restricted = c(2014:2017)
)

# 2018-2019: apply SIPRI growth rates to COW
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "mil.expenditure.sipri",
  "SDN", 2017, restricted = c(2018:2019)
)
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow.alt",
  col_ref = "mil.expenditure.sipri",
  "SDN", 2017, restricted = c(2018:2019)
)

##### wmeat expenditure
# 1956-1962: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "SDN", 1963, restricted = c(1956:1962)
)

# 1993-1994
mildata$mil.expenditure.wmeat[mildata$iso3c=="SDN"&mildata$year==1993] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "SDN", 1993, yr.minus.1 = 1992, yr.plus.1 = 1995)
mildata$mil.expenditure.wmeat[mildata$iso3c=="SDN"&mildata$year==1994] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "SDN", 1994, yr.minus.1 = 1992, yr.plus.1 = 1995)

# 2018-2019: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "SDN", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1989
mildata$mil.expenditure.sipri[mildata$iso3c=="SDN"&mildata$year==1989] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "SDN", 1989)

# 2010-2014
mildata$mil.expenditure.sipri[mildata$iso3c=="SDN"&mildata$year==2010] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "SDN", 2010, yr.minus.1 = 2009, yr.plus.1 = 2015)
mildata$mil.expenditure.sipri[mildata$iso3c=="SDN"&mildata$year==2011] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "SDN", 2011, yr.minus.1 = 2009, yr.plus.1 = 2015)
mildata$mil.expenditure.sipri[mildata$iso3c=="SDN"&mildata$year==2012] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "SDN", 2012, yr.minus.1 = 2009, yr.plus.1 = 2015)
mildata$mil.expenditure.sipri[mildata$iso3c=="SDN"&mildata$year==2013] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "SDN", 2013, yr.minus.1 = 2009, yr.plus.1 = 2015)
mildata$mil.expenditure.sipri[mildata$iso3c=="SDN"&mildata$year==2014] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "SDN", 2014, yr.minus.1 = 2009, yr.plus.1 = 2015)

##### cow personnel
# good

##### wmeat personnel
# 1956-1962: COW and WMEAT personnel match 1963-1999, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1956:1962)&iso3c=="SDN",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "SDN", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("SDN")
# y <- milper.viewer("SDN")

##### SEN ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1960-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "SEN", 1963, restricted = c(1960:1962)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "SEN", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1960-1978: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "SEN", 1979, restricted = c(1960:1978)
)

##### cow personnel
# good

##### wmeat personnel
# 1960-1962: COW and WMEAT personnel match 1963-1994, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1960:1962)&iso3c=="SEN",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2005-06 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="SEN"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="SEN"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="SEN"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="SEN"&mildata$year==2018]

# x <- milexp.viewer("SEN")
# y <- milper.viewer("SEN")

##### SGP ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1965: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "SGP", 1966, restricted = 1965
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "SGP", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1965-1969: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "SGP", 1970, restricted = c(1965:1969)
)

##### cow personnel
# good

##### wmeat personnel
# 1965-1966: COW and WMEAT personnel match 1967-1982, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1965:1966)&iso3c=="SGP",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2005-06 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="SGP"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="SGP"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="SGP"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="SGP"&mildata$year==2018]

# x <- milexp.viewer("SGP")
# y <- milper.viewer("SGP")

##### SLB(x) ----------------------------------------------------------------------

##### cow expenditure
# 2013-2019

##### wmeat expenditure
# N/A

##### sipri expenditure
# N/A

##### cow personnel
# 2013-2019

##### wmeat personnel
# N/A

x <- milexp.viewer("SLB")
y <- milper.viewer("SLB")

##### SLE ----------------------------------------------------------------------

##### cow expenditure
# 1992: calculate SIPRI 1992 as a percent of 1991 and 1993 values, apply proportions
# to COW estimates, and average the two estimates
mildata$mil.expenditure.cow[mildata$iso3c=="SLE"&mildata$year==1992] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "SLE", 1992)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="SLE"&mildata$year==1992] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "SLE", 1992)

##### wmeat expenditure
# 1961-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "SLE", 1963, restricted = c(1961:1962)
)

# 1986
mildata$mil.expenditure.wmeat[mildata$iso3c=="SLE"&mildata$year==1986] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "SLE", 1986)

# 1989
mildata$mil.expenditure.wmeat[mildata$iso3c=="SLE"&mildata$year==1989] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "SLE", 1989)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "SLE", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1998-1999
mildata$mil.expenditure.sipri[mildata$iso3c=="SLE"&mildata$year==1998] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "SLE", 1998, yr.minus.1 = 1997, yr.plus.1 = 2000)
mildata$mil.expenditure.sipri[mildata$iso3c=="SLE"&mildata$year==1999] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "SLE", 1999, yr.minus.1 = 1997, yr.plus.1 = 2000)

##### cow personnel
# good

##### wmeat personnel
# 1961-1962: COW and WMEAT personnel match 1963-1991, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1961:1962)&iso3c=="SLE",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: COW and WMEAT personnel match 2016-2017, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(2018:2019)&iso3c=="SLE",mil.personnel.cow,mil.personnel.wmeat))

# x <- milexp.viewer("SLE")
# y <- milper.viewer("SLE")

##### SLV ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1946-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "SLV", 1963, restricted = c(1946:1962)
)

# 2018-2019: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "SLV", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1952: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "SLV", 1953, restricted = c(1946:1952)
)

# 1954-1955
mildata$mil.expenditure.sipri[mildata$iso3c=="SLV"&mildata$year==1954] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "SLV", 1954, yr.minus.1 = 1953, yr.plus.1 = 1956)
mildata$mil.expenditure.sipri[mildata$iso3c=="SLV"&mildata$year==1955] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "SLV", 1955, yr.minus.1 = 1953, yr.plus.1 = 1956)

##### cow personnel
# good

##### wmeat personnel
# 1946-1962: COW and WMEAT personnel match 1963-1977, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1946:1962)&iso3c=="SLV",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "SLV", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("SLV")
# y <- milper.viewer("SLV")

##### SMR(x) ----------------------------------------------------------------------

##### cow expenditure
# 1946-1991
# 2013-2019

##### wmeat expenditure
# N/A

##### sipri expenditure
# N/A

##### cow personnel
# 1946-1991
# 2013-2019

##### wmeat personnel
# N/A

x <- milexp.viewer("SMR")
y <- milper.viewer("SMR")

##### SOM ----------------------------------------------------------------------

##### cow expenditure
# 2005-2008: apply WMEAT growth estimates based on 2009
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "military.expenditure.wmeat",
  "SOM", 2009, restricted = c(2005:2008)
)
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow.alt",
  col_ref = "military.expenditure.wmeat",
  "SOM", 2009, restricted = c(2005:2008)
)

# 1991-1994: assume consistent growth rate between 1990 and 1995
som.alpha.1 <- (mildata$mil.expenditure.cow[mildata$iso3c=="SOM"&mildata$year==1995]/mildata$mil.expenditure.cow[mildata$iso3c=="SOM"&mildata$year==1990])^(1/5)

mildata$mil.expenditure.cow[mildata$iso3c=="SOM"&mildata$year==1991] <- mildata$mil.expenditure.cow[mildata$iso3c=="SOM"&mildata$year==1990] * som.alpha.1
mildata$mil.expenditure.cow[mildata$iso3c=="SOM"&mildata$year==1992] <- mildata$mil.expenditure.cow[mildata$iso3c=="SOM"&mildata$year==1991] * som.alpha.1
mildata$mil.expenditure.cow[mildata$iso3c=="SOM"&mildata$year==1993] <- mildata$mil.expenditure.cow[mildata$iso3c=="SOM"&mildata$year==1992] * som.alpha.1
mildata$mil.expenditure.cow[mildata$iso3c=="SOM"&mildata$year==1994] <- mildata$mil.expenditure.cow[mildata$iso3c=="SOM"&mildata$year==1993] * som.alpha.1

mildata$mil.expenditure.cow.alt[mildata$iso3c=="SOM"&mildata$year==1991] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="SOM"&mildata$year==1990] * som.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="SOM"&mildata$year==1992] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="SOM"&mildata$year==1991] * som.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="SOM"&mildata$year==1993] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="SOM"&mildata$year==1992] * som.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="SOM"&mildata$year==1994] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="SOM"&mildata$year==1993] * som.alpha.1

# 2002-2004: assume consistent growth rate between 2001 and 2005
som.alpha.2 <- (mildata$mil.expenditure.cow[mildata$iso3c=="SOM"&mildata$year==2005]/mildata$mil.expenditure.cow[mildata$iso3c=="SOM"&mildata$year==2001])^(1/4)

mildata$mil.expenditure.cow[mildata$iso3c=="SOM"&mildata$year==2002] <- mildata$mil.expenditure.cow[mildata$iso3c=="SOM"&mildata$year==2001] * som.alpha.2
mildata$mil.expenditure.cow[mildata$iso3c=="SOM"&mildata$year==2003] <- mildata$mil.expenditure.cow[mildata$iso3c=="SOM"&mildata$year==2002] * som.alpha.2
mildata$mil.expenditure.cow[mildata$iso3c=="SOM"&mildata$year==2004] <- mildata$mil.expenditure.cow[mildata$iso3c=="SOM"&mildata$year==2003] * som.alpha.2

mildata$mil.expenditure.cow.alt[mildata$iso3c=="SOM"&mildata$year==2002] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="SOM"&mildata$year==2001] * som.alpha.2
mildata$mil.expenditure.cow.alt[mildata$iso3c=="SOM"&mildata$year==2003] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="SOM"&mildata$year==2002] * som.alpha.2
mildata$mil.expenditure.cow.alt[mildata$iso3c=="SOM"&mildata$year==2004] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="SOM"&mildata$year==2003] * som.alpha.2

# 2010-2017: apply WMEAT growth estimates based on 2009
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "military.expenditure.wmeat",
  "SOM", 2009, restricted = c(2010:2017)
)
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow.alt",
  col_ref = "military.expenditure.wmeat",
  "SOM", 2009, restricted = c(2010:2017)
)

# 2018-2019: apply SIPRI growth rates to COW
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "mil.expenditure.sipri",
  "SOM", 2017, restricted = c(2018:2019)
)
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow.alt",
  col_ref = "mil.expenditure.sipri",
  "SOM", 2017, restricted = c(2018:2019)
)

##### wmeat expenditure
# 1960-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "SOM", 1963, restricted = c(1960:1962)
)

# 1985
mildata$mil.expenditure.wmeat[mildata$iso3c=="SOM"&mildata$year==1985] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "SOM", 1985)

# 1987-1988
mildata$mil.expenditure.wmeat[mildata$iso3c=="SOM"&mildata$year==1987] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "SOM", 1987, yr.minus.1 = 1986, yr.plus.1 = 1989)
mildata$mil.expenditure.wmeat[mildata$iso3c=="SOM"&mildata$year==1988] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "SOM", 1988, yr.minus.1 = 1986, yr.plus.1 = 1989)

# 1995-2001
mildata$mil.expenditure.wmeat[mildata$iso3c=="SOM"&mildata$year==1995] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW est", "SOM", 1995, yr.minus.1 = 1990, yr.plus.1 = 2005)
mildata$mil.expenditure.wmeat[mildata$iso3c=="SOM"&mildata$year==1996] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW est", "SOM", 1996, yr.minus.1 = 1990, yr.plus.1 = 2005)
mildata$mil.expenditure.wmeat[mildata$iso3c=="SOM"&mildata$year==1997] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW est", "SOM", 1997, yr.minus.1 = 1990, yr.plus.1 = 2005)
mildata$mil.expenditure.wmeat[mildata$iso3c=="SOM"&mildata$year==1998] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW est", "SOM", 1998, yr.minus.1 = 1990, yr.plus.1 = 2005)
mildata$mil.expenditure.wmeat[mildata$iso3c=="SOM"&mildata$year==1999] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW est", "SOM", 1999, yr.minus.1 = 1990, yr.plus.1 = 2005)
mildata$mil.expenditure.wmeat[mildata$iso3c=="SOM"&mildata$year==2000] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW est", "SOM", 2000, yr.minus.1 = 1990, yr.plus.1 = 2005)
mildata$mil.expenditure.wmeat[mildata$iso3c=="SOM"&mildata$year==2001] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW est", "SOM", 2001, yr.minus.1 = 1990, yr.plus.1 = 2005)

# 1991-1994: assume consistent growth rate between 1990 and 1995
som.alpha.3 <- (mildata$mil.expenditure.wmeat[mildata$iso3c=="SOM"&mildata$year==1995]/mildata$mil.expenditure.wmeat[mildata$iso3c=="SOM"&mildata$year==1990])^(1/5)

mildata$mil.expenditure.wmeat[mildata$iso3c=="SOM"&mildata$year==1991] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="SOM"&mildata$year==1990] * som.alpha.3
mildata$mil.expenditure.wmeat[mildata$iso3c=="SOM"&mildata$year==1992] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="SOM"&mildata$year==1991] * som.alpha.3
mildata$mil.expenditure.wmeat[mildata$iso3c=="SOM"&mildata$year==1993] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="SOM"&mildata$year==1992] * som.alpha.3
mildata$mil.expenditure.wmeat[mildata$iso3c=="SOM"&mildata$year==1994] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="SOM"&mildata$year==1993] * som.alpha.3

# 2002-2004: assume consistent growth rate between 2001 and 2005
som.alpha.4 <- (mildata$mil.expenditure.cow[mildata$iso3c=="SOM"&mildata$year==2005]/mildata$mil.expenditure.wmeat[mildata$iso3c=="SOM"&mildata$year==2001])^(1/4)

mildata$mil.expenditure.wmeat[mildata$iso3c=="SOM"&mildata$year==2002] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="SOM"&mildata$year==2001] * som.alpha.4
mildata$mil.expenditure.wmeat[mildata$iso3c=="SOM"&mildata$year==2003] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="SOM"&mildata$year==2002] * som.alpha.4
mildata$mil.expenditure.wmeat[mildata$iso3c=="SOM"&mildata$year==2004] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="SOM"&mildata$year==2003] * som.alpha.4

# 2018-2019: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "SOM", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1960: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "SOM", 1961, restricted = 1960
)

# 1990: apply WMEAT growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.wmeat",
  "SOM", 1989, restricted = 1990
)

# 2005-2012: apply WMEAT growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.wmeat",
  "SOM", 2013, restricted = c(2005:2012)
)

# 1995-2001
mildata$mil.expenditure.sipri[mildata$iso3c=="SOM"&mildata$year==1995] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW est", "SOM", 1995, yr.minus.1 = 1990, yr.plus.1 = 2005)
mildata$mil.expenditure.sipri[mildata$iso3c=="SOM"&mildata$year==1996] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW est", "SOM", 1996, yr.minus.1 = 1990, yr.plus.1 = 2005)
mildata$mil.expenditure.sipri[mildata$iso3c=="SOM"&mildata$year==1997] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW est", "SOM", 1997, yr.minus.1 = 1990, yr.plus.1 = 2005)
mildata$mil.expenditure.sipri[mildata$iso3c=="SOM"&mildata$year==1998] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW est", "SOM", 1998, yr.minus.1 = 1990, yr.plus.1 = 2005)
mildata$mil.expenditure.sipri[mildata$iso3c=="SOM"&mildata$year==1999] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW est", "SOM", 1999, yr.minus.1 = 1990, yr.plus.1 = 2005)
mildata$mil.expenditure.sipri[mildata$iso3c=="SOM"&mildata$year==2000] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW est", "SOM", 2000, yr.minus.1 = 1990, yr.plus.1 = 2005)
mildata$mil.expenditure.sipri[mildata$iso3c=="SOM"&mildata$year==2001] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW est", "SOM", 2001, yr.minus.1 = 1990, yr.plus.1 = 2005)

# 1991-1994: assume consistent growth rate between 1990 and 1995
som.alpha.5 <- (mildata$mil.expenditure.sipri[mildata$iso3c=="SOM"&mildata$year==1995]/mildata$mil.expenditure.sipri[mildata$iso3c=="SOM"&mildata$year==1990])^(1/5)

mildata$mil.expenditure.sipri[mildata$iso3c=="SOM"&mildata$year==1991] <- mildata$mil.expenditure.sipri[mildata$iso3c=="SOM"&mildata$year==1990] * som.alpha.5
mildata$mil.expenditure.sipri[mildata$iso3c=="SOM"&mildata$year==1992] <- mildata$mil.expenditure.sipri[mildata$iso3c=="SOM"&mildata$year==1991] * som.alpha.5
mildata$mil.expenditure.sipri[mildata$iso3c=="SOM"&mildata$year==1993] <- mildata$mil.expenditure.sipri[mildata$iso3c=="SOM"&mildata$year==1992] * som.alpha.5
mildata$mil.expenditure.sipri[mildata$iso3c=="SOM"&mildata$year==1994] <- mildata$mil.expenditure.sipri[mildata$iso3c=="SOM"&mildata$year==1993] * som.alpha.5

# 2002-2004: assume consistent growth rate between 2001 and 2005
som.alpha.6 <- (mildata$mil.expenditure.cow[mildata$iso3c=="SOM"&mildata$year==2005]/mildata$mil.expenditure.sipri[mildata$iso3c=="SOM"&mildata$year==2001])^(1/4)

mildata$mil.expenditure.sipri[mildata$iso3c=="SOM"&mildata$year==2002] <- mildata$mil.expenditure.sipri[mildata$iso3c=="SOM"&mildata$year==2001] * som.alpha.6
mildata$mil.expenditure.sipri[mildata$iso3c=="SOM"&mildata$year==2003] <- mildata$mil.expenditure.sipri[mildata$iso3c=="SOM"&mildata$year==2002] * som.alpha.6
mildata$mil.expenditure.sipri[mildata$iso3c=="SOM"&mildata$year==2004] <- mildata$mil.expenditure.sipri[mildata$iso3c=="SOM"&mildata$year==2003] * som.alpha.6

##### cow personnel
# 1994-1998: assume consistent growth
som.alpha.7 <- (mildata$mil.personnel.cow[mildata$iso3c=="SOM"&mildata$year==1999] - mildata$mil.personnel.cow[mildata$iso3c=="SOM"&mildata$year==1993]) / 6

for(y in 1994:1998){
  mildata$mil.personnel.cow[mildata$iso3c=="SOM"&mildata$year==y] <- mildata$mil.personnel.cow[mildata$iso3c=="SOM"&mildata$year==(y-1)] + som.alpha.7
}

# 2001-2004: assume consistent growth
som.alpha.8 <- (mildata$mil.personnel.cow[mildata$iso3c=="SOM"&mildata$year==2005] - mildata$mil.personnel.cow[mildata$iso3c=="SOM"&mildata$year==2000]) / 5

for(y in 2001:2004){
  mildata$mil.personnel.cow[mildata$iso3c=="SOM"&mildata$year==y] <- mildata$mil.personnel.cow[mildata$iso3c=="SOM"&mildata$year==(y-1)] + som.alpha.8
}

# 2007-2008
mildata$mil.personnel.cow[mildata$iso3c=="SOM"&mildata$year==2007] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW personnel", col_ref = "WMEAT personnel", "SOM", 2007, yr.minus.1 = 2006, yr.plus.1 = 2009)
mildata$mil.personnel.cow[mildata$iso3c=="SOM"&mildata$year==2008] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW personnel", col_ref = "WMEAT personnel", "SOM", 2008, yr.minus.1 = 2006, yr.plus.1 = 2009)

##### wmeat personnel
# 1960-1962: COW and WMEAT personnel match 1963-1990, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1960:1962)&iso3c=="SOM",mil.personnel.cow,mil.personnel.wmeat))

# 1991-1993: COW and WMEAT personnel match 1963-1990, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1991:1993)&iso3c=="SOM",mil.personnel.cow,mil.personnel.wmeat))

# 1994-2004: assume consistent growth
som.alpha.9 <- (mildata$mil.personnel.wmeat[mildata$iso3c=="SOM"&mildata$year==2005] - mildata$mil.personnel.wmeat[mildata$iso3c=="SOM"&mildata$year==1993]) / 12

for(y in 1994:2004){
  mildata$mil.personnel.wmeat[mildata$iso3c=="SOM"&mildata$year==y] <- mildata$mil.personnel.wmeat[mildata$iso3c=="SOM"&mildata$year==(y-1)] + som.alpha.9
}

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "SOM", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("SOM")
# y <- milper.viewer("SOM")

##### SRB ----------------------------------------------------------------------

##### cow expenditure
# 1993: assume consistent growth rate between 1992 and 1994
srb.alpha.1 <- (mildata$mil.expenditure.cow[mildata$iso3c=="SRB"&mildata$year==1994]/mildata$mil.expenditure.cow[mildata$iso3c=="SRB"&mildata$year==1992])^(1/2)

mildata$mil.expenditure.cow[mildata$iso3c=="SRB"&mildata$year==1993] <- mildata$mil.expenditure.cow[mildata$iso3c=="SRB"&mildata$year==1992] * srb.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="SRB"&mildata$year==1993] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="SRB"&mildata$year==1992] * srb.alpha.1

# 2013-2019 (non-alt): apply IISS growth estimates based on 2012
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "defense.spending.iiss",
  "SRB", 2012, restricted = c(2013:2019)
)

## remove estimate flag for 2013-2019
mildata <- mildata %>%
  dplyr::mutate(mil.expenditure.cow.est.flag  = dplyr::case_when(
    iso3c == "SRB" & year %in% c(2013:2019) ~ 0,
    .default = mil.expenditure.cow.est.flag
  ))

##### wmeat expenditure
# 1992; 1994: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "SRB", 1995, restricted = c(1992:1994)
)

# 1993: assume consistent growth rate between 1992 and 1994
srb.alpha.2 <- (mildata$mil.expenditure.wmeat[mildata$iso3c=="SRB"&mildata$year==1994]/mildata$mil.expenditure.wmeat[mildata$iso3c=="SRB"&mildata$year==1992])^(1/2)

mildata$mil.expenditure.wmeat[mildata$iso3c=="SRB"&mildata$year==1993] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="SRB"&mildata$year==1992] * srb.alpha.2

# 2018-2019: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "SRB", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1992; 1994-1996: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "SRB", 1997, restricted = c(1992:1996)
)

# 1993: assume consistent growth rate between 1992 and 1994
srb.alpha.3 <- (mildata$mil.expenditure.sipri[mildata$iso3c=="SRB"&mildata$year==1994]/mildata$mil.expenditure.sipri[mildata$iso3c=="SRB"&mildata$year==1992])^(1/2)

mildata$mil.expenditure.sipri[mildata$iso3c=="SRB"&mildata$year==1993] <- mildata$mil.expenditure.sipri[mildata$iso3c=="SRB"&mildata$year==1992] * srb.alpha.3

##### cow personnel
# good

##### wmeat personnel
# 2018-2019: 2013-14 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="SRB"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="SRB"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="SRB"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="SRB"&mildata$year==2018]

# x <- milexp.viewer("SRB")
# y <- milper.viewer("SRB")

##### SSD ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 2018-2019: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "SSD", 2017, restricted = c(2018:2019)
)

# 2011: reestimate
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "SSD", 2012, restricted = 2011
)

##### sipri expenditure
# good

##### cow personnel
# 2011: apply WMEAT personnel growth rates to COW personnel
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.cow",
  col_ref = "mil.personnel.wmeat",
  "SSD", 2012, restricted = 2011
)

##### wmeat personnel
# 2018-2019: 2015-16 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="SSD"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="SSD"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="SSD"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="SSD"&mildata$year==2018]

# x <- milexp.viewer("SSD")
# y <- milper.viewer("SSD")

##### STP(x) ----------------------------------------------------------------------

##### cow expenditure
# 1981-1993
# 2000-2019

##### wmeat expenditure
# 1975
# 1981-1994
# 2010-2019

##### sipri expenditure
# N/A

##### cow personnel
# 2000-2019

##### wmeat personnel
# 1975
# 1979-1982
# 2010-2019

x <- milexp.viewer("STP")
y <- milper.viewer("STP")

##### SUR ----------------------------------------------------------------------

##### cow expenditure
# 1987: assume consistent growth rate between 1986 and 1988
sur.alpha.1 <- (mildata$mil.expenditure.cow[mildata$iso3c=="SUR"&mildata$year==1988]/mildata$mil.expenditure.cow[mildata$iso3c=="SUR"&mildata$year==1986])^(1/2)

mildata$mil.expenditure.cow[mildata$iso3c=="SUR"&mildata$year==1987] <- mildata$mil.expenditure.cow[mildata$iso3c=="SUR"&mildata$year==1986] * sur.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="SUR"&mildata$year==1987] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="SUR"&mildata$year==1986] * sur.alpha.1

# 2012-2013 (non-alt): apply IISS growth estimates based on 2011
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "defense.spending.iiss",
  "SUR", 2011, restricted = c(2012:2013)
)

## remove estimate flag for 2012-2013
mildata <- mildata %>%
  dplyr::mutate(mil.expenditure.cow.est.flag  = dplyr::case_when(
    iso3c == "SUR" & year %in% c(2012:2013) ~ 0,
    .default = mil.expenditure.cow.est.flag
  ))

# 2014-2017: apply WMEAT growth estimates based on 2013
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "military.expenditure.wmeat",
  "SUR", 2013, restricted = c(2014:2017)
)
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow.alt",
  col_ref = "military.expenditure.wmeat",
  "SUR", 2013, restricted = c(2014:2017)
)

# 1978-1981
mildata <- mil_estimator_no_data_func(
  mildata,"SUR",
  metric = "mil.expenditure.cow",
  1981,1978
)
mildata <- mil_estimator_no_data_func(
  mildata,"SUR",
  metric = "mil.expenditure.cow.alt",
  1981,1978
)

# 1975-1977: reestimate
mildata <- mil_estimator_no_data_func(
  mildata,"SUR",
  metric = "mil.expenditure.cow",
  1977,1975
)
mildata <- mil_estimator_no_data_func(
  mildata,"SUR",
  metric = "mil.expenditure.cow.alt",
  1977,1975
)

# 2018-2019
mildata <- mil_estimator_no_data_func(
  mildata,"SUR",
  metric = "mil.expenditure.cow",
  2018,2019
)
mildata <- mil_estimator_no_data_func(
  mildata,"SUR",
  metric = "mil.expenditure.cow.alt",
  2018,2019
)

##### wmeat expenditure
# 1982: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "SUR", 1983, restricted = 1982
)

# 1975-1981
mildata <- mil_estimator_no_data_func(
  mildata,"SUR",
  metric = "mil.expenditure.wmeat",
  1981,1975
)

# 1987: assume consistent growth rate between 1986 and 1988
sur.alpha.2 <- (mildata$mil.expenditure.wmeat[mildata$iso3c=="SUR"&mildata$year==1988]/mildata$mil.expenditure.wmeat[mildata$iso3c=="SUR"&mildata$year==1986])^(1/2)

mildata$mil.expenditure.wmeat[mildata$iso3c=="SUR"&mildata$year==1987] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="SUR"&mildata$year==1986] * sur.alpha.2

# 2018-2019
mildata <- mil_estimator_no_data_func(
  mildata,"SUR",
  metric = "mil.expenditure.wmeat",
  2018,2019
)

##### sipri expenditure
# N/A

##### cow personnel
# good

##### wmeat personnel
# 2018-2019: 2004-05 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="SUR"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="SUR"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="SUR"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="SUR"&mildata$year==2018]

# x <- milexp.viewer("SUR")
# y <- milper.viewer("SUR")

##### SVK ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 2018-2019: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "SVK", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# good

##### cow personnel
# good

##### wmeat personnel
# 2018-2019: 2013-14 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="SVK"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="SVK"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="SVK"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="SVK"&mildata$year==2018]

# x <- milexp.viewer("SVK")
# y <- milper.viewer("SVK")

##### SVN ----------------------------------------------------------------------

##### cow expenditure
# 1993: estimate COW 1993 as equidistant point based on WMEAT 1992-1994 values
mildata$mil.expenditure.cow[mildata$iso3c=="SVN"&mildata$year==1993] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "SVN", 1993)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="SVN"&mildata$year==1993] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "SVN", 1993)

##### wmeat expenditure
# 2018-2019: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "SVN", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# good

##### cow personnel
# good

##### wmeat personnel
# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "SVN", 2017, restricted = c(2018:2019)
)

x <- milexp.viewer("SVN")
# y <- milper.viewer("SVN")

##### SWE ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1946-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "SWE", 1963, restricted = c(1946:1962)
)

# 2018-2019: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "SWE", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1959: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "SWE", 1960, restricted = c(1946:1959)
)

##### cow personnel
# 1946-1956: assume consistent growth rate between 1945 and 1957
swe.alpha <- (mildata$mil.personnel.cow[mildata$iso3c=="SWE"&mildata$year==1957]/mildata$mil.personnel.cow[mildata$iso3c=="SWE"&mildata$year==1945])^(1/12)

mildata$mil.personnel.cow[mildata$iso3c=="SWE"&mildata$year==1946] <- mildata$mil.personnel.cow[mildata$iso3c=="SWE"&mildata$year==1945] * swe.alpha
mildata$mil.personnel.cow[mildata$iso3c=="SWE"&mildata$year==1947] <- mildata$mil.personnel.cow[mildata$iso3c=="SWE"&mildata$year==1946] * swe.alpha
mildata$mil.personnel.cow[mildata$iso3c=="SWE"&mildata$year==1948] <- mildata$mil.personnel.cow[mildata$iso3c=="SWE"&mildata$year==1947] * swe.alpha
mildata$mil.personnel.cow[mildata$iso3c=="SWE"&mildata$year==1949] <- mildata$mil.personnel.cow[mildata$iso3c=="SWE"&mildata$year==1948] * swe.alpha
mildata$mil.personnel.cow[mildata$iso3c=="SWE"&mildata$year==1950] <- mildata$mil.personnel.cow[mildata$iso3c=="SWE"&mildata$year==1949] * swe.alpha
mildata$mil.personnel.cow[mildata$iso3c=="SWE"&mildata$year==1951] <- mildata$mil.personnel.cow[mildata$iso3c=="SWE"&mildata$year==1950] * swe.alpha
mildata$mil.personnel.cow[mildata$iso3c=="SWE"&mildata$year==1952] <- mildata$mil.personnel.cow[mildata$iso3c=="SWE"&mildata$year==1951] * swe.alpha
mildata$mil.personnel.cow[mildata$iso3c=="SWE"&mildata$year==1953] <- mildata$mil.personnel.cow[mildata$iso3c=="SWE"&mildata$year==1952] * swe.alpha
mildata$mil.personnel.cow[mildata$iso3c=="SWE"&mildata$year==1954] <- mildata$mil.personnel.cow[mildata$iso3c=="SWE"&mildata$year==1953] * swe.alpha
mildata$mil.personnel.cow[mildata$iso3c=="SWE"&mildata$year==1955] <- mildata$mil.personnel.cow[mildata$iso3c=="SWE"&mildata$year==1954] * swe.alpha
mildata$mil.personnel.cow[mildata$iso3c=="SWE"&mildata$year==1956] <- mildata$mil.personnel.cow[mildata$iso3c=="SWE"&mildata$year==1955] * swe.alpha
mildata$mil.personnel.cow[mildata$iso3c=="SWE"&mildata$year==1957] <- mildata$mil.personnel.cow[mildata$iso3c=="SWE"&mildata$year==1956] * swe.alpha

##### wmeat personnel
# 1945-1956: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "SWE", 1957, restricted = c(1945:1956)
)

# 1957-1962: COW and WMEAT personnel match 1963-1976, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1957:1962)&iso3c=="SWE",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="SWE"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="SWE"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="SWE"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="SWE"&mildata$year==2018]

# x <- milexp.viewer("SWE")
# y <- milper.viewer("SWE")

##### SWZ ----------------------------------------------------------------------

##### cow expenditure
# 2000: it appears the COW 2000 military expenditure estimate has an extra 0
mildata$mil.expenditure.cow[mildata$iso3c=="SWZ"&mildata$year==2000] <- mildata$mil.expenditure.cow[mildata$iso3c=="SWZ"&mildata$year==2000] / 10
mildata$mil.expenditure.cow.alt[mildata$iso3c=="SWZ"&mildata$year==2000] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="SWZ"&mildata$year==2000] / 10

# 1992: estimate COW 1992 as equidistant point based on WMEAT 1991-1993 values
mildata$mil.expenditure.cow[mildata$iso3c=="SWZ"&mildata$year==1992] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "SWZ", 1992)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="SWZ"&mildata$year==1992] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "SWZ", 1992)

# 2002-2019: apply SIPRI growth rates to COW
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "mil.expenditure.sipri",
  "SWZ", 2001, restricted = c(2002:2019)
)
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow.alt",
  col_ref = "mil.expenditure.sipri",
  "SWZ", 2001, restricted = c(2002:2019)
)

# 1968-1973: recalculate
mildata <- mil_estimator_no_data_func(
  mildata,"SWZ",
  metric = "mil.expenditure.cow",
  1973,1968
)
mildata <- mil_estimator_no_data_func(
  mildata,"SWZ",
  metric = "mil.expenditure.cow.alt",
  1973,1968
)

##### wmeat expenditure
# 1973: recalculate
mildata <- mil_estimator_no_data_func(
  mildata,"SWZ",
  metric = "mil.expenditure.wmeat",
  1973,1973
)

# 1968-1972
mildata <- mil_estimator_no_data_func(
  mildata,"SWZ",
  metric = "mil.expenditure.wmeat",
  1972,1968
)

# 2018-2019: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "SWZ", 2017, restricted = c(2018:2019)
)

##### sipri expenditure

# 1974-1976: apply WMEAT growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.wmeat",
  "SWZ", 1977, restricted = c(1974:1976)
)

# 1968-1973
mildata <- mil_estimator_no_data_func(
  mildata,"SWZ",
  metric = "mil.expenditure.sipri",
  1973,1968
)

##### cow personnel
# 2000-2017: COW and WMEAT personnel match 1963-1999, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.cow = ifelse(year %in% c(2000:2017)&iso3c=="SWZ",mil.personnel.wmeat,mil.personnel.cow))

# 2018-2019
mildata <- mil_estimator_no_data_func(
  mildata,"SWZ",
  metric = "mil.personnel.cow",
  2018,2019
)

##### wmeat personnel
# 1968-1972: COW and WMEAT personnel match 1963-1999, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1968:1972)&iso3c=="SWZ",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019
mildata <- mil_estimator_no_data_func(
  mildata,"SWZ",
  metric = "mil.personnel.wmeat",
  2018,2019
)

# x <- milexp.viewer("SWZ")
# y <- milper.viewer("SWZ")

##### SYC(x) ----------------------------------------------------------------------

##### cow expenditure
# 1978-1979
# 1988
# 1990
# 2011
# 2013-2019

##### wmeat expenditure
# N/A

##### sipri expenditure
# 1976-1984

##### cow personnel
# 1995-1998

##### wmeat personnel
# N/A

x <- milexp.viewer("SYC")
y <- milper.viewer("SYC")

##### SYR ----------------------------------------------------------------------

##### cow expenditure
# 1959-1960
mildata$mil.expenditure.cow[mildata$iso3c=="SYR"&mildata$year==1959] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "SYR", 1959, yr.minus.1 = 1958, yr.plus.1 = 1961)
mildata$mil.expenditure.cow[mildata$iso3c=="SYR"&mildata$year==1960] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "SYR", 1960, yr.minus.1 = 1958, yr.plus.1 = 1961)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="SYR"&mildata$year==1959] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "SYR", 1959, yr.minus.1 = 1958, yr.plus.1 = 1961)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="SYR"&mildata$year==1960] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "SYR", 1960, yr.minus.1 = 1958, yr.plus.1 = 1961)

# 2011-2017: apply WMEAT growth estimates based on 2010
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "mil.expenditure.wmeat",
  "SYR", 2010, restricted = c(2011:2017)
)
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow.alt",
  col_ref = "mil.expenditure.wmeat",
  "SYR", 2010, restricted = c(2011:2017)
)

# 2018-2019
mildata <- mil_estimator_no_data_func(
  mildata,"SYR",
  metric = "mil.expenditure.cow",
  2018,2019
)
mildata <- mil_estimator_no_data_func(
  mildata,"SYR",
  metric = "mil.expenditure.cow.alt",
  2018,2019
)

##### wmeat expenditure
# 1946-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "SYR", 1963, restricted = c(1946:1962)
)

# 1988
mildata$mil.expenditure.wmeat[mildata$iso3c=="SYR"&mildata$year==1988] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "SYR", 1988)

# 1991-1994
mildata$mil.expenditure.wmeat[mildata$iso3c=="SYR"&mildata$year==1991] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "SYR", 1991, yr.minus.1 = 1990, yr.plus.1 = 1995)
mildata$mil.expenditure.wmeat[mildata$iso3c=="SYR"&mildata$year==1992] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "SYR", 1992, yr.minus.1 = 1990, yr.plus.1 = 1995)
mildata$mil.expenditure.wmeat[mildata$iso3c=="SYR"&mildata$year==1993] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "SYR", 1993, yr.minus.1 = 1990, yr.plus.1 = 1995)
mildata$mil.expenditure.wmeat[mildata$iso3c=="SYR"&mildata$year==1994] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "SYR", 1994, yr.minus.1 = 1990, yr.plus.1 = 1995)

# 2018-2019
mildata <- mil_estimator_no_data_func(
  mildata,"SYR",
  metric = "mil.expenditure.wmeat",
  2018,2019
)

##### sipri expenditure
# 1946-1955: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "SYR", 1956, restricted = c(1946:1955)
)

# 1963
mildata$mil.expenditure.sipri[mildata$iso3c=="SYR"&mildata$year==1963] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "SYR", 1963)

# 2012-2017: apply WMEAT growth estimates based on 2011
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.wmeat",
  "SYR", 2011, restricted = c(2012:2017)
)

# 2018-2019
mildata <- mil_estimator_no_data_func(
  mildata,"SYR",
  metric = "mil.expenditure.sipri",
  2018,2019
)

##### cow personnel
# 1959-1960: assume consistent growth rate between 1958 and 1961
syr.alpha <- (mildata$mil.personnel.cow[mildata$iso3c=="SYR"&mildata$year==1961]/mildata$mil.personnel.cow[mildata$iso3c=="SYR"&mildata$year==1958])^(1/3)

mildata$mil.personnel.cow[mildata$iso3c=="SYR"&mildata$year==1959] <- mildata$mil.personnel.cow[mildata$iso3c=="SYR"&mildata$year==1958] * syr.alpha
mildata$mil.personnel.cow[mildata$iso3c=="SYR"&mildata$year==1960] <- mildata$mil.personnel.cow[mildata$iso3c=="SYR"&mildata$year==1959] * syr.alpha

##### wmeat personnel
# 1946-1962
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "SYR", 1963, restricted = c(1946:1962)
)

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "SYR", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("SYR")
# y <- milper.viewer("SYR")

##### TCD ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1960-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "TCD", 1963, restricted = c(1960:1962)
)

# 1980-1982
mildata$mil.expenditure.wmeat[mildata$iso3c=="TCD"&mildata$year==1980] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "TCD", 1980, yr.minus.1 = 1979, yr.plus.1 = 1983)
mildata$mil.expenditure.wmeat[mildata$iso3c=="TCD"&mildata$year==1981] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "TCD", 1981, yr.minus.1 = 1979, yr.plus.1 = 1983)
mildata$mil.expenditure.wmeat[mildata$iso3c=="TCD"&mildata$year==1982] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "TCD", 1982, yr.minus.1 = 1979, yr.plus.1 = 1983)

# 1990
mildata$mil.expenditure.wmeat[mildata$iso3c=="TCD"&mildata$year==1990] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "TCD", 1990)

# 2018-2019: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "TCD", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1960-1982: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "TCD", 1983, restricted = c(1960:1982)
)

# 1991
mildata$mil.expenditure.sipri[mildata$iso3c=="TCD"&mildata$year==1991] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "TCD", 1991)

# 2012
mildata$mil.expenditure.sipri[mildata$iso3c=="TCD"&mildata$year==2012] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "TCD", 2012)

##### cow personnel
# good

##### wmeat personnel
# 1960-1962: COW and WMEAT personnel match 1963-1977, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1960:1962)&iso3c=="TCD",mil.personnel.cow,mil.personnel.wmeat))

# 1978-1982: COW and WMEAT personnel match 1963-1991, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1978:1982)&iso3c=="TCD",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="TCD"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="TCD"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="TCD"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="TCD"&mildata$year==2018]

# x <- milexp.viewer("TCD")
# y <- milper.viewer("TCD")

##### TGO ----------------------------------------------------------------------

##### cow expenditure
# 1992: calculate SIPRI 1992 as a percent of 1991 and 1993 values, apply proportions
# to COW estimates, and average the two estimates
mildata$mil.expenditure.cow[mildata$iso3c=="TGO"&mildata$year==1992] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "TGO", 1992)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="TGO"&mildata$year==1992] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "TGO", 1992)

##### wmeat expenditure
# 1960-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "TGO", 1963, restricted = c(1960:1962)
)

# 1988
mildata$mil.expenditure.wmeat[mildata$iso3c=="TGO"&mildata$year==1988] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "TGO", 1988)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "TGO", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1960: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "TGO", 1961, restricted = 1960
)

# 1996-2002
mildata$mil.expenditure.sipri[mildata$iso3c=="TGO"&mildata$year==1996] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "TGO", 1996, yr.minus.1 = 1995, yr.plus.1 = 2003)
mildata$mil.expenditure.sipri[mildata$iso3c=="TGO"&mildata$year==1997] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "TGO", 1997, yr.minus.1 = 1995, yr.plus.1 = 2003)
mildata$mil.expenditure.sipri[mildata$iso3c=="TGO"&mildata$year==1998] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "TGO", 1998, yr.minus.1 = 1995, yr.plus.1 = 2003)
mildata$mil.expenditure.sipri[mildata$iso3c=="TGO"&mildata$year==1999] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "TGO", 1999, yr.minus.1 = 1995, yr.plus.1 = 2003)
mildata$mil.expenditure.sipri[mildata$iso3c=="TGO"&mildata$year==2000] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "TGO", 2000, yr.minus.1 = 1995, yr.plus.1 = 2003)
mildata$mil.expenditure.sipri[mildata$iso3c=="TGO"&mildata$year==2001] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "TGO", 2001, yr.minus.1 = 1995, yr.plus.1 = 2003)
mildata$mil.expenditure.sipri[mildata$iso3c=="TGO"&mildata$year==2002] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "TGO", 2002, yr.minus.1 = 1995, yr.plus.1 = 2003)

# 2006-2007
mildata$mil.expenditure.sipri[mildata$iso3c=="TGO"&mildata$year==2006] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "TGO", 2006, yr.minus.1 = 2005, yr.plus.1 = 2008)
mildata$mil.expenditure.sipri[mildata$iso3c=="TGO"&mildata$year==2007] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "TGO", 2007, yr.minus.1 = 2005, yr.plus.1 = 2008)

##### cow personnel
# good

##### wmeat personnel
# 1960-1962: COW and WMEAT personnel match 1963-1991, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1960:1962)&iso3c=="TGO",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2010-11 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="TGO"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="TGO"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="TGO"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="TGO"&mildata$year==2018]

# x <- milexp.viewer("TGO")
# y <- milper.viewer("TGO")

##### THA ----------------------------------------------------------------------

# TODO: check COW 2001

##### cow expenditure
# good

##### wmeat expenditure
# 1946-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "THA", 1963, restricted = c(1946:1962)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "THA", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1956: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "THA", 1957, restricted = c(1946:1956)
)

##### cow personnel
# good

##### wmeat personnel
# 1946-1962: COW and WMEAT personnel match 1963-1982, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1946:1962)&iso3c=="THA",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2013-14 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="THA"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="THA"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="THA"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="THA"&mildata$year==2018]

# x <- milexp.viewer("THA")
# y <- milper.viewer("THA")

##### TJK ----------------------------------------------------------------------

##### cow expenditure
# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"TJK",
  metric = "mil.expenditure.cow",
  1991,1991
)
mildata <- mil_estimator_no_data_func(
  mildata,"TJK",
  metric = "mil.expenditure.cow.alt",
  1991,1991
)

##### wmeat expenditure
# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"TJK",
  metric = "mil.expenditure.wmeat",
  1991,1991
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "TJK", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"TJK",
  metric = "mil.expenditure.sipri",
  1991,1991
)

# 2005-2007
mildata$mil.expenditure.sipri[mildata$iso3c=="TJK"&mildata$year==2005] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "TJK", 2005, yr.minus.1 = 2004, yr.plus.1 = 2008)
mildata$mil.expenditure.sipri[mildata$iso3c=="TJK"&mildata$year==2006] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "TJK", 2006, yr.minus.1 = 2004, yr.plus.1 = 2008)
mildata$mil.expenditure.sipri[mildata$iso3c=="TJK"&mildata$year==2007] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "TJK", 2007, yr.minus.1 = 2004, yr.plus.1 = 2008)

# 2013
mildata$mil.expenditure.sipri[mildata$iso3c=="TJK"&mildata$year==2013] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "IISS", "TJK", 2013)

# 2016-2019: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "TJK", 2015, restricted = c(2016:2019)
)

##### cow personnel
# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"TJK",
  metric = "mil.personnel.cow",
  1991,1991
)

##### wmeat personnel
# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"TJK",
  metric = "mil.personnel.wmeat",
  1991,1991
)

# 1995: COW and WMEAT personnel match 1992-1999, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year==1995&iso3c=="TJK",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2014-15 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="TJK"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="TJK"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="TJK"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="TJK"&mildata$year==2018]

# x <- milexp.viewer("TJK")
# y <- milper.viewer("TJK")

##### TKM ----------------------------------------------------------------------

##### cow expenditure
# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"TKM",
  metric = "mil.expenditure.cow",
  1991,1991
)
mildata <- mil_estimator_no_data_func(
  mildata,"TKM",
  metric = "mil.expenditure.cow.alt",
  1991,1991
)

# 2009
mildata$mil.expenditure.cow[mildata$iso3c=="TKM"&mildata$year==2009] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "TKM", 2009)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="TKM"&mildata$year==2009] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "TKM", 2009)

# 2011: calculate WMEAT 2011 as a percent of 2010 and 2012 values, apply proportions
# to COW estimates, and average the two estimates
mildata$mil.expenditure.cow[mildata$iso3c=="TKM"&mildata$year==2011] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "TKM", 2011)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="TKM"&mildata$year==2011] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "TKM", 2011)

# 2013: apply IISS growth estimates based on 2012
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "defense.spending.iiss",
  "TKM", 2012, restricted = 2013
)

## remove estimate flag for 2013
mildata <- mildata %>%
  dplyr::mutate(mil.expenditure.cow.est.flag  = dplyr::case_when(
    iso3c == "TKM" & year == 2013 ~ 0,
    .default = mil.expenditure.cow.est.flag
  ))

# 2014-2017: apply WMEAT growth estimates based on 2012
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "mil.expenditure.wmeat",
  "TKM", 2013, restricted = c(2014:2017)
)
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow.alt",
  col_ref = "mil.expenditure.wmeat",
  "TKM", 2013, restricted = c(2014:2017)
)

# 2018-2019
mildata <- mil_estimator_no_data_func(
  mildata,"TKM",
  metric = "mil.expenditure.cow",
  2018,2019
)
mildata <- mil_estimator_no_data_func(
  mildata,"TKM",
  metric = "mil.expenditure.cow.alt",
  2018,2019
)

##### wmeat expenditure
# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"TKM",
  metric = "mil.expenditure.wmeat",
  1991,1991
)

# 2018-2019
mildata <- mil_estimator_no_data_func(
  mildata,"TKM",
  metric = "mil.expenditure.wmeat",
  2018,2019
)

##### sipri expenditure
# 1992-1993: apply WMEAT growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.wmeat",
  "TKM", 1994, restricted = c(1992:1993)
)

# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"TKM",
  metric = "mil.expenditure.sipri",
  1991,1991
)

# 2000-2017: apply WMEAT growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.wmeat",
  "TKM", 1999, restricted = c(2000:2017)
)

# 2018-2019
mildata <- mil_estimator_no_data_func(
  mildata,"TKM",
  metric = "mil.expenditure.sipri",
  2018,2019
)

##### cow personnel
# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"TKM",
  metric = "mil.personnel.cow",
  1991,1991
)

##### wmeat personnel
# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"TKM",
  metric = "mil.personnel.wmeat",
  1991,1991
)

# 2018-2019: 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="TKM"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="TKM"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="TKM"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="TKM"&mildata$year==2018]

# x <- milexp.viewer("TKM")
# y <- milper.viewer("TKM")

##### TLS ----------------------------------------------------------------------

##### cow expenditure
# 2002-2009: apply WMEAT growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "mil.expenditure.wmeat",
  "TLS", 2010, restricted = c(2002:2009)
)
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow.alt",
  col_ref = "mil.expenditure.wmeat",
  "TLS", 2010, restricted = c(2002:2009)
)

##### wmeat expenditure
# 2018-2019: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "TLS", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 2002-2004: apply WMEAT growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.wmeat",
  "TLS", 2005, restricted = c(2002:2004)
)

##### cow personnel
# 1999-2004: COW and WMEAT personnel match 2005-2012, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.cow = ifelse(year %in% c(1999:2004)&iso3c=="TLS",mil.personnel.wmeat,mil.personnel.cow))

##### wmeat personnel
# 2018-2019: 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="TLS"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="TLS"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="TLS"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="TLS"&mildata$year==2018]

# x <- milexp.viewer("TLS")
# y <- milper.viewer("TLS")

##### TON(x) ----------------------------------------------------------------------

##### cow expenditure
# 1999-2001
# 2013-2019

##### wmeat expenditure
# N/A

##### sipri expenditure
# N/A

##### cow personnel
# 2013-2019

##### wmeat personnel
# N/A

x <- milexp.viewer("TON")
y <- milper.viewer("TON")

##### TTO ----------------------------------------------------------------------

##### cow expenditure
# 1987-1988: assume consistent growth rate between 1986 and 1989
tto.alpha.1 <- (mildata$mil.expenditure.cow[mildata$iso3c=="TTO"&mildata$year==1989]/mildata$mil.expenditure.cow[mildata$iso3c=="TTO"&mildata$year==1986])^(1/3)

mildata$mil.expenditure.cow[mildata$iso3c=="TTO"&mildata$year==1987] <- mildata$mil.expenditure.cow[mildata$iso3c=="TTO"&mildata$year==1986] * tto.alpha.1
mildata$mil.expenditure.cow[mildata$iso3c=="TTO"&mildata$year==1988] <- mildata$mil.expenditure.cow[mildata$iso3c=="TTO"&mildata$year==1987] * tto.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="TTO"&mildata$year==1987] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="TTO"&mildata$year==1986] * tto.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="TTO"&mildata$year==1988] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="TTO"&mildata$year==1987] * tto.alpha.1

# 1990: assume consistent growth rate between 1989 and 1991
tto.alpha.2 <- (mildata$mil.expenditure.cow[mildata$iso3c=="TTO"&mildata$year==1991]/mildata$mil.expenditure.cow[mildata$iso3c=="TTO"&mildata$year==1989])^(1/2)

mildata$mil.expenditure.cow[mildata$iso3c=="TTO"&mildata$year==1990] <- mildata$mil.expenditure.cow[mildata$iso3c=="TTO"&mildata$year==1989] * tto.alpha.2
mildata$mil.expenditure.cow.alt[mildata$iso3c=="TTO"&mildata$year==1990] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="TTO"&mildata$year==1989] * tto.alpha.2

# 1993: calculate WMEAT 1993 as a percent of 1992 and 1994 values, apply proportions
# to COW estimates, and average the two estimates
mildata$mil.expenditure.cow[mildata$iso3c=="TTO"&mildata$year==1993] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "TTO", 1993)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="TTO"&mildata$year==1993] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "TTO", 1993)

# 1962: reestimate
mildata <- mil_estimator_no_data_func(
  mildata,"TTO",
  metric = "mil.expenditure.cow",
  1962,1962
)
mildata <- mil_estimator_no_data_func(
  mildata,"TTO",
  metric = "mil.expenditure.cow.alt",
  1962,1962
)

##### wmeat expenditure
# 1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "TTO", 1963, restricted = 1962
)

# 1985-1986: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "TTO", 1984, restricted = c(1985:1986)
)

# 1991: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "TTO", 1992, restricted = 1991
)

# 1987-1988: assume consistent growth rate between 1986 and 1989
tto.alpha.3 <- (mildata$mil.expenditure.wmeat[mildata$iso3c=="TTO"&mildata$year==1989]/mildata$mil.expenditure.wmeat[mildata$iso3c=="TTO"&mildata$year==1986])^(1/3)

mildata$mil.expenditure.wmeat[mildata$iso3c=="TTO"&mildata$year==1987] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="TTO"&mildata$year==1986] * tto.alpha.3
mildata$mil.expenditure.wmeat[mildata$iso3c=="TTO"&mildata$year==1988] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="TTO"&mildata$year==1987] * tto.alpha.3

# 1990: assume consistent growth rate between 1989 and 1991
tto.alpha.4 <- (mildata$mil.expenditure.wmeat[mildata$iso3c=="TTO"&mildata$year==1991]/mildata$mil.expenditure.wmeat[mildata$iso3c=="TTO"&mildata$year==1989])^(1/2)

mildata$mil.expenditure.wmeat[mildata$iso3c=="TTO"&mildata$year==1990] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="TTO"&mildata$year==1989] * tto.alpha.4

# 2018-2019: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "TTO", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1962-1967: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "TTO", 1968, restricted = c(1962:1967)
)

# 1982-1986: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "TTO", 1981, restricted = c(1982:1986)
)

# 1991-1992: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "TTO", 1993, restricted = c(1991:1992)
)

# 1989
mildata$mil.expenditure.sipri[mildata$iso3c=="TTO"&mildata$year==1989] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT est", "TTO", 1989, yr.minus.1 = 1986, yr.plus.1 = 1991)

# 1990: assume consistent growth rate between 1989 and 1991
tto.alpha.6 <- (mildata$mil.expenditure.sipri[mildata$iso3c=="TTO"&mildata$year==1991]/mildata$mil.expenditure.sipri[mildata$iso3c=="TTO"&mildata$year==1989])^(1/2)

mildata$mil.expenditure.sipri[mildata$iso3c=="TTO"&mildata$year==1990] <- mildata$mil.expenditure.sipri[mildata$iso3c=="TTO"&mildata$year==1989] * tto.alpha.6

# 1987-1988: assume consistent growth rate between 1986 and 1989
tto.alpha.5 <- (mildata$mil.expenditure.sipri[mildata$iso3c=="TTO"&mildata$year==1989]/mildata$mil.expenditure.sipri[mildata$iso3c=="TTO"&mildata$year==1986])^(1/3)

mildata$mil.expenditure.sipri[mildata$iso3c=="TTO"&mildata$year==1987] <- mildata$mil.expenditure.sipri[mildata$iso3c=="TTO"&mildata$year==1986] * tto.alpha.5
mildata$mil.expenditure.sipri[mildata$iso3c=="TTO"&mildata$year==1988] <- mildata$mil.expenditure.sipri[mildata$iso3c=="TTO"&mildata$year==1987] * tto.alpha.5

# 1995-2000
mildata$mil.expenditure.sipri[mildata$iso3c=="TTO"&mildata$year==1995] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "TTO", 1995, yr.minus.1 = 1994, yr.plus.1 = 2001)
mildata$mil.expenditure.sipri[mildata$iso3c=="TTO"&mildata$year==1996] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "TTO", 1996, yr.minus.1 = 1994, yr.plus.1 = 2001)
mildata$mil.expenditure.sipri[mildata$iso3c=="TTO"&mildata$year==1997] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "TTO", 1997, yr.minus.1 = 1994, yr.plus.1 = 2001)
mildata$mil.expenditure.sipri[mildata$iso3c=="TTO"&mildata$year==1998] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "TTO", 1998, yr.minus.1 = 1994, yr.plus.1 = 2001)
mildata$mil.expenditure.sipri[mildata$iso3c=="TTO"&mildata$year==1999] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "TTO", 1999, yr.minus.1 = 1994, yr.plus.1 = 2001)
mildata$mil.expenditure.sipri[mildata$iso3c=="TTO"&mildata$year==2000] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "TTO", 2000, yr.minus.1 = 1994, yr.plus.1 = 2001)

##### cow personnel
# good

##### wmeat personnel
# 1962-1963: COW and WMEAT personnel match 1964-1970, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1962:1963)&iso3c=="TTO",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: COW and WMEAT personnel match 2009-2017, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(2018:2019)&iso3c=="TTO",mil.personnel.cow,mil.personnel.wmeat))

# x <- milexp.viewer("TTO")
# y <- milper.viewer("TTO")

##### TUN ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1956-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "TUN", 1963, restricted = c(1956:1962)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "TUN", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# good

##### cow personnel
# good

##### wmeat personnel
# 1956-1962: COW and WMEAT personnel match 1963-2003, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1956:1962)&iso3c=="TUN",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "TUN", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("TUN")
# y <- milper.viewer("TUN")

##### TUR ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1946-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "TUR", 1963, restricted = c(1946:1962)
)

# 2018-2019: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "TUR", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1948: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "TUR", 1949, restricted = c(1946:1948)
)

##### cow personnel
# good

##### wmeat personnel
# 1945-1962: COW and WMEAT personnel match 1963-1975, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1945:1962)&iso3c=="TUR",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "TUR", 2017, restricted = c(2018:2019)
)

x <- milexp.viewer("TUR")
# y <- milper.viewer("TUR")

##### TUV(x) ----------------------------------------------------------------------

##### cow expenditure
# 2013-2019

##### wmeat expenditure
# N/A

##### sipri expenditure
# N/A

##### cow personnel
# 2013-2019

##### wmeat personnel
# N/A

x <- milexp.viewer("TUV")
y <- milper.viewer("TUV")

##### TWN ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1949-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "TWN", 1963, restricted = c(1949:1962)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "TWN", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1963-1987: apply WMEAT growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.wmeat",
  "TWN", 1988, restricted = c(1963:1987)
)

# 1949-1962: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "TWN", 1963, restricted = c(1949:1962)
)

##### cow personnel
# good

##### wmeat personnel
# 1949-1962: COW and WMEAT personnel match 1963-1971, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1949:1962)&iso3c=="TWN",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "TWN", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("TWN")
# y <- milper.viewer("TWN")

##### TZA ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1961-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "TZA", 1963, restricted = c(1961:1962)
)

# 1986
mildata$mil.expenditure.wmeat[mildata$iso3c=="TZA"&mildata$year==1986] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "TZA", 1986)

# 1991
mildata$mil.expenditure.wmeat[mildata$iso3c=="TZA"&mildata$year==1991] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "TZA", 1991)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "TZA", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1961-1966: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "TZA", 1967, restricted = c(1961:1966)
)

##### cow personnel
# good

##### wmeat personnel
# 1961-1962: COW and WMEAT personnel match 1963-1982, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1961:1962)&iso3c=="TZA",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2004-05 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="TZA"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="TZA"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="TZA"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="TZA"&mildata$year==2018]

# x <- milexp.viewer("TZA")
# y <- milper.viewer("TZA")

##### UGA ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "UGA", 1963, restricted = 1962
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "UGA", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# good

##### cow personnel
# good

##### wmeat personnel
# 1962: COW and WMEAT personnel match 1963-1998, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year==1962&iso3c=="UGA",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2005-06 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="UGA"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="UGA"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="UGA"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="UGA"&mildata$year==2018]

# x <- milexp.viewer("UGA")
# y <- milper.viewer("UGA")

##### UKR ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1991-1992: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "UKR", 1993, restricted = c(1991:1992)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "UKR", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1991-1992: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "UKR", 1993, restricted = c(1991:1992)
)

##### cow personnel
# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"UKR",
  metric = "mil.personnel.cow",
  1991,1991
)

##### wmeat personnel
# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"UKR",
  metric = "mil.personnel.wmeat",
  1991,1991
)

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "UKR", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("UKR")
# y <- milper.viewer("UKR")

##### URY ----------------------------------------------------------------------

##### cow expenditure
# 1947: assume consistent growth between 1946 and 1948
ury.alpha.1 <- (mildata$mil.expenditure.cow[mildata$iso3c=="URY"&mildata$year==1948]/mildata$mil.expenditure.cow[mildata$iso3c=="URY"&mildata$year==1946])^(1/2)

mildata$mil.expenditure.cow[mildata$iso3c=="URY"&mildata$year==1947] <- mildata$mil.expenditure.cow[mildata$iso3c=="URY"&mildata$year==1946] * ury.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="URY"&mildata$year==1947] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="URY"&mildata$year==1946] * ury.alpha.1

# 1949: assume consistent growth between 1948 and 1950
ury.alpha.2 <- (mildata$mil.expenditure.cow[mildata$iso3c=="URY"&mildata$year==1950]/mildata$mil.expenditure.cow[mildata$iso3c=="URY"&mildata$year==1948])^(1/2)

mildata$mil.expenditure.cow[mildata$iso3c=="URY"&mildata$year==1949] <- mildata$mil.expenditure.cow[mildata$iso3c=="URY"&mildata$year==1948] * ury.alpha.2
mildata$mil.expenditure.cow.alt[mildata$iso3c=="URY"&mildata$year==1949] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="URY"&mildata$year==1948] * ury.alpha.2

# 1951: assume consistent growth between 1950 and 1952
ury.alpha.3 <- (mildata$mil.expenditure.cow[mildata$iso3c=="URY"&mildata$year==1952]/mildata$mil.expenditure.cow[mildata$iso3c=="URY"&mildata$year==1950])^(1/2)

mildata$mil.expenditure.cow[mildata$iso3c=="URY"&mildata$year==1951] <- mildata$mil.expenditure.cow[mildata$iso3c=="URY"&mildata$year==1950] * ury.alpha.3
mildata$mil.expenditure.cow.alt[mildata$iso3c=="URY"&mildata$year==1951] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="URY"&mildata$year==1950] * ury.alpha.3

# 1954-1956: assume consistent growth between 1953 and 1957
ury.alpha.4 <- (mildata$mil.expenditure.cow[mildata$iso3c=="URY"&mildata$year==1957]/mildata$mil.expenditure.cow[mildata$iso3c=="URY"&mildata$year==1953])^(1/4)

mildata$mil.expenditure.cow[mildata$iso3c=="URY"&mildata$year==1954] <- mildata$mil.expenditure.cow[mildata$iso3c=="URY"&mildata$year==1953] * ury.alpha.4
mildata$mil.expenditure.cow[mildata$iso3c=="URY"&mildata$year==1955] <- mildata$mil.expenditure.cow[mildata$iso3c=="URY"&mildata$year==1954] * ury.alpha.4
mildata$mil.expenditure.cow[mildata$iso3c=="URY"&mildata$year==1956] <- mildata$mil.expenditure.cow[mildata$iso3c=="URY"&mildata$year==1955] * ury.alpha.4

mildata$mil.expenditure.cow.alt[mildata$iso3c=="URY"&mildata$year==1954] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="URY"&mildata$year==1953] * ury.alpha.4
mildata$mil.expenditure.cow.alt[mildata$iso3c=="URY"&mildata$year==1955] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="URY"&mildata$year==1954] * ury.alpha.4
mildata$mil.expenditure.cow.alt[mildata$iso3c=="URY"&mildata$year==1956] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="URY"&mildata$year==1955] * ury.alpha.4

##### wmeat expenditure
# 1946-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "URY", 1963, restricted = c(1946:1962)
)

# 2018-2019: apply SIPRI growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "URY", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1971: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "URY", 1972, restricted = c(1946:1971)
)

##### cow personnel
# good

##### wmeat personnel
# 1946-1962: COW and WMEAT personnel match 1963-1998, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1946:1962)&iso3c=="URY",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: COW and WMEAT personnel match 2007-2017, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(2018:2019)&iso3c=="URY",mil.personnel.cow,mil.personnel.wmeat))

# x <- milexp.viewer("URY")
# y <- milper.viewer("URY")

##### USA ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1946-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "USA", 1963, restricted = c(1946:1962)
)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "USA", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1948: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "USA", 1949, restricted = c(1946:1948)
)

##### cow personnel
# good

##### wmeat personnel
# 1945-1962: COW and WMEAT personnel match 1963-1964, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1945:1962)&iso3c=="USA",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "USA", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("USA")
# y <- milper.viewer("USA")

##### UZB ----------------------------------------------------------------------

##### cow expenditure
# 1991-1993
mildata <- mil_estimator_no_data_func(
  mildata,"UZB",
  metric = "mil.expenditure.cow",
  1993,1991
)
mildata <- mil_estimator_no_data_func(
  mildata,"UZB",
  metric = "mil.expenditure.cow.alt",
  1993,1991
)

# 2008-2009
mildata$mil.expenditure.cow[mildata$iso3c=="UZB"&mildata$year==2008] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "UZB", 2008, yr.minus.1 = 2007, yr.plus.1 = 2010)
mildata$mil.expenditure.cow[mildata$iso3c=="UZB"&mildata$year==2009] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "UZB", 2009, yr.minus.1 = 2007, yr.plus.1 = 2010)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="UZB"&mildata$year==2008] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "UZB", 2008, yr.minus.1 = 2007, yr.plus.1 = 2010)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="UZB"&mildata$year==2009] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "UZB", 2009, yr.minus.1 = 2007, yr.plus.1 = 2010)

# 2012-2013 (non-alt): apply IISS growth estimates based on 2011
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "defense.spending.iiss",
  "UZB", 2011, restricted = c(2012:2013)
)

## remove estimate flag for 2012-2013
mildata <- mildata %>%
  dplyr::mutate(mil.expenditure.cow.est.flag  = dplyr::case_when(
    iso3c == "UZB" & year %in% c(2012:2013) ~ 0,
    .default = mil.expenditure.cow.est.flag
  ))

# 2014-2017: apply WMEAT growth estimates based on 2013
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "mil.expenditure.wmeat",
  "UZB", 2013, restricted = c(2014:2017)
)
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow.alt",
  col_ref = "mil.expenditure.wmeat",
  "UZB", 2013, restricted = c(2014:2017)
)

# 2018-2019
mildata <- mil_estimator_no_data_func(
  mildata,"UZB",
  metric = "mil.expenditure.cow",
  2018,2019
)
mildata <- mil_estimator_no_data_func(
  mildata,"UZB",
  metric = "mil.expenditure.cow.alt",
  2018,2019
)

##### wmeat expenditure
# 1991-1993
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "UZB", 1994, restricted = c(1991:1993)
)

# 2018-2019
mildata <- mil_estimator_no_data_func(
  mildata,"UZB",
  metric = "mil.expenditure.wmeat",
  2018,2019
)

##### sipri expenditure
# 1991-1993
mildata <- mil_estimator_no_data_func(
  mildata,"UZB",
  metric = "mil.expenditure.sipri",
  1993,1991
)

# 1998
mildata$mil.expenditure.sipri[mildata$iso3c=="UZB"&mildata$year==1998] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "UZB", 1998)

# 2004-2017
mildata$mil.expenditure.sipri[mildata$iso3c=="UZB"&mildata$year==2004] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT est", "UZB", 2004, yr.minus.1 = 2003, yr.plus.1 = 2018)
mildata$mil.expenditure.sipri[mildata$iso3c=="UZB"&mildata$year==2005] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT est", "UZB", 2005, yr.minus.1 = 2003, yr.plus.1 = 2018)
mildata$mil.expenditure.sipri[mildata$iso3c=="UZB"&mildata$year==2006] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT est", "UZB", 2006, yr.minus.1 = 2003, yr.plus.1 = 2018)
mildata$mil.expenditure.sipri[mildata$iso3c=="UZB"&mildata$year==2007] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT est", "UZB", 2007, yr.minus.1 = 2003, yr.plus.1 = 2018)
mildata$mil.expenditure.sipri[mildata$iso3c=="UZB"&mildata$year==2008] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT est", "UZB", 2008, yr.minus.1 = 2003, yr.plus.1 = 2018)
mildata$mil.expenditure.sipri[mildata$iso3c=="UZB"&mildata$year==2009] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT est", "UZB", 2009, yr.minus.1 = 2003, yr.plus.1 = 2018)
mildata$mil.expenditure.sipri[mildata$iso3c=="UZB"&mildata$year==2010] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT est", "UZB", 2010, yr.minus.1 = 2003, yr.plus.1 = 2018)
mildata$mil.expenditure.sipri[mildata$iso3c=="UZB"&mildata$year==2011] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT est", "UZB", 2011, yr.minus.1 = 2003, yr.plus.1 = 2018)
mildata$mil.expenditure.sipri[mildata$iso3c=="UZB"&mildata$year==2012] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT est", "UZB", 2012, yr.minus.1 = 2003, yr.plus.1 = 2018)
mildata$mil.expenditure.sipri[mildata$iso3c=="UZB"&mildata$year==2013] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT est", "UZB", 2013, yr.minus.1 = 2003, yr.plus.1 = 2018)
mildata$mil.expenditure.sipri[mildata$iso3c=="UZB"&mildata$year==2014] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT est", "UZB", 2014, yr.minus.1 = 2003, yr.plus.1 = 2018)
mildata$mil.expenditure.sipri[mildata$iso3c=="UZB"&mildata$year==2015] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT est", "UZB", 2015, yr.minus.1 = 2003, yr.plus.1 = 2018)
mildata$mil.expenditure.sipri[mildata$iso3c=="UZB"&mildata$year==2016] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT est", "UZB", 2016, yr.minus.1 = 2003, yr.plus.1 = 2018)
mildata$mil.expenditure.sipri[mildata$iso3c=="UZB"&mildata$year==2017] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT est", "UZB", 2017, yr.minus.1 = 2003, yr.plus.1 = 2018)

# 2019
mildata <- mil_estimator_no_data_func(
  mildata,"UZB",
  metric = "mil.expenditure.sipri",
  2019,2019
)

##### cow personnel
# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"UZB",
  metric = "mil.personnel.cow",
  1991,1991
)

##### wmeat personnel
# 1991
mildata <- mil_estimator_no_data_func(
  mildata,"UZB",
  metric = "mil.personnel.wmeat",
  1991,1991
)

# 2018-2019: 2013-14 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="UZB"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="UZB"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="UZB"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="UZB"&mildata$year==2018]

# x <- milexp.viewer("UZB")
# y <- milper.viewer("UZB")

##### VCT(x) ----------------------------------------------------------------------

##### cow expenditure
# 1991-1993
# 2013-2019

##### wmeat expenditure
# N/A

##### sipri expenditure
# N/A

##### cow personnel
# 2013-2019

##### wmeat personnel
# N/A

x <- milexp.viewer("VCT")
y <- milper.viewer("VCT")

##### VEN ----------------------------------------------------------------------

##### cow expenditure
# 2013-2017 (non-alt): apply IISS growth estimates based on 2012
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "defense.spending.iiss",
  "VEN", 2012, restricted = c(2013:2017)
)

## remove estimate flag for 2013-2017
mildata <- mildata %>%
  dplyr::mutate(mil.expenditure.cow.est.flag  = dplyr::case_when(
    iso3c == "VEN" & year %in% c(2013:2017) ~ 0,
    .default = mil.expenditure.cow.est.flag
  ))

# 2018-2019
mildata <- mil_estimator_no_data_func(
  mildata,"VEN",
  metric = "mil.expenditure.cow",
  2018,2019
)
mildata <- mil_estimator_no_data_func(
  mildata,"VEN",
  metric = "mil.expenditure.cow.alt",
  2018,2019
)

##### wmeat expenditure
# 1946-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "VEN", 1963, restricted = c(1946:1962)
)

# 2018-2019
mildata <- mil_estimator_no_data_func(
  mildata,"VEN",
  metric = "mil.expenditure.wmeat",
  2018,2019
)

##### sipri expenditure
# 1946-1954: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "VEN", 1955, restricted = c(1946:1954)
)

# 2018-2019
mildata <- mil_estimator_no_data_func(
  mildata,"VEN",
  metric = "mil.expenditure.sipri",
  2018,2019
)

##### cow personnel
# good

##### wmeat personnel
# 1945-1962: COW and WMEAT personnel match 1963, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1945:1962)&iso3c=="VEN",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "VEN", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("VEN")
# y <- milper.viewer("VEN")

##### VNM ----------------------------------------------------------------------

##### cow expenditure
# 1955-1960
mildata <- mil_estimator_no_data_func(
  mildata,"VNM",
  metric = "mil.expenditure.cow",
  1960,1955
)
mildata <- mil_estimator_no_data_func(
  mildata,"VNM",
  metric = "mil.expenditure.cow.alt",
  1960,1955
)

# 1954: recalculate
mildata <- mil_estimator_no_data_func(
  mildata,"VNM",
  metric = "mil.expenditure.cow",
  1954,1954
)
mildata <- mil_estimator_no_data_func(
  mildata,"VNM",
  metric = "mil.expenditure.cow.alt",
  1954,1954
)

# 1974-1983: assume consistent growth between 1973 and 1984
vnm.alpha.1 <- (mildata$mil.expenditure.cow[mildata$iso3c=="VNM"&mildata$year==1984]/mildata$mil.expenditure.cow[mildata$iso3c=="VNM"&mildata$year==1973])^(1/11)

mildata$mil.expenditure.cow[mildata$iso3c=="VNM"&mildata$year==1973] <- mildata$mil.expenditure.cow[mildata$iso3c=="VNM"&mildata$year==1972] * vnm.alpha.1
mildata$mil.expenditure.cow[mildata$iso3c=="VNM"&mildata$year==1974] <- mildata$mil.expenditure.cow[mildata$iso3c=="VNM"&mildata$year==1973] * vnm.alpha.1
mildata$mil.expenditure.cow[mildata$iso3c=="VNM"&mildata$year==1975] <- mildata$mil.expenditure.cow[mildata$iso3c=="VNM"&mildata$year==1974] * vnm.alpha.1
mildata$mil.expenditure.cow[mildata$iso3c=="VNM"&mildata$year==1976] <- mildata$mil.expenditure.cow[mildata$iso3c=="VNM"&mildata$year==1975] * vnm.alpha.1
mildata$mil.expenditure.cow[mildata$iso3c=="VNM"&mildata$year==1977] <- mildata$mil.expenditure.cow[mildata$iso3c=="VNM"&mildata$year==1976] * vnm.alpha.1
mildata$mil.expenditure.cow[mildata$iso3c=="VNM"&mildata$year==1978] <- mildata$mil.expenditure.cow[mildata$iso3c=="VNM"&mildata$year==1977] * vnm.alpha.1
mildata$mil.expenditure.cow[mildata$iso3c=="VNM"&mildata$year==1979] <- mildata$mil.expenditure.cow[mildata$iso3c=="VNM"&mildata$year==1978] * vnm.alpha.1
mildata$mil.expenditure.cow[mildata$iso3c=="VNM"&mildata$year==1980] <- mildata$mil.expenditure.cow[mildata$iso3c=="VNM"&mildata$year==1979] * vnm.alpha.1
mildata$mil.expenditure.cow[mildata$iso3c=="VNM"&mildata$year==1981] <- mildata$mil.expenditure.cow[mildata$iso3c=="VNM"&mildata$year==1980] * vnm.alpha.1
mildata$mil.expenditure.cow[mildata$iso3c=="VNM"&mildata$year==1982] <- mildata$mil.expenditure.cow[mildata$iso3c=="VNM"&mildata$year==1981] * vnm.alpha.1
mildata$mil.expenditure.cow[mildata$iso3c=="VNM"&mildata$year==1983] <- mildata$mil.expenditure.cow[mildata$iso3c=="VNM"&mildata$year==1982] * vnm.alpha.1

mildata$mil.expenditure.cow.alt[mildata$iso3c=="VNM"&mildata$year==1973] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="VNM"&mildata$year==1972] * vnm.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="VNM"&mildata$year==1974] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="VNM"&mildata$year==1973] * vnm.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="VNM"&mildata$year==1975] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="VNM"&mildata$year==1974] * vnm.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="VNM"&mildata$year==1976] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="VNM"&mildata$year==1975] * vnm.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="VNM"&mildata$year==1977] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="VNM"&mildata$year==1976] * vnm.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="VNM"&mildata$year==1978] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="VNM"&mildata$year==1977] * vnm.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="VNM"&mildata$year==1979] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="VNM"&mildata$year==1978] * vnm.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="VNM"&mildata$year==1980] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="VNM"&mildata$year==1979] * vnm.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="VNM"&mildata$year==1981] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="VNM"&mildata$year==1980] * vnm.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="VNM"&mildata$year==1982] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="VNM"&mildata$year==1981] * vnm.alpha.1
mildata$mil.expenditure.cow.alt[mildata$iso3c=="VNM"&mildata$year==1983] <- mildata$mil.expenditure.cow.alt[mildata$iso3c=="VNM"&mildata$year==1982] * vnm.alpha.1

# 1987: apply SIPRI growth rates to COW based on 1988
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "mil.expenditure.sipri",
  "VNM", 1988, restricted = 1987
)
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow.alt",
  col_ref = "mil.expenditure.sipri",
  "VNM", 1988, restricted = 1987
)

##### wmeat expenditure
# 1961-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "VNM", 1963, restricted = c(1961:1962)
)

# 1954-1960
mildata <- mil_estimator_no_data_func(
  mildata,"VNM",
  metric = "mil.expenditure.wmeat",
  1960,1954
)

# 1974-1983: assume consistent growth between 1973 and 1984
vnm.alpha.2 <- (mildata$mil.expenditure.wmeat[mildata$iso3c=="VNM"&mildata$year==1984]/mildata$mil.expenditure.wmeat[mildata$iso3c=="VNM"&mildata$year==1973])^(1/11)

mildata$mil.expenditure.wmeat[mildata$iso3c=="VNM"&mildata$year==1973] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="VNM"&mildata$year==1972] * vnm.alpha.2
mildata$mil.expenditure.wmeat[mildata$iso3c=="VNM"&mildata$year==1974] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="VNM"&mildata$year==1973] * vnm.alpha.2
mildata$mil.expenditure.wmeat[mildata$iso3c=="VNM"&mildata$year==1975] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="VNM"&mildata$year==1974] * vnm.alpha.2
mildata$mil.expenditure.wmeat[mildata$iso3c=="VNM"&mildata$year==1976] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="VNM"&mildata$year==1975] * vnm.alpha.2
mildata$mil.expenditure.wmeat[mildata$iso3c=="VNM"&mildata$year==1977] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="VNM"&mildata$year==1976] * vnm.alpha.2
mildata$mil.expenditure.wmeat[mildata$iso3c=="VNM"&mildata$year==1978] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="VNM"&mildata$year==1977] * vnm.alpha.2
mildata$mil.expenditure.wmeat[mildata$iso3c=="VNM"&mildata$year==1979] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="VNM"&mildata$year==1978] * vnm.alpha.2
mildata$mil.expenditure.wmeat[mildata$iso3c=="VNM"&mildata$year==1980] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="VNM"&mildata$year==1979] * vnm.alpha.2
mildata$mil.expenditure.wmeat[mildata$iso3c=="VNM"&mildata$year==1981] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="VNM"&mildata$year==1980] * vnm.alpha.2
mildata$mil.expenditure.wmeat[mildata$iso3c=="VNM"&mildata$year==1982] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="VNM"&mildata$year==1981] * vnm.alpha.2
mildata$mil.expenditure.wmeat[mildata$iso3c=="VNM"&mildata$year==1983] <- mildata$mil.expenditure.wmeat[mildata$iso3c=="VNM"&mildata$year==1982] * vnm.alpha.2

# 1984-1985: apply COW growth rates to WMEAT based on 1986
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "VNM", 1986, restricted = c(1984:1985)
)

# 1988-1989
mildata$mil.expenditure.wmeat[mildata$iso3c=="VNM"&mildata$year==1988] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "VNM", 1988, yr.minus.1 = 1986, yr.plus.1 = 1990)
mildata$mil.expenditure.wmeat[mildata$iso3c=="VNM"&mildata$year==1989] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "VNM", 1989, yr.minus.1 = 1986, yr.plus.1 = 1990)

# 1987: apply SIPRI growth rates to WMEAT based on 1988
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "VNM", 1988, restricted = 1987
)

# 1992-1993
mildata$mil.expenditure.wmeat[mildata$iso3c=="VNM"&mildata$year==1992] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "VNM", 1992, yr.minus.1 = 1991, yr.plus.1 = 1994)
mildata$mil.expenditure.wmeat[mildata$iso3c=="VNM"&mildata$year==1993] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "VNM", 1993, yr.minus.1 = 1991, yr.plus.1 = 1994)

# 2018-2019: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "VNM", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1984-1986: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "VNM", 1988, restricted = c(1984:1986)
)

# 1961-1973: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "VNM", 1988, restricted = c(1961:1973)
)


# 1954-1960
mildata <- mil_estimator_no_data_func(
  mildata,"VNM",
  metric = "mil.expenditure.sipri",
  1960,1954
)

# 1974-1983: assume consistent growth between 1973 and 1984
vnm.alpha.3 <- (mildata$mil.expenditure.sipri[mildata$iso3c=="VNM"&mildata$year==1984]/mildata$mil.expenditure.sipri[mildata$iso3c=="VNM"&mildata$year==1973])^(1/11)

mildata$mil.expenditure.sipri[mildata$iso3c=="VNM"&mildata$year==1973] <- mildata$mil.expenditure.sipri[mildata$iso3c=="VNM"&mildata$year==1972] * vnm.alpha.3
mildata$mil.expenditure.sipri[mildata$iso3c=="VNM"&mildata$year==1974] <- mildata$mil.expenditure.sipri[mildata$iso3c=="VNM"&mildata$year==1973] * vnm.alpha.3
mildata$mil.expenditure.sipri[mildata$iso3c=="VNM"&mildata$year==1975] <- mildata$mil.expenditure.sipri[mildata$iso3c=="VNM"&mildata$year==1974] * vnm.alpha.3
mildata$mil.expenditure.sipri[mildata$iso3c=="VNM"&mildata$year==1976] <- mildata$mil.expenditure.sipri[mildata$iso3c=="VNM"&mildata$year==1975] * vnm.alpha.3
mildata$mil.expenditure.sipri[mildata$iso3c=="VNM"&mildata$year==1977] <- mildata$mil.expenditure.sipri[mildata$iso3c=="VNM"&mildata$year==1976] * vnm.alpha.3
mildata$mil.expenditure.sipri[mildata$iso3c=="VNM"&mildata$year==1978] <- mildata$mil.expenditure.sipri[mildata$iso3c=="VNM"&mildata$year==1977] * vnm.alpha.3
mildata$mil.expenditure.sipri[mildata$iso3c=="VNM"&mildata$year==1979] <- mildata$mil.expenditure.sipri[mildata$iso3c=="VNM"&mildata$year==1978] * vnm.alpha.3
mildata$mil.expenditure.sipri[mildata$iso3c=="VNM"&mildata$year==1980] <- mildata$mil.expenditure.sipri[mildata$iso3c=="VNM"&mildata$year==1979] * vnm.alpha.3
mildata$mil.expenditure.sipri[mildata$iso3c=="VNM"&mildata$year==1981] <- mildata$mil.expenditure.sipri[mildata$iso3c=="VNM"&mildata$year==1980] * vnm.alpha.3
mildata$mil.expenditure.sipri[mildata$iso3c=="VNM"&mildata$year==1982] <- mildata$mil.expenditure.sipri[mildata$iso3c=="VNM"&mildata$year==1981] * vnm.alpha.3
mildata$mil.expenditure.sipri[mildata$iso3c=="VNM"&mildata$year==1983] <- mildata$mil.expenditure.sipri[mildata$iso3c=="VNM"&mildata$year==1982] * vnm.alpha.3

# 1995-2002
mildata$mil.expenditure.sipri[mildata$iso3c=="VNM"&mildata$year==1995] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "VNM", 1995, yr.minus.1 = 1994, yr.plus.1 = 2003)
mildata$mil.expenditure.sipri[mildata$iso3c=="VNM"&mildata$year==1996] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "VNM", 1996, yr.minus.1 = 1994, yr.plus.1 = 2003)
mildata$mil.expenditure.sipri[mildata$iso3c=="VNM"&mildata$year==1997] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "VNM", 1997, yr.minus.1 = 1994, yr.plus.1 = 2003)
mildata$mil.expenditure.sipri[mildata$iso3c=="VNM"&mildata$year==1998] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "VNM", 1998, yr.minus.1 = 1994, yr.plus.1 = 2003)
mildata$mil.expenditure.sipri[mildata$iso3c=="VNM"&mildata$year==1999] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "VNM", 1999, yr.minus.1 = 1994, yr.plus.1 = 2003)
mildata$mil.expenditure.sipri[mildata$iso3c=="VNM"&mildata$year==2000] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "VNM", 2000, yr.minus.1 = 1994, yr.plus.1 = 2003)
mildata$mil.expenditure.sipri[mildata$iso3c=="VNM"&mildata$year==2001] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "VNM", 2001, yr.minus.1 = 1994, yr.plus.1 = 2003)
mildata$mil.expenditure.sipri[mildata$iso3c=="VNM"&mildata$year==2002] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "VNM", 2002, yr.minus.1 = 1994, yr.plus.1 = 2003)

# 2012-2019: apply COW growth rates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "VNM", 2011, restricted = c(2012:2019)
)

##### cow personnel
# good

##### wmeat personnel
# 1954-1962: COW and WMEAT personnel match 1963-1981, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1954:1962)&iso3c=="VNM",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "VNM", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("VNM")
# y <- milper.viewer("VNM")

##### VUT(x) ----------------------------------------------------------------------

##### cow expenditure
# 2013-2019

##### wmeat expenditure
# N/A

##### sipri expenditure
# N/A

##### cow personnel
# 2013-2019

##### wmeat personnel
# N/A

x <- milexp.viewer("VUT")
y <- milper.viewer("VUT")

##### WSM(x) ----------------------------------------------------------------------

##### cow expenditure
# 2013-2019

##### wmeat expenditure
# N/A

##### sipri expenditure
# N/A

##### cow personnel
# 2013-2019

##### wmeat personnel
# N/A

x <- milexp.viewer("WSM")
y <- milper.viewer("WSM")

##### YAR ----------------------------------------------------------------------

##### cow expenditure
# 1946-1959
mildata <- mil_estimator_no_data_func(
  mildata,"YAR",
  metric = "mil.expenditure.cow",
  1959,1946
)
mildata <- mil_estimator_no_data_func(
  mildata,"YAR",
  metric = "mil.expenditure.cow.alt",
  1959,1946
)

##### wmeat expenditure
# 1960-1962: apply COW growth rates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "YAR", 1963, restricted = c(1960:1962)
)

# 1946-1959
mildata <- mil_estimator_no_data_func(
  mildata,"YAR",
  metric = "mil.expenditure.wmeat",
  1959,1946
)

##### sipri expenditure
# N/A
# TODO: check

##### cow personnel
# good

##### wmeat personnel
# 1945-1962: COW and WMEAT personnel match 1963-1990, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1945:1962)&iso3c=="YAR",mil.personnel.cow,mil.personnel.wmeat))

# x <- milexp.viewer("YAR")
# y <- milper.viewer("YAR")

##### YEM ----------------------------------------------------------------------

##### cow expenditure
# 2013-2014: apply IISS growth estimates based on 2012
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow",
  col_ref = "defense.spending.iiss",
  "YEM", 2012, restricted = c(2013:2014)
)
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.cow.alt",
  col_ref = "defense.spending.iiss",
  "YEM", 2012, restricted = c(2013:2014)
)

## remove estimate flag for 2013-2014
mildata <- mildata %>%
  dplyr::mutate(mil.expenditure.cow.est.flag  = dplyr::case_when(
    iso3c == "YEM" & year %in% c(2013:2014) ~ 0,
    .default = mil.expenditure.cow.est.flag
  ))
mildata <- mildata %>%
  dplyr::mutate(mil.expenditure.cow.alt.est.flag  = dplyr::case_when(
    iso3c == "YEM" & year %in% c(2013:2014) ~ 0,
    .default = mil.expenditure.cow.est.flag
  ))

# 2015-2019
mildata <- mil_estimator_no_data_func(
  mildata,"YEM",
  metric = "mil.expenditure.cow",
  2015,2019
)
mildata <- mil_estimator_no_data_func(
  mildata,"YEM",
  metric = "mil.expenditure.cow.alt",
  2015,2019
)

##### wmeat expenditure
# 2015-2019
mildata <- mil_estimator_no_data_func(
  mildata,"YEM",
  metric = "mil.expenditure.wmeat",
  2015,2019
)

##### sipri expenditure
# 2015-2019
mildata <- mil_estimator_no_data_func(
  mildata,"YEM",
  metric = "mil.expenditure.sipri",
  2015,2019
)

##### cow personnel
# good

##### wmeat personnel
# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "YEM", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("YEM")
# y <- milper.viewer("YEM")

##### YPR ----------------------------------------------------------------------

##### cow expenditure
# 1967: recalculate
mildata <- mil_estimator_no_data_func(
  mildata,"YPR",
  metric = "mil.expenditure.cow",
  1967,1967
)
mildata <- mil_estimator_no_data_func(
  mildata,"YPR",
  metric = "mil.expenditure.cow.alt",
  1967,1967
)

# 1990
mildata <- mil_estimator_no_data_func(
  mildata,"YPR",
  metric = "mil.expenditure.cow",
  1990,1990, dir = "lower"
)
mildata <- mil_estimator_no_data_func(
  mildata,"YPR",
  metric = "mil.expenditure.cow.alt",
  1990,1990, dir = "lower"
)

##### wmeat expenditure
# 1967: apply COW growth estimates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "YPR", 1968, restricted = 1967
)

# 1990
mildata <- mil_estimator_no_data_func(
  mildata,"YPR",
  metric = "mil.expenditure.wmeat",
  1990,1990, dir = "lower"
)

##### sipri expenditure
# N/A
# TODO: check

##### cow personnel
# 1990
mildata <- mil_estimator_no_data_func(
  mildata,"YPR",
  metric = "mil.personnel.cow",
  1990,1990, dir = "lower"
)

##### wmeat personnel
# 1967: COW and WMEAT personnel match 1968-1989, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year==1967&iso3c=="YPR",mil.personnel.cow,mil.personnel.wmeat))

# 1990
mildata <- mil_estimator_no_data_func(
  mildata,"YPR",
  metric = "mil.personnel.wmeat",
  1990,1990, dir = "lower"
)

# x <- milexp.viewer("YPR")
# y <- milper.viewer("YPR")

##### YUG ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1946-1962: apply COW growth estimates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "YUG", 1963, restricted = c(1946:1962)
)

##### sipri expenditure
# N/A
# TODO: check

##### cow personnel
# good

##### wmeat personnel
# 1945-1962: COW and WMEAT personnel match 1963-1976, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1945:1962)&iso3c=="YUG",mil.personnel.cow,mil.personnel.wmeat))

x <- milexp.viewer("YUG")
# y <- milper.viewer("YUG")

##### ZAF ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# 1946-1962: apply COW growth estimates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "ZAF", 1963, restricted = c(1946:1962)
)

# 2018-2019: apply COW growth estimates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "ZAF", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1946-1950: apply COW growth estimates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "ZAF", 1951, restricted = c(1946:1950)
)

##### cow personnel
# good

##### wmeat personnel
# 1946-1962: COW and WMEAT personnel match 1963-1991, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year %in% c(1946:1962)&iso3c=="ZAF",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.personnel.wmeat",
  col_ref = "mil.personnel.cow",
  "ZAF", 2017, restricted = c(2018:2019)
)

# x <- milexp.viewer("ZAF")
# y <- milper.viewer("ZAF")

##### ZAN ----------------------------------------------------------------------

##### cow expenditure
# good

##### wmeat expenditure
# N/A

##### sipri expenditure
# N/A

##### cow personnel
# good

##### wmeat personnel
# N/A

# x <- milexp.viewer("ZAN")
# y <- milper.viewer("ZAN")

##### ZMB ----------------------------------------------------------------------

##### cow expenditure
# 1992
mildata$mil.expenditure.cow[mildata$iso3c=="ZMB"&mildata$year==1992] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "ZMB", 1992)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="ZMB"&mildata$year==1992] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "SIPRI", "ZMB", 1992)

##### wmeat expenditure
# 1964: apply COW growth estimates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "ZMB", 1965, restricted = 1964
)

# 1979
mildata$mil.expenditure.wmeat[mildata$iso3c=="ZMB"&mildata$year==1979] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "ZMB", 1979)

# 1981-1983
mildata$mil.expenditure.wmeat[mildata$iso3c=="ZMB"&mildata$year==1981] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI est", "ZMB", 1981, yr.minus.1 = 1980, yr.plus.1 = 1984)
mildata$mil.expenditure.wmeat[mildata$iso3c=="ZMB"&mildata$year==1982] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI est", "ZMB", 1982, yr.minus.1 = 1980, yr.plus.1 = 1984)
mildata$mil.expenditure.wmeat[mildata$iso3c=="ZMB"&mildata$year==1983] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI est", "ZMB", 1983, yr.minus.1 = 1980, yr.plus.1 = 1984)

# 1985-1987
mildata$mil.expenditure.wmeat[mildata$iso3c=="ZMB"&mildata$year==1985] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "ZMB", 1985, yr.minus.1 = 1984, yr.plus.1 = 1988)
mildata$mil.expenditure.wmeat[mildata$iso3c=="ZMB"&mildata$year==1986] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "ZMB", 1986, yr.minus.1 = 1984, yr.plus.1 = 1988)
mildata$mil.expenditure.wmeat[mildata$iso3c=="ZMB"&mildata$year==1987] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "COW", "ZMB", 1987, yr.minus.1 = 1984, yr.plus.1 = 1988)

# 1991
mildata$mil.expenditure.wmeat[mildata$iso3c=="ZMB"&mildata$year==1991] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "ZMB", 1991)

# 2018-2019: apply COW growth estimates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "ZMB", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 1964-1983: apply COW growth estimates to SIPRI
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.sipri",
  col_ref = "mil.expenditure.cow",
  "ZMB", 1984, restricted = c(1964:1983)
)

# 1998
mildata$mil.expenditure.sipri[mildata$iso3c=="ZMB"&mildata$year==1998] <- mil_expenditure_distance_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "ZMB", 1998)

# 2000-2003
mildata$mil.expenditure.sipri[mildata$iso3c=="ZMB"&mildata$year==2000] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "ZMB", 2000, yr.minus.1 = 1999, yr.plus.1 = 2004)
mildata$mil.expenditure.sipri[mildata$iso3c=="ZMB"&mildata$year==2001] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "ZMB", 2001, yr.minus.1 = 1999, yr.plus.1 = 2004)
mildata$mil.expenditure.sipri[mildata$iso3c=="ZMB"&mildata$year==2002] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "ZMB", 2002, yr.minus.1 = 1999, yr.plus.1 = 2004)
mildata$mil.expenditure.sipri[mildata$iso3c=="ZMB"&mildata$year==2003] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "COW", "ZMB", 2003, yr.minus.1 = 1999, yr.plus.1 = 2004)

##### cow personnel
# 1963: COW and WMEAT personnel match 1964-1994, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year==1963&iso3c=="ZMB",mil.personnel.cow,mil.personnel.wmeat))

##### wmeat personnel
# 2018-2019: 2006-07 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="ZMB"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="ZMB"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="ZMB"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="ZMB"&mildata$year==2018]

# x <- milexp.viewer("ZMB")
# y <- milper.viewer("ZMB")

##### ZWE ----------------------------------------------------------------------

##### cow expenditure
# 2007-2009
mildata$mil.expenditure.cow[mildata$iso3c=="ZWE"&mildata$year==2007] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "ZWE", 2007, yr.minus.1 = 2005, yr.plus.1 = 2010)
mildata$mil.expenditure.cow[mildata$iso3c=="ZWE"&mildata$year==2008] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "ZWE", 2008, yr.minus.1 = 2005, yr.plus.1 = 2010)
mildata$mil.expenditure.cow[mildata$iso3c=="ZWE"&mildata$year==2009] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "ZWE", 2009, yr.minus.1 = 2005, yr.plus.1 = 2010)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="ZWE"&mildata$year==2007] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "ZWE", 2007, yr.minus.1 = 2005, yr.plus.1 = 2010)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="ZWE"&mildata$year==2008] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "ZWE", 2008, yr.minus.1 = 2005, yr.plus.1 = 2010)
mildata$mil.expenditure.cow.alt[mildata$iso3c=="ZWE"&mildata$year==2009] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "COW", col_ref = "WMEAT", "ZWE", 2009, yr.minus.1 = 2005, yr.plus.1 = 2010)

##### wmeat expenditure
# 1965: apply COW growth estimates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.cow",
  "ZWE", 1966, restricted = 1965
)

# 2006
mildata$mil.expenditure.wmeat[mildata$iso3c=="ZWE"&mildata$year==2006] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "WMEAT", col_ref = "SIPRI", "ZWE", 2006, yr.minus.1 = 2005, yr.plus.1 = 2010)

# 2018-2019: apply SIPRI growth estimates to WMEAT
mildata <- mil_expenditure_growth_estimator_func(
  mildata,col_missing = "mil.expenditure.wmeat",
  col_ref = "mil.expenditure.sipri",
  "ZWE", 2017, restricted = c(2018:2019)
)

##### sipri expenditure
# 2007-2009
mildata$mil.expenditure.sipri[mildata$iso3c=="ZWE"&mildata$year==2007] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "ZWE", 2007, yr.minus.1 = 2005, yr.plus.1 = 2010)
mildata$mil.expenditure.sipri[mildata$iso3c=="ZWE"&mildata$year==2008] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "ZWE", 2008, yr.minus.1 = 2005, yr.plus.1 = 2010)
mildata$mil.expenditure.sipri[mildata$iso3c=="ZWE"&mildata$year==2009] <- mil_expenditure_gap_estimator_func(
  df = mildata, col_missing = "SIPRI", col_ref = "WMEAT", "ZWE", 2009, yr.minus.1 = 2005, yr.plus.1 = 2010)

##### cow personnel
# good

##### wmeat personnel
# 1965: COW and WMEAT personnel match 1966-1979, so use COW personnel estimates
mildata <- mildata %>%
  dplyr::mutate(mil.personnel.wmeat = ifelse(year==1965&iso3c=="ZWE",mil.personnel.cow,mil.personnel.wmeat))

# 2018-2019: 2004-05 - 2016-17 growth rates 0% for both COW and WMEAT; apply COW's continued 0% growth to WMEAT
mildata$mil.personnel.wmeat[mildata$iso3c=="ZWE"&mildata$year==2018] <- mildata$mil.personnel.wmeat[mildata$iso3c=="ZWE"&mildata$year==2017]
mildata$mil.personnel.wmeat[mildata$iso3c=="ZWE"&mildata$year==2019] <- mildata$mil.personnel.wmeat[mildata$iso3c=="ZWE"&mildata$year==2018]

# x <- milexp.viewer("ZWE")
# y <- milper.viewer("ZWE")

### add gdp and population data ----------------------------------------------------------------------
mildata <- mildata %>%
  dplyr::left_join(population,by=c("iso3c","country","year")) %>%
  dplyr::left_join(gdp,by=c("iso3c","country","year")) %>%
  dplyr::full_join(cyears,by=c("iso3c","country","year")) %>%
  dplyr::select(-c(
    "milex.cow","milper.cow","defense.spending.iiss","defense.spending.percapita.iiss","defense.spending.percgdp.iiss",
    "active.armed.forces.iiss","reservists.iiss","active.paramilitary.iiss","milexp.sipri","armed.personnel.wmeat",
    "population.wmeat","military.expenditure.wmeat","gdp.wmeat","pop.growth.rate.un","pop.growth.rate.cow","gdp.pwt.original",
    "gdp.gl.original","gdp.growth.rate.pwt.original","gdp.growth.rate.gl.original","gdp.growth.rate.pwt.est",
    "gdp.growth.rate.gl.est","imf.growth.rate.modern","imf.growth.rate.historical","wb.growth.rate","yrs_since_indep"
    )) %>%
  dplyr::mutate(
    # military expenditure per capita
    mil.expenditure.per.capita.cow.un = mil.expenditure.cow / un.pop,
    mil.expenditure.per.capita.cow.alt.un = mil.expenditure.cow.alt / un.pop,
    mil.expenditure.per.capita.cow.cow = mil.expenditure.cow / cow.pop,
    mil.expenditure.per.capita.cow.alt.cow = mil.expenditure.cow.alt / cow.pop,
    mil.expenditure.per.capita.wmeat.un = mil.expenditure.wmeat / un.pop,
    mil.expenditure.per.capita.wmeat.cow = mil.expenditure.wmeat / cow.pop,
    mil.expenditure.per.capita.sipri.un = mil.expenditure.sipri / un.pop,
    mil.expenditure.per.capita.sipri.cow = mil.expenditure.sipri / cow.pop,
    # military expenditure as a % of gdp
    mil.expenditure.perc.gdp.cow.pwt = 100 * mil.expenditure.cow / gdp.pwt.est,
    mil.expenditure.perc.gdp.cow.alt.pwt = 100 * mil.expenditure.cow.alt / gdp.pwt.est,
    mil.expenditure.perc.gdp.cow.gl = 100 * mil.expenditure.cow / gdp.gl.est,
    mil.expenditure.perc.gdp.cow.alt.gl = 100 * mil.expenditure.cow.alt / gdp.gl.est,
    mil.expenditure.perc.gdp.wmeat.pwt = 100 * mil.expenditure.wmeat / gdp.pwt.est,
    mil.expenditure.perc.gdp.wmeat.gl = 100 * mil.expenditure.wmeat / gdp.gl.est,
    mil.expenditure.perc.gdp.sipri.pwt = 100 * mil.expenditure.sipri / gdp.pwt.est,
    mil.expenditure.perc.gdp.sipri.gl = 100 * mil.expenditure.sipri / gdp.gl.est,
    # military expenditure per military personnel
    mil.expenditure.per.personnel.cow.cow = mil.expenditure.cow / mil.personnel.cow,
    mil.expenditure.per.personnel.cow.alt.cow = mil.expenditure.cow.alt / mil.personnel.cow,
    mil.expenditure.per.personnel.cow.wmeat = mil.expenditure.cow / mil.personnel.wmeat,
    mil.expenditure.per.personnel.cow.alt.wmeat = mil.expenditure.cow.alt / mil.personnel.wmeat,
    mil.expenditure.per.personnel.wmeat.cow = mil.expenditure.wmeat / mil.personnel.cow,
    mil.expenditure.per.personnel.wmeat.wmeat = mil.expenditure.wmeat / mil.personnel.wmeat,
    mil.expenditure.per.personnel.sipri.cow = mil.expenditure.sipri / mil.personnel.cow,
    mil.expenditure.per.personnel.sipri.wmeat = mil.expenditure.sipri / mil.personnel.wmeat,
    # military personnel per capita
    mil.personnel.per.capita.cow.un = mil.personnel.cow / un.pop,
    mil.personnel.per.capita.cow.cow = mil.personnel.cow / cow.pop,
    mil.personnel.per.capita.wmeat.un = mil.personnel.wmeat / un.pop,
    mil.personnel.per.capita.wmeat.cow = mil.personnel.wmeat / cow.pop,
    # gdp per military personnel
    gdp.per.mil.personnel.pwt.cow = gdp.pwt.est / mil.personnel.cow,
    gdp.per.mil.personnel.pwt.wmeat = gdp.pwt.est / mil.personnel.wmeat,
    gdp.per.mil.personnel.gl.cow = gdp.gl.est / mil.personnel.cow,
    gdp.per.mil.personnel.gl.wmeat = gdp.gl.est / mil.personnel.wmeat
  )

### calculate growth rates ----------------------------------------------------------------------
mildata_year_prior <- mildata %>%
  dplyr::mutate(year = year + 1)

names(mildata_year_prior)[4:52] <- paste0(names(mildata_year_prior)[4:52],".plus1")

mildata <- mildata %>%
  dplyr::left_join(mildata_year_prior,by=c("iso3c","country","year")) %>%
  dplyr::mutate(
    # military expenditure
    mil.expenditure.growth.rate.cow = 100 * (mil.expenditure.cow - mil.expenditure.cow.plus1) / mil.expenditure.cow.plus1,
    mil.expenditure.growth.rate.cow.alt = 100 * (mil.expenditure.cow.alt - mil.expenditure.cow.alt.plus1) / mil.expenditure.cow.alt.plus1,
    mil.expenditure.growth.rate.wmeat = 100 * (mil.expenditure.wmeat - mil.expenditure.wmeat.plus1) / mil.expenditure.wmeat.plus1,
    mil.expenditure.growth.rate.sipri = 100 * (mil.expenditure.sipri - mil.expenditure.sipri.plus1) / mil.expenditure.sipri.plus1,
    # military personnel
    mil.personnel.growth.rate.cow = 100 * (mil.personnel.cow - mil.personnel.cow.plus1) / mil.personnel.cow.plus1,
    mil.personnel.growth.rate.wmeat = 100 * (mil.personnel.wmeat - mil.personnel.wmeat.plus1) / mil.personnel.wmeat.plus1,
    # military expenditure per capita
    mil.expenditure.per.capita.growth.rate.cow.un = 100 * (mil.expenditure.per.capita.cow.un - mil.expenditure.per.capita.cow.un.plus1) / mil.expenditure.per.capita.cow.un.plus1,
    mil.expenditure.per.capita.growth.rate.cow.alt.un = 100 * (mil.expenditure.per.capita.cow.alt.un - mil.expenditure.per.capita.cow.alt.un.plus1) / mil.expenditure.per.capita.cow.alt.un,
    mil.expenditure.per.capita.growth.rate.cow.cow = 100 * (mil.expenditure.per.capita.cow.cow - mil.expenditure.per.capita.cow.cow.plus1) / mil.expenditure.per.capita.cow.cow.plus1,
    mil.expenditure.per.capita.growth.rate.cow.alt.cow = 100 * (mil.expenditure.per.capita.cow.alt.cow - mil.expenditure.per.capita.cow.alt.cow.plus1) / mil.expenditure.per.capita.cow.alt.cow.plus1,
    mil.expenditure.per.capita.growth.rate.wmeat.un = 100 * (mil.expenditure.per.capita.wmeat.un - mil.expenditure.per.capita.wmeat.un.plus1) / mil.expenditure.per.capita.wmeat.un.plus1,
    mil.expenditure.per.capita.growth.rate.wmeat.cow = 100 * (mil.expenditure.per.capita.wmeat.cow - mil.expenditure.per.capita.wmeat.cow.plus1) / mil.expenditure.per.capita.wmeat.cow.plus1,
    mil.expenditure.per.capita.growth.rate.sipri.un = 100 * (mil.expenditure.per.capita.sipri.un - mil.expenditure.per.capita.sipri.un.plus1) / mil.expenditure.per.capita.sipri.un.plus1,
    mil.expenditure.per.capita.growth.rate.sipri.cow = 100 * (mil.expenditure.per.capita.sipri.cow - mil.expenditure.per.capita.sipri.cow.plus1) / mil.expenditure.per.capita.sipri.cow.plus1,
    # military expenditure as a % of gdp
    mil.expenditure.perc.gdp.growth.rate.cow.pwt = 100 * (mil.expenditure.perc.gdp.cow.pwt - mil.expenditure.perc.gdp.cow.pwt.plus1) / mil.expenditure.perc.gdp.cow.pwt.plus1,
    mil.expenditure.perc.gdp.growth.rate.cow.alt.pwt = 100 * (mil.expenditure.perc.gdp.cow.alt.pwt - mil.expenditure.perc.gdp.cow.alt.pwt.plus1) / mil.expenditure.perc.gdp.cow.alt.pwt.plus1,
    mil.expenditure.perc.gdp.growth.rate.cow.gl = 100 * (mil.expenditure.perc.gdp.cow.gl - mil.expenditure.perc.gdp.cow.gl.plus1) / mil.expenditure.perc.gdp.cow.gl.plus1,
    mil.expenditure.perc.gdp.growth.rate.cow.alt.gl = 100 * (mil.expenditure.perc.gdp.cow.alt.gl - mil.expenditure.perc.gdp.cow.alt.gl.plus1) / mil.expenditure.perc.gdp.cow.alt.gl.plus1,
    mil.expenditure.perc.gdp.growth.rate.wmeat.pwt = 100 * (mil.expenditure.perc.gdp.wmeat.pwt - mil.expenditure.perc.gdp.wmeat.pwt.plus1) / mil.expenditure.perc.gdp.wmeat.pwt.plus1,
    mil.expenditure.perc.gdp.growth.rate.wmeat.gl = 100 * (mil.expenditure.perc.gdp.wmeat.gl - mil.expenditure.perc.gdp.wmeat.gl.plus1) / mil.expenditure.perc.gdp.wmeat.gl.plus1,
    mil.expenditure.perc.gdp.growth.rate.sipri.pwt = 100 * (mil.expenditure.perc.gdp.sipri.pwt - mil.expenditure.perc.gdp.sipri.pwt.plus1) / mil.expenditure.perc.gdp.sipri.pwt.plus1,
    mil.expenditure.perc.gdp.growth.rate.sipri.gl = 100 * (mil.expenditure.perc.gdp.sipri.gl - mil.expenditure.perc.gdp.sipri.gl.plus1) / mil.expenditure.perc.gdp.sipri.gl.plus1,
    # military expenditure per military personnel
    mil.expenditure.per.personnel.growth.rate.cow.cow = 100 * (mil.expenditure.per.personnel.cow.cow - mil.expenditure.per.personnel.cow.cow.plus1) / mil.expenditure.per.personnel.cow.cow.plus1,
    mil.expenditure.per.personnel.growth.rate.cow.alt.cow = 100 * (mil.expenditure.per.personnel.cow.alt.cow - mil.expenditure.per.personnel.cow.alt.cow.plus1) / mil.expenditure.per.personnel.cow.alt.cow.plus1,
    mil.expenditure.per.personnel.growth.rate.cow.wmeat = 100 * (mil.expenditure.per.personnel.cow.wmeat - mil.expenditure.per.personnel.cow.wmeat.plus1) / mil.expenditure.per.personnel.cow.wmeat.plus1,
    mil.expenditure.per.personnel.growth.rate.cow.alt.wmeat = 100 * (mil.expenditure.per.personnel.cow.alt.wmeat - mil.expenditure.per.personnel.cow.alt.wmeat.plus1) / mil.expenditure.per.personnel.cow.alt.wmeat.plus1,
    mil.expenditure.per.personnel.growth.rate.wmeat.cow = 100 * (mil.expenditure.per.personnel.wmeat.cow - mil.expenditure.per.personnel.wmeat.cow.plus1) / mil.expenditure.per.personnel.wmeat.cow.plus1,
    mil.expenditure.per.personnel.growth.rate.wmeat.wmeat = 100 * (mil.expenditure.per.personnel.wmeat.wmeat - mil.expenditure.per.personnel.wmeat.wmeat.plus1) / mil.expenditure.per.personnel.wmeat.wmeat.plus1,
    mil.expenditure.per.personnel.growth.rate.sipri.cow = 100 * (mil.expenditure.per.personnel.sipri.cow - mil.expenditure.per.personnel.sipri.cow.plus1) / mil.expenditure.per.personnel.sipri.cow.plus1,
    mil.expenditure.per.personnel.growth.rate.sipri.wmeat = 100 * (mil.expenditure.per.personnel.sipri.wmeat - mil.expenditure.per.personnel.sipri.wmeat.plus1) / mil.expenditure.per.personnel.sipri.wmeat.plus1,
    # military personnel per capita
    mil.personnel.per.capita.growth.rate.cow.un = 100 * (mil.personnel.per.capita.cow.un - mil.personnel.per.capita.cow.un.plus1) / mil.personnel.per.capita.cow.un.plus1,
    mil.personnel.per.capita.growth.rate.cow.cow = 100 * (mil.personnel.per.capita.cow.cow - mil.personnel.per.capita.cow.cow.plus1) / mil.personnel.per.capita.cow.cow.plus1,
    mil.personnel.per.capita.growth.rate.wmeat.un = 100 * (mil.personnel.per.capita.wmeat.un - mil.personnel.per.capita.wmeat.un.plus1) / mil.personnel.per.capita.wmeat.un.plus1,
    mil.personnel.per.capita.growth.rate.wmeat.cow = 100 * (mil.personnel.per.capita.wmeat.cow - mil.personnel.per.capita.wmeat.cow.plus1) / mil.personnel.per.capita.wmeat.cow.plus1,
    # gdp per military personnel
    gdp.per.mil.personnel.growth.rate.pwt.cow = 100 * (gdp.per.mil.personnel.pwt.cow - gdp.per.mil.personnel.pwt.cow.plus1) / gdp.per.mil.personnel.pwt.cow.plus1,
    gdp.per.mil.personnel.growth.rate.pwt.wmeat = 100 * (gdp.per.mil.personnel.pwt.wmeat - gdp.per.mil.personnel.pwt.wmeat.plus1) / gdp.per.mil.personnel.pwt.wmeat.plus1,
    gdp.per.mil.personnel.growth.rate.gl.cow = 100 * (gdp.per.mil.personnel.gl.cow - gdp.per.mil.personnel.gl.cow.plus1) / gdp.per.mil.personnel.gl.cow.plus1,
    gdp.per.mil.personnel.growth.rate.gl.wmeat = 100 * (gdp.per.mil.personnel.gl.wmeat - gdp.per.mil.personnel.gl.wmeat.plus1) / gdp.per.mil.personnel.gl.wmeat.plus1
  ) %>%
  dplyr::select(-contains("plus1")) %>%
  dplyr::filter(cn == 1) %>%
  dplyr::select(-c(un.pop,cow.pop,gdp.pwt.est,gdp.gl.est,cn))

### write data ----------------------------------------------------------------------
# writes formatted dataframe as csv files
write.csv(mildata,"Data files/Formatted data files/military_data.csv",row.names = FALSE)

### codebook ----------------------------------------------------------------------
# iso3c
### A country's standardized iso3c code, with non-standard codes for West Germany, East Germany, North Yemen, South Yemen,
### South Vietnam, the Soviet Union, Yugoslavia, and Zanzibar.
# country
### A country's commonly used English-language name.
# year
### The calendar year the specific variable is measured during.
# mil.expenditure.cow
# mil.expenditure.cow.alt
# mil.personnel.cow
# mil.expenditure.wmeat
# mil.personnel.wmeat
# mil.expenditure.sipri
# mil.expenditure.cow.est.flag
# mil.expenditure.cow.alt.est.flag
# mil.personnel.cow.est.flag
# mil.expenditure.per.capita.cow.un
# mil.expenditure.per.capita.cow.alt.un
# mil.expenditure.per.capita.cow.cow
# mil.expenditure.per.capita.cow.alt.cow
# mil.expenditure.per.capita.wmeat.un
# mil.expenditure.per.capita.wmeat.cow
# mil.expenditure.per.capita.sipri.un
# mil.expenditure.per.capita.sipri.cow
# mil.expenditure.perc.gdp.cow.pwt
# mil.expenditure.perc.gdp.cow.alt.pwt
# mil.expenditure.perc.gdp.cow.gl
# mil.expenditure.perc.gdp.cow.alt.gl
# mil.expenditure.perc.gdp.wmeat.pwt
# mil.expenditure.perc.gdp.wmeat.gl
# mil.expenditure.perc.gdp.sipri.pwt
# mil.expenditure.perc.gdp.sipri.gl
# mil.expenditure.per.personnel.cow.cow
# mil.expenditure.per.personnel.cow.alt.cow
# mil.expenditure.per.personnel.cow.wmeat
# mil.expenditure.per.personnel.cow.alt.wmeat
# mil.expenditure.per.personnel.wmeat.cow
# mil.expenditure.per.personnel.wmeat.wmeat
# mil.expenditure.per.personnel.sipri.cow
# mil.expenditure.per.personnel.sipri.wmeat
# mil.personnel.per.capita.cow.un
# mil.personnel.per.capita.cow.cow
# mil.personnel.per.capita.wmeat.un
# mil.personnel.per.capita.wmeat.cow
# gdp.per.mil.personnel.pwt.cow
# gdp.per.mil.personnel.pwt.wmeat
# gdp.per.mil.personnel.gl.cow
# gdp.per.mil.personnel.gl.wmeat
# mil.expenditure.growth.rate.cow
# mil.expenditure.growth.rate.cow.alt
# mil.expenditure.growth.rate.wmeat
# mil.expenditure.growth.rate.sipri
# mil.personnel.growth.rate.cow
# mil.personnel.growth.rate.wmeat
# mil.expenditure.per.capita.growth.rate.cow.un
# mil.expenditure.per.capita.growth.rate.cow.alt.un
# mil.expenditure.per.capita.growth.rate.cow.cow
# mil.expenditure.per.capita.growth.rate.cow.alt.cow
# mil.expenditure.per.capita.growth.rate.wmeat.un
# mil.expenditure.per.capita.growth.rate.wmeat.cow
# mil.expenditure.per.capita.growth.rate.sipri.un
# mil.expenditure.per.capita.growth.rate.sipri.cow
# mil.expenditure.perc.gdp.growth.rate.cow.pwt
# mil.expenditure.perc.gdp.growth.rate.cow.alt.pwt
# mil.expenditure.perc.gdp.growth.rate.cow.gl
# mil.expenditure.perc.gdp.growth.rate.cow.alt.gl
# mil.expenditure.perc.gdp.growth.rate.wmeat.pwt
# mil.expenditure.perc.gdp.growth.rate.wmeat.gl
# mil.expenditure.perc.gdp.growth.rate.sipri.pwt
# mil.expenditure.perc.gdp.growth.rate.sipri.gl
# mil.expenditure.per.personnel.growth.rate.cow.cow
# mil.expenditure.per.personnel.growth.rate.cow.alt.cow
# mil.expenditure.per.personnel.growth.rate.cow.wmeat
# mil.expenditure.per.personnel.growth.rate.cow.alt.wmeat
# mil.expenditure.per.personnel.growth.rate.wmeat.cow
# mil.expenditure.per.personnel.growth.rate.wmeat.wmeat
# mil.expenditure.per.personnel.growth.rate.sipri.cow
# mil.expenditure.per.personnel.growth.rate.sipri.wmeat
# mil.personnel.per.capita.growth.rate.cow.un
# mil.personnel.per.capita.growth.rate.cow.cow
# mil.personnel.per.capita.growth.rate.wmeat.un
# mil.personnel.per.capita.growth.rate.wmeat.cow
# gdp.per.mil.personnel.growth.rate.pwt.cow
# gdp.per.mil.personnel.growth.rate.pwt.wmeat
# gdp.per.mil.personnel.growth.rate.gl.cow
# gdp.per.mil.personnel.growth.rate.gl.wmeat



# ### add estimates from workbook ----------------------------------------------------------------------
# #### estimates entered from US WMEAT data
# 
# # ZWE
# # 2008 and 2011 value is same (in constant 2014$), so applying deflated 2011 value to 2008
# # same relationship between 2009 and 2010
# cow$milex[cow$iso3c=="ZWE"&cow$year==2007] <- 219065654 # WMEAT 2019
# cow$milex[cow$iso3c=="ZWE"&cow$year==2008] <- 192778000
# cow$milex[cow$iso3c=="ZWE"&cow$year==2009] <- 96800000
# 
# # ZMB
# cow$milex[cow$iso3c=="ZMB"&cow$year==1992] <- (69631057+95294117)/2
# 
# # YEM
# # Estimates between IISS and WMEAT are pretty similar, so applying % change for 2013 and 2014 from 2012 value
# # remembering to account for inflation (current year average)
# cow$milex[cow$iso3c=="YEM"&cow$year==1990] <- 1403460990
# cow$milex[cow$iso3c=="YEM"&cow$year==2013] <- 1704977800
# cow$milex[cow$iso3c=="YEM"&cow$year==2014] <- 1879896330
# 
# # VNM
# # WMEAT 1998
# cow$milex[cow$iso3c=="VNM"&cow$year==1987] <- 1621512070
# 
# # UZB
# # same as with YEM for the 2014 value
# # same idea with 2008 and 2009, except average of values for base year of 2007 and for base year of 2010
# # current year average exchange
# cow$milex[cow$iso3c=="UZB"&cow$year==2008] <- (143184216 + 1105336326)/2
# cow$milex[cow$iso3c=="UZB"&cow$year==2009] <- (167397797 + 1182549110)/2
# cow$milex[cow$iso3c=="UZB"&cow$year==1992] <- 679637838
# cow$milex[cow$iso3c=="UZB"&cow$year==1993] <- 828225260
# 
# cow$milex[cow$iso3c=="UZB"&cow$year==2014] <- 1642609920 # WMEAT 2019
# cow$milex[cow$iso3c=="UZB"&cow$year==2015] <- 1936716510
# cow$milex[cow$iso3c=="UZB"&cow$year==2016] <- 1942890120
# cow$milex[cow$iso3c=="UZB"&cow$year==2017] <- 1632585014
# 
# # ARE
# # YEM method again
# # current year average exchange
# # WMEAT 2019
# cow$milex[cow$iso3c=="ARE"&cow$year==2014] <- 13536091400
# cow$milex[cow$iso3c=="ARE"&cow$year==2015] <- 14394973100
# cow$milex[cow$iso3c=="ARE"&cow$year==2016] <- 12256934900
# cow$milex[cow$iso3c=="ARE"&cow$year==2017] <- 11386074130
# 
# # TTO
# cow$milex[cow$iso3c=="TTO"&cow$year==1993] <- (75040040+6600493.68)/2
# 
# # TGO
# cow$milex[cow$iso3c=="TGO"&cow$year==1992] <- (46218975+35645550)/2
# 
# # TKM
# # UZB method for 2009 and 2011, YEM for 2014
# # didn't go well...
# # current yeare xchange rate
# cow$milex[cow$iso3c=="TKM"&cow$year==2009] <- (70889487 + 143753504)/2      # yikes
# cow$milex[cow$iso3c=="TKM"&cow$year==2011] <- (258326203 + 430526268)/2     # yikes 2
# # WMEAT 2019
# cow$milex[cow$iso3c=="TKM"&cow$year==2014] <- 736547836
# cow$milex[cow$iso3c=="TKM"&cow$year==2015] <- 662766616
# cow$milex[cow$iso3c=="TKM"&cow$year==2016] <- 788029028
# cow$milex[cow$iso3c=="TKM"&cow$year==2017] <- 852951344
# 
# # TLS
# # doesn't look promising...
# # current year exchange rate
# # am gonna have to use the 2004 estimate as a base for WMEAT 1995-2005 data
# cow$milex[cow$iso3c=="TLS"&cow$year==2009] <- 1152761.02
# cow$milex[cow$iso3c=="TLS"&cow$year==2008] <- 1344941.60
# cow$milex[cow$iso3c=="TLS"&cow$year==2007] <- 955451.79
# cow$milex[cow$iso3c=="TLS"&cow$year==2006] <- 777554.84
# cow$milex[cow$iso3c=="TLS"&cow$year==2005] <- 388091.46
# cow$milex[cow$iso3c=="TLS"&cow$year==2004] <- 173752.74
# cow$milex[cow$iso3c=="TLS"&cow$year==2003] <- 130821.42
# cow$milex[cow$iso3c=="TLS"&cow$year==2002] <- 128118.20
# 
# cow$milper[cow$iso3c=="TLS"&cow$year==2002] <- 1000
# cow$milper[cow$iso3c=="TLS"&cow$year==2003] <- 1000
# cow$milper[cow$iso3c=="TLS"&cow$year==2004] <- 1000
# 
# # SYR
# # current year exchange rate
# # WMEAT 2019
# cow$milex[cow$iso3c=="SYR"&cow$year==2011] <- 2569660550
# cow$milex[cow$iso3c=="SYR"&cow$year==2012] <- 2509339880
# cow$milex[cow$iso3c=="SYR"&cow$year==2013] <- 2066513110
# cow$milex[cow$iso3c=="SYR"&cow$year==2014] <- 1605855120
# cow$milex[cow$iso3c=="SYR"&cow$year==2015] <- 1332103790
# cow$milex[cow$iso3c=="SYR"&cow$year==2016] <- 836283600
# cow$milex[cow$iso3c=="SYR"&cow$year==2017] <- 1005413378
# 
# # SWZ
# # current year exchange rates
# # also troops
# # all from WMEAT 2004-2014 or WMEAT 1995-2005
# cow$milex[cow$iso3c=="SWZ"&cow$year==1992] <- (12885174+17124654)/2
# cow$milex[cow$iso3c=="SWZ"&cow$year==2002] <- 35950000
# cow$milex[cow$iso3c=="SWZ"&cow$year==2003] <- 43290000
# cow$milex[cow$iso3c=="SWZ"&cow$year==2004] <- 47700000
# cow$milex[cow$iso3c=="SWZ"&cow$year==2005] <- 61510000
# cow$milex[cow$iso3c=="SWZ"&cow$year==2006] <- 64060000
# cow$milex[cow$iso3c=="SWZ"&cow$year==2007] <- 69940000
# cow$milex[cow$iso3c=="SWZ"&cow$year==2008] <- 78480000
# cow$milex[cow$iso3c=="SWZ"&cow$year==2009] <- 81350000
# cow$milex[cow$iso3c=="SWZ"&cow$year==2010] <- 100660000
# cow$milex[cow$iso3c=="SWZ"&cow$year==2011] <- 85340000
# cow$milex[cow$iso3c=="SWZ"&cow$year==2012] <- 86550000
# cow$milex[cow$iso3c=="SWZ"&cow$year==2013] <- 83340000
# cow$milex[cow$iso3c=="SWZ"&cow$year==2014] <- 90000000
# 
# cow$milper[cow$iso3c=="SWZ"&cow$year==2001] <- 3000 # WMEAT 2005
# cow$milper[cow$iso3c=="SWZ"&cow$year==2002] <- 3000 # WMEAT 2005
# cow$milper[cow$iso3c=="SWZ"&cow$year==2003] <- 3000 # WMEAT 2005
# cow$milper[cow$iso3c=="SWZ"&cow$year==2004] <- 3000
# cow$milper[cow$iso3c=="SWZ"&cow$year==2005] <- 3000
# cow$milper[cow$iso3c=="SWZ"&cow$year==2006] <- 3000
# cow$milper[cow$iso3c=="SWZ"&cow$year==2007] <- 3000
# cow$milper[cow$iso3c=="SWZ"&cow$year==2008] <- 3000
# cow$milper[cow$iso3c=="SWZ"&cow$year==2009] <- 3000
# cow$milper[cow$iso3c=="SWZ"&cow$year==2010] <- 3000
# cow$milper[cow$iso3c=="SWZ"&cow$year==2011] <- 3000
# cow$milper[cow$iso3c=="SWZ"&cow$year==2012] <- 3000
# cow$milper[cow$iso3c=="SWZ"&cow$year==2013] <- 3000
# cow$milper[cow$iso3c=="SWZ"&cow$year==2014] <- 3000
# 
# # SVN
# cow$milex[cow$iso3c=="SVN"&cow$year==1993] <- (291354249+300709738)/2
# 
# # SOM
# # current year exchange rates
# # all based on 2009
# # 2015-17 based on 2014 estimate using WMEAT 2019
# # milper 2005-17 from WMEAT
# cow$milex[cow$iso3c=="SOM"&cow$year==2005] <- 24553066
# cow$milex[cow$iso3c=="SOM"&cow$year==2006] <- 31973988
# cow$milex[cow$iso3c=="SOM"&cow$year==2007] <- 39282123
# cow$milex[cow$iso3c=="SOM"&cow$year==2008] <- 51863698
# cow$milex[cow$iso3c=="SOM"&cow$year==2010] <- 27394260
# cow$milex[cow$iso3c=="SOM"&cow$year==2011] <- 28388320
# cow$milex[cow$iso3c=="SOM"&cow$year==2012] <- 39601268
# cow$milex[cow$iso3c=="SOM"&cow$year==2013] <- 55077887
# cow$milex[cow$iso3c=="SOM"&cow$year==2014] <- 59923480
# cow$milex[cow$iso3c=="SOM"&cow$year==2015] <- 63566573
# cow$milex[cow$iso3c=="SOM"&cow$year==2016] <- 64092357
# cow$milex[cow$iso3c=="SOM"&cow$year==2017] <- 68832334
# 
# cow$milper[cow$iso3c=="SOM"&cow$year==2005] <- 10000
# cow$milper[cow$iso3c=="SOM"&cow$year==2006] <- 12000
# cow$milper[cow$iso3c=="SOM"&cow$year==2007] <- 15000
# cow$milper[cow$iso3c=="SOM"&cow$year==2008] <- 18000
# cow$milper[cow$iso3c=="SOM"&cow$year==2009] <- 20000
# cow$milper[cow$iso3c=="SOM"&cow$year==2010] <- 25000
# cow$milper[cow$iso3c=="SOM"&cow$year==2011] <- 26000
# cow$milper[cow$iso3c=="SOM"&cow$year==2012] <- 26000
# cow$milper[cow$iso3c=="SOM"&cow$year==2013] <- 26000
# cow$milper[cow$iso3c=="SOM"&cow$year==2014] <- 26000
# cow$milper[cow$iso3c=="SOM"&cow$year==2015] <- 31000
# cow$milper[cow$iso3c=="SOM"&cow$year==2016] <- 31000
# cow$milper[cow$iso3c=="SOM"&cow$year==2017] <- 31000
# 
# # SLE
# cow$milex[cow$iso3c=="SLE"&cow$year==1992] <- (16711454+13135388)/2
# 
# # SDN
# # current year exchange rate
# # oh boi...
# cow$milex[cow$iso3c=="SDN"&cow$year==2006] <- (1564434450+523527874)/2
# cow$milex[cow$iso3c=="SDN"&cow$year==2007] <- (1910377390+639547960)/2
# cow$milex[cow$iso3c=="SDN"&cow$year==2008] <- (2209490480+740056140)/2
# cow$milex[cow$iso3c=="SDN"&cow$year==2012] <- (1136922580+2128393150)/2
# 
# cow$milex[cow$iso3c=="SDN"&cow$year==2014] <- 1970218460 # WMEAT 2019
# cow$milex[cow$iso3c=="SDN"&cow$year==2015] <- 1876911000
# cow$milex[cow$iso3c=="SDN"&cow$year==2016] <- 2239349720
# cow$milex[cow$iso3c=="SDN"&cow$year==2017] <- 3564773954
# cow$milex[cow$iso3c=="SDN"&cow$year==2018] <- 1869674951 # SIPRI
# cow$milex[cow$iso3c=="SDN"&cow$year==2019] <- 1728178030
# 
# # PAN
# cow$milper[cow$iso3c=="PAN"&cow$year==2000] <- 7000
# cow$milper[cow$iso3c=="PAN"&cow$year==2000] <- 6000
# 
# # QAT
# cow$milex[cow$iso3c=="QAT"&cow$year==1982] <- (1249520760+269814967)/2
# cow$milex[cow$iso3c=="QAT"&cow$year==1989] <- (119084979+869239270)/2
# cow$milex[cow$iso3c=="QAT"&cow$year==1990] <- (112955020+824494470)/2
# cow$milex[cow$iso3c=="QAT"&cow$year==1992] <- (811511751+802983472)/2
# cow$milex[cow$iso3c=="QAT"&cow$year==2015] <- 4790830050 # WMEAT 2019
# cow$milex[cow$iso3c=="QAT"&cow$year==2016] <- 4367966320 # WMEAT 2019
# cow$milex[cow$iso3c=="QAT"&cow$year==2017] <- 3816134115 # WMEAT 2019
# 
# cow$milper[cow$iso3c=="QAT"&cow$year==2015] <- 12000 # WMEAT 2019
# 
# # NER
# cow$milex[cow$iso3c=="NER"&cow$year==2014] <- (104056653+176159287)/2
# 
# # MUS
# cow$milper[cow$iso3c=="MUS"&cow$year==2000] <- 500
# cow$milper[cow$iso3c=="MUS"&cow$year==2001] <- 500
# cow$milper[cow$iso3c=="MUS"&cow$year==2002] <- 500
# cow$milper[cow$iso3c=="MUS"&cow$year==2003] <- 500
# cow$milper[cow$iso3c=="MUS"&cow$year==2004] <- 0
# 
# # MRT
# cow$milex[cow$iso3c=="MRT"&cow$year==1990] <- (38703932+37913656)/2
# 
# # MMR
# # estimates from WMEAT 2019
# cow$milex[cow$iso3c=="MRT"&cow$year==2008] <- (11102268900+1242671400)/2
# cow$milex[cow$iso3c=="MRT"&cow$year==2009] <- (12108552200+1355850700)/2
# 
# # MLI
# # from WMEAT 2019
# cow$milper[cow$iso3c=="MLI"&cow$year==2017] <- 6858
# 
# # MDG
# cow$milex[cow$iso3c=="MDG"&cow$year==1999] <- (43436994+227536707)/2
# cow$milex[cow$iso3c=="MDG"&cow$year==2000] <- (47647058+249356829)/2
# cow$milex[cow$iso3c=="MDG"&cow$year==2001] <- (61423483+321401152)/2
# 
# cow$milper[cow$iso3c=="MDG"&cow$year==2000] <- 15833
# cow$milper[cow$iso3c=="MDG"&cow$year==2001] <- 15833
# 
# # LBY
# # current year exchange rate
# cow$milex[cow$iso3c=="LBY"&cow$year==2011] <- (3911403300+2047815400)/2
# cow$milex[cow$iso3c=="LBY"&cow$year==1993] <- (1626880590+1532047120)/2
# 
# # WMEAT 2019
# cow$milex[cow$iso3c=="LBY"&cow$year==2014] <- 4653787560
# cow$milex[cow$iso3c=="LBY"&cow$year==2015] <- 3329770700
# cow$milex[cow$iso3c=="LBY"&cow$year==2016] <- 3453235740
# cow$milex[cow$iso3c=="LBY"&cow$year==2017] <- 3171374297
# 
# cow$milper[cow$iso3c=="LBY"&cow$year==2012] <- cow$milper[cow$iso3c=="LBY"&cow$year==2014]
# cow$milper[cow$iso3c=="LBY"&cow$year==2013] <- cow$milper[cow$iso3c=="LBY"&cow$year==2014]
# cow$milper[cow$iso3c=="LBY"&cow$year==2015] <- cow$milper[cow$iso3c=="LBY"&cow$year==2014]
# cow$milper[cow$iso3c=="LBY"&cow$year==2016] <- cow$milper[cow$iso3c=="LBY"&cow$year==2014]
# cow$milper[cow$iso3c=="LBY"&cow$year==2017] <- cow$milper[cow$iso3c=="LBY"&cow$year==2014]
# 
# # LBR
# # current year exchange rate
# cow$milex[cow$iso3c=="LBR"&cow$year==2008] <- 4085684.73
# cow$milex[cow$iso3c=="LBR"&cow$year==2007] <- 3869015.13
# cow$milex[cow$iso3c=="LBR"&cow$year==2006] <- 4724846.34
# cow$milex[cow$iso3c=="LBR"&cow$year==2005] <- 8166206.01
# cow$milex[cow$iso3c=="LBR"&cow$year==2004] <- 3517978.80
# 
# # LBN
# cow$milex[cow$iso3c=="LBN"&cow$year==1987] <- (25869182+36360236)/2 # SIPRI
# 
# # LAO
# # current year exchange rate
# cow$milex[cow$iso3c=="LAO"&cow$year==2004] <- 11631529
# # WMEAT 2019
# cow$milex[cow$iso3c=="LAO"&cow$year==2015] <- 28848836
# cow$milex[cow$iso3c=="LAO"&cow$year==2016] <- 29087456
# cow$milex[cow$iso3c=="LAO"&cow$year==2017] <- 34521761
# 
# # KHM
# # from SIPRI
# cow$milex[cow$iso3c=="KHM"&cow$year==1986] <- 2525388.34
# cow$milex[cow$iso3c=="KHM"&cow$year==1987] <- 2380925.17
# cow$milex[cow$iso3c=="KHM"&cow$year==1988] <- 3738665.92
# cow$milex[cow$iso3c=="KHM"&cow$year==1989] <- 7200664
# cow$milex[cow$iso3c=="KHM"&cow$year==1990] <- 17549074
# 
# # KGZ
# # from SIPRI
# cow$milex[cow$iso3c=="KGZ"&cow$year==2015] <- 94016083
# cow$milex[cow$iso3c=="KGZ"&cow$year==2016] <- 101223696
# cow$milex[cow$iso3c=="KGZ"&cow$year==2017] <- 102972820
# cow$milex[cow$iso3c=="KGZ"&cow$year==2018] <- 109543626
# cow$milex[cow$iso3c=="KGZ"&cow$year==2019] <- 108891166
# 
# # KSV
# # completely from WMEAT 2019, 2018-19 from SIPRI
# cow$milex[cow$iso3c=="KSV"&cow$year==2008] <- 25000000
# cow$milex[cow$iso3c=="KSV"&cow$year==2009] <- 30000000
# cow$milex[cow$iso3c=="KSV"&cow$year==2010] <- 40000000
# cow$milex[cow$iso3c=="KSV"&cow$year==2011] <- 50000000
# cow$milex[cow$iso3c=="KSV"&cow$year==2012] <- 45000000
# cow$milex[cow$iso3c=="KSV"&cow$year==2013] <- 50000000
# cow$milex[cow$iso3c=="KSV"&cow$year==2014] <- 55000000
# cow$milex[cow$iso3c=="KSV"&cow$year==2015] <- 50000000
# cow$milex[cow$iso3c=="KSV"&cow$year==2016] <- 50000000
# cow$milex[cow$iso3c=="KSV"&cow$year==2017] <- 55000000
# cow$milex[cow$iso3c=="KSV"&cow$year==2018] <- 60488650
# cow$milex[cow$iso3c=="KSV"&cow$year==2019] <- 64469293
# 
# cow$milper[cow$iso3c=="KSV"&cow$year==2008] <- 3000
# cow$milper[cow$iso3c=="KSV"&cow$year==2009] <- 1000
# cow$milper[cow$iso3c=="KSV"&cow$year==2010] <- 2000
# cow$milper[cow$iso3c=="KSV"&cow$year==2011] <- 2000
# cow$milper[cow$iso3c=="KSV"&cow$year==2012] <- 2000
# cow$milper[cow$iso3c=="KSV"&cow$year==2013] <- 2000
# cow$milper[cow$iso3c=="KSV"&cow$year==2014] <- 2000
# cow$milper[cow$iso3c=="KSV"&cow$year==2015] <- 2000
# cow$milper[cow$iso3c=="KSV"&cow$year==2016] <- 2000
# cow$milper[cow$iso3c=="KSV"&cow$year==2017] <- 2000
# 
# # PRK
# # current year exchange rate
# # all from WMEAT, 2004-2014 (2015-17 from WMEAT 2019)
# # WMEAT 1995-2005 estimates only use 2001 estimate as base, not estimated 2004 base
# cow$milex[cow$iso3c=="PRK"&cow$year==2002] <- 2133067870
# cow$milex[cow$iso3c=="PRK"&cow$year==2003] <- 2335167380
# cow$milex[cow$iso3c=="PRK"&cow$year==2004] <- 2591580000
# cow$milex[cow$iso3c=="PRK"&cow$year==2005] <- 3001740000
# cow$milex[cow$iso3c=="PRK"&cow$year==2006] <- 3220130000
# cow$milex[cow$iso3c=="PRK"&cow$year==2007] <- 3392240000
# cow$milex[cow$iso3c=="PRK"&cow$year==2008] <- 3249840000
# cow$milex[cow$iso3c=="PRK"&cow$year==2009] <- 2576080000
# cow$milex[cow$iso3c=="PRK"&cow$year==2010] <- 3001370000
# cow$milex[cow$iso3c=="PRK"&cow$year==2011] <- 3451650000
# cow$milex[cow$iso3c=="PRK"&cow$year==2012] <- 3461800000
# cow$milex[cow$iso3c=="PRK"&cow$year==2013] <- 3931670000
# cow$milex[cow$iso3c=="PRK"&cow$year==2014] <- 4170000000
# cow$milex[cow$iso3c=="PRK"&cow$year==2015] <- 3950000000
# cow$milex[cow$iso3c=="PRK"&cow$year==2016] <- 3990000000
# cow$milex[cow$iso3c=="PRK"&cow$year==2017] <- 4170000000
# 
# # IRQ
# # current year exchange rate
# cow$milex[cow$iso3c=="IRQ"&cow$year==2008] <- 3768765190
# cow$milex[cow$iso3c=="IRQ"&cow$year==2007] <- 2326159520
# cow$milex[cow$iso3c=="IRQ"&cow$year==2006] <- 1494290720
# cow$milex[cow$iso3c=="IRQ"&cow$year==2005] <- 571683230
# cow$milex[cow$iso3c=="IRQ"&cow$year==2004] <- 452719760
# cow$milex[cow$iso3c=="IRQ"&cow$year==2003] <- (1362862870+541987061)/2
# cow$milex[cow$iso3c=="IRQ"&cow$year==2002] <- (734899627+291876604)/2
# 
# # IRN
# # WMEAT 1987
# cow$milex[cow$iso3c=="IRN"&cow$year==1981] <- (3774584210+13815099200)/2
# 
# # HTI
# # milex from SIPRI, milper from WMEAT
# cow$milex[cow$iso3c=="HTI"&cow$year==1993] <- 53939521 # SIPRI
# cow$milex[cow$iso3c=="HTI"&cow$year==1994] <- (45260643+47069933)/2 # SIPRI
# 
# cow$milper[cow$iso3c=="HTI"&cow$year==2000] <- 0
# cow$milper[cow$iso3c=="HTI"&cow$year==2001] <- 0
# cow$milper[cow$iso3c=="HTI"&cow$year==2002] <- 0
# cow$milper[cow$iso3c=="HTI"&cow$year==2003] <- 0
# cow$milper[cow$iso3c=="HTI"&cow$year==2004] <- 0
# cow$milper[cow$iso3c=="HTI"&cow$year==2007] <- 0
# cow$milper[cow$iso3c=="HTI"&cow$year==2008] <- 0
# 
# # GNQ
# # current year exchange rate
# # looks like a bad estimate
# # 2015-16 from SIPRI, based on 2014 estimate
# cow$milex[cow$iso3c=="GNQ"&cow$year==2014] <- 3761533
# cow$milex[cow$iso3c=="GNQ"&cow$year==2015] <- 3653752.15 # estimate corroberated by using 2009 as base
# cow$milex[cow$iso3c=="GNQ"&cow$year==2016] <- 3694566.86
# 
# # GNB
# # current year exchange rate
# cow$milex[cow$iso3c=="GNB"&cow$year==2014] <- 26518006 # WMEAT 2019
# cow$milex[cow$iso3c=="GNB"&cow$year==2015] <- 21250378 # WMEAT 2019
# cow$milex[cow$iso3c=="GNB"&cow$year==2016] <- 26782685 # WMEAT 2019
# cow$milex[cow$iso3c=="GNB"&cow$year==2017] <- 32694581 # WMEAT 2019
# 
# cow$milex[cow$iso3c=="GNB"&cow$year==1992] <- (8755837+5890985.48)/2
# cow$milex[cow$iso3c=="GNB"&cow$year==1993] <- (11569992+7784366.58)/2
# 
# # GMB
# # milex from SIPRI
# cow$milex[cow$iso3c=="GMB"&cow$year==1984] <- (10387762+1223912.98)/2
# cow$milex[cow$iso3c=="GMB"&cow$year==1986] <- (1660180.35+874869.40)/2
# cow$milex[cow$iso3c=="GMB"&cow$year==1992] <- (1235467.63+1185132.88)/2
# cow$milex[cow$iso3c=="GMB"&cow$year==1991] <- (1518429.54+4117008.00)/2
# cow$milex[cow$iso3c=="GMB"&cow$year==1990] <- (3609252.02+3462206.10)/2
# 
# cow$milex[cow$iso3c=="GMB"&cow$year==2015] <- 13973655 # WMEAT 2017
# cow$milex[cow$iso3c=="GMB"&cow$year==2016] <- 11059294 # WMEAT 2017
# cow$milex[cow$iso3c=="GMB"&cow$year==2017] <- 10325706 # WMEAT 2017
# cow$milex[cow$iso3c=="GMB"&cow$year==2018] <- 10919009 # SIPRI based on 2014
# cow$milex[cow$iso3c=="GMB"&cow$year==2019] <- 13686750 # SIPRI based on 2014
# 
# # GIN
# # current year exchange rate
# cow$milex[cow$iso3c=="GIN"&cow$year==2014] <- 48956316
# cow$milex[cow$iso3c=="GIN"&cow$year==1992] <- (44748849+3844632.05)/2
# 
# # GEO
# cow$milex[cow$iso3c=="GEO"&cow$year==1993] <- (327089949+50505452)/2
# 
# # GAB
# cow$milex[cow$iso3c=="GAB"&cow$year==1993] <- 137338468
# cow$milex[cow$iso3c=="GAB"&cow$year==1992] <- 124634916
# 
# # ERI
# # current year exchange rate
# cow$milex[cow$iso3c=="ERI"&cow$year==2006] <- (67694469.3+68032088.8)/2
# cow$milex[cow$iso3c=="ERI"&cow$year==2007] <- (69290861.3+69636442.6)/2
# cow$milex[cow$iso3c=="ERI"&cow$year==2008] <- (67098022.6+67401515.7)/2
# 
# cow$milex[cow$iso3c=="ERI"&cow$year==2014] <- 88304959 # WMEAT 2019
# cow$milex[cow$iso3c=="ERI"&cow$year==2015] <- 100408034 # WMEAT 2019
# cow$milex[cow$iso3c=="ERI"&cow$year==2016] <- 110076834 # WMEAT 2019
# cow$milex[cow$iso3c=="ERI"&cow$year==2017] <- 129143593 # WMEAT 2019
# 
# # ECU
# cow$milex[cow$iso3c=="ECU"&cow$year==1992] <- (278590655+580866235)/2
# 
# # DJI
# # current year exchange rate
# cow$milex[cow$iso3c=="DJI"&cow$year==2014] <- 12240104
# 
# # CRI
# cow$milper[cow$iso3c=="CRI"&cow$year==2000] <- 5000
# cow$milper[cow$iso3c=="CRI"&cow$year==2001] <- 4000
# 
# # CUB
# # current year exchange rate
# cow$milex[cow$iso3c=="CUB"&cow$year==2012] <- 128504790 # WMEAT 2017
# cow$milex[cow$iso3c=="CUB"&cow$year==2013] <- 120105662 # WMEAT 2017
# cow$milex[cow$iso3c=="CUB"&cow$year==2014] <- 122498562 # WMEAT 2017
# cow$milex[cow$iso3c=="CUB"&cow$year==2015] <- 115607571 # WMEAT 2017
# cow$milex[cow$iso3c=="CUB"&cow$year==2016] <- 116563807 # WMEAT 2017
# cow$milex[cow$iso3c=="CUB"&cow$year==2017] <- 113377214 # WMEAT 2017
# cow$milex[cow$iso3c=="CUB"&cow$year==1993] <- 690782377
# cow$milex[cow$iso3c=="CUB"&cow$year==1994] <- 696081148 # number from COW looked wack so I replaced it
# cow$milex[cow$iso3c=="CUB"&cow$year==2018] <- 111709264 # SIPRI based on 2011
# 
# # COG
# # yikes [2002]
# cow$milex[cow$iso3c=="COG"&cow$year==2002] <- (120411260+47896107)/2
# cow$milex[cow$iso3c=="COG"&cow$year==1991] <- (144123264+140521265)/2
# cow$milex[cow$iso3c=="COG"&cow$year==1993] <- (107045916+74721810)/2
# 
# 
# 
# # CAF
# # only based on 1990, not 1992 (1992 was a less certain estimate)
# cow$milex[cow$iso3c=="CAF"&cow$year==1991] <- 23453685
# 
# # BEN
# # current year exchange rate
# cow$milex[cow$iso3c=="BEN"&cow$year==2005] <- (77169550+47049614)/2
# cow$milex[cow$iso3c=="BEN"&cow$year==1993] <- (39606764+12510588)/2
# 
# # BLR
# cow$milper[cow$iso3c=="BLR"&cow$year==1997] <- 78000
# cow$milper[cow$iso3c=="BLR"&cow$year==1998] <- 78000
# 
# # BDI
# cow$milex[cow$iso3c=="BDI"&cow$year==1992] <- (30834695+26270776)/2
# 
# # AGO
# cow$milex[cow$iso3c=="AGO"&cow$year==1992] <- (690047211+1800969870)/2
# 
# # AFG
# # current year exchange rate
# cow$milex[cow$iso3c=="AFG"&cow$year==2004] <- 112438100
# cow$milex[cow$iso3c=="AFG"&cow$year==2002] <- 69068457
# cow$milex[cow$iso3c=="AFG"&cow$year==2003] <- 133300236
# 
# cow$milper[cow$iso3c=="AFG"&cow$year==2001] <- 45000 # WMEAT 2007(?)
# 
# # remove YUG values for 2012 (SRB values for 2012 already in dataset)
# cow <- cow %>%
#   filter(iso3c != "YUG" | year != 2012)
# 
# ### inflation ----------------------------------------------------------------------
# # inflation from https://www.bls.gov/data/inflation_calculator.htm
# # July to July, converting to 2019 dollars
# inflation_table <- data.frame(year = c(1946:2018),
#                               multiplier = c(1+11.96,1+10.56,1+9.52,1+9.83, #40s
#                                              1+9.65,1+8.91,1+8.61,1+8.57,1+8.54,1+8.57,1+8.36,1+8.07,1+7.85,1+7.79, #50s
#                                              1+7.67,1+7.55,1+7.47,1+7.36,1+7.25,1+7.12,1+6.89,1+6.68,1+6.35,1+5.97, #60s
#                                              1+5.58,1+5.30,1+5.12,1+4.79,1+4.19,1+3.73,1+3.49,1+3.21,1+2.91,1+2.51, #70s
#                                              1+2.10,1+1.80,1+1.63,1+1.57,1+1.46,1+1.38,1+1.34,1+1.25,1+1.17,1+1.06, #80s
#                                              1+0.97,1+0.88,1+0.83,1+0.78,1+0.73,1+0.68,1+0.63,1+0.60,1+0.57,1+0.54, #90s
#                                              1+0.48,1+0.45,1+0.42,1+0.40,1+0.35,1+0.31,1+0.26,1+0.23,1+0.17,1+0.19, #00s
#                                              1+0.18,1+0.14,1+0.12,1+0.10,1+0.08,1+0.08,1+0.07,1+0.05,1+0.02)) #10s
# 
# cow <- cow %>%
#   dplyr::left_join(inflation_table,by="year") %>%
#   # multiples dollar values in that year's dollar value to 2019 dollars
#   dplyr::mutate(milex = milex * multiplier,
#                 milexpc = milexpc * multiplier) %>%
#   dplyr::select(-multiplier)
# 
# ### calculate unified/divided country data ----------------------------------------------------------------------
# cow <- cow %>%
#   # expand dataframe to have all iso3c-year combos from 1946 - 2019
#   dplyr::full_join(expand.grid(iso3c = unique(cow$iso3c), year = c(1946:2019))) %>%
#   # change YUG coding to SRB for 1992 onward
#   dplyr::mutate(iso3c = ifelse(iso3c=="YUG"&year>1991,"SRB",iso3c),
#                 # change RUS coding to SOV for 1991 and before
#                 iso3c = ifelse(iso3c=="RUS"&year<=1991,"SOV",iso3c)) %>%
#   # filter out countries that merged together or split apart
#   dplyr::filter(iso3c %!in% c("YAR","YPR") | year <= 1989,
#                 iso3c != "YEM" | year >= 1990,
#                 iso3c %!in% c("BRD","DDR") | year <= 1990,
#                 iso3c != "DEU" | year >= 1991,
#                 iso3c %!in% c("ARM","AZE","BLR","EST","GEO","KAZ","KGZ","LTU","LVA","MDA",
#                               "RUS","TJK","TKM","UZB","BIH","HRV","MKD","SRB","SVN") | year >= 1992,
#                 iso3c != "KSV" | year >= 2008,
#                 # filter out Palestine for lack of data
#                 iso3c != "PSE")
# 
# ### interpolate missing values ----------------------------------------------------------------------
# cow <- cow %>%
#   dplyr::group_by(iso3c) %>%
#   dplyr::arrange(year) %>%
#   dplyr::mutate(milex.approx = imputeTS::na_interpolation(milex, option = "linear"),
#                 milper.approx = imputeTS::na_interpolation(milper, option = "linear")) %>%
#   dplyr::ungroup() %>%
#   dplyr::select(-c(milex,milper))
# 
# ### merge population and GDP data ----------------------------------------------------------------------
# # load formatted data output by scripts 003-GDP and 004-Population
# gdp <- read.csv("Data files/Formatted data files/gdp.csv")
# population <- read.csv("Data files/Formatted data files/population.csv")
# 
# # merge files by iso3c and year
# cow <- cow %>%
#   dplyr::left_join(gdp,by=c("iso3c","year")) %>%
#   dplyr::left_join(population,by=c("iso3c","year")) %>%
#   # gdp and population data not available for 2020
#   dplyr::filter(year < 2020) %>%
#   dplyr::relocate(country,.after="iso3c") %>%
#   dplyr::mutate(milexpc_est = milex.approx / population,
#                 milexpgdp_est = milex.approx / gdp,
#                 milexpc_ratio = milexpc_est / milexpc,
#                 milexpgdp_ratio = milexpgdp_est / milexpgdp)
# 
# # ranges of milexpc estimate ratios (milex.approx / population vs. milexpc) range from 58% - 431% of milexpc
# # ranges of milexpgdp estimate ratios (milex.approx / gdp vs. milexpgdp) range from 0.34% - 218% of milexpgdp
# 
# # replaces several calculations for specfic county-years
# cow$milex.approx[cow$iso3c=="ARE"&cow$year==2014] <- cow$milexpgdp[cow$iso3c=="ARE"&cow$year==2014] * 
#   cow$gdp[cow$iso3c=="ARE"&cow$year==2014]
# cow$milex.approx[cow$iso3c=="GIN"&cow$year==2014] <- cow$milexpgdp[cow$iso3c=="GIN"&cow$year==2014] * 
#   cow$gdp[cow$iso3c=="GIN"&cow$year==2014]
# cow$milex.approx[cow$iso3c=="GNB"&cow$year==2014] <- cow$milexpgdp[cow$iso3c=="GNB"&cow$year==2014] * 
#   cow$gdp[cow$iso3c=="GNB"&cow$year==2014]
# cow$milex.approx[cow$iso3c=="NER"&cow$year==2014] <- cow$milexpgdp[cow$iso3c=="NER"&cow$year==2014] * 
#   cow$gdp[cow$iso3c=="NER"&cow$year==2014]
# cow$milex.approx[cow$iso3c=="SDN"&cow$year==2014] <- cow$milexpgdp[cow$iso3c=="SDN"&cow$year==2014] * 
#   cow$gdp[cow$iso3c=="SDN"&cow$year==2014]
# 
# cow <- cow %>%
#   # calculates military expenditure per personnel (milexpt) and military personnel per capita (miltppc)
#   dplyr::mutate(milexpt = milex.approx / milper.approx,
#                 miltppc = milper.approx / population,
#                 # if estimates are not available for milexpgdp or milexpc, use milex.approx / gdp or population as estimates
#                 milexpgdp = dplyr::coalesce(milexpgdp,milexpgdp_est),
#                 milexpc = dplyr::coalesce(milexpc,milexpc_est)) %>%
#   dplyr::select(-c(gdp,population,milexpc_est,milexpgdp_est,milexpc_ratio,milexpgdp_ratio)) %>%
#   dplyr::rename(milex = milex.approx,
#                 milper = milper.approx)
# 
# # calculate how many variables are present for each country (out of 6 total)
# cow.names <- cow %>%
#   tidyr::pivot_longer(4:9, names_to = "variable", values_to = "value") %>%
#   na.omit() %>%
#   dplyr::group_by(iso3c,variable) %>%
#   dplyr::tally() %>%
#   dplyr::ungroup() %>%
#   dplyr::group_by(iso3c) %>%
#   dplyr::tally() %>%
#   dplyr::ungroup()
# 
# countries_missing_variables <- cow.names %>%
#   dplyr::filter(n < 6) %>%
#   dplyr::pull(iso3c)
# 
# ### interpolate missing values again ----------------------------------------------------------------------
# cow <- cow %>%
#   dplyr::filter(iso3c %!in% c("BLZ","MDV","ZAN",countries_missing_variables)) %>%
#   dplyr::group_by(iso3c) %>%
#   dplyr::arrange(year) %>%
#   dplyr::mutate(milexpc = imputeTS::na_interpolation(milexpc, option = "linear"),
#                 milexpgdp = imputeTS::na_interpolation(milexpgdp, option = "linear"),
#                 milex = imputeTS::na_interpolation(milex, option = "linear"),
#                 milper = imputeTS::na_interpolation(milper, option = "linear"),
#                 milexpt = imputeTS::na_interpolation(milexpt, option = "linear"),
#                 miltppc = imputeTS::na_interpolation(miltppc, option = "linear")) %>%
#   dplyr::ungroup() %>%
#   # replace all Infs and NaNs with 0s
#   dplyr::mutate_all(~ifelse(is.infinite(.), 0, .),
#                     ~ifelse(is.nan(.), 0, .)) %>%
#   # calculate natural logs of milexpgdp, miltppc, milexpc, and milexpt
#   dplyr::mutate(lnmilexpgdp = log(milexpgdp),
#                 lntroops = log(miltppc * 1000000),
#                 lntroops2 = log(miltppc),
#                 lnmilexpc = log(milexpc),
#                 lnmilexpt = log(milexpt)) %>%
#   # replace all Infs and NaNs with 0s
#   dplyr::mutate_all(~ifelse(is.infinite(.), 0, .),
#                     ~ifelse(is.nan(.), 0, .))
