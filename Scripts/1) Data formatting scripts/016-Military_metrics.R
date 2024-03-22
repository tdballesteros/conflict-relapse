# This script formats multiple military-related government metrics.

#TODO: weight for PCA
#      Issue with GDPs in 2018-19
#      Weirdness with population b/t 1991-92 (might be how YUG and SOV are coded)
#      Add PSE
#      Issue with UKR 1991 (2x, v diff values)

### load libraries ----------------------------------------------------------------------
library(readxl)
library(countrycode)
library(dplyr)
library(tidyr)

### not in function ----------------------------------------------------------------------
'%!in%' <- function(x,y)!('%in%'(x,y))

### load data files ----------------------------------------------------------------------
cow <- read.csv("Data files/Raw data files/NMC_5_0.csv")

# load all sheets in the "Military Balance.xlsx" file
military.balance <- lapply(readxl::excel_sheets("Data files/Raw data files/Military Balance.xlsx"),
                           read_xlsx, na = c("n.k.","n/a","n.k","n.a.","n.a"),
                           path = "Data files/Raw data files/Military Balance.xlsx")

# WMEAT files
wmeat.1995 <- lapply(readxl::excel_sheets("Data files/Raw data files/185685.xlsx"),
                     read_xlsx, skip = 5, na = c("NA","E","R","d","E,b","b","c","E,d","c,f"), col_names = FALSE,
                     path = "Data files/Raw data files/185685.xlsx")
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

### inflation table ----------------------------------------------------------------------
# inflation from https://www.bls.gov/data/inflation_calculator.htm
# July to July, converting to 2019 dollars
inflation_table <- data.frame(year = c(1946:2020),
                              multiplier = c(1+11.96,1+10.56,1+9.52,1+9.83, #40s
                                             1+9.65,1+8.91,1+8.61,1+8.57,1+8.54,1+8.57,1+8.36,1+8.07,1+7.85,1+7.79, #50s
                                             1+7.67,1+7.55,1+7.47,1+7.36,1+7.25,1+7.12,1+6.89,1+6.68,1+6.35,1+5.97, #60s
                                             1+5.58,1+5.30,1+5.12,1+4.79,1+4.19,1+3.73,1+3.49,1+3.21,1+2.91,1+2.51, #70s
                                             1+2.10,1+1.80,1+1.63,1+1.57,1+1.46,1+1.38,1+1.34,1+1.25,1+1.17,1+1.06, #80s
                                             1+0.97,1+0.88,1+0.83,1+0.78,1+0.73,1+0.68,1+0.63,1+0.60,1+0.57,1+0.54, #90s
                                             1+0.48,1+0.45,1+0.42,1+0.40,1+0.35,1+0.31,1+0.26,1+0.23,1+0.17,1+0.19, #00s
                                             1+0.18,1+0.14,1+0.12,1+0.10,1+0.08,1+0.08,1+0.07,1+0.05,1+0.02,1+0.00, #10s
                                             1-0.01)) #20s

### format data ----------------------------------------------------------------------
#### COW ----------------------------------------------------------------------
cow <- cow %>%
  dplyr::filter(year >= 1945) %>%
  # using the countrycode package, add iso3c based on country COW abbreviation
  dplyr::mutate(iso3c = countrycode::countrycode(stateabb,"cowc","iso3c"))

# codes iso3c values missing from the countrycode package: RVN, YPR, YAR, ZAN, KSV, YUG, CZE, DDR, BRD
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
  dplyr::select(iso3c,year,milex,milper) %>%
  # recodes -9 (missing) to NA for milex and milper variables
  dplyr::mutate(milex = ifelse(milex==-9,NA,milex),
                milper = ifelse(milper==-9,NA,milper),
                # multiplies milex and milper to be full numbers
                milex = 1000 * milex,
                milper = 1000 * milper)

# adjust for inflation
cow <- cow %>%
  dplyr::left_join(inflation_table,by="year") %>%
  # multiples dollar values in that year's dollar value to 2019 dollars
  dplyr::mutate(milex = milex * multiplier) %>%
  dplyr::select(-multiplier) %>%
  dplyr::rename(milex.cow = milex,
                milper.cow = milper)

#### Military Balance ----------------------------------------------------------------------
# rename datasets as Y20XX, referring to the year the report was published
names(military.balance) <- readxl::excel_sheets("Data files/Raw data files/Military Balance.xlsx")

# create empty dataframe to compile all formatted sheets into
military.balance.data <- data.frame()

# format each dataset and combine into one complete dataset
for(m in names(military.balance)){
  
  year <- as.numeric(stringr::str_sub(m, start = 2, end = 5))
  
  df <- military.balance[[m]]
  
  # remove footnotes column (column  14) if it exists
  if(ncol(df) == 14){
    
    df <- df[,-14]
    
  }
  
  names(df) <- c("country",
                 paste0("defense.spending.",year-3), # in current year USD
                 paste0("defense.spending.",year-2),
                 paste0("defense.spending.",year-1),
                 paste0("defense.spending.percapita.",year-3), # in current year USD
                 paste0("defense.spending.percapita.",year-2),
                 paste0("defense.spending.percapita.",year-1),
                 paste0("defense.spending.percgdp.",year-3), # as percentage of GDP
                 paste0("defense.spending.percgdp.",year-2),
                 paste0("defense.spending.percgdp.",year-1),
                 paste0("active.armed.forces.",year),
                 paste0("reservists.",year),
                 paste0("active.paramilitary.",year))
  
  # remove first row (secondary header row containing years)
  df <- df[-1,]
  
  # remove footnotes rows at the end, based on which year it is
  number_of_footnote_rows <- ifelse(year == 2020, 4,
                             ifelse(year == 2019, 5,
                             ifelse(year == 2018, 6,
                             ifelse(year == 2017, 5,
                             ifelse(year == 2016, 3,
                             ifelse(year == 2015, 4,
                             ifelse(year == 2014, 14,
                             ifelse(year == 2013, 28,
                             ifelse(year == 2012, 4, 0)))))))))
  
  df <- df[-c((nrow(df)-number_of_footnote_rows+1):nrow(df)),]
  
  # recoding country names countrycode package could not identify
  df$country[df$country=="UAE*"] <- "United Arab Emirates"
  df$country[df$country=="Somali Republic"] <- "Somalia"
  
  df <- df %>%
    # filter out headers (e.g., "North America") and total rows
    dplyr::filter(country %!in% c("North America","Europe","Russia and Eurasia","Asia","Middle East and North Africa",
                                  "Latin America and the Caribbean","Sub-Saharan Africa","Summary","Total","Total*","Total**",
                                  "Global totals","US","NATO EX-US","Total NATO","Non-NATO Europe","Russia 2",
                                  "South and Central Asia","East Asia and Australasia","Latin America and Caribbean",
                                  "Total *","Total **","Nato Europe","Non-Nato Europe","Subtotal NATO Ex-US",
                                  "Latin America & The Carribean","Latin America and the Carribean")) %>%
    tidyr::pivot_longer(cols = 2:13, names_to = "metric", values_to = "value") %>%
    # add variable denoting which year the data was published / which sheet the data is from
    dplyr::mutate(data.pub.year = year,
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

military.balance.data <- military.balance.data %>%
  # adjust for inflation
  dplyr::left_join(inflation_table,dplyr::join_by("data.pub.year"=="year")) %>%
  dplyr::mutate(value = ifelse(metric %in% c("defense.spending","defense.spending.percapita"),
                               value * multiplier, value),
                # using the countrycode package, add country name based on iso3c code
                country = countrycode::countrycode(iso3c,"iso3c","country.name")) %>%
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
  tidyr::pivot_wider(names_from = "metric", values_from = "value") %>%
  dplyr::mutate(year = as.numeric(year))

#### WMEAT ----------------------------------------------------------------------
##### WMEAT 1995 ----------------------------------------------------------------------
wmeat.1995.data <- wmeat.1995[[1]]

names(wmeat.1995.data) <- c(
  "country.and.year",
  "military.expenditure.current.dollars", # in millions
  "blank1",
  "military.expenditure.1994.dollars", # in millions
  "blank2",
  "armed.personnel", #in thousands
  "blank3",
  "gdp.current.dollars", # in millions
  "blank4",
  "gdp.1994.dollars", # in millions
  "blank5",
  "central.gov.expenditure.1994.dollars", # in millions
  "blank6",
  "population", # in millions
  "blank7",
  "mil.expenditure.over.gnp", # percentage
  "blank8",
  "mil.expenditure.over.central.gov.expenditure", # percentage
  "blank9",
  "mil.expenditure.per.capita", # 1994 dollars
  "blank10",
  "armed.forces.per.1000.pop",
  "blank11",
  "gdp.per.capita", # 1994 dollars
  "blank12"
  )

# remove letters from year variable
wmeat.1995.data$country.and.year[wmeat.1995.data$country.and.year=="1989g"] <- 1989
wmeat.1995.data$country.and.year[wmeat.1995.data$country.and.year=="1990e"] <- 1990
wmeat.1995.data$country.and.year[wmeat.1995.data$country.and.year=="1990g"] <- 1990
wmeat.1995.data$country.and.year[wmeat.1995.data$country.and.year=="1991e"] <- 1991

# adjust China notations
wmeat.1995.data$country.and.year[wmeat.1995.data$country.and.year=="China"] <- NA
wmeat.1995.data$country.and.year[wmeat.1995.data$country.and.year=="-Mainland"] <- "China"
wmeat.1995.data$country.and.year[wmeat.1995.data$country.and.year=="-Taiwan"] <- "Taiwan"

wmeat.1995.data <- wmeat.1995.data %>%
  # convert "see X" entries to NAs
  dplyr::mutate(country.and.year = ifelse(country.and.year %in% c("Cote d'Ivoire (see Ivory Coast)",
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
  # add WMEAT version year
  dplyr::mutate(version = 1995,
                # using the countrycode package, add iso3c code based on country name
                iso3c = countrycode::countrycode(country,"country.name","iso3c"))

# add missing iso3c codes
wmeat.1995.data$iso3c[wmeat.1995.data$country=="Czechoslovakia"] <- "CZE"
wmeat.1995.data$iso3c[wmeat.1995.data$country=="Germany, East"] <- "DDR"
wmeat.1995.data$iso3c[wmeat.1995.data$country=="Germany, West"] <- "BRD"
wmeat.1995.data$iso3c[wmeat.1995.data$country=="Serbia and Montenegro"] <- "SRB"
wmeat.1995.data$iso3c[wmeat.1995.data$country=="Yemen (Aden)"] <- "YPR"
wmeat.1995.data$iso3c[wmeat.1995.data$country=="Yemen (Sanaa)"] <- "YAR"
wmeat.1995.data$iso3c[wmeat.1995.data$country=="Yugoslavai"] <- "YUG"

wmeat.1995.data %>%
  dplyr::select(-country) %>%
  # using the countrycode package, add country name based on iso3c code
  dplyr::mutate(country = countrycode::countrycode(iso3c,"iso3c","country.name"))

# add missing country names
wmeat.1995.data$country[wmeat.1995.data$iso3c=="BRD"] <- "West Germany"
wmeat.1995.data$country[wmeat.1995.data$iso3c=="DDR"] <- "East Germany"
wmeat.1995.data$country[wmeat.1995.data$iso3c=="YAR"] <- "North Yemen"
wmeat.1995.data$country[wmeat.1995.data$iso3c=="YPR"] <- "South Yemen"

##### WMEAT 2005 ----------------------------------------------------------------------
# all the data needed is on Sheet 2- "By Country"
wmeat.2005.data <- wmeat.2005[[2]]

names(wmeat.2005.data) <- c("country.and.year",
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
                            "blank13")

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
  # add WMEAT version year
  dplyr::mutate(version = 2005,
                # using the countrycode package, add iso3c code based on country name
                iso3c = countrycode::countrycode(country,"country.name","iso3c"))

# add missing iso3c codes
wmeat.2005.data$iso3c[wmeat.2005.data$country=="New  Zealand"] <- "NZL"
wmeat.2005.data$iso3c[wmeat.2005.data$country=="Serbia and Montenegro"] <- "SRB"
wmeat.2005.data$iso3c[wmeat.2005.data$country=="Yemen (Sanaa)"] <- "YEM"

wmeat.2005.data <- wmeat.2005.data %>%
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
    # convert to full value - multiply by 1,000 if variable is armed.personnel, otherwise multiply by 1,000,000
    dplyr::mutate(value = ifelse(variable == "armed.personnel",
                                 value * 1000,
                                 value * 1000000),
                  country = c,
                  # using the countrycode package, add iso3c code based on country name
                  iso3c = countrycode::countrycode(country,"country.name","iso3c")) %>%

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
    # convert to full value - multiply by 1,000 if variable is armed.personnel, otherwise multiply by 1,000,000
    dplyr::mutate(value = ifelse(variable == "armed.personnel",
                                 value * 1000,
                                 value * 1000000),
                  country = c,
                  # using the countrycode package, add iso3c code based on country name
                  iso3c = countrycode::countrycode(country,"country.name","iso3c")) %>%
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
    # convert to full value - multiply by 1,000 if variable is armed.personnel, otherwise multiply by 1,000,000
    dplyr::mutate(value = ifelse(variable == "armed.personnel",
                                 value * 1000,
                                 value * 1000000),
                  country = c,
                  # using the countrycode package, add iso3c code based on country name
                  iso3c = countrycode::countrycode(country,"country.name","iso3c")) %>%
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
wmeat.data <- rbind(wmeat.2019.data,wmeat.2016.data,wmeat.2012.data,wmeat.2005.data,wmeat.1995.data) %>%
  dplyr::mutate(year = as.numeric(year)) %>%
  dplyr::left_join(inflation_table,dplyr::join_by("version"=="year")) %>%
  # adjust for inflation
  dplyr::mutate(value = ifelse(variable %in% c("gdp.current.dollars","military.expenditure.current.dollars"),
                               value * multiplier,
                               value)) %>%
  dplyr::mutate(variable = ifelse(variable == "military.expenditure.current.dollars",
                                  "military.expenditure.wmeat",
                                  ifelse(variable == "gdp.current.dollars",
                                         "gdp.wmeat",
                                         ifelse(variable == "population",
                                                "population.wmeat",
                                                ifelse(variable == "armed.personnel",
                                                       "armed.personnel.wmeat",
                                                       variable))))) %>%
  tidyr::drop_na(value) %>%
  # keep the most recent published version of the data
  dplyr::group_by(iso3c,year,variable) %>%
  dplyr::filter(version == max(version,na.rm=TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-c(multiplier,version,country)) %>%
  tidyr::pivot_wider(names_from = "variable", values_from = "value")

#### merge cow, iiss, and wmeat datasets ----------------------------------------------------------------------
mildata <- dplyr::full_join(cow,military.balance.data,by=c("iso3c","year")) %>%
  dplyr::full_join(wmeat.data,by=c("iso3c","year")) %>%
  # set COW data as default expenditure and personnel values
  dplyr::mutate(mil.expenditure = milex.cow,
                mil.personnel = milper.cow,
                # for missing COW data, add in IISS data
                mil.expenditure = dplyr::coalesce(mil.expenditure,defense.spending.iiss),
                mil.personnel = dplyr::coalesce(mil.personnel,active.armed.forces.iiss),
                # using the countrycode package, add country name based on iso3c code
                country = countrycode::countrycode(iso3c,"iso3c","country.name")) %>%
  dplyr::relocate(country, .after = iso3c) %>%
  dplyr::relocate(mil.expenditure, .after = year) %>%
  dplyr::relocate(mil.personnel, .after = mil.expenditure)

# add country names for missing iso3c codes
mildata$country[mildata$iso3c=="BRD"] <- "West Germany"
mildata$country[mildata$iso3c=="DDR"] <- "East Germany"
mildata$country[mildata$iso3c=="KSV"] <- "Kosovo"
mildata$country[mildata$iso3c=="RVN"] <- "South Vietnam"
mildata$country[mildata$iso3c=="YAR"] <- "North Yemen"
mildata$country[mildata$iso3c=="YPR"] <- "South Yemen"
mildata$country[mildata$iso3c=="YUG"] <- "Yugoslavia"
mildata$country[mildata$iso3c=="ZAN"] <- "Zanzibar"


### military metrics estimator functions ----------------------------------------------------------------------
# this function is used to estimate COW military expenditure and military personnel values based on rescaling
# WMEAT data to equal the COW data on either end of the missing data.

mil_estimator_rescaling_func <- function(df = mildata, iso, lower_year, upper_year, expenditure = FALSE,
                                         personnel = FALSE){

  # expenditure calculations
  
    # pull expenditure COW estimates from either end of the missing data range
    cow.expenditure.lower <- df$mil.expenditure[df$iso3c==iso&df$year==lower_year]
    cow.expenditure.upper <- df$mil.expenditure[df$iso3c==iso&df$year==upper_year]
    
    # pull expenditure WMEAT estimates from either end of the missing data range
    wmeat.expenditure.lower <- df$military.expenditure.wmeat[df$iso3c==iso&df$year==lower_year]
    wmeat.expenditure.upper <- df$military.expenditure.wmeat[df$iso3c==iso&df$year==upper_year]
    
    # calculate range between COW lower and upper values
    cow.expenditure.range <- cow.expenditure.upper - cow.expenditure.lower
    
  # personnel calculations
    
    # pull personnel COW estimates from either end of the missing data range
    cow.personnel.lower <- df$mil.personnel[df$iso3c==iso&df$year==lower_year]
    cow.personnel.upper <- df$mil.personnel[df$iso3c==iso&df$year==upper_year]
    
    # pull personnel WMEAT estimates from either end of the missing data range
    wmeat.personnel.lower <- df$armed.personnel.wmeat[df$iso3c==iso&df$year==lower_year]
    wmeat.personnel.upper <- df$armed.personnel.wmeat[df$iso3c==iso&df$year==upper_year]
    
    # calculate range between COW lower and upper values
    cow.personnel.range <- cow.personnel.upper - cow.personnel.lower
    
  
  for(y in (lower_year+1):(upper_year-1)){
    
    if(expenditure == TRUE){
      
      df$mil.expenditure[df$iso3c==iso&df$year==y] <- cow.expenditure.lower +
        (cow.expenditure.range*(df$military.expenditure.wmeat[df$iso3c==iso&df$year==y]-wmeat.expenditure.lower)/
           (wmeat.expenditure.upper-wmeat.expenditure.lower))
      
    }
    
    if(personnel == TRUE){
      
      df$mil.personnel[df$iso3c==iso&df$year==y] <- cow.personnel.lower +
        (cow.personnel.range*(df$armed.personnel.wmeat[df$iso3c==iso&df$year==y]-wmeat.personnel.lower)/
           (wmeat.personnel.upper-wmeat.personnel.lower))
      
    }
    
  }
    
    return(df)
  
}

#### estimate missing values ----------------------------------------------------------------------

mildata.wmeat.na <- mildata %>%
  dplyr::filter(
    year %in% c(1995:2017),
    is.na(mil.expenditure) | is.na(mil.personnel)
    )


##### AFG ----------------------------------------------------------------------

##### ARE ----------------------------------------------------------------------

##### BEN ----------------------------------------------------------------------
# mil.expenditure 1993

# mil.expenditure 2005
# mildata <- mil_estimator_rescaling_func(mildata, "BEN", lower_year = 2004,
#                                         upper_year = 2006, expenditure = TRUE)


##### BLR ----------------------------------------------------------------------

##### BLZ ----------------------------------------------------------------------

##### BTN ----------------------------------------------------------------------

##### CIV ----------------------------------------------------------------------
# mil.personnel 2012-2015
# WMEAT estimates largely match COW estimates before and after the gap, so apply
# WMEAT pattern to COW estimates
civ.wmeat.mil.personnel.2011 <- mildata$armed.personnel.wmeat[mildata$iso3c=="CIV"&mildata$year==2011] # min
civ.wmeat.mil.personnel.2016 <- mildata$armed.personnel.wmeat[mildata$iso3c=="CIV"&mildata$year==2016] # max
civ.cow.mil.personnel.2011 <- mildata$mil.personnel[mildata$iso3c=="CIV"&mildata$year==2011] # min
civ.cow.mil.personnel.2016 <- mildata$mil.personnel[mildata$iso3c=="CIV"&mildata$year==2016] # max
civ.cow.mil.personnel.diff <- civ.cow.mil.personnel.2016 - civ.cow.mil.personnel.2011 # diff

for(y in 2012:2015){
  
  mildata$mil.personnel[mildata$iso3c=="CIV"&mildata$year==y] <- civ.cow.mil.personnel.2011 +
    (civ.cow.mil.personnel.diff*(mildata$armed.personnel.wmeat[mildata$iso3c=="CIV"&mildata$year==y]-civ.wmeat.mil.personnel.2011)/
       (civ.wmeat.mil.personnel.2016-civ.wmeat.mil.personnel.2011))
  
}

##### COD ----------------------------------------------------------------------
# COD
cow$milex[cow$iso3c=="COD"&cow$year==2002] <- (723094959+96146337)/2
cow$milex[cow$iso3c=="COD"&cow$year==1992] <- 159321275
###

# mil.expenditure 1992


# mil.expenditure 2002


##### COG ----------------------------------------------------------------------

##### CRI ----------------------------------------------------------------------

##### CUB ----------------------------------------------------------------------

##### DJI ----------------------------------------------------------------------

##### ERI ----------------------------------------------------------------------

##### GMB ----------------------------------------------------------------------

##### GNB ----------------------------------------------------------------------

##### GNQ ----------------------------------------------------------------------

##### GUY ----------------------------------------------------------------------

##### HTI ----------------------------------------------------------------------

##### IRQ ----------------------------------------------------------------------

##### ISL ----------------------------------------------------------------------

##### KGZ ----------------------------------------------------------------------

##### KSV ----------------------------------------------------------------------

##### LAO ----------------------------------------------------------------------

##### LBR ----------------------------------------------------------------------

##### LBY ----------------------------------------------------------------------

##### MDG ----------------------------------------------------------------------

##### MMR ----------------------------------------------------------------------

##### MNE ----------------------------------------------------------------------

##### MUS ----------------------------------------------------------------------

##### NER ----------------------------------------------------------------------

##### PAN ----------------------------------------------------------------------

##### PRK ----------------------------------------------------------------------

##### QAT ----------------------------------------------------------------------

##### SDN ----------------------------------------------------------------------

##### SOM ----------------------------------------------------------------------

##### SRB ----------------------------------------------------------------------

##### SSD ----------------------------------------------------------------------

##### STP ----------------------------------------------------------------------

##### SUR ----------------------------------------------------------------------
# Missing mil.expenditure for 1978-1981, 1987, 2014-

##### SWZ ----------------------------------------------------------------------

##### SYR ----------------------------------------------------------------------

##### TKM ----------------------------------------------------------------------

##### TLS ----------------------------------------------------------------------

##### UZB ----------------------------------------------------------------------

##### YEM ----------------------------------------------------------------------

##### ZIM ----------------------------------------------------------------------













#### format merged dataset ----------------------------------------------------------------------
# reformat to long data
# note that the 22 country-years with two entries have mutually exclusive data, so no duplicate
# values will occur once in long format, only a real value and an NA value
cow <- cow %>%
  tidyr::pivot_longer(3:6, names_to = "variable", values_to = "value") %>%
  dplyr::group_by(iso3c,year,variable) %>%
  # takes the mean value ignoring NAs, though all entries are either one value or one
  # value and one NA
  dplyr::summarise(value = mean(value,na.rm=T)) %>%
  dplyr::ungroup() %>%
  # pivot back to having each column as its own variable
  tidyr::pivot_wider(names_from = "variable", values_from = "value") %>%
  # replace all NaNs with NAs
  dplyr::mutate_all(~ifelse(is.nan(.), NA, .))

### add estimates from workbook ----------------------------------------------------------------------
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

# remove YUG values for 2012 (SRB values for 2012 already in dataset)
cow <- cow %>%
  filter(iso3c != "YUG" | year != 2012)

### inflation ----------------------------------------------------------------------
# inflation from https://www.bls.gov/data/inflation_calculator.htm
# July to July, converting to 2019 dollars
inflation_table <- data.frame(year = c(1946:2018),
                              multiplier = c(1+11.96,1+10.56,1+9.52,1+9.83, #40s
                                             1+9.65,1+8.91,1+8.61,1+8.57,1+8.54,1+8.57,1+8.36,1+8.07,1+7.85,1+7.79, #50s
                                             1+7.67,1+7.55,1+7.47,1+7.36,1+7.25,1+7.12,1+6.89,1+6.68,1+6.35,1+5.97, #60s
                                             1+5.58,1+5.30,1+5.12,1+4.79,1+4.19,1+3.73,1+3.49,1+3.21,1+2.91,1+2.51, #70s
                                             1+2.10,1+1.80,1+1.63,1+1.57,1+1.46,1+1.38,1+1.34,1+1.25,1+1.17,1+1.06, #80s
                                             1+0.97,1+0.88,1+0.83,1+0.78,1+0.73,1+0.68,1+0.63,1+0.60,1+0.57,1+0.54, #90s
                                             1+0.48,1+0.45,1+0.42,1+0.40,1+0.35,1+0.31,1+0.26,1+0.23,1+0.17,1+0.19, #00s
                                             1+0.18,1+0.14,1+0.12,1+0.10,1+0.08,1+0.08,1+0.07,1+0.05,1+0.02)) #10s

cow <- cow %>%
  dplyr::left_join(inflation_table,by="year") %>%
  # multiples dollar values in that year's dollar value to 2019 dollars
  dplyr::mutate(milex = milex * multiplier,
                milexpc = milexpc * multiplier) %>%
  dplyr::select(-multiplier)

### calculate unified/divided country data ----------------------------------------------------------------------
cow <- cow %>%
  # expand dataframe to have all iso3c-year combos from 1946 - 2019
  dplyr::full_join(expand.grid(iso3c = unique(cow$iso3c), year = c(1946:2019))) %>%
  # change YUG coding to SRB for 1992 onward
  dplyr::mutate(iso3c = ifelse(iso3c=="YUG"&year>1991,"SRB",iso3c),
                # change RUS coding to SOV for 1991 and before
                iso3c = ifelse(iso3c=="RUS"&year<=1991,"SOV",iso3c)) %>%
  # filter out countries that merged together or split apart
  dplyr::filter(iso3c %!in% c("YAR","YPR") | year <= 1989,
                iso3c != "YEM" | year >= 1990,
                iso3c %!in% c("BRD","DDR") | year <= 1990,
                iso3c != "DEU" | year >= 1991,
                iso3c %!in% c("ARM","AZE","BLR","EST","GEO","KAZ","KGZ","LTU","LVA","MDA",
                              "RUS","TJK","TKM","UZB","BIH","HRV","MKD","SRB","SVN") | year >= 1992,
                iso3c != "KSV" | year >= 2008,
                # filter out Palestine for lack of data
                iso3c != "PSE")

### interpolate missing values ----------------------------------------------------------------------
cow <- cow %>%
  dplyr::group_by(iso3c) %>%
  dplyr::arrange(year) %>%
  dplyr::mutate(milex.approx = imputeTS::na_interpolation(milex, option = "linear"),
                milper.approx = imputeTS::na_interpolation(milper, option = "linear")) %>%
  dplyr::ungroup() %>%
  dplyr::select(-c(milex,milper))

### merge population and GDP data ----------------------------------------------------------------------
# load formatted data output by scripts 003-GDP and 004-Population
gdp <- read.csv("Data files/Formatted data files/gdp.csv")
population <- read.csv("Data files/Formatted data files/population.csv")

# merge files by iso3c and year
cow <- cow %>%
  dplyr::left_join(gdp,by=c("iso3c","year")) %>%
  dplyr::left_join(population,by=c("iso3c","year")) %>%
  # gdp and population data not available for 2020
  dplyr::filter(year < 2020) %>%
  dplyr::relocate(country,.after="iso3c") %>%
  dplyr::mutate(milexpc_est = milex.approx / population,
                milexpgdp_est = milex.approx / gdp,
                milexpc_ratio = milexpc_est / milexpc,
                milexpgdp_ratio = milexpgdp_est / milexpgdp)

# ranges of milexpc estimate ratios (milex.approx / population vs. milexpc) range from 58% - 431% of milexpc
# ranges of milexpgdp estimate ratios (milex.approx / gdp vs. milexpgdp) range from 0.34% - 218% of milexpgdp

# replaces several calculations for specfic county-years
cow$milex.approx[cow$iso3c=="ARE"&cow$year==2014] <- cow$milexpgdp[cow$iso3c=="ARE"&cow$year==2014] * 
  cow$gdp[cow$iso3c=="ARE"&cow$year==2014]
cow$milex.approx[cow$iso3c=="GIN"&cow$year==2014] <- cow$milexpgdp[cow$iso3c=="GIN"&cow$year==2014] * 
  cow$gdp[cow$iso3c=="GIN"&cow$year==2014]
cow$milex.approx[cow$iso3c=="GNB"&cow$year==2014] <- cow$milexpgdp[cow$iso3c=="GNB"&cow$year==2014] * 
  cow$gdp[cow$iso3c=="GNB"&cow$year==2014]
cow$milex.approx[cow$iso3c=="NER"&cow$year==2014] <- cow$milexpgdp[cow$iso3c=="NER"&cow$year==2014] * 
  cow$gdp[cow$iso3c=="NER"&cow$year==2014]
cow$milex.approx[cow$iso3c=="SDN"&cow$year==2014] <- cow$milexpgdp[cow$iso3c=="SDN"&cow$year==2014] * 
  cow$gdp[cow$iso3c=="SDN"&cow$year==2014]

cow <- cow %>%
  # calculates military expenditure per personnel (milexpt) and military personnel per capita (miltppc)
  dplyr::mutate(milexpt = milex.approx / milper.approx,
                miltppc = milper.approx / population,
                # if estimates are not available for milexpgdp or milexpc, use milex.approx / gdp or population as estimates
                milexpgdp = dplyr::coalesce(milexpgdp,milexpgdp_est),
                milexpc = dplyr::coalesce(milexpc,milexpc_est)) %>%
  dplyr::select(-c(gdp,population,milexpc_est,milexpgdp_est,milexpc_ratio,milexpgdp_ratio)) %>%
  dplyr::rename(milex = milex.approx,
                milper = milper.approx)

# calculate how many variables are present for each country (out of 6 total)
cow.names <- cow %>%
  tidyr::pivot_longer(4:9, names_to = "variable", values_to = "value") %>%
  na.omit() %>%
  dplyr::group_by(iso3c,variable) %>%
  dplyr::tally() %>%
  dplyr::ungroup() %>%
  dplyr::group_by(iso3c) %>%
  dplyr::tally() %>%
  dplyr::ungroup()

countries_missing_variables <- cow.names %>%
  dplyr::filter(n < 6) %>%
  dplyr::pull(iso3c)

### interpolate missing values again ----------------------------------------------------------------------
cow <- cow %>%
  dplyr::filter(iso3c %!in% c("BLZ","MDV","ZAN",countries_missing_variables)) %>%
  dplyr::group_by(iso3c) %>%
  dplyr::arrange(year) %>%
  dplyr::mutate(milexpc = imputeTS::na_interpolation(milexpc, option = "linear"),
                milexpgdp = imputeTS::na_interpolation(milexpgdp, option = "linear"),
                milex = imputeTS::na_interpolation(milex, option = "linear"),
                milper = imputeTS::na_interpolation(milper, option = "linear"),
                milexpt = imputeTS::na_interpolation(milexpt, option = "linear"),
                miltppc = imputeTS::na_interpolation(miltppc, option = "linear")) %>%
  dplyr::ungroup() %>%
  # replace all Infs and NaNs with 0s
  dplyr::mutate_all(~ifelse(is.infinite(.), 0, .),
                    ~ifelse(is.nan(.), 0, .)) %>%
  # calculate natural logs of milexpgdp, miltppc, milexpc, and milexpt
  dplyr::mutate(lnmilexpgdp = log(milexpgdp),
                lntroops = log(miltppc * 1000000),
                lntroops2 = log(miltppc),
                lnmilexpc = log(milexpc),
                lnmilexpt = log(milexpt)) %>%
  # replace all Infs and NaNs with 0s
  dplyr::mutate_all(~ifelse(is.infinite(.), 0, .),
                    ~ifelse(is.nan(.), 0, .))
