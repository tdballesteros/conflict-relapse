# creates variable on oil exports for 1980-2016

### load libraries ----------------------------------------------------------------------
library(countrycode)
library(dplyr)
library(tidyr)

### load data files ----------------------------------------------------------------------
oil <- read.csv("Data files/Raw data files/INT-Export-08-08-2020_06-00-39.csv", skip = 1)

### data formatting ----------------------------------------------------------------------
oil <- oil[-1,-1]

oil <- oil %>%
  tidyr::pivot_longer(2:38, names_to = "year", values_to = "oil") %>%
  dplyr::mutate(year = str_sub(year,start=2,end=5)) %>%
  dplyr::rename(country = X) %>%
  # using the countrycode package, add iso3c code based on country name
  dplyr::mutate(iso3c = countrycode(country,"country.name","iso3c"),
                year = as.numeric(year),
                oil = as.numeric(oil)) %>%
  dplyr::relocate(iso3c,.before = country)

# manually add iso3c codes to missing country names
oil$iso3c[oil$country=="    Former Czechoslovakia"] <- "CZE"
oil$iso3c[oil$country=="    Former Serbia and Montenegro"] <- "SRB"
oil$iso3c[oil$country=="    Former Yugoslavia"] <- "YUG"
oil$iso3c[oil$country=="    Germany, East"] <- "DDR"
oil$iso3c[oil$country=="    Hawaiian Trade Zone"] <- "HTZ"
oil$iso3c[oil$country=="    Kosovo"] <- "KSV"
oil$iso3c[oil$country=="    Micronesia"] <- "FSM"
oil$iso3c[oil$country=="    Netherlands Antilles"] <- "ANT"
oil$iso3c[oil$country=="    U.S. Pacific Islands"] <- "UPI"
oil$iso3c[oil$country=="    Wake Island"] <- "WAK"
oil$iso3c[oil$country=="    World"] <- "WLD"

# duplicate 2016 aid data and code as same amount of aid for 2017 - 2019
oil17 <- oil %>%
  dplyr::filter(year == 2016) %>%
  dplyr::mutate(year = 2017)

oil18 <- oil %>%
  dplyr::filter(year == 2016) %>%
  dplyr::mutate(year = 2018)

oil19 <- oil %>%
  dplyr::filter(year == 2016) %>%
  dplyr::mutate(year = 2019)

oil <- oil %>%
  rbind(oil17,oil18,oil19) %>%
  # create dummy variable for oil exporting countries (for a given year)
  dplyr::mutate(exporter = ifelse(oil >= 1000, 1, 0)) %>%
  filter(iso3c %!in% c("WLD","ASM","ATA","ATG","ABW","BRB","BLZ","BMU","BTN","VGB","BRN","CPV","CYM","COM","COK","DJI",
                       "DMA","FLK","FRO","FJI",NA,"GUF","PYF","GIB","GRL","GRD","GLP","GUM","HKG","ISL","KIR","LUX",
                       "MAC","MDV","MLT","MTQ","MSR","NRU","NCL","NIU","MNP","PSE","PRI","REU","SHN","KNA","LCA","SPM",
                       "VCT","WSM","STP","SYC","SLB","SUR","BHS","TON","TCA","TUV","VIR","VUT","ESH"))

# remove leading white space from country names
oil$country <- trimws(oil$country, which = "left")

### write data ----------------------------------------------------------------------
# writes formatted dataframe as csv files
write.csv(oil,"Data files/Formatted data files/oil.csv",row.names = FALSE)

