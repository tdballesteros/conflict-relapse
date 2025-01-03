# This script calculates gdp per capita (gdppc) estimates based on the tables formatted in the
# 004-GDP and 005-Population scripts.

# Estimates are calculated for:
## pwt (gdp) - un (population)
## pwt (gdp) - cow (population)
## gl (gdp) - un (population)
## gl (gdp) - cow (population)

### load libraries ----------------------------------------------------------------------
library(countrycode)
library(tibble)
library(dplyr)
library(tidyr)

### load data files ----------------------------------------------------------------------
# load formatted GDP table output by the 004-GDP script
gdp <- utils::read.csv("Data files/Formatted data files/gdp.csv")

# load formatted population table output by the 005-Population script
pd <- utils::read.csv("Data files/Formatted data files/population.csv")

### format and merge data files ----------------------------------------------------------------------
gdp <- gdp %>%
  dplyr::select(iso3c,country,year,gdp.pwt.est,gdp.gl.est)

pd <- pd %>%
  dplyr::select(iso3c,country,year,un.pop,cow.pop)

gdppc <- dplyr::full_join(gdp,pd,by=c("iso3c","country","year"))

### calculate gdp per capita estimates ----------------------------------------------------------------------
gdppc <- gdppc %>%
  dplyr::mutate(
    # pwt (gdp) - un (population)
    gdppc.pwt.un = gdp.pwt.est / un.pop,
    # pwt (gdp) - cow (population)
    gdppc.pwt.cow = gdp.pwt.est / cow.pop,
    # gl (gdp) - un (population)
    gdppc.gl.un = gdp.gl.est / un.pop,
    # gl (gdp) - cow (population)
    gdppc.gl.cow = gdp.gl.est / cow.pop
  ) %>%
  dplyr::select(-c(gdp.pwt.est,gdp.gl.est,un.pop,cow.pop))

### calculate change in gdp per capita estimates ----------------------------------------------------------------------
gdppc.plus1 <- gdppc %>%
  dplyr::mutate(year = year + 1) %>%
  dplyr::rename(gdppc.pwt.un.plus1 = gdppc.pwt.un,
                gdppc.pwt.cow.plus1 = gdppc.pwt.cow,
                gdppc.gl.un.plus1 = gdppc.gl.un,
                gdppc.gl.cow.plus1 = gdppc.gl.cow)

gdppc <- gdppc %>%
  dplyr::left_join(gdppc.plus1,by=c("iso3c","country","year")) %>%
  dplyr::mutate(gdppc.growth.pwt.un = 100*(gdppc.pwt.un-gdppc.pwt.un.plus1)/gdppc.pwt.un.plus1,
                gdppc.growth.pwt.cow = 100*(gdppc.pwt.cow-gdppc.pwt.cow.plus1)/gdppc.pwt.cow.plus1,
                gdppc.growth.gl.un = 100*(gdppc.gl.un-gdppc.gl.un.plus1)/gdppc.gl.un.plus1,
                gdppc.growth.gl.cow = 100*(gdppc.gl.cow-gdppc.gl.cow.plus1)/gdppc.gl.cow.plus1) %>%
  dplyr::select(-c(gdppc.pwt.un.plus1,gdppc.pwt.cow.plus1,gdppc.gl.un.plus1,gdppc.gl.cow.plus1))

### write data ----------------------------------------------------------------------
# writes formatted dataframe as csv files
write.csv(gdppc,"Data files/Formatted data files/gdppc.csv",row.names = FALSE)

### codebook ----------------------------------------------------------------------
# iso3c
### A country's standardized iso3c code, with non-standard codes for West Germany, East Germany, North Yemen, South Yemen,
### South Vietnam, the Netherlands Antilles, the Soviet Union, and Yugoslavia.
# country
### A country's commonly used English-language name.
# year
### The calendar year the specific variable is measured during.
# gdppc.pwt.un
### An estimate for gross domestic product per capita using estimates for GDP and population built off of Penn World Tables
### and United Nations data, respectively.
# gdppc.pwt.cow
### An estimate for gross domestic product per capita using estimates for GDP and population built off of Penn World Tables
### and Correlates of War data, respectively.
# gdppc.gl.un
### An estimate for gross domestic product per capita using estimates for GDP and population built off of Gleditsch
### and United Nations data, respectively.
# gdppc.gl.cow
### An estimate for gross domestic product per capita using estimates for GDP and population built off of Gleditsch
### and Correlates of War data, respectively.
