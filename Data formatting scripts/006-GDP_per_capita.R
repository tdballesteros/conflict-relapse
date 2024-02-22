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
  )

### write data ----------------------------------------------------------------------
# writes formatted dataframe as csv files
write.csv(gdppc,"Data files/Formatted data files/gdppc.csv",row.names = FALSE)
