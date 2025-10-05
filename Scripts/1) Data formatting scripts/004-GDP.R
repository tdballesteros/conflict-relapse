
# This script formats and extends two GDP estimates and multiple GDP growth estimates.

# TODO
## (p) means pwt estimates missing
### AFG, AND, CUB, ERI, FSM, GUY, KIR, LBY, LIE, MCO, MHL, NRU, PLW, PRK, SLB, SMR, SOM,
### TLS, TON, TUV
## (g) means gl estimates missing
### ISR/PSE
## (x) means other issues to address
### YUG
### ETH/ERI - ERI in ETH pre-independence?
### DOM, LBN, PRK 1940s estimates look off


### load libraries ---------------------------------------------------------------------------------
library(readxl)
library(utils)
library(countrycode)
library(tibble)
library(dplyr)
library(tidyr)
library(stringr)


### not in function --------------------------------------------------------------------------------
'%!in%' <- function(x,y)!('%in%'(x,y))


### load and format data ---------------------------------------------------------------------------

#### Penn World Tables (PWT) -----------------------------------------------------------------------
# IMF GDP growth rates from
# https://www.imf.org/external/datamapper/NGDP_RPCH@WEO/OEMDC/ADVEC/WEOWORLD

# Real GDP at constant 2011 national prices (in mil. 2011US$)
pwt <- readxl::read_xlsx("Data files/Raw data files/pwt91.xlsx", sheet = 3) %>%
  dplyr::select(country, year, rgdpna) %>%
  dplyr::mutate(
    # convert real gdp to 2019$
    rgdpna = rgdpna * 1000000 * (1 + 0.14),
    # using the countrycode package, add iso3c based on country name
    iso3c = dplyr::case_when(
      country == "Aruba" ~ "NLD",
      country == "Anguilla" ~ "GBR",
      country == "Bermuda" ~ "GBR",
      country == "Curaçao" ~ "NLD",
      country == "Cayman Islands" ~ "GBR",
      country == "China, Hong Kong SAR" ~ "CHN",
      country == "China, Macao SAR" ~ "CHN",    
      country == "Montserrat" ~ "GBR",
      country == "Sint Maarten (Dutch part)" ~ "NLD",
      country == "Turks and Caicos Islands" ~ "GBR",
      country == "British Virgin Islands" ~ "GBR",
      .default = countrycode::countrycode(country, "country.name", "iso3c")),
    # using the countrycode package, add country name based on iso3c code
    country = countrycode::countrycode(iso3c, "iso3c", "country.name")
    )  %>%
  dplyr::group_by(iso3c, country, year) %>%
  dplyr::summarise(gdp.pwt = sum(rgdpna, na.rm = TRUE)) %>%
  dplyr::ungroup()

#### Gleditsch -------------------------------------------------------------------------------------
# realgdp	- total real GDP, 2005 
gdpgl <- utils::read.delim("Data files/Raw data files/gdpv6.txt") %>%
  # using the countrycode package, add iso3c based on country name
  dplyr::mutate(
    iso3c = dplyr::case_when(
      stateid == "AAB" ~ "ATG",
      stateid == "ABK" ~ "GEO", # Abkhazia - recode to be part of Georgia
      stateid == "AND" ~ "AND",
      stateid == "CZE" ~ "CZE", # Czechoslovakia
      stateid == "DEU" & year < 1991 ~ "BRD", # West Germany
      stateid == "DEU" & year >= 1991 ~ "DEU", # unified Germany
      stateid == "DMA" ~ "DMA",
      stateid == "DRV" ~ "VNM",
      stateid == "FSM" ~ "FSM",
      stateid == "GDR" ~ "DDR", # East Germany
      stateid == "GRN" ~ "GRD",
      stateid == "KBI" ~ "KIR",
      stateid == "KOS" ~ "KSV",
      stateid == "LIE" ~ "LIE",
      stateid == "MNC" ~ "MCO",
      stateid == "MSI" ~ "MHL",
      stateid == "NAU" ~ "NRU",
      stateid == "PAL" ~ "PLW",
      stateid == "RVN" ~ "RVN",
      stateid == "SEY" ~ "SYC",
      stateid == "SKN" ~ "KNA",
      stateid == "SLU" ~ "LCA",
      stateid == "SNM" ~ "SMR",
      stateid == "SOT" ~ "GEO", # South Ossetia - recode to be part of Georgia
      stateid == "STP" ~ "STP",
      stateid == "SVG" ~ "VCT",
      stateid == "TBT" ~ "CHN", # Tibet - recode to be part of China
      stateid == "TON" ~ "TON",
      stateid == "TUV" ~ "TUV",
      stateid == "VAN" ~ "VUT",
      stateid == "WSM" ~ "WSM",
      stateid == "YEM" & year < 1991 ~ "YAR", # North Yemen
      stateid == "YEM" & year >= 1991 ~ "YAR", # unified Yemen
      stateid == "YPR" ~ "YPR",
      stateid == "YUG" ~ "YUG",
      stateid == "ZAN" ~ "ZAN",
      .default = countrycode::countrycode(stateid, "gwc", "iso3c")
      ),
    country = dplyr::case_when(
      iso3c == "BRD" ~ "West Germany",
      iso3c == "DDR" ~ "East Germany",
      iso3c == "KSV" ~ "Kosovo",
      iso3c == "YUG" ~ "Yugoslavia",
      iso3c == "YAR" ~ "North Yemen",
      iso3c == "YPR" ~ "South Yemen",
      iso3c == "RVN" ~ "South Vietnam",
      iso3c == "ZAN" ~ "Zanzibar",
      .default = countrycode::countrycode(iso3c,"iso3c","country.name")
      ),
    # convert real gdp to 2019$
    gdp.gl = realgdp * (1 + 0.31) * 1000000
    ) %>%
  dplyr::select(iso3c, country, year, gdp.gl) %>%
  # collapse across iso3c-year
  dplyr::group_by(iso3c, country, year) %>%
  dplyr::summarise(gdp.gl = sum(gdp.gl, na.rm = TRUE)) %>%
  dplyr::ungroup()


### growth rate data -------------------------------------------------------------------------------
#### IMF growth rates ------------------------------------------------------------------------------

imf.growth.modern <- readxl::read_xls("Data files/Raw data files/imf-dm-export-gdp-growth.xls")[-1,] %>%
  dplyr::rename(country = 1) %>%
  tidyr::pivot_longer(2:41, names_to = "year", values_to = "imf.growth.rate.modern") %>%
  dplyr::filter(
    # filter out groupings of countries
    country %!in% c(
      "Africa (Region)", "Asia and Pacific", "Australia and New Zealand", "Caribbean",
      "Central America", "Central Asia and the Caucasus", "East Asia", "Eastern Europe", "Europe",
      "Middle East (Region)", "North Africa", "North America", "Pacific Islands", "South America",
      "South Asia", "Southeast Asia", "Sub-Saharan Africa (Region)", "Western Europe",
      "Western Hemisphere (Region)", "ASEAN-5", "Advanced economies",
      "Emerging and Developing Asia", "Emerging and Developing Europe",
      "Emerging market and developing economies", "Euro area", "European Union",
      "Latin America and the Caribbean", "Major advanced economies (G7)",
      "Middle East and Central Asia", "Other advanced economies", "Sub-Saharan Africa"," World"
      ),
    # filter out non-sovereign entities
    country %!in% c("Aruba", "Hong Kong SAR", "Macao SAR")
    ) %>%
  dplyr::mutate(
    # using the countrycode package, add iso3c based on country name
    iso3c = dplyr::case_when(
      country == "Kosovo" ~ "KSV",
      .default = countrycode::countrycode(country, "country.name", "iso3c")
      ),
    # using the countrycode package, add country name based on iso3c code to standardize country
    # names
    country = dplyr::case_when(
      iso3c == "KSV" ~ "Kosovo",
      .default = countrycode::countrycode(iso3c,"iso3c","country.name")
      ),
    year = as.numeric(year),
    imf.growth.rate.modern = as.numeric(imf.growth.rate.modern)
    )

imf.growth.historical <- readxl::read_excel("Data files/Raw data files/imf-dm-export-public_finances.xls")[-1,] %>%
  dplyr::rename(country = 1) %>%
  # temporarily convert 2001-2019 columns to characters to facilitate pivoting to long data format
  dplyr::mutate(across(c("1999", starts_with("20")), as.character)) %>%
  tidyr::pivot_longer(2:220, names_to = "year", values_to = "imf.growth.rate.historical") %>%
  # filter out non-sovereign entities
  dplyr::filter(
    country %!in% c("Aruba", "Hong Kong SAR"),
    year >= 1946
    ) %>%
  dplyr::mutate(
    # using the countrycode package, add iso3c based on country name
    iso3c = countrycode::countrycode(country, "country.name", "iso3c"),
    # using the countrycode package, add iso3c based on country name
    country = countrycode::countrycode(iso3c, "iso3c", "country.name"),
    year = as.numeric(year),
    imf.growth.rate.historical = as.numeric(imf.growth.rate.historical)
    )

#### World Bank growth rates -----------------------------------------------------------------------

wb.growth.data <- read.csv("Data files/Raw data files/API_NY.GDP.MKTP.KD_DS2_en_csv_v2_1217522.csv",
                           skip = 4) %>%
  dplyr::filter(
    # filter groupings of countries
    Country.Name %!in% c(
      "Arab World", "Central Europe and the Baltics", "Caribbean small states",
      "East Asia & Pacific (excluding high income)", "Early-demographic dividend",
      "East Asia & Pacific", "Europe & Central Asia (excluding high income)",
      "Europe & Central Asia", "Euro area", "European Union",
      "Fragile and conflict affected situations", "High income",
      "Heavily indebted poor countries (HIPC)", "IBRD only", "IDA & IBRD total", "IDA total",
      "IDA blend", "IDA only", "Not classified",
      "Latin America & Caribbean (excluding high income)", "Latin America & Caribbean",
      "Least developed countries: UN classification","Low income",
      "Lower middle income", "Low & middle income", "Late-demographic dividend",
      "Middle East & North Africa", "Middle income",
      "Middle East & North Africa (excluding high income)", "North America", "OECD members",
      "Other small states", "Pre-demographic dividend", "Pacific island small states",
      "Post-demographic dividend", "Sub-Saharan Africa (excluding high income)",
      "Sub-Saharan Africa", "Small states", "East Asia & Pacific (IDA & IBRD countries)",
      "South Asia", "Europe & Central Asia (IDA & IBRD countries)",
      "Latin America & the Caribbean (IDA & IBRD countries)",
      "Middle East & North Africa (IDA & IBRD countries)", "South Asia (IDA & IBRD)",
      "Sub-Saharan Africa (IDA & IBRD countries)", "Upper middle income", "World"
      ),
    # filter non-sovereign entities
    Country.Name %!in% c(
      "Aruba", "American Samoa", "Bermuda", "Channel Islands", "Curacao", "Cayman Islands",
      "Faroe Islands", "Gibraltar", "Greenland", "Guam", "Hong Kong SAR, China", "Isle of Man",
      "Macao SAR, China", "St. Martin (French part)", "Northern Mariana Islands", "New Caledonia",
      "Puerto Rico", "French Polynesia", "Sint Maarten (Dutch part)", "Turks and Caicos Islands",
      "British Virgin Islands", "Virgin Islands (U.S.)"
      )
    ) %>%
  dplyr::select(-c("Indicator.Name", "Indicator.Code")) %>%
  tidyr::pivot_longer(3:63, names_to = "year", values_to = "gdp") %>%
  dplyr::mutate(year =  as.numeric(stringr::str_sub(year, start = 2, end = 5))) %>%
  dplyr::rename(
    country = Country.Name,
    iso3c = Country.Code
    )

# shift years to facilitate calculating growth rates
wb.growth.data.year.plus1 <- wb.growth.data %>%
  dplyr::mutate(year = year + 1) %>%
  dplyr::rename(gdp.plus1 = gdp)

# merge shifted year data with original WB dataset
wb.growth.data <- wb.growth.data %>%
  dplyr::full_join(wb.growth.data.year.plus1, by = c("iso3c", "country", "year")) %>%
  dplyr::mutate(
    wb.growth.rate = 100 * (gdp - gdp.plus1) / gdp.plus1,
    # using the countrycode package, add country name based on iso3c code to standardize country
    # names
    country = dplyr::case_when(
      iso3c == "XKX" ~ "Kosovo",
      .default = countrycode::countrycode(iso3c, "iso3c", "country.name")
      ),
    # recode Kosovo from XKX code to KSV code
    iso3c = ifelse(iso3c == "XKX", "KSV", iso3c)
    ) %>%
  dplyr::select(-c(gdp, gdp.plus1)) %>%
  tidyr::drop_na(year)


### Maddison Project Dataset (MPD) -----------------------------------------------------------------
# codebook (available on sheet "Legend")
## countrycode: 3-letter ISO country code
## country: Country name
## year: Year
## cgdppc: Real GDP per capita in 2011US$, multiple benchmarks (suitable for cross-country income
### comparisons)
## rgdpnapc: Real GDP per capita in 2011US$, 2011 benchmark (suitable for cross-country growth
### comparisons)
## pop: Population, mid-year (thousands)
## i_cig:	0/1/2: observation is extrapolated (0), benchmark (1), or interpolated (2)
## i_bm: For benchmark observations: 1: ICP PPP estimates, 2: Historical income benchmarks, 3: Real
### wages and urbanization,
### 4: Multiple of subsistence, 5: Braithwaite (1968) PPPs

mpd <- readxl::read_xlsx("Data files/Raw data files/mpd2018.xlsx",
                         sheet = "Full data") %>%
  dplyr::rename(iso3c = countrycode) %>%
  dplyr::mutate(
    # convert population estimates to full number
    pop = pop * 1000,
    cgdp = pop * cgdppc,
    rgdpna = pop * rgdpnapc,
    # using the countrycode package, add country name based on iso3c code to standardize country 
    # names
    country = dplyr::case_when(
      iso3c == "CSK" ~ "Czechoslovakia", # other entries coded as "Czechia," though this includes Czechoslovakia
      iso3c == "SUN" ~ "Soviet Union",
      iso3c == "YUG" ~ "Yugoslavia",
      .default = countrycode::countrycode(iso3c, "iso3c", "country.name")
      ),
    # recode Soviet Union iso3c code
    iso3c = ifelse(iso3c == "SUN", "SOV", iso3c)
  ) %>%
  dplyr::arrange(iso3c, year)

# create dataset with year shifted to facilitate gdp growth rate calculations
mpd.plus1 <- mpd %>%
  dplyr::mutate(year = year + 1) %>%
  dplyr::rename(
    cgdp.plus1 = cgdp,
    rgdpna.plus1 = rgdpna
    ) %>%
  dplyr::select(iso3c, country, year, cgdp.plus1, rgdpna.plus1)

mpd <- mpd %>%
  dplyr::left_join(mpd.plus1, by = c("iso3c", "country", "year")) %>%
  dplyr::mutate(
    mpd.cgdp.growth = 100 * (cgdp - cgdp.plus1) / cgdp.plus1,
    mpd.rgdpna.growth = 100 * (rgdpna - rgdpna.plus1) / rgdpna.plus1
    )


### merge data -------------------------------------------------------------------------------------
gdp <- dplyr::full_join(pwt, gdpgl, by = c("iso3c", "country", "year")) %>%
  # merge in growth rate data
  dplyr::full_join(imf.growth.modern, by = c("iso3c", "country", "year")) %>%
  dplyr::full_join(imf.growth.historical, by = c("iso3c", "country", "year")) %>%
  dplyr::mutate(
    # note: for 2018 and 2019 growth rates, modern dataset rate is a rounded version of the
    # historical rate create a variable using the historical rate when available, using modern rate
    # if not only to be used for the 2018 and 2019 estimates, though coded for all country-years -
    # the datasets contain significant discrepancies with older years
    imf.growth.rate.extend = dplyr::coalesce(imf.growth.rate.historical,  imf.growth.rate.modern)
    ) %>%
  dplyr::full_join(wb.growth.data, by = c("iso3c", "country", "year")) %>%
  dplyr::full_join(mpd, by = c("iso3c", "country", "year")) %>%

  # expand to include all missing country-year entries
  dplyr::full_join(utils::read.csv("Data files/Formatted data files/country_years.csv"),
                   by = c("iso3c", "country", "year")) %>%
  dplyr::mutate(
    # use pwt estimates as a baseline for the estimated gdp.pwt.est variable, adjusted below
    gdp.pwt.est = gdp.pwt,
    # use gl estimates as a baseline for the estimated gdp.gl.est variable, adjusted below
    gdp.gl.est = gdp.gl
  ) %>%
  dplyr::select("iso3c", "country", "year", "gdp.pwt", "gdp.gl", "gdp.pwt.est", "gdp.gl.est",
                "imf.growth.rate.extend", "wb.growth.rate", "mpd.cgdp.growth",
                "mpd.rgdpna.growth") %>%
  dplyr::arrange(iso3c, year) %>%
  dplyr::filter(year >= 1945) %>%
  # remove 0 values from gdp.pwt.est and gdp.gl.est columns
  dplyr::mutate(
    gdp.pwt.est = ifelse(gdp.pwt.est == 0, NA, gdp.pwt.est),
    gdp.gl.est = ifelse(gdp.gl.est == 0, NA, gdp.gl.est)
  )


### gdp growth estimator functions -----------------------------------------------------------------
# this function is used to estimate either the pwt or gl gdp data based on the relative difference
# in the size of the economy between two years within the gl or pwt gdp data and applying the
# proportion to the pwt or gl gdp data
gdp_growth_estimator_func <- function(df = gdp,
                                      estimate = "pwt",
                                      iso,
                                      yr,
                                      restricted = c(1946:2019)){
  
  # identify the name of the estimate column
  est_column <- paste0("gdp.", estimate, ".est")
  
  # identify the column index number of the estimate column
  est_column_number <- which(names(df) == est_column)
  
  # identify the name of the reference column
  ref_column <- dplyr::case_when(
    estimate == "pwt" ~ "gdp.gl.est",
    estimate == "gl" ~ "gdp.pwt.est"
  )
  
  # identify the column index number of the estimate column
  ref_column_number <- which(names(df) == ref_column)
  
  # identify baseline/relative row number
  row_num <- which(df$iso3c == iso & df$year == yr)
  
  # pull the baseline estimate from which to base the proportions
  baseline <- as.numeric(df[row_num, ref_column_number])
  
  # pull the relative estimate from which to base the proportions
  relative <- as.numeric(df[row_num, est_column_number])
  
  prop_values <- relative * df[, ref_column_number] / baseline
  
  df <- df %>%
    as.data.frame() %>%
    dplyr::mutate(
      !!est_column := ifelse(iso3c == iso & is.na((!!sym(est_column))) & year %in% restricted,
                             relative * (!!sym(ref_column)) / baseline,
                             !!sym(est_column))
      )
  
  return(df)
  
}

# this function uses a growth estimate (default IMF) to extend both pwt and gl estimates from a
# start year (default 2018) through to an end year (default 2019)
gdp_growth_estimator_rate_func <- function(df = gdp, growth = "imf", iso,
                                           restricted = c(2018:2019)){
  
  # pull the column for the growth rate to use
  growth_column_name <- dplyr::case_when(
    growth == "imf" ~ "imf.growth.rate.extend",
    growth == "wb" ~ "wb.growth.rate"
  )
  
  # pull the column index
  growth_column_number <- which(names(df) == growth_column_name)
  
  for(y in restricted){
    
    # pull row numbers
    row_number <- which(df$iso3c == iso & df$year == y)
    row_number_prior <- which(df$iso3c == iso & df$year == (y - 1))
    
    # pull growth rate
    growth.rate <- df[row_number, growth_column_number]
    
    # calculate estimates
    est.pwt <- ((100 + growth.rate) * df[row_number_prior, "gdp.pwt.est"]) / 100
    est.gl <- ((100 + growth.rate) * df[row_number_prior, "gdp.gl.est"]) / 100
    
    df[row_number, "gdp.pwt.est"] <- est.pwt
    df[row_number, "gdp.gl.est"] <- est.gl    
    
  }
  
  return(df)
  
}

# this function uses a growth estimate (default IMF) to backdate both pwt and gl estimates from an
# end year (default 1949) through to a start year (default 1946)
gdp_growth_estimator_prior_rate_func <- function(df = gdp, growth = "imf", iso,
                                                 restricted = c(1949:1946)){
  
  # pull the column for the growth rate to use
  growth_column_name <- dplyr::case_when(
    growth == "imf" ~ "imf.growth.rate.extend",
    growth == "wb" ~ "wb.growth.rate",
    growth == "mpd" ~ "mpd.rgdpna.growth"
  )
  
  # pull the column index
  growth_column_number <- which(names(df) == growth_column_name)
  
  for(y in restricted){
    
    # pull row numbers
    row_number <- which(df$iso3c == iso & df$year == y)
    row_number_next <- which(df$iso3c == iso & df$year == (y + 1))
    
    # pull growth rate
    growth.rate <- df[row_number, growth_column_number]
    
    # calculate estimates
    est.pwt <- df[row_number_next, "gdp.pwt.est"] / (1 + (growth.rate / 100))
    est.gl <- df[row_number_next, "gdp.gl.est"] / (1 + (growth.rate / 100))
    
    df[row_number, "gdp.pwt.est"] <- est.pwt
    df[row_number, "gdp.gl.est"] <- est.gl    
    
  }
  
  return(df)
  
}

# this function approximates the 1946-1949 gdp of a country based on its growth rates the subsequent
# years.
# the function applies weighted growth rates of 1/2 year+1, 1/3 year+2, and 1/6 year+3
gdp_growth_estimator_no_data_func <- function(df = gdp, iso, startyr = 1949){
  
  for(y in startyr:1946){
    
    # calculate growth rates - pwt
    pwt.growth <- ((1/2) * df$gdp.pwt.est[df$iso3c == iso & df$year == (y + 2)] /
                     df$gdp.pwt.est[df$iso3c == iso & df$year == (y + 1)]) +
                  ((1/3) * df$gdp.pwt.est[df$iso3c == iso & df$year == (y + 3)] /
                     df$gdp.pwt.est[df$iso3c == iso & df$year == (y + 2)]) +
                  ((1/6) * df$gdp.pwt.est[df$iso3c == iso & df$year == (y + 4)] /
                     df$gdp.pwt.est[df$iso3c == iso & df$year == (y + 3)])

    # calculate growth rates - gl
    gl.growth <- ((1/2) * df$gdp.gl.est[df$iso3c == iso & df$year == (y + 2)] /
                    df$gdp.gl.est[df$iso3c == iso & df$year == (y + 1)]) +
                 ((1/3) * df$gdp.gl.est[df$iso3c == iso & df$year == (y + 3)] /
                    df$gdp.gl.est[df$iso3c == iso & df$year == (y + 2)]) +
                 ((1/6) * df$gdp.gl.est[df$iso3c == iso & df$year == (y + 4)] /
                    df$gdp.gl.est[df$iso3c == iso & df$year == (y + 3)])
    
    # add estimates
    df$gdp.pwt.est[df$iso3c == iso & df$year == y] <- df$gdp.pwt.est[df$iso3c == iso &
                                                                       df$year == (y + 1)] /
                                                                        pwt.growth
    df$gdp.gl.est[df$iso3c == iso & df$year == y] <- df$gdp.gl.est[df$iso3c == iso &
                                                                     df$year == (y + 1)] /
                                                                      gl.growth

    }
  
  return(df)
  
}

# this function approximates the 2012-2019 gdp of a country based on its growth rates the prior
# years.
# the function applies weighted growth rates of 1/2 year-1, 1/3 year-2, and 1/6 year-3
gdp_growth_estimator_no_data_future_func <- function(df = gdp, iso, startyr = 2012){
  
  for(y in startyr:2019){
    
    # calculate growth rates - pwt
    pwt.growth <- ((1/2) * df$gdp.pwt.est[df$iso3c == iso & df$year == (y - 1)] /
                     df$gdp.pwt.est[df$iso3c == iso & df$year == (y - 2)]) +
                  ((1/3) * df$gdp.pwt.est[df$iso3c == iso & df$year == (y - 2)] /
                     df$gdp.pwt.est[df$iso3c == iso & df$year == (y - 3)]) +
                  ((1/6) * df$gdp.pwt.est[df$iso3c == iso & df$year == (y - 3)] /
                     df$gdp.pwt.est[df$iso3c == iso & df$year == (y - 4)])
    
    # calculate growth rates - gl
    pwt.growth <- ((1/2) * df$gdp.gl.est[df$iso3c == iso & df$year == (y - 1)] /
                     df$gdp.gl.est[df$iso3c == iso & df$year == (y - 2)]) +
                  ((1/3) * df$gdp.gl.est[df$iso3c == iso & df$year == (y - 2)] /
                     df$gdp.gl.est[df$iso3c == iso & df$year == (y - 3)]) +
                  ((1/6) * df$gdp.gl.est[df$iso3c == iso & df$year == (y - 3)] /
                     df$gdp.gl.est[df$iso3c == iso & df$year == (y - 4)])
    
    # add estimates
    df$gdp.pwt.est[df$iso3c == iso & df$yera == y] <- df$gdp.pwt.est[df$iso3c == iso &
                                                                       df$year == (y - 1)] *
                                                                        pwt.growth
    df$gdp.gl.est[df$iso3c == iso & df$yera == y] <- df$gdp.gl.est[df$iso3c == iso &
                                                                     df$year == (y - 1)] *
                                                                      gl.growth

  }
  
  return(df)
  
}


### calculate estimates ----------------------------------------------------------------------------
#### AFG: Afghanistan(p) ---------------------------------------------------------------------------
# AFG gl 2008-2011 estimates are the same
# recalculate based on 2008 estimate and imf growth rates + extend through to 2019

# 2009-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "AFG", restricted = c(2009:2019))

# # calculate gdp.gl.est based on imf growth rates and prior year gdp.gl.est
# for(a in c(2009:2019)){
#   
#   est <- gdp$gdp.gl.est[gdp$iso3c == "AFG" & gdp$year == (a - 1)] *
#     (1 + (growth.rate.datasets$imf.growth.rate.extend[growth.rate.datasets$iso3c == "AFG" &
#                                                         growth.rate.datasets$year == a] / 100))
#   
#   gdp$gdp.gl.est[gdp$iso3c == "AFG" & gdp$year == a] <- est
#   
# }

# no gdp.pwt data, so use gdp.gl data as an estimate
gdp <- gdp %>%
  dplyr::mutate(gdp.pwt.est = ifelse(iso3c == "AFG", gdp.gl.est, gdp.pwt.est))

# 1946-1949: apply 3-year moving average weighted growth rates
gdp <- gdp_growth_estimator_no_data_func(gdp, "AFG", startyr = 1949)

# # calculate the average differences (absolute differences and % differences) between
# # pwt and gl data for countries most comparable to AFG: PAK, IRN, TJK, TKM, KGZ, UZB
# gdp_afg_compare <- gdp %>%
#   dplyr::filter(iso3c %in% c("PAK","IRN","TJK","TKM","KGZ","UZB","BGD")) %>%
#   # only comparing original data (after converting to 2019$), not estimates
#   dplyr::select(-c(gdp.pwt.est,gdp.gl.est)) %>%
#   # for purposes of this comparison, recode BGD 1950-1970 as PAK for pwt estimates, as
#   # gl estimates for PAK include BGD before BGD independence, while pwt estimates do not
#   dplyr::mutate(gdp.gl = ifelse(iso3c=="BGD",NA,gdp.gl),
#                 iso3c = ifelse(iso3c=="BGD"&year<1971,"PAK",iso3c)) %>%
#   dplyr::group_by(iso3c,year) %>%
#   dplyr::summarise(gdp.pwt = sum(gdp.pwt,na.rm=TRUE),
#                   gdp.gl = sum(gdp.gl,na.rm=TRUE)) %>%
#   dplyr::ungroup() %>%
#   dplyr::filter(iso3c != "BGD") %>%
#   # converts gdp values of 0 back into NAs and calculate comparison metrics
#   dplyr::mutate(gdp.pwt = ifelse(gdp.pwt==0,NA,gdp.pwt),
#                 gdp.gl = ifelse(gdp.gl==0,NA,gdp.gl),
#                 # BGD pwt estimates start in 1959, so prior years' estimates are omitting East Pakistan;
#                 # replace estimates with NA
#                 gdp.pwt = ifelse(iso3c=="PAK"&year<1959,NA,gdp.pwt),
#                 pwt.minus.gl = gdp.pwt - gdp.gl,
#                 pwt.perc.gl = gdp.pwt/gdp.gl)

# # average differences by year (collapse countries)
# gdp_afg_compare_year <- gdp_afg_compare %>%
#   dplyr::select(iso3c,year,pwt.minus.gl,pwt.perc.gl) %>%
#   na.omit() %>%
#   dplyr::group_by(year) %>%
#   dplyr::summarise(pwt.minus.gl.avg = mean(pwt.minus.gl,na.rm=TRUE),
#                    pwt.perc.gl.avg = mean(pwt.perc.gl,na.rm=TRUE),
#                    n = n()) %>%
#   dplyr::ungroup()

# # average differences by country (collapse years)
# gdp_afg_compare_country <- gdp_afg_compare %>%
#   dplyr::select(iso3c,year,pwt.minus.gl,pwt.perc.gl) %>%
#   na.omit() %>%
#   dplyr::group_by(iso3c) %>%
#   dplyr::summarise(pwt.minus.gl.avg = mean(pwt.minus.gl,na.rm=TRUE),
#                    pwt.perc.gl.avg = mean(pwt.perc.gl,na.rm=TRUE),
#                    n = n()) %>%
#   dplyr::ungroup()
# # 5/6 countries (PAK, TJK, TKM, KGZ, UZB) match closesly between pwt and gl metrics, while
# # IRN is slightly over twice as large in the pwt estimates versus the gl estimates.

# # glm
# gdp_afg_compare_glm <- stats::glm(pwt.perc.gl ~ iso3c + year + log(gdp.gl), data = gdp_afg_compare)
# summary(gdp_afg_compare_glm)

#### AGO: Angola -----------------------------------------------------------------------------------
# 1970-1974: AGO coded as gaining independence in 1975

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "AGO", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "AGO", restricted = c(2018:2019))

#### ALB: Albania ----------------------------------------------------------------------------------
# 1950-69: apply gdp.gl proportion to 1970 gdp.gl estimate and use that ratio on gdp.pwt estimate
gdp <- gdp_growth_estimator_func(gdp, "pwt", "ALB", 1970, restricted = c(1950:1969))

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "ALB", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "ALB", restricted = c(2018:2019))

# 1946-1949: apply 3-year moving average weighted growth rates
gdp <- gdp_growth_estimator_no_data_func(gdp, "ALB")

#### AND: Andorra (p) ------------------------------------------------------------------------------
# no gdp.pwt data, so use gdp.gl data as an estimate
gdp <- gdp %>%
  dplyr::mutate(gdp.pwt.est = ifelse(iso3c == "AND",
                                     dplyr::coalesce(gdp.pwt.est, gdp.gl),
                                     gdp.pwt.est))

# 2012-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "AND", restricted = c(2018:2019))

# 1946-1949: apply 3-year moving average weighted growth rates
gdp <- gdp_growth_estimator_no_data_func(gdp, "AND")

#### ARE: United Arab Emirates ---------------------------------------------------------------------
# 1970: ARE coded as gaining independence in 1971

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "ARE", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "ARE", restricted = c(2018:2019))

#### ARG: Argentina --------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "ARG", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "ARG", restricted = c(2018:2019))

# 1946-1949: apply imf growth rates
gdp <- gdp_growth_estimator_prior_rate_func(gdp, "imf", "ARG", restricted = c(1949:1946))

# # plot gdp.pwt.est and gdp.gl.est line graphs
# gdp.arg <- gdp %>%
#   dplyr::filter(iso3c == "ARG") %>%
#   dplyr::arrange(year)

# plot(gdp.arg$year,gdp.arg$gdp.pwt.est, type = 'l')
# lines(gdp.arg$year,gdp.arg$gdp.gl.est, type = 'l')

#### ARM: Armenia ----------------------------------------------------------------------------------
# 1990: ARM coded as gaining independence in 1991

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "ARM", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "ARM", restricted = c(2018:2019))

#### ATG: Antigua and Barbuda ----------------------------------------------------------------------
# 1970-1980: ATG coded as gaining independence in 1981

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "ATG", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "ATG", restricted = c(2018:2019))

#### AUS: Australia --------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "AUS", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "AUS", restricted = c(2018:2019))

# 1946-1949: apply imf growth rates
gdp <- gdp_growth_estimator_prior_rate_func(gdp, "imf", "AUS", restricted = c(1949:1946))

#### AUT: Austria ----------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "AUT", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "AUT", restricted = c(2018:2019))

# 1946-1949: apply imf growth rates
gdp <- gdp_growth_estimator_prior_rate_func(gdp, "imf", "AUT", restricted = c(1949:1946))

#### AZE: Azerbaijan -------------------------------------------------------------------------------
# 1990: AZE coded as gaining independence in 1991

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "AZE", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "AZE", restricted = c(2018:2019))

#### BDI: Burundi ----------------------------------------------------------------------------------
# 1960-1961: BDI coded as gaining independence in 1962

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "BDI", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "BDI", restricted = c(2018:2019))

#### BEL: Belgium ----------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "BEL", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "BEL", restricted = c(2018:2019))

# 1946-1949: apply imf growth rates
gdp <- gdp_growth_estimator_prior_rate_func(gdp, "imf", "BEL", restricted = c(1949:1946))

#### BEN: Benin ------------------------------------------------------------------------------------
# 1959: BEN coded as gaining independence in 1960

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "BEN", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "BEN", restricted = c(2018:2019))

#### BFA: Burkina Faso -----------------------------------------------------------------------------
# 1959: BFA coded as gaining independence in 1960

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "BFA", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "BFA", restricted = c(2018:2019))

#### BGD/PAK: Bangladesh/Pakistan ------------------------------------------------------------------
# pwt codes PAK and BGD separate prior to 1971; gl codes PAK as unified until 1971,
# with PAK not including East Pakistan/Bangladesh starting that year

# BGD coded as gaining independence in 1971

# capture pwt 1970-1971 gdp growth
pak.pwt.growth.1970.1971 <- gdp$gdp.pwt.est[gdp$iso3c == "PAK" & gdp$year == 1971] /
  gdp$gdp.pwt.est[gdp$iso3c == "PAK" & gdp$year == 1970]
bgd.pwt.growth.1970.1971 <- gdp$gdp.pwt.est[gdp$iso3c == "BGD" & gdp$year == 1971] /
  gdp$gdp.pwt.est[gdp$iso3c == "BGD" & gdp$year == 1970]

# pwt estimates code PAK and BGD separate through 1971, though BGD estimates only start in 1959
# (Method 1) extend BGD estimates back based on PAK (pwt) growth rates
# (Method 2) combine PAK and BGD, then extend back using PAK (gl) growth rates, removing 1950-1958
# PAK pwt estimates

# Method 1
# extend BGD pwt estimates back to 1950 using PAK pwt growth rates, then combine BGD and PAK
# 1950-1970

# pull dataset of just PAK to calculate growth rates
gdp.bgd.method1.growth.rates <- gdp %>%
  dplyr::filter(iso3c == "PAK")

# pull reference year to calculate the growth rates relative to 1959
gdp.bgd.method1.ref.gdp <- gdp.bgd.method1.growth.rates$gdp.pwt.est[gdp.bgd.method1.growth.rates$year == 1959]

# calculate growth rates relative to 1959
gdp.bgd.method1.growth.rates <- gdp.bgd.method1.growth.rates %>%
  dplyr::mutate(multiplier = gdp.pwt.est / gdp.bgd.method1.ref.gdp) %>%
  dplyr::select(year,multiplier)

# pull dataset of just BGD to apply growth rates to
gdp.bgd.method1.estimates <- gdp %>%
  dplyr::filter(iso3c == "BGD") %>%
  dplyr::left_join(gdp.bgd.method1.growth.rates, by = "year")

gdp.bgd.method1.ref.gdp <- gdp.bgd.method1.estimates$gdp.pwt.est[gdp.bgd.method1.estimates$year == 1959]

gdp.bgd.method1.estimates <- gdp.bgd.method1.estimates %>%
  dplyr::mutate(gdp.pwt.est = ifelse(is.na(gdp.pwt.est),
                                     gdp.bgd.method1.ref.gdp * multiplier,
                                     gdp.pwt.est)) %>%
  dplyr::select(iso3c, year, gdp.pwt.est)

gdp.bgd.method1.combined <- gdp %>%
  dplyr::filter(iso3c == "PAK",
                year < 1971) %>%
  dplyr::select(iso3c, year, gdp.pwt.est) %>%
  rbind(gdp.bgd.method1.estimates) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(gdp.pwt.est = sum(gdp.pwt.est, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::rename(gdp.pwt.est.method1 = gdp.pwt.est)

# Method 2
# remove pwt PAK 1950-1958 estimates, combine pwt PAK and BGD estimates, and use PAK gl growth rates
# to extend back to 1950

# pull PAK and BGD gdp data
gdp.bgd.method2 <- gdp %>%
  dplyr::filter(iso3c %in% c("PAK", "BGD"))

# remove PAK pwt estimates from 1950-1958
gdp.bgd.method2$gdp.pwt.est[gdp.bgd.method2$iso3c == "PAK" &
                              gdp.bgd.method2$year %in% c(1950:1958)] <- NA

gdp.bgd.method2.combined <- gdp.bgd.method2 %>%
  # filter only years prior to BGD independence
  dplyr::filter(year %in% c(1950:1970)) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(gdp.pwt.est = sum(gdp.pwt.est, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(gdp.pwt.est = ifelse(gdp.pwt.est == 0, NA, gdp.pwt.est))

gdp.bgd.method2.growth.rates <- gdp %>%
  dplyr::filter(iso3c == "PAK")

gdp.bgd.method2.ref.gdp <- gdp.bgd.method2.growth.rates$gdp.gl.est[gdp.bgd.method2.growth.rates$year == 1959]

gdp.bgd.method2.growth.rates <- gdp.bgd.method2.growth.rates %>%
  dplyr::mutate(multiplier = gdp.gl.est / gdp.bgd.method2.ref.gdp) %>%
  dplyr::select(year, multiplier)

gdp.bgd.method2.ref.gdp2 <- gdp.bgd.method2.combined$gdp.pwt.est[gdp.bgd.method2.combined$year == 1959]

gdp.bgd.method2.combined <- gdp.bgd.method2.combined %>%
  dplyr::left_join(gdp.bgd.method2.growth.rates, by = "year") %>%
  dplyr::mutate(gdp.pwt.est = ifelse(is.na(gdp.pwt.est),
                                     gdp.bgd.method2.ref.gdp2 * multiplier,
                                     gdp.pwt.est)) %>%
  dplyr::select(-multiplier) %>%
  dplyr::rename(gdp.pwt.est.method2 = gdp.pwt.est)

# combine methods
gdp.bgd.combined.methods <- dplyr::full_join(gdp.bgd.method1.combined,
                                             gdp.bgd.method2.combined,
                                             by = "year") %>%
  dplyr::filter(year < 1971) %>%
  dplyr::mutate(iso3c = "PAK",
                # use the average between the two methods as the pwt estimate
                gdp.pwt.est.avg = (gdp.pwt.est.method1 + gdp.pwt.est.method2) / 2) %>%
  dplyr::select(iso3c, year, gdp.pwt.est.avg)

gdp <- gdp %>%
  dplyr::left_join(gdp.bgd.combined.methods, by = c("iso3c", "year")) %>%
  dplyr::mutate(gdp.pwt.est = ifelse(iso3c == "PAK" & year %in% c(1950:1970),
                                     gdp.pwt.est.avg,
                                     gdp.pwt.est)) %>%
  dplyr::select(-gdp.pwt.est.avg)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "BGD", 2011, restricted = c(2012:2017))
gdp <- gdp_growth_estimator_func(gdp, "gl", "PAK", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "BGD", restricted = c(2018:2019))
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "PAK", restricted = c(2018:2019))

# 1948-1949: apply imf growth rates
gdp <- gdp_growth_estimator_prior_rate_func(gdp, "imf", "PAK", restricted = c(1949:1948))

# remove blank 1946 and 1947 PAK entries
gdp <- gdp %>%
  dplyr::filter(iso3c != "PAK" | year %!in% c(1946:1947))

# 1946-1947: apply 3-year moving average weighted growth rates

# calculate growth rates - pwt
pak.pwt.growth.49 <- gdp$gdp.pwt.est[gdp$iso3c == "PAK" & gdp$year == 1949] /
  gdp$gdp.pwt.est[gdp$iso3c == "PAK" & gdp$year == 1948]
pak.pwt.growth.50 <- gdp$gdp.pwt.est[gdp$iso3c == "PAK" & gdp$year == 1950] /
  gdp$gdp.pwt.est[gdp$iso3c == "PAK" & gdp$year == 1949]
pak.pwt.growth.51 <- gdp$gdp.pwt.est[gdp$iso3c == "PAK" & gdp$year == 1951] /
  gdp$gdp.pwt.est[gdp$iso3c == "PAK" & gdp$year == 1950]

pak.pwt.growth.48 <- (1/2) * pak.pwt.growth.49 + (1/3) * pak.pwt.growth.50 +
  (1/6) * pak.pwt.growth.51
pak.pwt.growth.47 <- (1/2) * pak.pwt.growth.48 + (1/3) * pak.pwt.growth.49 +
  (1/6) * pak.pwt.growth.50

# calculate growth rates - gl
pak.gl.growth.49 <- gdp$gdp.gl.est[gdp$iso3c == "PAK" & gdp$year == 1949] /
  gdp$gdp.gl.est[gdp$iso3c == "PAK" & gdp$year == 1948]
pak.gl.growth.50 <- gdp$gdp.gl.est[gdp$iso3c == "PAK" & gdp$year == 1950] /
  gdp$gdp.gl.est[gdp$iso3c == "PAK" & gdp$year == 1949]
pak.gl.growth.51 <- gdp$gdp.gl.est[gdp$iso3c == "PAK" & gdp$year == 1951] /
  gdp$gdp.gl.est[gdp$iso3c == "PAK" & gdp$year == 1950]

pak.gl.growth.48 <- (1/2) * pak.gl.growth.49 + (1/3) * pak.gl.growth.50 + (1/6) * pak.gl.growth.51
pak.gl.growth.47 <- (1/2) * pak.gl.growth.48 + (1/3) * pak.gl.growth.49 + (1/6) * pak.gl.growth.50

# add 1946-1947
gdp$gdp.pwt.est[gdp$iso3c == "PAK" & gdp$year == 1947] <- gdp$gdp.pwt.est[gdp$iso3c == "PAK" &
                                                                            gdp$year == 1948] /
                                                                              pak.pwt.growth.48
gdp$gdp.gl.est[gdp$iso3c == "PAK" & gdp$year == 1947] <- gdp$gdp.gl.est[gdp$iso3c == "PAK" &
                                                                          gdp$year == 1948] /
                                                                            pak.gl.growth.48

gdp$gdp.pwt.est[gdp$iso3c == "PAK" & gdp$year == 1946] <- gdp$gdp.pwt.est[gdp$iso3c == "PAK" &
                                                                            gdp$year == 1947] / 
                                                                              pak.pwt.growth.47
gdp$gdp.gl.est[gdp$iso3c == "PAK" & gdp$year == 1946] <- gdp$gdp.gl.est[gdp$iso3c == "PAK" &
                                                                          gdp$year == 1947] / 
                                                                            pak.gl.growth.47

# capture gl 1970-1971 gdp growth
pak.gl.growth.1970.1971 <- sum(gdp$gdp.gl.est[gdp$iso3c %in% c("PAK", "BGD") & gdp$year == 1971]) /
                                gdp$gdp.gl.est[gdp$iso3c == "PAK" & gdp$year == 1970]
bgd.gl.growth.1970.1971 <- sum(gdp$gdp.gl.est[gdp$iso3c %in% c("PAK", "BGD") & gdp$year == 1971]) /
                                gdp$gdp.gl.est[gdp$iso3c == "PAK" & gdp$year == 1970]

#### BGR: Bulgaria ---------------------------------------------------------------------------------
# 1950-69: apply gdp.gl proportion to 1970 gdp.gl estimate and use that ratio on gdp.pwt estimate
gdp <- gdp_growth_estimator_func(gdp, "pwt", "BGR", 1970, restricted = c(1950:1969))

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "BGR", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "BGR", restricted = c(2018:2019))

# 1946-1949: estimated consistent growth rates between mpd's 1945 and 1950 gdp estimates (rgdpna)
bgr.growth.estimate <- (mpd$rgdpna[mpd$iso3c == "BGR" & mpd$year == 1950] /
                          mpd$rgdpna[mpd$iso3c == "BGR" & mpd$year == 1945])^(1/5)

for (b in c(1949:1946)){
  
  gdp$gdp.pwt.est[gdp$iso3c == "BGR" & gdp$year == b] <- gdp$gdp.pwt.est[gdp$iso3c == "BGR" &
                                                                           gdp$year == (b + 1)] /
                                                                            bgr.growth.estimate
  gdp$gdp.gl.est[gdp$iso3c == "BGR" & gdp$year == b] <- gdp$gdp.gl.est[gdp$iso3c == "BGR" &
                                                                         gdp$year == (b + 1)] /
                                                                          bgr.growth.estimate
  
}

#### BHR: Bahrain ----------------------------------------------------------------------------------
# 1970: BHR coded as gaining independence in 1971

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "BHR", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "BHR", restricted = c(2018:2019))

#### BHS: The Bahamas ------------------------------------------------------------------------------
# 1970-1972: BHS coded as gaining independence in 1973

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "BHS", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "BHS", restricted = c(2018:2019))

#### BIH: Bosnia and Herzegovina -------------------------------------------------------------------
# 1990-1991: BIH coded as gaining independence in 1992

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "BIH", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "BIH", restricted = c(2018:2019))

#### BLR: Belarus ----------------------------------------------------------------------------------
# 1990: BLR coded as gaining independence in 1991

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "BLR", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "BLR", restricted = c(2018:2019))

#### BLZ: Belize -----------------------------------------------------------------------------------
# 1970-1980: BLZ coded as gaining independence in 1981

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "BLZ", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "BLZ", restricted = c(2018:2019))

#### BOL: Bolivia ----------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "BOL", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "BOL", restricted = c(2018:2019))

# 1946-1949: apply imf growth rates
gdp <- gdp_growth_estimator_prior_rate_func(gdp, "imf", "BOL", restricted = c(1949:1946))

#### BRA: Brazil -----------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "BRA", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "BRA", restricted = c(2018:2019))

# 1946-1949: apply imf growth rates
gdp <- gdp_growth_estimator_prior_rate_func(gdp, "imf", "BRA", restricted = c(1949:1946))

#### BRB: Barbados ---------------------------------------------------------------------------------
# 1960-1965: BRB coded as gaining independence in 1966

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "BRB", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "BRB", restricted = c(2018:2019))

#### BRN: Brunei -----------------------------------------------------------------------------------
# 1970-1983: BRB coded as gaining independence in 1984

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "BRN", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "BRN", restricted = c(2018:2019))

#### BTN: Bhutan -----------------------------------------------------------------------------------
# 1950-1969: BTN coded as gaining independence in 1971 (both pwt and gl already have values for
# 1970)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "BTN", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "BTN", restricted = c(2018:2019))

#### BWA: Botswana ---------------------------------------------------------------------------------
# 1960-1965: BRB coded as gaining independence in 1966

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "BWA", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "BWA", restricted = c(2018:2019))

#### CAF: Central African Republic -----------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "CAF", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "CAF", restricted = c(2018:2019))

#### CAN: Canada -----------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "CAN", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "CAN", restricted = c(2018:2019))

# 1946-1949: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "CAN", restricted = c(1949:1946))

#### CHE: Switzerland ------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "CHE", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "CHE", restricted = c(2018:2019))

# 1946-1949: apply imf growth rates
gdp <- gdp_growth_estimator_prior_rate_func(gdp, "imf", "CHE", restricted = c(1949:1946))

#### CHL: Chile ------------------------------------------------------------------------------------
# 1950: apply gdp.gl proportion to 1951 gdp.gl estimate and use that ratio on gdp.pwt estimate
gdp <- gdp_growth_estimator_func(gdp, "pwt", "CHL", 1951, restricted = 1950)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "CHL", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "CHL", restricted = c(2018:2019))

# 1946-1949: apply imf growth rates
gdp <- gdp_growth_estimator_prior_rate_func(gdp, "imf", "CHL", restricted = c(1949:1946))

#### CHN: China ------------------------------------------------------------------------------------
# 1950-51: apply gdp.gl proportion to 1952 gdp.gl estimate and use that ratio on gdp.pwt estimate
gdp <- gdp_growth_estimator_func(gdp, "pwt", "CHN", 1952, restricted = c(1950:1951))

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "CHN", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "CHN", restricted = c(2018:2019))

# 1946-1949: apply 3-year moving average weighted growth rates
gdp <- gdp_growth_estimator_no_data_func(gdp, "CHN")

#### CIV: Côte d'Ivoire ----------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "CIV", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "CIV", restricted = c(2018:2019))

#### CMR: Cameroon ---------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "CMR", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "CMR", restricted = c(2018:2019))

#### COD: Democratic Republic of the Congo ---------------------------------------------------------
# 1950-1959: COD coded as gaining independence in 1960

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "COD", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "COD", restricted = c(2018:2019))

#### COG: Republic of the Congo --------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "COG", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "COG", restricted = c(2018:2019))

#### COL: Colombia ---------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "COL", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "COL", restricted = c(2018:2019))

# 1946-1949: apply imf growth rates
gdp <- gdp_growth_estimator_prior_rate_func(gdp, "imf", "COL", restricted = c(1949:1946))

#### COM: Comoros ----------------------------------------------------------------------------------
# 1960-1974: COM coded as gaining independence in 1975

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "COM", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "COM", restricted = c(2018:2019))

#### CPV: Cabo Verde -------------------------------------------------------------------------------
# 1960-1974: CPV coded as gaining independence in 1975

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "CPV", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "CPV", restricted = c(2018:2019))

#### CRI: Costa Rica -------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "CRI", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "CRI", restricted = c(2018:2019))

# 1946-1949: apply imf growth rates
gdp <- gdp_growth_estimator_prior_rate_func(gdp, "imf", "CRI", restricted = c(1949:1946))

#### CUB: Cuba (p) ---------------------------------------------------------------------------------
# no gdp.pwt data, so use gdp.gl data as an estimate
gdp <- gdp %>%
  dplyr::mutate(gdp.pwt.est = ifelse(iso3c == "CUB",
                                     dplyr::coalesce(gdp.pwt.est, gdp.gl),
                                     gdp.pwt.est))

# 2012-2018: apply wb growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "wb", "CUB", restricted = c(2012:2019))

# # 2019: EIU country profile estimates 0.5% growth in 2019
gdp <- gdp %>%
  dplyr::mutate(
    gdp.pwt.est = ifelse(iso3c == "CUB" & year == 2019,
                         gdp$gdp.pwt.est[gdp$iso3c=="CUB"&gdp$year==2018] * 1.005,
                         gdp.pwt.est),
    gdp.gl.est = ifelse(iso3c == "CUB" & year == 2019,
                        gdp$gdp.gl.est[gdp$iso3c == "CUB" & gdp$year == 2018] * 1.005,
                        gdp.gl.est))

# 1946-1949: apply mpd growth rates
gdp <- gdp_growth_estimator_prior_rate_func(gdp, "mpd", "CUB", restricted = c(1949:1946))

#### CYP: Cyprus -----------------------------------------------------------------------------------
# 1950-1959: CYP coded as gaining independence in 1960

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "CYP", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "CYP", restricted = c(2018:2019))

#### CZE/SVK: Czechoslovakia, Czechia, and Slovakia ------------------------------------------------
# both pwt and gl code CZE as solely Czechia throughout the timeseries

# CZE
# 1950-89: apply gdp.gl proportion to 1990 gdp.gl estimate and use that ratio on gdp.pwt estimate
gdp <- gdp_growth_estimator_func(gdp, "pwt", "CZE", 1990, restricted = c(1989:1950))

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "CZE", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "CZE", restricted = c(2018:2019))

# 1948-1949: apply mpd growth rates (coded as CSK)
for(y in 1949:1948){
  
  gdp$gdp.pwt.est[gdp$iso3c == "CZE" & gdp$year == y] <- gdp$gdp.pwt.est[gdp$iso3c == "CZE" &
                                                                           gdp$year == (y + 1)] /
    (1 + (gdp$mpd.rgdpna.growth[gdp$iso3c == "CSK" & gdp$year == (y + 1)] / 100))
  gdp$gdp.gl.est[gdp$iso3c == "CZE" & gdp$year == y] <- gdp$gdp.gl.est[gdp$iso3c == "CZE" &
                                                                         gdp$year == (y + 1)] /
    (1 + (gdp$mpd.rgdpna.growth[gdp$iso3c == "CSK" & gdp$year == (y + 1)] / 100))

}

# 1946-1947: apply 3-year moving average weighted growth rates
gdp <- gdp_growth_estimator_no_data_func(gdp, "CZE", startyr = 1947)

# SVK
# SVK coded as gaining independence in 1993

# 1985-1992: calculate mpd growth rates and apply to gl estimates
for(y in 1992:1985){
  
  # calculate growth rates - gl
  gl.growth <- (1/2) * gdp$gdp.gl.est[gdp$iso3c == "SVK" & gdp$year == (y + 2)] /
                  gdp$gdp.gl.est[gdp$iso3c == "SVK" & gdp$year == (y + 1)] +
               (1/3) * gdp$gdp.gl.est[gdp$iso3c == "SVK" & gdp$year == (y + 3)] /
                  gdp$gdp.gl.est[gdp$iso3c == "SVK" & gdp$year == (y + 2)] +
               (1/6) * gdp$gdp.gl.est[gdp$iso3c == "SVK" & gdp$year == (y + 4)] /
                  gdp$gdp.gl.est[gdp$iso3c == "SVK" & gdp$year == (y + 3)]
    
  gdp <- gdp %>%
    dplyr::mutate(gdp.gl.est = ifelse(iso3c == "SVK" & year == y,
                                      gdp$gdp.gl.est[gdp$iso3c == "SVK" & gdp$year == (y + 1)] /
                                        gl.growth,
                                       gdp.gl.est))
  
}

# 1985-1990: calculate mpd growth rates and apply to pwt estimates
# change SVK 1990 estimate to NA, as it does not align with 1991- values
gdp$gdp.pwt.est[gdp$iso3c == "SVK" & gdp$year == 1990] <- NA

for(y in 1990:1985){
  
  # calculate growth rates - pwt
  pwt.growth <- (1/2) * gdp$gdp.pwt.est[gdp$iso3c == "SVK" & gdp$year == (y + 2)] /
                  gdp$gdp.pwt.est[gdp$iso3c == "SVK" & gdp$year == (y + 1)] +
                (1/3) * gdp$gdp.pwt.est[gdp$iso3c == "SVK" & gdp$year == (y + 3)] /
                  gdp$gdp.pwt.est[gdp$iso3c == "SVK" & gdp$year == (y + 2)] +
                (1/6) * gdp$gdp.pwt.est[gdp$iso3c == "SVK" & gdp$year == (y + 4)] /
                  gdp$gdp.pwt.est[gdp$iso3c == "SVK" & gdp$year == (y + 3)]
  
  gdp <- gdp %>%
    dplyr::mutate(gdp.pwt.est = ifelse(iso3c == "SVK" & year == y,
                                       gdp$gdp.pwt.est[gdp$iso3c == "SVK" & gdp$year == (y + 1)] /
                                         pwt.growth,
                                      gdp.pwt.est))

}

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "SVK", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "SVK", restricted = c(2018:2019))

# capture growth rates
cze.pwt.growth.1992.1993 <- gdp$gdp.pwt.est[gdp$iso3c == "CZE" & gdp$year == 1993] /
                              gdp$gdp.pwt.est[gdp$iso3c == "CZE" & gdp$year == 1992]
svk.pwt.growth.1992.1993 <- gdp$gdp.pwt.est[gdp$iso3c == "SVK" & gdp$year == 1993] /
                              gdp$gdp.pwt.est[gdp$iso3c == "SVK" & gdp$year == 1992]
cze.gl.growth.1992.1993 <- gdp$gdp.gl.est[gdp$iso3c == "CZE" & gdp$year == 1993] /
                              gdp$gdp.gl.est[gdp$iso3c == "CZE" & gdp$year == 1992]
svk.gl.growth.1992.1993 <- gdp$gdp.gl.est[gdp$iso3c == "SVK" & gdp$year == 1993] /
                              gdp$gdp.gl.est[gdp$iso3c == "SVK" & gdp$year == 1992]

# 1950-1984: apply CZE growth rates to SVK
# pull CZE growth rates
cze.growth <- gdp %>%
  dplyr::filter(iso3c == "CZE",
                year < 1993) %>%
  dplyr::select(-c(gdp.pwt, gdp.gl))

cze.growth.plus1 <- cze.growth %>%
  dplyr::mutate(year = year + 1) %>%
  dplyr::rename(gdp.pwt.est.plus1 = gdp.pwt.est,
                gdp.gl.est.plus1 = gdp.gl.est)

cze.growth <- cze.growth %>%
  dplyr::left_join(cze.growth.plus1, by = c("iso3c", "country", "year")) %>%
  dplyr::mutate(gdp.growth.pwt = gdp.pwt.est / gdp.pwt.est.plus1,
                gdp.growth.gl = gdp.gl.est / gdp.gl.est.plus1) %>%
  dplyr::select(iso3c, year, gdp.growth.pwt, gdp.growth.gl)

for(s in 1984:1950){
  
  gdp <- gdp %>%
    dplyr::mutate(
      gdp.pwt.est = ifelse(iso3c == "SVK" & year == s,
                           gdp$gdp.pwt.est[gdp$iso3c == "SVK" & gdp$year == (s + 1)] /
                             cze.growth$gdp.growth.pwt[cze.growth$year == (s + 1)],
                           gdp.pwt.est),
      gdp.gl.est = ifelse(iso3c == "SVK" & year == s,
                            gdp$gdp.gl.est[gdp$iso3c == "SVK" & gdp$year == (s + 1)] /
                              cze.growth$gdp.growth.gl[cze.growth$year == (s + 1)],
                           gdp.gl.est)
      )

}

# 1946-1949: apply 3-year moving average weighted growth rates
gdp <- gdp_growth_estimator_no_data_func(gdp, "SVK", startyr = 1949)

# 1946-1992: add SVK estimates to CZE estimates
for(i in 1946:1992){
  
  gdp$gdp.pwt.est[gdp$iso3c == "CZE" & gdp$year == i] <- sum(gdp$gdp.pwt.est[gdp$iso3c %in% c("CZE", "SVK") &
                                                                           gdp$year == i])
  gdp$gdp.gl.est[gdp$iso3c=="CZE"&gdp$year==i] <- sum(gdp$gdp.gl.est[gdp$iso3c %in% c("CZE", "SVK") &
                                                                       gdp$year == i])

}

#### DEU/BRD/DDR: Germany, East Germany, and West Germany ------------------------------------------
# pwt data combines East and West Germany, while gl separates East and West Germany through to 1990 
# (inclusive)

# East and West Germany are coded as existing through 1989 (inclusive), with a unified Germany coded
# as beginning starting in 1990

# gl data for 1950-1970 and for 1988-1990 is the same

# recalculate gl DDR estimates for 1950-1969
## (1) combine gl BRD and gl DDR 1970 gdp estimates
## (2) calculate pwt combined DEU growth estimates for 1950-1970
## (3) apply pwt combined DEU growth estimates to gl combined 1970 estimate
## (4) subtract gl BRD estimates from combined gl estimates

gl.deu.combined.70 <- sum(gdp$gdp.gl.est[gdp$iso3c %in% c("BRD", "DDR") & gdp$year == 1970])

pwt.deu.growth.estimates <- gdp %>%
  dplyr::filter(iso3c == "DEU") %>%
  dplyr::select(year, gdp.pwt.est) %>%
  dplyr::arrange(year) %>%
  dplyr::mutate(
    gdp.pwt.est.lag = dplyr::lag(gdp.pwt.est),
    pwt.combined.growth = (gdp.pwt.est - gdp.pwt.est.lag) / gdp.pwt.est.lag
    )

pwt.deu.growth.estimates$gl.combined.estimate[pwt.deu.growth.estimates$year == 1970] <- gl.deu.combined.70

# calculate pwt estimates 1950-1969
for(g in 1969:1950){
  
  est <- pwt.deu.growth.estimates$gl.combined.estimate[pwt.deu.growth.estimates$year == (g + 1)] /
    (1 + pwt.deu.growth.estimates$pwt.combined.growth[pwt.deu.growth.estimates$year == (g + 1)])
  
  pwt.deu.growth.estimates$gl.combined.estimate[pwt.deu.growth.estimates$year == g] <- est
  
}

gl.deu.combined.estimates <- pwt.deu.growth.estimates %>%
  dplyr::select(year, gl.combined.estimate)

gl.brd <- gdp %>%
  dplyr::filter(iso3c == "BRD") %>%
  dplyr::select(year, gdp.gl.brd.est = gdp.gl.est)

gl.deu.combined.estimates <- gl.deu.combined.estimates %>%
  dplyr::left_join(gl.brd, by = "year") %>%
  dplyr::mutate(
    gdp.gl.ddr.est = gl.combined.estimate - gdp.gl.brd.est,
    iso3c = "DDR",
    country = "East Germany"
    ) %>%
  dplyr::select(iso3c, country, year, gdp.gl.ddr.est) %>%
  dplyr::filter(year <= 1970)

# merge 1950-1969 DDR estimates with main gdp dataset and replace old values
gdp <- gdp %>%
  dplyr::full_join(gl.deu.combined.estimates, by = c("iso3c", "country", "year")) %>%
  dplyr::mutate(gdp.gl.est = ifelse(iso3c == "DDR" & year < 1970, gdp.gl.ddr.est, gdp.gl.est)) %>%
  dplyr::select(-gdp.gl.ddr.est)

# recalculate gl DDR estimates for 1989-1990
## (1) combine gl BRD and gl DDR 1988 gdp estimates
## (2) calculate pwt combined DEU growth estimates for 1988-1990
## (3) apply pwt combined DEU growth estimates to gl combined 1988 estimate
## (4) subtract gl BRD estimates from combined gl estimates

gl.deu.combined.88 <- sum(gdp$gdp.gl.est[gdp$iso3c %in% c("BRD", "DDR") & gdp$year == 1988])

pwt.deu.growth.estimates <- gdp %>%
  dplyr::filter(iso3c == "DEU") %>%
  dplyr::select(year, gdp.pwt.est) %>%
  dplyr::mutate(
    gdp.pwt.est.lag = dplyr::lag(gdp.pwt.est),
    pwt.combined.growth = (gdp.pwt.est - gdp.pwt.est.lag) / gdp.pwt.est.lag
  )

pwt.deu.growth.estimates$gl.combined.estimate[pwt.deu.growth.estimates$year == 1988] <- gl.deu.combined.88

for(g in 1989:1990){
  
  est <- pwt.deu.growth.estimates$gl.combined.estimate[pwt.deu.growth.estimates$year == (g - 1)] *
    (1 + pwt.deu.growth.estimates$pwt.combined.growth[pwt.deu.growth.estimates$year == g])
  
  pwt.deu.growth.estimates$gl.combined.estimate[pwt.deu.growth.estimates$year == g] <- est
  
}

gl.deu.combined.estimates <- pwt.deu.growth.estimates %>%
  dplyr::select(year, gl.combined.estimate)

gl.brd <- gdp %>%
  dplyr::filter(iso3c == "BRD") %>%
  dplyr::select(year, gdp.gl.brd.est = gdp.gl.est)

gl.deu.combined.estimates <- gl.deu.combined.estimates %>%
  dplyr::left_join(gl.brd, by = "year") %>%
  dplyr::mutate(
    gdp.gl.ddr.est = gl.combined.estimate - gdp.gl.brd.est,
    iso3c = "DDR",
    country = "East Germany"
    ) %>%
  dplyr::select(iso3c, country, year, gdp.gl.ddr.est) %>%
  dplyr::filter(year < 1991)

# merge 1988-1990 DDR estimates with main gdp dataset and replace old values
gdp <- gdp %>%
  dplyr::full_join(gl.deu.combined.estimates, by = c("iso3c", "country", "year")) %>%
  dplyr::mutate(gdp.gl.est = ifelse(iso3c == "DDR" & year > 1988 & year < 1991,
                                    gdp.gl.ddr.est,
                                    gdp.gl.est)) %>%
  dplyr::select(-gdp.gl.ddr.est)

# split pwt estimate for East and West Germany
# calculate ratio between gdp.gl.est's East and West Germany GDPs
gdp_split_germany <- gdp %>%
  dplyr::select(iso3c, year, gdp.gl.est) %>%
  dplyr::filter(
    iso3c %in% c("BRD", "DDR"),
    # filter out 1990, which is coded as a unified Germany
    year < 1990
    ) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(multiplier = gdp.gl.est / sum(gdp.gl.est, na.rm = TRUE)) %>%
  dplyr::ungroup()

# pull pwt's unified German GDP data for before reunification
gdp_combined_germany <- gdp %>%
  dplyr::filter(
    iso3c == "DEU",
    # filter out 1990, which is coded as a unified Germany
    year < 1990
    ) %>%
  dplyr::select(-c(iso3c, gdp.gl.est)) %>%
  # merge split East and West Germany ratios to combined Germany datasest
  dplyr::full_join(gdp_split_germany, by = "year") %>%
  dplyr::mutate(
    gdp.pwt.est = gdp.pwt.est * multiplier,
    country = ifelse(iso3c == "BRD", "West Germany", "East Germany")
    ) %>%
  dplyr::select(-multiplier)

# calculate 1990 estimates for pwt and gl
deu.1990.pwt <- gdp$gdp.pwt.est[gdp$iso3c == "DEU" & gdp$year == 1990]
deu.1990.gl <- sum(gdp$gdp.gl.est[gdp$iso3c %in% c("BRD", "DDR") & gdp$year == 1990])

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "DEU", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "DEU", restricted = c(2018:2019))

# 1946-1949: apply mpd's (combined) DEU growth rates to both BRD and DDR estimates
# for pwt and gl
for(b in c(1949:1946)){

  brd.pwt.est <- gdp$gdp.pwt.est[gdp$iso3c == "BRD" & gdp$year == (b + 1)] /
    (1 + (gdp$mpd.rgdpna.growth[gdp$iso3c == "DEU" & gdp$year == (b + 1)] / 100))

  brd.gl.est <- gdp$gdp.gl.est[gdp$iso3c == "BRD" & gdp$year == (b + 1)] /
    (1 + (gdp$mpd.rgdpna.growth[gdp$iso3c == "DEU" & gdp$year == (b + 1)] / 100))

  ddr.pwt.est <- gdp$gdp.pwt.est[gdp$iso3c == "DDR" & gdp$year == (b + 1)] /
    (1 + (gdp$mpd.rgdpna.growth[gdp$iso3c == "DEU" & gdp$year == (b + 1)] / 100))

  ddr.gl.est <- gdp$gdp.gl.est[gdp$iso3c == "DDR" & gdp$year == (b + 1)] /
    (1 + (gdp$mpd.rgdpna.growth[gdp$iso3c == "DEU" & gdp$year == (b + 1)] / 100))

  gdp <- gdp %>%
    dplyr::mutate(
      gdp.pwt.est = dplyr::case_when(
        iso3c == "BRD" & year == b ~ brd.pwt.est,
        iso3c == "DDR" & year == b ~ ddr.pwt.est,
        .default = gdp.pwt.est
      ),
      gdp.gl.est = dplyr::case_when(
        iso3c == "BRD" & year == b ~ brd.gl.est,
        iso3c == "DDR" & year == b ~ ddr.gl.est,
        .default = gdp.gl.est
      ))

}

gdp <- gdp %>%
  # filter out entries for Germany 1950-1990
  dplyr::filter(iso3c %!in% c("BRD", "DDR", "DEU") | year > 1990) %>%
  rbind(gdp_combined_germany) %>%
  tibble::add_row(
    iso3c = "DEU",
    country = "Germany",
    year = 1990,
    gdp.pwt = deu.1990.pwt,
    gdp.gl = NA,
    gdp.pwt.est = deu.1990.pwt,
    gdp.gl.est = deu.1990.gl
  )

# calculate 1989-1990 growth rate
deu.pwt.growth.1989.1990 <- gdp$gdp.pwt.est[gdp$iso3c == "DEU" & gdp$year == 1990] /
                              sum(gdp$gdp.pwt.est[gdp$iso3c %in% c("BRD", "DDR") & gdp$year == 1989])
deu.gl.growth.1989.1990 <- gdp$gdp.gl.est[gdp$iso3c == "DEU" & gdp$year == 1990] /
                              sum(gdp$gdp.gl.est[gdp$iso3c %in% c("BRD", "DDR") & gdp$year == 1989])

#### DJI: Djibouti ---------------------------------------------------------------------------------
# 1970-1976: DJI coded as gaining independence in 1977

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "DJI", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "DJI", restricted = c(2018:2019))

#### DMA: Dominica ---------------------------------------------------------------------------------
# 1970-1977: DMA coded as gaining independence in 1978

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "DMA", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "DMA", restricted = c(2018:2019))

#### DNK: Denmark ----------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "DNK", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "DNK", restricted = c(2018:2019))

# 1946-1949: apply imf growth rates
gdp <- gdp_growth_estimator_prior_rate_func(gdp, "imf", "DNK", restricted = c(1949:1946))

#### DOM: Dominican Republic -----------------------------------------------------------------------
# 1950: apply gdp.gl proportion to 1951 gdp.gl estimate and use that ratio on gdp.pwt estimate
gdp <- gdp_growth_estimator_func(gdp, "pwt", "DOM", 1951, restricted = 1950)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "DOM", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "DOM", restricted = c(2018:2019))

# 1946-1949: apply 3-year moving average weighted growth rates
gdp <- gdp_growth_estimator_no_data_func(gdp, "DOM")

#### DZA: Algeria ----------------------------------------------------------------------------------
# 1960-1961: DZA coded as gaining independence in 1962

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "DZA", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "DZA", restricted = c(2018:2019))

#### ECU: Ecuador ----------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "ECU", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "ECU", restricted = c(2018:2019))

# 1946-1949: apply mpd growth rates
gdp <- gdp_growth_estimator_prior_rate_func(gdp, "imf", "ECU", restricted = c(1949:1946))

#### EGY: Egypt ------------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "EGY", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "EGY", restricted = c(2018:2019))

# 1946-1949: apply 3-year moving average weighted growth rates
gdp <- gdp_growth_estimator_no_data_func(gdp, "EGY")

#### ESP: Spain ------------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "ESP", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "ESP", restricted = c(2018:2019))

# 1946-1949: apply imf growth rates
gdp <- gdp_growth_estimator_prior_rate_func(gdp, "imf", "ESP", restricted = c(1949:1946))

#### EST: Estonia ----------------------------------------------------------------------------------
# 1990: EST coded as gaining independence in 1991

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "EST", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "EST", restricted = c(2018:2019))

#### ETH/ERI: Ethiopia/Eritrea (x) -----------------------------------------------------------------
# ETH
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "ETH", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "ETH", restricted = c(2018:2019))

# 1946-1949: apply 3-year moving average weighted growth rates
gdp <- gdp_growth_estimator_no_data_func(gdp, "ETH")

# ERI
# no gdp.pwt data, so use gdp.gl data as an estimate
gdp <- gdp %>%
  dplyr::mutate(gdp.pwt.est = ifelse(iso3c == "ERI",
                                     dplyr::coalesce(gdp.pwt.est, gdp.gl),
                                     gdp.pwt.est))

# 2012-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "ERI", restricted = c(2012:2019))

#### FIN: Finland ----------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "FIN", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "FIN", restricted = c(2018:2019))

# 1946-1949: apply imf growth rates
gdp <- gdp_growth_estimator_prior_rate_func(gdp, "imf", "FIN", restricted = c(1949:1946))

#### FJI: Fiji -------------------------------------------------------------------------------------
# 1960-1969: FJI coded as gaining independence in 1970

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "FJI", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "FJI", restricted = c(2018:2019))

#### FRA: France -----------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "FRA", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "FRA", restricted = c(2018:2019))

# 1946-1949: apply imf growth rates
gdp <- gdp_growth_estimator_prior_rate_func(gdp, "imf", "FRA", restricted = c(1949:1946))

#### FSM: Federal States of Micronesia (p) ---------------------------------------------------------
# no gdp.pwt data, so use gdp.gl data as an estimate
gdp <- gdp %>%
  dplyr::mutate(gdp.pwt.est = ifelse(iso3c == "FSM",
                                     dplyr::coalesce(gdp.pwt.est, gdp.gl),
                                     gdp.pwt.est))

# 2012-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "FSM", restricted = c(2012:2019))

#### GAB: Gabon ------------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "GAB", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "GAB", restricted = c(2018:2019))

#### GBR: United Kingdom ---------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "GBR", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "GBR", restricted = c(2018:2019))

# 1946-1949: apply imf growth rates
gdp <- gdp_growth_estimator_prior_rate_func(gdp, "imf", "GBR", restricted = c(1949:1946))

#### GEO: Georgia ----------------------------------------------------------------------------------
# 1990: GEO coded as gaining independence in 1991

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "GEO", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "GEO", restricted = c(2018:2019))

#### GHA: Ghana ------------------------------------------------------------------------------------
# 1955-1956: GHA coded as gaining independence in 1957

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "GHA", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "GHA", restricted = c(2018:2019))

#### GIN: Guinea -----------------------------------------------------------------------------------
# 1958: apply gdp.gl proportion to 1959 gdp.gl estimate and use that ratio on gdp.pwt estimate
gdp <- gdp_growth_estimator_func(gdp, "pwt", "GIN", 1959, restricted = 1958)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "GIN", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "GIN", restricted = c(2018:2019))

#### GMB: The Gambia -------------------------------------------------------------------------------
# 1960-1964: GMB coded as gaining independence in 1965

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "GMB", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "GMB", restricted = c(2018:2019))

#### GNB: Guinea-Bissau ----------------------------------------------------------------------------
# 1960-1973: GNB coded as gaining independence in 1974

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "GNB", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "GNB", restricted = c(2018:2019))

#### GNQ: Equatorial Guinea ------------------------------------------------------------------------
# 1960-1967: GNQ coded as gaining independence in 1968

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "GNQ", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "GNQ", restricted = c(2018:2019))

#### GRC: Greece -----------------------------------------------------------------------------------
# 1950: apply gdp.gl proportion to 1951 gdp.gl estimate and use that ratio on gdp.pwt estimate
gdp <- gdp_growth_estimator_func(gdp, "pwt", "GRC", 1951, restricted = 1950)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "GRC", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "GRC", restricted = c(2018:2019))

# 1946-1949: apply imf growth rates
gdp <- gdp_growth_estimator_prior_rate_func(gdp, "imf", "GRC", restricted = c(1949:1946))

#### GRD: Grenada ----------------------------------------------------------------------------------
# 1970-1993: GRD coded as gaining independence in 1974

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "GRD", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "GRD", restricted = c(2018:2019))

#### GTM: Guatemala --------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "GTM", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "GTM", restricted = c(2018:2019))

# 1946-1949: apply mpd growth rates
gdp <- gdp_growth_estimator_prior_rate_func(gdp, "mpd", "GTM", restricted = c(1949:1946))

#### GUY: Guyana (p) -------------------------------------------------------------------------------
# no gdp.pwt data, so use gdp.gl data as an estimate
gdp <- gdp %>%
  dplyr::mutate(gdp.pwt.est = ifelse(iso3c == "GUY",
                                     dplyr::coalesce(gdp.pwt.est, gdp.gl),
                                     gdp.pwt.est))

# 2012-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "GUY", restricted = c(2012:2019))

#### HND: Honduras ---------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "HND", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "HND", restricted = c(2018:2019))

# 1946-1949: apply imf growth rates
gdp <- gdp_growth_estimator_prior_rate_func(gdp, "imf", "HND", restricted = c(1949:1946))

#### HRV: Croatia ----------------------------------------------------------------------------------
# 1990: HRV coded as gaining independence in 1992 (both pwt and gl already have values for 1991)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "HRV", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "HRV", restricted = c(2018:2019))

#### HTI: Haiti ------------------------------------------------------------------------------------
# 1950-1959: apply gdp.gl proportion to 1960 gdp.gl estimate and use that ratio on gdp.pwt estimate
gdp <- gdp_growth_estimator_func(gdp, "pwt", "HTI", 1960, restricted = c(1950:1959))

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "HTI", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "HTI", restricted = c(2018:2019))

# 1946-1949: apply imf growth rates
gdp <- gdp_growth_estimator_prior_rate_func(gdp, "imf", "HTI", restricted = c(1949:1946))

#### HUN: Hungary ----------------------------------------------------------------------------------
# 1950-1969: apply gdp.gl proportion to 1970 gdp.gl estimate and use that ratio on gdp.pwt estimate
gdp <- gdp_growth_estimator_func(gdp, "pwt", "HUN", 1970, restricted = c(1950:1969))

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "HUN", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "HUN", restricted = c(2018:2019))

# 1946-1949: apply imf growth rates
gdp <- gdp_growth_estimator_prior_rate_func(gdp, "imf", "HUN", restricted = c(1949:1946))

#### IDN: Indonesia --------------------------------------------------------------------------------
# 1950-1959: apply gdp.gl proportion to 1960 gdp.gl estimate and use that ratio on gdp.pwt estimate
gdp <- gdp_growth_estimator_func(gdp, "pwt", "IDN", 1960, restricted = c(1950:1959))

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "IDN", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "IDN", restricted = c(2018:2019))

# 1949: apply mpd growth rates
gdp$gdp.pwt.est[gdp$iso3c == "IDN" & gdp$year == 1949] <- gdp$gdp.pwt.est[gdp$iso3c == "IDN" &
                                                                            gdp$year == 1950] /
  (1 + (gdp$mpd.rgdpna.growth[gdp$iso3c == "IDN" & gdp$year == 1950] / 100))

gdp$gdp.gl.est[gdp$iso3c == "IDN" & gdp$year == 1949] <- gdp$gdp.gl.est[gdp$iso3c == "IDN" &
                                                                          gdp$year == 1950] /
  (1 + (gdp$mpd.rgdpna.growth[gdp$iso3c == "IDN" & gdp$year == 1950] / 100))

#### IND: India ------------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "IND", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "IND", restricted = c(2018:2019))

# 1946-1949: apply imf growth rates
gdp <- gdp_growth_estimator_prior_rate_func(gdp, "imf", "IND", restricted = c(1949:1946))

#### IRL: Ireland ----------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "IRL", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "IRL", restricted = c(2018:2019))

# 1946-1949: apply imf growth rates
gdp <- gdp_growth_estimator_prior_rate_func(gdp, "imf", "IRL", restricted = c(1949:1946))

#### IRN: Iran -------------------------------------------------------------------------------------
# 1950-1954: apply gdp.gl proportion to 1955 gdp.gl estimate and use that ratio on gdp.pwt estimate
gdp <- gdp_growth_estimator_func(gdp, "pwt", "IRN", 1955, restricted = c(1950:1954))

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "IRN", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "IRN", restricted = c(2018:2019))

# 1946-1949: apply 3-year moving average weighted growth rates
gdp <- gdp_growth_estimator_no_data_func(gdp, "IRN")

#### IRQ: Iraq -------------------------------------------------------------------------------------
# 1950-1969: apply gdp.gl proportion to 1970 gdp.gl estimate and use that ratio on gdp.pwt estimate
gdp <- gdp_growth_estimator_func(gdp, "pwt", "IRQ", 1970, restricted = c(1950:1969))

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "IRQ", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "IRQ", restricted = c(2018:2019))

# 1946-1949: apply 3-year moving average weighted growth rates
gdp <- gdp_growth_estimator_no_data_func(gdp, "IRQ")

#### ISL: Iceland ----------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "ISL", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "ISL", restricted = c(2018:2019))

# 1946-1949: apply 3-year moving average weighted growth rates
gdp <- gdp_growth_estimator_no_data_func(gdp, "ISL")

#### ISR/PSE: Israel/Palestine (g) -----------------------------------------------------------------
# For purposes of this analysis, conflict between the Israeli government / Israeli groups and
# Palestinian officials / Palestinian groups operating in Israeli/Palestinian territories are
# considered domestic conflicts. As such, the GDP of both Israel and Palestinian territories should
# be combined.

# pwt and gl estimates for ISR appear to only include ISR proper
# pwt estimates for Palestinian territories begin in 1970

# PSE 1950-1969: use mpd growth estimates to calculate gdp.pwt.est
for(p in c(1969:1950)){
  
  est <- gdp$gdp.pwt.est[gdp$iso3c == "PSE" & gdp$year == (p + 1)] /
    (1 + (gdp$mpd.rgdpna.growth[gdp$iso3c == "PSE" & gdp$year == (p + 1)] / 100))
  
  gdp$gdp.pwt.est[gdp$iso3c == "PSE" & gdp$year == p] <- est
}

# ISR 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "ISR", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "ISR", restricted = c(2018:2019))
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "PSE", restricted = c(2018:2019))

# 1946-1949: apply 3-year moving average weighted growth rates
gdp <- gdp_growth_estimator_no_data_func(gdp, "ISR")
gdp <- gdp_growth_estimator_no_data_func(gdp, "PSE")

# combined ISR/PSE code
gdp.isr.pse <- gdp %>%
  dplyr::filter(iso3c %in% c("ISR", "PSE")) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(
    iso3c = "ISR",
    country = "Israel",
    gdp.pwt = NA,
    gdp.gl = NA,
    gdp.pwt.est = sum(gdp.pwt.est, na.rm = TRUE),
    gdp.gl.est = sum(gdp.gl.est, na.rm = TRUE),
    imf.growth.rate.extend = NA,
    wb.growth.rate = NA,
    mpd.cgdp.growth = NA,
    mpd.rgdpna.growth = NA
    ) %>%
  dplyr::ungroup() # %>%
  # dplyr::mutate(
  #   iso3c = "ISP",
  #   country = "Israel/Palestine"
  #   )

gdp <- gdp %>%
  rbind(gdp.isr.pse) %>%
  dplyr::filter(iso3c != "PSE")

#### ITA: Italy ------------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "ITA", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "ITA", restricted = c(2018:2019))

# 1946-1949: apply imf growth rates
gdp <- gdp_growth_estimator_prior_rate_func(gdp, "imf", "ITA", restricted = c(1949:1946))

#### JAM: Jamaica ----------------------------------------------------------------------------------
# 1953-1961: JAM coded as gaining independence in 1962

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "JAM", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "JAM", restricted = c(2018:2019))

#### JOR: Jordan (x) -------------------------------------------------------------------------------
# 1950-1953: apply gdp.gl proportion to 1954 gdp.gl estimate and use that ratio on gdp.pwt estimate
gdp <- gdp_growth_estimator_func(gdp, "pwt", "JOR", 1954, restricted = c(1950:1953))

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "JOR", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "JOR", restricted = c(2018:2019))

# 1946-1949: apply 3-year moving average weighted growth rates
gdp <- gdp_growth_estimator_no_data_func(gdp, "JOR")

#### JPN: Japan ------------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "JPN", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "JPN", restricted = c(2018:2019))

# 1946-1949: apply imf growth rates
gdp <- gdp_growth_estimator_prior_rate_func(gdp, "imf", "JPN", restricted = c(1949:1946))

#### KAZ: Kazakhstan -------------------------------------------------------------------------------
# 1990: KAZ coded as gaining independence in 1991

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "KAZ", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "KAZ", restricted = c(2018:2019))

#### KEN: Kenya ------------------------------------------------------------------------------------
# 1950-1962: KEN coded as gaining independence in 1963

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "KEN", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "KEN", restricted = c(2018:2019))

#### KGZ: Kyrgyzstan -------------------------------------------------------------------------------
# 1990: KGZ coded as gaining independence in 1991

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "KGZ", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "KGZ", restricted = c(2018:2019))

#### KHM: Cambodia ---------------------------------------------------------------------------------
# 1953-1969: apply gdp.gl proportion to 1970 gdp.gl estimate and use that ratio on gdp.pwt estimate
gdp <- gdp_growth_estimator_func(gdp, "pwt", "KHM", 1970, restricted = c(1953:1969))

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "KHM", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "KHM", restricted = c(2018:2019))

#### KIR: Kiribati (p) -----------------------------------------------------------------------------
# no gdp.pwt data, so use gdp.gl data as an estimate
gdp <- gdp %>%
  dplyr::mutate(gdp.pwt.est = ifelse(iso3c == "KIR",
                                     dplyr::coalesce(gdp.pwt.est, gdp.gl),
                                     gdp.pwt.est))

# 2012-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "KIR", restricted = c(2012:2019))

#### KNA: St. Kitts and Nevis ----------------------------------------------------------------------
# 1970-1982: KNA coded as gaining independence in 1983

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "KNA", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "KNA", restricted = c(2018:2019))

#### KOR: South Korea ------------------------------------------------------------------------------
# 1950-1952: apply gdp.gl proportion to 1953 gdp.gl estimate and use that ratio on gdp.pwt estimate
gdp <- gdp_growth_estimator_func(gdp, "pwt", "KOR", 1953, restricted = c(1950:1952))

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "KOR", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "KOR", restricted = c(2018:2019))

# 1946-1949: apply imf growth rates
gdp <- gdp_growth_estimator_prior_rate_func(gdp, "imf", "KOR", restricted = c(1949:1946))

#### KWT: Kuwait -----------------------------------------------------------------------------------
# 1961-1969: apply gdp.gl proportion to 1970 gdp.gl estimate and use that ratio on gdp.pwt estimate
gdp <- gdp_growth_estimator_func(gdp, "pwt", "KWT", 1970, restricted = c(1961:1969))

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "KWT", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "KWT", restricted = c(2018:2019))

#### LAO: Laos -------------------------------------------------------------------------------------
# 1953: LAO coded as gaining independence in 1953, though neither pwt nor gl have estimates for that
# year

# estimate 1953 by assuming 1953-1954 GDP growth is the same as 1954-1955 growth; no IMF growth
# rates available before 1980
lao.54.55.growth <- gdp$gdp.gl.est[gdp$iso3c == "LAO" & gdp$year == 1955] /
                      gdp$gdp.gl.est[gdp$iso3c == "LAO" & gdp$year == 1954]

# estimate 1953 value
gdp$gdp.gl.est[gdp$iso3c == "LAO" & gdp$year == 1953] <- gdp$gdp.gl.est[gdp$iso3c == "LAO" &
                                                                          gdp$year == 1954] /
                                                                            lao.54.55.growth

# 1953-1969: apply gdp.gl proportion to 1970 gdp.gl estimate and use that ratio on gdp.pwt estimate
gdp <- gdp_growth_estimator_func(gdp, "pwt", "LAO", 1970, restricted = c(1953:1969))

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "LAO", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "LAO", restricted = c(2018:2019))

#### LBN: Lebanon (x) ------------------------------------------------------------------------------
# 1950-1969: apply gdp.gl proportion to 1970 gdp.gl estimate and use that ratio on gdp.pwt estimate
gdp <- gdp_growth_estimator_func(gdp, "pwt", "LBN", 1970, restricted = c(1950:1969))

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "LBN", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "LBN", restricted = c(2018:2019))

# 1946-1949: apply 3-year moving average weighted growth rates
gdp <- gdp_growth_estimator_no_data_func(gdp, "LBN")

#### LBR: Liberia ----------------------------------------------------------------------------------
# 1950-1963: apply gdp.gl proportion to 1964 gdp.gl estimate and use that ratio on gdp.pwt estimate
gdp <- gdp_growth_estimator_func(gdp, "pwt", "LBR", 1964, restricted = c(1950:1963))

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "LBR", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "LBR", restricted = c(2018:2019))

# 1946-1949: apply 3-year moving average weighted growth rates
gdp <- gdp_growth_estimator_no_data_func(gdp, "LBR")

#### LBY: Libya (p) --------------------------------------------------------------------------------
# LBY gl 2008-2011 estimates are the same
# recalculate based on 2008 estimate and imf growth rates + extend through to 2019

# calculate gdp.gl.est based on imf growth rates and prior year gdp.gl.est
for(l in 2009:2019){
  
  est <- gdp$gdp.gl.est[gdp$iso3c == "LBY" & gdp$year == (l - 1)] *
    (1 + (gdp$imf.growth.rate.extend[gdp$iso3c == "LBY" & gdp$year == l] / 100))
  
  gdp$gdp.gl.est[gdp$iso3c == "LBY" & gdp$year == l] <- est
}

gdp <- gdp %>%
  dplyr::mutate(gdp.pwt.est = ifelse(iso3c == "LBY", gdp.gl.est, gdp.pwt.est))

#### LCA: St. Lucia --------------------------------------------------------------------------------
# 1970-1978: LCA coded as gaining independence in 1979

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "LCA", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "LCA", restricted = c(2018:2019))

#### LIE: Liechtenstein (p) ------------------------------------------------------------------------
# no gdp.pwt data, so use gdp.gl data as an estimate
gdp <- gdp %>%
  dplyr::mutate(gdp.pwt.est = ifelse(iso3c == "LIE",
                                     dplyr::coalesce(gdp.pwt.est, gdp.gl),
                                     gdp.pwt.est))

# 1946-1949: apply 3-year moving average weighted growth rates
gdp <- gdp_growth_estimator_no_data_func(gdp, "LIE")

# 2012-2019: apply 3-year moving average weighted growth rates
gdp <- gdp_growth_estimator_no_data_future_func(gdp, "LIE")

#### LKA: Sri Lanka --------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "LKA", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "LKA", restricted = c(2018:2019))

# 1946-1949: apply mpd growth rates (LKA coded as being independent starting in 1948)
gdp <- gdp_growth_estimator_prior_rate_func(gdp, "mpd", "LKA", restricted = c(1949:1946))

#### LSO: Lesotho ----------------------------------------------------------------------------------
# 1960-1966: LSO coded as gaining independence in 1966

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "LSO", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "LSO", restricted = c(2018:2019))

#### LTU: Lithuania --------------------------------------------------------------------------------
# 1990: LTU coded as gaining independence in 1991

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "LTU", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "LTU", restricted = c(2018:2019))

#### LUX: Luxembourg -------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "LUX", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "LUX", restricted = c(2018:2019))

# 1946-1949: apply 3-year moving average weighted growth rates
gdp <- gdp_growth_estimator_no_data_func(gdp, "LUX")

#### LVA: Latvia -----------------------------------------------------------------------------------
# 1990: LVA coded as gaining independence in 1991

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "LVA", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "LVA", restricted = c(2018:2019))

#### MAR: Morocco ----------------------------------------------------------------------------------
# 1950-1955: MAR coded as gaining independence in 1956

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "MAR", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "MAR", restricted = c(2018:2019))

#### MCO: Monaco (p/x) -----------------------------------------------------------------------------
# 2012-2018: apply wb growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "wb", "MCO", restricted = c(2012:2019))

# no gdp.pwt data, so use gdp.gl data as an estimate
gdp <- gdp %>%
  dplyr::mutate(gdp.pwt.est = ifelse(iso3c == "MCO",
                                     dplyr::coalesce(gdp.pwt.est, gdp.gl),
                                     gdp.pwt.est))

# 2019: apply 3-year moving average weighted growth rates
# calculate growth rates
mco.pwt.growth.19 <- (1/2) * gdp$gdp.pwt.est[gdp$iso3c == "MCO" & gdp$year == 2018] /
                        gdp$gdp.pwt.est[gdp$iso3c == "MCO" & gdp$year == 2017] +
                      (1/3) * gdp$gdp.pwt.est[gdp$iso3c == "MCO" & gdp$year == 2017] /
                        gdp$gdp.pwt.est[gdp$iso3c == "MCO" & gdp$year == 2016] +
                      (1/6) * gdp$gdp.pwt.est[gdp$iso3c == "MCO" & gdp$year == 2016] /
                        gdp$gdp.pwt.est[gdp$iso3c == "MCO" & gdp$year == 2015]
mco.gl.growth.19 <- (1/2) * gdp$gdp.gl.est[gdp$iso3c == "MCO" & gdp$year == 2018] /
                        gdp$gdp.gl.est[gdp$iso3c == "MCO" & gdp$year == 2017] +
                    (1/3) * gdp$gdp.gl.est[gdp$iso3c == "MCO" & gdp$year == 2017] /
                        gdp$gdp.gl.est[gdp$iso3c == "MCO" & gdp$year == 2016] +
                    (1/6) * gdp$gdp.gl.est[gdp$iso3c == "MCO" & gdp$year == 2016] /
                        gdp$gdp.gl.est[gdp$iso3c == "MCO" & gdp$year == 2015]

# add 2019 estimates
gdp$gdp.pwt.est[gdp$iso3c == "MCO" & gdp$year == 2019] <- gdp$gdp.pwt.est[gdp$iso3c == "MCO" &
                                                                            gdp$year == 2018] *
                                                                              mco.pwt.growth.19
gdp$gdp.gl.est[gdp$iso3c == "MCO" & gdp$year == 2019] <- gdp$gdp.gl.est[gdp$iso3c == "MCO" &
                                                                          gdp$year == 2018] *
                                                                            mco.gl.growth.19

# 1946-1949: apply 3-year moving average weighted growth rates
gdp <- gdp_growth_estimator_no_data_func(gdp, "MCO")

#### MDA: Moldova ----------------------------------------------------------------------------------
# 1990: MDA coded as gaining independence in 1991

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "MDA", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "MDA", restricted = c(2018:2019))

#### MDG: Madagascar -------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "MDG", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "MDG", restricted = c(2018:2019))

#### MDV: Maldives ---------------------------------------------------------------------------------
# 1965-1969: apply gdp.gl proportion to 1970 gdp.gl estimate and use that ratio on gdp.pwt estimate
gdp <- gdp_growth_estimator_func(gdp, "pwt", "MDV", 1970, restricted = c(1965:1969))

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "MDV", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "MDV", restricted = c(2018:2019))

#### MEX: Mexico -----------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "MEX", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "MEX", restricted = c(2018:2019))

# 1946-1949: apply imf growth rates
gdp <- gdp_growth_estimator_prior_rate_func(gdp, "imf", "MEX", restricted = c(1949:1946))

#### MHL: Marshall Islands (p) ---------------------------------------------------------------------
# no gdp.pwt data, so use gdp.gl data as an estimate
# gl data starts in 1986 - Compact of Free Association with US
gdp <- gdp %>%
  dplyr::mutate(gdp.pwt.est = ifelse(iso3c == "MHL",
                                     dplyr::coalesce(gdp.pwt.est, gdp.gl),
                                     gdp.pwt.est))

# 2012-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "MHL", restricted = c(2012:2019))

#### MKD: North Macedonia --------------------------------------------------------------------------
# 1990-1992: MKD coded as gaining independence in 1993 (pwt has values for 1990-1992, gl has values
# for 1991-1992)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "MKD", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "MKD", restricted = c(2018:2019))

#### MLI: Mali -------------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "MLI", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "MLI", restricted = c(2018:2019))

#### MLT: Malta ------------------------------------------------------------------------------------
# 1954-1963: MDA coded as gaining independence in 1964

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "MLT", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "MLT", restricted = c(2018:2019))

#### MMR: Myanmar ----------------------------------------------------------------------------------
# 1950-1961: apply gdp.gl proportion to 1962 gdp.gl estimate and use that ratio on gdp.pwt estimate
gdp <- gdp_growth_estimator_func(gdp, "pwt", "MMR", 1962, restricted = c(1950:1961))

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "MMR", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "MMR", restricted = c(2018:2019))

# 1946-1949: apply 3-year moving average weighted growth rates (MMR coded as gaining independence in
# 1948)
gdp <- gdp_growth_estimator_no_data_func(gdp, "MMR")

#### MNG: Mongolia ---------------------------------------------------------------------------------
# 1950-1969: apply gdp.gl proportion to 1970 gdp.gl estimate and use that ratio on gdp.pwt estimate
gdp <- gdp_growth_estimator_func(gdp, "pwt", "MNG", 1970, restricted = c(1950:1969))

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "MNG", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "MNG", restricted = c(2018:2019))

# 1946-1949: apply 3-year moving average weighted growth rates
gdp <- gdp_growth_estimator_no_data_func(gdp, "MNG")

#### MOZ: Mozambique -------------------------------------------------------------------------------
# 1960-1974: MOZ coded as gaining independence in 1975

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "MOZ", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "MOZ", restricted = c(2018:2019))

#### MRT: Mauritania -------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "MRT", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "MRT", restricted = c(2018:2019))

#### MUS: Mauritus ---------------------------------------------------------------------------------
# 1950-1967: MUS coded as gaining independence in 1968

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "MUS", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "MUS", restricted = c(2018:2019))

#### MWI: Malawi -----------------------------------------------------------------------------------
# 1954-1963: MWI coded as gaining independence in 1964

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "MWI", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "MWI", restricted = c(2018:2019))

#### MYS/SGP: Malaysia/Singapore -------------------------------------------------------------------
# MYS coded as gaining independence in 1957
# SGP coded as gaining independence in 1965
# SGP part of MYS 1963-1964; neither MYS estimates do not include SGP

# 1963-1964: apply gdp.pwt proportion to 1965 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp.sgp <- gdp_growth_estimator_func(gdp, "gl", "SGP", 1965, restricted = c(1963:1964))

# pull estimates and add to original dataset (so that the function does not code 2012-2017 values
# based on 1965 data)
gdp$gdp.gl.est[gdp$iso3c == "SGP" & gdp$year == 1963] <- gdp.sgp$gdp.gl.est[gdp.sgp$iso3c == "SGP" &
                                                                              gdp.sgp$year == 1963]
gdp$gdp.gl.est[gdp$iso3c == "SGP" & gdp$year == 1964] <- gdp.sgp$gdp.gl.est[gdp.sgp$iso3c == "SGP" &
                                                                              gdp.sgp$year == 1964]

# recodes the MYS gdp.pwt.est and gdp.gl.est for 1963-1964 as the sum of MYS and SGP estimates
gdp$gdp.pwt.est[gdp$iso3c == "MYS" & gdp$year == 1963] <- sum(gdp$gdp.pwt.est[gdp$iso3c %in% c("MYS", "SGP") &
                                                                                gdp$year == 1963])
gdp$gdp.pwt.est[gdp$iso3c == "MYS"&gdp$year==1964] <- sum(gdp$gdp.pwt.est[gdp$iso3c %in% c("MYS", "SGP") &
                                                                            gdp$year == 1964])

gdp$gdp.gl.est[gdp$iso3c == "MYS" & gdp$year == 1963] <- sum(gdp$gdp.gl.est[gdp$iso3c %in% c("MYS", "SGP") &
                                                                              gdp$year == 1963])
gdp$gdp.gl.est[gdp$iso3c == "MYS" & gdp$year == 1964] <- sum(gdp$gdp.gl.est[gdp$iso3c %in% c("MYS", "SGP") &
                                                                              gdp$year == 1964])

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "MYS", 2011, restricted = c(2012:2017))
gdp <- gdp_growth_estimator_func(gdp, "gl", "SGP", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "MYS", restricted = c(2018:2019))
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "SGP", restricted = c(2018:2019))

#### NAM/ZAF: Namibia/South Africa -----------------------------------------------------------------
# Note: both pwt and gl code NAM as separate throughout its pre-independence period
# gl data starts in 1990, pwt data starts in 1960
# NAM coded as gaining independence in 1990

# merge pre-independence NAM with ZAF

# capture pwt + gl ZAF 1989-1990 growth rates
nam.pwt.growth.1989.1990 <- gdp$gdp.pwt.est[gdp$iso3c == "NAM" & gdp$year == 1990] /
                              gdp$gdp.pwt.est[gdp$iso3c == "NAM" & gdp$year == 1989]
zaf.pwt.growth.1989.1990 <- gdp$gdp.pwt.est[gdp$iso3c == "ZAF" & gdp$year == 1990] /
                              gdp$gdp.pwt.est[gdp$iso3c == "ZAF" & gdp$year == 1989]
zaf.gl.growth.1989.1990 <- gdp$gdp.gl.est[gdp$iso3c == "ZAF" & gdp$year == 1990] /
                              gdp$gdp.gl.est[gdp$iso3c == "ZAF" & gdp$year == 1989]

# 1960-1989: apply gdp.pwt proportion to 1990 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp.nam.backdating <- gdp_growth_estimator_func(gdp, "gl", "NAM", 1990,
                                                restricted = c(1960:1989)) %>%
  dplyr::filter(iso3c == "NAM")

# capture gl NAM 1989-1990 growth rates
nam.gl.growth.1989.1990 <- gdp$gdp.gl.est[gdp$iso3c == "NAM" & gdp$year == 1990] /
                             gdp$gdp.gl.est[gdp$iso3c == "NAM" & gdp$year == 1989]

# mpd growth estimates
for(n in 1959:1950){
  
  gdp.nam.backdating$gdp.pwt.est[gdp.nam.backdating$year == n] <- gdp.nam.backdating$gdp.pwt.est[gdp.nam.backdating$year == (n + 1)] /
                                                                  (1 + (gdp.nam.backdating$mpd.rgdpna.growth[gdp.nam.backdating$year == (n + 1)] / 100))
  gdp.nam.backdating$gdp.gl.est[gdp.nam.backdating$year==n] <- gdp.nam.backdating$gdp.gl.est[gdp.nam.backdating$year == (n + 1)] /
                                                                  (1 + (gdp.nam.backdating$mpd.rgdpna.growth[gdp.nam.backdating$year == (n + 1)] / 100))

}

gdp.nam.backdating <- gdp.nam.backdating %>%
  dplyr::select(iso3c, country, year, gdp.pwt.est2 = gdp.pwt.est, gdp.gl.est2 = gdp.gl.est) %>%
  # this is only for estimating pre-independence gdp
  dplyr::filter(year < 1990)

# merge pre-independence estimates with main gdp dataset
gdp <- gdp %>%
  dplyr::left_join(gdp.nam.backdating, by = c("iso3c", "country", "year")) %>%
  dplyr::mutate(
    gdp.pwt.est = dplyr::coalesce(gdp.pwt.est, gdp.pwt.est2),
    gdp.gl.est = dplyr::coalesce(gdp.gl.est, gdp.gl.est2)
    ) %>%
  dplyr::select(-c(gdp.pwt.est2, gdp.gl.est2))

# gdp.nam.plus1 <- gdp.nam.backdating %>%
#   dplyr::mutate(year = year + 1) %>%
#   dplyr::rename(gdp.pwt.plus1 = gdp.pwt,
#                 gdp.gl.plus1 = gdp.gl,
#                 gdp.pwt.est.plus1 = gdp.pwt.est,
#                 gdp.gl.est.plus1 = gdp.gl.est)
# 
# gdp.nam.growth <- dplyr::full_join(gdp.nam.backdating,gdp.nam.plus1) %>%
#   dplyr::mutate(gdp.pwt.growth = gdp.pwt / gdp.pwt.plus1,
#                 gdp.gl.growth = gdp.gl / gdp.gl.plus1,
#                 gdp.pwt.est.growth = gdp.pwt.est / gdp.pwt.est.plus1,
#                 gdp.gl.est.growth = gdp.gl.est / gdp.gl.est.plus1,
#                 pwt.growth = 100*(gdp.pwt.est.growth-1),
#                 gl.growth = 100*(gdp.gl.est.growth-1))

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "NAM", 2011, restricted = c(2012:2017))
gdp <- gdp_growth_estimator_func(gdp, "gl", "ZAF", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "NAM", restricted = c(2018:2019))
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "ZAF", restricted = c(2018:2019))

# 1946-1949: apply 3-year moving average weighted growth rates
gdp <- gdp_growth_estimator_no_data_func(gdp, "NAM")
gdp <- gdp_growth_estimator_no_data_func(gdp, "ZAF")

# combine pre-independence NAM (-1989) gdp with ZAF
for(z in 1946:1989){
  
  gdp$gdp.pwt.est[gdp$iso3c == "ZAF" & gdp$year == z] <- sum(gdp$gdp.pwt.est[gdp$iso3c %in% c("ZAF", "NAM") &
                                                                               gdp$year == z])
  
  gdp$gdp.gl.est[gdp$iso3c == "ZAF" & gdp$year == z] <- sum(gdp$gdp.gl.est[gdp$iso3c %in% c("ZAF", "NAM") &
                                                                             gdp$year == z])

}

#### NER: Niger ------------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "NER", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "NER", restricted = c(2018:2019))

#### NGA: Nigeria ----------------------------------------------------------------------------------
# 1950-1959: NGA coded as gaining independence in 1960

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "NGA", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "NGA", restricted = c(2018:2019))

#### NIC: Nicaragua --------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "NIC", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "NIC", restricted = c(2018:2019))

# 1946-1949: apply imf growth rates
gdp <- gdp_growth_estimator_prior_rate_func(gdp, "imf", "NIC", restricted = c(1949:1946))

#### NLD: Netherlands ------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "NLD", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "NLD", restricted = c(2018:2019))

# 1946-1949: apply imf growth rates
gdp <- gdp_growth_estimator_prior_rate_func(gdp, "imf", "NLD", restricted = c(1949:1946))

#### NOR: Norway -----------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "NOR", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "NOR", restricted = c(2018:2019))

# 1946-1949: apply imf growth rates
gdp <- gdp_growth_estimator_prior_rate_func(gdp, "imf", "NOR", restricted = c(1949:1946))

#### NPL: Nepal ------------------------------------------------------------------------------------
# 1950-1959: apply gdp.gl proportion to 1960 gdp.gl estimate and use that ratio on gdp.pwt estimate
gdp <- gdp_growth_estimator_func(gdp, "pwt", "NPL", 1960, restricted = c(1950:1959))

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "NPL", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "NPL", restricted = c(2018:2019))

# 1946-1949: apply 3-year moving average weighted growth rates
gdp <- gdp_growth_estimator_no_data_func(gdp, "NPL")

#### NRU: Nauru (p) --------------------------------------------------------------------------------
# no gdp.pwt data, so use gdp.gl data as an estimate
# gl data starts in 1968 - end of UN trusteeship
gdp <- gdp %>%
  dplyr::mutate(gdp.pwt.est = ifelse(iso3c == "NRU",
                                     dplyr::coalesce(gdp.pwt.est, gdp.gl),
                                     gdp.pwt.est))

# 2012-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "NRU", restricted = c(2012:2019))

#### NZL: New Zealand ------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "NZL", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "NZL", restricted = c(2018:2019))

# 1946-1949: apply imf growth rates
gdp <- gdp_growth_estimator_prior_rate_func(gdp, "imf", "NZL", restricted = c(1949:1946))

#### OMN: Oman -------------------------------------------------------------------------------------
# OMN coded as gaining independence in 1971, but due to Dhofar War, extending GDP data back to 1950,
# as gl contains data from this point forward

# 1950-1969: apply gdp.gl proportion to 1970 gdp.gl estimate and use that ratio on gdp.pwt estimate
gdp <- gdp_growth_estimator_func(gdp, "pwt", "OMN", 1970, restricted = c(1950:1969))

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "OMN", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "OMN", restricted = c(2018:2019))

# 1946-1949: apply 3-year moving average weighted growth rates
gdp <- gdp_growth_estimator_no_data_func(gdp, "OMN")

#### PAN: Panama -----------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "PAN", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "PAN", restricted = c(2018:2019))

# 1946-1949: apply imf growth rates
gdp <- gdp_growth_estimator_prior_rate_func(gdp, "imf", "PAN", restricted = c(1949:1946))

#### PER: Peru -------------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "PER", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "PER", restricted = c(2018:2019))

# 1946-1949: apply imf growth rates
gdp <- gdp_growth_estimator_prior_rate_func(gdp, "imf", "PER", restricted = c(1949:1946))

#### PHL: Philippines (4) --------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "PHL", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "PHL", restricted = c(2018:2019))

# 1947-1949: apply imf growth rates
gdp <- gdp_growth_estimator_prior_rate_func(gdp, "imf", "PHL", restricted = c(1949:1947))

#### PLW: Palau (p) --------------------------------------------------------------------------------
# no gdp.pwt data, so use gdp.gl data as an estimate
gdp <- gdp %>%
  dplyr::mutate(gdp.pwt.est = ifelse(iso3c == "PLW",
                                     dplyr::coalesce(gdp.pwt.est, gdp.gl),
                                     gdp.pwt.est))

# 2012-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "PLW", restricted = c(2012:2019))

#### PNG: Papua New Guinea -------------------------------------------------------------------------
# gl's PNG estimates for 1992-2011 are the same
# replace 1993-2011 estimates using IMF's growth rates + extend through 2019

for(p in 1993:2019){
  
  est <- gdp$gdp.gl.est[gdp$iso3c == "PNG" & gdp$year == (p - 1)] *
    (1 + (gdp$imf.growth.rate.extend[gdp$iso3c == "PNG" & gdp$year == p] / 100))
  
  gdp$gdp.gl.est[gdp$iso3c == "PNG" & gdp$year == p] <- est

}

# no gdp.pwt data, so use gdp.gl data as an estimate
gdp <- gdp %>%
  dplyr::mutate(gdp.pwt.est = ifelse(iso3c=="PNG", gdp.gl.est, gdp.pwt.est))

#### POL: Poland -----------------------------------------------------------------------------------
# 1950-1969: apply gdp.gl proportion to 1970 gdp.gl estimate and use that ratio on gdp.pwt estimate
gdp <- gdp_growth_estimator_func(gdp, "pwt", "POL", 1970, restricted = c(1950:1969))

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "POL", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "POL", restricted = c(2018:2019))

# 1946-1949: apply 3-year moving average weighted growth rates
gdp <- gdp_growth_estimator_no_data_func(gdp, "POL")

#### PRK: North Korea (p) --------------------------------------------------------------------------
# 2012-2018: use Bank of Korea's estimated PRK growth rates for 2012-2018 applied to gl's 2011 estimate
# https://www.bok.or.kr/eng/bbs/E0000634/view.do?nttId=10053001&menuNo=400069
gdp$gdp.gl.est[gdp$iso3c == "PRK" & gdp$year == 2012] <- gdp$gdp.gl.est[gdp$iso3c == "PRK" & gdp$year == 2011] * (1 + 0.013)
gdp$gdp.gl.est[gdp$iso3c == "PRK" & gdp$year == 2013] <- gdp$gdp.gl.est[gdp$iso3c == "PRK" & gdp$year == 2012] * (1 + 0.011)
gdp$gdp.gl.est[gdp$iso3c == "PRK" & gdp$year == 2014] <- gdp$gdp.gl.est[gdp$iso3c == "PRK" & gdp$year == 2013] * (1 + 0.010)
gdp$gdp.gl.est[gdp$iso3c == "PRK" & gdp$year == 2015] <- gdp$gdp.gl.est[gdp$iso3c == "PRK" & gdp$year == 2014] * (1 - 0.011)
gdp$gdp.gl.est[gdp$iso3c == "PRK" & gdp$year == 2016] <- gdp$gdp.gl.est[gdp$iso3c == "PRK" & gdp$year == 2015] * (1 + 0.039)
gdp$gdp.gl.est[gdp$iso3c == "PRK" & gdp$year == 2017] <- gdp$gdp.gl.est[gdp$iso3c == "PRK" & gdp$year == 2016] * (1 - 0.035)
gdp$gdp.gl.est[gdp$iso3c == "PRK" & gdp$year == 2018] <- gdp$gdp.gl.est[gdp$iso3c == "PRK" & gdp$year == 2017] * (1 - 0.041)

# 2019: EIU country profile estimates 2.3% growth in 2019
gdp$gdp.gl.est[gdp$iso3c == "PRK" & gdp$year == 2019] <- 1.023 * gdp$gdp.gl.est[gdp$iso3c == "PRK" &
                                                                                  gdp$year == 2018]

# no gdp.pwt data, so use gdp.gl data as an estimate
gdp <- gdp %>%
  dplyr::mutate(gdp.pwt.est = ifelse(iso3c == "PRK",
                                     dplyr::coalesce(gdp.pwt.est, gdp.gl),
                                     gdp.pwt.est))

# 1946-1949: apply 3-year moving average weighted growth rates
gdp <- gdp_growth_estimator_no_data_func(gdp, "PRK")

#### PRT: Portugal ---------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "PRT", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "PRT", restricted = c(2018:2019))

# 1946-1949: apply imf growth rates
gdp <- gdp_growth_estimator_prior_rate_func(gdp, "imf", "PRT", restricted = c(1949:1946))

#### PRY: Paraguay ---------------------------------------------------------------------------------
# 1950: apply gdp.gl proportion to 1951 gdp.gl estimate and use that ratio on gdp.pwt estimate
gdp <- gdp_growth_estimator_func(gdp, "pwt", "PRY", 1951, restricted = 1950)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "PRY", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "PRY", restricted = c(2018:2019))

# 1946-1949: apply imf growth rates
gdp <- gdp_growth_estimator_prior_rate_func(gdp, "imf", "PRY", restricted = c(1949:1946))

#### QAT: Qatar ------------------------------------------------------------------------------------
# 1970: QAT coded as gaining independence in 1971

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "QAT", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "QAT", restricted = c(2018:2019))

#### ROU: Romania ----------------------------------------------------------------------------------
# gl values constant 1950-1960
# recalculate gl 1950-1959 estimates based on IMF growth rates
for(r in c(1959:1950)){
  
  est <- gdp$gdp.gl.est[gdp$iso3c == "ROU" & gdp$year == (r + 1)] /
    (1 + (gdp$imf.growth.rate.extend[gdp$iso3c == "ROU" & gdp$year == (r + 1)] / 100))
  
  gdp <- gdp %>%
    dplyr::mutate(gdp.pwt.est = ifelse(iso3c == "ROU" & year == r, est, gdp.pwt.est))

}

# 1950-1959: apply gdp.gl proportion to 1960 gdp.gl estimate and use that ratio on gdp.pwt estimate
# not using function so the gdp.gl.est estimates are used instead of the gdp.gl estimates

# this function is used to estimate pwt gdp data based on the relative difference in the size of the
# economy between two years within the gl gdp data and applying the proportion to the pwt gdp data

# the gdp.gl baseline to estimate the proportions from
baseline.rou <- gdp$gdp.gl[gdp$iso3c == "ROU" & gdp$year == 1960]
  
# the gdp.pwt relative gdp to apply the proportions to
relative.rou <- gdp$gdp.pwt[gdp$iso3c == "ROU" & gdp$year == 1960]

gdp <- gdp %>%
  dplyr::mutate(gdp.pwt.est = ifelse(iso3c == "ROU" & is.na(gdp.pwt.est),
                                     relative.rou * gdp.gl.est / baseline.rou,
                                     gdp.pwt.est))

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "ROU", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "ROU", restricted = c(2018:2019))

# 1946-1949: estimated consistent growth rates between mpd's 1948 and 1950 gdp estimates (rgdpna);
# use mpd's growth estimates for other years

rou.growth.estimate <- (gdp$rgdpna[gdp$iso3c == "ROU" & gdp$year==1950] /
                          gdp$rgdpna[gdp$iso3c == "ROU" & gdp$year == 1948])^(1/2)

# average mpd growth estimates
for (r in 1949:1948){
  
  gdp <- gdp %>%
    dplyr::mutate(
      gdp.pwt.est = ifelse(iso3c == "ROU" & year == r,
                           gdp$gdp.pwt.est[gdp$iso3c == "ROU" & gdp$year == (r + 1)] /
                             rou.growth.estimate,
                           gdp.pwt.est),
      gdp.gl.est = ifelse(iso3c == "ROU" & year == r,
                          gdp$gdp.gl.est[gdp$iso3c == "ROU" & gdp$year == (r + 1)] /
                            rou.growth.estimate,
                          gdp.gl.est)
      )
  
}

# mpd growth estimates
for(r in 1947:1946){
  
  gdp$gdp.pwt.est[gdp$iso3c == "ROU" & gdp$year == r] <- gdp$gdp.pwt.est[gdp$iso3c == "ROU" &
                                                                           gdp$year == (r + 1)] /
    (1 + (gdp$mpd.rgdpna.growth[gdp$iso3c == "ROU" & gdp$year == (r + 1)] / 100))
  gdp$gdp.gl.est[gdp$iso3c == "ROU" & gdp$year == r] <- gdp$gdp.gl.est[gdp$iso3c == "ROU" &
                                                                         gdp$year == (r + 1)] /
    (1 + (gdp$mpd.rgdpna.growth[gdp$iso3c == "ROU" & gdp$year == (r + 1)] / 100))

}

#### RUS/SOV: Russia/Soviet Union ------------------------------------------------------------------
# pwt and gl estimates coded as "RUS" for 1991 and before are SOV, 1992 and after are just RUS

# 1950-1989: apply gdp.gl proportion to 1990 gdp.gl estimate and use that ratio on gdp.pwt estimate
gdp <- gdp_growth_estimator_func(gdp, "pwt", "RUS", 1990, restricted = c(1950:1989))

# recode RUS as SOV pre-1992
gdp$iso3c[gdp$iso3c == "RUS" & gdp$year < 1992] <- "SOV"
gdp$country[gdp$country == "Russia" & gdp$year < 1992] <- "Soviet Union"

# 1946-1949: apply mpd growth rates
gdp <- gdp_growth_estimator_prior_rate_func(gdp, "mpd", "SOV", restricted = c(1949:1946))

gdp$country[gdp$iso3c == "SOV"] <- "Soviet Union"

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "RUS", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "RUS", restricted = c(2018:2019))

#### RWA: Rwanda -----------------------------------------------------------------------------------
# 1960-1961: QAT coded as gaining independence in 1962

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "RWA", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "RWA", restricted = c(2018:2019))

#### SAU: Saudi Arabia -----------------------------------------------------------------------------
# 1950-1969: apply gdp.gl proportion to 1970 gdp.gl estimate and use that ratio on gdp.pwt estimate
gdp <- gdp_growth_estimator_func(gdp, "pwt", "SAU", 1970, restricted = c(1950:1969))

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "SAU", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "SAU", restricted = c(2018:2019))

# 1946-1949: apply 3-year moving average weighted growth rates
gdp <- gdp_growth_estimator_no_data_func(gdp, "SAU")

#### SDN: Sudan ------------------------------------------------------------------------------------
# 1956-1969: apply gdp.gl proportion to 1970 gdp.gl estimate and use that ratio on gdp.pwt estimate
gdp <- gdp_growth_estimator_func(gdp, "pwt", "SDN", 1970, restricted = c(1956:1969))

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "SDN", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "SDN", restricted = c(2018:2019))

#### SEN: Senegal ----------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "SEN", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "SEN", restricted = c(2018:2019))

#### SLB: Solomon Islands (p) ----------------------------------------------------------------------
# no gdp.pwt data, so use gdp.gl data as an estimate
gdp <- gdp %>%
  dplyr::mutate(gdp.pwt.est = ifelse(iso3c == "SLB",
                                     dplyr::coalesce(gdp.pwt.est, gdp.gl),
                                     gdp.pwt.est))

# 2012-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "SLB", restricted = c(2012:2019))

#### SLE: Sierra Leone -----------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "SLE", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "SLE", restricted = c(2018:2019))

#### SLV: El Salvador ------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "SLV", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "SLV", restricted = c(2018:2019))

# 1946-1949: apply mpd growth rates
gdp <- gdp_growth_estimator_prior_rate_func(gdp, "mpd", "SLV", restricted = c(1949:1946))

#### SMR: San Marino (p) ---------------------------------------------------------------------------
# no gdp.pwt data, so use gdp.gl data as an estimate
gdp <- gdp %>%
  dplyr::mutate(gdp.pwt.est = ifelse(iso3c == "SMR",
                                     dplyr::coalesce(gdp.pwt.est, gdp.gl),
                                     gdp.pwt.est))

# 2012-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "SMR", restricted = c(2012:2019))

# 1946-1949: apply 3-year moving average weighted growth rates
gdp <- gdp_growth_estimator_no_data_func(gdp, "SMR")

#### SOM: Somalia (p) ------------------------------------------------------------------------------
# no gdp.pwt data, so use gdp.gl data as an estimate
gdp <- gdp %>%
  dplyr::mutate(gdp.pwt.est = ifelse(iso3c == "SOM",
                                     dplyr::coalesce(gdp.pwt.est, gdp.gl),
                                     gdp.pwt.est))

# 2012-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "SOM", restricted = c(2012:2019))

#### SSD: South Sudan (p/x) ------------------------------------------------------------------------
# no gdp.pwt data and only have gl's 2011 estimate

# 2012-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "SSD", restricted = c(2012:2019))

# # 2012-2019: apply IMF growth rates to gl's 2011 estimate
# 
# # pull IMF's growth rates
# ssd.growth.est <- imf.growth %>%
#   # only modern dataset has estimates
#   dplyr::select(c(iso3c,year,imf.growth.rate.modern)) %>%
#   dplyr::filter(iso3c == "SSD",
#                 # SSD coded as gaining independence in 2011, so growth rates start
#                 # in 2012
#                 year >= 2011)
# 
# # add gl estimate to the table
# ssd.growth.est$gdp.gl.est[ssd.growth.est$year==2011] <- gdp$gdp.gl[gdp$iso3c=="SSD"&gdp$year==2011]
# 
# ssd.growth.est <- ssd.growth.est %>%
#   dplyr::arrange(year)
# 
# for(s in 2012:2019){
#   
#   ssd.growth.est$gdp.gl.est[ssd.growth.est$year==s] <- ssd.growth.est$gdp.gl.est[ssd.growth.est$year==(s-1)]*
#                                                        (100+ssd.growth.est$imf.growth.rate.modern[ssd.growth.est$year==s])/100
#   
# }

#### STP: São Tomé and Príncipe --------------------------------------------------------------------
# 1970-1974: STP coded as gaining independence in 1975

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "STP", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "STP", restricted = c(2018:2019))

#### SUR: Suriname ---------------------------------------------------------------------------------
# 1970-1974: SUR coded as gaining independence in 1975

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "SUR", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "SUR", restricted = c(2018:2019))

#### SVN: Slovenia ---------------------------------------------------------------------------------
# 1990-1991: SVN coded as gaining independence in 1992

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "SVN", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "SVN", restricted = c(2018:2019))

#### SWE: Sweden -----------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "SWE", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "SWE", restricted = c(2018:2019))

# 1946-1949: apply imf growth rates
gdp <- gdp_growth_estimator_prior_rate_func(gdp, "imf", "SWE", restricted = c(1949:1946))

#### SWZ: eSwatini ---------------------------------------------------------------------------------
# 1968-1969: apply gdp.gl proportion to 1970 gdp.gl estimate and use that ratio on gdp.pwt estimate
gdp <- gdp_growth_estimator_func(gdp, "pwt", "SWZ", 1970, restricted = c(1968:1969))

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "SWZ", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "SWZ", restricted = c(2018:2019))

#### SYC: Seychelles -------------------------------------------------------------------------------
# 1960-1975: SYC coded as gaining independence in 1976

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "SYC", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "SYC", restricted = c(2018:2019))

#### SYR: Syria (p) --------------------------------------------------------------------------------
# 1950-1959: apply gdp.gl proportion to 1960 gdp.gl estimate and use that ratio on gdp.pwt estimate
gdp <- gdp_growth_estimator_func(gdp, "pwt", "SYR", 1960, restricted = c(1950:1959))

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "SYR", 2011, restricted = c(2012:2017))

# 2018: The Heritage Foundation estimated 5.0% growth in 2018
gdp$gdp.pwt.est[gdp$iso3c == "SYR" & gdp$year == 2018] <- gdp$gdp.pwt.est[gdp$iso3c == "SYR" &
                                                                            gdp$year == 2017] * 1.05
gdp$gdp.gl.est[gdp$iso3c == "SYR" & gdp$year == 2018] <- gdp$gdp.gl.est[gdp$iso3c == "SYR" &
                                                                          gdp$year == 2017] * 1.05

# The EIU estimated 2.3% growth in 2019
gdp$gdp.pwt.est[gdp$iso3c == "SYR" & gdp$year == 2019] <- gdp$gdp.pwt.est[gdp$iso3c == "SYR" &
                                                                            gdp$year == 2018] * 1.023
gdp$gdp.gl.est[gdp$iso3c == "SYR" & gdp$year == 2019] <- gdp$gdp.gl.est[gdp$iso3c == "SYR" &
                                                                          gdp$year == 2018] * 1.023

# 1946-1949: apply 3-year moving average weighted growth rates
gdp <- gdp_growth_estimator_no_data_func(gdp, "SYR")

#### TCD: Chad -------------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "TCD", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "TCD", restricted = c(2018:2019))

#### TGO: Togo -------------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "TGO", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "TGO", restricted = c(2018:2019))

#### THA: Thailand ---------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "THA", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "THA", restricted = c(2018:2019))

# 1946-1949: apply 3-year moving average weighted growth rates
gdp <- gdp_growth_estimator_no_data_func(gdp, "THA")

#### TJK: Tajikistan -------------------------------------------------------------------------------
# 1990: TJK coded as gaining independence in 1991

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "TJK", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "TJK", restricted = c(2018:2019))

#### TKM: Turkmenistan -----------------------------------------------------------------------------
# 1990: TKM coded as gaining independence in 1991

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "TKM", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "TKM", restricted = c(2018:2019))

#### TLS: Timor Leste (p) --------------------------------------------------------------------------
# no gdp.pwt data, so use gdp.gl data as an estimate
gdp <- gdp %>%
  dplyr::mutate(gdp.pwt.est = ifelse(iso3c == "TLS",
                                     dplyr::coalesce(gdp.pwt.est, gdp.gl),
                                     gdp.pwt.est))

# 2012-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "TLS", restricted = c(2012:2019))

#### TON: Tonga (p) --------------------------------------------------------------------------------
# no gdp.pwt data, so use gdp.gl data as an estimate
# gl data starts in 1970 - end of protection status
gdp <- gdp %>%
  dplyr::mutate(gdp.pwt.est = ifelse(iso3c == "TON",
                                     dplyr::coalesce(gdp.pwt.est, gdp.gl),
                                     gdp.pwt.est))

# 2012-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "TON", restricted = c(2012:2019))

#### TTO: Trinidad and Tobago ----------------------------------------------------------------------
# 1950-1961: TTO coded as gaining independence in 1962

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "TTO", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "TTO", restricted = c(2018:2019))

#### TUN: Tunisia ----------------------------------------------------------------------------------
# 1950-1959: apply gdp.gl proportion to 1960 gdp.gl estimate and use that ratio on gdp.pwt estimate
gdp <- gdp_growth_estimator_func(gdp, "pwt", "TUN", 1960, restricted = c(1950:1959))

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "TUN", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "TUN", restricted = c(2018:2019))

#### TUR: Türkiye ----------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "TUR", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "TUR", restricted = c(2018:2019))

# 1946-1949: apply imf growth rates
gdp <- gdp_growth_estimator_prior_rate_func(gdp, "imf", "TUR", restricted = c(1949:1946))

#### TUV: Tuvalu (p) -------------------------------------------------------------------------------
# no gdp.pwt data, so use gdp.gl data as an estimate
# gl data starts in 1978 - independence from UK
gdp <- gdp %>%
  dplyr::mutate(gdp.pwt.est = ifelse(iso3c == "TUV",
                                     dplyr::coalesce(gdp.pwt.est, gdp.gl),
                                     gdp.pwt.est))

# 2012-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "TUV", restricted = c(2012:2019))

#### TWN: Taiwan -----------------------------------------------------------------------------------
# 1950: apply gdp.gl proportion to 1951 gdp.gl estimate and use that ratio on gdp.pwt estimate
gdp <- gdp_growth_estimator_func(gdp, "pwt", "TWN", 1951, restricted = 1950)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "TWN", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "TWN", restricted = c(2018:2019))

# 1946-1949: apply 3-year moving average weighted growth rates
gdp <- gdp_growth_estimator_no_data_func(gdp, "TWN")

#### TZA/ZAN: Tanzania and Zanzibar ----------------------------------------------------------------
# Tanganyika gained independence 1961; Zanzibar in 1963; unification in 1964
# code TZA as Tanganyika 1961-1963 and Tanzania 1964-; code Zanzibar as 1963

# both pwt and gl estimates do not include ZAN for -1963 but include 1964-
# pwt does not have estimates for independent ZAN, but gl has estimates for 1963-1964

# for pwt ZAN estimates for 1963-1964, use gl TZA to ZAN ratio
# for 1963, gl estimates are for Zanzibar and Tanganyika
zan.tza.ratio.1963 <- gdp$gdp.gl.est[gdp$iso3c == "ZAN" & gdp$year == 1963] /
                        gdp$gdp.gl.est[gdp$iso3c == "TZA" & gdp$year == 1963]

# for 1964, gl estimates are for Zanzibar and unified Tanzania
zan.tza.ratio.1964 <- gdp$gdp.gl.est[gdp$iso3c == "ZAN" & gdp$year == 1964] /
                        gdp$gdp.gl.est[gdp$iso3c == "TZA"&gdp$year == 1964]

# apply ratios to pwt TZA estimates for 1963 and 1964
gdp$gdp.pwt.est[gdp$iso3c == "ZAN" & gdp$year == 1963] <- zan.tza.ratio.1963 *
                                                            gdp$gdp.pwt.est[gdp$iso3c == "TZA" &
                                                                              gdp$year == 1963]
gdp$gdp.pwt.est[gdp$iso3c == "ZAN" & gdp$year == 1964] <- zan.tza.ratio.1964 *
                                                            gdp$gdp.pwt.est[gdp$iso3c == "TZA" &
                                                                              gdp$year == 1964]

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "TZA", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "TZA", restricted = c(2018:2019))

# calculate 1963-1964 growth rate
tza.pwt.growth.1963.1964 <- gdp$gdp.pwt.est[gdp$iso3c == "TZA" & gdp$year == 1964] /
                              sum(gdp$gdp.pwt.est[gdp$iso3c %in% c("TZA", "ZAN") &
                                                    gdp$year == 1963])
tza.gl.growth.1963.1964 <- gdp$gdp.gl.est[gdp$iso3c == "TZA" & gdp$year == 1964] /
                              sum(gdp$gdp.gl.est[gdp$iso3c %in% c("TZA", "ZAN") &
                                                   gdp$year == 1963])

#### UGA: Uganda -----------------------------------------------------------------------------------
# 1950-1961: UGA coded as gaining independence in 1962

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "UGA", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "UGA", restricted = c(2018:2019))

#### UKR: Ukraine ----------------------------------------------------------------------------------
# 1990: UKR coded as gaining independence in 1991

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "UKR", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "UKR", restricted = c(2018:2019))

#### URY: Uruguay ----------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "URY", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "URY", restricted = c(2018:2019))

# 1946-1949: apply imf growth rates
gdp <- gdp_growth_estimator_prior_rate_func(gdp, "imf", "URY", restricted = c(1949:1946))

#### USA: United States ----------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "USA", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "USA", restricted = c(2018:2019))

# 1946-1949: apply imf growth rates
gdp <- gdp_growth_estimator_prior_rate_func(gdp, "imf", "USA", restricted = c(1949:1946))

#### UZB: Uzbekistan -------------------------------------------------------------------------------
# 1990: UZB coded as gaining independence in 1991

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "UZB", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "UZB", restricted = c(2018:2019))

#### VCT: St. Vincent and the Grenedines -----------------------------------------------------------
# 1970-1978: VCT coded as gaining independence in 1979

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "VCT", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "VCT", restricted = c(2018:2019))

#### VEN: Venezuela --------------------------------------------------------------------------------
# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "VEN", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "VEN", restricted = c(2018:2019))

# 1946-1949: apply imf growth rates
gdp <- gdp_growth_estimator_prior_rate_func(gdp, "imf", "VEN", restricted = c(1949:1946))

#### VNM/RVN: Vietnam and South Vietnam ------------------------------------------------------------
# VNM pwt estimates 1970-1975 contain both North and South Vietnam - use ratio between economies
# from gl estimates in those years to break up pwt estimates
gdp.vnm.multiplier <- gdp %>%
  dplyr::filter(iso3c %in% c("VNM", "RVN")) %>%
  dplyr::select(iso3c, year, gdp.gl) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(multiplier = gdp.gl / sum(gdp.gl, na.rm = TRUE)) %>%
  dplyr::ungroup()

# pull pwt estimates for 1970-1975
gdp.vnm.70.75 <- gdp %>%
  dplyr::filter(
    iso3c == "VNM",
    year %in% c(1970:1975)
    ) %>%
  dplyr::select(year, gdp.pwt)

# merge pwt 1970-1975 estimates with ratios
gdp.vnm.multiplier <- gdp.vnm.multiplier %>%
  dplyr::left_join(gdp.vnm.70.75, by = "year") %>%
  dplyr::mutate(gdp.pwt.est2 = gdp.pwt * multiplier) %>%
  dplyr::select(iso3c, year, gdp.pwt.est2)
  
# merge gdp.pwt.est 1970-1975 with main gdp dataset
gdp <- gdp %>%
  dplyr::left_join(gdp.vnm.multiplier) %>%
  dplyr::mutate(gdp.pwt.est = ifelse(year %in% c(1970:1975) & iso3c %in% c("VNM", "RVN"),
                                     gdp.pwt.est2,
                                     gdp.pwt.est)) %>%
  dplyr::select(-gdp.pwt.est2)

# RVN 1954-1969: apply gdp.gl proportion to 1970 gdp.gl estimate and use that ratio on gdp.pwt estimate
gdp <- gdp_growth_estimator_func(gdp, "pwt", "RVN", 1970, restricted = c(1954:1969))

# calculate VNM gdp.pwt.est using separate dataset with gdp.pwt (North and South Vietnam combined) as NAs
gdp.vnm <- gdp %>%
  dplyr::filter(iso3c == "VNM") %>%
  dplyr::mutate(gdp.pwt = NA)

gdp.vnm <- gdp_growth_estimator_func(gdp.vnm, "pwt", "VNM", 1970, restricted = c(1954:1969)) %>%
  dplyr::select(iso3c, year, gdp.pwt.est2 = gdp.pwt.est)

gdp <- gdp %>%
  dplyr::full_join(gdp.vnm, by = c("iso3c", "year")) %>%
  dplyr::mutate(gdp.pwt.est = ifelse(year %in% c(1954:1969) & iso3c=="VNM",
                                     gdp.pwt.est2,
                                     gdp.pwt.est)) %>%
  dplyr::select(-gdp.pwt.est2)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "VNM", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "VNM", restricted = c(2018:2019))

# calculate 1975-1976 growth rate
vnm.pwt.growth.1975.1976 <- gdp$gdp.pwt.est[gdp$iso3c == "VNM" & gdp$year == 1976] /
                              sum(gdp$gdp.pwt.est[gdp$iso3c %in% c("VNM", "RVN") &
                                                    gdp$year == 1975])
vnm.gl.growth.1975.1976 <- gdp$gdp.gl.est[gdp$iso3c == "VNM" & gdp$year == 1976] /
                              sum(gdp$gdp.gl.est[gdp$iso3c %in% c("VNM", "RVN") &
                                                   gdp$year == 1975])

#### VUT: Vanuatu (p) ------------------------------------------------------------------------------
# no gdp.pwt data, so use gdp.gl data as an estimate
# gl data starts in 1980
gdp <- gdp %>%
  dplyr::mutate(gdp.pwt.est = ifelse(iso3c == "VUT",
                                     dplyr::coalesce(gdp.pwt.est, gdp.gl),
                                     gdp.pwt.est))

# 2012-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "VUT", restricted = c(2012:2019))

#### WSM: Samoa (p) --------------------------------------------------------------------------------
# no gdp.pwt data, so use gdp.gl data as an estimate
# gl data starts in 1962- Western Samoa Act of 1961 enters effect
gdp <- gdp %>%
  dplyr::mutate(gdp.pwt.est = ifelse(iso3c == "WSM",
                                     dplyr::coalesce(gdp.pwt.est, gdp.gl),
                                     gdp.pwt.est))

# 2012-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "WSM", restricted = c(2012:2019))

#### YEM/YAR/YPR: Yemen, North Yemen, and South Yemen ----------------------------------------------
# pwt data contains combined Yemen beginning in 1989, while gl separates North and South Yemen from
# 1950 / 1967 (respectively) through to 1990 (inclusive)
# Yemen coded as beginning starting in 1991

# calculate the proportions of Yemen's 1989 and 1990 economies
gdp.north.yemen.prop.89 <- gdp$gdp.gl[gdp$iso3c == "YAR" & gdp$year == 1989] /
                            sum(gdp$gdp.gl[gdp$iso3c %in% c("YAR", "YPR") & gdp$year == 1989])
gdp.south.yemen.prop.89 <- gdp$gdp.gl[gdp$iso3c == "YPR" & gdp$year == 1989] /
                            sum(gdp$gdp.gl[gdp$iso3c %in% c("YAR", "YPR") & gdp$year == 1989])

gdp.north.yemen.prop.90 <- gdp$gdp.gl[gdp$iso3c == "YAR" & gdp$year == 1990] /
                            sum(gdp$gdp.gl[gdp$iso3c %in% c("YAR", "YPR") & gdp$year == 1990])
gdp.south.yemen.prop.90 <- gdp$gdp.gl[gdp$iso3c == "YPR" & gdp$year == 1990] /
                            sum(gdp$gdp.gl[gdp$iso3c %in% c("YAR", "YPR") & gdp$year == 1990])

# calculate gdp.gl proportions for YAR (1950-1988) and YPR (1967-1988) based on 1989 gl GDPs
yar.multiplier <- gdp %>%
  dplyr::filter(iso3c == "YAR",
                year < 1989) %>%
  dplyr::mutate(yar.multiplier = gdp.gl / gdp$gdp.gl[gdp$iso3c == "YAR" & gdp$year == 1989]) %>%
  dplyr::select(year, yar.multiplier)

ypr.multiplier <- gdp %>%
  dplyr::filter(iso3c == "YPR",
                year < 1989) %>%
  dplyr::mutate(ypr.multiplier = gdp.gl / gdp$gdp.gl[gdp$iso3c == "YPR" & gdp$year == 1989]) %>%
  dplyr::select(year, ypr.multiplier)

gdp_split_yemen <- data.frame(iso3c = c(rep("YAR", 41), rep("YPR", 24)),
                              year = c(1950:1990, 1967:1990),
                              # calculate 1989 and 1990 pwt estimates based on gl ratios between
                              # North and South Yemen
                              gdp.pwt.est2 = c(
                                rep(NA, 39),
                                gdp.north.yemen.prop.89 * gdp$gdp.pwt[gdp$iso3c == "YEM" &
                                                                        gdp$year == 1989],
                                gdp.north.yemen.prop.90 * gdp$gdp.pwt[gdp$iso3c == "YEM" &
                                                                        gdp$year == 1990],
                                rep(NA, 22),
                                gdp.south.yemen.prop.89 * gdp$gdp.pwt[gdp$iso3c == "YEM" &
                                                                        gdp$year == 1989],
                                gdp.south.yemen.prop.90 * gdp$gdp.pwt[gdp$iso3c == "YEM" &
                                                                        gdp$year == 1990])
                              ) %>%
  dplyr::full_join(yar.multiplier, by = "year") %>%
  dplyr::full_join(ypr.multiplier, by = "year")

gdp_split_yemen <- gdp_split_yemen %>%
  dplyr::mutate(gdp.pwt.est2 = ifelse(iso3c == "YAR" & is.na(gdp.pwt.est2),
                                     gdp_split_yemen$gdp.pwt.est2[gdp_split_yemen$iso3c == "YAR" &
                                                                    gdp_split_yemen$year == 1989] *
                                                                      yar.multiplier,
                                     gdp.pwt.est2),
                gdp.pwt.est2 = ifelse(iso3c == "YPR" & is.na(gdp.pwt.est2),
                                     gdp_split_yemen$gdp.pwt.est2[gdp_split_yemen$iso3c == "YPR" &
                                                                    gdp_split_yemen$year == 1989] *
                                                                      ypr.multiplier,
                                     gdp.pwt.est2)) %>%
  dplyr::select(-c(yar.multiplier, ypr.multiplier))

# merge pwt split estimates into the main dataset
gdp <- gdp %>%
  dplyr::full_join(gdp_split_yemen, by = c("iso3c", "year")) %>%
  dplyr::mutate(gdp.pwt.est = dplyr::coalesce(gdp.pwt.est, gdp.pwt.est2)) %>%
  # filter out YEM entries for 1950-1990
  dplyr::filter(iso3c != "YEM" | year > 1990) %>%
  dplyr::select(-gdp.pwt.est2)

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "YEM", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "YEM", restricted = c(2018:2019))

# 1946-1949: apply 3-year moving average weighted growth rates
gdp <- gdp_growth_estimator_no_data_func(gdp, "YAR")

# calculate 1990-1991 growth rate
yem.pwt.growth.1990.1991 <- gdp$gdp.pwt.est[gdp$iso3c == "YEM" & gdp$year == 1991] /
                             sum(gdp$gdp.pwt.est[gdp$iso3c %in% c("YAR", "YPR") & gdp$year == 1990])
yem.gl.growth.1990.1991 <- gdp$gdp.gl.est[gdp$iso3c == "YEM" & gdp$year == 1991] /
                             sum(gdp$gdp.pwt.est[gdp$iso3c %in% c("YAR", "YPR") & gdp$year == 1990])

# original estimates - appear to be different methodology for calculating than what is done above
# # YEM
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1950] <- 36403208086*0.766175876
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1951] <- 36403208086*0.782980248
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1952] <- 36403208086*0.802124909
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1953] <- 36403208086*0.823335607
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1954] <- 36403208086*0.845934022
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1955] <- 36403208086*0.86899501
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1956] <- 36403208086*0.89063537
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1957] <- 36403208086*0.913630538
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1958] <- 36403208086*0.936079029
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1959] <- 36403208086*0.959410613
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1960] <- 36403208086*0.985146478
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1961] <- 36403208086*1.012566255
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1962] <- 36403208086*1.042024643
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1963] <- 36403208086*1.075295142
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1964] <- 36403208086*1.103530363
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1965] <- 36403208086*1.132005097
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1966] <- 36403208086*1.163361313
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1967] <- 36403208086*2.159944126*0.553142898
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1968] <- 36403208086*2.239144624*0.545840546
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1969] <- 36403208086*2.234323263*0.508364272
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1970] <- 36403208086*2.578976526*0.531671379
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1971] <- 36403208086*2.069952701*0.717879913
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1972] <- 36403208086*2.306435246*0.740464405
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1973] <- 36403208086*2.423371077*0.740840973
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1974] <- 36403208086*2.791522303*0.765063879
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1975] <- 36403208086*3.13662534*0.783567826
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1976] <- 36403208086*3.339016457*0.785461425
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1977] <- 36403208086*3.440034666*0.785064651
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1978] <- 36403208086*3.539224523*0.777036978
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1979] <- 36403208086*3.756606288*0.781373105
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1980] <- 36403208086*4.08193754*0.78433014
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1981] <- 36403208086*4.425800955*0.789147717
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1982] <- 36403208086*4.609332634*0.786674748
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1983] <- 36403208086*4.728460657*0.778670468
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1984] <- 36403208086*4.873117941*0.772559119
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1985] <- 36403208086*5.699428275*0.792284933
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1986] <- 36403208086*5.872827234*0.79340554
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1987] <- 36403208086*6.301204335*0.795408931
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1988] <- 36403208086*7.246468998*0.809809327
# gdp$rgdpna[gdp$iso3c=="YEM"&gdp$year==1989] <- 36403208086*0.409221266

# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1967] <- 36403208086*2.159944126*0.446857102
# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1968] <- 36403208086*2.239144624*0.454159454
# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1969] <- 36403208086*2.234323263*0.491635728
# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1970] <- 36403208086*2.578976526*0.468328621
# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1971] <- 36403208086*2.069952701*0.282120087
# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1972] <- 36403208086*2.306435246*0.259535595
# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1973] <- 36403208086*2.423371077*0.259159027
# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1974] <- 36403208086*2.791522303*0.234936121
# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1975] <- 36403208086*3.13662534*0.216432174
# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1976] <- 36403208086*3.339016457*0.214538575
# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1977] <- 36403208086*3.440034666*0.214935349
# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1978] <- 36403208086*3.539224523*0.222963022
# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1979] <- 36403208086*3.756606288*0.218626895
# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1980] <- 36403208086*4.08193754*0.21566986
# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1981] <- 36403208086*4.425800955*0.210852283
# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1982] <- 36403208086*4.609332634*0.213325252
# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1983] <- 36403208086*4.728460657*0.221329532
# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1984] <- 36403208086*4.873117941*0.227440881
# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1985] <- 36403208086*5.699428275*0.207715067
# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1986] <- 36403208086*5.872827234*0.20659446
# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1987] <- 36403208086*6.301204335*0.204591069
# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1988] <- 36403208086*7.246468998*0.190190673
# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1989] <- 36403208086*0.590778734
# gdp$rgdpna[gdp$iso3c=="YPR"&gdp$year==1990] <- 36403208086*0.590778734 # rough estimate - double check

#### YUG: Yugoslavia, Serbia, Montenegro, Kosovo (p/x) ---------------------------------------------
# Data time frames (note all gl data extends only through 2011, inclusive):
## BIH - pwt 1990-; gl 1992-; coded as starting 1992
## HRV - pwt 1990-; gl 1991-; coded as starting 1992
## MKD - pwt 1990-; gl 1991-; coded as starting 1992
## SLN - pwt 1990-; gl 1992-; coded as starting 1992
## KSV - pwt no data; gl 2008-; coded as starting 2008
## MNE - pwt 1990-; gl 2006-; coded as starting in 2006
## SRB - pwt 1990-; gl 2006-; coded as starting in 1992
## YUG - pwt no data; gl 1950-2006; coded as ending 1991

## YUG 1992-2005 gl data is for Serbia and Montenegro
## SRB 2008- pwt data is for both Serbia and Kosovo
gdp.srb.ksv <- gdp %>%
  dplyr::filter(
    iso3c %in% c("SRB", "KSV"),
    year >= 2008
    ) %>%
  # calculate gl ratio between SRB and KSV GDP for 2008-2011
  dplyr::group_by(year) %>%
  dplyr::mutate(multiplier = gdp.gl / sum(gdp.gl, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::select(iso3c, year, multiplier) %>%
  dplyr::group_by(iso3c) %>%
  dplyr::arrange(year) %>%
  # extends 2011 ratios through 2017
  dplyr::mutate(multiplier = imputeTS::na_interpolation(multiplier, option = "linear")) %>%
  dplyr::ungroup()

# SRB pwt data 2008-2017
srb.pwt.08.17 <- gdp %>%
  dplyr::filter(
    iso3c == "SRB",
    year %in% c(2008:2017)
    ) %>%
  dplyr::select(year, gdp.pwt)

# merge SRB pwt estimates with ratio dataset
gdp.srb.ksv <- gdp.srb.ksv %>%
  dplyr::full_join(srb.pwt.08.17, by = "year") %>%
  dplyr::mutate(gdp.pwt.est2 = gdp.pwt * multiplier) %>%
  dplyr::select(-c(multiplier, gdp.pwt))

# merge gdp.pwt.est2 estimates with main gdp dataset
gdp <- gdp %>%
  dplyr::full_join(gdp.srb.ksv, by = c("iso3c", "year")) %>%
  dplyr::mutate(gdp.pwt.est = ifelse(iso3c %in% c("SRB", "KSV") &
                                       year %in% c(2008:2017),
                                     gdp.pwt.est2,
                                     gdp.pwt.est)) %>%
  dplyr::select(-gdp.pwt.est2)

## SRB and MNE coded separately gdp.pwt 1991-2005 and together (as YUG) for gl ???-2006
## note gl has YUG, SRB, and MNE values for 2006, though SRB alone is larger than YUG
gdp.srb.mne <- gdp %>%
  dplyr::filter(
    iso3c %in% c("YUG", "SRB", "MNE"),
    year %in% c(1990:2005)
  ) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(
    gdp.pwt.est = sum(gdp.pwt, na.rm = TRUE),
    gdp.gl.est = sum(gdp.gl, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    iso3c = "SRB",
    country = "Serbia and Montenegro",
    gdp.pwt = NA,
    gdp.gl = gdp.gl.est,
    imf.growth.rate.extend = NA,
    wb.growth.rate = NA,
    mpd.cgdp.growth = NA,
    mpd.rgdpna.growth = NA
  )

gdp <- gdp %>%
  # filter out YUG/SRB/MNE for 1990-2005
  dplyr::filter(
    iso3c %!in% c("YUG", "SRB", "MNE") | year %!in% c(1990:2005),
    # filter out YUG 2006
    iso3c != "YUG" | year != 2006
  ) %>%
  rbind(gdp.srb.mne)

# YUG 1990-1991: reestimate gdp.pwt.est and gdp.gl.est as sums of republics for each year
yug.90.91.est <- gdp %>%
  dplyr::filter(iso3c %in% c("SVN", "HRV", "BIH", "SRB", "MKD")) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(
    gdp.pwt.est = sum(gdp.pwt.est, na.rm = TRUE),
    gdp.gl.est = sum(gdp.gl.est, na.rm = TRUE)
  )

# add rows for YUG 1990 and 1991
gdp <- gdp %>%
  tibble::add_row(
    iso3c = "YUG",
    country = "Yugoslavia",
    year = 1990,
    gdp.pwt = NA,
    gdp.gl = NA,
    gdp.pwt.est = yug.90.91.est$gdp.pwt.est[yug.90.91.est$year == 1990],
    gdp.gl.est = yug.90.91.est$gdp.gl.est[yug.90.91.est$year == 1990]
  ) %>%
  tibble::add_row(
    iso3c = "YUG",
    country = "Yugoslavia",
    year = 1991,
    gdp.pwt = NA,
    gdp.gl = NA,
    gdp.pwt.est = yug.90.91.est$gdp.pwt.est[yug.90.91.est$year == 1991],
    gdp.gl.est = yug.90.91.est$gdp.gl.est[yug.90.91.est$year == 1991]
  ) 

# recode gdp.pwt.est and gdp.gl.est 1990 and 1991 as NAs for the republics
# gdp <- gdp %>%
#   dplyr::mutate(gdp.pwt.est = ifelse(iso3c %in% c("SVN","HRV","BIH","SRB","MKD")&year %in% c(1990:1991),NA,gdp.pwt.est),
#                 gdp.gl.est = ifelse(iso3c %in% c("SVN","HRV","BIH","SRB","MKD")&year %in% c(1990:1991),NA,gdp.gl.est))

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio
# on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "MNE", 2011, restricted = c(2012:2017))
gdp <- gdp_growth_estimator_func(gdp, "gl", "SRB", 2011, restricted = c(2012:2017))

# KSV
# pwt codes SRB and KSV together, even after 2008; gl codes KSV as separate beginning in 2008,
# with SRB not including Kosovo starting that year

# no gdp.pwt data, so use gdp.gl data as an estimate
gdp <- gdp %>%
  dplyr::mutate(gdp.pwt.est = ifelse(iso3c == "KSV",
                                     dplyr::coalesce(gdp.pwt.est, gdp.gl),
                                     gdp.pwt.est))

# 2012-2019: apply imf growth rates
gdp <- gdp %>%
  # filter out KSV 2012-2017 blank entries
  dplyr::filter(iso3c != "KSV" | year < 2012)
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "KSV", restricted = c(2012:2019))
gdp$country[gdp$iso3c == "KSV"] <- "Kosovo"

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "MNE", restricted = c(2018:2019))
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "SRB", restricted = c(2018:2019))

# 1947-1949: apply mpd growth rates
gdp <- gdp_growth_estimator_prior_rate_func(gdp, "mpd", "YUG", restricted = c(1949:1947))
gdp$country[gdp$iso3c == "YUG"] <- "Yugoslavia"

# 1946: apply 3-year moving average weighted growth rates

# calculate growth rates - gl
yug.gl.growth.48 <- gdp$gdp.gl.est[gdp$iso3c == "YUG" & gdp$year == 1948] /
  gdp$gdp.gl.est[gdp$iso3c == "YUG" & gdp$year == 1947]
yug.gl.growth.49 <- gdp$gdp.gl.est[gdp$iso3c == "YUG" & gdp$year == 1949] /
  gdp$gdp.gl.est[gdp$iso3c == "YUG" & gdp$year == 1948]
yug.gl.growth.50 <- gdp$gdp.gl.est[gdp$iso3c == "YUG" & gdp$year == 1950] /
  gdp$gdp.gl.est[gdp$iso3c == "YUG" & gdp$year == 1949]

yug.gl.growth.47 <- (1/2) * yug.gl.growth.48 + (1/3) * yug.gl.growth.49 + (1/6) * yug.gl.growth.50

# add 1946
gdp$gdp.gl.est[gdp$iso3c == "YUG" & gdp$year == 1946] <- gdp$gdp.gl.est[gdp$iso3c == "YUG" &
                                                                          gdp$year == 1947] /
  yug.gl.growth.47

# YUG 1946-1989 gdp.pwt.est aply gdp.gl.est proportion to 1990 gdp.gl.est estimate and use that
# ratio on gdp.pwt.estimate

# the gdp.gl.est baseline to estimate the proportions from
yug.baseline <- gdp$gdp.gl.est[gdp$iso3c == "YUG" & gdp$year == 1990]

# the gdp.pwt.est relative gdp to apply the proportions to
yug.relative <- gdp$gdp.pwt.est[gdp$iso3c == "YUG" & gdp$year == 1990]

gdp <- gdp %>%
  dplyr::mutate(gdp.pwt.est = ifelse(iso3c == "YUG" & year %in% c(1946:1989),
                                     yug.relative * gdp.gl.est / yug.baseline,
                                     gdp.pwt.est))

#### ZMB: Zambia -----------------------------------------------------------------------------------
# 1955-1963: ZMB coded as gaining independence in 1964

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "ZMB", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "ZMB", restricted = c(2018:2019))

#### ZWE: Zimbabwe ---------------------------------------------------------------------------------
# 1955-1964: ZWE coded as gaining independence in 1965

# 2012-2017: apply gdp.pwt proportion to 2011 gdp.pwt estimate and use that ratio on gdp.gl estimate
gdp <- gdp_growth_estimator_func(gdp, "gl", "ZWE", 2011, restricted = c(2012:2017))

# 2018-2019: apply imf growth rates
gdp <- gdp_growth_estimator_rate_func(gdp, "imf", "ZWE", restricted = c(2018:2019))


### formatting ------------------------------------------------------------------------------------
gdp <- gdp %>%
  dplyr::rename(
    gdp.pwt.original = gdp.pwt,
    gdp.gl.original = gdp.gl
    )


### calculate growth rates -------------------------------------------------------------------------
gdp_year_prior <- gdp %>%
  dplyr::mutate(year = year + 1) %>%
  dplyr::rename(gdp.pwt.original.plus1 = gdp.pwt.original,
                gdp.gl.original.plus1 = gdp.gl.original,
                gdp.pwt.est.plus1 = gdp.pwt.est,
                gdp.gl.est.plus1 = gdp.gl.est)

gdp <- gdp %>%
  dplyr::left_join(gdp_year_prior,by=c("iso3c","country","year")) %>%
  dplyr::mutate(gdp.growth.rate.pwt.original = 100 * (gdp.pwt.original - gdp.pwt.original.plus1) /
                  gdp.pwt.original.plus1,
                gdp.growth.rate.gl.original = 100 * (gdp.gl.original - gdp.gl.original.plus1) /
                  gdp.gl.original.plus1,
                gdp.growth.rate.pwt.est = 100 * (gdp.pwt.est - gdp.pwt.est.plus1) /
                  gdp.pwt.est.plus1,
                gdp.growth.rate.gl.est = 100 * (gdp.gl.est - gdp.gl.est.plus1) /
                  gdp.gl.est.plus1) %>%
  dplyr::select(-c(gdp.pwt.original.plus1, gdp.gl.original.plus1, gdp.pwt.est.plus1,
                   gdp.gl.est.plus1))


### adjust growth estimates for countries uniting/dissolving ---------------------------------------
# Czechia/Slovakia
gdp$gdp.growth.rate.pwt.est[gdp$iso3c == "CZE" & gdp$year == 1993] <- 100 *
                                                                      (cze.pwt.growth.1992.1993 - 1)
gdp$gdp.growth.rate.pwt.est[gdp$iso3c == "SVK" & gdp$year == 1993] <- 100 *
                                                                      (svk.pwt.growth.1992.1993 - 1)
gdp$gdp.growth.rate.gl.est[gdp$iso3c == "CZE" & gdp$year == 1993] <- 100 *
                                                                       (cze.gl.growth.1992.1993 - 1)
gdp$gdp.growth.rate.gl.est[gdp$iso3c == "SVK" & gdp$year == 1993] <- 100 *
                                                                       (cze.gl.growth.1992.1993 - 1)

# Germany
gdp$gdp.growth.rate.pwt.est[gdp$iso3c == "DEU" & gdp$year == 1990] <- 100 *
                                                                      (deu.pwt.growth.1989.1990 - 1)
gdp$gdp.growth.rate.gl.est[gdp$iso3c == "DEU" & gdp$year == 1990] <- 100 *
                                                                       (deu.gl.growth.1989.1990 - 1)

# Ethiopia/Eritrea

# Malaysia/Singapore

# Pakistan/Bangladesh
gdp$gdp.growth.rate.pwt.est[gdp$iso3c == "PAK" & gdp$year == 1971] <- 100 *
                                                                      (pak.pwt.growth.1970.1971 - 1)
gdp$gdp.growth.rate.pwt.est[gdp$iso3c == "BGD" & gdp$year == 1971] <- 100 *
                                                                      (bgd.pwt.growth.1970.1971 - 1)
gdp$gdp.growth.rate.gl.est[gdp$iso3c == "PAK" & gdp$year == 1971] <- 100 *
                                                                       (pak.gl.growth.1970.1971 - 1)
gdp$gdp.growth.rate.gl.est[gdp$iso3c == "BGD" & gdp$year == 1971] <- 100 *
                                                                       (bgd.gl.growth.1970.1971 - 1)

# Serbia/Kosovo

# Serbia/Montenegro

# South Africa/Namibia
gdp$gdp.growth.rate.pwt.est[gdp$iso3c == "NAM" & gdp$year == 1990] <- 100 *
                                                                      (nam.pwt.growth.1989.1990 - 1)
gdp$gdp.growth.rate.pwt.est[gdp$iso3c == "ZAF" & gdp$year == 1990] <- 100 *
                                                                      (zaf.pwt.growth.1989.1990 - 1)
gdp$gdp.growth.rate.gl.est[gdp$iso3c == "NAM" & gdp$year == 1990] <- 100 *
                                                                       (nam.gl.growth.1989.1990 - 1)
gdp$gdp.growth.rate.gl.est[gdp$iso3c == "ZAF" & gdp$year == 1990] <- 100 *
                                                                       (zaf.gl.growth.1989.1990 - 1)

# Sudan/South Sudan

# Soviet Union + Successors

# Tanzania/Zanzibar
gdp$gdp.growth.rate.pwt.est[gdp$iso3c == "TZA" & gdp$year == 1964] <- 100 *
                                                                      (tza.pwt.growth.1963.1964 - 1)
gdp$gdp.growth.rate.gl.est[gdp$iso3c == "TZA" & gdp$year == 1964] <- 100 *
                                                                       (tza.gl.growth.1963.1964 - 1)

# Vietnam
gdp$gdp.growth.rate.pwt.est[gdp$iso3c == "VNM" & gdp$year == 1976] <- 100 *
                                                                      (vnm.pwt.growth.1975.1976 - 1)
gdp$gdp.growth.rate.gl.est[gdp$iso3c == "VNM" & gdp$year == 1976] <- 100 *
                                                                       (vnm.gl.growth.1975.1976 - 1)

# Yemen
gdp$gdp.growth.rate.pwt.est[gdp$iso3c == "YEM" & gdp$year == 1991] <- 100 *
                                                                      (yem.pwt.growth.1990.1991 - 1)
gdp$gdp.growth.rate.gl.est[gdp$iso3c == "YEM" & gdp$year == 1991] <- 100 *
                                                                       (yem.gl.growth.1990.1991 - 1)

# Yugoslavia + Successors


### write data -------------------------------------------------------------------------------------
# writes formatted dataframe as csv files
write.csv(gdp, "Data files/Formatted data files/gdp.csv", row.names = FALSE)
