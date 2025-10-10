
# creates variable on oil exports for 1980-2016


### load libraries ---------------------------------------------------------------------------------
library(countrycode)
library(dplyr)
library(tidyr)


### load data files --------------------------------------------------------------------------------
oil <- read.csv("Data files/Raw data files/INT-Export-08-08-2020_06-00-39.csv", skip = 1)


### data formatting --------------------------------------------------------------------------------
oil <- oil[-1,-1]

oil <- oil %>%
  tidyr::pivot_longer(2:38, names_to = "year", values_to = "oil") %>%
  dplyr::mutate(year = str_sub(year, start=2, end=5)) %>%
  dplyr::rename(country = X) %>%
  # using the countrycode package, add iso3c code based on country name
  dplyr::mutate(
    country = trimws(country, which = "left"),
    iso3c = dplyr::case_when(
      country == "Former Czechoslovakia" ~ "CZE",
      country == "Former Serbia and Montenegro" ~ "SRB",
      country == "Former Yugoslavia" ~ "YUG",
      country == "Germany, East" ~ "DDR",
      country == "Germany, West" ~ "BRD",
      country == "Kosovo" ~ "KSV",
      country == "Micronesia" ~ "FSM",
      country == "Former U.S.S.R." ~ "SOV",
      country == "World" ~ "WLD",
      # collapse territories/dependencies into main country
      country %in% c("Aruba", "Netherlands Antilles") ~ "ANT",
      country %in% c("Bermuda", "British Virgin Islands", "Cayman Islands", "Gibraltar",
                     "Monserrat", "Saint Helena", "Turks and Caicos Islands") ~ "GBR",
      country %in% c("Cook Islands", "Niue") ~ "NZL",
      country %in% c("French Guinea", "French Polynesia", "Guadeloupe", "Martinique",
                     "New Caledonia", "Reunion", "Saint Pierre and Miquelon") ~ "FRA",
      country == "Greenland" ~ "DNK",
      country %in% c("Guam", "Hawaiian Trade Zone", "Northern Mariana Islands", "Puerto Rico",
                     "U.S. Pacific Islands", "U.S. Territories", "U.S. Virgin Islands")  ~ "USA",
      country %in% c("Hong Kong", "Macau") ~ "CHN",
      country == "Palestinian Territories" ~ "ISR",
      country == "Wake Island" ~ "WAK",
      country == "Western Sahara" ~ "MAR",
      .default = countrycode(country, "country.name", "iso3c")
      ),
    year = as.numeric(year),
    oil = as.numeric(oil)
    ) %>%
  dplyr::relocate(iso3c, .before = country) %>%
  dplyr::filter(
    country %!in% c("World", "Antarctica"),
    # filter out Germany -1989 values
    iso3c != "DEU" | year > 1989,
    # filter out East/West Germany 1990- values
    iso3c %!in% c("BRD", "DDR") | year < 1990
  ) %>%
  # collapse across each country-year
  dplyr::group_by(iso3c, year) %>%
  dplyr::summarise(oil = sum(oil, na.rm = TRUE)) %>%
  dplyr::ungroup()

# duplicate 2016 aid data and code as same amount of aid for 2017 - 2019
oil <- oil %>%
  rbind(
    oil %>%
      dplyr::filter(year == 2016) %>%
      dplyr::mutate(year = 2017),
    oil %>%
      dplyr::filter(year == 2016) %>%
      dplyr::mutate(year = 2018),
    oil %>%
      dplyr::filter(year == 2016) %>%
      dplyr::mutate(year = 2019)
  ) %>%
  # create dummy variable for oil exporting countries (for a given year)
  dplyr::mutate(exporter = ifelse(oil >= 1000, 1, 0))


### write data -------------------------------------------------------------------------------------
# writes formatted dataframe as csv files
write.csv(oil, "Data files/Formatted data files/oil.csv", row.names = FALSE)
