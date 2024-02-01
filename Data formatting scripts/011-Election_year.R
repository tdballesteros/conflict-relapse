# creates a binary variable coding if a national election was held in a country in a given year

# TODO: years since last nat'l election

### load libraries ----------------------------------------------------------------------
library(readxl)
library(countrycode)
library(dplyr)

### load data files ----------------------------------------------------------------------
elec <- readxl::read_excel("~/Downloads/idea_export_40_5f18dedb47ec1.xls")

### data formatting ----------------------------------------------------------------------
elec <- elec %>%
  # filter out EU Parliament elections
  dplyr::filter(`Election type` != "EU Parliament") %>%
  dplyr::select(Country,Year) %>%
  # using the countrycode package, add iso3c code based on country name
  dplyr::mutate(iso3c = countrycode(Country,"country.name","iso3c")) %>%
  dplyr::relocate(iso3c,.before = Country) %>%
  dplyr::rename_with(tolower)

# manually add country codes to missing country names: Kosovo; Netherlands Antilles;
# Yugoslavia, FR/Union of Serbia and Montenegro; Yugoslavia, SFR (1943-1992)
elec$iso3c[elec$country=="Kosovo"] <- "KSV"
elec$iso3c[elec$country=="Netherlands Antilles"] <- "ANT"
elec$iso3c[elec$country=="Yugoslavia, FR/Union of Serbia and Montenegro"] <- "SRB"
elec$iso3c[elec$country=="Yugoslavia, SFR (1943-1992)"] <- "YUG"

### election dummy variable ----------------------------------------------------------------------
elec <- elec %>%
  # create dummy variable for years where a national parliament/presidential election was held
  dplyr::mutate(natelec = 1) %>%
  dplyr::full_join(expand.grid(iso3c = unique(elec$iso3c), year = c(1946:2019))) %>%
  dplyr::mutate(natelec = ifelse(is.na(natelec),0,natelec),
                # using the countrycode package, add country name based on iso3c code
                country = countrycode(iso3c,"iso3c","country.name")) %>%
  unique()

# manually add country names to missing iso3c codes: ANT, KSV, YUG
elec$country[elec$iso3c=="ANT"] <- "Netherlands Antilles"
elec$country[elec$iso3c=="KSV"] <- "Kosovo"
elec$country[elec$iso3c=="YUG"] <- "Yugoslavia"

### prior/next year election dummy variable ----------------------------------------------------------------------
# create dummy variables for national election happening in the next year (natelec.n) and
# happened in the prior year (natelec.l)
elec_next <- elec %>%
  dplyr::mutate(year = year - 1) %>%
  dplyr::rename(natelec.n = natelec)

elec_last <- elec %>%
  dplyr::mutate(year = year + 1) %>%
  dplyr::rename(natelec.l = natelec)
# join next and prior year election variables with main dataset
elec <- elec %>%
  dplyr::full_join(elec_next,by=c("iso3c","country","year")) %>%
  dplyr::full_join(elec_last,by=c("iso3c","country","year")) %>%
  dplyr::mutate(natelec.n = ifelse(is.na(natelec.n),0,natelec.n),
                natelec.l = ifelse(is.na(natelec.l),0,natelec.l)) %>%
  dplyr::filter(year >= 1946,
                # remove 2020 and 2021 - the future
                year < 2020)
                
### write data ----------------------------------------------------------------------
# writes formatted dataframe as csv files
write.csv(elec,"Data files/Formatted data files/elections.csv",row.names = FALSE)
