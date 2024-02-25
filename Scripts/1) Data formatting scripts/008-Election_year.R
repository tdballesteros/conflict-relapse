# This script creates three binary variables coding if a national (presidential/parliamentary)
# election was held in a country in a given year, in the previous year, and in the subsequent
# year. Note that not all elections are scheduled a year in advance, so the predictive power
# of an election in the subsequent year should be used with caution.

# TODO: years since last nat'l election - NA for grouping 1

### load libraries ----------------------------------------------------------------------
library(readxl)
library(countrycode)
library(dplyr)

### load data file ----------------------------------------------------------------------
elec <- readxl::read_excel("~/Downloads/idea_export_40_5f18dedb47ec1.xls")

### data formatting ----------------------------------------------------------------------
elec <- elec %>%
  # filter out EU Parliament elections
  dplyr::filter(`Election type` != "EU Parliament") %>%
  dplyr::select(Country,Year) %>%
  # using the countrycode package, add iso3c code based on country name
  dplyr::mutate(iso3c = countrycode::countrycode(Country,"country.name","iso3c")) %>%
  dplyr::relocate(iso3c,.before = Country) %>%
  dplyr::rename_with(tolower) %>%
  # filter out non-sovereign entities
  dplyr::filter(iso3c %!in% c("ABW","AIA","ANT","BMU","COK","CYM","FRO","GIB","GRL","VGB"))

# manually add iso3c codes to missing country names: Kosovo;
# Yugoslavia, FR/Union of Serbia and Montenegro; Yugoslavia, SFR (1943-1992)
elec$iso3c[elec$country=="Kosovo"] <- "KSV"
elec$iso3c[elec$country=="Yugoslavia, FR/Union of Serbia and Montenegro"] <- "SRB"
elec$iso3c[elec$country=="Yugoslavia, SFR (1943-1992)"] <- "YUG"

elec <- elec %>%
  # filter out Netherlands Antilles
  dplyr::filter(country != "Netherlands Antilles") %>%
  dplyr::select(-country) %>%
  # using the countrycode package, add country name based on iso3c code
  dplyr::mutate(country = countrycode::countrycode(iso3c,"iso3c","country.name"))

### election dummy variable ----------------------------------------------------------------------
elec <- elec %>%
  # create dummy variable for years where a national parliament/presidential election was held
  dplyr::mutate(natelec = 1) %>%
  dplyr::full_join(expand.grid(iso3c = unique(elec$iso3c), year = c(1945:2019))) %>%
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
  # remove 2020 and 2021 - the future
  dplyr::filter(year < 2020)

### expand dataset ----------------------------------------------------------------------
elec <- elec %>%
  # expand dataframe to have all iso3c-year combos from 1946 - 2019
  dplyr::full_join(expand.grid(iso3c = unique(elec$iso3c), year = c(1945:2019))) %>%
  # using the countrycode package, add country name based on iso3c code
  dplyr::mutate(country = countrycode::countrycode(iso3c,"iso3c","country.name")) %>%
  dplyr::arrange(iso3c,year)

elec$country[elec$iso3c=="KSV"] <- "Kosovo"
elec$country[elec$iso3c=="YUG"] <- "Yugoslavia"

# filter out year 1944
elec <- elec %>%
  dplyr::filter(year > 1944)

### years since last election ----------------------------------------------------------------------
# pull all iso3c codes in the dataset
elec_iso_list <- elec %>%
  dplyr::select(iso3c) %>%
  unique() %>%
  dplyr::pull(iso3c)

# create groupings - an election year or a string of consecutive non-election years
# create an empty placeholder dataframe
elec.df1 <- data.frame()

for(iso in elec_iso_list){
  
  df <- elec_yrs_since %>%
    dplyr::filter(iso3c == iso)
  
  # pull minimum year
  min_yr <- min(df$year)
  
  # start grouping at minimum year with 1
  df$grouping[df$year==min_yr] <- 1
  
  
  for(y in (min_yr+1):2019){
    
    # if year is an election year or the election year binary code differs between this year and the prior year, code as a new grouping
    if((df$natelec[df$iso3c==iso&df$year==y]==1) | (df$natelec[df$iso3c==iso&df$year==y]!=df$natelec[df$iso3c==iso&df$year==(y-1)])){
      
      df$grouping[df$iso3c==iso&df$year==y] <- df$grouping[df$iso3c==iso&df$year==(y-1)] + 1
      
    } else{
      
      # if year is not an election year and prior year was coded the same, code as same grouping
      df$grouping[df$iso3c==iso&df$year==y] <- df$grouping[df$iso3c==iso&df$year==(y-1)]
      
    }
    
  }
  
  elec.df1 <- rbind(elec.df1, df)
  
}

# create list of iso3c-groupings to run the for loop through
elec.list <- elec.df1 %>%
  dplyr::select(iso3c,grouping) %>%
  unique()

# create an empty placeholder dataframe
elec.df2 <- data.frame()

for(i in 1:nrow(elec.list)){
  
  df <- elec.df1 %>%
    dplyr::filter(iso3c == elec.list$iso3c[i],
                  grouping == elec.list$grouping[i]) %>%
    dplyr::arrange(year)
  
  df$yrsince <- row.names(df)
  
  elec.df2 <- rbind(elec.df2,df)
  
}

elec <- elec %>%
  dplyr::left_join(elec.df2 %>%
                     dplyr::select(-natelec),by=c("iso3c","country","year"))

### write data ----------------------------------------------------------------------
# writes formatted dataframe as csv files
write.csv(elec,"Data files/Formatted data files/elections.csv",row.names = FALSE)
