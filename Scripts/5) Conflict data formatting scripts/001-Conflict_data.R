
# This script formats UCDP's conflict dataset.


### load libraries ---------------------------------------------------------------------------------
library(readxl)
library(countrycode)
library(dplyr)


### load data file ---------------------------------------------------------------------------------
ucdp_original <- read.csv("Data files/Raw data files/ucdp-prio-acd-201.csv")
# ucdp_original <- readxl::read_xlsx("Data files/Raw data files/UcdpPrioConflict_v25_1.xlsx")


### format data ------------------------------------------------------------------------------------
ucdp <- ucdp_original %>%
  dplyr::select(confid = conflict_id, country = side_a, year, intensity = intensity_level,
                type_of_conflict) %>%
  # Remove colonial/imperial wars and interstate conflict
  dplyr::filter(type_of_conflict > 2) %>%
  dplyr::select(-type_of_conflict) %>%
  dplyr::arrange(confid, year) %>%
  dplyr::mutate(
    country = gsub(pattern = "Government of ", replacement = "", country),
    confid = as.factor(confid),
    # using the countrycode package, add iso3c based on country name
    iso3c = dplyr::case_when(
      country == "Hyderabad" ~ "IND",
      country == "South Vietnam" ~ "RVN",
      country == "South Yemen" ~ "YPR",
      country == "Yemen (North Yemen)" & year < 1991 ~ "YAR",
      country == "Yemen (North Yemen)" & year >= 1991 ~ "YEM",
      .default = countrycode::countrycode(country,"country.name","iso3c")
      )) %>%
  dplyr::relocate(iso3c, .before = country) %>%
  dplyr::select(-country) %>%
  dplyr::mutate(
    # using the countrycode package, add country name based on iso3c code
    country = dplyr::case_when(
      iso3c == "RVN" ~ "South Vietnam",
      iso3c == "YAR" ~ "North Yemen",
      iso3c == "YPR" ~ "South Yemen",
      .default = countrycode::countrycode(iso3c,"iso3c","country.name")
      )) %>%
  dplyr::relocate(country, .after = iso3c)


### recode countries -------------------------------------------------------------------------------
#### Soviet Union + successors ---------------------------------------------------------------------
# recode SSRs in the Soviet Union from RUS to the specific SSR

# 210 - EST (SOV)
ucdp$country[ucdp$confid == "210"] <- "Estonia"
ucdp$iso3c[ucdp$confid == "210"] <- "EST"

# 211 - LVA (SOV)
ucdp$country[ucdp$confid == "211"] <- "Latvia"
ucdp$iso3c[ucdp$confid == "211"] <- "LVA"

# 212 - LTU (SOV)
ucdp$country[ucdp$confid == "212"] <- "Lithuania"
ucdp$iso3c[ucdp$confid == "212"] <- "LTU"

# 213 - UKR (SOV)
ucdp$country[ucdp$confid == "213"] <- "Ukraine"
ucdp$iso3c[ucdp$confid == "213"] <- "UKR"

# 376 - AZE (SOV)
ucdp$country[ucdp$confid == "376"] <- "Azerbaijan"
ucdp$iso3c[ucdp$confid == "376"] <- "AZE"

# 377 - AZE (SOV)
ucdp$country[ucdp$confid == "377"] <- "Azerbaijan"
ucdp$iso3c[ucdp$confid == "377"] <- "AZE"

# keep coded as RUS: 399, 401, 414, 432, 13588

#### Yemen -----------------------------------------------------------------------------------------
# recode into YAR or YPR

# 13645 - YEM (coding as YAR)
ucdp$country[ucdp$confid == "13645"] <- "North Yemen"
ucdp$iso3c[ucdp$confid == "13645"] <- "YAR"

# 230 - YAR
ucdp$country[ucdp$confid == "230"] <- "North Yemen"
ucdp$iso3c[ucdp$confid == "230"] <- "YAR"

# 359 - YPR
ucdp$country[ucdp$confid == "359"] <- "South Yemen"
ucdp$iso3c[ucdp$confid == "235930"] <- "YPR"

# 402 - YEM (coding as YPR)
ucdp$country[ucdp$confid == "402"] <- "South Yemen"
ucdp$iso3c[ucdp$confid == "402"] <- "YPR"

#### Yugoslavia + successors -----------------------------------------------------------------------
# recode republics of Yugoslavia from SRB to the specific republic

# 384 - SVN
ucdp$country[ucdp$confid == "384"] <- "Slovenia"
ucdp$iso3c[ucdp$confid == "384"] <- "SVN"

# 385 - HRV
ucdp$country[ucdp$confid == "385"] <- "Croatia"
ucdp$iso3c[ucdp$confid == "385"] <- "HRV"

# 412 - SRB
ucdp$country[ucdp$confid == "412"] <- "Serbia"
ucdp$iso3c[ucdp$confid == "412"] <- "SRB"


### expand dataset ---------------------------------------------------------------------------------
ucdp <- ucdp %>%
  # expand dataset to have entries for each conflict id for all years 1946-2019
  dplyr::full_join(expand.grid(confid = unique(ucdp$confid), year = c(1946:2019))) %>%
  dplyr::mutate(
    # create binary conflict variable based on intensity variable from source dataset
    conflict = ifelse(intensity %in% c(1:2), 1, 0),
    # fill entries without intensity coding as 0
    intensity = ifelse(is.na(intensity), 0, intensity)
    )

list_of_conflicts_and_countries <- ucdp %>%
  dplyr::select(confid, iso3c, country) %>%
  na.omit() %>%
  unique()

# add iso3c and country names to newly expanded dataset to fill in missing iso3c/country values
ucdp <- ucdp %>%
  dplyr::select(-c(iso3c, country)) %>%
  dplyr::full_join(list_of_conflicts_and_countries, by = "confid") %>%
  dplyr::select(confid, iso3c, country, year, conflict, intensity)
  
list_of_conflicts <- list_of_conflicts_and_countries %>%
  dplyr::pull(confid)


### fill short peace gaps --------------------------------------------------------------------------
# replaces one-year peace intervals with continued conflict, per literature standard
ucdp2 <- data.frame()

for(i in list_of_conflicts){
  
  tmp <- ucdp %>%
    dplyr::filter(confid == i) %>%
    dplyr::arrange(year) %>%
    dplyr::mutate(conflict_recoded = 0)
  
  for(j in 2:(nrow(tmp) - 1)){
    
    # if the preceding and proceding year are both coded as a conflict...
    if(tmp$conflict[j - 1] == 1 & tmp$conflict[j + 1] == 1){
      
      if(tmp$conflict[j] == 0){
        
        tmp$conflict_recoded[j] <- 1
        
      }
      
      # ... code year as conflict
      tmp$conflict[j] <- 1
      
    }
  }
  
  ucdp2 <- rbind(ucdp2, tmp)
  
}


### create peace/conflict groupings ----------------------------------------------------------------
ucdp3 <- data.frame()

for(i in list_of_conflicts){
  
  tmp <- ucdp2 %>%
    dplyr::filter(confid == i) %>%
    dplyr::arrange(year)
  
  index <- c(TRUE, diff(tmp$conflict) != 0)
  index <- cumsum(index)
  
  tmp$grouping <- index
  
  ucdp3 <- rbind(ucdp3, tmp)
  
}

# filter out first grouping if it is not a conflict, as this is before the
# initial outbreak of conflict
ucdp3 <- ucdp3 %>%
  dplyr::filter(grouping != 1 | conflict == 1)


### remove unneeded conflict year entries ----------------------------------------------------------
ucdp3 <- ucdp3 %>%
  dplyr::mutate(remove = NA)

ucdp4 <- data.frame()

for(i in list_of_conflicts){
  
  tmp <- ucdp3 %>%
    dplyr::filter(confid == i)
  
  if(nrow(tmp) > 1){
  for(k in 2:nrow(tmp)){
    
    # if the previous and current year are both coded as conflict...
    if(tmp$conflict[k - 1] == 1 && tmp$conflict[k] == 1){
      
      # flag current year for removal from dataset
      tmp$remove[k] <- 1
      
      }
     }
    }

  ucdp4 <- rbind(ucdp4,tmp)

  }

# remove flagged entries
ucdp4 <- ucdp4 %>%
  dplyr::filter(is.na(remove)) %>%
  dplyr::select(-remove)


### create summary table ---------------------------------------------------------------------------
ucdp5 <- ucdp4 %>%
  dplyr::group_by(confid, conflict, grouping) %>%
  dplyr::mutate(
    years = paste(min(year), "-", max(year)),
    intensity = max(intensity)
    ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(confid, conflict, years) %>%
  dplyr::summarise(length = n()) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(confid, years) %>%
  dplyr::full_join(list_of_conflicts_and_countries, by = "confid") %>%
  dplyr::mutate(
    yrstart = as.numeric(stringr::str_sub(years, start = 1, end = 4)),
    yrend = as.numeric(stringr::str_sub(years, start = -4))
    ) %>%
  dplyr::relocate(iso3c, .after = confid) %>%
  dplyr::relocate(country, .after = iso3c)


### write data -------------------------------------------------------------------------------------
# writes formatted dataframes as csv files
write.csv(ucdp3, "Data files/Formatted data files/conflict_full_data.csv", row.names = FALSE)
write.csv(ucdp4, "Data files/Formatted data files/conflict_years.csv", row.names = FALSE)
write.csv(ucdp5, "Data files/Formatted data files/conflict_table.csv", row.names = FALSE)


# #### termination data on ucdp4 ####
# term <- read_excel("~/Documents/conflict_outcomes2.xlsx") %>%
#   select(-reason)
# term$confid <- as.character(term$confid)
# 
# term$outcome[is.na(term$outcome)] <- -1
# 
# ucdp4a <- ucdp4 %>%
#   full_join(term,by=c("confid","year")) %>%
#   arrange(year) %>%
#   arrange(confid)
# 
# for(i in 2:nrow(ucdp4a)){
#   if(is.na(ucdp4a$outcome[i])){
#     ucdp4a$outcome[i] <- ucdp4a$outcome[i-1]
#   }
# }
# 
# ucdp4a <- ucdp4a %>%
#   pivot_wider(names_from = outcome, values_from = outcome)
# 
# ucdp4a$`6`[!is.na(ucdp4a$`6`)] <- 1
# ucdp4a$`5`[!is.na(ucdp4a$`5`)] <- 1
# ucdp4a$`4`[!is.na(ucdp4a$`4`)] <- 1
# ucdp4a$`3`[!is.na(ucdp4a$`3`)] <- 1
# ucdp4a$`2`[!is.na(ucdp4a$`2`)] <- 1
# ucdp4a$`1`[!is.na(ucdp4a$`1`)] <- 1
# ucdp4a$`-1`[!is.na(ucdp4a$`-1`)] <- 1
# ucdp4a$`6`[is.na(ucdp4a$`6`)] <- 0
# ucdp4a$`5`[is.na(ucdp4a$`5`)] <- 0
# ucdp4a$`4`[is.na(ucdp4a$`4`)] <- 0
# ucdp4a$`3`[is.na(ucdp4a$`3`)] <- 0
# ucdp4a$`2`[is.na(ucdp4a$`2`)] <- 0
# ucdp4a$`1`[is.na(ucdp4a$`1`)] <- 0
# ucdp4a$`-1`[is.na(ucdp4a$`-1`)] <- 0
