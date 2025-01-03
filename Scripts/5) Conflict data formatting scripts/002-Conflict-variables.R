
# This script uses the formatted conflict data and calculates conflict ID-year level variables.

### load libraries ----------------------------------------------------------------------
library(readxl)
library(countrycode)
library(dplyr)

### load data file ----------------------------------------------------------------------
conflict_years <- read.csv("Data files/Formatted data files/conflict_years.csv")
cd <- read.csv("Data files/Raw data files/contdird.csv")
conflict_table <- read.csv("Data files/Formatted data files/conflict_table.csv")
conflict_full_data <- read.csv("Data files/Formatted data files/conflict_full_data.csv")
# conflict_termination_data <- readxl::read_xlsx("Data files/Raw data files/ucdp-term-conf-2015.xlsx")
conflict_termination_data <- readxl::read_xlsx("Data files/Raw data files/ucdp-term-acd-3-2021.xlsx")

### years since conflict ----------------------------------------------------------------------

# calculate years since last year of conflict (/ last year of
# non-conflict for conflict entries)
years_since_conf <- conflict_years %>%
  dplyr::group_by(confid,grouping) %>%
  dplyr::mutate(yr_lower = min(year,na.rm=TRUE) - 1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(yrs_since = year - yr_lower) %>%
  
  # Buhaug 2006 p669 - decay
  dplyr::mutate(
    yrs_since_e = 2^(-yrs_since/2),
    # Based on Hegre et al. 2001 (I think this is the half-life they used)
    yrs_since_e2 = 2^(-yrs_since/16),
    yrs_since_4 = 2^(-yrs_since)
  ) %>%
  
  dplyr::select(confid,year,yrs_since,yrs_since_e,yrs_since_e2,yrs_since_4)

# half-life calculation

# hold3 <- years_since_conf %>%
#   group_by(confid,grouping) %>%
#   filter(year == max(year)) %>%
#   ungroup() %>%
#   filter(year != 2019)
# 
# hist(hold3$yrs_since)
# 
# hold4 <- data.frame(t = c(2:51), y = c(38,27,17,6,11,6,5,7,5,2,4,3,3,3,2,3,3,5,1,1,2,1,0,1,3,0,1,1,2,0,0,2,1,0,0,0,0,0,
#                                        0,0,1,0,0,0,0,0,0,0,1,0))
# 
# hold5 <- data.frame(t = c(3:34), y = c(27,24,12,2,7,5,2,5,2,1,2,1,0,1,0,0,2,1,1,1,0,0,0,0,0,0,0,0,2,0,0,1))
# 
# 
# fit <- nls(y ~ SSasymp(t, yf, y0, log_alpha), data = hold5)
# fit
# #qplot(t, y, data = broom::augment(fit)) + geom_line(aes(y = .fitted))
# 
# alpha <- 1/0.8533
# 
# yrsince <- yrsince %>%
#   mutate(yrsince3 = 2^(-yrsince/alpha))

### other conflicts in the country ----------------------------------------------------------------------

other_conf <- conflict_years %>%
  dplyr::select(-country) %>%
  dplyr::mutate(iso3c = dplyr::case_when(
    # recode post-Soviet states to SOV 1946-1990
    # aka rebellions in EST, LVA, LTU and UKR all happened in the same country in the 1940s: the Soviet Union
    iso3c %in% c(
      "EST","LVA","LTU","BLR","MDA","UKR","RUS","GEO","ARM",
      "AZE","KAZ","TKM","TJK","UZB","KGZ") & year < 1991 ~ "SOV",
    .default = iso3c
  )) %>%
  dplyr::group_by(iso3c,year) %>%
  dplyr::summarise(num_conf = sum(conflict)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    # removing the conflict in reference to the list, so it is additional ones
    num_other_conf = dplyr::case_when(
      num_conf == 0 ~ 0,
      .default = num_conf - 1
    ),
    other_conf = dplyr::case_when(
      num_other_conf > 0 ~ 1,
      .default = 0
    )
  ) %>%
  # merge in conflict ID information
  dplyr::full_join(conflict_years,by=c("iso3c","year")) %>%
  dplyr::select(confid,iso3c,year,num_conf,num_other_conf,other_conf)

### conflicts in neighbouring country ----------------------------------------------------------------------

# Binary for a neighbouring country being in conflict
# can do number of neighbouring countries in conflict and number of conflicts in neighbouring countries... if time allows

# for the country adjacency matrix, assume no changes in 2017-2019;
# duplicate 2016 values for these years
cd16 <- cd %>%
  dplyr::filter(year == 2016)
cd <- cd %>%
  rbind(cd17 <- cd16 %>%
          mutate(year = 2017),
        cd18 <- cd16 %>%
          mutate(year = 2018),
        cd19 <- cd16 %>%
          mutate(year = 2019))

# format country adjacency matrix
cd <- cd %>%
  dplyr::filter(
    conttype <= 3,
    year >= 1946
    ) %>%
  dplyr::mutate(
    iso3c = dplyr::case_when(
      state1ab=="CZE" ~ "CZE",
      state1ab=="GDR" ~ "DDR",
      state1ab=="GFR" ~ "BRD",
      state1ab=="KOS" ~ "KOS",
      state1ab=="RVN" ~ "RVN",
      state1ab=="YAR" ~ "YAR",
      state1ab=="YPR" ~ "YPR",
      state1ab=="YUG" ~ "YUG",
      state1ab=="ZAN" ~ "ZAN",
      .default = countrycode(state1ab,"cowc","iso3c")
      ),
    neighbour = dplyr::case_when(
      state2ab=="CZE" ~ "CZE",
      state2ab=="GDR" ~ "DDR",
      state2ab=="GFR" ~ "BRD",
      state2ab=="KOS" ~ "KOS",
      state2ab=="RVN" ~ "RVN",
      state2ab=="YAR" ~ "YAR",
      state2ab=="YPR" ~ "YPR",
      state2ab=="YUG" ~ "YUG",
      state2ab=="ZAN" ~ "ZAN",
      .default = countrycode(state2ab,"cowc","iso3c")
    )) %>%
  dplyr::select(iso3c,neighbour,year,conttype)


# constructing neighbours in conflict df
neigh_in_conf <- expand.grid(iso3c = unique(cd$iso3c), year = c(1946:2019), num_neigh_conf = NA)

for(i in 1:nrow(neigh_in_conf)){
  
  # list of neighboring countries
  neighbours <- cd %>%
    dplyr::filter(iso3c == neigh_in_conf$iso3c[i]) %>%
    dplyr::filter(year == neigh_in_conf$year[i]) %>%
    dplyr::pull(neighbour)
  
  tmp <- other_conf %>%
    dplyr::select(iso3c,year,num_conf) %>%
    unique() %>%
    dplyr::filter(iso3c %in% neighbours) %>%
    dplyr::filter(year == neigh_in_conf$year[i])
  
  if(nrow(tmp)>0){
    
    conf_sum <- sum(tmp$num_conf,na.rm=TRUE)
    
  } else{
    
    conf_sum <- 0
    
  }
  
  neigh_in_conf$num_neigh_conf[i] <- conf_sum

}

neigh_in_conf <- neigh_in_conf %>%
  # create binary for if a conflict is present in a
  # neighbouring country
  dplyr::mutate(binary_neigh_conf = dplyr::case_when(
    num_neigh_conf > 0 ~ 1,
    num_neigh_conf == 0 ~ 0
  )) %>%
  # merge in conflict ID information
  dplyr::right_join(conflict_years,by=c("iso3c","year")) %>%
  dplyr::select(confid,iso3c,year,num_neigh_conf,binary_neigh_conf)

### previous relapse ----------------------------------------------------------------------

prev_relap <- conflict_years %>%
  dplyr::mutate(
    num_repalses = dplyr::case_when(
      grouping %in% c(3:4) ~ 1,
      grouping %in% c(5:6) ~ 2,
      grouping %in% c(7:8) ~ 3,
      grouping %in% c(9:10) ~ 4,
      grouping %in% c(11:12) ~ 5,
      grouping %in% c(13:14) ~ 6,
      grouping %in% c(15:16) ~ 7,
      .default = 0
      ),
    binary_num_relapses = dplyr::case_when(
      num_repalses >= 1 ~ 1,
      .default = 0
    )
  ) %>%
  dplyr::select(confid,iso3c,year,num_repalses,binary_num_relapses)

### length of conflict ----------------------------------------------------------------------

conf_length <- conflict_full_data %>%
  dplyr::filter(conflict == 1) %>%
  dplyr::group_by(confid,grouping) %>%
  dplyr::summarise(
    year_end = max(year,na.rm=TRUE),
    conf_length = n()
  ) %>%
  dplyr::ungroup()

confid_list <- unique(conf_length$confid)

conf_length2 <- data.frame()

for(i in 1:length(confid_list)){

  tmp <- conf_length %>%
    dplyr::filter(confid == confid_list[i])

  startyr <- min(tmp$year_end,na.rm=TRUE)

  tmp <- tmp %>%
    tidyr::complete(confid, year_end = c(startyr:2019)) %>%
    dplyr::arrange(year_end)

  for(j in 1:nrow(tmp)){
    if(is.na(tmp$conf_length[j])){
      tmp$conf_length[j] <- tmp$conf_length[j-1]
    }
  }

  conf_length2 <- rbind(conf_length2,tmp)
  
}

conf_length2 <- conf_length2 %>%
  dplyr::rename(year = year_end) %>%
  dplyr::select(-grouping) %>%
  # merge in iso3c code information
  dplyr::right_join(conflict_years,by=c("confid","year")) %>%
  dplyr::select(confid,iso3c,year,conf_length)

### conflict termination ----------------------------------------------------------------------

conf_term <- conflict_termination_data %>%
  dplyr::rename(
    confid = conflict_id,
    end_year = year) %>%
  dplyr::filter(
    type_of_conflict > 2,
    confterm == 1) %>%
  dplyr::select(confid,end_year,outcome)

# replace confid 442 end year with 2011 to match the termination dataset coding
conf_term$end_year[conf_term$confid==442] <- 2011

# format conflict data
conflict_termination <- conflict_full_data %>%
  dplyr::group_by(confid,grouping) %>%
  dplyr::mutate(end_year = dplyr::case_when(
    conflict == 1 ~ max(year,na.rm=TRUE),
    conflict == 0 ~ min(year,na.rm=TRUE)-1
  )) %>%
  dplyr::ungroup() %>%
  
  # merge in termination data
  dplyr::left_join(conf_term,by=c("confid","end_year")) %>%
  dplyr::select(confid,iso3c,year,conflict,outcome) %>%
  
  # recode outcome variable
  dplyr::mutate(outcome = dplyr::case_match(outcome,
    1 ~ "1: Peace Agreement",
    2 ~ "2: Ceasefire Agreement",
    3 ~ "3: Government Victory",
    4 ~ "4: Non-Government Victory",
    5 ~ "5: Low Activity",
    6 ~ "6: Actor Ceases to Exist"
  ))

### merge data ----------------------------------------------------------------------
conflict_variables <- dplyr::full_join(years_since_conf,other_conf,by=c("confid","year")) %>%
  dplyr::full_join(neigh_in_conf,by=c("confid","iso3c","year")) %>%
  dplyr::full_join(prev_relap,by=c("confid","iso3c","year")) %>%
  dplyr::full_join(conf_length2,by=c("confid","iso3c","year")) %>%
  dplyr::full_join(conflict_termination,by=c("confid","iso3c","year"))
  
### write data ----------------------------------------------------------------------
# writes formatted dataframe as csv files
write.csv(conflict_variables,"Data files/Formatted data files/conflict_variables.csv",row.names = FALSE)
