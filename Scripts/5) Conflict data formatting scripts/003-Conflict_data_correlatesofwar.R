# This script formats Correlates of War's conflict dataset.
# https://correlatesofwar.org/data-sets/cow-war/

### load libraries ----------------------------------------------------------------------
library(readxl)
library(countrycode)
library(lubridate)
library(dplyr)

### load data file ----------------------------------------------------------------------
cow_conflict_original <- read.csv("Data files/Raw data files/INTRA-STATE WARS v5.1 CSV.csv")

### format data ----------------------------------------------------------------------

cow_conflict <- cow_conflict_original %>%
  
  # general filters
  dplyr::filter(
    #StartYr1 >= 1900,
    WarType != 7 #intercommunal
  ) %>%
  
  # drop unneeded variables
  dplyr::select(-c(V5RegionNum,CcodeA,Version)) %>%
  
  # add codes for country participants (SideA)
  dplyr::mutate(
    iso3c = dplyr::case_when(
      SideA == "Yugoslavia" ~ "YUG",
      SideA == "Regional Militaries" ~ "CHN",
      SideA == "Republic of Vietnam" ~ "RVN",
      SideA == "Ukraine and Russia" ~ "UKR",
      SideA == "Yemen Arab Republic" ~ "YAR",
      SideA == "Yemen People's Republic" ~ "YPR",
      .default = countrycode::countrycode(SideA,"country.name","iso3c")
    )) %>%
  dplyr::relocate(iso3c, .before = SideA) %>%
  
  # calculate number of conflict periods per line / binary coding
  dplyr::mutate(
    conf1 = 1,
    conf2 = dplyr::case_when(
      StartYr2 > 0 ~ 1,
      .default = 0
    ),
    conf3 = dplyr::case_when(
      StartYr2 > 0 & StartYr3 > 0 ~ 1,
      .default = 0
    ),
    conf4 = dplyr::case_when(
      StartYr2 > 0 & StartYr3 > 0 & StartYr4 > 0 ~ 1,
      .default = 0
    ),
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    conf_on_line = sum(conf1 + conf2 + conf3 + conf4)
  ) %>%
  
  # recode outcome variable
  dplyr::mutate(
    Outcome = dplyr::case_match(
      Outcome,
      1 ~ "1: Side A wins",
      2 ~ "2: Side B wins",
      3 ~ "3: Compromise",
      4 ~ "4: The war was transformed into another type of war",
      5 ~ "5: The war is ongoing as of 12/31/2007",
      6 ~ "6: Stalemate",
      7 ~ "7: Conflict continues at below war level",
      .default = "Unknown"
    )) %>%
  
  # recode wartype variable
  dplyr::mutate(
    WarType = dplyr::case_match(
      WarType,
      4 ~ "4: Civil War for Central Control",
      5 ~ "5: Civil War over Local Issue",
      6 ~ "6: Regional Internal",
      7 ~ "7: Intercommunal"
    )) %>%
  
  # format all Day/Month variables as numeric
  dplyr::mutate(
    StartDy1 = as.numeric(StartDy1),
    StartMo1 = as.numeric(StartMo1),
    EndDy1 = as.numeric(EndDy1),
    EndMo1 = as.numeric(EndMo1),
    
    StartDy2 = as.numeric(StartDy2),
    StartMo2 = as.numeric(StartMo2),
    EndDy2 = as.numeric(EndDy2),
    EndMo2 = as.numeric(EndMo2),
    
    StartDy3 = as.numeric(StartDy3),
    StartMo3 = as.numeric(StartMo3),
    EndDy3 = as.numeric(EndDy3),
    EndMo3 = as.numeric(EndMo3),
    
    StartDy4 = as.numeric(StartDy4),
    StartMo4 = as.numeric(StartMo4),
    EndDy4 = as.numeric(EndDy4),
    EndMo4 = as.numeric(EndMo4)
  )


#### conflict-wide data ----------------------------------------------------------------------

conflict_line_data <- cow_conflict %>%
  dplyr::select(
    # identifying variables
    WarNum, WarName, iso3c, SideA, SideB, StartYr1, conf_on_line,
    
    # across-row variables
    WDuratDays, WDuratMo, TotNatMonWar, TransFrom, Initiator, Outcome, TransTo, DeathsSideA,
    DeathsSideB, TotalBDeaths
  )

# remove across-row variables from dataset moving forward
cow_conflict_year <- cow_conflict %>%
  dplyr::select(-c(WDuratDays, WDuratMo, TotNatMonWar, TransFrom, Initiator, Outcome, TransTo,
                   DeathsSideA, DeathsSideB, TotalBDeaths))


#### lengthen data ----------------------------------------------------------------------

# create datasets for each of the four conflict period columns, filtering out if unused

#
# PERIOD 1 #
#
cow_conflict_pd1 <- cow_conflict_year %>%
  dplyr::filter(conf1 == 1) %>%
  dplyr::select(-c(StartDy2,StartMo2,StartYr2,EndDy2,EndMo2,EndYr2,
                   StartDy3,StartMo3,StartYr3,EndDy3,EndMo3,EndYr3,
                   StartDy4,StartMo4,StartYr4,EndDy4,EndMo4,EndYr4)) %>%
  dplyr::rename(
    StartMo = StartMo1,
    StartDy = StartDy1,
    StartYr = StartYr1,
    EndMo = EndMo1,
    EndDy = EndDy1,
    EndYr = EndYr1
  ) %>%
  dplyr::mutate(
    period = 1
  )


#
# PERIOD 2 #
#
cow_conflict_pd2 <- cow_conflict_year %>%
  dplyr::filter(conf2 == 1) %>%
  dplyr::select(-c(StartDy1,StartMo1,StartYr1,EndDy1,EndMo1,EndYr1,
                   StartDy3,StartMo3,StartYr3,EndDy3,EndMo3,EndYr3,
                   StartDy4,StartMo4,StartYr4,EndDy4,EndMo4,EndYr4)) %>%
  dplyr::rename(
    StartMo = StartMo2,
    StartDy = StartDy2,
    StartYr = StartYr2,
    EndMo = EndMo2,
    EndDy = EndDy2,
    EndYr = EndYr2
  ) %>%
  dplyr::mutate(
    period = 2
  )


#
# PERIOD 3 #
#
cow_conflict_pd3 <- cow_conflict_year %>%
  dplyr::filter(conf3 == 1) %>%
  dplyr::select(-c(StartDy1,StartMo1,StartYr1,EndDy1,EndMo1,EndYr1,
                   StartDy2,StartMo2,StartYr2,EndDy2,EndMo2,EndYr2,
                   StartDy4,StartMo4,StartYr4,EndDy4,EndMo4,EndYr4)) %>%
  dplyr::rename(
    StartMo = StartMo3,
    StartDy = StartDy3,
    StartYr = StartYr3,
    EndMo = EndMo3,
    EndDy = EndDy3,
    EndYr = EndYr3
  ) %>%
  dplyr::mutate(
    period = 3
  )


#
# PERIOD 4 #
#
cow_conflict_pd4 <- cow_conflict_year %>%
  dplyr::filter(conf4 == 1) %>%
  dplyr::select(-c(StartDy1,StartMo1,StartYr1,EndDy1,EndMo1,EndYr1,
                   StartDy2,StartMo2,StartYr2,EndDy2,EndMo2,EndYr2,
                   StartDy3,StartMo3,StartYr3,EndDy3,EndMo3,EndYr3)) %>%
  dplyr::rename(
    StartMo = StartMo4,
    StartDy = StartDy4,
    StartYr = StartYr4,
    EndMo = EndMo4,
    EndDy = EndDy4,
    EndYr = EndYr4
  ) %>%
  dplyr::mutate(
    period = 4
  )

cow_conflict_year <- rbind(cow_conflict_pd1,
                           cow_conflict_pd2,
                           cow_conflict_pd3,
                           cow_conflict_pd4) %>%
  
  rowwise() %>%
  dplyr::mutate(
    date_start = as.Date(paste0(StartDy,"-",StartMo,"-",StartYr), "%d-%m-%Y"),
    date_end = as.Date(paste0(EndDy,"-",EndMo,"-",EndYr), "%d-%m-%Y") ,
  
    # estimate missing days/months as 15th (day) and June/6th (month)
    StartDy_fmt = dplyr::case_when(
      StartDy == -9 | is.na(StartDy) ~ 15,
      .default = StartDy
    ),
    StartMo_fmt = dplyr::case_when(
      StartMo == -9 | is.na(StartMo) ~ 6,
      .default = StartMo
    ),
    EndDy_fmt = dplyr::case_when(
      EndDy == -9 | is.na(EndDy) ~ 15,
      .default = EndDy
    ),
    EndMo_fmt = dplyr::case_when(
      EndMo == -9 | is.na(EndMo) ~ 6,
      .default = EndMo
    ),
    
    date_start_est = as.Date(paste0(StartDy_fmt,"-",StartMo_fmt,"-",StartYr), "%d-%m-%Y"),
    date_end_est = as.Date(paste0(EndDy_fmt,"-",EndMo_fmt,"-",EndYr), "%d-%m-%Y"),
    
    date_exact_flag = dplyr::case_when(
      !is.na(date_start) & !is.na(date_end) ~ 1,
      .default = 0
      ),
    duration_days = date_end - date_start,
    duration_days_est = date_end_est - date_start_est
  )


#### add all years ----------------------------------------------------------------------

# blank dataframe
# cow_conflict_years2 <- data.frame(
#   WarNum = as.numeric(), WarName = as.character(), WarType = as.character(), iso3c = as.character(),
#   SideA = as.character(), SideB = as.character(), Intnl = as.integer(), StartMo = as.numeric(),
#   StartDy = as.numeric(), StartYr = as.integer(), EndMo = as.numeric(), EndDy = as.numeric(),
#   EndYr = as.integer(), conf1 = as.numeric(), conf2 = as.numeric(), conf3 = as.numeric(),
#   conf4 = as.numeric(), conf_on_line = as.numeric(), period = as.numeric(), date_start = as.Date(),
#   date_end = as.Date(), StartDy_fmt = as.numeric(), StartMo_fmt = as.numeric(), EndDy_fmt = as.numeric(),
#   EndMo_fmt = as.numeric(), date_start_est = as.Date(), date_end_est = as.Date(),
#   date_exact_flag = as.numeric(), duration_days = as.difftime(), duration_days_est = as.difftime()
#   )

cow_conflict_year <- cow_conflict_year %>%
  dplyr::mutate(
    EndYr_tmp = dplyr::case_when(
      EndYr == -7 | is.na(EndYr) ~ 2014,
      .default = EndYr
    ),
    year = StartYr
  )

for(r in 1:nrow(cow_conflict_year)){
  
  df <- cow_conflict_year[r,]
  
  years <- c(df$StartYr:df$EndYr_tmp)
  years_additional <- years[years != df$StartYr]
  
  for(y in years_additional){
    
    df2 <- df %>%
      dplyr::mutate(year = y)
    
    cow_conflict_year <- cow_conflict_year %>%
      rbind(df2)
    
  }}

cow_conflict_year <- cow_conflict_year %>%
  dplyr::select(-EndYr_tmp)
  



# cow_current <- cow_conflict_data_by_year %>%
#   dplyr::filter(year >= 1800)
# 
# cow_old <- cow_conflict_data_by_year %>%
#   dplyr::filter(year < 1800)
# 

#### save data ----------------------------------------------------------------------

# write.csv(cow_conflict_data_by_year,"cow_conf_data.csv")

