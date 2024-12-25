# This script formats Correlates of War's conflict dataset.
# https://correlatesofwar.org/data-sets/cow-war/

### load libraries ----------------------------------------------------------------------
library(readxl)
library(countrycode)
library(dplyr)

### load data file ----------------------------------------------------------------------
cow_conflict_original <- read.csv("Data files/Raw data files/INTRA-STATE WARS v5.1 CSV.csv")

### format data ----------------------------------------------------------------------
cow_conflict <- cow_conflict_original %>%
  dplyr::filter(
    StartYr1 >= 1900,
    WarType != 7 #intercommunal
    ) %>%
  dplyr::select(-c(StartMo1,StartDy1,EndMo1,EndDy1,StartMo2,StartDy2,EndMo2,EndDy2,
                   StartMo3,StartDy3,EndMo3,EndDy3,StartMo4,StartDy4,EndMo4,EndDy4,
                   Version)) %>%
  
  # add codes for country participants
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
  dplyr::relocate(iso3c, .before = SideA)

# individual parts 1-4 of data frame
cow_conflict_pt1 <- cow_conflict %>%
  dplyr::select(-c(StartYr2,EndYr2,StartYr3,EndYr3,StartYr4,EndYr4))
cow_conflict_pt2 <- cow_conflict %>%
  dplyr::select(-c(StartYr1,EndYr1,StartYr3,EndYr3,StartYr4,EndYr4))
cow_conflict_pt3 <- cow_conflict %>%
  dplyr::select(-c(StartYr1,EndYr1,StartYr2,EndYr2,StartYr4,EndYr4))
cow_conflict_pt4 <- cow_conflict %>%
  dplyr::select(-c(StartYr1,EndYr1,StartYr2,EndYr2,StartYr3,EndYr3))


cow_data_to_combine1 <- data.frame(
  WarNum = as.numeric(),
  WarName = as.character(),
  V5RegionNum = as.integer(),
  WarType = as.integer(),
  CcodeA = as.integer(),
  iso3c = as.character(),
  SideA = as.character(),
  SideB = as.character(),
  Intnl = as.integer(),
  # StartYr1 = as.integer(),
  # EndYr1 = as.integer(),
  WDuratDays = as.integer(),
  WDuratMo = as.numeric(),
  TotNatMonWar = as.numeric(),
  TransFrom = as.integer(),
  Initiator = as.character(),
  Outcome = as.integer(),
  TransTo = as.numeric(),
  DeathsSideA = as.integer(),
  DeathsSideB = as.integer(),
  TotalBDeaths = as.integer(),
  year = as.integer(),
  conflictInst = as.integer(),
  maxYr = as.integer()
)

cow_data_to_combine2 <- cow_data_to_combine1
cow_data_to_combine3 <- cow_data_to_combine1
cow_data_to_combine4 <- cow_data_to_combine1

# list of conflicts (according to number)
conflict_list_pt1 <- cow_conflict_pt1 %>%
  dplyr::select(WarNum,StartYr1,EndYr1) %>%
  dplyr::filter(StartYr1 != -8) %>%
  dplyr::pull(WarNum) %>%
  unique()
conflict_list_pt2 <- cow_conflict_pt2 %>%
  dplyr::select(WarNum,StartYr2,EndYr2) %>%
  dplyr::filter(StartYr2 != -8) %>%
  dplyr::pull(WarNum) %>%
  unique()
conflict_list_pt3 <- cow_conflict_pt3 %>%
  dplyr::select(WarNum,StartYr3,EndYr3) %>%
  dplyr::filter(StartYr3 != -8) %>%
  dplyr::pull(WarNum) %>%
  unique()
conflict_list_pt4 <- cow_conflict_pt4 %>%
  dplyr::select(WarNum,StartYr4,EndYr4) %>%
  dplyr::filter(StartYr4 != -8) %>%
  dplyr::pull(WarNum) %>%
  unique()

for(i in conflict_list_pt1){
  
  df <- cow_conflict_pt1 %>%
    dplyr::filter(WarNum == i)
  
  years <- c(df$StartYr1:df$EndYr1)

for(j in min(years):max(years)){
  
  cow_data_to_combine1 <- cow_data_to_combine1 %>%
    tibble::add_row(
      WarNum = df$WarNum,
      WarName = df$WarName,
      V5RegionNum = df$V5RegionNum,
      WarType = df$WarType,
      CcodeA = df$CcodeA,
      iso3c = df$iso3c,
      SideA = df$SideA,
      SideB = df$SideB,
      Intnl = df$Intnl,
      # StartYr1 = as.integer(),
      # EndYr1 = as.integer(),
      WDuratDays = df$WDuratDays,
      WDuratMo = df$WDuratMo,
      TotNatMonWar = df$TotNatMonWar,
      TransFrom = df$TransFrom,
      Initiator = df$Initiator,
      Outcome = df$Outcome,
      TransTo = df$TransTo,
      DeathsSideA = df$DeathsSideA,
      DeathsSideB = df$DeathsSideB,
      TotalBDeaths = df$TotalBDeaths,
      year = j,
      conflictInst = 1,
      maxYr = max(years)
    )
  
}}

  
# part 2 of wars
i = 1
j = 1
for(i in conflict_list_pt2){
  
  df <- cow_conflict_pt2 %>%
    dplyr::filter(WarNum == i)
  
  years <- c(df$StartYr2:df$EndYr2)
  
  for(j in min(years):max(years)){
    
    cow_data_to_combine2 <- cow_data_to_combine2 %>%
      tibble::add_row(
        WarNum = df$WarNum,
        WarName = df$WarName,
        V5RegionNum = df$V5RegionNum,
        WarType = df$WarType,
        CcodeA = df$CcodeA,
        iso3c = df$iso3c,
        SideA = df$SideA,
        SideB = df$SideB,
        Intnl = df$Intnl,
        # StartYr1 = as.integer(),
        # EndYr1 = as.integer(),
        WDuratDays = df$WDuratDays,
        WDuratMo = df$WDuratMo,
        TotNatMonWar = df$TotNatMonWar,
        TransFrom = df$TransFrom,
        Initiator = df$Initiator,
        Outcome = df$Outcome,
        TransTo = df$TransTo,
        DeathsSideA = df$DeathsSideA,
        DeathsSideB = df$DeathsSideB,
        TotalBDeaths = df$TotalBDeaths,
        year = j,
        conflictInst = 2,
        maxYr = max(years)
      )
    
  }}


# part 3 of wars
i = 1
j = 1
for(i in conflict_list_pt3){
  
  df <- cow_conflict_pt3 %>%
    dplyr::filter(WarNum == i)
  
  years <- c(df$StartYr3:df$EndYr3)
  
  for(j in min(years):max(years)){
    
    cow_data_to_combine3 <- cow_data_to_combine3 %>%
      tibble::add_row(
        WarNum = df$WarNum,
        WarName = df$WarName,
        V5RegionNum = df$V5RegionNum,
        WarType = df$WarType,
        CcodeA = df$CcodeA,
        iso3c = df$iso3c,
        SideA = df$SideA,
        SideB = df$SideB,
        Intnl = df$Intnl,
        # StartYr1 = as.integer(),
        # EndYr1 = as.integer(),
        WDuratDays = df$WDuratDays,
        WDuratMo = df$WDuratMo,
        TotNatMonWar = df$TotNatMonWar,
        TransFrom = df$TransFrom,
        Initiator = df$Initiator,
        Outcome = df$Outcome,
        TransTo = df$TransTo,
        DeathsSideA = df$DeathsSideA,
        DeathsSideB = df$DeathsSideB,
        TotalBDeaths = df$TotalBDeaths,
        year = j,
        conflictInst = 3,
        maxYr = max(years)
      )
    
  }}

# part 4 of wars
i = 1
j = 1
for(i in conflict_list_pt4){
  
  df <- cow_conflict_pt4 %>%
    dplyr::filter(WarNum == i)
  
  years <- c(df$StartYr4:df$EndYr4)
  
  for(j in min(years):max(years)){
    
    cow_data_to_combine4 <- cow_data_to_combine4 %>%
      tibble::add_row(
        WarNum = df$WarNum,
        WarName = df$WarName,
        V5RegionNum = df$V5RegionNum,
        WarType = df$WarType,
        CcodeA = df$CcodeA,
        iso3c = df$iso3c,
        SideA = df$SideA,
        SideB = df$SideB,
        Intnl = df$Intnl,
        # StartYr1 = as.integer(),
        # EndYr1 = as.integer(),
        WDuratDays = df$WDuratDays,
        WDuratMo = df$WDuratMo,
        TotNatMonWar = df$TotNatMonWar,
        TransFrom = df$TransFrom,
        Initiator = df$Initiator,
        Outcome = df$Outcome,
        TransTo = df$TransTo,
        DeathsSideA = df$DeathsSideA,
        DeathsSideB = df$DeathsSideB,
        TotalBDeaths = df$TotalBDeaths,
        year = j,
        conflictInst = 4,
        maxYr = max(years)
      )
    
  }}

cow_conflict_data_by_year <- rbind(cow_data_to_combine1,cow_data_to_combine2,
                                   cow_data_to_combine3,cow_data_to_combine4)

cow2 <- cow_conflict_data_by_year %>%
  dplyr::filter(maxYr >= 1945)
