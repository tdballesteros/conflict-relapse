
### load libraries ---------------------------------------------------------------------------------
library(countrycode)
library(survival)
library(ggsurvfit)
# library(survivalAnalysis)
library(dplyr)
library(tidyr)

### load data --------------------------------------------------------------------------------------
country_regions1 <- read.csv("Data files/Formatted data files/country_regions1.csv")
country_regions2 <- read.csv("Data files/Formatted data files/country_regions2.csv")
country_regions3 <- read.csv("Data files/Formatted data files/country_regions3.csv")
colonialism <- read.csv("Data files/Formatted data files/colonialism.csv")
gdp <- read.csv("Data files/Formatted data files/gdp.csv")
population <- read.csv("Data files/Formatted data files/population.csv")
gdppc <- read.csv("Data files/Formatted data files/gdppc.csv")
polity <- read.csv("Data files/Formatted data files/polity.csv")
pko <- read.csv("Data files/Formatted data files/peacekeeping_operations.csv")
ppi <- read.csv("Data files/Formatted data files/positive_peace.csv")
pop_density <- read.csv("Data files/Formatted data files/population_density.csv")
vdem_hl <- read.csv("Data files/Formatted data files/vdem_hl_index.csv")
elec <- read.csv("Data files/Formatted data files/elections.csv")
aid <- read.csv("Data files/Formatted data files/aid.csv")
tax_revenue <- read.csv("Data files/Formatted data files/tax_revenue.csv")
trade_volume <- read.csv("Data files/Formatted data files/trade_volume.csv")
shadow_economy <- read.csv("Data files/Formatted data files/shadow_economy.csv")
energy_and_steel <- read.csv("Data files/Formatted data files/energy_and_steel.csv")
ethnic_fractionalization <- read.csv("Data files/Formatted data files/ethnic_fractionalization.csv")
oil <- read.csv("Data files/Formatted data files/oil.csv")
bureaucratic_capacity <- read.csv("Data files/Formatted data files/icrg_bureaucratic_capacity.csv")

military_capacity_ccpu <- read.csv("Data files/Formatted data files/military_capacity_index_ccpu.csv")
fiscal_capacity <- read.csv("Data files/Formatted data files/fiscal_capacity_index.csv")

conflict_variables <- read.csv("Data files/Formatted data files/conflict_variables.csv")
conflict_table <- read.csv("Data files/Formatted data files/conflict_table.csv")
conflict_years <- read.csv("Data files/Formatted data files/conflict_years.csv")
conflict_issues <- read.csv("Data files/Formatted data files/conflict_issues.csv")


### format data ------------------------------------------------------------------------------------

# COLONIALISM
# variables: colony of GBR, colony of ESP, colony of FRA, colony of other, not colonized
colonialism <- colonialism %>%
  dplyr::mutate(colony_other = colony_nld + colony_ita + colony_bel + colony_usa + colony_other,
                colony_never = 1 - (colony_gbr + colony_esp + colony_fra + colony_prt +
                                      colony_other)) %>%
  dplyr::select(iso3c, colony, colony_gbr, colony_esp, colony_fra, colony_prt, colony_other,
                colony_never)

# REGION
country_region <- country_regions3 %>%
  dplyr::select(iso3c, region1) %>%
  dplyr::mutate(value = 1) %>%
  tidyr::pivot_wider(names_from = region1, values_from = value) %>%
  replace(is.na(.), 0)

# POPULATION
population <- population %>%
  dplyr::select(iso3c, yrstart = year, un.pop, cow.pop) %>%
  dplyr::mutate(
    un.pop.log = log(un.pop),
    cow.pop.log = log(cow.pop)
  )

# SOV fixes
population <- population %>%
  dplyr::filter(iso3c %!in% c("EST", "LVA", "LTU", "BLR", "UKR", "MDA", "RUS", "GEO", "ARM",
                              "AZE", "KAZ", "KGZ", "TJK", "TKM", "UZB") | yrstart >= 1991) %>%
  rbind(
    population %>%
      dplyr::filter(iso3c == "SOV" & yrstart %in% c(1946:1990)) %>%
      dplyr::mutate(iso3c = "EST"),
    population %>%
      dplyr::filter(iso3c == "SOV" & yrstart %in% c(1946:1990)) %>%
      dplyr::mutate(iso3c = "LVA"),
    population %>%
      dplyr::filter(iso3c == "SOV" & yrstart %in% c(1946:1990)) %>%
      dplyr::mutate(iso3c = "LTU"),
    population %>%
      dplyr::filter(iso3c == "SOV" & yrstart %in% c(1946:1990)) %>%
      dplyr::mutate(iso3c = "BLR"),
    population %>%
      dplyr::filter(iso3c == "SOV" & yrstart %in% c(1946:1990)) %>%
      dplyr::mutate(iso3c = "UKR"),
    population %>%
      dplyr::filter(iso3c == "SOV" & yrstart %in% c(1946:1990)) %>%
      dplyr::mutate(iso3c = "MDA"),
    population %>%
      dplyr::filter(iso3c == "SOV" & yrstart %in% c(1946:1990)) %>%
      dplyr::mutate(iso3c = "RUS"),
    population %>%
      dplyr::filter(iso3c == "SOV" & yrstart %in% c(1946:1990)) %>%
      dplyr::mutate(iso3c = "GEO"),
    population %>%
      dplyr::filter(iso3c == "SOV" & yrstart %in% c(1946:1990)) %>%
      dplyr::mutate(iso3c = "ARM"),
    population %>%
      dplyr::filter(iso3c == "SOV" & yrstart %in% c(1946:1990)) %>%
      dplyr::mutate(iso3c = "AZE"),
    population %>%
      dplyr::filter(iso3c == "SOV" & yrstart %in% c(1946:1990)) %>%
      dplyr::mutate(iso3c = "KAZ"),
    population %>%
      dplyr::filter(iso3c == "SOV" & yrstart %in% c(1946:1990)) %>%
      dplyr::mutate(iso3c = "KGZ"),
    population %>%
      dplyr::filter(iso3c == "SOV" & yrstart %in% c(1946:1990)) %>%
      dplyr::mutate(iso3c = "TJK"),
    population %>%
      dplyr::filter(iso3c == "SOV" & yrstart %in% c(1946:1990)) %>%
      dplyr::mutate(iso3c = "TKM"),
    population %>%
      dplyr::filter(iso3c == "SOV" & yrstart %in% c(1946:1990)) %>%
      dplyr::mutate(iso3c = "UZB")
  )

# GDP
gdp <- gdp %>%
  dplyr::rename(yrstart = year)

# GDP PER CAPITA
gdppc <- gdppc %>%
  dplyr::select(iso3c, yrstart = year, gdppc.pwt.un, gdppc.pwt.cow, gdppc.gl.un, gdppc.gl.cow) %>%
  dplyr::filter(!is.na(gdppc.pwt.un) & !is.na(gdppc.pwt.cow) & !is.na(gdppc.gl.un) & !is.na(gdppc.gl.cow))

# POLITY
polity <- polity %>%
  dplyr::select(-country) %>%
  dplyr::rename(yrstart = year)

# VDEM
vdem_hl <- vdem_hl %>%
  dplyr::rename(yrstart = year)

# PKO
pko <- pko %>%
  dplyr::select(iso3c, yrstart = year, pko_mission)

# POPULATION DENSITY
pop_density <- pop_density %>%
  dplyr::rename(yrstart = year)

# ELECTIONS
elec <- elec %>%
  dplyr::select(-country) %>%
  dplyr::rename(yrstart = year)

# AID
aid <- aid %>%
  dplyr::rename(yrstart = year)

# code SOV aid as $0
aid$aid_value[aid$iso3c == "SOV"] <- 0

# TAX REVENUE
tax_revenue <- tax_revenue %>%
  dplyr::select(-country) %>%
  dplyr::rename(yrstart = year)

# TRADE VOLUME
trade_volume <- trade_volume %>%
  dplyr::rename(yrstart = year)

# SHADOW ECONOMY
shadow_economy <- shadow_economy %>%
  dplyr::rename(yrstart = year)

# ENERGY AND STEEL
energy_and_steel <- energy_and_steel %>%
  dplyr::rename(yrstart = year)

# ETHNIC FRACTIONALIZATION
ethnic_fractionalization <- ethnic_fractionalization %>%
  dplyr::rename(yrstart = year)

# OIL
oil <- oil %>%
  dplyr::rename(yrstart = year)

# BUREAU CAP
bureaucratic_capacity <- bureaucratic_capacity %>%
  dplyr::rename(yrstart = year)

# MILITARY CAPACITY
military_capacity_ccpu <- military_capacity_ccpu %>%
  dplyr::rename(yrstart = year)

# FISCAL CAPACITY
fiscal_capacity <- fiscal_capacity %>%
  dplyr::rename(yrstart = year)

# CONFLICT VARIABLES
cv <- conflict_variables %>%
  dplyr::rename(yrstart = year)

# CONFLICT TABLE
ct <- conflict_table %>%
  dplyr::filter(conflict == 0) %>%
  dplyr::mutate(ongoing_peace = ifelse(yrend == 2019, 1, 0)) %>%
  dplyr::group_by(confid, iso3c) %>%
  dplyr::mutate(id = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(uniqueid = paste0(confid, iso3c, id)) %>%
  dplyr::group_by(confid, conflict) %>%
  dplyr::arrange(yrstart) %>%
  dplyr::mutate(episode = row_number()) %>%
  dplyr::ungroup()

# CONFLICT ISSUES
conflict_issues <- conflict_issues %>%
  dplyr::rename(yrstart = year)

# VARS:
# 1 - Length of conflict, in years [current episode of conflict only]
## 1a - Length of conflict, in years [since initial outbreak across all episodes]
# 2 - Episode number
# 3 - Colony of GBR
# 4 - Colony of ESP
# 5 - Colony of FRA
# 6 - Colony of PRT
# 7 - Colony of other power
# 7a - Not colonized
# 8 - country region
# 8a - country subregion
# 9 - population at end of conflict, log scale
# 10 - gdp per capita at end of conflict
# 11 - change in gdp per capita between start and end of conflict
# 12 - polity score at end of conflict
# 13 - change in polity score between start and end of conflict
# 14 - ppi
# 15 - deaths [current episode of conflict only]
# 15a - deaths per 100,000 population
# 16 - population density
# 16a - country area

# merge data
merge_df <- gdp %>%
  dplyr::left_join(population, by = c("iso3c", "yrstart")) %>%
  dplyr::left_join(gdppc, by = c("iso3c", "yrstart")) %>%
  dplyr::left_join(polity, by = c("iso3c", "yrstart")) %>%
  dplyr::left_join(vdem_hl, by = c("iso3c", "yrstart")) %>%
  dplyr::left_join(pko, by = c("iso3c", "yrstart")) %>%
  dplyr::left_join(pop_density, by = c("iso3c", "yrstart")) %>%
  dplyr::left_join(elec, by = c("iso3c", "yrstart")) %>%
  dplyr::left_join(aid, by = c("iso3c", "yrstart")) %>%
  # dplyr::left_join(tax_revenue, by = c("iso3c", "yrstart")) %>%
  dplyr::left_join(trade_volume, by = c("iso3c", "yrstart")) %>%
  dplyr::left_join(shadow_economy, by = c("iso3c", "yrstart")) %>%
  dplyr::left_join(energy_and_steel, by = c("iso3c", "yrstart")) %>%
  dplyr::left_join(ethnic_fractionalization, by = c("iso3c", "yrstart")) %>%
  dplyr::left_join(oil, by = c("iso3c", "yrstart")) %>%
  dplyr::left_join(bureaucratic_capacity, by = c("iso3c", "yrstart")) %>%
  dplyr::left_join(military_capacity_ccpu, by = c("iso3c", "yrstart")) # %>%
  # dplyr::left_join(fiscal_capacity, by = c("iso3c", "yrstart"))

x <- merge_df %>%
  dplyr::group_by(iso3c, yrstart) %>%
  dplyr::tally()

# append SOV duplicates for post-Soviet countries using the iso3c code for data 1946-1990
merge_df_sov <- merge_df %>%
  dplyr::filter(
    iso3c == "SOV",
    yrstart <= 1990
    )

merge_df <- merge_df %>%
  dplyr::filter(
    iso3c %!in% c("ARM", "AZE", "BLR", "EST", "GEO", "KAZ", "KGZ", "LTU", "LVA", "MDA", "RUS",
                  "TJK", "TKM", "UKR", "UZB") | yrstart >= 1991
  ) %>%
  rbind(merge_df_sov %>%
          dplyr::mutate(iso3c = "ARM")) %>%
  rbind(merge_df_sov %>%
          dplyr::mutate(iso3c = "AZE")) %>%
  rbind(merge_df_sov %>%
          dplyr::mutate(iso3c = "BLR")) %>%
  rbind(merge_df_sov %>%
          dplyr::mutate(iso3c = "EST")) %>%
  rbind(merge_df_sov %>%
          dplyr::mutate(iso3c = "GEO")) %>%
  rbind(merge_df_sov %>%
          dplyr::mutate(iso3c = "KAZ")) %>%
  rbind(merge_df_sov %>%
          dplyr::mutate(iso3c = "KGZ")) %>%
  rbind(merge_df_sov %>%
          dplyr::mutate(iso3c = "LTU")) %>%
  rbind(merge_df_sov %>%
          dplyr::mutate(iso3c = "LVA")) %>%
  rbind(merge_df_sov %>%
          dplyr::mutate(iso3c = "MDA")) %>%
  rbind(merge_df_sov %>%
          dplyr::mutate(iso3c = "RUS")) %>%
  rbind(merge_df_sov %>%
          dplyr::mutate(iso3c = "TJK")) %>%
  rbind(merge_df_sov %>%
          dplyr::mutate(iso3c = "TKM")) %>%
  rbind(merge_df_sov %>%
          dplyr::mutate(iso3c = "UKR")) %>%
  rbind(merge_df_sov %>%
          dplyr::mutate(iso3c = "UZB"))

# append YEM duplicates for North and South Yemen using the iso3c code for data 1946-1990
merge_df_yem <- merge_df %>%
  dplyr::filter(
    iso3c == "YEM",
    yrstart > 1990
  )

merge_df <- merge_df %>%
  dplyr::filter(iso3c != "YEM",
                iso3c %!in% c("YAR", "YPR") | yrstart <= 1990) %>%
  rbind(merge_df_yem %>%
          dplyr::mutate(iso3c = "YAR")) %>%
  rbind(merge_df_yem %>%
          dplyr::mutate(iso3c = "YPR"))

survival_df <- ct %>%
  dplyr::left_join(country_region, by = c("iso3c")) %>%
  dplyr::left_join(colonialism, by = c("iso3c")) %>%
  dplyr::left_join(merge_df, by = c("iso3c", "yrstart")) %>%
  dplyr::left_join(cv, by = c("confid", "yrstart", "iso3c")) %>%
  dplyr::left_join(conflict_issues, by = c("confid", "yrstart")) %>%
  dplyr::mutate(
    aid_perc_gdp.pwt = aid_value / gdp.pwt.est,
    aid_perc_gdp.gl = aid_value / gdp.gl.est,
    aid_per_capita.un = aid_value / un.pop,
    aid_per_capita.cow = aid_value / cow.pop
    ) %>%
  # drop OMN's 258 conflict as it occurred one year in 1958
  dplyr::filter(confid != 258)
  

# table(survival_df$yrstart >= 1989, survival_df$issue_l1_justice, useNA = "always")
# 
# 
# sdf <- survival_df %>%
#   dplyr::select(confid, yrstart)
# 
# ci <- conflict_issues %>%
#   dplyr::select(confid, yrstart, issue_l1_justice)
# 
# sdf_ci <- dplyr::full_join(sdf, ci, by = c("confid", "yrstart"))
# 
# table(sdf_ci$issue_l1_justice, useNA = "always")


# create model -------------------------------------------------------------------------------------

cor(survival_df$vdem.hl, survival_df$vdem.hl.sq, use = "pairwise.complete.obs")
# plot(survival_df$vdem.hl, survival_df$vdem.hl.sq)

survival_model_all_years_vars <- c(
  "Americas", "Asia", "SSA", "MENA", "colony_gbr", "colony_esp", "colony_fra", "colony_prt", "colony_other", "episode",
  "un.pop.log", "gdppc.pwt.un", "conf_length", "vdem.hl", "vdem.hl.sq", "binary_neigh_conf", "other_conf",
  "outcome", "mil.cap", "pko_mission", "pop.per.km.un", "natelec", "years_since_last_elec", "aid_perc_gdp.pwt",
  "total.trade.perc.gdp.pwt", "land_area", "shec", "irst_per_capita_un", "trade.balance.perc.total.trade",
  "ethf"
  )

survival_model_modern_vars <- c("exporter")

surv_model_df_missing <- survival_df %>%
  dplyr::mutate(across(everything(), as.character)) %>%
  dplyr::select(iso3c, yrstart, dplyr::all_of(survival_model_all_years_vars)) %>%
  tidyr::pivot_longer(3:(length(survival_model_all_years_vars) + 2), names_to = "variable",
                      values_to = "value") %>%
  dplyr::filter(is.na(value))

surv_df_missing <- surv_model_df_missing %>%
  dplyr::group_by(iso3c, yrstart) %>%
  dplyr::tally()

surv_model1 <- survival::coxph(survival::Surv(length, ongoing_peace) ~ Americas + Asia + SSA +
                                 MENA + colony_gbr + colony_esp + colony_fra + colony_prt +
                                 colony_other + episode + un.pop.log + gdppc.pwt.un + conf_length +
                                 # polity.pca + polity.pca.sq +
                                 vdem.hl + vdem.hl.sq +
                                 binary_neigh_conf + other_conf +
                                 o1_peace_agreement + o2_ceasefire + o3_gov_victory +
                                 o4_nongov_victory + o6_actor_ceases_to_exist +
                                 mil.cap + pko_mission +
                                 pop.per.km.un +  natelec + years_since_last_elec +
                                 aid_perc_gdp.pwt +
                                 # aid_per_capita.un +
                                 # total.trade.perc.gdp.pwt + trade.balance.perc.total.trade +
                                 log(land_area) + shec + irst_per_capita_un, #+ ethf,
                                   data = survival_df)
summary(surv_model1)


surv_model2 <- survival::coxph(survival::Surv(length, ongoing_peace) ~ Americas + Asia + SSA +
                                 MENA + colony_gbr + colony_esp + colony_fra + colony_prt +
                                 colony_other + episode + un.pop.log + gdppc.pwt.un + conf_length +
                                 # polity.pca + polity.pca.sq +
                                 vdem.hl + vdem.hl.sq +
                                 binary_neigh_conf + other_conf +
                                 o1_peace_agreement + o2_ceasefire + o3_gov_victory +
                                 o4_nongov_victory + o6_actor_ceases_to_exist +
                                 mil.cap + pko_mission +
                                 pop.per.km.un +  natelec + years_since_last_elec +
                                 aid_perc_gdp.pwt +
                                 # aid_per_capita.un +
                                 # total.trade.perc.gdp.pwt + trade.balance.perc.total.trade +
                                 log(land_area) + shec + irst_per_capita_un +
                                 
                                 # new vars
                                 exporter + bq + lo + cr,
                                data = survival_df %>%
                                  dplyr::filter(yrstart >= 1980))
summary(surv_model2)


x <- survival_df %>%
  dplyr::filter(is.na(total.trade.perc.gdp.pwt))

### length of peace model --------------------------------------------------------------------------
lop_df <- survival_df %>%
  dplyr::filter(ongoing_peace == 0)

lop_glm <- glm(length ~ Americas + Asia + SSA + MENA + colony_gbr + colony_esp + colony_fra +
                 colony_prt + colony_other + episode + un.pop.log + gdppc.pwt.un + conf_length +
                 # polity.pca + polity.pca.sq +
                 vdem.hl + vdem.hl.sq +
                 binary_neigh_conf + other_conf + o1_peace_agreement + o2_ceasefire +
                 o3_gov_victory + o4_nongov_victory + o6_actor_ceases_to_exist + mil.cap +
                 pko_mission + pop.per.km.un +  natelec + years_since_last_elec + aid_perc_gdp.pwt +
                 total.trade.perc.gdp.pwt + log(land_area) + shec + irst_per_capita_un +
                 trade.balance.perc.total.trade + ethf,
               data = lop_df)
summary(lop_glm)



# survival_df <- conflict_table %>%
#   dplyr::filter(conflict == 0) %>%
#   dplyr::mutate(ongoing_peace = ifelse(yrend == 2019, 1, 0)) %>%
#   dplyr::group_by(confid,iso3c) %>%
#   dplyr::mutate(id = dplyr::row_number()) %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(uniqueid = paste0(confid,iso3c,id)) %>%
#   dplyr::rename(year = yrend) %>%
#   dplyr::left_join(country_regions1,by=c("iso3c")) %>%
#   dplyr::left_join(country_regions2,by=c("iso3c")) %>%
#   dplyr::left_join(country_regions3,by=c("iso3c")) %>%
#   dplyr::left_join(colonialism,by=c("iso3c")) %>%
#   dplyr::left_join(population,by=c("iso3c","year")) %>%
#   dplyr::left_join(gdppc,by=c("iso3c","year")) %>%
#   dplyr::left_join(polity,by=c("iso3c","year")) %>%
#   dplyr::left_join(ppi,by=c("iso3c","year")) %>%
#   dplyr::mutate(
#     ln.un.pop = log(un.pop),
#     ln.cow.pop = log(cow.pop)
#   )
  
# survival2 <- ggsurvfit::survfit2(survival::Surv(length, ongoing_peace) ~ gdppc.pwt.un, data = survival_df)
# summary(survival2)
# 
# survival2 %>% 
#   ggsurvfit() +
#   labs(
#     x = "years",
#     y = "Overall survival probability"
#   )

survival3 <- survivalAnalysis::analyse_multivariate(
  data = survival_df,
  time_status = vars(length, ongoing_peace),
  covariates = vars(gdppc.pwt.un,polity.pca,colony,ln.un.pop,region1)
                    #Acceptance.of.the.Rights.of.Others,Equitable.Distribution.of.Resources,Free.Flow.of.Information,High.Levels.of.Human.Capital,Low.Levels.of.Corruption,Well.Functioning.Government)
)
survival3
