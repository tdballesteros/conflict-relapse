
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
icrg_bureaucracy <- read.csv("Data files/Formatted data files/icrg_bureaucratic_capacity.csv")
battle_deaths <- read.csv("Data files/Formatted data files/battle_related_deaths.csv")

military_capacity_ccpu <- read.csv("Data files/Formatted data files/military_capacity_index_ccpu.csv")
fiscal_capacity <- read.csv("Data files/Formatted data files/fiscal_capacity_index.csv")
bureaucratic_capacity <- read.csv("Data files/Formatted data files/bureaucratic_capacity_index.csv")

conflict_names <- read.csv("Data files/Formatted data files/conflict_names.csv")
conflict_variables <- read.csv("Data files/Formatted data files/conflict_variables.csv")
conflict_table <- read.csv("Data files/Formatted data files/conflict_table.csv")
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
  dplyr::select(iso3c, year, un.pop, cow.pop) %>%
  dplyr::mutate(
    un.pop.log = log(un.pop),
    cow.pop.log = log(cow.pop)
  )

# SOV fixes
population <- population %>%
  dplyr::filter(iso3c %!in% c("EST", "LVA", "LTU", "BLR", "UKR", "MDA", "RUS", "GEO", "ARM",
                              "AZE", "KAZ", "KGZ", "TJK", "TKM", "UZB") | year >= 1991) %>%
  rbind(
    population %>%
      dplyr::filter(iso3c == "SOV" & year %in% c(1946:1990)) %>%
      dplyr::mutate(iso3c = "EST"),
    population %>%
      dplyr::filter(iso3c == "SOV" & year %in% c(1946:1990)) %>%
      dplyr::mutate(iso3c = "LVA"),
    population %>%
      dplyr::filter(iso3c == "SOV" & year %in% c(1946:1990)) %>%
      dplyr::mutate(iso3c = "LTU"),
    population %>%
      dplyr::filter(iso3c == "SOV" & year %in% c(1946:1990)) %>%
      dplyr::mutate(iso3c = "BLR"),
    population %>%
      dplyr::filter(iso3c == "SOV" & year %in% c(1946:1990)) %>%
      dplyr::mutate(iso3c = "UKR"),
    population %>%
      dplyr::filter(iso3c == "SOV" & year %in% c(1946:1990)) %>%
      dplyr::mutate(iso3c = "MDA"),
    population %>%
      dplyr::filter(iso3c == "SOV" & year %in% c(1946:1990)) %>%
      dplyr::mutate(iso3c = "RUS"),
    population %>%
      dplyr::filter(iso3c == "SOV" & year %in% c(1946:1990)) %>%
      dplyr::mutate(iso3c = "GEO"),
    population %>%
      dplyr::filter(iso3c == "SOV" & year %in% c(1946:1990)) %>%
      dplyr::mutate(iso3c = "ARM"),
    population %>%
      dplyr::filter(iso3c == "SOV" & year %in% c(1946:1990)) %>%
      dplyr::mutate(iso3c = "AZE"),
    population %>%
      dplyr::filter(iso3c == "SOV" & year %in% c(1946:1990)) %>%
      dplyr::mutate(iso3c = "KAZ"),
    population %>%
      dplyr::filter(iso3c == "SOV" & year %in% c(1946:1990)) %>%
      dplyr::mutate(iso3c = "KGZ"),
    population %>%
      dplyr::filter(iso3c == "SOV" & year %in% c(1946:1990)) %>%
      dplyr::mutate(iso3c = "TJK"),
    population %>%
      dplyr::filter(iso3c == "SOV" & year %in% c(1946:1990)) %>%
      dplyr::mutate(iso3c = "TKM"),
    population %>%
      dplyr::filter(iso3c == "SOV" & year %in% c(1946:1990)) %>%
      dplyr::mutate(iso3c = "UZB")
  )

# GDP
# gdp <- gdp %>%
#   dplyr::rename(start_year = year)

# GDP PER CAPITA
gdppc <- gdppc %>%
  dplyr::select(iso3c, year, gdppc.pwt.un, gdppc.pwt.cow, gdppc.gl.un, gdppc.gl.cow) %>%
  dplyr::filter(!is.na(gdppc.pwt.un) & !is.na(gdppc.pwt.cow) & !is.na(gdppc.gl.un) & !is.na(gdppc.gl.cow))

# POLITY
polity <- polity %>%
  dplyr::select(-country) # %>%
  # dplyr::rename(start_year = year)

# VDEM
# vdem_hl <- vdem_hl %>%
#   dplyr::rename(start_year = year)

# PKO
pko <- pko %>%
  dplyr::select(iso3c, year, pko_mission)

# POPULATION DENSITY
# pop_density <- pop_density %>%
#   dplyr::rename(start_year = year)

# ELECTIONS
elec <- elec %>%
  dplyr::select(-country) # %>%
  # dplyr::rename(start_year = year)

# AID
# aid <- aid %>%
#   dplyr::rename(start_year = year)

# code SOV aid as $0
aid$aid_value[aid$iso3c == "SOV"] <- 0

# TAX REVENUE
tax_revenue <- tax_revenue %>%
  dplyr::select(-country) # %>%
  # dplyr::rename(start_year = year)

# TRADE VOLUME
# trade_volume <- trade_volume %>%
#   dplyr::rename(start_year = year)

# SHADOW ECONOMY
# shadow_economy <- shadow_economy %>%
#   dplyr::rename(start_year = year)

# ENERGY AND STEEL
# energy_and_steel <- energy_and_steel %>%
#   dplyr::rename(start_year = year)

# ETHNIC FRACTIONALIZATION
# ethnic_fractionalization <- ethnic_fractionalization %>%
#   dplyr::rename(start_year = year)

# OIL
# oil <- oil %>%
#   dplyr::rename(start_year = year)

# BUREAU CAP
# icrg_bureaucracy <- icrg_bureaucracy %>%
#   dplyr::rename(start_year = year)

# MILITARY CAPACITY
# military_capacity_ccpu <- military_capacity_ccpu %>%
#   dplyr::rename(start_year = year)

# FISCAL CAPACITY
# fiscal_capacity <- fiscal_capacity %>%
#   dplyr::rename(start_year = year)

# CONFLICT VARIABLES
# cv <- conflict_variables %>%
#   dplyr::rename(start_year = year)

# CONFLICT TABLE
ct <- conflict_table %>%
  dplyr::filter(
    conflict == 0,
    group_id != 1
    ) %>%
  dplyr::rename(year = start_year) %>%
  dplyr::mutate(ongoing_peace = ifelse(end_year == 2019, 1, 0)) %>%
  dplyr::group_by(confid, iso3c) %>%
  dplyr::mutate(id = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(uniqueid = paste0(confid, iso3c, id)) %>%
  dplyr::group_by(confid, conflict) %>%
  dplyr::arrange(year) %>%
  dplyr::mutate(episode = row_number()) %>%
  dplyr::ungroup() %>%
  dplyr::full_join(conflict_names, by = "confid")

# CONFLICT ISSUES
# conflict_issues <- conflict_issues %>%
#   dplyr::rename(start_year = year)

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
  dplyr::left_join(population, by = c("iso3c", "year")) %>%
  dplyr::left_join(gdppc, by = c("iso3c", "year")) %>%
  dplyr::left_join(polity, by = c("iso3c", "year")) %>%
  dplyr::left_join(vdem_hl, by = c("iso3c", "year")) %>%
  dplyr::left_join(pko, by = c("iso3c", "year")) %>%
  dplyr::left_join(pop_density, by = c("iso3c", "year")) %>%
  dplyr::left_join(elec, by = c("iso3c", "year")) %>%
  dplyr::left_join(aid, by = c("iso3c", "year")) %>%
  # dplyr::left_join(tax_revenue, by = c("iso3c", "year")) %>%
  dplyr::left_join(trade_volume, by = c("iso3c", "year")) %>%
  dplyr::left_join(shadow_economy, by = c("iso3c", "year")) %>%
  dplyr::left_join(energy_and_steel, by = c("iso3c", "year")) %>%
  dplyr::left_join(ethnic_fractionalization, by = c("iso3c", "year")) %>%
  dplyr::left_join(oil, by = c("iso3c", "year")) %>%
  dplyr::left_join(icrg_bureaucracy, by = c("iso3c", "year")) %>%
  dplyr::left_join(military_capacity_ccpu, by = c("iso3c", "year")) %>%
  dplyr::left_join(bureaucratic_capacity, by = c("iso3c", "year")) # %>%
  # dplyr::left_join(fiscal_capacity, by = c("iso3c", "year"))

# x <- merge_df %>%
#   dplyr::group_by(iso3c, year) %>%
#   dplyr::tally()

# append SOV duplicates for post-Soviet countries using the iso3c code for data 1946-1990
merge_df_sov <- merge_df %>%
  dplyr::filter(
    iso3c == "SOV",
    year <= 1990
    )

merge_df <- merge_df %>%
  dplyr::filter(
    iso3c %!in% c("ARM", "AZE", "BLR", "EST", "GEO", "KAZ", "KGZ", "LTU", "LVA", "MDA", "RUS",
                  "TJK", "TKM", "UKR", "UZB") | year >= 1991
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
    year > 1990
  )

merge_df <- merge_df %>%
  dplyr::filter(iso3c != "YEM",
                iso3c %!in% c("YAR", "YPR") | year <= 1990) %>%
  rbind(merge_df_yem %>%
          dplyr::mutate(iso3c = "YAR")) %>%
  rbind(merge_df_yem %>%
          dplyr::mutate(iso3c = "YPR"))

survival_df <- ct %>%
  dplyr::left_join(country_region, by = c("iso3c")) %>%
  dplyr::left_join(colonialism, by = c("iso3c")) %>%
  dplyr::left_join(merge_df, by = c("iso3c", "year")) %>%
  dplyr::left_join(conflict_variables, by = c("confid", "year", "iso3c")) %>%
  dplyr::left_join(conflict_issues, by = c("confid", "iso3c", "year" = "peace_start_year")) %>%
  dplyr::left_join(battle_deaths, by = c("confid" = "conflict_id", "group_id")) %>%
  
  dplyr::mutate(
    aid_perc_gdp.pwt = aid_value / gdp.pwt.est,
    aid_perc_gdp.gl = aid_value / gdp.gl.est,
    aid_per_capita.un = aid_value / un.pop,
    aid_per_capita.cow = aid_value / cow.pop
    ) %>%
  # drop OMN's 258 conflict as it occurred one year in 1958
  dplyr::filter(confid != 258)

# table(survival_df$year >= 1989, survival_df$issue_l1_justice, useNA = "always")
# 
# 
# sdf <- survival_df %>%
#   dplyr::select(confid, year)
# 
# ci <- conflict_issues %>%
#   dplyr::select(confid, year, issue_l1_justice)
# 
# sdf_ci <- dplyr::full_join(sdf, ci, by = c("confid", "year"))
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
  dplyr::select(iso3c, year, dplyr::all_of(survival_model_all_years_vars)) %>%
  tidyr::pivot_longer(3:(length(survival_model_all_years_vars) + 2), names_to = "variable",
                      values_to = "value") %>%
  dplyr::filter(is.na(value))

surv_df_missing <- surv_model_df_missing %>%
  dplyr::group_by(iso3c, year) %>%
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
                                 total.trade.perc.gdp.pwt + trade.balance.perc.total.trade +
                                 log(land_area) + shec + irst_per_capita_un + ethf,
                                   data = survival_df)
summary(surv_model1)

# pull conflicts currently in a state of peace
current_peace_survival_df <- survival_df %>%
  dplyr::group_by(confid) %>%
  dplyr::filter(group_id == max(group_id)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(conflict.x == 0)
                  
predictions_surv_model1 <- stats::predict(surv_model1, current_peace_survival_df, type = "risk") %>%
  as.data.frame() %>%
  cbind(current_peace_survival_df$iso3c, current_peace_survival_df$confid, current_peace_survival_df$conf_name)
names(predictions_surv_model1) <- c("prob", "iso3c", "confid", "conf_name")



survival_df2 <- survival_df %>%
  dplyr::filter(year >= 1989)

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
                                 exporter + bur.cap + bd_best,
                                 
                                 ### L1 ###
                                 # issue_l1_territory + # 164 / 60
                                 # issue_l1_state_structure + # 208 / 16
                                 # issue_l1_governance + # 169 / 55
                                 # issue_l1_political_rights + # 193 / 31
                                 # issue_l1_distribution_of_resources +
                                 # issue_l1_foreign_involvement +
                                 # issue_l1_refugees_prisoners +
                                 # issue_l1_negotiations +
                                 # issue_l1_justice,
                                 
                                 ### L2 ###
                                 # issue_l2_subcategory_separatism +
                                 # issue_l2_subcategory_unification +
                                 # issue_l2_subcategory_selfrule +
                                 # issue_l2_admin_arrangements +
                                 # issue_l2_change_political_system +
                                 # issue_l2_executive +
                                 # issue_l2_parliament +
                                 # issue_l2_judicial +
                                 # issue_l2_security_sector +
                                 # issue_l2_bureaucratic +
                                 # issue_l2_quality_of_governance +
                                 # issue_l2_elections +
                                 # issue_l2_civil_rights_and_freedoms +
                                 # issue_l2_religious_rights_and_freedoms +
                                 # issue_l2_cultural_rights_and_freedoms +
                                 # issue_l2_gender_rights_and_freedoms +
                                 # issue_l2_labor_rights_and_freedoms +
                                 # issue_l2_childrens_rights_and_freedoms +
                                 # issue_l2_referendums +
                                 # # issue_l2_state_distribution_systems +
                                 # issue_l2_natural_resources +
                                 # issue_l2_ifi +
                                 # issue_l2_dfi +
                                 # issue_l2_attrocities_and_abuses +
                                 # issue_l2_collective_targeting +
                                 # issue_l2_call_for_collective_targeting +
                                 # issue_l2_military_conduct +
                                 # issue_l2_revenge +
                                 # issue_l2_return_of_refugees +
                                 # issue_l2_release_of_prisoners +
                                 # issue_l2_negotiations +
                                 # # issue_l2_ceasefires +
                                 # issue_l2_peace_agreement +
                                 # issue_l2_truth_reconciliation +
                                 # issue_l2_liability,
                               
                               ### L3 ###
                                 # issue_l3_independence +
                                 # issue_l3_irredentism +
                                 # issue_l3_unification_of_states +
                                 # issue_l3_autonomy +
                                 # issue_l3_federalism +
                                 # issue_l3_confederation_union +
                                 # issue_l3_decentralization +
                                 # issue_l3_change_of_admin_divisions +
                                 # issue_l3_cps_democracy +
                                 # issue_l3_cps_socialism +
                                 # issue_l3_cps_islamic +
                                 # issue_l3_cps_other +
                                 # issue_l3_oust_full_executive +
                                 # issue_l3_oust_head_of_executive +
                                 # issue_l3_reform_executive_structure +
                                 # issue_l3_executive_power_sharing +
                                 # issue_l3_executive_power_sharing_interim +
                                 # issue_l3_oust_local_executive +
                                 # issue_l3_oust_parliament +
                                 # issue_l3_reform_parliament +
                                 # issue_l3_parliamentary_power_sharing +
                                 # issue_l3_change_judicial_system +
                                 # issue_l3_reform_judicial_system +
                                 # issue_l3_constitutional_issues +
                                 # issue_l3_restructure_military_forces +
                                 # issue_l3_restructure_police_forces +
                                 # issue_l3_disband_paramilitary_forces +
                                 # issue_l3_security_sector_power_sharing +
                                 # issue_l3_bureaucratic_setup +
                                 # issue_l3_rule_of_law +
                                 # issue_l3_law_and_order +
                                 # issue_l3_corruption +
                                 # issue_l3_foreign_policy_reform +
                                 # issue_l3_hold_elections +
                                 # issue_l3_opposition_to_elections +
                                 # issue_l3_electoral_reform +
                                 # issue_l3_electoral_fraud +
                                 # issue_l3_civil_rights +
                                 # issue_l3_restriction_on_civil_rights +
                                 # issue_l3_citizenship_reform +
                                 # issue_l3_human_rights +
                                 # issue_l3_freedom_of_expression +
                                 # issue_l3_freedom_of_association +
                                 # issue_l3_freedom_of_movement +
                                 # issue_l3_recognition_as_political_party +
                                 # issue_l3_restrictions_on_freedom_of_expression +
                                 # issue_l3_religious_freedom +
                                 # issue_l3_restriction_on_religious_rights +
                                 # issue_l3_education_system_increase_religion +
                                 # issue_l3_blasphemy +
                                 # issue_l3_cultural_rights +
                                 # issue_l3_restrictions_on_cultural_rights +
                                 # issue_l3_language_rights +
                                 # issue_l3_education_system_culture +
                                 # issue_l3_protection_of_cultural_heritage +
                                 # issue_l3_destruction_of_cultural_heritage +
                                 # issue_l3_gender_relations +
                                 # issue_l3_restriction_of_gender_rights  +
                                 # issue_l3_labor_rights +
                                 # issue_l3_childrens_rights +
                                 # issue_l3_referendum +
                                 # issue_l3_change_economic_system +
                                 # issue_l3_economic_reforms +
                                 # issue_l3_public_services +
                                 # issue_l3_basic_needs +
                                 # issue_l3_land_reforms +
                                 # issue_l3_water_resources +
                                 # issue_l3_revenue_from_natural_resources +
                                 # issue_l3_protection_of_natural_resources_environment +
                                 # # issue_l3_climate_change +
                                 # issue_l3_ifi_military_intervention_foreign_forces +
                                 # issue_l3_ifi_military_support +
                                 # issue_l3_ifi_political_support +
                                 # issue_l3_ifi_financial_support +
                                 # issue_l3_ifi_intal_monitoring +
                                 # issue_l3_ifi_support_from_diasporas_foreign_fighters +
                                 # issue_l3_ifi_sanctions +
                                 # issue_l3_ifi_humanitarian_aid +
                                 # issue_l3_ifi_intl_investigation_court_tribunal +
                                 # issue_l3_ifi_foreign_mediator +
                                 # issue_l3_ifi_recognition +
                                 # issue_l3_ifi_use_of_foreign_influence +
                                 # issue_l3_dfi_withdrawal_of_military_intervention +
                                 # issue_l3_dfi_withdrawal_of_military_support +
                                 # issue_l3_dfi_withdrawal_of_political_support +
                                 # issue_l3_dfi_withdrawal_of_financial_support +
                                 # # issue_l3_dfi_withdrawal_of_intl_monitoring +
                                 # issue_l3_dfi_withdrawal_of_sanctions +
                                 # issue_l3_dfi_removal_of_foreign_mediator +
                                 # issue_l3_dfi_removal_of_foreigners +
                                 # issue_l3_dfi_removal_of_foreign_influence +
                                 # issue_l3_dfi_withdrawal_of_humanitarian_aid +
                                 # issue_l3_attrocities_and_abuses +
                                 # issue_l3_collective_targeting_ethnic +
                                 # # issue_l3_collective_targeting_political +
                                 # issue_l3_collective_targeting_religious +
                                 # issue_l3_collective_targeting_other_group +
                                 # issue_l3_call_for_collective_targeting_ethnic +
                                 # issue_l3_call_for_collective_targeting_political +
                                 # issue_l3_call_for_collective_targeting_religious +
                                 # issue_l3_call_for_collective_targeting_othergroup +
                                 # issue_l3_military_conduct +
                                 # issue_l3_revenge_vengance +
                                 # issue_l3_return_of_refugees +
                                 # issue_l3_nonrefoulement_of_refugees +
                                 # issue_l3_release_of_prisoners +
                                 # issue_l3_call_for_negotiations +
                                 # issue_l3_opposition_to_negotiations +
                                 # issue_l3_structure_of_negotiations +
                                 # issue_l3_national_dialogue +
                                 # issue_l3_call_for_ceasefire +
                                 # issue_l3_call_for_implementaion_of_ceasefire +
                                 # issue_l3_peace_agreement_implementation +
                                 # issue_l3_opposition_to_peace_agreement +
                                 # issue_l3_ddr_issues +
                                 # issue_l3_call_for_truth_and_reconciliation_processes +
                                 # issue_l3_accountability_prosecution_investigation +
                                 # issue_l3_amnesties +
                                 # issue_l3_recognition_of_wrongdoing +
                                 # issue_l3_compensation_restoration,
                               data = survival_df2,
                               control = coxph.control(iter.max = 100))
summary(surv_model2)

table(survival_df2$issue_l3_climate_change, useNA = "always")

  
x <- survival_df %>%
  dplyr::filter(is.na(total.trade.perc.gdp.pwt))

### length of peace model --------------------------------------------------------------------------
lop_df <- survival_df %>%
  dplyr::filter(ongoing_peace == 0)

lop_glm <- glm(length ~ Americas + Asia + SSA +
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
                 total.trade.perc.gdp.pwt + trade.balance.perc.total.trade +
                 log(land_area) + shec + irst_per_capita_un + ethf,
               data = lop_df)
summary(lop_glm)



# survival_df <- conflict_table %>%
#   dplyr::filter(conflict == 0) %>%
#   dplyr::mutate(ongoing_peace = ifelse(end_year == 2019, 1, 0)) %>%
#   dplyr::group_by(confid,iso3c) %>%
#   dplyr::mutate(id = dplyr::row_number()) %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(uniqueid = paste0(confid,iso3c,id)) %>%
#   dplyr::rename(year = end_year) %>%
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

# survival3 <- survivalAnalysis::analyse_multivariate(
#   data = survival_df,
#   time_status = vars(length, ongoing_peace),
#   covariates = vars(gdppc.pwt.un,polity.pca,colony,ln.un.pop,region1)
#                     #Acceptance.of.the.Rights.of.Others,Equitable.Distribution.of.Resources,Free.Flow.of.Information,High.Levels.of.Human.Capital,Low.Levels.of.Corruption,Well.Functioning.Government)
# )
# survival3
