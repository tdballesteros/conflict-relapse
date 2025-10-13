
# This script uses the formatted conflict data and calculates conflict ID-year level variables.


### load libraries ---------------------------------------------------------------------------------
library(readxl)
library(countrycode)
library(dplyr)


### not in function --------------------------------------------------------------------------------
'%!in%' <- function(x,y)!('%in%'(x,y))


### load data file ---------------------------------------------------------------------------------
# conflict issues dataset
conflict_issues_data <- readxl::read_xlsx("Data files/Raw data files/ucdp_issues_dataset_dyadyear_232.xlsx")

# formatted conflict data
conflict_full_data <- read.csv("Data files/Formatted data files/conflict_full_data.csv")


### format conflict issues data --------------------------------------------------------------------
conflict_issues <- conflict_issues_data %>%
  dplyr::select(confid = conflict_id, year, names(conflict_issues_data)[c(23:143)]) %>%
  # dfi = decrease foreign involvement
  # ifi = increase foreign involvement
  # cps = change political system
  dplyr::rename(
    # TERRITORY (1000)
    ## -Separatism- (1100)
    issue_l3_independence = `1101`, # Independence
    issue_l3_irredentism = `1102`, # Irredentism
    ## -Unification- (1200)
    issue_l3_unification_of_states = `1201`, # Unification of states
    ## -Self-rule- (1300)
    issue_l3_autonomy = `1301`, # Autonomy
    issue_l3_federalism = `1302`, # Federalism
    issue_l3_confederation_union = `1303`, # Confederation/union
    ## -Adjustments to administrative arrangements- (1400)
    issue_l3_decentralization = `1401`, # Decentralization
    issue_l3_change_of_admin_divisions = `1402`, # Change of administrative divisions
    
    # STATE STRUCTURE (2000)
    ## -Change political system- (2100)
    issue_l3_cps_democracy = `2101`, # CPS: Democracy
    issue_l3_cps_socialism = `2102`, # CPS: Socialist state
    issue_l3_cps_islamic = `2103`, # CPS: Islamic state
    issue_l3_cps_other = `2104`, # CPS: Other/undefined system
    ## -Executive- (2200)
    issue_l3_oust_full_executive = `2201`, # Oust full executive
    issue_l3_oust_head_of_executive = `2202`, # Oust head of the executive
    issue_l3_reform_executive_structure = `2203`, # Reform executive structure
    issue_l3_executive_power_sharing = `2204`, # Executive power-sharing
    issue_l3_executive_power_sharing_interim = `2205`, # Executive power-sharing: interim
    issue_l3_oust_local_executive = `2206`, # Oust local executive
    ## -Parliament- (2300)
    issue_l3_oust_parliament = `2301`, # Oust parliament
    issue_l3_reform_parliament = `2302`, # Reform parliament
    issue_l3_parliamentary_power_sharing = `2303`, # Parliamentary power-sharing
    ## -Judiciary- (2400)
    issue_l3_change_judicial_system = `2401`, # Change judicial system
    issue_l3_reform_judicial_system = `2402`, # Reform judicial system
    issue_l3_constitutional_issues = `2403`, # Constitutional issues
    ## -Security sector- (2500)
    issue_l3_restructure_military_forces = `2501`, # Restructure military forces
    issue_l3_restructure_police_forces = `2502`, # Restructure police forces
    issue_l3_disband_paramilitary_forces = `2503`, # Disband para-military forces
    issue_l3_security_sector_power_sharing = `2504`, # Security sector power-sharing
    ## -Bureaucratic structure- (2600)
    issue_l3_bureaucratic_setup = `2601`, # Bureaucratic setup
    
    # GOVERNANCE (3000)
    ## -Quality of governance- (3100)
    issue_l3_rule_of_law = `3101`, # Rule of law
    issue_l3_law_and_order = `3102`, # Law and order
    issue_l3_corruption = `3103`, # Corruption
    issue_l3_foreign_policy_reform = `3104`, # Foreign policy reform
    ## -Elections- (3200)
    issue_l3_hold_elections = `3201`, # Hold elections
    issue_l3_opposition_to_elections = `3202`, # Opposition to elections
    issue_l3_electoral_reform = `3203`, # Electoral reform
    issue_l3_electoral_fraud = `3204`, # Electoral fraud
    
    # POLITICAL RIGHTS AND FREEDOMS (4000)
    ## -Civil rights and freedoms- (4100)
    issue_l3_civil_rights = `4101`, # Civil rights
    issue_l3_restriction_on_civil_rights = `4102`, # Restrictions on civil rights
    issue_l3_citizenship_reform = `4103`, # Citizenship reform
    issue_l3_human_rights = `4104`, # Human rights
    issue_l3_freedom_of_expression = `4105`, # Freedom of expression
    issue_l3_freedom_of_association = `4106`, # Freedom of association
    issue_l3_freedom_of_movement = `4107`, # Freedom of movement
    issue_l3_recognition_as_political_party = `4108`, # Recognition as a political party
    issue_l3_restrictions_on_freedom_of_expression = `4109`, # Restrictions on freedom of expression
    ## -Religious rights and freedoms- (4200)
    issue_l3_religious_freedom = `4201`, # Religious rights
    issue_l3_restriction_on_religious_rights = `4202`, # Restrictions on religious rights
    issue_l3_education_system_increase_religion = `4205`, # Education system: increase religion
    issue_l3_blasphemy = `4206`, # Blasphemy
    ## -Cultural rights and freedoms- (4300)
    issue_l3_cultural_rights = `4301`, # Cultural rights
    issue_l3_restrictions_on_cultural_rights = `4302`, # Restrictions on cultural rights
    issue_l3_language_rights = `4303`, # Language rights
    issue_l3_education_system_culture = `4304`, # Education system: culture
    issue_l3_protection_of_cultural_heritage = `4305`, # Protection of cultural heritage
    issue_l3_destruction_of_cultural_heritage = `4306`, # Destruction of cultural heritage
    ## -Gender rights and freedoms- (4400)
    issue_l3_gender_relations = `4401`, # Gender rights
    issue_l3_restriction_of_gender_rights = `4402`, # Restriction of gender rights
    ## -Labor rights and freedoms- (4500)
    issue_l3_labor_rights = `4501`, # Labor rights
    ## -Children's rights and freedoms- (4600)
    issue_l3_childrens_rights = `4601`, # Children's rights
    ## -Referendum- (4700)
    issue_l3_referendum = `4701`, # Referendum
    
    # DISTRIBUTION OF RESOURCES (5000)
    ## -State distribution systems- (5100)
    issue_l3_change_economic_system = `5101`, # Change economic system
    issue_l3_economic_reforms = `5102`, # Economic reforms
    issue_l3_public_services = `5103`, # Public services
    issue_l3_basic_needs = `5104`, # Basic needs
    ## -Natural resources- (5200)
    issue_l3_land_reforms = `5201`, # Land reforms
    issue_l3_water_resources = `5202`, # Water resources
    issue_l3_revenue_from_natural_resources = `5203`, # Revenues from natural resources
    issue_l3_protection_of_natural_resources_environment = `5204`, # Protection of natural resources/the environment
    issue_l3_climate_change = `5205`, # Climate change
    
    # FOREIGN INVOLVEMENT (6000)
    ## -Increase foreign involvement- (6100)
    issue_l3_ifi_military_intervention_foreign_forces = `6101`, # IFI: Military intervention/foreign forces
    issue_l3_ifi_military_support = `6102`, # IFI: Military support
    issue_l3_ifi_political_support = `6103`, # IFI: Political support
    issue_l3_ifi_financial_support = `6104`, # IFI: Financial support
    issue_l3_ifi_intal_monitoring = `6105`, # IFI: International monitoring
    issue_l3_ifi_support_from_diasporas_foreign_fighters = `6106`, # IFI :Support from diasporas/foreign fighters
    issue_l3_ifi_sanctions = `6107`, # IFI: Sanctions
    issue_l3_ifi_humanitarian_aid = `6108`, # IFI: Humanitarian aid
    issue_l3_ifi_intl_investigation_court_tribunal = `6109`, # IFI: International investigations/court/tribunal
    issue_l3_ifi_foreign_mediator = `6110`, #IFI: Foreign mediator
    issue_l3_ifi_recognition = `6111`, # IFI: Recognition
    issue_l3_ifi_use_of_foreign_influence = `6112`, # IFI: Use of foreign influence
    ## -Decrease foreign involvement- (6200)
    issue_l3_dfi_withdrawal_of_military_intervention = `6201`, # DFI: Withdrawal of military intervention/foreign forces
    issue_l3_dfi_withdrawal_of_military_support = `6202`, # DFI: Withdrawal of military support
    issue_l3_dfi_withdrawal_of_political_support = `6203`, # DFI: Withdrawal of political support
    issue_l3_dfi_withdrawal_of_financial_support = `6204`, # DFI: Withdrawal of financial support
    issue_l3_dfi_withdrawal_of_intl_monitoring = `6205`, # DFI: Withdrawal of international monitoring
    issue_l3_dfi_withdrawal_of_sanctions = `6206`, # DFI: Withdrawal of sanctions
    issue_l3_dfi_removal_of_foreign_mediator = `6207`, # DFI: Removal of foreign mediator/facilitator
    issue_l3_dfi_removal_of_foreigners = `6208`, # DFI: Removal of foreigners
    issue_l3_dfi_removal_of_foreign_influence = `6209`, # DFI: Removal of foreign influence
    issue_l3_dfi_withdrawal_of_humanitarian_aid = `6210`, # DFI: Withdrawal of humanitarian aid
    
    # VIOLENT TARGETING (7000)
    ## -Atrocities and abuses- (7100)
    issue_l3_attrocities_and_abuses = `7101`, # Atrocities and abuses
    ## -Collective targeting- (7200)
    issue_l3_collective_targeting_ethnic = `7201`, # Collective targeting: ethnic
    issue_l3_collective_targeting_political = `7202`, # Collective targeting: political
    issue_l3_collective_targeting_religious = `7203`, # Collective targeting: religious
    issue_l3_collective_targeting_other_group = `7204`, # Collective targeting: other group
    ## -Call for collective targeting- (7300)
    issue_l3_call_for_collective_targeting_ethnic = `7301`, # Call for collective targeting: ethnic
    issue_l3_call_for_collective_targeting_political = `7302`, # Call for collective targeting: political
    issue_l3_call_for_collective_targeting_religious = `7303`, # Call for collective targeting: religious
    issue_l3_call_for_collective_targeting_othergroup = `7304`, # Call for collective targeting: other group
    ## -Military conduct- (7400)
    issue_l3_military_conduct = `7401`, # Military conduct
    ## -Revenge/vengance- (7500)
    issue_l3_revenge_vengance = `7501`, # Revenge/vengeance
    
    # REFUGEES, IDPS AND PRISONERS (8000)
    ## -Return of refugees/IDPs- (8100)
    issue_l3_return_of_refugees = `8101`, # Return of refugees/IDPs
    issue_l3_nonrefoulement_of_refugees = `8102`, # Non-refoulement of refugees/IDPs
    ## -Release of prisoners- (8200)
    issue_l3_release_of_prisoners = `8201`, # Release of prisoners
    
    # NEGOTIATIONS, CEASEFIRES AND PEACE AGREEMENTS (9000)
    ## -Negotiations- (9100)
    issue_l3_call_for_negotiations = `9101`, # Call for negotiations
    issue_l3_opposition_to_negotiations = `9102`, # Opposition to negotiations
    issue_l3_structure_of_negotiations = `9103`, # Structure of negotiations
    issue_l3_national_dialogue = `9104`, # National dialogue
    ## -Ceasefires- (9200)
    issue_l3_call_for_ceasefire = `9201`, # Call for ceasefire
    issue_l3_call_for_implementaion_of_ceasefire = `9202`, # Call for implementation/abiding to ceasefire
    ## -Peace agreement- (9300)
    issue_l3_peace_agreement_implementation = `9301`, # Peace agreement implementation
    issue_l3_opposition_to_peace_agreement = `9302`, # Opposition to peace agreement
    issue_l3_ddr_issues = `9304`, # DDR issues
    
    # TRANSITIONAL JUSTICE AND LIABILITY (10000)
    ## -Truth and reconciliation processes- (10100)
    issue_l3_call_for_truth_and_reconciliation_processes = `10101`, # Call for truth and reconciliation processes
    ## -Liability- (10200)
    issue_l3_accountability_prosecution_investigation = `10201`, # Accountability/prosecution/investigation
    issue_l3_amnesties = `10202`, # Amnesties
    issue_l3_recognition_of_wrongdoing = `10203`, # Recognition of wrongdoing
    issue_l3_compensation_restoration = `10204`, # Compensation/restoration
    
    # NONE RECORDED
    issue_l3_none_recorded = `9999`
  ) %>%
  
  # collapse different actors within the same confid-year
  dplyr::group_by(confid, year) %>%
  dplyr::summarise(across(everything(), max, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  
  # collapse into larger categories
  dplyr::mutate(
    # subcategories
    # 1000: TERRITORY
    issue_l2_subcategory_separatism = dplyr::case_when(
      issue_l3_independence > 0 | issue_l3_irredentism > 0 ~ 1,
      .default = 0
    ),
    issue_l2_subcategory_unification = dplyr::case_when(
      issue_l3_unification_of_states > 0 ~ 1,
      .default = 0
    ),
    issue_l2_subcategory_selfrule = dplyr::case_when(
      issue_l3_autonomy > 0 | issue_l3_federalism > 0 | issue_l3_confederation_union > 0 ~ 1,
      .default = 0
    ),
    issue_l2_admin_arrangements = dplyr::case_when(
      issue_l3_decentralization > 0 | issue_l3_change_of_admin_divisions > 0 ~ 1,
      .default = 0
    ),
    issue_l1_territory = dplyr::case_when(
      issue_l2_subcategory_separatism > 0 | issue_l2_subcategory_unification > 0 |
        issue_l2_subcategory_selfrule > 0 | issue_l2_admin_arrangements > 0 ~ 1,
      .default = 0
    ),
    
    # 2000: STATE STRUCTURE
    issue_l2_change_political_system = dplyr::case_when(
      issue_l3_cps_democracy > 0 | issue_l3_cps_socialism > 0 | issue_l3_cps_islamic > 0 |
        issue_l3_cps_other > 0 ~ 1,
      .default = 0
    ),
    issue_l2_executive = dplyr::case_when(
      issue_l3_oust_full_executive > 0 | issue_l3_oust_head_of_executive > 0 |
        issue_l3_reform_executive_structure > 0 | issue_l3_executive_power_sharing > 0 |
        issue_l3_executive_power_sharing_interim > 0 | issue_l3_oust_local_executive > 0 ~ 1,
      .default = 0
    ),
    issue_l2_parliament = dplyr::case_when(
      issue_l3_oust_parliament > 0 | issue_l3_reform_parliament > 0 |
        issue_l3_parliamentary_power_sharing > 0 ~ 1,
      .default = 0
    ),
    issue_l2_judicial = dplyr::case_when(
      issue_l3_change_judicial_system > 0 | issue_l3_reform_judicial_system > 0 |
        issue_l3_constitutional_issues > 0 ~ 1,
      .default = 0
    ),
    issue_l2_security_sector = dplyr::case_when(
      issue_l3_restructure_military_forces > 0 | issue_l3_restructure_police_forces > 0 |
        issue_l3_disband_paramilitary_forces > 0 | issue_l3_security_sector_power_sharing > 0 ~ 1,
      .default = 0
    ),
    issue_l2_bureaucratic = dplyr::case_when(
      issue_l3_bureaucratic_setup > 0 ~ 1,
      .default = 0
    ),
    issue_l1_state_structure = dplyr::case_when(
      issue_l2_change_political_system > 0 | issue_l2_executive > 0 | issue_l2_parliament > 0 |
        issue_l2_judicial > 0 | issue_l2_security_sector > 0 | issue_l2_bureaucratic > 0 ~ 1,
      .default = 0
    ),
    
    # 3000: GOVERNANCE
    issue_l2_quality_of_governance = dplyr::case_when(
      issue_l3_rule_of_law > 0 | issue_l3_law_and_order > 0 | issue_l3_corruption > 0 |
        issue_l3_foreign_policy_reform > 0 ~ 1,
      .default = 0
    ),
    issue_l2_elections = dplyr::case_when(
      issue_l3_hold_elections > 0 | issue_l3_opposition_to_elections > 0 | issue_l3_electoral_reform > 0 |
        issue_l3_electoral_fraud > 0 ~ 1,
      .default = 0
    ),
    issue_l1_governance = dplyr::case_when(
      issue_l2_quality_of_governance > 0 | issue_l2_elections > 0 ~ 1,
      .default = 0
    ),
    
    # 4000: POLITICAL RIGHTS AND FREEDOMS
    issue_l2_civil_rights_and_freedoms = dplyr::case_when(
      issue_l3_civil_rights > 0 | issue_l3_restriction_on_civil_rights > 0 |
        issue_l3_citizenship_reform > 0 | issue_l3_human_rights > 0 | issue_l3_freedom_of_expression > 0 |
        issue_l3_freedom_of_association > 0 | issue_l3_freedom_of_movement > 0 |
        issue_l3_recognition_as_political_party > 0 |
        issue_l3_restrictions_on_freedom_of_expression > 0 ~ 1,
      .default = 0
    ),
    issue_l2_religious_rights_and_freedoms = dplyr::case_when(
      issue_l3_religious_freedom > 0 | issue_l3_restriction_on_religious_rights > 0 |
        issue_l3_education_system_increase_religion > 0 | issue_l3_blasphemy > 0 ~ 1,
      .default = 0
    ),
    issue_l2_cultural_rights_and_freedoms = dplyr::case_when(
      issue_l3_cultural_rights > 0 | issue_l3_restrictions_on_cultural_rights > 0 |
        issue_l3_language_rights > 0 | issue_l3_education_system_culture > 0 |
        issue_l3_protection_of_cultural_heritage > 0 | issue_l3_destruction_of_cultural_heritage > 0 ~ 1,
      .default = 0
    ),
    issue_l2_gender_rights_and_freedoms = dplyr::case_when(
      issue_l3_gender_relations > 0 | issue_l3_restriction_of_gender_rights > 0 ~ 1,
      .default = 0
    ),
    issue_l2_labor_rights_and_freedoms = dplyr::case_when(
      issue_l3_labor_rights > 0 ~ 1,
      .default = 0
    ),
    issue_l2_childrens_rights_and_freedoms = dplyr::case_when(
      issue_l3_childrens_rights > 0 ~ 1,
      .default = 0
    ),
    issue_l2_referendums = dplyr::case_when(
      issue_l3_referendum > 0 ~ 1,
      .default = 0
    ),
    issue_l1_political_rights = dplyr::case_when(
      issue_l2_civil_rights_and_freedoms > 0 | issue_l2_religious_rights_and_freedoms > 0 |
        issue_l2_cultural_rights_and_freedoms > 0 | issue_l2_gender_rights_and_freedoms > 0 |
        issue_l2_labor_rights_and_freedoms > 0 | issue_l2_childrens_rights_and_freedoms > 0 |
        issue_l2_referendums > 0 ~ 1,
      .default = 0
    ),
    
    # 5000: DISTRIBUTION OF RESOURCES
    issue_l2_state_distribution_systems = dplyr::case_when(
      issue_l3_change_economic_system > 0 | issue_l3_economic_reforms > 0 | issue_l3_public_services > 0 |
        issue_l3_basic_needs > 0 ~ 1,
      .default = NA
    ),
    issue_l2_natural_resources = dplyr::case_when(
      issue_l3_land_reforms > 0 | issue_l3_water_resources > 0 |
        issue_l3_revenue_from_natural_resources > 0 |
        issue_l3_protection_of_natural_resources_environment > 0 | issue_l3_climate_change > 0 ~ 1,
      .default = 0
    ),
    issue_l1_distribution_of_resources = dplyr::case_when(
      issue_l2_state_distribution_systems > 0 | issue_l2_natural_resources > 0 ~ 1,
      .default = 0
    ),
    
    # 6000: FOREIGN INVOLVEMENT (6000)
    issue_l2_ifi = dplyr::case_when(
      issue_l3_ifi_military_intervention_foreign_forces > 0 | issue_l3_ifi_military_support > 0 |
        issue_l3_ifi_political_support > 0 | issue_l3_ifi_financial_support > 0 |
        issue_l3_ifi_intal_monitoring > 0 | issue_l3_ifi_support_from_diasporas_foreign_fighters > 0 |
        issue_l3_ifi_sanctions > 0 | issue_l3_ifi_humanitarian_aid > 0 |
        issue_l3_ifi_intl_investigation_court_tribunal > 0 | issue_l3_ifi_foreign_mediator > 0 |
        issue_l3_ifi_recognition > 0 | issue_l3_ifi_use_of_foreign_influence > 0 ~ 1,
      .default = 0
    ),
    issue_l2_dfi = dplyr::case_when(
      issue_l3_dfi_withdrawal_of_military_intervention > 0 |
        issue_l3_dfi_withdrawal_of_military_support > 0 |
        issue_l3_dfi_withdrawal_of_political_support > 0 |
        issue_l3_dfi_withdrawal_of_financial_support > 0 |
        issue_l3_dfi_withdrawal_of_intl_monitoring > 0 | issue_l3_dfi_withdrawal_of_sanctions > 0 |
        issue_l3_dfi_removal_of_foreign_mediator > 0 | issue_l3_dfi_removal_of_foreigners > 0 |
        issue_l3_dfi_removal_of_foreign_influence > 0 |
        issue_l3_dfi_withdrawal_of_humanitarian_aid > 0 ~ 1,
      .default = 0
    ),
    issue_l1_foreign_involvement = dplyr::case_when(
      issue_l2_ifi > 0 | issue_l2_dfi > 0 ~ 1,
      .default = 0
    ),
    
    # 7000: VIOLENT TARGETING (7000)
    issue_l2_attrocities_and_abuses = dplyr::case_when(
      issue_l3_attrocities_and_abuses > 0 ~ 1,
      .default = 0
    ),
    issue_l2_collective_targeting = dplyr::case_when(
      issue_l3_collective_targeting_ethnic > 0 | issue_l3_collective_targeting_political > 0 |
        issue_l3_collective_targeting_religious > 0 | issue_l3_collective_targeting_other_group > 0 ~ 1,
      .default = 0
    ),
    issue_l2_call_for_collective_targeting = dplyr::case_when(
      issue_l3_call_for_collective_targeting_ethnic > 0 |
        issue_l3_call_for_collective_targeting_political > 0 |
        issue_l3_call_for_collective_targeting_religious > 0 |
        issue_l3_call_for_collective_targeting_othergroup > 0 ~ 1,
      .default = 0
    ),
    issue_l2_military_conduct = dplyr::case_when(
      issue_l3_military_conduct > 0 ~ 1,
      .default = 0
    ),
    issue_l2_revenge = dplyr::case_when(
      issue_l3_revenge_vengance > 0 ~ 1,
      .default = 0
    ),
    issue_l1_violent_targeting = dplyr::case_when(
      issue_l2_attrocities_and_abuses > 0 | issue_l2_collective_targeting > 0 |
        issue_l2_call_for_collective_targeting > 0 | issue_l2_military_conduct > 0 |
        issue_l2_revenge > 0 ~ 1,
      .default = 0
    ),
    
    # 8000: REFUGEES, IDPS AND PRISONERS
    issue_l2_return_of_refugees = dplyr::case_when(
      issue_l3_return_of_refugees > 0 | issue_l3_nonrefoulement_of_refugees > 0 ~ 1,
      .default = 0
    ),
    issue_l2_release_of_prisoners = dplyr::case_when(
      issue_l3_release_of_prisoners > 0 ~ 1,
      .default = 0
    ),
    issue_l1_refugees_prisoners = dplyr::case_when(
      issue_l2_return_of_refugees > 0 | issue_l2_release_of_prisoners > 0 ~ 1,
      .default = 0
    ),
    
    # 9000: NEGOTIATIONS, CEASEFIRES AND PEACE AGREEMENTS
    issue_l2_negotiations = dplyr::case_when(
      issue_l3_call_for_negotiations > 0 | issue_l3_opposition_to_negotiations > 0 |
        issue_l3_structure_of_negotiations > 0 | issue_l3_national_dialogue > 0 ~ 1,
      .default = 0
    ),
    issue_l2_ceasefires = dplyr::case_when(
      issue_l3_call_for_ceasefire > 0 | issue_l3_call_for_implementaion_of_ceasefire > 0 ~ 1,
      .default = 1
    ),
    issue_l2_peace_agreement = dplyr::case_when(
      issue_l3_peace_agreement_implementation > 0 | issue_l3_opposition_to_peace_agreement > 0 |
        issue_l3_ddr_issues > 0 ~ 1,
      .default = 0
    ),
    issue_l1_negotiations = dplyr::case_when(
      issue_l2_negotiations > 0 | issue_l2_ceasefires > 0 | issue_l2_peace_agreement > 0 ~ 1,
      .default = 0
    ),
    
    # 10000: TRANSITIONAL JUSTICE AND LIABILITY
    issue_l2_truth_reconciliation = dplyr::case_when(
      issue_l3_call_for_truth_and_reconciliation_processes > 0 ~ 1,
      .default = 0
    ),
    issue_l2_liability = dplyr::case_when(
      issue_l3_accountability_prosecution_investigation > 0 | issue_l3_amnesties > 0 |
        issue_l3_recognition_of_wrongdoing > 0 | issue_l3_compensation_restoration > 0 ~ 1,
      .default = 0
    ),
    issue_l1_justice = dplyr::case_when(
      issue_l2_truth_reconciliation > 0 | issue_l2_liability > 0 ~ 1,
      .default = 0
    )
  )


### missing data -----------------------------------------------------------------------------------

# for years 1946-1988, use summary 1989 value, or earliest year value if that is NA
ci_summary_min <- conflict_issues %>%
  dplyr::filter(year > 1945) %>%
  dplyr::group_by(confid) %>%
  dplyr::filter(year == min(year, na.rm = TRUE)) %>%
  dplyr::ungroup()

for(y in 1946:1988){

  tmp <- ci_summary_min %>%
    dplyr::mutate(year = y)

  conflict_issues <- conflict_issues %>%
    rbind(tmp)

}

# for years 2018-2019, use summary 2017 value, or latest year value if that is NA
ci_summary_max <- conflict_issues %>%
  dplyr::filter(year > 1945) %>%
  dplyr::group_by(confid) %>%
  dplyr::filter(year == max(year, na.rm = TRUE)) %>%
  dplyr::ungroup()


for(y in 2018:2019){

  tmp <- ci_summary_max %>%
    dplyr::mutate(year = y)

  conflict_issues <- conflict_issues %>%
    rbind(tmp)

}

# expand dataset to contain any missing years 1989-2017
conflict_issues <- conflict_issues %>%
  dplyr::full_join(expand.grid(confid = unique(conflict_issues$confid), year = c(1989:2017))) %>%
  dplyr::arrange(year, confid)


### collapse by conflict ---------------------------------------------------------------------------
summarise_vars <- names(conflict_issues)[names(conflict_issues) %!in% c("confid", "year", "iso3c",
                                                                        "conflict", "group_id")]

conflict_issues <- conflict_issues %>%
  dplyr::filter(
    year > 1945,
    # filter confid not in the UCDP 20.1 dataset
    confid %!in% c(438, 13349)
    ) %>%
  dplyr::left_join(conflict_full_data, by = c("confid", "year")) %>%
  dplyr::mutate(
    group_id = ifelse(conflict == 0, group_id - 1, group_id),
    peace_start_year = ifelse(conflict == 0, year, NA)
    ) %>%
  dplyr::group_by(confid, group_id) %>%
  dplyr::summarise(
    across(all_of(summarise_vars), max, na.rm = TRUE),
    iso3c = max(iso3c, na.rm = TRUE),
    conflict = max(conflict, na.rm = TRUE),
    year_start = min(year, na.rm = TRUE),
    year_end = max(year, na.rm = TRUE),
    peace_start_year = min(peace_start_year, na.rm = TRUE)
    ) %>%
  dplyr::ungroup() %>%
  # fix -Inf values
  dplyr::mutate(
    dplyr::across(dplyr::everything(), .fns = ~ replace(., is.infinite(.), NA))
    ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    issue_count = sum(dplyr::across(dplyr::all_of(summarise_vars)), na.rm = TRUE),
    # if peace_start_year is NA, replace with year_start value
    peace_start_year = dplyr::coalesce(peace_start_year, year_start)
    ) %>%
  dplyr::select(confid, iso3c, peace_start_year, dplyr::all_of(summarise_vars))
 

### missing conflict data --------------------------------------------------------------------------
all_conflict_ids <- unique(conflict_full_data$confid)

missing_conflict_ids <- all_conflict_ids[all_conflict_ids %!in% conflict_issues$confid]
missing_conflict_ids


### write data -------------------------------------------------------------------------------------
# writes formatted dataframe as csv files
write.csv(conflict_issues,
          "Data files/Formatted data files/conflict_issues.csv",
          row.names = FALSE)
