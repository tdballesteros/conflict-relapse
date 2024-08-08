
### load libraries ----------------------------------------------------------------------
library(countrycode)
library(survival)
library(ggsurvfit)
library(survivalAnalysis)
library(dplyr)
library(tidyr)

### load data ----------------------------------------------------------------------
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

# military_capacity_ccpu <- read.csv("Data files/Formatted data files/military_capacity_index_ccpu.csv")

conflict_variables <- read.csv("Data files/Formatted data files/conflict_variables.csv")
conflict_table <- read.csv("Data files/Formatted data files/conflict_table.csv")
conflict_years <- read.csv("Data files/Formatted data files/conflict_years.csv")



survival_df <- conflict_table %>%
  dplyr::filter(conflict == 0) %>%
  dplyr::mutate(ongoing_peace = ifelse(yrend == 2019, 1, 0)) %>%
  dplyr::group_by(confid,iso3c) %>%
  dplyr::mutate(id = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(uniqueid = paste0(confid,iso3c,id)) %>%
  dplyr::rename(year = yrend) %>%
  dplyr::left_join(country_regions1,by=c("iso3c")) %>%
  dplyr::left_join(country_regions2,by=c("iso3c")) %>%
  dplyr::left_join(country_regions3,by=c("iso3c")) %>%
  dplyr::left_join(colonialism,by=c("iso3c")) %>%
  dplyr::left_join(population,by=c("iso3c","year")) %>%
  dplyr::left_join(gdppc,by=c("iso3c","year")) %>%
  dplyr::left_join(polity,by=c("iso3c","year")) %>%
  dplyr::left_join(ppi,by=c("iso3c","year")) %>%
  dplyr::mutate(
    ln.un.pop = log(un.pop),
    ln.cow.pop = log(cow.pop)
  )
  
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
