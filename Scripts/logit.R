
### load libraries ----------------------------------------------------------------------
library(countrycode)
library(car)
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
elections <- read.csv("Data files/Formatted data files/elections.csv")

military_capacity_ccpu <- read.csv("Data files/Formatted data files/military_capacity_index_ccpu.csv")

conflict_variables <- read.csv("Data files/Formatted data files/conflict_variables.csv")
conflict_table <- read.csv("Data files/Formatted data files/conflict_table.csv")
conflict_years <- read.csv("Data files/Formatted data files/conflict_years.csv")

### combine data ----------------------------------------------------------------------

logit_data <- conflict_years %>%
  # dplyr::left_join(country_regions1,by=c("iso3c","country")) %>%
  # dplyr::left_join(country_regions2,by=c("iso3c","country")) %>%
  dplyr::left_join(country_regions3,by=c("iso3c","country")) %>%
  dplyr::left_join(colonialism,by=c("iso3c","country")) %>%
  # dplyr::left_join(gdp,by=c("iso3c","country","year")) %>%
  dplyr::left_join(population,by=c("iso3c","country","year")) %>%
  dplyr::left_join(gdppc,by=c("iso3c","country","year")) %>%
  dplyr::left_join(conflict_variables,by=c("confid","iso3c","year","conflict")) %>%
  dplyr::left_join(polity,by=c("iso3c","country","year")) %>%
  dplyr::left_join(military_capacity_ccpu,by=c("iso3c","year")) %>%
  dplyr::left_join(pko,by=c("iso3c","country","year")) %>%
  dplyr::left_join(ppi,by=c("iso3c","year")) %>%
  dplyr::left_join(elections,by=c("iso3c","country","year"))

logit_data$region1 <- factor(logit_data$region1,
                             levels = c("Europe","Americas","Asia","MENA","SSA"))
logit_data$region2 <- factor(logit_data$region2,
                             levels = c("WEOG","South America","East Asia","Middle East","Southeast Asia",
                                        "Eastern Europe","Central America","South Asia","Caribbean","Central Africa",
                                        "East Africa","West Africa","Southern Africa","North Africa","Central Asia"))
logit_data$outcome <- factor(logit_data$outcome,
                             levels = c("5: Low Activity","1: Peace Agreement","2: Ceasefire Agreement",
                                        "3: Government Victory","4: Non-Government Victory","6: Actor Ceases to Exist"))
logit_data$colonizer <- factor(logit_data$colonizer,
                             levels = c("Not colonized","Colony of ESP","Colony of FRA","Colony of GBR","Colony of PRT",
                                        "Colony of another country"))
  
test_variables <- c(
  "region1",
  "gdppc.pwt.un",
  "un.pop",
  # "yrs_since",
  "other_conf",
  "pko_mission",
  "colonizer",
  "binary_neigh_conf",
  "conf_length",
  "outcome",
  "polity.pca",
  "mil.cap",
  "natelec","natelec.n","natelec.l","years_since_last_elec",
                    # "Overall.PPI.Score",
                    "Acceptance.of.the.Rights.of.Others","Equitable.Distribution.of.Resources","Free.Flow.of.Information",
                    "High.Levels.of.Human.Capital","Low.Levels.of.Corruption","Sound.Business.Environment","Well.Functioning.Government"
                    )

logit_data_slim <- logit_data %>%
  dplyr::select(conflict,dplyr::all_of(test_variables)) %>%
  dplyr::mutate(
    un.pop.ln = log(un.pop),
    gdppc.pwt.un.ln = log(gdppc.pwt.un)
    ) %>%
  dplyr::select(-c(un.pop,gdppc.pwt.un)) %>%
  na.omit()
  
# summary(glm(conflict ~ log(yrs_since),
#         data = logit_data_slim, family = "binomial"))

glm1 <- glm(conflict ~ .,
            data = logit_data_slim, family = "binomial")
summary(glm1)
car::vif(glm1)

glm.test <- glm(conflict ~ natelec + natelec.l + natelec.n + log(years_since_last_elec),
                data = logit_data_slim, family = "binomial")
summary(glm.test)







#### Setup ####
fiscap2 <- fiscap %>%
  select(-c(pop.pd,gdppc,lngdppc,gdp))

relap <- ucdp4a %>%
  select(confid,iso3c,year,conflict,`-1`,`1`,`2`,`3`,`4`,`5`,`6`) %>%
  group_by(confid,iso3c,year) %>%
  mutate(conflict = max(conflict)) %>%
  ungroup() %>%
  left_join(cap,by=c("iso3c","year")) %>%
  left_join(yrsince,by=c("confid","year")) %>%
  left_join(oc,by=c("iso3c","year")) %>%
  left_join(neigh,by=c("iso3c","year")) %>%
  left_join(pko,by=c("iso3c","year")) %>%
  left_join(elec,by=c("iso3c","year")) %>%
  left_join(age2,by=c("iso3c")) %>%
  left_join(pr,by=c("confid","year")) %>%
  left_join(lc,by=c("confid","year")) %>%
  left_join(cow,by=c("iso3c","year")) %>%
  left_join(fiscap2,by=c("iso3c","year")) %>%
  left_join(oil,by=c("iso3c","year")) %>%
  left_join(efi,by=c("iso3c","year")) %>%  
  mutate(lngdppc = log(gdppc)) %>%
  mutate(gdppc = gdppc / 1000) %>%
  rename(peace.ag = `1`) %>%
  rename(ceasefire = `2`) %>%
  rename(gov.vic = `3`) %>%
  rename(rebel.vic = `4`) %>%
  rename(low.act = `5`) %>%
  rename(cease.exist = `6`) %>%
  mutate(agrmt = peace.ag + ceasefire) %>%
  mutate(mil.vic = gov.vic + rebel.vic) %>%
  mutate(lngdpgrowth = log(gdpgrowth)) %>%
  mutate(lnpopgrowth = log(popgrowth)) %>%
  #mutate(countryage = year - beginning) %>%
  #mutate(countryagesq = countryage^2) %>%
  mutate(countryage = year - independence) %>%
  mutate(lnage = log(countryage)) %>%
  mutate(lnyearsince = log(yrsince)) %>%
  mutate(lndelta.mc = log(delta.mc)) %>%
  mutate(lndelta.fc = log(delta.fc)) %>%
  mutate(lndelta.bac = log(delta.bac)) %>%
  mutate(x40s = 0) %>%
  mutate(x50s = 0) %>%
  mutate(x60s = 0) %>%
  mutate(x70s = 0) %>%
  mutate(x80s = 0) %>%
  mutate(x90s = 0) %>%
  mutate(x00s = 0) %>%
  mutate(x10s = 0) %>%
  mutate(observer_dummy = 0) %>%
  mutate(pcw = 0) %>%
  mutate(pre84 = 0) %>%
  unique()

relap$conflict <- as.factor(relap$conflict)
relap$natelec[is.na(relap$natelec)] <- 0
relap$natelec.n[is.na(relap$natelec.n)] <- 0
relap$natelec.l[is.na(relap$natelec.l)] <- 0
relap$prevrelap[is.na(relap$prevrelap)] <- 0
relap$mission[is.na(relap$mission)] <- 0

relap[relap==Inf] <- NA
relap[relap==-Inf] <- NA
relap$lnage[relap$lnage==-Inf] <- NA
relap$oil[is.na(relap$oil)] <- 0

for(i in 1:nrow(relap)){
  if(relap$year[i] > 1991){
    relap$pcw[i] <- 1
  }
  if(relap$year[i] < 1984){
    relap$pre84[i] <- 1
  }
  if(relap$year[i] %in% c(1940:1949)){
    relap$x40s[i] <- 1
  }
  else if(relap$year[i] %in% c(1950:1959)){
    relap$x50s[i] <- 1
  }
  else if(relap$year[i] %in% c(1960:1969)){
    relap$x60s[i] <- 1
  }
  else if(relap$year[i] %in% c(1970:1979)){
    relap$x70s[i] <- 1
  }
  else if(relap$year[i] %in% c(1980:1989)){
    relap$x80s[i] <- 1
  }
  else if(relap$year[i] %in% c(1990:1999)){
    relap$x90s[i] <- 1
  }
  else if(relap$year[i] %in% c(2000:2009)){
    relap$x00s[i] <- 1
  }
  else if(relap$year[i] %in% c(2010:2019)){
    relap$x10s[i] <- 1
  }
}

relap <- relap %>%
  mutate(pop = pop.pd / 1000000) %>%
  #filter(year >= 1983) %>%
  unique()

relap <- relap[complete.cases(relap),]

relaptest <- relap %>%
  select(-c(ba.cap,delta.bac))

relaptest <- relaptest[!complete.cases(relaptest),]

idkglm <- glm(conflict ~ lnmilexpgdp + lntroops + lnmilexpt + lnaid2gdp + lnaid2pc + taxgdp + polity + politysq +
                gdppc + lnpop + peace.ag + ceasefire + gov.vic + rebel.vic + cease.exist + yrsince + othco + cn +
                troops_dummy + observer_dummy + minus.fc + minus.fcsq + minus.mc + minus.mcsq + gdpgrowth + natelec +
                natelec.n + natelec.l + shec + minus.polity + minus.politysq + lnage + prevrelap,
              data = relap, family = "binomial")
summary(idkglm)
car::vif(idkglm)

idklogistf <- logistf(conflict ~ lnmilexpgdp + lntroops + lnaid2pc + taxgdp + polity +
                        gdppc + lnpop + peace.ag + ceasefire + gov.vic + rebel.vic +
                        troops_dummy + minus.fc + minus.fcsq + minus.mc +
                        minus.mcsq + gdpgrowth + natelec + natelec.n + natelec.l + shec + minus.polity +
                        minus.politysq,
                      data = relap, pl = T)
summary(idklogistf)

# lnaid2gdp, politysq, lnmilexpt, cease.exist, lnage, prevrelap,yrsince, othco, cn, observer_dummy


# troops_dummy > mission_dummy
test.glm1 <- glm(conflict ~ fis.cap + mil.cap + shec + minus.fc + minus.mc + perc.shec +
                   gdppc + perc.gdppc + polity + politysq + minus.polity + lnpop + mission + peace.ag +
                   ceasefire + mil.vic + rebel.vic + cease.exist + natelec + natelec.n + natelec.l + yrsince +
                   prevrelap + log(length) + othco + cn + ethf +
                   `Central America` + Carribean + `South America` + `Eastern Europe` + `Middle East` +
                   `North Africa` + `East Africa` + `West Africa` + `Central Africa` + `Southern Africa` +
                   `Central Asia` + `South Asia` + `East Asia` + `Southeast Asia`,
                 data = relap, family = "binomial")
summary(test.glm1)
car::vif(test.glm1)

### maybe
test.glm1 <- glm(conflict ~ gdppc + gdpgrowth + polity + politysq + minus.polity + lnpop +
                   peace.ag + ceasefire + gov.vic + rebel.vic + cease.exist +
                   natelec + natelec.n + natelec.l + yrsince + mission + pcw +
                   mil.cap*minus.mc + fis.cap*minus.fc + shec*delta.shec,
                 data = relap, family = "binomial")
###

test.glm2 <- glm(conflict ~ fis.cap + mil.cap + shec + minus.fcsq + minus.mcsq + perc.shecsq +
                   gdppc + perc.gdppcsq + polity + politysq + minus.politysq + lnpop + mission + peace.ag +
                   ceasefire + mil.vic + rebel.vic + cease.exist + natelec + natelec.n + natelec.l + yrsince +
                   prevrelap + log(length) + othco + cn + ethf +
                   `Central America` + Carribean + `South America` + `Eastern Europe` + `Middle East` +
                   `North Africa` + `East Africa` + `West Africa` + `Central Africa` + `Southern Africa` +
                   `Central Asia` + `South Asia` + `East Asia` + `Southeast Asia`,
                 data = relap, family = "binomial")
summary(test.glm2)
car::vif(test.glm2)

# https://stats.idre.ucla.edu/r/dae/logit-regression/
with(test.glm1, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

test.logistf1 <- logistf(conflict ~ fis.cap + mil.cap + shec + minus.fc + minus.mc + perc.shec +
                           gdppc + perc.gdppc + polity + politysq + minus.polity + lnpop + mission + peace.ag +
                           ceasefire + mil.vic + rebel.vic + cease.exist + log(yrsince) + cn + prevrelap + log(length) +
                           ethf + othco,
                         data = relap, pl = T)
summary(test.logistf1)

relap0 <- relap %>%
  filter(conflict == 0)
relap1 <- relap %>%
  filter(conflict == 1)
par(mfrow=c(1,2))

relap.cor <- relap %>%
  select(conflict,mil.cap,fis.cap,polity,politysq,lngdppc,lnpop,mil.vic,agrmt,yrsince,othco,cn,troops_dummy,minus.fc,
         minus.mc,gdpgrowth,natelec,natelec.n,natelec.l,shec,minus.polity,minus.politysq,prevrelap) %>%
  mutate_if(is.factor,as.numeric)

cor(relap.cor,use="complete.obs")

one <- glm(conflict ~ yrsince, data = relap, family = "binomial")
summary(one)

relap.ba <- relap[complete.cases(relap$ba.cap),]

ba.glm1 <- glm(conflict ~ ba.cap + fis.cap + mil.cap + shec + minus.bac + minus.fc + minus.mc + perc.shec +
                 gdppc + perc.gdppc + polity + politysq + minus.polity + lnpop + mission + peace.ag +
                 ceasefire + mil.vic + rebel.vic + cease.exist + natelec + natelec.n + natelec.l + yrsince +
                 prevrelap + log(length) + othco + cn + ethf + exporter +
                 MENA + SSA + Asia + Americas,
               data = relap, family = "binomial")
summary(ba.glm1)
car::vif(ba.glm1)

ba.glm2 <- glm(conflict ~ ba.cap + fis.cap + mil.cap + shec + minus.bacsq + minus.fcsq + minus.mcsq + perc.shecsq +
                 gdppc + perc.gdppcsq + polity + politysq + minus.politysq + lnpop + mission + peace.ag +
                 ceasefire + mil.vic + rebel.vic + cease.exist + natelec + natelec.n + natelec.l + yrsince +
                 prevrelap + log(length) + othco + cn + ethf + exporter +
                 MENA + SSA + Asia + Americas,
               data = relap, family = "binomial")
summary(ba.glm2)
car::vif(ba.glm2)


ba.logistf <- logistf(conflict ~ ba.cap + fis.cap + mil.cap + shec + minus.bac + minus.fc + minus.mc + delta.shec +
                        gdppc + gdpgrowth + polity + minus.polity + lnpop + mission +
                        peace.ag + ceasefire + gov.vic + rebel.vic + cease.exist +
                        natelec + natelec.n + natelec.l + yrsince + prevrelap + length +
                        `Central America` + Carribean + `South America` + `Eastern Europe` + `Middle East` +
                        `North Africa` + `East Africa` + `West Africa` + `Central Africa` + `Southern Africa` +
                        `South Asia` + `East Asia` + `Southeast Asia` +
                        x80s + x90s + x00s,
                      data = relap, pl = T)
summary(ba.logistf)

#### relap2 - only 10 years after the conflict ends ####
relap2 <- relap %>%
  filter(yrsince <= 10)

relap2.glm1 <- glm(conflict ~ mil.cap + fis.cap + polity + lngdppc + lnpop +
                     mil.vic + agrmt + yrsince + othco + cn + minus.fc + minus.mc +
                     gdpgrowth + natelec + natelec.n + natelec.l + shec + minus.polity + minus.politysq +
                     prevrelap + `Central America` + Carribean + `South America` + `Eastern Europe` +
                     `Middle East` + `North Africa` + `East Africa` + `West Africa` + `Central Africa` +
                     `Southern Africa` + `Central Asia` + `South Asia` + `East Asia` + `Southeast Asia`,
                   data = relap2, family = "binomial")
summary(relap2.glm1)
car::vif(relap2.glm1)

relap2.logistf1 <- logistf(conflict ~ mil.cap + fis.cap + polity + politysq + lngdppc + lnpop +
                             mil.vic + agrmt + yrsince + othco + cn + minus.fc + minus.mc +
                             gdpgrowth + natelec + shec + minus.polity + minus.politysq +
                             prevrelap, #+ `Central America` + Carribean + `South America` + `Eastern Europe` +
                           #`Middle East` + `North Africa` + `East Africa` + `West Africa` + `Central Africa` +
                           #`Southern Africa` + `Central Asia` + `South Asia` + `East Asia` + `Southeast Asia`, +
                           #x40s + x50s + x60s + x70s + x80s + x90s + x00s,
                           data = relap2, pl = T)
summary(relap2.logistf1)

#
relap2.logistf1 <- logistf(conflict ~ mil.cap + fis.cap + XPolity2 + XPolity2sq + gdppc + lnpop + 
                             mil.vic + agrmt + yrsince + othco + cn + delta.fc + lnage +
                             delta.xpolity + delta.mc + gdpgrowth + natelec + shec + prevrelap + length +
                             `Central America` + Carribean + `South America` + `Eastern Europe` + `Middle East` +
                             `North Africa` + `East Africa` + `West Africa` + `Central Africa` + `Southern Africa` +
                             `Central Asia` + `South Asia` + `East Asia` + `Southeast Asia` + WEOG + x50s + x60s +
                             x70s + x80s + x90s + x00s + x10s,
                           data = relap2, pl = T)
summary(relap2.logistf1)
#

logistf.test <- logistf(conflict ~ mil.cap, data = relap2, pl = T)
summary(logistf.test)

#### relap3 - 5 years #####
relap3 <- relap %>%
  filter(yrsince <= 5)

relap3.glm <- glm(conflict ~ fis.cap + mil.cap + shec + minus.fc + minus.mc + delta.shec +
                    gdppc + gdpgrowth + polity + minus.polity + lnpop +
                    peace.ag + ceasefire + gov.vic + rebel.vic + cease.exist +
                    natelec + natelec.n + natelec.l + yrsince + othco + cn + prevrelap + length + pcw +
                    `Central America` + Carribean + `South America` + `Eastern Europe` + `Middle East` +
                    `North Africa` + `East Africa` + `West Africa` + `Central Africa` + `Southern Africa` +
                    `South Asia` + `East Asia` + `Southeast Asia`,
                  data = relap3, family = "binomial")
summary(relap3.glm)

relap3.logistf1 <- logistf(conflict ~ fis.cap + mil.cap + shec + minus.fc + minus.mc + delta.shec +
                             gdppc + gdpgrowth + polity + minus.polity + lnpop +
                             peace.ag + ceasefire + gov.vic + rebel.vic + cease.exist +
                             natelec + natelec.n + natelec.l + yrsince + othco + cn + prevrelap + length + pcw +
                             `Central America` + Carribean + `South America` + `Eastern Europe` + `Middle East` +
                             `North Africa` + `East Africa` + `West Africa` + `Central Africa` + `Southern Africa` +
                             `South Asia` + `East Asia` + `Southeast Asia`,
                           data = relap3, pl = T)
summary(relap3.logistf1)
