# This script formats the polity measurement from the Center for Systemic Peace.

# Polity component scores of XCONST, XRCOMP, and XROPEN are used, while PARCOMP and PARREG are dropped.

# load libraries
library(readxl)
library(countrycode)
library(dplyr)

# load data file
polity <- readxl::read_excel("Data files/Raw data files/p5v2018.xls")

# formatting
polity <- polity %>%
  dplyr::select(country,year,democ,autoc,polity2,parcomp,parreg,xconst,xrcomp,xropen) %>%
  dplyr::filter(year >= 1946) %>%
  # using the countrycode package, add country name based on iso3c value
  dplyr::mutate(iso3c = countrycode::countrycode(country,"country.name","iso3c"))

# manually add country codes to missing country names: Czechoslovakia, Germany East, Kosovo, Serbia and Montenegro, South Vietnam, Yemen North, Yemen South, Yugoslavia
polity$iso3c[polity$country=="Czechoslovakia"] <- "CZE"
polity$iso3c[polity$country=="Germany East"] <- "DDR"
polity$iso3c[polity$country=="Kosovo"] <- "KSV"
polity$iso3c[polity$country=="Serbia and Montenegro"] <- "SRB"
polity$iso3c[polity$country=="South Vietnam"] <- "RVN"
polity$iso3c[polity$country=="Yemen North"] <- "YAR"
polity$iso3c[polity$country=="Yemen South"] <- "YPR"
polity$iso3c[polity$country=="Yugoslavia"] <- "YUG"

# recode missing values (-66, -77, -88) as NA
polity$democ[polity$democ %in% c(-66,-77,-88)] <- NA
polity$autoc[polity$autoc %in% c(-66,-77,-88)] <- NA
polity$polity2[polity$polity2 %in% c(-66,-77,-88)] <- NA
polity$parcomp[polity$parcomp %in% c(-66,-77,-88)] <- NA
polity$parreg[polity$parreg %in% c(-66,-77,-88)] <- NA
polity$xconst[polity$xconst %in% c(-66,-77,-88)] <- NA
polity$xrcomp[polity$xrcomp %in% c(-66,-77,-88)] <- NA
polity$xropen[polity$xropen %in% c(-66,-77,-88)] <- NA

# alters some entries to correct countries breaking apart                                                                                   
polity <- polity %>%
  filter(country != "Serbia and Montenegro" | year != 2006)
polity$iso3c[polity$iso3c=="RUS"&polity$year<=1991] <- "SOV"
polity$iso3c[polity$iso3c=="YUG"&polity$year>=1992] <- "SRB"
polity$iso3c[polity$iso3c=="DEU"&polity$year<=1990] <- "BRD"

# duplicates 2018 values for 2019, as 2019 values have yet to be published
polity <- rbind(polity,polity %>% dplyr::filter(year == 2018) %>% dplyr::mutate(year = 2019))

##### PCA for 3 components
# XCONST, XRCOMP, XROPEN
polity_pca3_df1 <- polity %>%
  dplyr::select(iso3c,year,xconst,xrcomp,xropen,polity2) %>%
  unique()

polity_pca3_df2 <- polity_pca3_df1 %>%
  na.omit()

polity_pca3 <- prcomp(~ xconst + xrcomp + xropen, data = polity_pca3_df2, retx = T, center = T, scale. = T)

polity_pca3_df2 <- polity_pca3_df2 %>%
  cbind(polity_pca3$x[,1]) %>%
  dplyr::select(-c(xconst,xrcomp,xropen,polity2)) %>%
  dplyr::rename(pc1 = 3) %>%
  unique()

polity_pca3_df1 <- dplyr::full_join(polity_pca3_df1,polity_pca3_df2,by=c("iso3c","year")) %>%
  dplyr::mutate(xpolity = xconst + xrcomp + xropen) %>%
  unique()

polity_complete <- polity_pca3_df1 %>%
  na.omit()

pol.glm <- glm(pc1 ~ polity2 + iso3c + year, data = polity_complete)
summary(pol.glm)
pol.est <- fitted(pol.glm) %>%
  as.data.frame()

polity_complete <- polity_complete %>%
  cbind(pol.est) %>%
  dplyr::select(-c(xconst,xrcomp,xropen,polity2,pc1,xpolity)) %>%
  dplyr::rename(estimate = 3) %>%
  unique()

polity_pca3_df3 <- dplyr::full_join(polity_pca3_df1,polity_complete,by=c("iso3c","year"))

polity_pca3_df3$pc1[is.na(polity_pca3_df3$pc1)] <- 0

polity_pca3_df3 <- polity_pca3_df3 %>%
  select(iso3c,year,pc1) %>%
  rename(polity = pc1) %>%
  mutate(politysq = polity^2)

# writes formatted dataframe as csv files
write.csv(polity_pca3_df3,"Data files/Formatted data files/polity.csv")
