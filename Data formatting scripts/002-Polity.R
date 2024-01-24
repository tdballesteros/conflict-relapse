# This script takes the Center for Systemic Peace's Polity V (2018) dataset and recalculates a polity
# variable using 3 of the 5 component scores of the Center for Systemic Peace's polity metric.

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

# manually add country codes to missing country names: Czechoslovakia, Germany East, Kosovo, Serbia and Montenegro,
# South Vietnam, Yemen North, Yemen South, Yugoslavia
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

# alters some entries to correct countries breaking apart or unifying
polity <- polity %>%
  dplyr::filter(country != "Serbia and Montenegro" | year != 2006)
polity$iso3c[polity$iso3c=="RUS"&polity$year<=1991] <- "SOV"
polity$iso3c[polity$iso3c=="YUG"&polity$year>=1992] <- "SRB"
polity$iso3c[polity$iso3c=="DEU"&polity$year<=1990] <- "BRD"

# duplicates 2018 values for 2019, as 2019 values have yet to be published
polity <- rbind(polity,
                polity %>%
                  dplyr::filter(year == 2018) %>%
                  dplyr::mutate(year = 2019)) %>%
  # drops country variable and moves iso3c variable first
  dplyr::select(-country) %>%
  dplyr::relocate(iso3c,.before = year) %>%
  # creates xpolity score summing the 3 components of interest (XCONST, XRCOMP, XROPEN)
  dplyr::mutate(xpolity = xconst + xrcomp + xropen)

# writes formatted dataframe as csv files
write.csv(polity,"Data files/Formatted data files/polity_full.csv")

##### PCA for 3 components (XCONST, XRCOMP, XROPEN)
# select 3 component variables, CSP's overall polity2 score, and the xpolity score
polity_pca3_df1 <- polity %>%
  dplyr::select(iso3c,year,xconst,xrcomp,xropen,polity2,xpolity) %>%
  unique()

# creates second df filtering out all country-years missing values
# all country-years missing a polity2 score are missing the 3 component scores,
# though some country-years missing the 3 component scores are not missing the polity2 score
polity_pca3_df2 <- polity_pca3_df1 %>%
  na.omit()

# PCA using the 3 component scores
polity_pca3 <- stats::prcomp(~ xconst + xrcomp + xropen, data = polity_pca3_df2, retx = T, center = T, scale. = T)

# replaces data with first principal component calculation, only for the country-years with no missing values
polity_pca3_df2 <- polity_pca3_df2 %>%
  cbind(polity_pca3$x[,1]) %>%
  dplyr::select(-c(xconst,xrcomp,xropen,polity2,xpolity)) %>%
  dplyr::rename(pc1 = 3) %>%
  unique()

# merges the dataset including missing values with the principal component dataset
polity_pca3_df1 <- dplyr::full_join(polity_pca3_df1,polity_pca3_df2,by=c("iso3c","year")) %>%
  unique()

# creates a df filtering out all country-years missing values
polity_complete <- polity_pca3_df1 %>%
  na.omit()

# creates a linear model predicting the principal component based on CSP's polity2 score, the country, and the year
pol.glm <- glm(pc1 ~ polity2 + iso3c + year, data = polity_complete)
summary(pol.glm)
pol.est <- fitted(pol.glm) %>%
  as.data.frame()

polity_complete <- polity_complete %>%
  # adds the estimated principal component from the linear model to the dataset
  cbind(pol.est) %>%
  dplyr::select(-c(xconst,xrcomp,xropen,polity2,pc1,xpolity)) %>%
  dplyr::rename(estimate = 3) %>%
  unique()

# adds the linear model estimate to the full PCA dataset
polity_pca3_df3 <- dplyr::full_join(polity_pca3_df1,polity_complete,by=c("iso3c","year"))

# codes all country-years missing a principal component score as 0
polity_pca3_df3$pc1[is.na(polity_pca3_df3$pc1)] <- 0

# selects only the principal component variable and creates a second variable squaring the
# principal component score
polity_pca3_df3 <- polity_pca3_df3 %>%
  dplyr::select(iso3c,year,pc1) %>%
  dplyr::rename(polity = pc1) %>%
  dplyr::mutate(politysq = polity^2)

# writes formatted dataframe as csv files
write.csv(polity_pca3_df3,"Data files/Formatted data files/polity.csv")
