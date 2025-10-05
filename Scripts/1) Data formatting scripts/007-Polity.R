
# This script takes the Center for Systemic Peace's Polity V (2018) dataset and calculates three
# additional metrics using three of the five component scores in the Polity V dataset. The variable
# xpolity is a simple sum of component scores XCONST, XRCOMP, and XROPEN, omitting PARCOMP and
# PARREG. The variable polity is calculated by taking the first component of the principal component
# analysis (PCA) on the XCONST, XRCOMP, and XROPEN variables. The variable politysq is the square of
# polity.

# Polity component scores of XCONST, XRCOMP, and XROPEN are used, while PARCOMP and PARREG are
# dropped.

# Center for Systemic Peace's codebook for Polity V:
## https://www.systemicpeace.org/inscr/p5manualv2018.pdf
# XRCOMP - Competitiveness of Executive Recruitment
# XROPEN - Openness of Executive Recruitment
# XCONST - Constraints on Chief Executive
# PARREG - Regulation of Participation
# PARCOMP - Competitiveness of pParticipation

# NOTE: fragmentation metric coded for 2000-2018; not available for historic data, though
# conceptually aligns with several concepts of state capacity


### load libraries ---------------------------------------------------------------------------------
library(readxl)
library(stats)
library(countrycode)
library(dplyr)


### load data file ---------------------------------------------------------------------------------
polity <- readxl::read_excel("Data files/Raw data files/p5v2018.xls")


### data formatting --------------------------------------------------------------------------------
polity <- polity %>%
  dplyr::filter(
    year >= 1946,
    # Ethiopia has two entries for 1993 - use scode ETH entry to correspond to the 1993- polity
    scode != "ETI" | year != 1993,
    # Yugoslavia has two entries for 1991 - use scode YUG entry to correspond to the -1991 polity
    scode != "YGS" | year != 1991,
    # Sudan has two entries for 2011 - use scode SDN entry to correspond to the 2011- polity
    scode != "SUD" | year != 2011,
    # Vietnam has two entries for 1976 - use scode VIE entry to correspond to the 1976- polity
    scode != "DRV" | year != 1976
  ) %>%
  dplyr::select(country, year, polity2, parcomp, parreg, xconst, xrcomp, xropen) %>%
  # using the countrycode package, add iso3c code based on country name
  dplyr::mutate(iso3c = dplyr::case_when(
    country == "Czechoslovakia" ~ "CZE",
    country == "Germany East" ~ "DDR",
    country == "Kosovo" ~ "KSV",
    country == "Serbia and Montenegro" ~ "SRB",
    country == "South Vietnam" ~ "RVN",
    country == "Yemen North" ~ "YAR",
    country == "Yemen South" ~ "YPR",
    country == "Yugoslavia" ~ "YUG",
    .default = countrycode::countrycode(country, "country.name", "iso3c")
  )) %>%
  # moves iso3c variable first
  dplyr::relocate(iso3c, .before = country)

# recode missing values (-66, -77, -88) as NA
polity$polity2[polity$polity2 %in% c(-66,-77,-88)] <- NA
polity$parcomp[polity$parcomp %in% c(-66,-77,-88)] <- NA
polity$parreg[polity$parreg %in% c(-66,-77,-88)] <- NA
polity$xconst[polity$xconst %in% c(-66,-77,-88)] <- NA
polity$xrcomp[polity$xrcomp %in% c(-66,-77,-88)] <- NA
polity$xropen[polity$xropen %in% c(-66,-77,-88)] <- NA

# alters some entries to correct countries breaking apart or unifying
polity <- polity %>%
  dplyr::filter(country != "Serbia and Montenegro" | year != 2006) # remove 2006 Serbia and Montenegro country-year (they are coded separately)
polity$iso3c[polity$iso3c == "RUS" & polity$year <= 1991] <- "SOV" # Soviet Union is coded as RUS; code Russia as USSR for 1991 and before
polity$iso3c[polity$iso3c == "YUG" & polity$year %in% c(1992:2002)] <- "SRB" # code Yugoslavia (Serbia and Montenegro) as Serbia for 1992-2002
polity$iso3c[polity$iso3c == "DEU" & polity$year <= 1990] <- "BRD" # West Germany is codes as DEU; code Germany as West German for 1990 and before

# duplicates 2018 values for 2019, as 2019 values have yet to be published
polity <- rbind(polity,
                polity %>%
                  dplyr::filter(year == 2018) %>%
                  dplyr::mutate(year = 2019)
                ) %>%
  # creates xpolity score summing the 3 components of interest (XCONST, XRCOMP, XROPEN)
  dplyr::mutate(xpolity = xconst + xrcomp + xropen) %>%
  unique()


### PCA on components ------------------------------------------------------------------------------
# creates dataset of only iso3c, year, and the three component metrics
polity_pca_df <- polity %>%
  dplyr::select(iso3c, year, xconst, xrcomp, xropen) %>%
  unique() %>%
  # omits country-years with missing component metric values
  na.omit()

# computes PCA
polity_pca <- stats::prcomp(~ xconst + xrcomp + xropen,
                            data = polity_pca_df,
                            retx = T, center = T, scale. = T)

polity_pca_df <- polity_pca_df %>%
  # combines principal component scores with PCA dataset
  cbind(polity_pca[["x"]]) %>%
  # drops second and third principal component scores as well as the three component metrics
  dplyr::select(-c(PC2, PC3, xconst, xrcomp, xropen)) %>%
  # renames first principal component as polity.pca
  dplyr::rename(polity.pca = PC1) %>%
  # creates polity.pca.sq variable, the square of polity.pca
  dplyr::mutate(polity.pca.sq = polity.pca^2)

# merges PCA score with full polity dataset
polity <- dplyr::full_join(polity, polity_pca_df, by = c("iso3c", "year")) %>%
  unique()

### linear model -----------------------------------------------------------------------------------
# Tests the polity.pca metric based on linear modelling using the country and year along with either
# polity2 or the three component metrics. The polity.pca metric is not meant to align fully with the
# polity2 metric, though they are expected to generally align.

#### polity2 linear model --------------------------------------------------------------------------
# creates a dataset for a linear model using the polity2 variable
polity_lm_df1 <- polity %>%
  dplyr::select(iso3c, year, polity2, polity.pca) %>%
  unique() %>%
  na.omit()

# creates a linear model predicting the principal component based on CSP's polity2 score, the
# country, and the year
pol_lm1 <- stats::glm(polity.pca ~ polity2 + iso3c + year, data = polity_lm_df1)
summary(pol_lm1)

# creates a dataframe with the fitted results of the linear model
pol_lm1_fitted <- stats::fitted(pol_lm1) %>%
  as.data.frame()

#### component variables linear model --------------------------------------------------------------
# creates a dataset for a linear model using the XCONST, XRCOMP, and XROPEN variables
polity_lm_df2 <- polity %>%
  dplyr::select(iso3c, year, xconst, xrcomp, xropen, polity.pca) %>%
  unique() %>%
  na.omit()

# creates a linear model predicting the principal component based on CSP's XCONST, XRCOMP, and
# XROPEN scores; the country; and the year
pol_lm2 <- stats::glm(polity.pca ~ xconst + xrcomp + xropen + iso3c + year, data = polity_lm_df2)
summary(pol_lm2)

# creates a dataframe with the fitted results of the linear model
pol_lm2_fitted <- stats::fitted(pol_lm2) %>%
  as.data.frame()

### final dataset formatting -----------------------------------------------------------------------
# codes all country-years missing polity.pca, polity.pca.sq, xpolity, and polity2 as 0
polity$polity.pca[is.na(polity$polity.pca)] <- 0
polity$polity.pca.sq[is.na(polity$polity.pca.sq)] <- 0
polity$xpolity[is.na(polity$xpolity)] <- 0
polity$polity2[is.na(polity$polity2)] <- 0

# selects only the iso3c, county, year, three constructed polity variables, and CSP's five component
# and overall polity2 variables
polity <- polity %>%
  dplyr::select(iso3c, country, year, polity.pca, polity.pca.sq, xpolity, parcomp, parreg, xconst,
                xrcomp, xropen, polity2)

### write data -------------------------------------------------------------------------------------
# writes formatted dataframe as csv files
write.csv(polity, "Data files/Formatted data files/polity.csv", row.names = FALSE)
