
# This script creates a military capacity index score for each country-year.


### load libraries ----------------------------------------------------------------------
library(countrycode)
library(lavaan)
library(semPlot)
library(dplyr)
library(tidyr)


### load data --------------------------------------------------------------------------------------
mil.metrics <- read.csv("Data files/Formatted data files/Military_data.csv")

population <- read.csv("Data files/Formatted data files/population.csv")
gdp <- read.csv("Data files/Formatted data files/gdp.csv")
energy_and_steel <- read.csv("Data files/Formatted data files/energy_and_steel.csv")
land_area <- read.csv("Data files/Formatted data files/population_density.csv") %>%
  dplyr::select(iso3c, year, land_area)
cyears <- read.csv("Data files/Formatted data files/country_years.csv")


### construct sem ----------------------------------------------------------------------------------
MC_sem_data <- mil.metrics %>%
  dplyr::left_join(land_area, by = c("iso3c", "year")) %>%
  dplyr::left_join(cyears, by = c("iso3c", "year")) %>%
  dplyr::mutate(
    PERPERLOGSQK = mil.personnel.cow / log(land_area)
  ) %>%
  dplyr::filter(
    cn == 1,
    iso3c %!in% c(
      "DMA","GRD","LCA","VCT","ATG","KNA","MCO","LIE","AND","SMR","ISL","MDV","VUT",
      "SLB","KIR","TUV","TON","NRU","MHL","PLW","FSM","WSM","PSE",
      "BHS","BRB","BRN","BTN","CPV","MUS","SYC","STP" ###
    ), # 11,286 to 9,951
    # drop Inf/-Inf values
    dplyr::if_all(c(mil.expenditure.perc.gdp.cow.pwt,
                    mil.personnel.per.capita.cow.un, mil.expenditure.per.capita.cow.un, PERPERLOGSQK), is.finite
    ), # 9,951 to 9,771
    mil.expenditure.cow.ef == 0, # 9,771 to 8,190
    mil.personnel.cow.ef == 0 # 8,190 to 8,136
  ) %>%
  dplyr::select(
    iso3c, year,
    EXPPERGDP = mil.expenditure.perc.gdp.cow.pwt,
    PERPERCAP = mil.personnel.per.capita.cow.un,
    EXPPERCAP = mil.expenditure.per.capita.cow.un,
    PERPERLOGSQK
  ) %>%
  dplyr::mutate(
    EXPPERGDP_scaled = scale(EXPPERGDP),
    PERPERCAP_scaled = scale(PERPERCAP),
    EXPPERCAP_scaled = scale(EXPPERCAP),
    PERPERLOGSQK_scaled = scale(PERPERLOGSQK) 
  )

sem_model <- '
  MC =~ EXPPERGDP_scaled + PERPERCAP_scaled + EXPPERCAP_scaled + PERPERLOGSQK_scaled

  PERPERCAP_scaled ~~ EXPPERCAP_scaled
'

cor(MC_sem_data[,c("EXPPERGDP_scaled", "PERPERCAP_scaled", "PERPERLOGSQK_scaled", "EXPPERCAP_scaled")])

fit <- sem(sem_model, data = MC_sem_data)
summary(fit, standardized = TRUE, fit.measures = TRUE)
# CFI = 0.997 | TLI = 0.983 | RMSEA = 0.044

latent_scores <- lavPredict(fit)
latent_df <- as.data.frame(latent_scores)
MC_sem_data_appended1 <- MC_sem_data %>%
  bind_cols(latent_df) %>%
  dplyr::select(iso3c, year, MC, EXPPERGDP, PERPERCAP, EXPPERCAP, PERPERLOGSQK)

semPaths(fit, 
         whatLabels = "std",        # Show standardized coefficients on the arrows
         layout = "tree2",          # Tree layout (predictors left, latent center, indicators right)
         edge.label.cex = 1,        # Font size for labels
         curvePivot = TRUE,         # Makes the covariance curves look cleaner
         sizeMan = 10,              # Size of the manifest (square) boxes
         sizeLat = 10,              # Size of the latent (circle) boxes
         edge.color = "black")      # Color of the paths

# pull country-years in the original model
MC_contruction_country_years <- MC_sem_data %>%
  dplyr::select(iso3c, year) %>%
  dplyr::mutate(model_data = 1)



# pull means / sds from data pre-scaling
orig_means <- colMeans(MC_sem_data[, c("EXPPERGDP", "PERPERCAP", "PERPERLOGSQK", "EXPPERCAP")], na.rm = TRUE)
orig_sds <- apply(MC_sem_data[, c("EXPPERGDP", "PERPERCAP", "PERPERLOGSQK", "EXPPERCAP")], 2, sd, na.rm = TRUE)

MC_metric <- mil.metrics %>%
  dplyr::left_join(land_area, by = c("iso3c", "year")) %>%
  dplyr::mutate(
    PERPERLOGSQK = mil.personnel.cow / log(land_area)
  ) %>%
  dplyr::select(
    iso3c, year,
    EXPPERGDP = mil.expenditure.perc.gdp.cow.pwt,
    PERPERCAP = mil.personnel.per.capita.cow.un,
    EXPPERCAP = mil.expenditure.per.capita.cow.un,
    PERPERLOGSQK
  ) %>%
  dplyr::filter(
    !is.na(EXPPERGDP),
    !is.na(PERPERCAP),
    !is.na(EXPPERCAP),
    !is.na(PERPERLOGSQK)
  ) # 11,286 to 11,106

# apply same scaling to new data
MC_metric_scaled <- MC_metric %>%
  dplyr::mutate(
    EXPPERGDP_scaled = (EXPPERGDP - orig_means["EXPPERGDP"]) / orig_sds["EXPPERGDP"],
    PERPERCAP_scaled = (PERPERCAP - orig_means["PERPERCAP"]) / orig_sds["PERPERCAP"],
    PERPERLOGSQK_scaled = (PERPERLOGSQK - orig_means["PERPERLOGSQK"]) / orig_sds["PERPERLOGSQK"],
    EXPPERCAP_scaled = (EXPPERCAP - orig_means["EXPPERCAP"]) / orig_sds["EXPPERCAP"]
  )

# fit new data to the model
mc_scores <- lavPredict(fit, newdata = MC_metric_scaled)

# Add the scores back to your dataframe
MC_metric$MC_score <- as.numeric(mc_scores)

# add in which country-years were estimates vs. used to make the model
MC_metric <- MC_metric %>%
  dplyr::full_join(MC_contruction_country_years, by = c("iso3c", "year"))




# 
# ### filter data ------------------------------------------------------------------------------------
# MC_sem_data <- mil.metrics %>%
#   dplyr::left_join(population, by = c("iso3c", "year")) %>%
#   dplyr::left_join(gdp, by = c("iso3c", "year")) %>%
#   dplyr::left_join(energy_and_steel, by = c("iso3c", "year")) %>%
#   dplyr::left_join(land_area, by = c("iso3c", "year")) %>%
#   dplyr::mutate(
#     PERPERLOGSQK = mil.personnel.cow / log(land_area),
#     PERPERSQK = mil.personnel.cow / land_area,
#     LOGPERPERSQK = log(PERPERSQK)
#   ) %>%
#   dplyr::filter(
#     iso3c %!in% c(
#       "DMA","GRD","LCA","VCT","ATG","KNA","MCO","LIE","AND","SMR","ISL","MDV","VUT",
#       "SLB","KIR","TUV","TON","NRU","MHL","PLW","FSM","WSM","PSE",
#       "BHS","BRB","BRN","BTN","CPV","MUS","SYC","STP" ###
#     ), # 11286 to 9951
#     # drop Inf/-Inf values
#     dplyr::if_all(c(mil.expenditure.perc.gdp.cow.pwt,
#                     mil.personnel.per.capita.cow.un, PERPERLOGSQK, PERPERSQK, LOGPERPERSQK, pec, un.pop), is.finite
#     ), # 9951 to 9756
#     mil.expenditure.cow.ef == 0, # 9756 to 8215
#     mil.personnel.cow.ef == 0 # 8215 to 8161
#   ) %>%
#   dplyr::mutate(
#     PECPERCAP = pec / un.pop
#     ) %>%
#   dplyr::select(
#     iso3c, year,
#     EXPPERGDP = mil.expenditure.perc.gdp.cow.pwt,
#     PERPERCAP = mil.personnel.per.capita.cow.un,
#     EXPPERCAP = mil.expenditure.per.capita.cow.un,
#     EXPPERPER = mil.expenditure.per.personnel.cow.cow,
#     PERPERLOGSQK, PECPERCAP, PERPERSQK, LOGPERPERSQK
#   ) %>%
#   dplyr::mutate(
#     EXPPERGDP_scaled = scale(EXPPERGDP),
#     PERPERCAP_scaled = scale(PERPERCAP),
#     EXPPERCAP_scaled = scale(EXPPERCAP),
#     EXPPERPER_scaled = scale(EXPPERPER),
#     PERPERLOGSQK_scaled = scale(PERPERLOGSQK),
#     PECPERCAP_scaled = scale(PECPERCAP),
#     PERPERSQK_scaled = scale(PERPERSQK),
#     LOGPERPERSQK_scaled = scale(LOGPERPERSQK)
#   )
# 
# sem_model <- '
#   MC =~ PERPERCAP_scaled + EXPPERGDP_scaled + PERPERLOGSQK_scaled + PECPERCAP_scaled # + EXPPERCAP_scaled + EXPPERPER_scaled
#   
#   # Force error variances to be positive (The "~~" is the error variance)
#   # PERPERCAP ~~ a*PERPERCAP; a > 0.01
#   # PERPERLOGSQK ~~ b*PERPERLOGSQK; b > 0.01
#   # EXPPERGDP ~~ c*EXPPERGDP; c > 0.01
#   # PECPERCAP ~~ d*PECPERCAP; d > 0.01
# 
# '
# # CFI = 0.974 | TLI - 0.921 | RMSEA = 0.080
# 
# sem_model <- '
#   MC =~ EXPPERGDP_scaled + PERPERCAP_scaled + PERPERLOGSQK_scaled + EXPPERCAP_scaled
#   # Force error variances to be positive (The "~~" is the error variance)
#   # PERPERCAP ~~ a*PERPERCAP; a > 0.01
#   # PERPERLOGSQK ~~ b*PERPERLOGSQK; b > 0.01
#   # EXPPERGDP ~~ c*EXPPERGDP; c > 0.01
#   # PECPERCAP ~~ d*PECPERCAP; d > 0.01
# '
# # CFI = 0.992 | TLI = 0.976 | RMSEA = 0.052
# 
# sem_model <- '
#   MC =~ EXPPERGDP_scaled + PERPERCAP_scaled + PERPERLOGSQK_scaled + EXPPERCAP_scaled
# 
#   PERPERCAP_scaled ~~ EXPPERCAP_scaled
# '
# # CFI = 0.997 | TLI = 0.979 | RMSEA = 0.048
# 
# cor(MC_sem_data[,c("EXPPERGDP_scaled", "PERPERCAP_scaled", "EXPPERCAP_scaled", "EXPPERPER_scaled",
#                    "PERPERLOGSQK_scaled", "PECPERCAP_scaled", "PERPERSQK_scaled",
#                    "LOGPERPERSQK_scaled")])
# 
# cor(MC_sem_data[,c("EXPPERGDP_scaled", "PERPERCAP_scaled", "PERPERLOGSQK_scaled", "EXPPERCAP_scaled")])
# 
# 
# MC_sem_data_cor <- cor(MC_sem_data[,c(3:ncol(MC_sem_data))])
# 
# fit <- sem(sem_model, data = MC_sem_data)
# 
# 
# summary(fit, standardized = TRUE, fit.measures = TRUE)
# 
# latent_scores <- lavPredict(fit)
# latent_df <- as.data.frame(latent_scores)
# MC_sem_data_appended1 <- MC_sem_data %>%
#   bind_cols(latent_df) %>%
#   dplyr::select(iso3c, year, MC, PERPERCAP, EXPPERGDP, PERPERLOGSQK, PECPERCAP)
# 
# semPaths(fit, 
#          whatLabels = "std",        # Show standardized coefficients on the arrows
#          layout = "tree2",          # Tree layout (predictors left, latent center, indicators right)
#          edge.label.cex = 1,        # Font size for labels
#          curvePivot = TRUE,         # Makes the covariance curves look cleaner
#          sizeMan = 10,              # Size of the manifest (square) boxes
#          sizeLat = 10,              # Size of the latent (circle) boxes
#          edge.color = "black")      # Color of the paths
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ### filter data ------------------------------------------------------------------------------------
# MC_sem_data <- mil.metrics %>%
#   dplyr::left_join(population, by = c("iso3c", "year")) %>%
#   dplyr::left_join(gdp, by = c("iso3c", "year")) %>%
#   dplyr::left_join(energy_and_steel, by = c("iso3c", "year")) %>%
#   dplyr::left_join(land_area, by = c("iso3c", "year")) %>%
#   dplyr::mutate(
#     PERPERLOGSQK = mil.personnel.cow / log(land_area),
#     PERPERSQK = mil.personnel.cow / land_area
#   ) %>%
#   dplyr::filter(
#     iso3c %!in% c(
#       "DMA","GRD","LCA","VCT","ATG","KNA","MCO","LIE","AND","SMR","ISL","MDV","VUT",
#       "SLB","KIR","TUV","TON","NRU","MHL","PLW","FSM","WSM","PSE",
#       "BHS","BRB","BRN","BTN","CPV","MUS","SYC","STP" ###
#     ), # 11286 to 9951
#     # drop Inf/-Inf values
#     dplyr::if_all(c(mil.expenditure.per.capita.cow.un, mil.expenditure.perc.gdp.cow.pwt,
#                     mil.personnel.per.capita.cow.un, gdp.per.mil.personnel.pwt.cow,
#                     mil.expenditure.per.personnel.cow.cow, PERPERLOGSQK, PERPERSQK), is.finite
#                   ), # 9951 to 9756
#     mil.expenditure.cow.ef == 0, # 9756 to 8215
#     mil.personnel.cow.ef == 0 # 8215 to 8161
#     ) %>%
#   dplyr::select(
#     iso3c, year, mil.expenditure.cow, mil.personnel.cow,
#     EXPPERCAP = mil.expenditure.per.capita.cow.un,
#     EXPPERGDP = mil.expenditure.perc.gdp.cow.pwt,
#     PERPERCAP = mil.personnel.per.capita.cow.un,
#     GDPPERPER = gdp.per.mil.personnel.pwt.cow,
#     EXPPERPER = mil.expenditure.per.personnel.cow.cow,
#     PERPERLOGSQK, PERPERSQK, un.pop, gdp.pwt.est, pec, land_area
#   ) %>%
#   dplyr::mutate(
#     PERPERCAPK = 1000 * PERPERCAP,
#     mil.expenditure.cow.ln = ifelse(is.infinite(log(mil.expenditure.cow)), 0, log(mil.expenditure.cow)),
#     un.pop.ln = log(un.pop),
#     gdp.pwt.est.ln = log(gdp.pwt.est),
#     pec.ln = ifelse(is.infinite(log(pec)), 0, log(pec)),
#     land_area.ln = log(land_area),
#     PECPERCAP = pec / un.pop,
#     across(3:last_col(), ~ as.numeric(scale(.)))
#   )
# 
# 
# sem_model <- '
#   MC =~ PERPERCAP + EXPPERGDP + PERPERLOGSQK + PECPERCAP # + EXPPERCAP + EXPPERPER
#   
#   # Force error variances to be positive (The "~~" is the error variance)
#   PERPERCAP ~~ a*PERPERCAP; a > 0.01
#   PERPERLOGSQK ~~ b*PERPERLOGSQK; b > 0.01
#   # EXPPERPER ~~ c*EXPPERPER; c > 0.01
#   EXPPERGDP ~~ d*EXPPERGDP; d > 0.01
#   # EXPPERCAP ~~ e*EXPPERCAP; e > 0.01
#   PECPERCAP ~~ f*PECPERCAP; f > 0.01
#   
#   # PERPERCAP ~~ PERPERLOGSQK
#   # PERPERCAP ~~ EXPPERPER
#   # EXPPERPER ~~ EXPPERGDP
#   # EXPPERPER ~~ EXPPERCAP
#   # EXPPERGDP ~~ EXPPERCAP
# '
# # CFI = 0.974 | TLI - 0.921 | RMSEA = 0.080
# 
# MC_sem_data_cor <- cor(MC_sem_data[,c(3:ncol(MC_sem_data))])
# cor(MC_sem_data[,c("PERPERCAP", "EXPPERGDP", "PERPERLOGSQK", "PECPERCAP")], use = "complete.obs")
# 
# fit <- sem(sem_model, data = MC_sem_data)
# varTable(fit)
# lavInspect(fit, "theta")
# 
# summary(fit, standardized = TRUE, fit.measures = TRUE)
# 
# 
# latent_scores <- lavPredict(fit)
# latent_df <- as.data.frame(latent_scores)
# MC_sem_data_appended1 <- MC_sem_data %>%
#   bind_cols(latent_df) %>%
#   dplyr::select(iso3c, year, MC, PERPERCAP, EXPPERGDP, PERPERLOGSQK, PECPERCAP)
# 
# semPaths(fit, 
#          whatLabels = "std",        # Show standardized coefficients on the arrows
#          layout = "tree2",          # Tree layout (predictors left, latent center, indicators right)
#          edge.label.cex = 1,        # Font size for labels
#          curvePivot = TRUE,         # Makes the covariance curves look cleaner
#          sizeMan = 10,              # Size of the manifest (square) boxes
#          sizeLat = 10,              # Size of the latent (circle) boxes
#          edge.color = "black")      # Color of the paths
# 
# 
# 
# 
# 
# 
# sem_model2 <- '
#   MC =~ mil.expenditure.cow.ln + mil.personnel.cow + pec
#   MC ~ un.pop.ln + gdp.pwt.est.ln + land_area.ln
#   un.pop.ln ~~ gdp.pwt.est.ln
#   un.pop.ln ~~ land_area.ln
#   gdp.pwt.est.ln ~~ land_area.ln
#   gdp.pwt.est.ln ~~ pec
#   un.pop.ln ~~ mil.expenditure.cow.ln
# 
#   # Force error variances to be positive (The "~~" is the error variance)
#   mil.expenditure.cow.ln ~~ a*mil.expenditure.cow.ln; a > 0.01
#   mil.personnel.cow ~~ b*mil.personnel.cow; b > 0.01
#   pec ~~ c*pec; c > 0.01
# '
# 
# fit2 <- sem(sem_model2, data = MC_sem_data, std.lv = TRUE)
# varTable(fit2)
# 
# summary(fit2, standardized = TRUE, fit.measures = TRUE)
# 
# latent_scores <- lavPredict(fit2)
# latent_df <- as.data.frame(latent_scores)
# index_ccpu_sem_data2_appended <- index_ccpu_sem_data2 %>%
#   bind_cols(latent_df)
# 
# semPaths(fit2, 
#          whatLabels = "std",        # Show standardized coefficients on the arrows
#          layout = "tree2",          # Tree layout (predictors left, latent center, indicators right)
#          edge.label.cex = 1,        # Font size for labels
#          curvePivot = TRUE,         # Makes the covariance curves look cleaner
#          sizeMan = 10,              # Size of the manifest (square) boxes
#          sizeLat = 10,              # Size of the latent (circle) boxes
#          edge.color = "black")      # Color of the paths
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ### add logarithmic metrics ----------------------------------------------------------------------
# mil.metrics <- mil.metrics %>%
#   dplyr::left_join(population, by = c("iso3c", "year")) %>%
#   dplyr::left_join(gdp, by = c("iso3c", "year")) %>%
#   dplyr::left_join(energy_and_steel, by = c("iso3c", "year")) %>%
#   dplyr::left_join(land_area, by = c("iso3c", "year")) %>%
#   dplyr::mutate(
#     mil.expenditure.cow.ln = log(mil.expenditure.cow),
#     mil.expenditure.cow.alt.ln = log(mil.expenditure.cow.alt),
#     mil.personnel.cow.ln = log(mil.personnel.cow),
#     mil.expenditure.wmeat.ln = log(mil.expenditure.wmeat),
#     mil.personnel.wmeat.ln = log(mil.personnel.wmeat),
#     mil.expenditure.sipri.ln = log(mil.expenditure.sipri),
#     mil.expenditure.per.capita.cow.un.ln = log(mil.expenditure.per.capita.cow.un),
#     mil.expenditure.per.capita.cow.alt.un.ln = log(mil.expenditure.per.capita.cow.alt.un),
#     mil.expenditure.per.capita.cow.cow.ln = log(mil.expenditure.per.capita.cow.cow),
#     mil.expenditure.per.capita.cow.alt.cow.ln = log(mil.expenditure.per.capita.cow.alt.cow),
#     mil.expenditure.per.capita.wmeat.un.ln = log(mil.expenditure.per.capita.wmeat.un),
#     mil.expenditure.per.capita.wmeat.cow.ln = log(mil.expenditure.per.capita.wmeat.cow),
#     mil.expenditure.per.capita.sipri.un.ln = log(mil.expenditure.per.capita.sipri.un),
#     mil.expenditure.per.capita.sipri.cow.ln = log(mil.expenditure.per.capita.sipri.cow),
#     mil.expenditure.perc.gdp.cow.pwt.ln = log(mil.expenditure.perc.gdp.cow.pwt),
#     mil.expenditure.perc.gdp.cow.alt.pwt.ln = log(mil.expenditure.perc.gdp.cow.alt.pwt),
#     mil.expenditure.perc.gdp.cow.gl.ln = log(mil.expenditure.perc.gdp.cow.gl),
#     mil.expenditure.perc.gdp.cow.alt.gl.ln = log(mil.expenditure.perc.gdp.cow.alt.gl),
#     mil.expenditure.perc.gdp.wmeat.pwt.ln = log(mil.expenditure.perc.gdp.wmeat.pwt),
#     mil.expenditure.perc.gdp.wmeat.gl.ln = log(mil.expenditure.perc.gdp.wmeat.gl),
#     mil.expenditure.perc.gdp.sipri.pwt.ln = log(mil.expenditure.perc.gdp.sipri.pwt),
#     mil.expenditure.perc.gdp.sipri.gl.ln = log(mil.expenditure.perc.gdp.sipri.gl),
#     mil.expenditure.per.personnel.cow.cow.ln = log(mil.expenditure.per.personnel.cow.cow),
#     mil.expenditure.per.personnel.cow.alt.cow.ln = log(mil.expenditure.per.personnel.cow.alt.cow),
#     mil.expenditure.per.personnel.cow.wmeat.ln = log(mil.expenditure.per.personnel.cow.wmeat),
#     mil.expenditure.per.personnel.cow.alt.wmeat.ln = log(mil.expenditure.per.personnel.cow.alt.wmeat),
#     mil.expenditure.per.personnel.wmeat.cow.ln = log(mil.expenditure.per.personnel.wmeat.cow),
#     mil.expenditure.per.personnel.wmeat.wmeat.ln = log(mil.expenditure.per.personnel.wmeat.wmeat),
#     mil.expenditure.per.personnel.sipri.cow.ln = log(mil.expenditure.per.personnel.sipri.cow),
#     mil.expenditure.per.personnel.sipri.wmeat.ln = log(mil.expenditure.per.personnel.sipri.wmeat),
#     mil.personnel.per.capita.cow.un.ln = log(mil.personnel.per.capita.cow.un),
#     mil.personnel.per.capita.cow.cow.ln = log(mil.personnel.per.capita.cow.cow),
#     mil.personnel.per.capita.wmeat.un.ln = log(mil.personnel.per.capita.wmeat.un),
#     mil.personnel.per.capita.wmeat.cow.ln = log(mil.personnel.per.capita.wmeat.cow),
#     gdp.per.1000.mil.personnel.pwt.cow = gdp.gl.est / (mil.personnel.cow / 1000),
#     gdp.per.1000.mil.personnel.pwt.cow.ln = log(gdp.per.1000.mil.personnel.pwt.cow),
#     gdp.per.mil.personnel.pwt.cow.ln = log(gdp.per.mil.personnel.pwt.cow),
#     gdp.per.mil.personnel.pwt.wmeat.ln = log(gdp.per.mil.personnel.pwt.wmeat),
#     gdp.per.mil.personnel.gl.cow.ln = log(gdp.per.mil.personnel.gl.cow),
#     gdp.per.mil.personnel.gl.wmeat.ln = log(gdp.per.mil.personnel.gl.wmeat),
#     mil.personnel.cow.per.sqkm = mil.personnel.cow / land_area,
#     mil.personnel.cow.per.sqkm.ln = log(mil.personnel.cow.per.sqkm),
#     exp.log.per.mil.personnel = log(mil.expenditure.cow) / mil.personnel.cow,
#     exp.log.per.1000.mil.personnel = log(mil.expenditure.cow) / (mil.personnel.cow / 1000),
#     # if 0 military personnel, 0
#     exp.log.per.mil.personnel = ifelse(is.infinite(exp.log.per.mil.personnel), 0, exp.log.per.mil.personnel),
#     exp.log.per.1000.mil.personnel = ifelse(is.infinite(exp.log.per.1000.mil.personnel), 0, exp.log.per.1000.mil.personnel),
#     exp.log.per.mil.personnel.ln = log(exp.log.per.mil.personnel),
#     exp.log.per.1000.mil.personnel.ln = log(exp.log.per.1000.mil.personnel),
#     gdp.log.per.mil.personnel = log(gdp.gl.est) / mil.personnel.cow,
#     gdp.log.per.mil.personnel.ln = log(gdp.log.per.mil.personnel)
#   )
# 
# # # which countries have Inf values?
# # mil.metrics.inf <- mil.metrics %>%
# #   dplyr::select(
# #     iso3c, year, dplyr::all_of(ccpu_vars_pca)
# #   ) %>%
# #   dplyr::filter(
# #     is.infinite(mil.expenditure.per.capita.cow.un.ln) |
# #       is.infinite(mil.expenditure.perc.gdp.cow.pwt.ln) |
# #       is.infinite(mil.expenditure.per.personnel.cow.cow.ln) |
# #       is.infinite(mil.personnel.per.capita.cow.un.ln) |
# #       is.infinite(gdp.per.mil.personnel.pwt.cow.ln)
# #   ) %>%
# #   dplyr::group_by(iso3c) %>%
# #   dplyr::tally()
# #    
# # mil.metrics[as.matrix(mil.metrics) == Inf]  <- 0
# # mil.metrics[as.matrix(mil.metrics) == -Inf]  <- 0
# # 
# # mil.metrics <- do.call(data.frame,lapply(mil.metrics, function(x) replace(x, is.infinite(x),0)))
# 
# 
# ### Index CCPU -------------------------------------------------------------------------------------
# # military expenditure - COW
# # military personnel - COW
# # GDP - PWT
# # Population - UN
# 
# ccpu_vars_all <- c(
#   "mil.expenditure.cow","mil.personnel.cow","mil.expenditure.per.capita.cow.un",
#   "mil.expenditure.perc.gdp.cow.pwt","mil.expenditure.per.personnel.cow.cow",
#   "mil.personnel.per.capita.cow.un","gdp.per.mil.personnel.pwt.cow","mil.expenditure.cow.ln",
#   "mil.personnel.cow.ln","mil.expenditure.per.capita.cow.un.ln","mil.expenditure.perc.gdp.cow.pwt.ln",
#   "mil.expenditure.per.personnel.cow.cow.ln","mil.personnel.per.capita.cow.un.ln",
#   "gdp.per.mil.personnel.pwt.cow.ln", "mil.personnel.cow.per.sqkm"
#   )
# 
# ccpu_vars_pca <- c(
#   "mil.expenditure.per.capita.cow.un.ln","mil.expenditure.perc.gdp.cow.pwt.ln",
#   "mil.expenditure.per.personnel.cow.cow.ln","mil.personnel.per.capita.cow.un.ln",
#   "gdp.per.mil.personnel.pwt.cow.ln", "mil.personnel.cow.per.sqkm"
#   )
# 
# # index_ccpu_data <- index_ccpu_data %>%
# #   dplyr::mutate(
# #     mil.expenditure.per.capita.cow.un.ln <- dplyr::case_when(
# #       is.infinite(mil.expenditure.per.capita.cow.un.ln)  ~ 0,
# #       .default = mil.expenditure.per.capita.cow.un.ln
# #     )
# #   ) %>%
# #   dplyr::filter(
# #     is.infinite(mil.expenditure.per.capita.cow.un.ln)
# #   )
# # 
# # index_ccpu_data$mil.expenditure.per.capita.cow.un.ln[is.infinite(index_ccpu_data$mil.expenditure.per.capita.cow.un.ln)] <- 0
# # index_ccpu_data$mil.expenditure.perc.gdp.cow.pwt.ln[is.infinite(index_ccpu_data$mil.expenditure.perc.gdp.cow.pwt.ln)] <- 0
# # index_ccpu_data$mil.expenditure.per.personnel.cow.cow.ln[is.infinite(index_ccpu_data$mil.expenditure.per.personnel.cow.cow.ln)] <- 0
# # index_ccpu_data$mil.personnel.per.capita.cow.un.ln[is.infinite(index_ccpu_data$mil.personnel.per.capita.cow.un.ln)] <- 0
# # index_ccpu_data$gdp.per.mil.personnel.pwt.cow.ln[is.infinite(index_ccpu_data$gdp.per.mil.personnel.pwt.cow.ln)] <- 0
# # 
# # table(is.infinite(index_ccpu_data$mil.expenditure.per.capita.cow.un.ln), useNA='always')
# 
# index_ccpu_data <- mil.metrics %>%
#   dplyr::select(
#     iso3c, year, dplyr::all_of(ccpu_vars_pca)
#     ) %>%
#   dplyr::filter(
#     iso3c %!in% c(
#       "DMA","GRD","LCA","VCT","ATG","KNA","MCO","LIE","AND","SMR","ISL","MDV","VUT",
#       "SLB","KIR","TUV","TON","NRU","MHL","PLW","FSM","WSM","PSE",
#       "BHS","BRB","BRN","BTN","CPV","MUS","SYC","STP" ###
#     )
#   )
# 
# # standardize: (x - μ) / σ
# 
# # mil.expenditure.per.capita.cow.un.ln
# mu_var1 <- mean(index_ccpu_data$mil.expenditure.per.capita.cow.un.ln, na.rm = TRUE)
# sd_var1 <- sd(index_ccpu_data$mil.expenditure.per.capita.cow.un.ln, na.rm = TRUE)
# 
# # mil.expenditure.perc.gdp.cow.pwt.ln
# mu_var2 <- mean(index_ccpu_data$mil.expenditure.perc.gdp.cow.pwt.ln, na.rm = TRUE)
# sd_var2 <- sd(index_ccpu_data$mil.expenditure.perc.gdp.cow.pwt.ln, na.rm = TRUE)
# 
# # mil.expenditure.per.personnel.cow.cow.ln
# mu_var3 <- mean(index_ccpu_data$mil.expenditure.per.personnel.cow.cow.ln, na.rm = TRUE)
# sd_var3 <- sd(index_ccpu_data$mil.expenditure.per.personnel.cow.cow.ln, na.rm = TRUE)
# 
# # mil.personnel.per.capita.cow.un.ln
# mu_var4 <- mean(index_ccpu_data$mil.personnel.per.capita.cow.un.ln, na.rm = TRUE)
# sd_var4 <- sd(index_ccpu_data$mil.personnel.per.capita.cow.un.ln, na.rm = TRUE)
# 
# # gdp.per.mil.personnel.pwt.cow.ln
# mu_var5 <- mean(index_ccpu_data$gdp.per.mil.personnel.pwt.cow.ln, na.rm = TRUE)
# sd_var5 <- sd(index_ccpu_data$gdp.per.mil.personnel.pwt.cow.ln, na.rm = TRUE)
# 
# index_ccpu_data <- index_ccpu_data %>%
#   dplyr::mutate(
#     mil.expenditure.per.capita.cow.un.ln = (mil.expenditure.per.capita.cow.un.ln - mu_var1) / sd_var1,
#     mil.expenditure.perc.gdp.cow.pwt.ln = (mil.expenditure.perc.gdp.cow.pwt.ln - mu_var2) / sd_var2,
#     mil.expenditure.per.personnel.cow.cow.ln = (mil.expenditure.per.personnel.cow.cow.ln - mu_var3) / sd_var3,
#     mil.personnel.per.capita.cow.un.ln = (mil.personnel.per.capita.cow.un.ln - mu_var4) / sd_var4,
#     gdp.per.mil.personnel.pwt.cow.ln = (gdp.per.mil.personnel.pwt.cow.ln - mu_var5) / sd_var5
#   )
# 
# 
# # %>%
# #   tidyr::drop_na(dplyr::all_of(ccpu_vars_pca)) %>%
# #   dplyr::filter(
# #     mil.expenditure.per.capita.cow.un.ln %!in% c(-Inf,Inf),
# #     mil.expenditure.perc.gdp.cow.pwt.ln %!in% c(-Inf,Inf),
# #     mil.expenditure.per.personnel.cow.cow.ln %!in% c(-Inf,Inf),
# #     mil.personnel.per.capita.cow.un.ln %!in% c(-Inf,Inf),
# #     gdp.per.mil.personnel.pwt.cow.ln %!in% c(-Inf,Inf)
# #   )
# # 
# # index_ccpu_cor <- cor(index_ccpu_data %>%
# #                         dplyr::select(-c(iso3c,year)))
# 
# ccpu_pca <- stats::prcomp(
#   ~  mil.expenditure.per.capita.cow.un.ln + mil.expenditure.perc.gdp.cow.pwt.ln +
#     mil.expenditure.per.personnel.cow.cow.ln + mil.personnel.per.capita.cow.un.ln +
#     gdp.per.mil.personnel.pwt.cow.ln,
#   data = index_ccpu_data, retx = F, center = F, scale. = F)
# 
# index_ccpu <- index_ccpu_data %>%
#   dplyr::mutate(
#     mil.expenditure.per.capita.cow.un.ln = ccpu_pca$rotation[1,1] * mil.expenditure.per.capita.cow.un.ln,
#     mil.expenditure.perc.gdp.cow.pwt.ln = ccpu_pca$rotation[2,1] * mil.expenditure.perc.gdp.cow.pwt.ln,
#     mil.expenditure.per.personnel.cow.cow.ln = ccpu_pca$rotation[3,1] * mil.expenditure.per.personnel.cow.cow.ln,
#     mil.personnel.per.capita.cow.un.ln = ccpu_pca$rotation[4,1] * mil.personnel.per.capita.cow.un.ln,
#     gdp.per.mil.personnel.pwt.cow.ln = ccpu_pca$rotation[5,1] * gdp.per.mil.personnel.pwt.cow.ln,
#     mil.cap = mil.expenditure.per.capita.cow.un.ln + mil.expenditure.perc.gdp.cow.pwt.ln +
#       mil.expenditure.per.personnel.cow.cow.ln + mil.personnel.per.capita.cow.un.ln +
#       gdp.per.mil.personnel.pwt.cow.ln,
#     mil.cap.sq = mil.cap^2
#   )
# 
# 
# # index_ccpu <- index_ccpu_data %>%
# #   # combines principal component scores with PCA dataset
# #   cbind(ccpu_pca[["x"]]) %>%
# #   # drops 2nd-5th principal component scores
# #   dplyr::select(-c(PC2,PC3,PC4,PC5)) %>%
# #   # renames first principal component as mil.cap
# #   dplyr::rename(mil.cap = PC1) %>%
# #   # creates mil.cap.sq variable, the square of mil.cap
# #   # this tests for extremes of military capacity - extremely strong/weak vs. average capacity
# #   dplyr::mutate(mil.cap.sq = mil.cap^2)
# 
# # test directionality of metric
# # invert direction if KWT 1992 is negative - higher value is higher capacity.
# if(index_ccpu$mil.cap[index_ccpu$iso3c=="KWT"&index_ccpu$year==1992]<1){
#   index_ccpu <- index_ccpu %>%
#     dplyr::mutate(mil.cap = -1 * mil.cap)
# }
# 
# 
# #### write data ----------------------------------------------------------------------
# # writes formatted dataframe as csv files
# write.csv(index_ccpu,"Data files/Formatted data files/military_capacity_index_ccpu.csv",row.names = FALSE)
# 
# 
# ### Latent CCPU ----------------------------------------------------------------------
# 
# ccpu_vars_sem <- c(
#   "mil.expenditure.per.capita.cow.un.ln",
#   "mil.expenditure.per.capita.cow.un",
#   "mil.expenditure.perc.gdp.cow.pwt.ln",
#   "mil.expenditure.perc.gdp.cow.pwt",
#   "mil.expenditure.per.personnel.cow.cow.ln",
#   "mil.expenditure.per.personnel.cow.cow",
#   "mil.personnel.per.capita.cow.un.ln",
#   "mil.personnel.per.capita.cow.un",
#   "gdp.per.mil.personnel.pwt.cow.ln",
#   "gdp.per.mil.personnel.pwt.cow",
#   "mil.personnel.cow.per.sqkm.ln",
#   "mil.personnel.cow.per.sqkm",
#   "gdp.per.1000.mil.personnel.pwt.cow.ln",
#   "gdp.per.1000.mil.personnel.pwt.cow",
#   "exp.log.per.mil.personnel.ln",
#   "exp.log.per.mil.personnel",
#   "exp.log.per.1000.mil.personnel.ln",
#   "exp.log.per.1000.mil.personnel",
#   "gdp.log.per.mil.personnel.ln",
#   "gdp.log.per.mil.personnel"
# )
# 
# 
# 
# index_ccpu_sem_data <- mil.metrics %>%
#   dplyr::select(
#     iso3c, year, dplyr::all_of(ccpu_vars_sem)
#   ) %>%
#   dplyr::filter(
#     iso3c %!in% c(
#       "DMA","GRD","LCA","VCT","ATG","KNA","MCO","LIE","AND","SMR","ISL","MDV","VUT",
#       "SLB","KIR","TUV","TON","NRU","MHL","PLW","FSM","WSM","PSE",
#       "BHS","BRB","BRN","BTN","CPV","MUS","SYC","STP" ###
#     ),
#     # drop Inf/-Inf values
#     if_all(where(is.numeric), is.finite)
#   ) %>%
#   # rename variables for ease of use
#   dplyr::rename(
#     EXPPERCAPln = mil.expenditure.per.capita.cow.un.ln,
#     EXPPERCAP = mil.expenditure.per.capita.cow.un,
#     EXPPERGDPln = mil.expenditure.perc.gdp.cow.pwt.ln,
#     EXPPERGDP = mil.expenditure.perc.gdp.cow.pwt,
#     EXPPERPERln = mil.expenditure.per.personnel.cow.cow.ln,
#     EXPPERPER = mil.expenditure.per.personnel.cow.cow,
#     PERPERCAPln = mil.personnel.per.capita.cow.un.ln,
#     PERPERCAP = mil.personnel.per.capita.cow.un,
#     GDPPERPERln = gdp.per.mil.personnel.pwt.cow.ln,
#     GDPPERPER = gdp.per.mil.personnel.pwt.cow,
#     MILPERSKMln = mil.personnel.cow.per.sqkm.ln,
#     MILPERSKM = mil.personnel.cow.per.sqkm,
#     GDPPERPERKln = gdp.per.1000.mil.personnel.pwt.cow.ln,
#     GDPPERPERK = gdp.per.1000.mil.personnel.pwt.cow,
#     EXPLOGPERPERln = exp.log.per.mil.personnel.ln,
#     EXPLOGPERPER = exp.log.per.mil.personnel,
#     EXPLOGPERPERKln = exp.log.per.1000.mil.personnel.ln,
#     EXPLOGPERPERK = exp.log.per.1000.mil.personnel,
#     GDPLNPERPERln = gdp.log.per.mil.personnel.ln,
#     GDPLNPERPER = gdp.log.per.mil.personnel
#   ) %>%
#   dplyr::mutate(
#     # personnel per 1,000 people
#     PERPERCAPK = 1000 * PERPERCAP,
#     PERPERCAPKln = log(PERPERCAPK),
#     KMILPERSKM = 1000 * MILPERSKM,
#     KMILPERSKM = log(KMILPERSKM),
#     GDPLNPERPERK = 1000 * GDPLNPERPER,
#     GDPLNPERPERKln = log(GDPLNPERPERK)
#   )
# 
# cor(index_ccpu_sem_data[,c(3:ncol(index_ccpu_sem_data))])
# 
# # my_model <- '
# #   # Latent variables (if you had them)
# #   # MC =~ EXPPERGDP + PERPERCAP + GDPPERPER
# # 
# #   # Regressions (Direct Paths)
# #   M ~ a * X
# #   Y ~ b * M + c * X
# # 
# #   # Indirect effect (a * b)
# #   indirect := a * b
# #   # Total effect (a * b + c)
# #   total := (a * b) + c
# # '
# 
# 
# # 1
# sem_model <- '
#   MC =~ EXPPERGDP + PERPERCAPK + GDPPERPERln + MILPERSKM
# '
# # CFI = 0.967 | TLI = 0.902 | RMSEA = 0.101
# # Military Expenditure as a Percentage of GDP
# # Personnel per 1,000 Population
# # GDP per Personnel, log
# 
# 
# # 2
# sem_model <- '
#   MC =~ EXPPERCAPln + PERPERCAPK + GDPPERPERln + MILPERSKM
# '
# # CFI = 0.958 | TLI - 0.873 | RMSEA = 0.148
# 
# # 3
# sem_model <- '
#   MC =~ EXPPERGDP + EXPPERPERln + GDPPERPERln + MILPERSKM
# '
# # CFI = 0.994 | TLI = 0.981 | RMSEA = 0.064
# 
# # 3.1
# sem_model <- '
#   MC =~ EXPPERGDP + EXPPERPERln + GDPPERPERln + MILPERSKM
# 
#   EXPPERGDP ~~ v1*EXPPERGDP
#   v1 > 0.001
# '
# 
# # 4
# sem_model <- '
#   MC =~ EXPPERGDP + PERPERCAPK + EXPLOGPERPERKln + GDPPERPERln + MILPERSKM
# '
# # CFI = 0.971 | TLI - 0.942 | RMSEA = 0.069
# 
# # 5
# sem_model <- '
#   MC =~ EXPPERGDP + EXPPERPERln + GDPLNPERPERK + KMILPERSKM
# '
# # CFI = 0.738 | TLI = 0.213 | RMSEA = 0.156
# 
# # 6
# sem_model <- '
#   MC =~ EXPPERGDP + EXPPERPERln + GDPLNPERPERK + MILPERSKM
# '
# # CFI = 0.938 | TLI = 0.813 | RMSEA = 0.055
# 
# # 7
# sem_model <- '
#   MC =~ EXPPERGDP + EXPPERPERln + GDPPERPERln + KMILPERSKM
# '
# 
# # 8
# sem_model <- '
#   MC =~ EXPPERCAP + EXPPERPERln + GDPPERPERln + KMILPERSKM
# '
# 
# # 9
# sem_model <- '
#   MC =~ EXPPERGDPln + EXPPERPERln + GDPPERPERln + MILPERSKM
# '
# 
# sem_model <- '
#   MC =~ EXPPERGDPln + PERPERCAPKln + GDPPERPERln + MILPERSKMln
#   
#   # Force error variances to be positive (The "~~" is the error variance)
#   EXPPERGDPln ~~ a*EXPPERGDPln; a > 0.01
#   PERPERCAPKln ~~ b*PERPERCAPKln; b > 0.01
#   GDPPERPERln ~~ c*GDPPERPERln; c > 0.01
#   MILPERSKMln ~~ d*MILPERSKMln; d > 0.01
# '
# # CFI = 0.964 | TLI = 0.891 | RMSEA = 0.146
# 
# 
# sem_model <- '
#   MC =~ EXPPERGDP + PERPERCAPK + GDPPERPER + MILPERSKM
#   
#   # Force error variances to be positive (The "~~" is the error variance)
#   EXPPERGDP ~~ a*EXPPERGDP; a > 0.01
#   PERPERCAPK ~~ b*PERPERCAPK; b > 0.01
#   GDPPERPER ~~ c*GDPPERPER; c > 0.01
#   MILPERSKM ~~ d*MILPERSKM; d > 0.01
# '
# # CFI = 0.992 | TLI = 0.977 | RMSEA = 0.037
# # Military Expenditure as % of GDP
# # Military Personnel per 1,000 Population
# # GDP per Military Personnel
# # Military Personnel per Sq. KM
# 
# sem_model <- '
#   MC =~ EXPPERGDP + PERPERCAPK + GDPPERPER + MILPERSKMln
#   
#   # Force error variances to be positive (The "~~" is the error variance)
#   EXPPERGDP ~~ a*EXPPERGDP; a > 0.01
#   PERPERCAPK ~~ b*PERPERCAPK; b > 0.01
#   GDPPERPER ~~ c*GDPPERPER; c > 0.01
#   MILPERSKMln ~~ d*MILPERSKMln; d > 0.01
# '
# # CFI = 0.984 | TLI = 0.953 | RMSEA = 0.071
# # Military Expenditure as % of GDP
# # Military Personnel per 1,000 Population
# # GDP per Military Personnel
# # Military Personnel per Sq. KM, log
# 
# sem_model <- '
#   MC =~ EXPPERCAP + EXPPERGDP + PERPERCAPK + GDPPERPER
#   
#   # Force error variances to be positive (The "~~" is the error variance)
#   EXPPERCAP ~~ d*EXPPERCAP; d > 0.01
#   EXPPERGDP ~~ a*EXPPERGDP; a > 0.01
#   PERPERCAPK ~~ b*PERPERCAPK; b > 0.01
#   GDPPERPER ~~ c*GDPPERPER; c > 0.01
# '
# # CFI = 0.812 | TLI = 0.435 | RMSEA = 0.233
# 
# ### **
# sem_model <- '
#   MC =~ EXPPERGDP + PERPERCAPK + GDPPERPERK + MILPERSKM
#   
#   # Force error variances to be positive (The "~~" is the error variance)
#   EXPPERGDP ~~ a*EXPPERGDP; a > 0.01
#   PERPERCAPK ~~ b*PERPERCAPK; b > 0.01
#   GDPPERPERK ~~ c*GDPPERPERK; c > 0.01
#   MILPERSKM ~~ d*MILPERSKM; d > 0.01
# '
# # CFI = 0.994 | TLI = 0.983 | RMSEA = 0.032
# 
# # scale data
# x <- index_ccpu_sem_data[,c("EXPPERGDP", "PERPERCAPK", "GDPPERPERK", "MILPERSKM")] %>%
#   mutate(across(where(is.numeric), ~ as.numeric(scale(.))))
# x.cor <- cor(x)
# 
# fit <- sem(sem_model, data = x)
# varTable(fit)
# 
# summary(fit, standardized = TRUE, fit.measures = TRUE)
# 
# 
# latent_scores <- lavPredict(fit)
# latent_df <- as.data.frame(latent_scores)
# index_ccpu_sem_data_appended <- index_ccpu_sem_data %>%
#   bind_cols(latent_df) %>%
#   dplyr::select(iso3c, year, MC, EXPPERCAP, EXPPERGDP, PERPERCAPK, GDPPERPER)
# 
# semPaths(fit, 
#          whatLabels = "std",        # Show standardized coefficients on the arrows
#          layout = "tree2",          # Tree layout (predictors left, latent center, indicators right)
#          edge.label.cex = 1,        # Font size for labels
#          curvePivot = TRUE,         # Makes the covariance curves look cleaner
#          sizeMan = 10,              # Size of the manifest (square) boxes
#          sizeLat = 10,              # Size of the latent (circle) boxes
#          edge.color = "black")      # Color of the paths
# 
# 
# ### V2
# 
# vars_sem2 <- c("mil.expenditure.cow", "mil.personnel.cow", "mil.expenditure.per.personnel.cow.cow", "un.pop", "gdp.pwt.est", "irst", "pec", "land_area")
# 
# index_ccpu_sem_data2 <- mil.metrics %>%
#   dplyr::select(iso3c, year, dplyr::all_of(vars_sem2)) %>%
#   dplyr::mutate(
#     mil.expenditure.cow.ln = log(mil.expenditure.cow),
#     un.pop.ln = log(un.pop),
#     gdp.pwt.est.ln = log(gdp.pwt.est),
#     land_area.ln = log(land_area),
#     mil.personnel.cow.per100000 = mil.personnel.cow / 100000,
#     mil.expenditure.per.personnel.cow.cow.ln = log(mil.expenditure.per.personnel.cow.cow),
#     mil.personnel.per.sqkm = mil.personnel.cow / land_area
#   ) %>%
#   dplyr::filter(
#     iso3c %!in% c(
#       "DMA","GRD","LCA","VCT","ATG","KNA","MCO","LIE","AND","SMR","ISL","MDV","VUT",
#       "SLB","KIR","TUV","TON","NRU","MHL","PLW","FSM","WSM","PSE",
#       "BHS","BRB","BRN","BTN","CPV","MUS","SYC","STP" ###
#     ),
#     # drop Inf/-Inf values
#     if_all(where(is.numeric), is.finite),
#     # drop missing values
#     !is.na(mil.expenditure.cow),
#     !is.na(mil.personnel.cow),
#     !is.na(mil.expenditure.per.personnel.cow.cow)
#   )
# 
# sem_model2 <- '
#   MC =~ mil.expenditure.cow.ln + mil.personnel.cow.per100000
#   MC ~ un.pop.ln + gdp.pwt.est.ln + land_area.ln
#   un.pop.ln ~~ gdp.pwt.est.ln
#   un.pop.ln ~~ land_area.ln
#   gdp.pwt.est.ln ~~ land_area.ln
# 
#   # Force error variances to be positive (The "~~" is the error variance)
#   mil.expenditure.cow.ln ~~ a*mil.expenditure.cow.ln; a > 0.01
#   mil.personnel.cow.per100000 ~~ b*mil.personnel.cow.per100000; b > 0.01
# '
# 
# fit2 <- sem(sem_model2, data = index_ccpu_sem_data2, std.lv = TRUE)
# varTable(fit2)
# 
# summary(fit2, standardized = TRUE, fit.measures = TRUE)
# 
# latent_scores <- lavPredict(fit2)
# latent_df <- as.data.frame(latent_scores)
# index_ccpu_sem_data2_appended <- index_ccpu_sem_data2 %>%
#   bind_cols(latent_df)
# 
# semPaths(fit2, 
#          whatLabels = "std",        # Show standardized coefficients on the arrows
#          layout = "tree2",          # Tree layout (predictors left, latent center, indicators right)
#          edge.label.cex = 1,        # Font size for labels
#          curvePivot = TRUE,         # Makes the covariance curves look cleaner
#          sizeMan = 10,              # Size of the manifest (square) boxes
#          sizeLat = 10,              # Size of the latent (circle) boxes
#          edge.color = "black")      # Color of the paths
# 
# #### OLD
# 
# 
# # create covariance matrix
# regression.cor <- cor(index_ccpu_sem_data %>%
#                         dplyr::select(-c(iso3c,year)))
# 
# regression.sd <- list()
# for(c in 1:ncol(index_ccpu_cor)){
#   regression.sd[c] <- sd(index_ccpu_cor[,c])
# }
# regression.sd <- unlist(regression.sd)
# 
# # name list
# milindex_names <- c("ExpPerCap", "ExpPercGDP", "ExpPerPers", "PersPerCap", "GDPPerPers")
# 
# # name the variables
# colnames(regression.cor) <- rownames(regression.cor) <- milindex_names
# names(regression.sd) <- milindex_names
# names(index_ccpu_data) <- c("iso3c", "year", milindex_names)
# 
# # convert correlations and SDs to covarainces
# regression.cov <- cor2cov(regression.cor, regression.sd)
# 
# # specify single factor model
# # regression.model <- 'MilCap =~ a*ExpPerCap + b*ExpPercGDP + c*ExpPerPers + d*PersPerCap + e*GDPPerPers'
# # 
# # regression.model <- 'MilCap =~ a*ExpPercGDP + d*PersPerCap + e*GDPPerPers'
# # 
# # model.expenditure <- 'MilCap.Exp =~ ExpPerCap + ExpPercGDP + ExpPerPers'
# 
# regression.model <- 'MilCap =~ a*ExpPerCap + b*ExpPercGDP + d*PersPerCap + e*GDPPerPers'
# 
# 
# # fit model
# regression.fit <- lavaan::cfa(model = regression.model,
#                               data = index_ccpu_data)
# summary(regression.fit,
#         standardized = TRUE)
# parameterEstimates(regression.fit,
#                    standardized=TRUE)
# 
# # residual correlations
# residuals(regression.fit, type="cor")
# # measures of model fit 
# fitMeasures(regression.fit)
# # modification indices
# modificationIndices(regression.fit)
# 
# # regression.fit.exp <- lavaan::cfa(model = model.expenditure,
# #                                   data = index_ccpu_data)
# # summary(regression.fit.exp,
# #         standardized = TRUE)
# # parameterEstimates(regression.fit.exp,
# #                    standardized=TRUE)
# 
# 
# 
# 
# 
# regression.fit <- lavaan::cfa(model = regression.model, sample.cov=regression.cov, sample.nobs=550,  std.lv=FALSE)
# 
# # examine parameter estimates
# summary(wisc4.fit,standardized=TRUE)
# parameterEstimates(wisc4.fit,standardized=TRUE)
# 
# # check model
# # model-implied covariances
# fitted(wisc4.fit)
# # transform model-implied covariances to correlations
# wisc4Fit.cov <- fitted(wisc4.fit)$cov
# wisc4Fit.cor <- cov2cor(wisc4Fit.cov)
# # residual correlations
# residuals(wisc4.fit,type="cor")
# # measures of model fit 
# fitMeasures(wisc4.fit)
# # modification indices
# modificationIndices(wisc4.fit)
# 
# # 
# # ### load libraries ----------------------------------------------------------------------
# # library(readxl)
# # library(countrycode)
# # library(imputeTS)
# # library(mice)
# # library(mtsdi)
# # library(EFAtools)
# # library(corrplot)
# # library(dplyr)
# # library(tidyr)
# # 
# # ### load data ----------------------------------------------------------------------
# # mil.metrics <- read.csv("Data files/Formatted data files/Military_data.csv")
# # 
# # ### standardize data ----------------------------------------------------------------------
# # mil.metrics <- mil.metrics %>%
# #   dplyr::mutate(
# #     mil.expenditure.cow.ln = log(mil.expenditure.cow),
# #     mil.expenditure.cow.alt.ln = log(mil.expenditure.cow.alt),
# #     mil.personnel.cow.ln = log(mil.personnel.cow),
# #     mil.expenditure.wmeat.ln = log(mil.expenditure.wmeat),
# #     mil.personnel.wmeat.ln = log(mil.personnel.wmeat),
# #     mil.expenditure.sipri.ln = log(mil.expenditure.sipri),
# #     mil.expenditure.per.capita.cow.un.ln = log(mil.expenditure.per.capita.cow.un),
# #     mil.expenditure.per.capita.cow.alt.un.ln = log(mil.expenditure.per.capita.cow.alt.un),
# #     mil.expenditure.per.capita.cow.cow.ln = log(mil.expenditure.per.capita.cow.cow),
# #     mil.expenditure.per.capita.cow.alt.cow.ln = log(mil.expenditure.per.capita.cow.alt.cow),
# #     mil.expenditure.per.capita.wmeat.un.ln = log(mil.expenditure.per.capita.wmeat.un),
# #     mil.expenditure.per.capita.wmeat.cow.ln = log(mil.expenditure.per.capita.wmeat.cow),
# #     mil.expenditure.per.capita.sipri.un.ln = log(mil.expenditure.per.capita.sipri.un),
# #     mil.expenditure.per.capita.sipri.cow.ln = log(mil.expenditure.per.capita.sipri.cow),
# #     mil.expenditure.perc.gdp.cow.pwt.ln = log(mil.expenditure.perc.gdp.cow.pwt),
# #     mil.expenditure.perc.gdp.cow.alt.pwt.ln = log(mil.expenditure.perc.gdp.cow.alt.pwt),
# #     mil.expenditure.perc.gdp.cow.gl.ln = log(mil.expenditure.perc.gdp.cow.gl),
# #     mil.expenditure.perc.gdp.cow.alt.gl.ln = log(mil.expenditure.perc.gdp.cow.alt.gl),
# #     mil.expenditure.perc.gdp.wmeat.pwt.ln = log(mil.expenditure.perc.gdp.wmeat.pwt),
# #     mil.expenditure.perc.gdp.wmeat.gl.ln = log(mil.expenditure.perc.gdp.wmeat.gl),
# #     mil.expenditure.perc.gdp.sipri.pwt.ln = log(mil.expenditure.perc.gdp.sipri.pwt),
# #     mil.expenditure.perc.gdp.sipri.gl.ln = log(mil.expenditure.perc.gdp.sipri.gl),
# #     mil.expenditure.per.personnel.cow.cow.ln = log(mil.expenditure.per.personnel.cow.cow),
# #     mil.expenditure.per.personnel.cow.alt.cow.ln = log(mil.expenditure.per.personnel.cow.alt.cow),
# #     mil.expenditure.per.personnel.cow.wmeat.ln = log(mil.expenditure.per.personnel.cow.wmeat),
# #     mil.expenditure.per.personnel.cow.alt.wmeat.ln = log(mil.expenditure.per.personnel.cow.alt.wmeat),
# #     mil.expenditure.per.personnel.wmeat.cow.ln = log(mil.expenditure.per.personnel.wmeat.cow),
# #     mil.expenditure.per.personnel.wmeat.wmeat.ln = log(mil.expenditure.per.personnel.wmeat.wmeat),
# #     mil.expenditure.per.personnel.sipri.cow.ln = log(mil.expenditure.per.personnel.sipri.cow),
# #     mil.expenditure.per.personnel.sipri.wmeat.ln = log(mil.expenditure.per.personnel.sipri.wmeat),
# #     mil.personnel.per.capita.cow.un.ln = log(mil.personnel.per.capita.cow.un),
# #     mil.personnel.per.capita.cow.cow.ln = log(mil.personnel.per.capita.cow.cow),
# #     mil.personnel.per.capita.wmeat.un.ln = log(mil.personnel.per.capita.wmeat.un),
# #     mil.personnel.per.capita.wmeat.cow.ln = log(mil.personnel.per.capita.wmeat.cow),
# #     gdp.per.mil.personnel.pwt.cow.ln = log(gdp.per.mil.personnel.pwt.cow),
# #     gdp.per.mil.personnel.pwt.wmeat.ln = log(gdp.per.mil.personnel.pwt.wmeat),
# #     gdp.per.mil.personnel.gl.cow.ln = log(gdp.per.mil.personnel.gl.cow),
# #     gdp.per.mil.personnel.gl.wmeat.ln = log(gdp.per.mil.personnel.gl.wmeat)
# #     )
# # 
# # ### define version variables ----------------------------------------------------------------------
# # # expenditure: cow (c), cow alt (a), wmeat (w), sipri (s)
# # # personnel: cow (c), wmeat (w)
# # # gdp: pwt (p), gl (g)
# # # population: un (u), wmeat (w)
# # 
# # # mil.expenditure
# # # mil.personnel
# # # mil.expenditure.per.capita
# # # mil.expenditure.perc.gdp
# # # mil.expenditure.per.personnel
# # # mil.personnel.per.capita
# # # gdp.per.mil.personnel
# # # mil.expenditure.growth.rate
# # # mil.personnel.growth.rate
# # # mil.expenditure.per.capita.growth.rate
# # # mil.expenditure.perc.gdp.growth.rate
# # # mil.expenditure.per.personnel.growth.rate
# # # mil.personnel.per.capita.growth.rate
# # # gdp.per.mil.personnel.growth.rate
# # 
# # 
# # # cow.cow.pwt.un - ccpu
# # ccpu <- c(
# #   "mil.expenditure.cow","mil.personnel.cow","mil.expenditure.per.capita.cow.un",
# #           "mil.expenditure.perc.gdp.cow.pwt","mil.expenditure.per.personnel.cow.cow",
# #           "mil.personnel.per.capita.cow.un","gdp.per.mil.personnel.pwt.cow"#,
# #           #"mil.expenditure.growth.rate.cow","mil.personnel.growth.rate.cow",
# #           #"mil.expenditure.per.capita.growth.rate.cow.un",
# #           #"mil.expenditure.per.personnel.growth.rate.cow.cow",
# #           #"mil.personnel.per.capita.growth.rate.cow.un",
# #           #"gdp.per.mil.personnel.growth.rate.pwt.cow"
# #   )
# # ccpu <- c(ccpu,paste0(ccpu,".ln"))
# # 
# # # cow.cow.pwt.wmeat -ccpw
# # # cow.cow.gl.un - ccgu
# # # cow.cow.gl.wmeat - ccgw
# # # cow.wmeat.pwt.un - cwpu
# # # cow.wmeat.pwt.wmeat - cwpw
# # # cow.wmeat.gl.un - cwgu
# # # cow.wmeat.gl.wmeat - cwgw
# # 
# # # cow.alt.cow.pwt.un - acpu
# # # cow.alt.cow.pwt.wmeat - acpw
# # # cow.alt.cow.gl.un - acgu
# # # cow.alt.cow.gl.wmeat - acgw
# # # cow.alt.wmeat.pwt.un - awpu
# # # cow.alt.wmeat.pwt.wmeat - awpw
# # # cow.alt.wmeat.gl.un - awgu
# # # cow.alt.wmeat.gl.wmeat - awgw
# # 
# # # wmeat.cow.pwt.un - wcpu
# # # wmeat.cow.pwt.wmeat - wcpw
# # # wmeat.cow.gl.un - wcgu
# # # wmeat.cow.gl.wmeat - wcgw
# # # wmeat.wmeat.pwt.un - wwpu
# # # wmeat.wmeat.pwt.wmeat - wwpw
# # # wmeat.wmeat.gl.un - wwgu
# # # wmeat.wmeat.gl.wmeat -wwgw
# # 
# # # sipri.cow.pwt.un - scpu
# # # sipri.cow.pwt.wmeat - scpw
# # # sipri.cow.gl.un - scgu
# # # sipri.cow.gl.wmeat - scgw
# # # sipri.wmeat.pwt.un - swpu
# # # sipri.wmeat.pwt.wmeat - swpw
# # # sipri.wmeat.gl.un - swgu
# # # sipri.wmeat.gl.wmeat - swgw
# # 
# # ##### test round ccpu #####
# # mil.ccpu <- mil.metrics %>%
# #   dplyr::select(iso3c,year,all_of(ccpu))%>%
# #   dplyr::filter(
# #     mil.expenditure.cow != 0,
# #     mil.personnel.cow != 0,
# #     iso3c %!in% c(
# #       "DMA","GRD","LCA","VCT","ATG","KNA","MCO","LIE","AND","SMR","ISL","COM","MDV","VUT",
# #       "SLB","KIR","TUV","TON","NRU","MHL","PLW","FSM","WSM","PSE"
# #     )
# #   )
# # 
# # mil.ccpu.noest <- mil.metrics %>%
# #   dplyr::select(iso3c,year,all_of(ccpu),
# #                 mil.expenditure.cow.est.flag,
# #                 mil.personnel.cow.est.flag) %>%
# #   dplyr::filter(
# #     mil.expenditure.cow != 0,
# #     mil.personnel.cow != 0,
# #     mil.expenditure.cow.est.flag == 0,
# #     mil.personnel.cow.est.flag == 0,
# #     iso3c %!in% c(
# #       "DMA","GRD","LCA","VCT","ATG","KNA","MCO","LIE","AND","SMR","ISL","COM","MDV","VUT",
# #       "SLB","KIR","TUV","TON","NRU","MHL","PLW","FSM","WSM","PSE"
# #     )
# #   )
# #   
# # 
# # # original grad school pca: lnmilexpgdp + lntroops + lnmilexpc + lnmilexpt
# # 
# # # non-growth
# # ccpu.ng <- mil.ccpu %>%
# #   dplyr::select(
# #     mil.expenditure.cow,mil.personnel.cow,mil.expenditure.per.capita.cow.un,mil.expenditure.perc.gdp.cow.pwt,
# #     mil.expenditure.per.personnel.cow.cow,mil.personnel.per.capita.cow.un,gdp.per.mil.personnel.pwt.cow,
# #     mil.expenditure.cow.ln,mil.personnel.cow.ln,mil.expenditure.per.capita.cow.un.ln,mil.expenditure.perc.gdp.cow.pwt.ln,
# #     mil.expenditure.per.personnel.cow.cow.ln,mil.personnel.per.capita.cow.un.ln,gdp.per.mil.personnel.pwt.cow.ln
# #   ) %>%
# #   na.omit()
# # 
# # ccpu.ng.cor <- cor(ccpu.ng)
# # 
# # corrplot(ccpu.ng.cor)
# #                 
# # 
# # mil.all <- mil.metrics %>%
# #   dplyr::select(
# #     iso3c,
# #     year,
# #     # mil.expenditure.per.capita.cow.un.ln,
# #     mil.expenditure.per.capita.cow.alt.un.ln,
# #     # mil.expenditure.per.capita.cow.cow.ln,
# #     # mil.expenditure.per.capita.cow.alt.cow.ln,
# #     mil.expenditure.per.capita.wmeat.un.ln,
# #     # mil.expenditure.per.capita.wmeat.cow.ln,
# #     # mil.expenditure.per.capita.sipri.un.ln,
# #     # mil.expenditure.per.capita.sipri.cow.ln,
# #     # mil.expenditure.perc.gdp.cow.pwt.ln,
# #     mil.expenditure.perc.gdp.cow.alt.pwt.ln,
# #     # mil.expenditure.perc.gdp.cow.gl.ln,
# #     # mil.expenditure.perc.gdp.cow.alt.gl.ln,
# #     mil.expenditure.perc.gdp.wmeat.pwt.ln,
# #     # mil.expenditure.perc.gdp.wmeat.gl.ln,
# #     # mil.expenditure.perc.gdp.sipri.pwt.ln,
# #     # mil.expenditure.perc.gdp.sipri.gl.ln,
# #     # mil.expenditure.per.personnel.cow.cow.ln,
# #     mil.expenditure.per.personnel.cow.alt.cow.ln,
# #     # mil.expenditure.per.personnel.cow.wmeat.ln,
# #     # mil.expenditure.per.personnel.cow.alt.wmeat.ln,
# #     mil.expenditure.per.personnel.wmeat.cow.ln,
# #     # mil.expenditure.per.personnel.wmeat.wmeat.ln,
# #     # mil.expenditure.per.personnel.sipri.cow.ln,
# #     # mil.expenditure.per.personnel.sipri.wmeat.ln,
# #     mil.personnel.per.capita.cow.un.ln,
# #     # mil.personnel.per.capita.cow.cow.ln,
# #     mil.personnel.per.capita.wmeat.un.ln,
# #     # mil.personnel.per.capita.wmeat.cow.ln,
# #     gdp.per.mil.personnel.pwt.cow.ln,
# #     gdp.per.mil.personnel.pwt.wmeat.ln,
# #     # gdp.per.mil.personnel.gl.cow.ln,
# #     # gdp.per.mil.personnel.gl.wmeat.ln
# #   ) %>%
# #   tidyr::pivot_longer(3:12, names_to = "variable", values_to = "value") %>%
# #   dplyr::filter(value != Inf,
# #                 value != -Inf) %>%
# #   tidyr::pivot_wider(names_from = "variable", values_from = "value") %>%
# #   dplyr::select(-c(iso3c,year)) %>%
# #   drop_na()
# # 
# # mil.ccpu.complete <- mil.ccpu %>%
# #   dplyr::select(mil.expenditure.perc.gdp.cow.pwt.ln,mil.personnel.cow.ln,mil.expenditure.per.capita.cow.un.ln,mil.expenditure.per.personnel.cow.cow.ln) %>%
# #   na.omit() %>%
# #   as.matrix()
# # 
# # mil.ccpu.complete2 <- mil.ccpu %>%
# #   dplyr::select(mil.expenditure.perc.gdp.cow.pwt,mil.personnel.cow,mil.expenditure.per.capita.cow.un,mil.expenditure.per.personnel.cow.cow) %>%
# #   na.omit()
# # 
# # ccpu3 <- mil.ccpu %>%
# #   dplyr::select(mil.expenditure.per.capita.cow.un.ln,mil.expenditure.perc.gdp.cow.pwt,mil.expenditure.per.personnel.cow.cow.ln,
# #                 mil.personnel.per.capita.cow.un,gdp.per.mil.personnel.pwt.cow.ln) %>%
# #   na.omit()
# # 
# # ccpu4 <- mil.ccpu %>%
# #   dplyr::select(mil.expenditure.per.capita.cow.un,mil.expenditure.perc.gdp.cow.pwt,mil.expenditure.per.personnel.cow.cow,
# #                 mil.personnel.per.capita.cow.un,gdp.per.mil.personnel.pwt.cow) %>%
# #   na.omit()
# # 
# # ccpu5 <- mil.ccpu %>%
# #   dplyr::select(mil.expenditure.per.capita.cow.un.ln,mil.expenditure.perc.gdp.cow.pwt.ln,mil.expenditure.per.personnel.cow.cow.ln,
# #                 mil.personnel.per.capita.cow.un.ln,gdp.per.mil.personnel.pwt.cow.ln) %>%
# #   na.omit()
# # 
# # ccpu6 <- mil.ccpu %>%
# #   dplyr::select(mil.expenditure.per.capita.cow.un.ln,mil.expenditure.perc.gdp.cow.pwt.ln,mil.expenditure.per.personnel.cow.cow.ln,
# #                 mil.personnel.per.capita.cow.un,gdp.per.mil.personnel.pwt.cow.ln) %>%
# #   na.omit()
# # 
# # ccpu7 <- mil.ccpu %>%
# #   dplyr::select(mil.expenditure.per.capita.cow.un.ln,mil.expenditure.perc.gdp.cow.pwt.ln,mil.expenditure.per.personnel.cow.cow.ln,
# #                 mil.personnel.per.capita.cow.un,gdp.per.mil.personnel.pwt.cow) %>%
# #   na.omit()
# # 
# # ccpu8 <- mil.ccpu %>%
# #   dplyr::select(mil.personnel.cow.ln,mil.expenditure.per.capita.cow.un.ln,mil.expenditure.perc.gdp.cow.pwt.ln,mil.expenditure.per.personnel.cow.cow.ln,
# #                 mil.personnel.per.capita.cow.un,gdp.per.mil.personnel.pwt.cow) %>%
# #   na.omit()
# # 
# # ccpu9 <- mil.ccpu %>%
# #   dplyr::select(mil.personnel.cow.ln,mil.expenditure.per.capita.cow.un.ln,mil.expenditure.perc.gdp.cow.pwt.ln,mil.expenditure.per.personnel.cow.cow.ln,
# #                 mil.personnel.per.capita.cow.un.ln,gdp.per.mil.personnel.pwt.cow) %>%
# #   na.omit()
# # 
# # ccpu10 <- mil.ccpu %>%
# #   dplyr::select(mil.expenditure.perc.gdp.cow.pwt,mil.personnel.cow.ln,mil.expenditure.per.capita.cow.un,mil.expenditure.per.personnel.cow.cow,
# #                 mil.personnel.per.capita.cow.un.ln,gdp.per.mil.personnel.pwt.cow) %>%
# #   na.omit()
# # 
# # ccpu11 <- mil.ccpu %>%
# #   dplyr::select(mil.expenditure.perc.gdp.cow.pwt,mil.expenditure.per.capita.cow.un,mil.expenditure.per.personnel.cow.cow,
# #                 mil.personnel.per.capita.cow.un.ln,gdp.per.mil.personnel.pwt.cow) %>%
# #   na.omit()
# # 
# # ccpu11 <- mil.ccpu %>%
# #   dplyr::select(mil.expenditure.perc.gdp.cow.pwt,mil.expenditure.per.capita.cow.un,mil.expenditure.per.personnel.cow.cow,
# #                 mil.personnel.per.capita.cow.un,gdp.per.mil.personnel.pwt.cow) %>%
# #   na.omit()
# # 
# # ccpu12 <- mil.ccpu %>%
# #   dplyr::select(mil.expenditure.per.capita.cow.un.ln,mil.expenditure.perc.gdp.cow.pwt.ln,mil.expenditure.per.personnel.cow.cow.ln,
# #                 mil.personnel.per.capita.cow.un,gdp.per.mil.personnel.pwt.cow.ln) %>%
# #   na.omit()
# # 
# # ccpu13 <- mil.ccpu %>%
# #   dplyr::select(mil.expenditure.per.capita.cow.un.ln,mil.expenditure.perc.gdp.cow.pwt.ln,mil.expenditure.per.personnel.cow.cow.ln,
# #                 mil.personnel.per.capita.cow.un,gdp.per.mil.personnel.pwt.cow) %>%
# #   na.omit()
# # 
# # ccpu11.noest <- mil.ccpu.noest %>%
# #   dplyr::select(mil.expenditure.perc.gdp.cow.pwt,mil.expenditure.per.capita.cow.un,mil.expenditure.per.personnel.cow.cow,
# #                 mil.personnel.per.capita.cow.un,gdp.per.mil.personnel.pwt.cow) %>%
# #   na.omit()
# # 
# # ccpu11.names <- mil.ccpu %>%
# #   dplyr::select(iso3c,year,mil.expenditure.perc.gdp.cow.pwt,mil.expenditure.per.capita.cow.un,mil.expenditure.per.personnel.cow.cow,
# #                 mil.personnel.per.capita.cow.un.ln,gdp.per.mil.personnel.pwt.cow) %>%
# #   na.omit()
# # 
# # mil.ccpu.complete.cor <- cor(mil.ccpu.complete)
# # mil.ccpu.complete2.cor <- cor(mil.ccpu.complete2)
# # ccpu3.cor <- cor(ccpu3)
# # ccpu4.cor <- cor(ccpu4)
# # ccpu5.cor <- cor(ccpu5)
# # ccpu6.cor <- cor(ccpu6)
# # ccpu7.cor <- cor(ccpu7)
# # ccpu8.cor <- cor(ccpu8)
# # ccpu9.cor <- cor(ccpu9)
# # ccpu10.cor <- cor(ccpu10)
# # ccpu11.cor <- cor(ccpu11)
# # ccpu12.cor <- cor(ccpu12)
# # ccpu13.cor <- cor(ccpu13)
# # ccpu11.noest.cor <- cor(ccpu11.noest)
# # mil.all.cor <- cor(mil.all)
# # 
# # corrplot(mil.ccpu.complete2.cor)
# # corrplot(ccpu4.cor)
# # corrplot(ccpu6.cor)
# # corrplot(ccpu7.cor)
# # 
# # unlist(determinant(mil.ccpu.complete.cor))
# # unlist(determinant(mil.ccpu.complete2.cor))
# # unlist(determinant(ccpu3.cor))
# # unlist(determinant(ccpu4.cor))
# # unlist(determinant(ccpu5.cor))
# # unlist(determinant(ccpu10.cor))
# # unlist(determinant(ccpu11.cor))
# # unlist(determinant(mil.all.cor))
# # 
# # KMO(mil.ccpu.complete.cor)
# # KMO(mil.ccpu.complete2.cor) # mediocre
# # KMO(ccpu3.cor)
# # KMO(ccpu4.cor) # mediocre
# # KMO(ccpu5.cor)
# # KMO(ccpu6.cor)
# # KMO(ccpu7.cor)
# # KMO(ccpu8.cor)
# # KMO(ccpu9.cor)
# # KMO(ccpu10.cor) # mediocre
# # KMO(ccpu11.cor) # mediocre *
# # KMO(ccpu12.cor)
# # KMO(ccpu13.cor)
# # KMO(ccpu11.noest.cor)
# # KMO(mil.all.cor)
# # 
# # BARTLETT(mil.ccpu.complete2.cor, N = nrow(mil.ccpu.complete2))
# # BARTLETT(ccpu4.cor, N = nrow(ccpu4))
# # BARTLETT(ccpu10.cor, N = nrow(ccpu10))
# # BARTLETT(ccpu11.cor, N = nrow(ccpu11))
# # 
# # ccpu_pca <- stats::prcomp(
# #   ~  mil.expenditure.perc.gdp.cow.pwt.ln + mil.personnel.cow.ln + mil.expenditure.per.capita.cow.un.ln + mil.expenditure.per.personnel.cow.cow.ln,
# #                          data = mil.ccpu, retx = T, center = T, scale. = T)
# # 
# # ### tests
# # # determinant of the correlation matrix
# # # bartlett test of sphericity
# # # kaiser-meyer-olkin measure of sampling adequacy
# # 
# # ### construct metrics ----------------------------------------------------------------------
# # ccpu_pca <- stats::prcomp(
# #   ~  mil.expenditure.per.capita.cow.un.ln + mil.expenditure.perc.gdp.cow.pwt + mil.expenditure.per.personnel.cow.cow.ln +
# #     mil.personnel.per.capita.cow.un.ln + gdp.per.mil.personnel.pwt.cow.ln,
# #   data = mil.ccpu, retx = T, center = T, scale. = T)
# # 
# # # "mil.expenditure.per.capita.cow.un"   /   "mil.expenditure.per.capita.cow.un.ln"
# # # "mil.expenditure.perc.gdp.cow.pwt"  /   "mil.expenditure.perc.gdp.cow.pwt.ln"
# # # "mil.expenditure.per.personnel.cow.cow"   /   "mil.expenditure.per.personnel.cow.cow.ln"
# # # "mil.personnel.per.capita.cow.un"   /   "mil.personnel.per.capita.cow.un.ln"
# # # "gdp.per.mil.personnel.pwt.cow"   /   "gdp.per.mil.personnel.pwt.cow.ln"
# # 
# # ### principal component analysis ----------------------------------------------------------------------
# # # PCA on lnmilexpgdp, lntroops, lnmilexpc, and lnmilexpt
# # cow_pca <- stats::prcomp(~  lnmilexpgdp + lntroops + lnmilexpc + lnmilexpt,
# #                          data = cow, retx = T, center = T, scale. = T)
# # 
# # cow <- cow %>%
# #   # combines principal component scores with PCA dataset
# #   cbind(cow_pca[["x"]]) %>%
# #   # drops second, third, and fourth principal component scores
# #   dplyr::select(-c(PC2,PC3,PC4)) %>%
# #   # renames first principal component as mil.cap
# #   dplyr::rename(mil.cap = PC1) %>%
# #   # creates mil.cap.sq variable, the square of mil.cap
# #   # this tests for extremes of military capacity - extremely strong/weak vs. average capacity
# #   dplyr::mutate(mil.cap.sq = mil.cap^2)
# # 
# # # average unweighted military capacity by year
# # mil_cap_yearly_avg <- cow %>%
# #   dplyr::group_by(year) %>%
# #   dplyr::summarise(avg = mean(mil.cap)) %>%
# #   dplyr::ungroup()
# # 
# # plot(mil_cap_yearly_avg$year,mil_cap_yearly_avg$avg,type='l')
# # 
# # ### format data for countries that unified/dissolved ----------------------------------------------------------------------
# # #### Soviet successor states ----------------------------------------------------------------------
# # arm.ts <- cow %>%
# #   dplyr::filter(iso3c %in% c("ARM","SOV")) %>%
# #   dplyr::mutate(iso3c = "ARM")
# # 
# # aze.ts <- cow %>%
# #   dplyr::filter(iso3c %in% c("AZE","SOV")) %>%
# #   dplyr::mutate(iso3c = "AZE")
# # 
# # blr.ts <- cow %>%
# #   dplyr::filter(iso3c %in% c("BLR","SOV")) %>%
# #   dplyr::mutate(iso3c = "BLR")
# # 
# # est.ts <- cow %>%
# #   dplyr::filter(iso3c %in% c("EST","SOV")) %>%
# #   dplyr::mutate(iso3c = "EST")
# # 
# # geo.ts <- cow %>%
# #   dplyr::filter(iso3c %in% c("GEO","SOV")) %>%
# #   dplyr::mutate(iso3c = "GEO")
# # 
# # kaz.ts <- cow %>%
# #   dplyr::filter(iso3c %in% c("KAZ","SOV")) %>%
# #   dplyr::mutate(iso3c = "KAZ")
# # 
# # kgz.ts <- cow %>%
# #   dplyr::filter(iso3c %in% c("KGZ","SOV")) %>%
# #   dplyr::mutate(iso3c = "KGZ")
# # 
# # ltu.ts <- cow %>%
# #   dplyr::filter(iso3c %in% c("LTU","SOV")) %>%
# #   dplyr::mutate(iso3c = "LTU")
# # 
# # lva.ts <- cow %>%
# #   dplyr::filter(iso3c %in% c("LVA","SOV")) %>%
# #   dplyr::mutate(iso3c = "LVA")
# # 
# # mda.ts <- cow %>%
# #   dplyr::filter(iso3c %in% c("MDA","SOV")) %>%
# #   dplyr::mutate(iso3c = "MDA")
# # 
# # rus.ts <- cow %>%
# #   dplyr::filter(iso3c %in% c("RUS","SOV")) %>%
# #   dplyr::mutate(iso3c = "RUS")
# # 
# # tjk.ts <- cow %>%
# #   dplyr::filter(iso3c %in% c("TJK","SOV")) %>%
# #   dplyr::mutate(iso3c = "TJK")
# # 
# # tkm.ts <- cow %>%
# #   dplyr::filter(iso3c %in% c("TKM","SOV")) %>%
# #   dplyr::mutate(iso3c = "TKM")
# # 
# # ukr.ts <- cow %>%
# #   dplyr::filter(iso3c %in% c("UKR","SOV")) %>%
# #   dplyr::mutate(iso3c = "UKR")
# # 
# # uzb.ts <- cow %>%
# #   dplyr::filter(iso3c %in% c("UZB","SOV")) %>%
# #   dplyr::mutate(iso3c = "UZB")
# # 
# # #### Yugoslav successor states ----------------------------------------------------------------------
# # bih.ts <- cow %>%
# #   dplyr::filter(iso3c %in% c("BIH","YUG")) %>%
# #   dplyr::mutate(iso3c = "BIH")
# # 
# # hrv.ts <- cow %>%
# #   dplyr::filter(iso3c %in% c("HRV","YUG")) %>%
# #   dplyr::mutate(iso3c = "HRV")
# # 
# # mkd.ts <- cow %>%
# #   dplyr::filter(iso3c %in% c("MKD","YUG")) %>%
# #   dplyr::mutate(iso3c = "MKD")
# # 
# # srb.ts <- cow %>%
# #   dplyr::filter(iso3c %in% c("SRB","YUG")) %>%
# #   dplyr::mutate(iso3c = "SRB")
# # 
# # svn.ts <- cow %>%
# #   dplyr::filter(iso3c %in% c("SVN","YUG")) %>%
# #   dplyr::mutate(iso3c = "SVN")
# # 
# # ksv.ts <- cow %>%
# #   dplyr::filter(iso3c %in% c("KSV","SRB","YUG"),
# #                 iso3c != "SRB" | year %in% c(1992:2007)) %>%
# #   dplyr::mutate(iso3c = "KSV")
# # 
# # #### Yemen ----------------------------------------------------------------------
# # yar.ts <- cow %>%
# #   dplyr::filter(iso3c %in% c("YAR","YEM")) %>%
# #   dplyr::mutate(iso3c = "YAR")
# # 
# # ypr.ts <- cow %>%
# #   dplyr::filter(iso3c %in% c("YPR","YEM")) %>%
# #   dplyr::mutate(iso3c = "YPR")
# # 
# # #### Germany ----------------------------------------------------------------------
# # brd.ts <- cow %>%
# #   dplyr::filter(iso3c %in% c("BRD","DEU")) %>%
# #   dplyr::mutate(iso3c = "BRD")
# # 
# # ddr.ts <- cow %>%
# #   dplyr::filter(iso3c %in% c("DDR","DEU")) %>%
# #   dplyr::mutate(iso3c = "DDR")
# # 
# # #### Czechoslovakia ----------------------------------------------------------------------
# # svk.ts <- cow %>%
# #   dplyr::filter(iso3c %in% c("SVK","CZE"),
# #                 iso3c != "CZE" | year <= 1992) %>%
# #   dplyr::mutate(iso3c = "SVK")
# # 
# # #### Vietnam ----------------------------------------------------------------------
# # rvn.ts <- cow %>%
# #   dplyr::filter(iso3c %in% c("RVN","VNM"),
# #                 iso3c != "VNM" | year >= 1976) %>%
# #   dplyr::mutate(iso3c = "RVN")
# # 
# # cow <- cow %>%
# #   dplyr::filter(iso3c %!in% c("ARM","AZE","BLR","EST","GEO","KAZ","KGZ","LTU","LVA","MDA",
# #                               "RUS","TJK","TKM","UKR","UZB","BIH","HRV","MKD","SRB","SVN",
# #                               "KSV","YAR","YPR","BRD","DDR","SVK","RVN")) %>%
# #   rbind(arm.ts,aze.ts,blr.ts,est.ts,geo.ts,kaz.ts,kgz.ts,ltu.ts,lva.ts,mda.ts,rus.ts,tjk.ts,
# #         tkm.ts,ukr.ts,uzb.ts,bih.ts,hrv.ts,mkd.ts,srb.ts,svn.ts,ksv.ts,yar.ts,ypr.ts,brd.ts,
# #         ddr.ts,svk.ts,rvn.ts)
# # 
# # #### plot military capacity function ----------------------------------------------------------------------
# # # function that plots the military capacity of a single country by its iso3c code
# # plot.mc <- function(iso = "USA"){
# #   tmp <- cow %>%
# #     dplyr::filter(iso3c == iso) %>%
# #     dplyr::arrange(year)
# #   
# #   plot(tmp$year,tmp$mil.cap,type='l')
# # }
# # 
# # ### write data ----------------------------------------------------------------------
# # # writes formatted dataframe as csv files
# # write.csv(cow,"Data files/Formatted data files/military_capacity.csv",row.names = FALSE)
# 
# 
# # #### logistf against conflict by component ####
# # miltest <- ucdp4 %>%
# #   dplyr::select(confid,iso3c,year,conflict) %>%
# #   dplyr::full_join(cow,by=c("iso3c","year"))
# # 
# # milglm1 <- logistf(conflict ~ lnmilexpgdp, data = miltest, pl = T)
# # summary(milglm1)
# # 
# # milglm2 <- logistf(conflict ~ lntroops, data = miltest, pl = T)
# # summary(milglm2)
# # 
# # milglm3 <- logistf(conflict ~ lnmilexpc, data = miltest, pl = T)
# # summary(milglm3)
# # 
# # milglm4 <- logistf(conflict ~ lnmilexpt, data = miltest, pl = T)
# # summary(milglm4)
