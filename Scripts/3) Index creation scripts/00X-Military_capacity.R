# This script creates a military capacity index score for each country-year.

### load libraries ----------------------------------------------------------------------
library(countrycode)
library(lavaan)
library(dplyr)
library(tidyr)

### load data ----------------------------------------------------------------------
mil.metrics <- read.csv("Data files/Formatted data files/Military_data.csv")

### add logarithmic metrics ----------------------------------------------------------------------
mil.metrics <- mil.metrics %>%
  dplyr::mutate(
    mil.expenditure.cow.ln = log(mil.expenditure.cow),
    mil.expenditure.cow.alt.ln = log(mil.expenditure.cow.alt),
    mil.personnel.cow.ln = log(mil.personnel.cow),
    mil.expenditure.wmeat.ln = log(mil.expenditure.wmeat),
    mil.personnel.wmeat.ln = log(mil.personnel.wmeat),
    mil.expenditure.sipri.ln = log(mil.expenditure.sipri),
    mil.expenditure.per.capita.cow.un.ln = log(mil.expenditure.per.capita.cow.un),
    mil.expenditure.per.capita.cow.alt.un.ln = log(mil.expenditure.per.capita.cow.alt.un),
    mil.expenditure.per.capita.cow.cow.ln = log(mil.expenditure.per.capita.cow.cow),
    mil.expenditure.per.capita.cow.alt.cow.ln = log(mil.expenditure.per.capita.cow.alt.cow),
    mil.expenditure.per.capita.wmeat.un.ln = log(mil.expenditure.per.capita.wmeat.un),
    mil.expenditure.per.capita.wmeat.cow.ln = log(mil.expenditure.per.capita.wmeat.cow),
    mil.expenditure.per.capita.sipri.un.ln = log(mil.expenditure.per.capita.sipri.un),
    mil.expenditure.per.capita.sipri.cow.ln = log(mil.expenditure.per.capita.sipri.cow),
    mil.expenditure.perc.gdp.cow.pwt.ln = log(mil.expenditure.perc.gdp.cow.pwt),
    mil.expenditure.perc.gdp.cow.alt.pwt.ln = log(mil.expenditure.perc.gdp.cow.alt.pwt),
    mil.expenditure.perc.gdp.cow.gl.ln = log(mil.expenditure.perc.gdp.cow.gl),
    mil.expenditure.perc.gdp.cow.alt.gl.ln = log(mil.expenditure.perc.gdp.cow.alt.gl),
    mil.expenditure.perc.gdp.wmeat.pwt.ln = log(mil.expenditure.perc.gdp.wmeat.pwt),
    mil.expenditure.perc.gdp.wmeat.gl.ln = log(mil.expenditure.perc.gdp.wmeat.gl),
    mil.expenditure.perc.gdp.sipri.pwt.ln = log(mil.expenditure.perc.gdp.sipri.pwt),
    mil.expenditure.perc.gdp.sipri.gl.ln = log(mil.expenditure.perc.gdp.sipri.gl),
    mil.expenditure.per.personnel.cow.cow.ln = log(mil.expenditure.per.personnel.cow.cow),
    mil.expenditure.per.personnel.cow.alt.cow.ln = log(mil.expenditure.per.personnel.cow.alt.cow),
    mil.expenditure.per.personnel.cow.wmeat.ln = log(mil.expenditure.per.personnel.cow.wmeat),
    mil.expenditure.per.personnel.cow.alt.wmeat.ln = log(mil.expenditure.per.personnel.cow.alt.wmeat),
    mil.expenditure.per.personnel.wmeat.cow.ln = log(mil.expenditure.per.personnel.wmeat.cow),
    mil.expenditure.per.personnel.wmeat.wmeat.ln = log(mil.expenditure.per.personnel.wmeat.wmeat),
    mil.expenditure.per.personnel.sipri.cow.ln = log(mil.expenditure.per.personnel.sipri.cow),
    mil.expenditure.per.personnel.sipri.wmeat.ln = log(mil.expenditure.per.personnel.sipri.wmeat),
    mil.personnel.per.capita.cow.un.ln = log(mil.personnel.per.capita.cow.un),
    mil.personnel.per.capita.cow.cow.ln = log(mil.personnel.per.capita.cow.cow),
    mil.personnel.per.capita.wmeat.un.ln = log(mil.personnel.per.capita.wmeat.un),
    mil.personnel.per.capita.wmeat.cow.ln = log(mil.personnel.per.capita.wmeat.cow),
    gdp.per.mil.personnel.pwt.cow.ln = log(gdp.per.mil.personnel.pwt.cow),
    gdp.per.mil.personnel.pwt.wmeat.ln = log(gdp.per.mil.personnel.pwt.wmeat),
    gdp.per.mil.personnel.gl.cow.ln = log(gdp.per.mil.personnel.gl.cow),
    gdp.per.mil.personnel.gl.wmeat.ln = log(gdp.per.mil.personnel.gl.wmeat)
  )

# which countries have Inf values?
mil.metrics.inf <- mil.metrics %>%
  dplyr::select(
    iso3c, year, dplyr::all_of(ccpu_vars_pca)
  ) %>%
  dplyr::filter(
    is.infinite(mil.expenditure.per.capita.cow.un.ln) |
      is.infinite(mil.expenditure.perc.gdp.cow.pwt.ln) |
      is.infinite(mil.expenditure.per.personnel.cow.cow.ln) |
      is.infinite(mil.personnel.per.capita.cow.un.ln) |
      is.infinite(gdp.per.mil.personnel.pwt.cow.ln)
  ) %>%
  dplyr::group_by(iso3c) %>%
  dplyr::tally()
    
mil.metrics[as.matrix(mil.metrics) == Inf]  <- 0
mil.metrics[as.matrix(mil.metrics) == -Inf]  <- 0

mil.metrics <- do.call(data.frame,lapply(mil.metrics, function(x) replace(x, is.infinite(x),0)))


### Index CCPU -------------------------------------------------------------------------------------
# military expenditure - COW
# military personnel - COW
# GDP - PWT
# Population - UN

ccpu_vars_all <- c(
  "mil.expenditure.cow","mil.personnel.cow","mil.expenditure.per.capita.cow.un",
  "mil.expenditure.perc.gdp.cow.pwt","mil.expenditure.per.personnel.cow.cow",
  "mil.personnel.per.capita.cow.un","gdp.per.mil.personnel.pwt.cow","mil.expenditure.cow.ln",
  "mil.personnel.cow.ln","mil.expenditure.per.capita.cow.un.ln","mil.expenditure.perc.gdp.cow.pwt.ln",
  "mil.expenditure.per.personnel.cow.cow.ln","mil.personnel.per.capita.cow.un.ln",
  "gdp.per.mil.personnel.pwt.cow.ln"
  )

ccpu_vars_pca <- c(
  "mil.expenditure.per.capita.cow.un.ln","mil.expenditure.perc.gdp.cow.pwt.ln",
  "mil.expenditure.per.personnel.cow.cow.ln","mil.personnel.per.capita.cow.un.ln",
  "gdp.per.mil.personnel.pwt.cow.ln"
  )

# index_ccpu_data <- index_ccpu_data %>%
#   dplyr::mutate(
#     mil.expenditure.per.capita.cow.un.ln <- dplyr::case_when(
#       is.infinite(mil.expenditure.per.capita.cow.un.ln)  ~ 0,
#       .default = mil.expenditure.per.capita.cow.un.ln
#     )
#   ) %>%
#   dplyr::filter(
#     is.infinite(mil.expenditure.per.capita.cow.un.ln)
#   )
# 
# index_ccpu_data$mil.expenditure.per.capita.cow.un.ln[is.infinite(index_ccpu_data$mil.expenditure.per.capita.cow.un.ln)] <- 0
# index_ccpu_data$mil.expenditure.perc.gdp.cow.pwt.ln[is.infinite(index_ccpu_data$mil.expenditure.perc.gdp.cow.pwt.ln)] <- 0
# index_ccpu_data$mil.expenditure.per.personnel.cow.cow.ln[is.infinite(index_ccpu_data$mil.expenditure.per.personnel.cow.cow.ln)] <- 0
# index_ccpu_data$mil.personnel.per.capita.cow.un.ln[is.infinite(index_ccpu_data$mil.personnel.per.capita.cow.un.ln)] <- 0
# index_ccpu_data$gdp.per.mil.personnel.pwt.cow.ln[is.infinite(index_ccpu_data$gdp.per.mil.personnel.pwt.cow.ln)] <- 0
# 
# table(is.infinite(index_ccpu_data$mil.expenditure.per.capita.cow.un.ln), useNA='always')

index_ccpu_data <- mil.metrics %>%
  dplyr::select(
    iso3c, year, dplyr::all_of(ccpu_vars_pca)
    ) %>%
  dplyr::filter(
    iso3c %!in% c(
      "DMA","GRD","LCA","VCT","ATG","KNA","MCO","LIE","AND","SMR","ISL","MDV","VUT",
      "SLB","KIR","TUV","TON","NRU","MHL","PLW","FSM","WSM","PSE",
      "BHS","BRB","BRN","BTN","CPV","MUS","SYC","STP" ###
    )
  )

# standardize: (x - μ) / σ

# mil.expenditure.per.capita.cow.un.ln
mu_var1 <- mean(index_ccpu_data$mil.expenditure.per.capita.cow.un.ln, na.rm = TRUE)
sd_var1 <- sd(index_ccpu_data$mil.expenditure.per.capita.cow.un.ln, na.rm = TRUE)

# mil.expenditure.perc.gdp.cow.pwt.ln
mu_var2 <- mean(index_ccpu_data$mil.expenditure.perc.gdp.cow.pwt.ln, na.rm = TRUE)
sd_var2 <- sd(index_ccpu_data$mil.expenditure.perc.gdp.cow.pwt.ln, na.rm = TRUE)

# mil.expenditure.per.personnel.cow.cow.ln
mu_var3 <- mean(index_ccpu_data$mil.expenditure.per.personnel.cow.cow.ln, na.rm = TRUE)
sd_var3 <- sd(index_ccpu_data$mil.expenditure.per.personnel.cow.cow.ln, na.rm = TRUE)

# mil.personnel.per.capita.cow.un.ln
mu_var4 <- mean(index_ccpu_data$mil.personnel.per.capita.cow.un.ln, na.rm = TRUE)
sd_var4 <- sd(index_ccpu_data$mil.personnel.per.capita.cow.un.ln, na.rm = TRUE)

# gdp.per.mil.personnel.pwt.cow.ln
mu_var5 <- mean(index_ccpu_data$gdp.per.mil.personnel.pwt.cow.ln, na.rm = TRUE)
sd_var5 <- sd(index_ccpu_data$gdp.per.mil.personnel.pwt.cow.ln, na.rm = TRUE)

index_ccpu_data <- index_ccpu_data %>%
  dplyr::mutate(
    mil.expenditure.per.capita.cow.un.ln = (mil.expenditure.per.capita.cow.un.ln - mu_var1) / sd_var1,
    mil.expenditure.perc.gdp.cow.pwt.ln = (mil.expenditure.perc.gdp.cow.pwt.ln - mu_var2) / sd_var2,
    mil.expenditure.per.personnel.cow.cow.ln = (mil.expenditure.per.personnel.cow.cow.ln - mu_var3) / sd_var3,
    mil.personnel.per.capita.cow.un.ln = (mil.personnel.per.capita.cow.un.ln - mu_var4) / sd_var4,
    gdp.per.mil.personnel.pwt.cow.ln = (gdp.per.mil.personnel.pwt.cow.ln - mu_var5) / sd_var5
  )


# %>%
#   tidyr::drop_na(dplyr::all_of(ccpu_vars_pca)) %>%
#   dplyr::filter(
#     mil.expenditure.per.capita.cow.un.ln %!in% c(-Inf,Inf),
#     mil.expenditure.perc.gdp.cow.pwt.ln %!in% c(-Inf,Inf),
#     mil.expenditure.per.personnel.cow.cow.ln %!in% c(-Inf,Inf),
#     mil.personnel.per.capita.cow.un.ln %!in% c(-Inf,Inf),
#     gdp.per.mil.personnel.pwt.cow.ln %!in% c(-Inf,Inf)
#   )
# 
# index_ccpu_cor <- cor(index_ccpu_data %>%
#                         dplyr::select(-c(iso3c,year)))

ccpu_pca <- stats::prcomp(
  ~  mil.expenditure.per.capita.cow.un.ln + mil.expenditure.perc.gdp.cow.pwt.ln +
    mil.expenditure.per.personnel.cow.cow.ln + mil.personnel.per.capita.cow.un.ln +
    gdp.per.mil.personnel.pwt.cow.ln,
  data = index_ccpu_data, retx = F, center = F, scale. = F)

index_ccpu <- index_ccpu_data %>%
  dplyr::mutate(
    mil.expenditure.per.capita.cow.un.ln = ccpu_pca$rotation[1,1] * mil.expenditure.per.capita.cow.un.ln,
    mil.expenditure.perc.gdp.cow.pwt.ln = ccpu_pca$rotation[2,1] * mil.expenditure.perc.gdp.cow.pwt.ln,
    mil.expenditure.per.personnel.cow.cow.ln = ccpu_pca$rotation[3,1] * mil.expenditure.per.personnel.cow.cow.ln,
    mil.personnel.per.capita.cow.un.ln = ccpu_pca$rotation[4,1] * mil.personnel.per.capita.cow.un.ln,
    gdp.per.mil.personnel.pwt.cow.ln = ccpu_pca$rotation[5,1] * gdp.per.mil.personnel.pwt.cow.ln,
    mil.cap = mil.expenditure.per.capita.cow.un.ln + mil.expenditure.perc.gdp.cow.pwt.ln +
      mil.expenditure.per.personnel.cow.cow.ln + mil.personnel.per.capita.cow.un.ln +
      gdp.per.mil.personnel.pwt.cow.ln,
    mil.cap.sq = mil.cap^2
  )


# index_ccpu <- index_ccpu_data %>%
#   # combines principal component scores with PCA dataset
#   cbind(ccpu_pca[["x"]]) %>%
#   # drops 2nd-5th principal component scores
#   dplyr::select(-c(PC2,PC3,PC4,PC5)) %>%
#   # renames first principal component as mil.cap
#   dplyr::rename(mil.cap = PC1) %>%
#   # creates mil.cap.sq variable, the square of mil.cap
#   # this tests for extremes of military capacity - extremely strong/weak vs. average capacity
#   dplyr::mutate(mil.cap.sq = mil.cap^2)

# test directionality of metric
# invert direction if KWT 1992 is negative - higher value is higher capacity.
if(index_ccpu$mil.cap[index_ccpu$iso3c=="KWT"&index_ccpu$year==1992]<1){
  index_ccpu <- index_ccpu %>%
    dplyr::mutate(mil.cap = -1 * mil.cap)
}


#### write data ----------------------------------------------------------------------
# writes formatted dataframe as csv files
write.csv(index_ccpu,"Data files/Formatted data files/military_capacity_index_ccpu.csv",row.names = FALSE)


### Latent CCPU ----------------------------------------------------------------------

# create covariance matrix
regression.cor <- cor(index_ccpu_data %>%
                        dplyr::select(-c(iso3c,year)))

regression.sd <- list()
for(c in 1:ncol(index_ccpu_cor)){
  regression.sd[c] <- sd(index_ccpu_cor[,c])
}
regression.sd <- unlist(regression.sd)

# name list
milindex_names <- c("ExpPerCap", "ExpPercGDP", "ExpPerPers", "PersPerCap", "GDPPerPers")

# name the variables
colnames(regression.cor) <- rownames(regression.cor) <- milindex_names
names(regression.sd) <- milindex_names
names(index_ccpu_data) <- c("iso3c", "year", milindex_names)

# convert correlations and SDs to covarainces
regression.cov <- cor2cov(regression.cor, regression.sd)

# specify single factor model
# regression.model <- 'MilCap =~ a*ExpPerCap + b*ExpPercGDP + c*ExpPerPers + d*PersPerCap + e*GDPPerPers'
# 
# regression.model <- 'MilCap =~ a*ExpPercGDP + d*PersPerCap + e*GDPPerPers'
# 
# model.expenditure <- 'MilCap.Exp =~ ExpPerCap + ExpPercGDP + ExpPerPers'

regression.model <- 'MilCap =~ a*ExpPerCap + b*ExpPercGDP + d*PersPerCap + e*GDPPerPers'


# fit model
regression.fit <- lavaan::cfa(model = regression.model,
                              data = index_ccpu_data)
summary(regression.fit,
        standardized = TRUE)
parameterEstimates(regression.fit,
                   standardized=TRUE)

# residual correlations
residuals(regression.fit, type="cor")
# measures of model fit 
fitMeasures(regression.fit)
# modification indices
modificationIndices(regression.fit)

# regression.fit.exp <- lavaan::cfa(model = model.expenditure,
#                                   data = index_ccpu_data)
# summary(regression.fit.exp,
#         standardized = TRUE)
# parameterEstimates(regression.fit.exp,
#                    standardized=TRUE)





regression.fit <- lavaan::cfa(model = regression.model, sample.cov=regression.cov, sample.nobs=550,  std.lv=FALSE)

# examine parameter estimates
summary(wisc4.fit,standardized=TRUE)
parameterEstimates(wisc4.fit,standardized=TRUE)

# check model
# model-implied covariances
fitted(wisc4.fit)
# transform model-implied covariances to correlations
wisc4Fit.cov <- fitted(wisc4.fit)$cov
wisc4Fit.cor <- cov2cor(wisc4Fit.cov)
# residual correlations
residuals(wisc4.fit,type="cor")
# measures of model fit 
fitMeasures(wisc4.fit)
# modification indices
modificationIndices(wisc4.fit)

# 
# ### load libraries ----------------------------------------------------------------------
# library(readxl)
# library(countrycode)
# library(imputeTS)
# library(mice)
# library(mtsdi)
# library(EFAtools)
# library(corrplot)
# library(dplyr)
# library(tidyr)
# 
# ### load data ----------------------------------------------------------------------
# mil.metrics <- read.csv("Data files/Formatted data files/Military_data.csv")
# 
# ### standardize data ----------------------------------------------------------------------
# mil.metrics <- mil.metrics %>%
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
#     gdp.per.mil.personnel.pwt.cow.ln = log(gdp.per.mil.personnel.pwt.cow),
#     gdp.per.mil.personnel.pwt.wmeat.ln = log(gdp.per.mil.personnel.pwt.wmeat),
#     gdp.per.mil.personnel.gl.cow.ln = log(gdp.per.mil.personnel.gl.cow),
#     gdp.per.mil.personnel.gl.wmeat.ln = log(gdp.per.mil.personnel.gl.wmeat)
#     )
# 
# ### define version variables ----------------------------------------------------------------------
# # expenditure: cow (c), cow alt (a), wmeat (w), sipri (s)
# # personnel: cow (c), wmeat (w)
# # gdp: pwt (p), gl (g)
# # population: un (u), wmeat (w)
# 
# # mil.expenditure
# # mil.personnel
# # mil.expenditure.per.capita
# # mil.expenditure.perc.gdp
# # mil.expenditure.per.personnel
# # mil.personnel.per.capita
# # gdp.per.mil.personnel
# # mil.expenditure.growth.rate
# # mil.personnel.growth.rate
# # mil.expenditure.per.capita.growth.rate
# # mil.expenditure.perc.gdp.growth.rate
# # mil.expenditure.per.personnel.growth.rate
# # mil.personnel.per.capita.growth.rate
# # gdp.per.mil.personnel.growth.rate
# 
# 
# # cow.cow.pwt.un - ccpu
# ccpu <- c(
#   "mil.expenditure.cow","mil.personnel.cow","mil.expenditure.per.capita.cow.un",
#           "mil.expenditure.perc.gdp.cow.pwt","mil.expenditure.per.personnel.cow.cow",
#           "mil.personnel.per.capita.cow.un","gdp.per.mil.personnel.pwt.cow"#,
#           #"mil.expenditure.growth.rate.cow","mil.personnel.growth.rate.cow",
#           #"mil.expenditure.per.capita.growth.rate.cow.un",
#           #"mil.expenditure.per.personnel.growth.rate.cow.cow",
#           #"mil.personnel.per.capita.growth.rate.cow.un",
#           #"gdp.per.mil.personnel.growth.rate.pwt.cow"
#   )
# ccpu <- c(ccpu,paste0(ccpu,".ln"))
# 
# # cow.cow.pwt.wmeat -ccpw
# # cow.cow.gl.un - ccgu
# # cow.cow.gl.wmeat - ccgw
# # cow.wmeat.pwt.un - cwpu
# # cow.wmeat.pwt.wmeat - cwpw
# # cow.wmeat.gl.un - cwgu
# # cow.wmeat.gl.wmeat - cwgw
# 
# # cow.alt.cow.pwt.un - acpu
# # cow.alt.cow.pwt.wmeat - acpw
# # cow.alt.cow.gl.un - acgu
# # cow.alt.cow.gl.wmeat - acgw
# # cow.alt.wmeat.pwt.un - awpu
# # cow.alt.wmeat.pwt.wmeat - awpw
# # cow.alt.wmeat.gl.un - awgu
# # cow.alt.wmeat.gl.wmeat - awgw
# 
# # wmeat.cow.pwt.un - wcpu
# # wmeat.cow.pwt.wmeat - wcpw
# # wmeat.cow.gl.un - wcgu
# # wmeat.cow.gl.wmeat - wcgw
# # wmeat.wmeat.pwt.un - wwpu
# # wmeat.wmeat.pwt.wmeat - wwpw
# # wmeat.wmeat.gl.un - wwgu
# # wmeat.wmeat.gl.wmeat -wwgw
# 
# # sipri.cow.pwt.un - scpu
# # sipri.cow.pwt.wmeat - scpw
# # sipri.cow.gl.un - scgu
# # sipri.cow.gl.wmeat - scgw
# # sipri.wmeat.pwt.un - swpu
# # sipri.wmeat.pwt.wmeat - swpw
# # sipri.wmeat.gl.un - swgu
# # sipri.wmeat.gl.wmeat - swgw
# 
# ##### test round ccpu #####
# mil.ccpu <- mil.metrics %>%
#   dplyr::select(iso3c,year,all_of(ccpu))%>%
#   dplyr::filter(
#     mil.expenditure.cow != 0,
#     mil.personnel.cow != 0,
#     iso3c %!in% c(
#       "DMA","GRD","LCA","VCT","ATG","KNA","MCO","LIE","AND","SMR","ISL","COM","MDV","VUT",
#       "SLB","KIR","TUV","TON","NRU","MHL","PLW","FSM","WSM","PSE"
#     )
#   )
# 
# mil.ccpu.noest <- mil.metrics %>%
#   dplyr::select(iso3c,year,all_of(ccpu),
#                 mil.expenditure.cow.est.flag,
#                 mil.personnel.cow.est.flag) %>%
#   dplyr::filter(
#     mil.expenditure.cow != 0,
#     mil.personnel.cow != 0,
#     mil.expenditure.cow.est.flag == 0,
#     mil.personnel.cow.est.flag == 0,
#     iso3c %!in% c(
#       "DMA","GRD","LCA","VCT","ATG","KNA","MCO","LIE","AND","SMR","ISL","COM","MDV","VUT",
#       "SLB","KIR","TUV","TON","NRU","MHL","PLW","FSM","WSM","PSE"
#     )
#   )
#   
# 
# # original grad school pca: lnmilexpgdp + lntroops + lnmilexpc + lnmilexpt
# 
# # non-growth
# ccpu.ng <- mil.ccpu %>%
#   dplyr::select(
#     mil.expenditure.cow,mil.personnel.cow,mil.expenditure.per.capita.cow.un,mil.expenditure.perc.gdp.cow.pwt,
#     mil.expenditure.per.personnel.cow.cow,mil.personnel.per.capita.cow.un,gdp.per.mil.personnel.pwt.cow,
#     mil.expenditure.cow.ln,mil.personnel.cow.ln,mil.expenditure.per.capita.cow.un.ln,mil.expenditure.perc.gdp.cow.pwt.ln,
#     mil.expenditure.per.personnel.cow.cow.ln,mil.personnel.per.capita.cow.un.ln,gdp.per.mil.personnel.pwt.cow.ln
#   ) %>%
#   na.omit()
# 
# ccpu.ng.cor <- cor(ccpu.ng)
# 
# corrplot(ccpu.ng.cor)
#                 
# 
# mil.all <- mil.metrics %>%
#   dplyr::select(
#     iso3c,
#     year,
#     # mil.expenditure.per.capita.cow.un.ln,
#     mil.expenditure.per.capita.cow.alt.un.ln,
#     # mil.expenditure.per.capita.cow.cow.ln,
#     # mil.expenditure.per.capita.cow.alt.cow.ln,
#     mil.expenditure.per.capita.wmeat.un.ln,
#     # mil.expenditure.per.capita.wmeat.cow.ln,
#     # mil.expenditure.per.capita.sipri.un.ln,
#     # mil.expenditure.per.capita.sipri.cow.ln,
#     # mil.expenditure.perc.gdp.cow.pwt.ln,
#     mil.expenditure.perc.gdp.cow.alt.pwt.ln,
#     # mil.expenditure.perc.gdp.cow.gl.ln,
#     # mil.expenditure.perc.gdp.cow.alt.gl.ln,
#     mil.expenditure.perc.gdp.wmeat.pwt.ln,
#     # mil.expenditure.perc.gdp.wmeat.gl.ln,
#     # mil.expenditure.perc.gdp.sipri.pwt.ln,
#     # mil.expenditure.perc.gdp.sipri.gl.ln,
#     # mil.expenditure.per.personnel.cow.cow.ln,
#     mil.expenditure.per.personnel.cow.alt.cow.ln,
#     # mil.expenditure.per.personnel.cow.wmeat.ln,
#     # mil.expenditure.per.personnel.cow.alt.wmeat.ln,
#     mil.expenditure.per.personnel.wmeat.cow.ln,
#     # mil.expenditure.per.personnel.wmeat.wmeat.ln,
#     # mil.expenditure.per.personnel.sipri.cow.ln,
#     # mil.expenditure.per.personnel.sipri.wmeat.ln,
#     mil.personnel.per.capita.cow.un.ln,
#     # mil.personnel.per.capita.cow.cow.ln,
#     mil.personnel.per.capita.wmeat.un.ln,
#     # mil.personnel.per.capita.wmeat.cow.ln,
#     gdp.per.mil.personnel.pwt.cow.ln,
#     gdp.per.mil.personnel.pwt.wmeat.ln,
#     # gdp.per.mil.personnel.gl.cow.ln,
#     # gdp.per.mil.personnel.gl.wmeat.ln
#   ) %>%
#   tidyr::pivot_longer(3:12, names_to = "variable", values_to = "value") %>%
#   dplyr::filter(value != Inf,
#                 value != -Inf) %>%
#   tidyr::pivot_wider(names_from = "variable", values_from = "value") %>%
#   dplyr::select(-c(iso3c,year)) %>%
#   drop_na()
# 
# mil.ccpu.complete <- mil.ccpu %>%
#   dplyr::select(mil.expenditure.perc.gdp.cow.pwt.ln,mil.personnel.cow.ln,mil.expenditure.per.capita.cow.un.ln,mil.expenditure.per.personnel.cow.cow.ln) %>%
#   na.omit() %>%
#   as.matrix()
# 
# mil.ccpu.complete2 <- mil.ccpu %>%
#   dplyr::select(mil.expenditure.perc.gdp.cow.pwt,mil.personnel.cow,mil.expenditure.per.capita.cow.un,mil.expenditure.per.personnel.cow.cow) %>%
#   na.omit()
# 
# ccpu3 <- mil.ccpu %>%
#   dplyr::select(mil.expenditure.per.capita.cow.un.ln,mil.expenditure.perc.gdp.cow.pwt,mil.expenditure.per.personnel.cow.cow.ln,
#                 mil.personnel.per.capita.cow.un,gdp.per.mil.personnel.pwt.cow.ln) %>%
#   na.omit()
# 
# ccpu4 <- mil.ccpu %>%
#   dplyr::select(mil.expenditure.per.capita.cow.un,mil.expenditure.perc.gdp.cow.pwt,mil.expenditure.per.personnel.cow.cow,
#                 mil.personnel.per.capita.cow.un,gdp.per.mil.personnel.pwt.cow) %>%
#   na.omit()
# 
# ccpu5 <- mil.ccpu %>%
#   dplyr::select(mil.expenditure.per.capita.cow.un.ln,mil.expenditure.perc.gdp.cow.pwt.ln,mil.expenditure.per.personnel.cow.cow.ln,
#                 mil.personnel.per.capita.cow.un.ln,gdp.per.mil.personnel.pwt.cow.ln) %>%
#   na.omit()
# 
# ccpu6 <- mil.ccpu %>%
#   dplyr::select(mil.expenditure.per.capita.cow.un.ln,mil.expenditure.perc.gdp.cow.pwt.ln,mil.expenditure.per.personnel.cow.cow.ln,
#                 mil.personnel.per.capita.cow.un,gdp.per.mil.personnel.pwt.cow.ln) %>%
#   na.omit()
# 
# ccpu7 <- mil.ccpu %>%
#   dplyr::select(mil.expenditure.per.capita.cow.un.ln,mil.expenditure.perc.gdp.cow.pwt.ln,mil.expenditure.per.personnel.cow.cow.ln,
#                 mil.personnel.per.capita.cow.un,gdp.per.mil.personnel.pwt.cow) %>%
#   na.omit()
# 
# ccpu8 <- mil.ccpu %>%
#   dplyr::select(mil.personnel.cow.ln,mil.expenditure.per.capita.cow.un.ln,mil.expenditure.perc.gdp.cow.pwt.ln,mil.expenditure.per.personnel.cow.cow.ln,
#                 mil.personnel.per.capita.cow.un,gdp.per.mil.personnel.pwt.cow) %>%
#   na.omit()
# 
# ccpu9 <- mil.ccpu %>%
#   dplyr::select(mil.personnel.cow.ln,mil.expenditure.per.capita.cow.un.ln,mil.expenditure.perc.gdp.cow.pwt.ln,mil.expenditure.per.personnel.cow.cow.ln,
#                 mil.personnel.per.capita.cow.un.ln,gdp.per.mil.personnel.pwt.cow) %>%
#   na.omit()
# 
# ccpu10 <- mil.ccpu %>%
#   dplyr::select(mil.expenditure.perc.gdp.cow.pwt,mil.personnel.cow.ln,mil.expenditure.per.capita.cow.un,mil.expenditure.per.personnel.cow.cow,
#                 mil.personnel.per.capita.cow.un.ln,gdp.per.mil.personnel.pwt.cow) %>%
#   na.omit()
# 
# ccpu11 <- mil.ccpu %>%
#   dplyr::select(mil.expenditure.perc.gdp.cow.pwt,mil.expenditure.per.capita.cow.un,mil.expenditure.per.personnel.cow.cow,
#                 mil.personnel.per.capita.cow.un.ln,gdp.per.mil.personnel.pwt.cow) %>%
#   na.omit()
# 
# ccpu11 <- mil.ccpu %>%
#   dplyr::select(mil.expenditure.perc.gdp.cow.pwt,mil.expenditure.per.capita.cow.un,mil.expenditure.per.personnel.cow.cow,
#                 mil.personnel.per.capita.cow.un,gdp.per.mil.personnel.pwt.cow) %>%
#   na.omit()
# 
# ccpu12 <- mil.ccpu %>%
#   dplyr::select(mil.expenditure.per.capita.cow.un.ln,mil.expenditure.perc.gdp.cow.pwt.ln,mil.expenditure.per.personnel.cow.cow.ln,
#                 mil.personnel.per.capita.cow.un,gdp.per.mil.personnel.pwt.cow.ln) %>%
#   na.omit()
# 
# ccpu13 <- mil.ccpu %>%
#   dplyr::select(mil.expenditure.per.capita.cow.un.ln,mil.expenditure.perc.gdp.cow.pwt.ln,mil.expenditure.per.personnel.cow.cow.ln,
#                 mil.personnel.per.capita.cow.un,gdp.per.mil.personnel.pwt.cow) %>%
#   na.omit()
# 
# ccpu11.noest <- mil.ccpu.noest %>%
#   dplyr::select(mil.expenditure.perc.gdp.cow.pwt,mil.expenditure.per.capita.cow.un,mil.expenditure.per.personnel.cow.cow,
#                 mil.personnel.per.capita.cow.un,gdp.per.mil.personnel.pwt.cow) %>%
#   na.omit()
# 
# ccpu11.names <- mil.ccpu %>%
#   dplyr::select(iso3c,year,mil.expenditure.perc.gdp.cow.pwt,mil.expenditure.per.capita.cow.un,mil.expenditure.per.personnel.cow.cow,
#                 mil.personnel.per.capita.cow.un.ln,gdp.per.mil.personnel.pwt.cow) %>%
#   na.omit()
# 
# mil.ccpu.complete.cor <- cor(mil.ccpu.complete)
# mil.ccpu.complete2.cor <- cor(mil.ccpu.complete2)
# ccpu3.cor <- cor(ccpu3)
# ccpu4.cor <- cor(ccpu4)
# ccpu5.cor <- cor(ccpu5)
# ccpu6.cor <- cor(ccpu6)
# ccpu7.cor <- cor(ccpu7)
# ccpu8.cor <- cor(ccpu8)
# ccpu9.cor <- cor(ccpu9)
# ccpu10.cor <- cor(ccpu10)
# ccpu11.cor <- cor(ccpu11)
# ccpu12.cor <- cor(ccpu12)
# ccpu13.cor <- cor(ccpu13)
# ccpu11.noest.cor <- cor(ccpu11.noest)
# mil.all.cor <- cor(mil.all)
# 
# corrplot(mil.ccpu.complete2.cor)
# corrplot(ccpu4.cor)
# corrplot(ccpu6.cor)
# corrplot(ccpu7.cor)
# 
# unlist(determinant(mil.ccpu.complete.cor))
# unlist(determinant(mil.ccpu.complete2.cor))
# unlist(determinant(ccpu3.cor))
# unlist(determinant(ccpu4.cor))
# unlist(determinant(ccpu5.cor))
# unlist(determinant(ccpu10.cor))
# unlist(determinant(ccpu11.cor))
# unlist(determinant(mil.all.cor))
# 
# KMO(mil.ccpu.complete.cor)
# KMO(mil.ccpu.complete2.cor) # mediocre
# KMO(ccpu3.cor)
# KMO(ccpu4.cor) # mediocre
# KMO(ccpu5.cor)
# KMO(ccpu6.cor)
# KMO(ccpu7.cor)
# KMO(ccpu8.cor)
# KMO(ccpu9.cor)
# KMO(ccpu10.cor) # mediocre
# KMO(ccpu11.cor) # mediocre *
# KMO(ccpu12.cor)
# KMO(ccpu13.cor)
# KMO(ccpu11.noest.cor)
# KMO(mil.all.cor)
# 
# BARTLETT(mil.ccpu.complete2.cor, N = nrow(mil.ccpu.complete2))
# BARTLETT(ccpu4.cor, N = nrow(ccpu4))
# BARTLETT(ccpu10.cor, N = nrow(ccpu10))
# BARTLETT(ccpu11.cor, N = nrow(ccpu11))
# 
# ccpu_pca <- stats::prcomp(
#   ~  mil.expenditure.perc.gdp.cow.pwt.ln + mil.personnel.cow.ln + mil.expenditure.per.capita.cow.un.ln + mil.expenditure.per.personnel.cow.cow.ln,
#                          data = mil.ccpu, retx = T, center = T, scale. = T)
# 
# ### tests
# # determinant of the correlation matrix
# # bartlett test of sphericity
# # kaiser-meyer-olkin measure of sampling adequacy
# 
# ### construct metrics ----------------------------------------------------------------------
# ccpu_pca <- stats::prcomp(
#   ~  mil.expenditure.per.capita.cow.un.ln + mil.expenditure.perc.gdp.cow.pwt + mil.expenditure.per.personnel.cow.cow.ln +
#     mil.personnel.per.capita.cow.un.ln + gdp.per.mil.personnel.pwt.cow.ln,
#   data = mil.ccpu, retx = T, center = T, scale. = T)
# 
# # "mil.expenditure.per.capita.cow.un"   /   "mil.expenditure.per.capita.cow.un.ln"
# # "mil.expenditure.perc.gdp.cow.pwt"  /   "mil.expenditure.perc.gdp.cow.pwt.ln"
# # "mil.expenditure.per.personnel.cow.cow"   /   "mil.expenditure.per.personnel.cow.cow.ln"
# # "mil.personnel.per.capita.cow.un"   /   "mil.personnel.per.capita.cow.un.ln"
# # "gdp.per.mil.personnel.pwt.cow"   /   "gdp.per.mil.personnel.pwt.cow.ln"
# 
# ### principal component analysis ----------------------------------------------------------------------
# # PCA on lnmilexpgdp, lntroops, lnmilexpc, and lnmilexpt
# cow_pca <- stats::prcomp(~  lnmilexpgdp + lntroops + lnmilexpc + lnmilexpt,
#                          data = cow, retx = T, center = T, scale. = T)
# 
# cow <- cow %>%
#   # combines principal component scores with PCA dataset
#   cbind(cow_pca[["x"]]) %>%
#   # drops second, third, and fourth principal component scores
#   dplyr::select(-c(PC2,PC3,PC4)) %>%
#   # renames first principal component as mil.cap
#   dplyr::rename(mil.cap = PC1) %>%
#   # creates mil.cap.sq variable, the square of mil.cap
#   # this tests for extremes of military capacity - extremely strong/weak vs. average capacity
#   dplyr::mutate(mil.cap.sq = mil.cap^2)
# 
# # average unweighted military capacity by year
# mil_cap_yearly_avg <- cow %>%
#   dplyr::group_by(year) %>%
#   dplyr::summarise(avg = mean(mil.cap)) %>%
#   dplyr::ungroup()
# 
# plot(mil_cap_yearly_avg$year,mil_cap_yearly_avg$avg,type='l')
# 
# ### format data for countries that unified/dissolved ----------------------------------------------------------------------
# #### Soviet successor states ----------------------------------------------------------------------
# arm.ts <- cow %>%
#   dplyr::filter(iso3c %in% c("ARM","SOV")) %>%
#   dplyr::mutate(iso3c = "ARM")
# 
# aze.ts <- cow %>%
#   dplyr::filter(iso3c %in% c("AZE","SOV")) %>%
#   dplyr::mutate(iso3c = "AZE")
# 
# blr.ts <- cow %>%
#   dplyr::filter(iso3c %in% c("BLR","SOV")) %>%
#   dplyr::mutate(iso3c = "BLR")
# 
# est.ts <- cow %>%
#   dplyr::filter(iso3c %in% c("EST","SOV")) %>%
#   dplyr::mutate(iso3c = "EST")
# 
# geo.ts <- cow %>%
#   dplyr::filter(iso3c %in% c("GEO","SOV")) %>%
#   dplyr::mutate(iso3c = "GEO")
# 
# kaz.ts <- cow %>%
#   dplyr::filter(iso3c %in% c("KAZ","SOV")) %>%
#   dplyr::mutate(iso3c = "KAZ")
# 
# kgz.ts <- cow %>%
#   dplyr::filter(iso3c %in% c("KGZ","SOV")) %>%
#   dplyr::mutate(iso3c = "KGZ")
# 
# ltu.ts <- cow %>%
#   dplyr::filter(iso3c %in% c("LTU","SOV")) %>%
#   dplyr::mutate(iso3c = "LTU")
# 
# lva.ts <- cow %>%
#   dplyr::filter(iso3c %in% c("LVA","SOV")) %>%
#   dplyr::mutate(iso3c = "LVA")
# 
# mda.ts <- cow %>%
#   dplyr::filter(iso3c %in% c("MDA","SOV")) %>%
#   dplyr::mutate(iso3c = "MDA")
# 
# rus.ts <- cow %>%
#   dplyr::filter(iso3c %in% c("RUS","SOV")) %>%
#   dplyr::mutate(iso3c = "RUS")
# 
# tjk.ts <- cow %>%
#   dplyr::filter(iso3c %in% c("TJK","SOV")) %>%
#   dplyr::mutate(iso3c = "TJK")
# 
# tkm.ts <- cow %>%
#   dplyr::filter(iso3c %in% c("TKM","SOV")) %>%
#   dplyr::mutate(iso3c = "TKM")
# 
# ukr.ts <- cow %>%
#   dplyr::filter(iso3c %in% c("UKR","SOV")) %>%
#   dplyr::mutate(iso3c = "UKR")
# 
# uzb.ts <- cow %>%
#   dplyr::filter(iso3c %in% c("UZB","SOV")) %>%
#   dplyr::mutate(iso3c = "UZB")
# 
# #### Yugoslav successor states ----------------------------------------------------------------------
# bih.ts <- cow %>%
#   dplyr::filter(iso3c %in% c("BIH","YUG")) %>%
#   dplyr::mutate(iso3c = "BIH")
# 
# hrv.ts <- cow %>%
#   dplyr::filter(iso3c %in% c("HRV","YUG")) %>%
#   dplyr::mutate(iso3c = "HRV")
# 
# mkd.ts <- cow %>%
#   dplyr::filter(iso3c %in% c("MKD","YUG")) %>%
#   dplyr::mutate(iso3c = "MKD")
# 
# srb.ts <- cow %>%
#   dplyr::filter(iso3c %in% c("SRB","YUG")) %>%
#   dplyr::mutate(iso3c = "SRB")
# 
# svn.ts <- cow %>%
#   dplyr::filter(iso3c %in% c("SVN","YUG")) %>%
#   dplyr::mutate(iso3c = "SVN")
# 
# ksv.ts <- cow %>%
#   dplyr::filter(iso3c %in% c("KSV","SRB","YUG"),
#                 iso3c != "SRB" | year %in% c(1992:2007)) %>%
#   dplyr::mutate(iso3c = "KSV")
# 
# #### Yemen ----------------------------------------------------------------------
# yar.ts <- cow %>%
#   dplyr::filter(iso3c %in% c("YAR","YEM")) %>%
#   dplyr::mutate(iso3c = "YAR")
# 
# ypr.ts <- cow %>%
#   dplyr::filter(iso3c %in% c("YPR","YEM")) %>%
#   dplyr::mutate(iso3c = "YPR")
# 
# #### Germany ----------------------------------------------------------------------
# brd.ts <- cow %>%
#   dplyr::filter(iso3c %in% c("BRD","DEU")) %>%
#   dplyr::mutate(iso3c = "BRD")
# 
# ddr.ts <- cow %>%
#   dplyr::filter(iso3c %in% c("DDR","DEU")) %>%
#   dplyr::mutate(iso3c = "DDR")
# 
# #### Czechoslovakia ----------------------------------------------------------------------
# svk.ts <- cow %>%
#   dplyr::filter(iso3c %in% c("SVK","CZE"),
#                 iso3c != "CZE" | year <= 1992) %>%
#   dplyr::mutate(iso3c = "SVK")
# 
# #### Vietnam ----------------------------------------------------------------------
# rvn.ts <- cow %>%
#   dplyr::filter(iso3c %in% c("RVN","VNM"),
#                 iso3c != "VNM" | year >= 1976) %>%
#   dplyr::mutate(iso3c = "RVN")
# 
# cow <- cow %>%
#   dplyr::filter(iso3c %!in% c("ARM","AZE","BLR","EST","GEO","KAZ","KGZ","LTU","LVA","MDA",
#                               "RUS","TJK","TKM","UKR","UZB","BIH","HRV","MKD","SRB","SVN",
#                               "KSV","YAR","YPR","BRD","DDR","SVK","RVN")) %>%
#   rbind(arm.ts,aze.ts,blr.ts,est.ts,geo.ts,kaz.ts,kgz.ts,ltu.ts,lva.ts,mda.ts,rus.ts,tjk.ts,
#         tkm.ts,ukr.ts,uzb.ts,bih.ts,hrv.ts,mkd.ts,srb.ts,svn.ts,ksv.ts,yar.ts,ypr.ts,brd.ts,
#         ddr.ts,svk.ts,rvn.ts)
# 
# #### plot military capacity function ----------------------------------------------------------------------
# # function that plots the military capacity of a single country by its iso3c code
# plot.mc <- function(iso = "USA"){
#   tmp <- cow %>%
#     dplyr::filter(iso3c == iso) %>%
#     dplyr::arrange(year)
#   
#   plot(tmp$year,tmp$mil.cap,type='l')
# }
# 
# ### write data ----------------------------------------------------------------------
# # writes formatted dataframe as csv files
# write.csv(cow,"Data files/Formatted data files/military_capacity.csv",row.names = FALSE)


# #### logistf against conflict by component ####
# miltest <- ucdp4 %>%
#   dplyr::select(confid,iso3c,year,conflict) %>%
#   dplyr::full_join(cow,by=c("iso3c","year"))
# 
# milglm1 <- logistf(conflict ~ lnmilexpgdp, data = miltest, pl = T)
# summary(milglm1)
# 
# milglm2 <- logistf(conflict ~ lntroops, data = miltest, pl = T)
# summary(milglm2)
# 
# milglm3 <- logistf(conflict ~ lnmilexpc, data = miltest, pl = T)
# summary(milglm3)
# 
# milglm4 <- logistf(conflict ~ lnmilexpt, data = miltest, pl = T)
# summary(milglm4)
