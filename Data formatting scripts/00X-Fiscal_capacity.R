# this script takes relative capacity and tax revenue metrics to calculate a fiscal capacity
# metric for each country-year

### load libraries ----------------------------------------------------------------------
library(countrycode)
library(dplyr)

### load data files ----------------------------------------------------------------------
# load formatted data output by script 007-Tax_revenue
tax_revenue <- read.csv("Data files/Formatted data files/tax_revenue.csv") %>%
  dplyr::select(-country)

# load formatted data output by script 008-Relative_capacity
relative_capacity <- read.csv("Data files/Formatted data files/relative_capacity.csv") %>%
  dplyr::select(-country)

# load formatted data output by script 009-Aid
aid <- read.csv("Data files/Formatted data files/aid.csv") %>%
  dplyr::select(-country)

# load formatted data output by script 005-Country_years
cyears <- read.csv("Data files/Formatted data files/country_years.csv") %>%
  dplyr::select(-country)

# load formatted data output by script 004-Population
population <- read.csv("Data files/Formatted data files/population.csv") %>%
  dplyr::select(-country)

# load formatted data output by script 003-GDP
gdp <- read.csv("Data files/Formatted data files/gdp.csv") %>%
  dplyr::select(-country)

### format datasets ----------------------------------------------------------------------
# join datasets
fiscap <- dplyr::full_join(tax_revenue,relative_capacity,by=c("iso3c","year")) %>%
  dplyr::full_join(aid,by=c("iso3c","year")) %>%
  dplyr::left_join(cyears,by=c("iso3c","year")) %>%
  dplyr::left_join(population,by=c("iso3c","year")) %>%
  dplyr::left_join(gdp,by=c("iso3c","year")) %>%
  dplyr::filter(cn == 1) %>%
  dplyr::select(-cn) %>%
  #dplyr::filter(iso3c %!in% c("CPV","LUX","MLT","FSM","VUT","BHS","ATG","BRB","DMA","GRD","LCA","VCT",
  #                            "KNA","MCO","LIE","AND",NA,"VAT","SMR","MNE","ZAN","COM","BTN","MDV",
  #                            "BRN","SLB","KIR","TUV","TON","NRU","MHL","PLW","WSM","DJI","BLZ","FJI")) %>%
  dplyr::mutate(gdppc = gdp / population, # gdp per capita
                lngdppc = log(gdppc), # natural log of gdp per capita
                aidpc = aid2 / population, # aid per capita
                lnaidpc = log(aidpc), # natural log of aid per capita
                lnaidgdp = log(aidgdp), # natural log of aid as a proportion of gdp
                lntaxgdp = log(taxgdp)) %>% # natural log of tax as a proportion of gdp
  # replace NaNs and Infs with 0s
  dplyr::mutate_all(~ifelse(is.infinite(.), 0, .),
                    ~ifelse(is.nan(.), 0, .))

### Principal component analysis ----------------------------------------------------------------------
# correlation matrix using complete pairwise observations
cor(fiscap %>% dplyr::select(c(rpe_gdp,taxgdp,aidgdp,lntaxgdp,lnaidgdp,aidpc,lnaidpc,gdppc,lngdppc)),use="pairwise.complete.obs")

# creates dataset of only iso3c, year, and the component metrics
fiscap_pca_df <- fiscap %>%
  dplyr::select(iso3c,year,rpe_gdp,taxgdp,aidgdp,lntaxgdp,lnaidgdp,aidpc,lnaidpc,gdppc,lngdppc) %>%
  # omits country-years with missing component metric values
  na.omit()

# computes PCA
fiscap_pca <- stats::prcomp(~ rpe_gdp + taxgdp + aidgdp, data = fiscap_pca_df, retx = T, center = T, scale. = T)

# correlations between each of the three components
cor(fiscap %>% dplyr::select(c(rpe_gdp,taxgdp,aidgdp)),use="pairwise.complete.obs")

fiscap_pca_df <- fiscap_pca_df %>%
  # combines principal component scores with PCA dataset
  cbind(fiscap_pca[["x"]]) %>%
  # drops second and third principal component scores as well as the component metrics
  dplyr::select(iso3c,year,PC1) %>%
  # renames first principal component as polity.pca
  dplyr::rename(fiscap.pca = PC1) %>%
  # creates polity.pca.sq variable, the square of polity.pca
  dplyr::mutate(fiscap.pca.sq = fiscap.pca^2)

# average unweighted fiscal capacity by year
fis_cap_yearly_avg <- fiscap_pca_df %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(avg = mean(fiscap.pca)) %>%
  dplyr::ungroup()

plot(fis_cap_yearly_avg$year,fis_cap_yearly_avg$avg,type='l')

### time series for countries that unified/dissolved ----------------------------------------------------------------------
# SOV
arm.f <- fcpca1 %>%
  filter(iso3c %in% c("ARM","SOV")) %>%
  mutate(iso3c = "ARM")

aze.f <- fcpca1 %>%
  filter(iso3c %in% c("AZE","SOV")) %>%
  mutate(iso3c = "AZE")

blr.f <- fcpca1 %>%
  filter(iso3c %in% c("BLR","SOV")) %>%
  mutate(iso3c = "BLR")

est.f <- fcpca1 %>%
  filter(iso3c %in% c("EST","SOV")) %>%
  mutate(iso3c = "EST")

geo.f <- fcpca1 %>%
  filter(iso3c %in% c("GEO","SOV")) %>%
  mutate(iso3c = "GEO")

ltu.f <- fcpca1 %>%
  filter(iso3c %in% c("LTU","SOV")) %>%
  mutate(iso3c = "LTU")

lva.f <- fcpca1 %>%
  filter(iso3c %in% c("LVA","SOV")) %>%
  mutate(iso3c = "LVA")

kaz.f <- fcpca1 %>%
  filter(iso3c %in% c("KAZ","SOV")) %>%
  mutate(iso3c = "KAZ")

kgz.f <- fcpca1 %>%
  filter(iso3c %in% c("KGZ","SOV")) %>%
  mutate(iso3c = "KGZ")

mda.f <- fcpca1 %>%
  filter(iso3c %in% c("MDA","SOV")) %>%
  mutate(iso3c = "MDA")

rus.f <- fcpca1 %>%
  filter(iso3c %in% c("RUS","SOV")) %>%
  mutate(iso3c = "RUS")

tjk.f <- fcpca1 %>%
  filter(iso3c %in% c("TJK","SOV")) %>%
  mutate(iso3c = "TJK")

tkm.f <- fcpca1 %>%
  filter(iso3c %in% c("TKM","SOV")) %>%
  mutate(iso3c = "TKM")

ukr.f <- fcpca1 %>%
  filter(iso3c %in% c("UKR","SOV")) %>%
  mutate(iso3c = "UKR")

uzb.f <- fcpca1 %>%
  filter(iso3c %in% c("UZB","SOV")) %>%
  mutate(iso3c = "UZB")

fcpca1 <- fcpca1 %>%
  filter(iso3c %!in% c("ARM","AZE","BLR","EST","GEO","KAZ","KGZ","LTU","LVA","MDA","RUS","TJK","TKM","UKR","UZB")) %>%
  rbind(arm.f,aze.f,blr.f,est.f,geo.f,kaz.f,kgz.f,ltu.f,lva.f,mda.f,rus.f,tjk.f,tkm.f,ukr.f,uzb.f)

# 2016
gdppop <- full_join(gdp,pd,by=c("iso3c","year")) %>%
  mutate(gdppc = gdp / pop.pd) %>%
  mutate(lngdppc = log(gdppc)) %>%
  filter(year == 2016)

fc.2016 <- fcpca1 %>%
  filter(year == 2016) %>%
  full_join(gdppop)

plot(fc.2016$gdppc,fc.2016$fis.cap)
cor(fc.2016$gdppc,fc.2016$fis.cap, use = "complete.obs")


#### plot fc country ####
plot.fc <- function(iso = "USA"){
  tmp <- fcpca1 %>%
    filter(iso3c == iso) %>%
    arrange(year)
  
  plot(tmp$year,tmp$fis.cap,type='l')
}

plot(fcpca1$year,fcpca1$fis.cap)

# milandfis <- full_join(cowpca1,fcpca1,by=c("iso3c","year"))
# cor(milandfis$mil.cap,milandfis$fis.cap, use = "complete.obs")

# 
plot(milandfis$mil.cap,milandfis$fis.cap)

#### fiscap and gdppc ####
fvg <- fcpca1 %>%
  left_join(gdp) %>%
  left_join(pd) %>%
  mutate(gdppc = gdp / pop.pd)

plot(fvg$fis.cap,fvg$gdppc)
cor(fvg$fis.cap,fvg$gdppc)
