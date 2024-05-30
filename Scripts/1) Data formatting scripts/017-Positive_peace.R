
### load libraries ----------------------------------------------------------------------
library(tidyr)
library(dplyr)

### load data file ----------------------------------------------------------------------
ppi <- read.csv("~/Downloads/ltppi.csv")

### data formatting ----------------------------------------------------------------------
ppitotal <- ppi %>%
  dplyr::filter(variablename == "Overall PPI Score") %>%
  dplyr::rename(ppiscore = value)

ppiparts <- ppi %>%
  dplyr::filter(variablename != "Overall PPI Score") %>%
  dplyr::select(-c(region,ranking)) %>%
  tidyr::pivot_wider(names_from = variablename, values_from = value)

ppiparts <- ppiparts[,c(1:10)]
names(ppiparts) <- c("iso3c","country","year","ARO","EDR","FFI","HLC","LLC","SBE","WFG")

ppi <- ppi %>%
  dplyr::select(iso3c,variablename,year,value) %>%
  tidyr::pivot_wider(names_from = "variablename", values_from = value)

### write data ----------------------------------------------------------------------
# writes formatted dataframe as csv files
write.csv(ppi,"Data files/Formatted data files/positive_peace.csv",row.names = FALSE)
