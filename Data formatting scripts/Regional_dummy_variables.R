# This script formats two regional dummy variables at the country level, based on country_regions.xlsx and country_regions2.xlsx.
# Continent-Region level: Americas, Asia, Europe, Middle East & North Africa (MENA), and Sub-Saharan Africa (SSA)
# Sub-Region level: Western Europe & Others Group (WEOG),	Central America,	Carribean,	South America,	Eastern Europe,	Middle East,	North Africa,	East Africa,	West Africa,	Central Africa,	Southern Africa,	Central Asia,	South Asia,	East Asia, and	Southeast Asia

# load libraries
library(readxl)
library(tidyverse)

# load data files and add 0s
country_regions1 <- readxl::read_excel("Data files/country_regions.xlsx") %>%
  dplyr::mutate(across(everything(), .fns = ~replace_na(.,0))) 
country_regions2 <- readxl::read_excel("Data files/country_regions2.xlsx") %>%
  dplyr::mutate(across(everything(), .fns = ~replace_na(.,0))) 