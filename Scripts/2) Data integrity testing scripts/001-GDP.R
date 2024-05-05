# This script tests the integrity of the formatted GDP dataset.

### load libraries ----------------------------------------------------------------------
library(dplyr)

### load data ----------------------------------------------------------------------
gdp <- read.csv("Data files/Formatted data files/gdp.csv")

### GDP estimate correlations ----------------------------------------------------------------------

# GDP pwt and gl estimates - overall
gdp.cor.overall <- cor(
  gdp$gdp.pwt.est, gdp$gdp.gl.est,
  use = "pairwise.complete.obs"
  )

# GDP pwt and gl estimates - by iso3c code
gdp.cor.iso3c <- gdp %>%
  dplyr::group_by(iso3c) %>%
  dplyr::summarise(
    cor = cor(gdp.pwt.est, gdp.gl.est, use = "pairwise.complete.obs")
    ) %>%
  dplyr::ungroup()

# GDP pwt and gl estimates - by year
gdp.cor.year <- gdp %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(
    cor = cor(gdp.pwt.est, gdp.gl.est, use = "pairwise.complete.obs")
  ) %>%
  dplyr::ungroup()

### GDP growth estimate correlations ----------------------------------------------------------------------

# GDP growth rates - overall
gdp.growth.cor.overall <- gdp %>%
  dplyr::select(
    "gdp.growth.rate.pwt.est",
    "gdp.growth.rate.gl.est",
    "imf.growth.rate.modern",
    "imf.growth.rate.historical",
    "wb.growth.rate"
  )
gdp.growth.cor.overall <- cor(
  gdp.growth.cor.overall,
  use = "pairwise.complete.obs"
  )

# GDP growth rates pwt and gl estimates - by iso3c code
gdp.growth.cor.iso3c <- gdp %>%
  dplyr::group_by(iso3c) %>%
  dplyr::summarise(
    cor = cor(
      gdp.growth.rate.pwt.est,
      gdp.growth.rate.gl.est,
      use = "pairwise.complete.obs"
      )
  ) %>%
  dplyr::ungroup()

# GDP growth rates pwt and gl estimates - by year
gdp.growth.cor.year <- gdp %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(
    cor = cor(
      gdp.growth.rate.pwt.est,
      gdp.growth.rate.gl.est,
      use = "pairwise.complete.obs"
    )
  ) %>%
  dplyr::ungroup()
