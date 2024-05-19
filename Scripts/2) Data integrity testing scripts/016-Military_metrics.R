# This script tests the integrity of the formatted GDP dataset.

### load libraries ----------------------------------------------------------------------
library(dplyr)

### load data ----------------------------------------------------------------------
mil.metrics <- read.csv("Data files/Formatted data files/Military_data.csv")

### filter out small countries ----------------------------------------------------------------------
mil.metrics.no.small <- mil.metrics %>%
  dplyr::filter(
    iso3c %!in% c(
      "DMA","GRD","LCA","VCT","ATG","KNA","MCO","LIE","AND","SMR","ISL","COM","MDV","VUT","SLB","KIR",
      "TUV","TON","NRU","MHL","PLW","FSM","WSM","PSE","KSV"
      )
  ) %>%
  dplyr::mutate(
    # convert 0 values to NAs
    mil.expenditure.cow = ifelse(mil.expenditure.cow==0,NA,mil.expenditure.cow),
    mil.expenditure.cow.alt = ifelse(mil.expenditure.cow.alt==0,NA,mil.expenditure.cow.alt),
    mil.expenditure.wmeat = ifelse(mil.expenditure.wmeat==0,NA,mil.expenditure.wmeat),
    mil.expenditure.sipri = ifelse(mil.expenditure.sipri==0,NA,mil.expenditure.sipri),
    mil.personnel.cow = ifelse(mil.personnel.cow==0,NA,mil.personnel.cow),
    mil.personnel.wmeat = ifelse(mil.personnel.wmeat==0,NA,mil.personnel.wmeat)
  )

xx <- mil.metrics %>%
  dplyr::filter(
    iso3c %!in% c(
      "DMA","GRD","LCA","VCT","ATG","KNA","MCO","LIE","AND","SMR","ISL","COM","MDV","VUT","SLB","KIR",
      "TUV","TON","NRU","MHL","PLW","FSM","WSM","PSE","KSV"
    ),
    mil.expenditure.cow == 0 | mil.expenditure.cow.alt == 0 | mil.expenditure.wmeat == 0 |
      mil.expenditure.sipri == 0 | mil.personnel.cow == 0 | mil.personnel.wmeat == 0
  )

### variable groups ----------------------------------------------------------------------
mil.expenditure <- c("mil.expenditure.cow","mil.expenditure.cow.alt","mil.expenditure.wmeat","mil.expenditure.sipri")
mil.expenditure.flags <- c("mil.expenditure.cow.est.flag","mil.expenditure.cow.alt.est.flag",
                           "mil.expenditure.wmeat.est.flag","mil.expenditure.sipri.est.flag")
mil.personnel <- c("mil.personnel.cow","mil.personnel.wmeat")
mil.personnel.flags <- c("mil.personnel.cow.est.flag","mil.personnel.wmeat.est.flag")
mil.expenditure.per.capita <- c("mil.expenditure.per.capita.cow.un","mil.expenditure.per.capita.cow.alt.un",
                                "mil.expenditure.per.capita.cow.cow","mil.expenditure.per.capita.cow.alt.cow",
                                "mil.expenditure.per.capita.wmeat.un","mil.expenditure.per.capita.wmeat.cow",
                                "mil.expenditure.per.capita.sipri.un","mil.expenditure.per.capita.sipri.cow")    
mil.expenditure.perc.gdp <- c("mil.expenditure.perc.gdp.cow.pwt","mil.expenditure.perc.gdp.cow.alt.pwt",
                              "mil.expenditure.perc.gdp.cow.gl","mil.expenditure.perc.gdp.cow.alt.gl",
                              "mil.expenditure.perc.gdp.wmeat.pwt","mil.expenditure.perc.gdp.wmeat.gl",
                              "mil.expenditure.perc.gdp.sipri.pwt","mil.expenditure.perc.gdp.sipri.gl")
mil.expenditure.perc.personnel <- c("mil.expenditure.per.personnel.cow.cow","mil.expenditure.per.personnel.cow.alt.cow",
                                    "mil.expenditure.per.personnel.cow.wmeat","mil.expenditure.per.personnel.cow.alt.wmeat",
                                    "mil.expenditure.per.personnel.wmeat.cow","mil.expenditure.per.personnel.wmeat.wmeat",
                                    "mil.expenditure.per.personnel.sipri.cow","mil.expenditure.per.personnel.sipri.wmeat")
mil.personnel.per.capita <- c("mil.personnel.per.capita.cow.un","mil.personnel.per.capita.cow.cow",
                              "mil.personnel.per.capita.wmeat.un","mil.personnel.per.capita.wmeat.cow")
gdp.per.mil.personnel <- c("gdp.per.mil.personnel.pwt.cow","gdp.per.mil.personnel.pwt.wmeat",
                           "gdp.per.mil.personnel.gl.cow","gdp.per.mil.personnel.gl.wmeat")

mil.expenditure.growth <- c("mil.expenditure.growth.rate.cow","mil.expenditure.growth.rate.cow.alt",
                            "mil.expenditure.growth.rate.wmeat","mil.expenditure.growth.rate.sipri")
mil.personnel.growth <- c("mil.personnel.growth.rate.cow","mil.personnel.growth.rate.wmeat")
mil.expenditure.per.capita.growth <- c("mil.expenditure.per.capita.growth.rate.cow.un","mil.expenditure.per.capita.growth.rate.cow.alt.un",
                                       "mil.expenditure.per.capita.growth.rate.cow.cow","mil.expenditure.per.capita.growth.rate.cow.alt.cow",
                                       "mil.expenditure.per.capita.growth.rate.wmeat.un","mil.expenditure.per.capita.growth.rate.wmeat.cow",
                                       "mil.expenditure.per.capita.growth.rate.sipri.un","mil.expenditure.per.capita.growth.rate.sipri.cow")
mil.expenditure.perc.gdp.growth <- c("mil.expenditure.perc.gdp.growth.rate.cow.pwt","mil.expenditure.perc.gdp.growth.rate.cow.alt.pwt",
                                     "mil.expenditure.perc.gdp.growth.rate.cow.gl","mil.expenditure.perc.gdp.growth.rate.cow.alt.gl",
                                     "mil.expenditure.perc.gdp.growth.rate.wmeat.pwt","mil.expenditure.perc.gdp.growth.rate.wmeat.gl",
                                     "mil.expenditure.perc.gdp.growth.rate.sipri.pwt","mil.expenditure.perc.gdp.growth.rate.sipri.gl")
mil.expenditure.per.personnel.growth <- c("mil.expenditure.per.personnel.growth.rate.cow.cow","mil.expenditure.per.personnel.growth.rate.cow.alt.cow",
                                          "mil.expenditure.per.personnel.growth.rate.cow.wmeat","mil.expenditure.per.personnel.growth.rate.cow.alt.wmeat",
                                          "mil.expenditure.per.personnel.growth.rate.wmeat.cow","mil.expenditure.per.personnel.growth.rate.wmeat.wmeat",
                                          "mil.expenditure.per.personnel.growth.rate.sipri.cow","mil.expenditure.per.personnel.growth.rate.sipri.wmeat")
mil.personnel.per.capita.growth <- c("mil.personnel.per.capita.growth.rate.cow.un","mil.personnel.per.capita.growth.rate.cow.cow",
                                     "mil.personnel.per.capita.growth.rate.wmeat.un","mil.personnel.per.capita.growth.rate.wmeat.cow")
gdp.per.mil.personnel.growth <- c("gdp.per.mil.personnel.growth.rate.pwt.cow","gdp.per.mil.personnel.growth.rate.pwt.wmeat",
                                  "gdp.per.mil.personnel.growth.rate.gl.cow","gdp.per.mil.personnel.growth.rate.gl.wmeat")

### Military metrics estimate correlations ----------------------------------------------------------------------

# Military expenditure - overall
mil.expenditure.cor.overall <- cor(
  mil.metrics %>%
    dplyr::select(all_of(mil.expenditure)),
  use = "pairwise.complete.obs"
)

mil.expenditure.no.small.cor.overall <- cor(
  mil.metrics.no.small %>%
    dplyr::select(all_of(mil.expenditure)),
  use = "pairwise.complete.obs"
)

# Military expenditure - overall; natural log
mil.expenditure.cor.overall.ln <- cor(
  mil.metrics %>%
    dplyr::select(all_of(mil.expenditure)) %>%
    dplyr::filter(
      mil.expenditure.cow != 0 &
      mil.expenditure.cow.alt != 0 &
      mil.expenditure.wmeat != 0 &
      mil.expenditure.sipri != 0
      ) %>%
    dplyr::mutate(
      across(everything(), log)
      ),
  use = "pairwise.complete.obs"
)

mil.expenditure.no.small.cor.overall.ln <- cor(
  mil.metrics.no.small %>%
    dplyr::select(all_of(mil.expenditure)) %>%
    dplyr::filter(
      mil.expenditure.cow != 0 &
        mil.expenditure.cow.alt != 0 &
        mil.expenditure.wmeat != 0 &
        mil.expenditure.sipri != 0
    ) %>%
    dplyr::mutate(
      across(everything(), log)
    ),
  use = "pairwise.complete.obs"
)

# Military expenditure - overall; no estimates
mil.expenditure.cor.overall.no.est <- cor(
  mil.metrics %>%
    dplyr::mutate(
      mil.expenditure.cow = ifelse(mil.expenditure.cow.est.flag==0,NA,mil.expenditure.cow),
      mil.expenditure.cow.alt = ifelse(mil.expenditure.cow.alt.est.flag==0,NA,mil.expenditure.cow.alt),
      mil.expenditure.wmeat = ifelse(mil.expenditure.cow.est.flag==0,NA,mil.expenditure.wmeat),
      mil.expenditure.sipri = ifelse(mil.expenditure.cow.est.flag==0,NA,mil.expenditure.sipri)
    ) %>%
    dplyr::select(all_of(mil.expenditure)),
  use = "pairwise.complete.obs"
)

mil.expenditure.no.small.cor.overall.no.est <- cor(
  mil.metrics.no.small %>%
    dplyr::mutate(
      mil.expenditure.cow = ifelse(mil.expenditure.cow.est.flag==0,NA,mil.expenditure.cow),
      mil.expenditure.cow.alt = ifelse(mil.expenditure.cow.alt.est.flag==0,NA,mil.expenditure.cow.alt),
      mil.expenditure.wmeat = ifelse(mil.expenditure.cow.est.flag==0,NA,mil.expenditure.wmeat),
      mil.expenditure.sipri = ifelse(mil.expenditure.cow.est.flag==0,NA,mil.expenditure.sipri)
    ) %>%
    dplyr::select(all_of(mil.expenditure)),
  use = "pairwise.complete.obs"
)

##################################################################

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
