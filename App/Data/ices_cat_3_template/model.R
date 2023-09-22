### ------------------------------------------------------------------------ ###
### Apply rfb rule ####
### ------------------------------------------------------------------------ ###

## Before: data/idx.csv
##         data/advice_history.csv
##         data/length_data.rds
## After:  model/advice.rds

library(TAF)
library(cat3advice)
library(dplyr)

mkdir("model")

### ------------------------------------------------------------------------ ###
### load data ####
### ------------------------------------------------------------------------ ###

### history of catch and advice
catch <- read.taf("data/advice_history.csv")
catch <- catch %>%
  select(year, advice = advice_catch_stock, discards = ICES_discards_stock,
         landings = ICES_landings_stock , catch = ICES_catch_stock)

### biomass index
idxB <- read.taf("data/idx.csv")

### catch length data
lngth <- read.taf("data/length_data.csv")

### ------------------------------------------------------------------------ ###
### reference catch ####
### ------------------------------------------------------------------------ ###
### use last catch advice value
A <- A(catch, units = "tonnes", basis = "advice", advice_metric = "catch")

### ------------------------------------------------------------------------ ###
### r - biomass index trend/ratio ####
### ------------------------------------------------------------------------ ###
### 2 over 3 ratio
r <- rfb_r(idxB, units = "kg/hr")

### ------------------------------------------------------------------------ ###
### b - biomass safeguard ####
### ------------------------------------------------------------------------ ###
### first application -> calculate Itrigger from time series
b <- rfb_b(idxB, units = "kg/hr")

### ------------------------------------------------------------------------ ###
### f - length-based indicator/fishing pressure proxy ####
### ------------------------------------------------------------------------ ###

### calculate length at first capture - pool last 5 years' data
lc <- Lc(lngth, pool = 2017:2021, units = "mm")

### mean annual catch length above Lc
lmean <- Lmean(lngth, Lc = lc, units = "mm")

### calculate new reference length LF=M (only in first year of application)
### Linf calculated by fitting von Bertalanffy model to age-length data
lref <- Lref(basis = "LF=M", Lc = lc, Linf = 585, units = "mm")

### length indicator
f <- rfb_f(Lmean = lmean, Lref = lref, units = "mm")

### ------------------------------------------------------------------------ ###
### multiplier ####
### ------------------------------------------------------------------------ ###
### generic multiplier based on life history (von Bertalanffy k)
### k value from fitting von Bertalanffy model to age-length data

m <- rfb_m(k = 0.11)

### ------------------------------------------------------------------------ ###
### apply rfb rule - combine elements ####
### ------------------------------------------------------------------------ ###
### includes consideration of stability clause

advice <- rfb(A = A, r = r, f = f, b = b, m = m,
              cap = "conditional", cap_upper = 20, cap_lower = -30,
              frequency = "biennial",
              discard_rate = 26.71693)

### ------------------------------------------------------------------------ ###
### save output ####
### ------------------------------------------------------------------------ ###
saveRDS(advice, file = "model/advice.rds")
