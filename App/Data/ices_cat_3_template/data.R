### ------------------------------------------------------------------------ ###
### Preprocess data, write TAF data tables ####
### ------------------------------------------------------------------------ ###

## Before: bootstrap/data/FSP7e.csv
##         bootstrap/data/advice/advice_history.csv
##         bootstrap/data/InterCatch_length.csv
## After:  data/idx.csv
##         data/advice_history.csv
##         data/length_data.rds

library(TAF)
library(tidyr)
library(dplyr)

### create folder to store data
mkdir("data")

### ------------------------------------------------------------------------ ###
### Biomass index data ####
### ------------------------------------------------------------------------ ###
### 1 biomass index: FSP7e
### - biomass at age (standardised)

### load data from csv
idxB <- read.csv("boot/data/FSP7e.csv")

### only ages 2-8 are used -> sum up biomass
idxB <- cbind(idxB["year"],
              index = apply(idxB[, paste0("X", as.character(2:8))], 1, sum))

### save in data directory
write.taf(idxB, file = "data/idx.csv")
saveRDS(idxB, file = "data/idx.rds")


### ------------------------------------------------------------------------ ###
### catch and advice data ####
### ------------------------------------------------------------------------ ###

catch <- read.csv("boot/data/advice_history.csv")
names(catch)[1] <- "year"
write.taf(catch, file = "data/advice_history.csv")
saveRDS(catch, file = "data/advice_history.rds")

### ------------------------------------------------------------------------ ###
### length data ####
### ------------------------------------------------------------------------ ###
### raised data from InterCatch

### load data
lngth_full <- read.csv("boot/data/InterCatch_length.csv")

### summarise data
lngth <- lngth_full %>%
  filter(CatchCategory %in% c("Discards", "Landings")) %>%
  select(year = Year, catch_category = CatchCategory, length = AgeOrLength,
         numbers = CANUM) %>%
  group_by(year, catch_category, length) %>%
  summarise(numbers = sum(numbers))
write.taf(lngth, file = "data/length_data.csv")
saveRDS(lngth, file = "data/length_data.rds")
