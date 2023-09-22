# Initial formatting of the data

library(icesTAF)
library(icesFO)
library(dplyr)

mkdir("data")

# set working directory
setwd("D:/Profile/Documents/GitHub/2021_CS_FisheriesOverview")

# load species list
species_list <- read.taf("bootstrap/initial/data/FAO_ASFIS_species/species_list.csv")
sid <- read.taf("bootstrap/initial/data/ICES_StockInformation/sid.csv")







# 1: ICES official catch statistics

hist <- read.taf("bootstrap/initial/data/ICES_nominal_catches/ICES_historical_catches.csv")
official <- read.taf("bootstrap/initial/data/ICES_nominal_catches/ICES_2006_2019_catches.csv")
prelim <- read.taf("bootstrap/initial/data/ICES_nominal_catches/ICES_preliminary_catches.csv")

catches_frmt <- format_catches_noecoregion(hist, official, species_list, sid)


write.taf(catches_frmt, dir = "data", quote = TRUE)





# 2: SAG

#glitches common to all ecoregions here: 

#DGS has a custom ref point for F 
sag_complete$FMSY[which(sag_complete$FishStock == "dgs.27.nea")] <- 0.0429543
sag_complete$MSYBtrigger[which(sag_complete$FishStock == "dgs.27.nea")] <- 336796
sag_complete$StockSize[which(sag_complete$FishStock == "dgs.27.nea")] <- sag_complete$TBiomass[which(sag_complete$FishStock == "dgs.27.nea")]


# 2022 update: this still applies:
sag_complete$FMSY[which(sag_complete$FishStock == "pok.27.1-2")] <- 0.32
sag_complete$MSYBtrigger[which(sag_complete$FishStock == "pok.27.1-2")] <- 220000

sag_complete$FMSY[which(sag_complete$FishStock == "cod.27.1-2.coastN")] <- 0.176
sag_complete$MSYBtrigger[which(sag_complete$FishStock == "cod.27.1-2.coastN")] <- 67743

# sag_complete$FMSY[which(sag_complete$FishStock == "cod.27.1-2.coastN")] <- 0.32
sag_complete$MSYBtrigger[which(sag_complete$FishStock == "reg.27.1-2")] <- 68600 #PA
sag_complete$MSYBtrigger[which(sag_complete$FishStock == "cap.27.1-2")] <- 200000 #PA


sag_complete$MSYBtrigger[which(sag_complete$StockKeyLabel == "ple.27.7e")] <- "0.39"
sag_complete$MSYBtrigger[which(sag_complete$StockKeyLabel == "spr.27.7de")] <- "11527.9"
sag_complete$FMSY[which(sag_complete$StockKeyLabel == "spr.27.7de")] <- "0.0857"


#CHECK if this as been solved
# Have to substitute time-series of SSB of lez.27.6b with the custom column 2,
# which is the biomass index used in the advice.
# icesSAG::findAssessmentKey("lez.27.6b", year = 2021)
# 
# lezdat <- icesSAG::getCustomColumns(16879)
# lezdat <- lezdat %>% filter(customColumnId == 2)
# sag_complete <- sag_complete %>% mutate(SSB = replace(SSB,StockKeyLabel == "lez.27.6b", lezdat$customValue))


sag_complete_frmt <- format_sag(sag_complete, sid)


write.taf(sag_complete_frmt, dir = "data", quote = TRUE)





# 3: STECF landings and effort

landings1 <- read.taf("bootstrap/initial/data/Landings_2014.csv", check.names = TRUE)
landings2 <- read.taf("bootstrap/initial/data/Landings_2015.csv", check.names = TRUE)
landings3 <- read.taf("bootstrap/initial/data/Landings_2016.csv", check.names = TRUE)
landings4 <- read.taf("bootstrap/initial/data/Landings_2017.csv", check.names = TRUE)
landings5 <- read.taf("bootstrap/initial/data/Landings_2018.csv", check.names = TRUE)
landings6 <- read.taf("bootstrap/initial/data/Landings_2019.csv", check.names = TRUE)
landings7 <- read.taf("bootstrap/initial/data/Landings_2020.csv", check.names = TRUE)
STECF_landings <- rbind(landings1, landings2, landings3, landings4, landings5, landings6, landings7)
STECF_effort <- read.taf("bootstrap/initial/data/FDI effort by country.csv", check.names = TRUE)
# names(landings)
# landings$Sub.region <- tolower(landings$Sub.region)

# landings_CS <- dplyr::filter(landings, grepl("27.6.a|27.7.b|27.7.j|27.7.g|27.7.a|
#                                           27.7.h|27.7.f|27.6.b|27.7.c|27.7.k", Sub.region))

# need to group gears, Sarah help.
unique(STECF_landings$Gear.Type)

STECF_landings <- dplyr::mutate(STECF_landings, gear_class = case_when(
  grepl("TBB", Gear.Type) ~ "Beam trawl",
  grepl("DRB|DRH|HMD", Gear.Type) ~ "Dredge",
  grepl("GNS|GND|GTN|LHP|LLS|FPN|GTR|FYK|LLD|SDN|LTL|LNB", Gear.Type) ~ "Static/Gill net/LL",
  grepl("OTT|OTB|PTB|SSC|SB|SPR|SV", Gear.Type) ~ "Otter trawl/seine",
  grepl("PTM|OTM|PS", Gear.Type) ~ "Pelagic trawl/seine",
  grepl("FPO", Gear.Type) ~ "Pots",
  grepl("NK|NO|LHM", Gear.Type) ~ "other",
  is.na(Gear.Type) ~ "other",
  TRUE ~ "other"
)
)


STECF_effort <- dplyr::mutate(STECF_effort, gear_class = case_when(
  grepl("TBB", Gear.Type) ~ "Beam trawl",
  grepl("DRB|DRH|HMD", Gear.Type) ~ "Dredge",
  grepl("GNS|GND|GTN|LHP|LLS|FPN|GTR|FYK|LLD|SDN|LTL|LNB", Gear.Type) ~ "Static/Gill net/LL",
  grepl("OTT|OTB|PTB|SSC|SB|SPR|SV", Gear.Type) ~ "Otter trawl/seine",
  grepl("PTM|OTM|PS", Gear.Type) ~ "Pelagic trawl/seine",
  grepl("FPO", Gear.Type) ~ "Pots",
  grepl("NK|NO|LHM", Gear.Type) ~ "other",
  is.na(Gear.Type) ~ "other",
  TRUE ~ "other"
)
)

unique(STECF_landings[c("Gear.Type", "gear_class")])
unique(STECF_effort[c("Gear.Type", "gear_class")])

write.taf(STECF_landings, dir = "data", quote = TRUE)
write.taf(STECF_effort, dir = "data", quote = TRUE)

