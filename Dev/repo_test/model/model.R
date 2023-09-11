
# Here we run a few intermediate formatting steps done on SAG. 


library(icesTAF)
library(dplyr)
taf.library(icesFO)

mkdir("model")


#A. Trends by guild

sag_complete_frmt <- read.taf("data/sag_complete_frmt.csv")

sag_trends <- stock_trends(sag_complete_frmt)

#Check if this still applies in 2023

sag_trends <- trends[!(trends$StockKeyLabel == "whb.27.1-91214" & trends$Metric == "F_FMEAN" & trends$Year == 2022) &
                         !(trends$StockKeyLabel == "whb.27.1-91214" & trends$Metric == "F_FMSY" & trends$Year == 2022) &
                         !(trends$StockKeyLabel == "MEAN" & trends$Metric == "F_FMSY" & trends$Year == 2022) &
                         !(trends$StockKeyLabel == "whb.27.1-91214" & trends$Metric == "SSB_MSYBtrigger" & trends$Year == 2023) &
                         !(trends$StockKeyLabel == "MEAN" & trends$Metric == "SSB_MSYBtrigger" & trends$Year == 2023),]



sag_guild <- guild_trends(sag_complete_frmt)

write.taf(sag_trends, dir = "model")
write.taf(sag_guild, dir = "model")

#B.Trends and current catches, landings and discards

sag_catch_trends <- CLD_trends(sag_complete_frmt)
sag_catch_current <- stockstatus_CLD_current(sag_complete_frmt)

write.taf(sag_catch_trends, dir = "model")
write.taf(sag_catch_current, dir = "model")
