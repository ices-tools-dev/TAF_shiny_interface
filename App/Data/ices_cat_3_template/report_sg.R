### ------------------------------------------------------------------------ ###
### create ICES standard graphs for advice sheet ####
### ------------------------------------------------------------------------ ###

## Before: report/tables_sag.csv
## After:

### load packages
library(TAF)
library(icesSAG)

### ------------------------------------------------------------------------ ###
### preparation ####
### ------------------------------------------------------------------------ ###

### ICES standard graphs
### create token for authentication
### go to https://standardgraphs.ices.dk/manage/index.aspx
### login
### click on create token or go directly to
### https://standardgraphs.ices.dk/manage/CreateToken.aspx
### create new token, save in file
# file.edit("~/.Renviron")
### in the format
### SG_PAT=some_token_......
### save and restart R

### load token
Sys.getenv("SG_PAT")
options(icesSAG.use_token = TRUE)

### assessment year
ass_yr <- 2021

### check assessments keys
key <- findAssessmentKey("ple.27.7e", year = ass_yr)
key_last <- findAssessmentKey("ple.27.7e", year = ass_yr - 1)

### last year's graphs
plot(getSAGGraphs(key_last))

### list of possible elements:
### https://datsu.ices.dk/web/selRep.aspx?Dataset=126
### allowed units:
### https://vocab.ices.dk/?ref=155

### set up stock info
stk_info <- stockInfo(
  StockCode = "ple.27.7e",
  AssessmentYear = ass_yr,
  ContactPerson = "your.email@email.gov",
  Purpose = "Advice"
)

### add some more data manually
stk_info$MSYBtrigger <- 2443
stk_info$FMSY <- 0.238
stk_info$Blim <- 1745
stk_info$Flim <- 0.88
stk_info$Bpa <- 2443
stk_info$Fpa <- 0.69 ### updated in 2021, Fp.05 is new basis
stk_info$Fage <- "3-6"
stk_info$RecruitmentAge <- 2
stk_info$RecruitmentDescription <- "Recruitment"
stk_info$RecruitmentUnits <- "NE3" ### NE3 stands for thousands
# stk_info$RecruitmentUnits <- "Relative Recruitment"
stk_info$CatchesLandingsUnits <- "t" ### t for tonnes
stk_info$StockSizeUnits <- "t" ### t for tonnes
stk_info$StockSizeDescription <- "SSB"
# stk_info$StockSizeDescription <- "Stock Size: Relative"
stk_info$FishingPressureDescription <- "F"
# stk_info$FishingPressureDescription <- "Fishing pressure: Relative"
stk_info$Purpose <- "Advice"
stk_info$ModelType <- "A"
stk_info$ModelName <- "XSA"

### load data from assessment/forecast
sag <- read.csv(file = "report/tables_sag.csv", as.is = TRUE)

### set up data
# https://datsu.ices.dk/web/selRep.aspx?Dataset=126  # Record: AF - Fish Data
stk_data <- stockFishdata(
  Year = sag$Year,
  Recruitment = sag$Recruitment,
  TBiomass = sag$TBiomass,
  StockSize = sag$StockSize,
  Landings = sag$Landings,
  Discards = sag$Discards,
  FishingPressure = sag$FishingPressure
)

### create xml file
stkxml <- createSAGxml(info, fishdata)
cat(stkxml, file = "SAG_upload.xml")
