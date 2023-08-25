getListStockAssessments <- function(){
    stocklist <- jsonlite::read_json("https://adminweb06.ices.dk/api/getListStockAssessments", simplifyVector = TRUE)
    return(stocklist)
}

getEGStatistics <- function() {
  EGStats <- jsonlite::read_json("https://adminweb06.ices.dk/api/getEGStatistics", simplifyVector = TRUE)
  return(EGStats)
}

getTAFStocksStatistics <- function() {
  TAFStats <- jsonlite::read_json("https://adminweb06.ices.dk/api/getTAFStocksStatistics", simplifyVector = TRUE)
  return(TAFStats)
}