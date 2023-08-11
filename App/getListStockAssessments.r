getListStockAssessments <- function(){
    # stocklist <- jsonlite::read_json("https://adminweb06.ices.dk/api/getListStockAssessments", simplifyVector = TRUE)
    stocklist <- jsonlite::read_json("./www/test_data.json", simplifyVector = TRUE)
    return(stocklist)
}

callback <- c(
  "$('input[name=rdbtn]').on('click', function(){",
  "  var value = $('input[name=rdbtn]:checked').val();",
  "  Shiny.setInputValue('rdbtn', value);",
  "});"
)