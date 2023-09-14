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



########### test
# Function to apply shiny::icon() based on a condition
get_icon <- function(text) {
  if (nchar(text) == 0) {
    x <- paste(shiny::icon("folder-open"))
  } else if (text == "csv") {
    x <- paste(shiny::icon('file-csv'))
  } else if (text == "png") {
    x <- paste(shiny::icon('file-image'))
  } else if (text == "rds") {
    x <- paste(shiny::icon('r-project'))
  } else if (text == "txt") {
    x <- paste(shiny::icon('code'))
  } else if (text == "bib") {
    x <- paste(shiny::icon('book'))
  } else{
    x <- ""
  }
  return(x)
}


create_interactive_tree <- function(path, repo) {
  paths <- list.files(path,
    recursive = TRUE, full.names = TRUE,
    include.dirs = TRUE
  )

  # to clean off initial path -  will not need this in production
  paths <- gsub("D:/GitHub_2023/tafXplorer/App/Data/ices_cat_3_template", "", paths)

  tree <- as.Node(data.frame(pathString = paths))

  output <- ToDataFrameTree(tree, "pathString", "isLeaf", "level")
  output$filename <- basename(output$pathString)
  # output$filename <- paste0("`r shiny::icon('markdown')` ", output$filename)

  output$urlString <- paste0("https://ices-taf.shinyapps.io/tafexplorer?pathstring=", output$pathString, "&repo=", repo)

  # could be handy for file icons
  FileFormats <- tools::file_ext(output$filename)


  makeOne <- function(i) {
    paste0(
      paste(rep("  ", output$level[i] - 1), collapse = ""),
      "* ",
       sapply(FileFormats[i], get_icon), 
      " [", output$filename[i], "]",
      "(", output$urlString[i], ")"
    )
  }

  all <- paste(
    sapply(1:nrow(output), makeOne),
    collapse = "\n"
  )


  # cat(all)


  html <- markdown::mark(text = all)

  return(html)
}


path <- "D:/GitHub_2023/tafXplorer/App/Data/ices_cat_3_template"
repo <- "testRepo"





