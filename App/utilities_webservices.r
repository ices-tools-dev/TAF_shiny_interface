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
  } else if (text == "rds" | text == "R" | text == "r") {
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


CreateInteractiveTreeDF <- function(repo) {
  # paths <- list.files(jsonlite::read_json("https://adminweb06.ices.dk/api/dir/ices_cat_3_template", simplifyVector = TRUE),
  #   recursive = TRUE, full.names = TRUE,
  #   include.dirs = TRUE
  # )

  paths <-jsonlite::read_json(paste0("https://adminweb06.ices.dk/api/dir/",repo), simplifyVector = TRUE)
  # print(paths)
  # to clean off initial path -  will not need this in production
  paths <- paths[!(grepl("/[.]git", paths) | grepl("(bootstrap|boot)/library", paths))]

  tree <- as.Node(data.frame(pathString = paths))

  output <- ToDataFrameTree(tree, "pathString", "isLeaf", "level")
  output$filename <- basename(output$pathString)
  # output$filename <- paste0("`r shiny::icon('markdown')` ", output$filename)

  output$urlString <- paste0("https://ices-taf.shinyapps.io/tafxplorer/?Assessmentresults?pathstring=", output$pathString, "&repo=", repo)
  output$ServerUrlString <- paste0("https://adminweb06.ices.dk/api/blob/", output$pathString)
  # could be handy for file icons
  output$FileFormats <- tools::file_ext(output$filename)

  return(output)
}

CreateInteractiveTreeHTML <- function(output){

  makeOne <- function(i) {
    paste0(
      paste(rep("  ", output$level[i] - 1), collapse = ""),
      "* ",
       sapply( output$FileFormats[i], get_icon),
       " ",
       tags$a(href = "#", id = i, output$filename[i])

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


# path <- "./Data/ices_cat_3_template"
# repo <- "ices_cat_3_template"
# CreateInteractiveTreeDF(repo)
# CreateInteractiveTreeHTML(CreateInteractiveTreeDF(path, repo))

# HTML(create_interactive_tree(path, repo))
