library(data.tree)


path <- "D:/TAF/dashboard/"

repo <- "testRepo"

paths <- list.files(path, recursive = TRUE, full.names = TRUE,
            include.dirs = TRUE)

# to clean off initial path -  will not need this in production
paths <- gsub("D:/TAF/dashboard/", "", paths)

tree <- as.Node(data.frame(pathString = paths))

output <- ToDataFrameTree(tree, "pathString", "isLeaf", "level")
output$filename <- basename(output$pathString)

output$urlString <- paste0("https://ices-taf.shinyapps.io/tafexplorer?pathstring=", output$pathString, "&repo=",repo)

# could be handy for file icons
tools::file_ext(output$filename)


makeOne <- function(i) {
  paste0(
    paste(rep("  ", output$level[i] - 1), collapse = ""),
    "* ",
    "[", output$filename[i], "]",
    "(",  output$urlString[i], ")"
  )
}

all <- paste(
  sapply(1:nrow(output), makeOne),
  collapse = "\n"
)


cat(all)


html <- markdown::mark(text = all)

# could potentially add icons to bullets... by modifying html string



# to check

tempfilename <- tempfile(fileext = ".html")
markdown::mark(text = all, , output = tempfilename)

browseURL(tempfilename)



output
