############# Libraries ############
library(data.table)
library(dplyr)
library(dygraphs)
library(DT)
library(fisheryO)
library(htmltools)
library(htmlwidgets)
library(ggplot2)
library(ggradar)
library(ggtext)
library(glue)
library(gsubfn)
library(icesFO)
library(icesSAG)
library(icesTAF)
library(icesVocab)
library(leaflet)
library(plotly)
library(reshape2)
library(rintrojs)
library(RColorBrewer)
library(RCurl)
library(rvest)
library(scales)
library(sf)
library(shinyalert)
library(shinyBS)
library(shinycssloaders)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(stringr)
library(tidyr)
library(tidyverse)
library(tm)
library(widgetframe)
library(icesASD)
library(mixfishtools)
library(shiny)
library(shinyWidgets)
library(mapplots)
library(maps)
library(mapdata)
library(pals)
library(leaflet)
library(sf)
library(shinyjs)
library(reshape2)







########## Load utilities ############
source("utilities_load_ecoregion_shp.r")
source("utilities_ecoregion_mapping.r")
source("getListStockAssessments.r")



# Load data





title_html <- tags$a(
    href = "https://www.ices.dk/data/assessment-tools/Pages/transparent-assessment-framework.aspx",
    tags$img(
        src = "taf_bot.png",
        style = "margin-top: -10px; padding-right:10px;padding-bottom:10px",
        height = "50px"
    )
)
tagList(
    useShinyjs(),
    introjsUI(),
    tags$script(src = "https://kit.fontawesome.com/ac71e9cf8e.js"),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
    navbarPage(
        position = "static-top",
        collapsible = TRUE,
        # tab title
        windowTitle = "tafXplorer",
        id = "tabset",
        fluid = TRUE,
        # navbar title
        title = title_html,
        tabPanel(
            "Stock assessment selection",
            actionBttn(inputId = "login", label = "LOGIN", style = "simple", size = "sm", color = "warning"),
            verbatimTextOutput(outputId = "text"),
            br(),
            sidebarLayout(
                sidebarPanel(
                    width = 3,
                    leafletOutput("map_ecoregion"),
                    br(),
                    selectizeInput(
                        inputId = "selected_locations",
                        label = "ICES Ecoregions",
                        choices = sort(shape_eco$Ecoregion),
                        selected = "Greater North Sea",
                        multiple = TRUE,
                        width = "100%",
                        options = list(
                            placeholder = "Select Ecoregion(s)"
                        )
                    ),
                    selectizeGroupUI(
                        id = "my-filters",
                        params = list(
                            year = list(inputId = "year", title = "Assessment year:"),
                            stockCode = list(inputId = "stockCode", title = "Stock code:"),
                            species = list(inputId = "species", title = "Common name:"),
                            expertGroup = list(inputId = "expertGroup", title = "Expert group:"),
                            dataCategory = list(inputId = "dataCategory", title = "Data category:")
                        ),
                        inline = FALSE
                    )
                ),
                mainPanel(
                    width = 9,
                    withSpinner(DTOutput("tbl"))
                )
            )
        ),
        tabPanel(
            "Assessment results",
        ),
        tabPanel(
            "TAF overview",
            plotlyOutput("plot1", height = "100%", width = "50%"),
            radioButtons(
                inputId = "category",
                label = "Data category to show:",
                choices = c(
                    "Category 1" = "Cat 1",
                    "Category 2-6" = "Cat 2-6"
                ),
                inline = TRUE,
                selected = "Cat 1",
                width = "100%"
            )
        ),
        tabPanel(
            "Resources",
        )
    )
)
