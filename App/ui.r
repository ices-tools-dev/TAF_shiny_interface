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
                    width = 5,
                    leafletOutput("map_ecoregion"),
                    selectizeInput(
                        inputId = "selected_locations",
                        label = "ICES Ecoregions",
                        choices = sort(shape_eco$Ecoregion),
                        selected = "Greater North Sea",
                        multiple = FALSE,
                        width = "100%",
                        options = list(
                            placeholder = "Select Ecoregion(s)"
                        )
                    ), selectizeInput(
                        inputId = "selected_years",
                        label = "Assessment Year",
                        choices = Years$Year,
                        selected = 2023,
                        multiple = FALSE,
                        width = "100%",
                        options = list(
                            placeholder = "Select assessment year"
                        )
                    ),
                    selectizeGroupUI(
                        id = "my-filters",
                        params = list(
                            stockCode = list(inputId = "stockCode", title = "Stock code:"),
                            species = list(inputId = "species", title = "Common name:"),
                            expertGroup = list(inputId = "expertGroup", title = "Expert group:"),
                            dataCategory = list(inputId = "dataCategory", title = "Data category:")
                        ),
                        inline = FALSE
                    )
                ),
                mainPanel(
                    width = 7,
                    withSpinner(DTOutput("tbl"))
                )
            )
        ),
        tabPanel(
            "TAF progress"
        ),
        tabPanel(
            "Repo results",
        ),
        tabPanel(
            "Resources",
        )
    )
)
