
# msg <- function(...) {
#   emph <- "\n****************\n"
#   cat(emph, ..., emph)
# }


# # required if using most recent version of sf
# sf::sf_use_s2(FALSE)

# options(icesSAG.use_token = FALSE)





############# Start server function ################

server <- function(input, output, session) {
  msg("server loop start:\n  ", getwd())

  # values of the query string and first visit flag
  query <- reactiveValues(query_from_table = FALSE, update_from_url = TRUE)

  old_tabset <- ""

  observe({
    # reacts to tab click, and url update
    # get stuff we need
    print(""); print("")
    print("*** observing url and input ***")
      query_string <- getQueryString()
      names(query_string) <- tolower(names(query_string))

      tabset <- input$tabset

      print("old tab:")
      print(old_tabset)
      print("current url:")
      print(str(query_string))
      print("current tab:")
      print(input$tabset)

      if (old_tabset == "") {
        # then server loop has restarted: i.e. a url has been entered

        if (is.null(query_string$tab)) {
          # no tab specified, update url and old tabset
          print("updating url from empty")
          updateQueryString(paste0("?tab=", tabset), mode = "push")
          old_tabset <<- tabset # always will be 1st tab
        } else
        {
          # no tab specified, update url and old tabset
          print("updating tab from new URL")
          old_tabset <<- query_string$tab
          updateNavbarPage(session, "tabset", selected = query_string$tab)
        }
      } else
      {
        if (input$tabset != old_tabset) {
          # then a tab has been clicked inside a session
          # page is correct, url out of date
          print("updating query - tab clicked")
          old_tabset <<- tabset
          updateQueryString(paste0("?tab=", tabset), mode = "push")
        } else
        if (query_string$tab != old_tabset) {
          # then a url has been written inside a session without a tab click
          # url is correct, page out of date
          print("updating tab - url modified")
          old_tabset <<- query_string$tab
          updateNavbarPage(session, "tabset", selected = query_string$tab)
        }
        else {
          print("nothing to do")
        }
      }

      print("*** END observing query and input ***")
      print("")
      print("")
  })

 # observe({
    # read url string


    # if (!is.null(query$assessmentkey) && !query$query_from_table) {
    #   info <- getFishStockReferencePoints(query$assessmentkey)[[1]]

    #   query$stockkeylabel <- info$StockKeyLabel
    #   query$year <- info$AssessmentYear ####

    #   msg("stock selected from url:", query$stockkeylabel)
    #   msg("year of SAG/SID selected from url:", query$year) #####

    #   updateNavbarPage(session, "tabset", selected = "Development over time")
    #   shinyjs::enable(selector = '.navbar-nav a[data-value="Development over time"')
    #   shinyjs::enable(selector = '.navbar-nav a[data-value="Quality of assessment"')
    #   shinyjs::enable(selector = '.navbar-nav a[data-value="Catch scenarios"')

    # }
  #})



  observeEvent(input$login, {
    # display a modal dialog with a header, textinput and action buttons
    showModal(modalDialog(
      tags$h2('Please enter your personal ICES token [icesConnect::ices_token()]'),
      textInput('token', 'Token'),
      footer = tagList(
        actionButton('submit', 'Submit'),
        modalButton('cancel')
      )
    ))
  })

  submittedToken <- reactiveVal("paste your token here")

  # only store the information if the user clicks submit
  observeEvent(input$submit, {
    removeModal()
    submittedToken(input$token)
  })

  output$text <- renderPrint({
    paste('Token:', submittedToken())
  })

  map_panel_server(input, output, session)


  repo_list <- reactive({
    req(input$selected_locations)
    stock_list_long <- getListStockAssessments()
    #print(str(stock_list_long))
    # stock_list_long[stock_list_long$EcoRegion == "Iceland Sea Ecoregion", "EcoRegion"] <- "Icelandic Waters Ecoregion"
    # stock_list_long <- stock_list_long %>% drop_na(AssessmentKey)
    stock_list_long <- purrr::map_dfr(
      .x = input$selected_locations,
      .f = function(.x) stock_list_long %>% dplyr::filter(str_detect(ecoregion, .x))
    )

    if (nrow(stock_list_long) != 0) {
    stock_list_long %>%
      dplyr::arrange(stockCode) %>%
      dplyr::mutate(
        # EcoRegion = removeWords(EcoRegion, "Ecoregion"),
        Select = sprintf('<input type="radio" name="rdbtn" value="rdbtn_%s"/>', 1:nrow(.)),
        RepoUrl = paste0("<a href='", gitHubUrl,"' target='_blank'>Link")
        # stock_description = purrr::map_chr(StockKeyLabel, .f = ~ access_sag_data_local(.x, input$selected_years)$StockDescription[1]),
        # stock_location = parse_location_from_stock_description(stock_description)
      )
  }
  })

  group_filter_temp <- callModule(

    module = selectizeGroupServer,
    id = "my-filters",
    data = repo_list,
    vars = c(
      "year", "stockCode", "species", "expertGroup", "dataCategory"
    ),
    inline = FALSE
  )

  group_filter <- reactive({
    validate(
      need(!nrow(repo_list()) == 0, "No published stocks in the selected ecoregion and year")
    )

   group_filter_temp() %>% select(
      "Select",
      "stockCode",
      "year",
      # "EcoRegion",
      # "icon",
      "species",
      "expertGroup",
      "dataCategory",
      "RepoUrl"
    ) %>%
      rename(
        "Select" = Select,
        "Stock code" = stockCode,
        "Year"= year,
        # "Ecoregion" = EcoRegion,
        # " " = icon,
        "Common name" = species,
        "Expert group" = expertGroup,
        "Data category" = dataCategory,
        "Repo Url" = RepoUrl
      )
  })

  output$tbl <- DT::renderDT(
    group_filter(),
    escape = FALSE,
    selection = "none",
    server = FALSE,
    caption = HTML("<b><font size= 6> Stock assessment selection</b></font></br><font size= 5> To select a stock, click on the corresponding button in the 'Select' column. </font>"),
    options = list(
      order = list(2, "asc"),
      dom = "Bfrtip",
      pageLength = 300
      # columnDefs = list(
      #   list(visible = FALSE, targets = c(0, 3)),
      #   list(className = "dt-center", targets = c(1, 4))
      # )
    ),
    callback = JS(callback)
  )

    ## process radio button selection
  observeEvent(input$rdbtn, {
    # shinyjs::enable(selector = '.navbar-nav a[data-value="Development over time"')
    # shinyjs::enable(selector = '.navbar-nav a[data-value="Quality of assessment"')
    # shinyjs::enable(selector = '.navbar-nav a[data-value="Catch scenarios"')

    filtered_row <- group_filter_temp()[str_detect(group_filter_temp()$Select, regex(paste0("\\b", input$rdbtn,"\\b"))), ]

    updateQueryString(paste0("?tab=Assessment%20results&repo=", basename(filtered_row$gitHubUrl)), mode = "push") ####
  })

  observeEvent(input$repo_year, {
    updateSelectInput(
      session = getDefaultReactiveDomain(),
      inputId = "stock_code",
      label = "Stock code",
      choices = c(icesFO::load_sid(input$repo_year) %>% arrange(StockKeyLabel) %>% dplyr::select(., StockKeyLabel) %>% pull(.)),
      selected = NULL
    )
  })
  output$repo_string <- renderPrint({
    paste('Repo name:', paste0(input$repo_year, "_", input$stock_code, "_", input$repo_type))
  })

  html_treeDF <- reactive({
    #print("###########")
    #print(query$repo)
    #print("###########")
    # HTML(create_interactive_tree("./Data/ices_cat_3_template", "testRepo"))
    query_string <- getQueryString()
    CreateInteractiveTreeDF(repo = query_string$repo)
  })

  output$html_tree <- renderUI({
      # HTML(create_interactive_tree("./Data/ices_cat_3_template", "testRepo"))
      HTML(CreateInteractiveTreeHTML(html_treeDF()))
    })

  # output$clicked_text <- eventReactive(input$clicked_text, {
  #  print(input$clicked_text)
  # })
  #########################################################################
  observeEvent(input$clicked_text, {
    validate(
      need(input$clicked_text != "", "No file selected")
    )
    # Check if a valid URL is provided
    # if (!grepl("^https?://", input$urlInput)) {
    #   shinyjs::alert("Please enter a valid URL starting with http:// or https://.")
    #   return()
    # }
  })

  observe({

    # Download the file from the URL
    file_extension <- tolower(tools::file_ext(html_treeDF()$ServerUrlString[as.numeric(input$clicked_text)]))
    # print(file_extension)
    fileURL <- html_treeDF()$ServerUrlString[as.numeric(input$clicked_text)]

    fileName <- URLencode(html_treeDF()$pathString[as.numeric(input$clicked_text)])

    query_string <- getQueryString()
    updateQueryString(
      paste0("?tab=Assessment%20results&repo=", query_string$repo, "&file=", fileName),
      "push"
    )

    if (file_extension == "csv") {
      # data <- read.table(fileURL, sep = ",", header = TRUE)

      output$file_viz <- renderTable({
        fileToDisplay <- read.table(fileURL, sep = ",", header = TRUE)
      })
      # output$downloadCSV <- downloadHandler(
      #   filename = function() {
      #     paste("downloaded_data.csv")
      #   },
      #   content = function(file) {
      #     write.csv(data, file)
      #   }
      # )
    } else if (file_extension == "png") {

      output$file_viz <- renderText({
        c('<img src="', fileURL,'" width="100%">')
        })
      # output$fileViewer <- renderImage({
      #   list(src = input$urlInput, contentType = "image/png")
      # }, deleteFile = FALSE)
    } else if (file_extension == "bib") {

      output$file_viz <- renderUI({
        fileToDisplay <- getURL(fileURL)
        # html_text <- gsub("\r\n", "</br>", fileToDisplay)
        # HTML(html_text)

        aceEditor(
        outputId = "code_bib",
        value = fileToDisplay,
        mode = "yaml",
        theme = "clouds_midnight",
        fontSize = 14,
        height = "1000px",
        readOnly = TRUE
      )
      })

    } else if (file_extension %in% c("r", "R", "Rmd")) {

      output$file_viz <- renderUI({
        fileToDisplay <- getURL(fileURL)
        # print(fileToDisplay)
        # html_text <- gsub("\r\n", "</br>", fileToDisplay)
        # HTML(html_text)
        # HTML(paste("<pre><code>", html_text, "</code></pre>"))


        aceEditor(
        outputId = "code",
        value = fileToDisplay,
        mode = "r",
        theme = "chrome",
        fontSize = 14,
        height = "1000px",
        readOnly = TRUE
      )
      })

    } else if (file_extension == "md") {

      output$file_viz <- renderUI({
        fileToDisplay <- getURL(fileURL)
        HTML(markdown::mark(fileToDisplay))
        # print(fileToDisplay)
        # html_text <- gsub("\r\n", "</br>", fileToDisplay)
        # HTML(html_text)
      })

    } else if (file_extension == "html") {

      output$file_viz <- renderUI({
        fileToDisplay <- getURL(fileURL)
        HTML(fileToDisplay)
        # print(fileToDisplay)
        # html_text <- gsub("\r\n", "</br>", fileToDisplay)
        # HTML(html_text)
      })

    } else if (file_extension == "txt") {

      output$file_viz <- renderUI({
        fileToDisplay <- getURL(fileURL)
        aceEditor(
        outputId = "code",
        value = fileToDisplay,
        mode = "text",
        theme = "chrome",
        fontSize = 14,
        height = "1000px",
        readOnly = TRUE
      )
      })

    } else {
      #shinyjs::alert("Invalid file type or file format.")
    }

  })
  #########################################################################
  # output$file_viz <- renderTable({ ### this now works only for csv files
  #   validate(
  #     need(input$clicked_text != "", "No file selected")
  #   )
  #   fileURL <- html_treeDF()$ServerUrlString[as.numeric(input$clicked_text)]
  #   # fileToDisplay <- getURL(fileURL)
  #   if (html_treeDF()$FileFormats[as.numeric(input$clicked_text)] == "csv") {
  #     fileToDisplay <- read.table(fileURL, sep = ",", header = TRUE)
  #   } else {
  #     print(getURL(fileURL))
  #   }
  # })




################################################# TAF Overview
  TAFStatistics <- reactive({
    TAFStats <- getTAFStocksStatistics()
  })

   output$plot1 <- renderPlotly({
    req(TAFStatistics())

    TAFStatsPlot(TAFStatistics(), input$category, input$percentages)

   })

  EgStatistics <- reactive({
    EgStats <- getEGStatistics()
  })

  output$plot2 <- renderPlotly({
    req(EgStatistics())

    EGStatsPlot(EgStatistics())

   })

}
