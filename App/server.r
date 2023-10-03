
############# Start server function ################

server <- function(input, output, session) {
  msg("server loop start:\n  ", getwd())

  # values of the query string and first visit flag
  query <- reactiveValues(query_from_table = FALSE, update_from_url = TRUE)
  selectedFile <- reactiveValues()

  old_tabset <- ""

  observe({
    # reacts to tab click, and url update
    # get stuff we need
    print("")
    print("")
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
        shinyjs::disable(selector = '.navbar-nav a[data-value="Assessment results"')
      } else {
        # no tab specified, update url and old tabset
        print("updating tab from new URL")
        old_tabset <<- query_string$tab
        updateNavbarPage(session, "tabset", selected = query_string$tab)
      }
    } else {
      if (input$tabset != old_tabset) {
        # then a tab has been clicked inside a session
        # page is correct, url out of date
        print("updating query - tab clicked")
        old_tabset <<- tabset
        updateQueryString(paste0("?tab=", tabset), mode = "push")
      } else if (query_string$tab != old_tabset) {
        # then a url has been written inside a session without a tab click
        # url is correct, page out of date
        print("updating tab - url modified")
        old_tabset <<- query_string$tab
        updateNavbarPage(session, "tabset", selected = query_string$tab)
      } else {
        print("nothing to do")
      }
    }

    # special case for assessment results tab
    if (query_string$tab == "Assessment results" && !is.null(query_string$repo) && !is.null(query_string$file)) {
      print("setting selected file to:")
      print(query_string$file)
      selectedFile$name <- paste0(query_string$repo, "/", query_string$file)
    }

    print("*** END observing query and input ***")
    print("")
    print("")
  })


  observeEvent(input$login, {
    # display a modal dialog with a header, textinput and action buttons
    showModal(modalDialog(
      tags$h2("Please enter your personal ICES token [icesConnect::ices_token()]"),
      textInput("token", "Token"),
      footer = tagList(
        actionButton("submit", "Submit"),
        modalButton("cancel")
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
    paste("Token:", submittedToken())
  })

  map_panel_server(input, output, session)


  repo_list <- reactive({
    req(input$selected_locations)
    stock_list_long <- getListStockAssessments()
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
          RepoUrl = paste0("<a href='", gitHubUrl, "' target='_blank'>Link")
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

    group_filter_temp() %>%
      select(
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
        "Year" = year,
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
    ),
    callback = JS(callback)
  )

  ## process radio button selection
  observeEvent(input$rdbtn, {
    shinyjs::enable(selector = '.navbar-nav a[data-value="Assessment results"')
    filtered_row <- group_filter_temp()[str_detect(group_filter_temp()$Select, regex(paste0("\\b", input$rdbtn, "\\b"))), ]
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
    paste("Repo name:", paste0(input$repo_year, "_", input$stock_code, "_", input$repo_type))
  })
  

  html_treeDF <- reactive({
    # reacts to url changes
    query_string <- getQueryString()
    CreateInteractiveTreeDF(repo = query_string$repo)
  })

  output$html_tree <- renderUI({
    HTML(CreateInteractiveTreeHTML(html_treeDF()))
  })

  observeEvent(input$clicked_text, {
    validate(
      need(input$clicked_text != "", "No file selected")
    )

    query_string <- getQueryString()

    fileName <- URLencode(html_treeDF()$pathString[as.numeric(input$clicked_text)])
    # remove repo from file path
    fileName <- substring(fileName, nchar(query_string$repo) + 2)

    updateQueryString(
      paste0("?tab=Assessment%20results&repo=", query_string$repo, "&file=", fileName),
      "push"
    )

    # fileNameTitle <- renderUI({
    #   print(fileName)
    #   print("here")
    # })
  })
  
  observeEvent(selectedFile$name, {
    id <- which(html_treeDF()$pathString == selectedFile$name)

    if (length(id) == 1) {
      # Download the file from the URL
      file_extension <- tolower(tools::file_ext(html_treeDF()$ServerUrlString[id]))
      # print(file_extension)
      fileURL <- html_treeDF()$ServerUrlString[id]

      if (file_extension == "csv") {
        # data <- read.table(fileURL, sep = ",", header = TRUE)

        output$file_viz <- renderTable({
          fileToDisplay <- read.table(fileURL, sep = ",", header = TRUE)
        })
      } else if (file_extension %in% c("png", "jpg")) {
        output$file_viz <- renderText({
          c('<img src="', fileURL, '" width="85%" height="85%">')
        })
      } else if (file_extension == "bib") {
        output$file_viz <- renderUI({
          fileToDisplay <- getURL(fileURL)


          aceEditor(
            outputId = "code_bib",
            value = fileToDisplay,
            mode = "yaml",
            theme = "clouds_midnight",
            fontSize = 14,
            height = "80vh",
            readOnly = TRUE
          )
        })
      } else if (file_extension %in% c("r", "R", "Rmd")) {
        output$file_viz <- renderUI({
          fileToDisplay <- getURL(fileURL)

          aceEditor(
            outputId = "code",
            value = fileToDisplay,
            mode = "r",
            theme = "chrome",
            fontSize = 14,
            height = "80vh",
            readOnly = TRUE
          )
        })
      } else if (file_extension == "md") {
        output$file_viz <- renderUI({
          fileToDisplay <- getURL(fileURL)
          HTML(markdown::mark(fileToDisplay))
        })
      } else if (file_extension == "html") {
        output$file_viz <- renderUI({
          fileToDisplay <- getURL(fileURL)
          HTML(fileToDisplay)
        })
      } else if (file_extension %in% c("txt", "dat")) {
        output$file_viz <- renderUI({
          fileToDisplay <- getURL(fileURL)
          aceEditor(
            outputId = "code",
            value = fileToDisplay,
            mode = "text",
            theme = "chrome",
            fontSize = 14,
            height = "80vh",
            readOnly = TRUE
          )
        })
      } else {
        # shinyjs::alert("Invalid file type or file format.")
      }
    } else {
      # render an image or text saying file doesnt exist
      shinyjs::alert("requested file not found.")
    }
  })

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
