
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
        # "Ecoregion" = EcoRegion,
        # " " = icon,
        "Common name" = species,
        "Expert group" = expertGroup,
        "Data category" = dataCategory,
        "Repo Url" = RepoUrl
      )
  })

  # stock_list_reactive <- reactive({
  #   req(input$selected_locations)
  #   getListStockAssessments()
  # })
  
  output$tbl <- DT::renderDT(
    # stock_list_reactive()
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


  TAFStatistics <- reactive({
    TAFStats <- getTAFStocksStatistics()
  })

   output$plot1 <- renderPlotly({
    req(TAFStatistics())
    fig1 <- plot_ly(
      data = TAFStatistics() %>% filter(categories == input$category),
      x = ~year, 
      y = ~stocks,
      type = 'bar',
      name = "Assessments not in TAF")
    fig1 <- fig1 %>% add_trace(
      y = ~taF_Stocks,
      name = "Assessments in TAF")
    fig1 <- fig1 %>% layout(yaxis = list(title = 'N. of stocks'), barmode = 'stack')
    fig1

   })

}

