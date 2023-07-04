
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


  map_panel_server(input, output, session)

  stock_list_reactive <- reactive({
    req(input$selected_locations)
    getListStockAssessments()
  })

  output$tbl <- DT::renderDT(
    stock_list_reactive()
  )
}

