
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

  stock_list_reactive <- reactive({
    req(input$selected_locations)
    getListStockAssessments()
  })

  output$tbl <- DT::renderDT(
    stock_list_reactive()
  )
}

