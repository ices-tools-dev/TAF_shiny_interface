shinyApp(
      ui = basicPage(
        actionButton("show", "Show modal dialog"),
        verbatimTextOutput("dataInfo")
      ),

      server = function(input, output) {
        # reactiveValues object for storing current data set.
        vals <- reactiveValues(data = NULL)

        # Return the UI for a modal dialog with data selection input. If 'failed' is
        # TRUE, then display a message that the previous value was invalid.
        dataModal <- function(failed = FALSE) {
          modalDialog(
            textInput("dataset", "Choose data set",
              placeholder = 'Try "mtcars" or "abc"'
            ),
            span('(Try the name of a valid data object like "mtcars", ',
                 'then a name of a non-existent object like "abc")'),
            if (failed)
              div(tags$b("Invalid name of data object", style = "color: red;")),

            footer = tagList(
              modalButton("Cancel"),
              actionButton("ok", "OK")
            )
          )
        }

        # Show modal when button is clicked.
        observeEvent(input$show, {
          showModal(dataModal())
        })

        # When OK button is pressed, attempt to load the data set. If successful,
        # remove the modal. If not show another modal, but this time with a failure
        # message.
        observeEvent(input$ok, {
          # Check that data object exists and is data frame.
          if (!is.null(input$dataset) && nzchar(input$dataset) &&
              exists(input$dataset) && is.data.frame(get(input$dataset))) {
            vals$data <- get(input$dataset)
            removeModal()
          } else {
            showModal(dataModal(failed = TRUE))
          }
        })

        # Display information about selected data
        output$dataInfo <- renderPrint({
          if (is.null(vals$data))
            "No data selected"
          else
            summary(vals$data)
        })
      }
    )

#########################################
library(shiny)

shinyApp(
  ui = basicPage(
    actionButton("show", "Show modal dialog"),
    verbatimTextOutput("print")
  ),
  
  server = function(input, output) {
    
    # Create object to store reactive values
    vals <- reactiveValues(
      txt = NULL,
      error_msg = NULL,
      print = FALSE
    )
    
    # Create modal
    popupModal <- function() {
      modalDialog(
        textInput("txt", "Write something"),
        
        textOutput("skip_error_msg"),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton("ok", "OK")
        )
      )
    }
    
    # Show modal when button is clicked
    observeEvent(input$show, {
      vals$error_msg <- NULL
      showModal(popupModal())
    })
    
    # Validate submission
    observeEvent(input$ok, {
      vals$txt <- input$txt
      
      if (!is.null(vals$txt) && nzchar(vals$txt)) {
        removeModal()
        vals$print <- TRUE
      } else {
        vals$error_msg <- "You did not input anything."
      }
    })
    
    # Output error message
    output$skip_error_msg <- renderText({
      vals$error_msg
    })
    
    # Output inputted text
    output$print <- renderPrint({
      if (vals$print) {
        vals$txt
      } else {
        NULL
      }
    })
  }
)


#######################################
library(shiny)
library(shinyWidgets)

ui <- fluidPage(
  br(),
  actionBttn(inputId = "reset", label = "RESET", style="simple", size="sm", color = "warning"),
  verbatimTextOutput(outputId = "text")
)

server <- function(input, output, session) {
  observeEvent(input$reset, {
    # display a modal dialog with a header, textinput and action buttons
    showModal(modalDialog(
      tags$h2('Please enter your personal information'),
      textInput('name', 'Name'),
      footer = tagList(
        actionButton('submit', 'Submit'),
        modalButton('cancel')
      )
    ))
  })
  
  submittedName <- reactiveVal("placeholder")
  
  # only store the information if the user clicks submit
  observeEvent(input$submit, {
    removeModal()
    submittedName(input$name)
  })
  
  output$text <- renderPrint({
    paste('Name:', submittedName())
  })
}

shinyApp(ui, server)
########################## progress bar
library(glue)
library(DT)
library(htmltools)
myiris <- iris |>
  group_by(Species) |>
  slice_head(n = 3) |>
  mutate(
    frac_of_range_temp =
      100 * (Petal.Length - min(Petal.Length)) /
        (max(Petal.Length) - min(Petal.Length))
  ) |>
  rowwise() |>
  mutate(
    Petal.Length =
      as.character(tagList(
        span(Petal.Length),
        div(style = glue("width:8em;height:1em; background: linear-gradient(
    to right,
    red,
    red {frac_of_range_temp}%,
    blue {frac_of_range_temp}%,
    blue
  );"))
      ))
  ) |>
  select(-frac_of_range_temp)

datatable(myiris, escape = FALSE, options = list(dom = "t"))
