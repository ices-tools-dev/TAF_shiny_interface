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




###############

library(shiny) 
library(data.tree)
library(DiagrammeR)
ui <- fluidPage(
  uiOutput("mainpage")
)
server <- function(input, output){
  active <- reactiveValues(main = 'initpage')
  working <- reactiveValues(org = NULL)
  current <- reactiveValues(ch_name = NULL,prefix  = NULL,addchild=NULL,addsibling=NULL,trunk_nm = NULL)
  output$mainpage <- renderUI({
    uiOutput(active$main)
  })
  output$initpage <- renderUI({
    sidebarLayout(
      sidebarPanel(
        fixedRow(
          textInput("parent","name of parent")
        ),
        fixedRow(
          textInput("initch_name","first child node name")
        ),
        fixedRow(
          h5("Must assign parent with initial child node")
        ),
        fixedRow(
          actionButton("crt_parent","Create Parent")
        )
      ),
      mainPanel(
      )
    )
  })
output$info_panel <- renderUI({
  tagList(
    fixedRow(
    h3("you are here")
  ),
  fixedRow(
    column(h4(current$trunk_nm$name),
           width=4
           ),
    column(h4(current$trunk_nm$level),
           width=2
    ),
    column(textOutput('chi_data'),
           width=4
           )
  )
  )
})
output$chi_data <- renderText({
  chdata <- chi_cnt()
  chdata
})
chi_cnt <- eventReactive(current$ch_names,{
 ln <- length(current$trunk_nm$children)
  for(i in 1:ln){
    if(i == 1){
      chi_list <- current$trunk_nm$children[[i]]$name
    }else{
      chi_list <- c(chi_list,current$trunk_nm$children[[i]]$name)
    }
  }
  return(chi_list)
})
  output$corepage <- renderUI({
    sidebarLayout(
      sidebarPanel(
        #textInput("tree_name","tree name"),
    uiOutput("info_panel"),
    fixedRow(
      if(!is.null(working$org)){
        selectInput("ttl","traverse to  level",choices=c(current$trunk_nm$name,chi_cnt()),selected = NULL)
      }
    ),
    fixedRow(
      actionButton("tverse","Climb to level")
    ),
    fixedRow(
      textInput("ch_name","Child node name")
    ),
    fixedRow(
      actionButton("add_child", "Add Child")
    ),
    fixedRow(
      textInput("sib_name","sibling node name")
    ),
    fixedRow(
      actionButton("add_sibling", "Add Sibling")
    )
  ),
  mainPanel(grVizOutput("HTATree")   )
    )
  })
  observeEvent(input$tverse,{
    current$trunk_nm <- current$trunk_nm$Climb(name=input$ttl)
  })
  output$HTATree=renderGrViz({
   working$org 
  if(!is.null(working$org)){
    if(!is.null(current$addchild)){
    current$trunk_nm$AddChild(current$addchild)
      current$ch_names <- names(current$trunk_nm$children)
      current$addchild <- NULL}
    if(!is.null(current$addsibling)){
      current$trunk_nm$AddSibling(current$addsibling)

      current$ch_names <- names(current$trunk_nm$children)
      current$addsibling <- NULL}
#child2 = org$AddChild("Child_2")
#child1$AddSibling("Sibling")
    grViz(DiagrammeR::generate_dot(ToDiagrammeRGraph(working$org)))
    }
  })

  observeEvent(input$crt_parent,{
    active$main <- 'corepage'
    working$org <- makeorg()
    current$trunk_nm <- working$org
  })
  makeorg <- reactive({
    if(is.null(input$parent)){
      return(NULL)
    }
    if(is.null(working$org)){
   working$org <- Node$new(input$parent)
   working$org$AddChild(input$initch_name)
   current$ch_names <- names(working$org$children)
    return(working$org)}else{
     return(working$org)
   }
  })
  observeEvent(input$add_child,{
    current$addchild <- input$ch_name
      })
  observeEvent(input$add_sibling,{
        current$addsibling <- input$sib_name
  })
}
shinyApp(ui = ui, server = server) 



#########################################

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
ui <- dashboardPage(
  title = "Box API",
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    tags$style("body { background-color: ghostwhite}"),
    fluidRow(
      actionButton("toggle_box", "Toggle Box"),
      actionButton("remove_box", "Remove Box", class = "bg-danger"),
      actionButton("restore_box", "Restore Box", class = "bg-success"),
      actionButton("update_box", "Update Box", class = "bg-primary")
    ),
    br(),
    box(
      title = textOutput("box_state"),
      "Box body",
      id = "mybox",
      collapsible = TRUE,
      closable = TRUE,
      plotOutput("plot")
    )
  )
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    req(!input$mybox$collapsed)
    plot(rnorm(200))
  })
  
  output$box_state <- renderText({
    state <- if (input$mybox$collapsed) "collapsed" else "uncollapsed"
    paste("My box is", state)
  })
  
  observeEvent(input$toggle_box, {
    updateBox("mybox", action = "toggle")
  })
  
  observeEvent(input$remove_box, {
    updateBox("mybox", action = "remove")
  })
  
  observeEvent(input$restore_box, {
    updateBox("mybox", action = "restore")
  })
  
  observeEvent(input$update_box, {
    updateBox(
      "mybox", 
      action = "update", 
      options = list(
        title = h2("New title", dashboardLabel(1, status = "primary")),
        status = "danger", 
        solidHeader = TRUE,
        width = 4
      )
    )
  })
  
  observeEvent(input$mybox$visible, {
    collapsed <- if (input$mybox$collapsed) "collapsed" else "uncollapsed"
    visible <- if (input$mybox$visible) "visible" else "hidden"
    message <- paste("My box is", collapsed, "and", visible)
    showNotification(message, type = "warning", duration = 1)
  })
}

shinyApp(ui, server)


library(shiny)
library(shinyBS)

shinyApp(
 ui =
 fluidPage(
   sidebarLayout(
     sidebarPanel(HTML("This button will open Panel 1 using updateCollapse."),
                  actionButton("p1Button", "Push Me!"),
                  selectInput("styleSelect", "Select style for Panel 1",
                   c("default", "primary", "danger", "warning", "info", "success"))
     ),
     mainPanel(
       bsCollapse(id = "collapseExample", open = "Panel 2",
                  bsCollapsePanel("Panel 1", "This is a panel with just text ",
                   "and has the default style. You can change the style in ",
                   "the sidebar.", style = "info"),
                  bsCollapsePanel("Panel 2", "This panel has a generic plot. ",
                   "and a 'success' style.", plotOutput("genericPlot"), style = "success")
       )
     )
   )
 ),
 server =
 function(input, output, session) {
   output$genericPlot <- renderPlot(plot(rnorm(100)))
   observeEvent(input$p1Button, ({
     updateCollapse(session, "collapseExample", open = c("Panel 1", "Panel 2"))
   }))
   observeEvent(input$styleSelect, ({
     updateCollapse(session, "collapseExample", style = list("Panel 1" = input$styleSelect))
   }))
 }
)

  body <- dashboardBody(
    useShinyjs(),
    
      tags$style(HTML("
      .box-header {
        padding: 0 10px 0 0;
      }
      .box-header h3 {
        width: 100%;
        padding: 10px;
      }")),
    
      fluidRow(
        box(id="box1", title = "Histogram box title",
            status = "warning", solidHeader = TRUE, collapsible = T,
            plotOutput("plot", height = 250)
        )
      )
    )
  
    server <- function(input, output) {
    
      output$plot <- renderPlot({
        hist(rnorm(50))
      })
    
      runjs("
      $('.box').on('click', '.box-header h3', function() {
          $(this).closest('.box')
                 .find('[data-widget=collapse]')
                 .click();
      });")
    }
  
  shinyApp(
    ui = dashboardPage(
      dashboardHeader(),
      dashboardSidebar(),
      body
    ),
    server = server
  )


  library(shiny)

  ui <- shinyUI(bootstrapPage(
    absolutePanel(
      id = "controls", class = "panel panel-default", fixed = TRUE,
      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
      width = 330, height = "auto",
      HTML('<button data-toggle="collapse" data-target="#demo">Collapsible</button>'),
      tags$div(
        id = "demo", class = "collapse",
        checkboxInput("input_draw_point", "Draw point", FALSE),
        verbatimTextOutput("summary")
      )
    )
  ))

  server <- shinyServer(function(input, output, session) {
    output$summary <- renderPrint(print(cars))
  })

  shinyApp(ui = ui, server = server)




library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

  includeCSS(path = "AdminLTE.css"), #added 
  includeCSS(path = "shinydashboard.css"), #added

  #add this file and collapsible nature should work.
  includeScript(path = "app.js"), # 

   # Application title
   titlePanel("Old Faithful Geyser Data"),

   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),

      # Show a plot of the generated distribution
      mainPanel(
         box(plotOutput("distPlot"), solidHeader = T, collapsible = T, title = "collapsible box not collapsing", status = "primary")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)

      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)


library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
  # Application title
  titlePanel("Old Faithful Geyser Data"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
         sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30)
      ),
      # Show a plot of the generated distribution
      mainPanel(
         box(plotOutput("distPlot"), solidHeader = T, collapsible = T, 
             title = "collapsible box not collapsing", status = "primary")
      )
    ) 
  )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)



test <- icesFO::load_sid(2023) %>% dplyr::select(.,StockKeyLabel)

icesTAF::dir.tree(path = "D:/GitHub_2023/2023_FisheriesOverview")



library(shiny)
library(shinyTree)

ui <- fluidPage(
  shinyTree::shinyTree('myShinyTree',
                       checkbox = TRUE,
                       themeIcons = FALSE,
                       theme = 'proton'
                       )
)

server <- function(input, output, session) {
  df <- data.frame(continent = c(rep("Letteria", 2), rep("Numberalia", 1)),
                   country = c("<img src = 'flag_A.png' width = '50px' /> A-Land", 
                               "<img src = 'flag_B.png' width = '50px' /> B-Country",
                               "<img src = 'flag_1.png' width = '50px' /> Uni Kingdom"
                               )
                   )

  output$myShinyTree <-  renderTree({
    tree_list <- dfToTree(df) ## dfToTree converts df into a hierarch. list
    attr(tree_list, 'stopened') <- TRUE
    ## set the 'stopened' attribute on each node to uncollapse the whole tree
    tree_list <- tree_list |>
      Map(f = \(.) {attr(., 'stopened') <- TRUE; .})

  })
}

shinyApp(ui, server)






library(data.tree)
library(shiny)
library(shinyTree)
library(shinyjs)

## load acme and alter some nodes
data(acme)
acme$IT$li_attr         <- list(class = "myl")
acme$Accounting$icon    <- "file"


ui <- fluidPage(
  tags$head(
    tags$style(HTML(".myl {color: red}"))
  ),
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      h2("Input"),
      verbatimTextOutput("input_tree"),
      helpText("This shinyTree control receives this ", code("data.tree"), " as input.",
               "In order to use that, ", code("treeToJSON(.)"), " is called with the tree.",
               "The resulting JSON can be found below."),
      verbatimTextOutput("json")
    ),
    mainPanel(
      h2("Output"),
      fluidRow(
        column(width = 4,
               h3("ShinyTree"),
               shinyTree("tree", dragAndDrop = TRUE)
        ),
        column(width = 7,
               h3(code("input$tree")),
               helpText("Change radio buttons once or change the tree to",
                        "force rendering of", code("input$tree")),
               radioButtons("parser",
                            "Parser:",
                            c("tree", "list")),
               verbatimTextOutput("output_tree"),
               helpText("As you can see, only attributes", code("cost"), "and",
                        code("p"), "are in slot", code("data"), ".",
                        code("li_attr"), "and", code("icon"), "were on the top level",
                        "of the node and are thus not returned by shinyTree.",
                        "State is always generated anew and hence also part of",
                        code("input$tree"), "despite sitting also on the top level.")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  print_tree <- function(tree) {
    if (is(tree, "Node")) {
      do.call(print, c(x = tree, as.list(tree$fieldsAll)))
    } else {
      str(tree)
    }
  }
  
  observe({
    options(shinyTree.defaultParser = input$parser)
    ## trigger "ready.jstree" by hand to force input$tree to update
    runjs("$('.jstree').trigger('ready.jstree')")
  })
  
  get_json <- reactive({
    treeToJSON(acme, pretty = TRUE)
  })
  
  output$input_tree  <- renderPrint(print_tree(acme))
  output$output_tree <- renderPrint(print_tree(req(input$tree)))
  output$json        <- renderPrint(cat(get_json()))   
  output$tree        <- renderTree(get_json())
}

shinyApp(ui, server)


library(shinyFileTree)

install.packages("shinyFileTree")
shinyFileTree(icesTAF::dir.tree(path = "D:/GitHub_2023/2023_FisheriesOverview"), 
             is_directory = TRUE,
             plugins = c("checkbox"),
             multiple = TRUE,
             opts = shinyFileTreeOpts(icons = TRUE)
)
devtools::install_github("fbreitwieser/shinyFileTree")




library(shiny)
library(shinyTree)

#' Examples of using jstree types to define node attributes
#' @author Michael Bell \email{bellma@@lilly.com}
shinyServer(function(input, output, session) {
  log <- c(paste0(Sys.time(), ": Interact with the tree to see the logs here..."))
  
  treeData <- reactive({
    list(
      root1 = structure("", stselected=TRUE,sttype="root"),
      root2 = structure(list(
        SubListA = structure(list(
            leaf1 = structure("",sttype="file",sticon="fa fa-signal"), 
            leaf2 = structure("",sttype="file"),
            leaf3 = structure("",sttype="file")),
            sttype="root",stopened=TRUE
            ),
        SubListB = structure(list(
          leafA = structure("",sttype="default",sticon="glyphicon glyphicon-leaf"),
          leafB = structure("",sttype="default",sticon="shinyTree/icon.png"),
          leafC = structure("",sttype="default",sticon="map-signs")
          ),stopened=TRUE,sttype="root")
      ),
      sttype="root",stopened=TRUE
    )
  )
  })
  
  observeEvent(input$updateTree,{
    updateTree(session, treeId = "tree", data = treeData())
  })
  
  output$tree <- renderTree({
    treeData()
  })
})
