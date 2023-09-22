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

test_tree <- icesTAF::dir.tree(path = "D:/GitHub_2023/2023_FisheriesOverview")
shinyTree::treeToDf(test, hierarchy = NULL)
shinyTree::treeToJSON(test_tree)

library(devtools)
install_github("trinker/pathr")
library(pathr)
test <- tree(path = "D:/GitHub_2023/2023_FisheriesOverview", include.files = TRUE, all.files = TRUE,
  use.data.tree = TRUE, out = NULL, additional = NULL,
  copy2clip = FALSE)



dat <- tibble::tribble(
  ~level1,~level2,~level3,~level4,
  "Beverages","Water","","",
  "Beverages","Coffee","","",
  "Beverages","Tea","Black tea","",
  "Beverages","Tea","White tea","",
  "Beverages","Tea","Green tea","Sencha",
  "Beverages","Tea","Green tea","Gyokuro",
  "Beverages","Tea","Green tea","Matcha",
  "Beverages","Tea","Green tea","Pi Lo Chun"
)

paths <- data.frame(pathString = apply(dat, 1, paste0, collapse = "/"))
paths <- list.dirs(path = "D:/GitHub_2023/2023_FisheriesOverview", full.names = TRUE, recursive = TRUE)


library(data.tree)
tree <- as.Node(paths)
LL <- as.list(tree)
L <- LL[-1]

library(htmltools)

f <- function(node, nodeName){
  if(all(lengths(node) == 0) && length(names(node))){
    tagList(
      tags$p(nodeName),
      do.call(tags$ul, unname(lapply(names(node), tags$li)))
    )
  }else{
    if(length(names(node))){
      tags$li(
        tags$p(nodeName),
        do.call(tags$ul, mapply(f, node, names(node), SIMPLIFY = FALSE, USE.NAMES = FALSE))
      )
    }else{
      tags$li(
        tags$p(nodeName)
      )
    }
  }
}

lis <- mapply(f, L, names(L), SIMPLIFY = FALSE, USE.NAMES = FALSE)
ul <- do.call(tags$ul, lis)

html <- as.character(tagList(tags$p(LL$name), ul))


library(plotly)
library(crosstalk)

d <- SharedData$new(txhousing, ~city)
p <- ggplot(d, aes(date, median, group = city)) + geom_line()
p <- ggplot(EGStats, aes(year, percent, group = expertGroup)) + geom_line()
fig <- ggplotly(p, tooltip = "expertGroup") %>%
  highlight(on = "plotly_hover", color = "red")



d <- highlight_key(txhousing, ~city)
p <- ggplot(d, aes(date, median, group = city)) + geom_line()
gg <- ggplotly(p, tooltip = "city") 
highlight(gg,on = "plotly_hover", color = "red",dynamic = TRUE)


d <- highlight_key(EGStats, ~expertGroup)
p <- ggplot(d, aes(year, percent, group = expertGroup)) + geom_line() + geom_point()
gg <- ggplotly(p, tooltip = "expertGroup") 
highlight(gg, on = "plotly_hover", color = "red",,dynamic = TRUE)




library(shiny)
library(shinyjs)
library(dplyr)
library(ggplot2)

set.seed(1)
words <- sort(sapply(1:50, USE.NAMES = F, FUN = function (x) paste(sample(letters, 15), collapse = "")), decreasing = T)

dat <- data.frame(words, f = sort(rgamma(50, shape = 5, scale = 1)),stringsAsFactors = F)

ui <- pageWithSidebar(
  headerPanel("Playground"),
  sidebarPanel(),
  mainPanel(
    uiOutput("links"),
    plotOutput("out.plot"),
    useShinyjs()
  ))

server <- function(input, output, session) {
  urls <- lapply(dat$words, FUN = function (x) {
    div(id=x, a(paste0(" ", x, " "),
      href = paste0("https://", x, ".de"),
      target = "_blank"))
  })
  output$links <- renderUI({
    tagList(urls)
  })

  # Add a reactieVal that we can update once an object is hovered.
  hovered_element <- reactiveVal('')

  # Add onevent for each element in dat$words, to update reactiveVal.
  lapply(dat$words,function(x){
    onevent(event='mouseleave',id=x,hovered_element(''))
    onevent(event='mouseenter',id=x,hovered_element(x))
  })

  # Add a reactive for the dataset, which we debounce so it does not invalidate too often.
  my_data <- reactive({    
    dat$color <- ifelse(dat$words==hovered_element(),'hovered','')
    dat
  })
  my_data <- my_data %>% debounce(50) # tune for responsiveness

  # Plot
  output$out.plot <- renderPlot({
    ggplot(my_data(), aes(x = words, y = f,fill=color)) +
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = 90)) + theme(legend.position="none")
  })
}

shinyApp(ui, server)



#############################################

set.seed(1014)
df <- data.frame(x = rnorm(100), y = rnorm(100))

ui <- fluidPage(
  plotOutput("plot", click = "plot_click", )
)
server <- function(input, output, session) {
  dist <- reactiveVal(rep(1, nrow(df)))
  observeEvent(input$plot_click,
    dist(nearPoints(df, input$plot_click, allRows = TRUE, addDist = TRUE)$dist_)  
  )
  
  output$plot <- renderPlot({
    df$dist <- dist()
    ggplot(df, aes(x, y, size = dist)) + 
      geom_point() + 
      scale_size_area(limits = c(0, 1000), max_size = 10, guide = NULL)
  }, res = 96)
}

EGStats <- jsonlite::read_json("https://adminweb06.ices.dk/api/getEGStatistics", simplifyVector = TRUE)

ui <- fluidPage(
  plotOutput("plot", click = "plot_click", )
)

server <- function(input, output, session) {
  # dist <- reactiveVal(rep(1, nrow(EGStats)))
  
  test <- eventReactive(input$plot_click,{
    nearPoints(df = EGStats, 
                coordinfo = input$plot_click, 
                xvar = year,
                yvar = percent,
                allRows = TRUE)
})

  output$plot <- renderPlot({
    print(test())
    # EGStats$dist <- dist()
    ggplot(EGStats, aes(year, percent, color = expertGroup)) + 
      geom_point() + 
      scale_size_area(limits = c(0, 1000), max_size = 10, guide = NULL)
  }, res = 96)
}


#create a SharedData object for use in the ggplot below, group by 'groups' 
d <- highlight_key(xy, ~groups )

#create a normal ggplot to fit your needs, but use the SharedData object as data for the chart
p <- ggplot( d, aes(x = letters, y = values, group = groups)) + theme_bw() + geom_point()

#now ggplotly the newly created ggplot, and add text for the tooltips as needed
gg <- ggplotly( p, tooltip = "groups" )

#set the highlight-options to your liking, and plot...
highlight( gg, on = "plotly_hover", off = "plotly_deselect", color = "red" )



library(gapminder)
g <- highlight_key(gapminder, ~country)
continent_filter <- filter_select(
  "filter", "Select a country", 
  g, ~continent
)

p <- plot_ly(g) %>%
  group_by(country) %>%
  add_trace(x = ~year, y = ~lifeExp, color = ~continent, mode = 'lines+markers') %>%
  layout(xaxis = list(title = "")) %>%
  highlight(selected = attrs_selected(showlegend = FALSE))

bscols(continent_filter, p, widths = 12)


library(gapminder)
g <- highlight_key(EGStats, ~expertGroup)


p <- plot_ly(g) %>%
  group_by(expertGroup) %>%
  add_trace(x = ~year, y = ~percent, color = ~expertGroup, mode = 'lines+markers', line = list(shape = 'spline', smoothing = .9)) %>%
  layout(xaxis = list(title = "")) %>%
  highlight(selected = attrs_selected(showlegend = FALSE))

bscols(continent_filter, p, widths = 12)


install.packages("BiocManager")
library(rhdf5)
BiocManager::install("rhdf5")
h5f = h5read("D:/GitHub_2023/tafXplorer/App/Sample.h5","df")
h5f = H5Fopen("D:/GitHub_2023/tafXplorer/App/Sample.h5")



library(shiny)

ui <- fluidPage(
  h4("Embedded image"),
  uiOutput("img")
)

server <- function(input, output, session) {
  output$img <- renderUI({
      tags$img(src = "https://www.r-project.org/logo/Rlogo.png")
  })
}

shinyApp(ui, server)




library(shiny)

ui <- fluidPage(
        htmlOutput("markup"),
        textOutput("clicked_text"),
        includeScript("D:/GitHub_2023/tafXplorer/Dev/detect_click.js")
)

# sample data
data <- c("This", "is", "a", "sentence")

# function to markup text
markup.text <- function(data) {
        markup <- lapply(data, function(text) {
                paste0("<a href='#' onclick='detect_click(this)'>text</a>")
                       
        }) %>%
                paste(collapse = " ")
        return(markup)
}

server <- function(input, output) {

        output$markup <- renderUI({ # display the marked-up text
                HTML(markup.text(c("This", "is", "a", "sentence")))
                })
        output$clicked_text <- eventReactive(input$clicked_text, { # display clicked text output
                print(input$clicked_text)
        })
}

shinyApp(ui = ui, server = server)




#####################################good example to follow
library(shiny)

ui <- fluidPage(
  
  tags$script(HTML(
    "$(document).ready(function(){
  $('body').on('click', 'a', function(evt){
    Shiny.setInputValue('clicked_text', evt.target.id);
  });
})"
  )),
  
  tags$a(href = "#", id = 1, "id is 1"),
  br(),
  tags$a(href = "#", id = 2, "id is 2"),
  br(),
  tags$a(href = "#", id = 3, "id is 3"),
  
  br(),
  tags$a(href = "#", id = 4, "id is 4, javascript function should be suppressed for this link"),
  br(),
  tags$a(href = "#", id = 5, "id is 5, javascript function should be suppressed for this link"),
  
  textOutput("clicked_text"),
)

server <- function(input, output) {
  
  output$clicked_text <- eventReactive(input$clicked_text, {
    if(input$clicked_text %in% 1:3) input$clicked_text
  })
}

shinyApp(ui = ui, server = server)




get_icon <- function(text) {
  if (nchar(text) == 0) {
    x <- paste(shiny::icon("folder-open"))
  } else if (text == "csv") {
    x <- paste(shiny::icon('file-csv'))
  } else if (text == "png") {
    x <- paste(shiny::icon('file-image'))
  } else if (text == "rds") {
    x <- paste(shiny::icon('r-project'))
  } else if (text == "txt") {
    x <- paste(shiny::icon('code'))
  } else if (text == "bib") {
    x <- paste(shiny::icon('book'))
  } else{
    x <- ""
  }
  return(x)
}


create_interactive_tree <- function(path, repo) {
  paths <- list.files(path,
    recursive = TRUE, full.names = TRUE,
    include.dirs = TRUE
  )

  # to clean off initial path -  will not need this in production
  paths <- gsub("D:/GitHub_2023/tafXplorer/App/Data/ices_cat_3_template", "", paths)

  tree <- as.Node(data.frame(pathString = paths))

  output <- ToDataFrameTree(tree, "pathString", "isLeaf", "level")
  output$filename <- basename(output$pathString)
  # output$filename <- paste0("`r shiny::icon('markdown')` ", output$filename)

  output$urlString <- paste0("https://ices-taf.shinyapps.io/tafxplorer/?Assessmentresults?pathstring=", output$pathString, "&repo=", repo)

  # could be handy for file icons
  FileFormats <- tools::file_ext(output$filename)


  makeOne <- function(i) {
    paste0(
      paste(rep("  ", output$level[i] - 1), collapse = ""),
      "* ",
       sapply(FileFormats[i], get_icon), 
       tags$a(href = "#", id = output$filename[i], output$filename[i])
      # " [", output$filename[i], "]",
      # "(", "href= '#' ", "id=", output$filename[i], ")"
    )
  }

  all <- paste(
    sapply(1:nrow(output), makeOne),
    collapse = "\n"
  )


  cat(all)


  html <- markdown::mark(text = all)

  return(html)
}


path <- "D:/GitHub_2023/tafXplorer/App/Data/ices_cat_3_template"
repo <- "testRepo"
HTML(create_interactive_tree(path, repo))




library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  titlePanel("File Viewer"),
  sidebarLayout(
    sidebarPanel(
      textInput("urlInput", "Enter URL:", ""),
      selectInput("fileType", "Select File Type:", choices = c("CSV", "PNG")),
      actionButton("loadButton", "Load File"),
      downloadButton("downloadCSV", "Download CSV")
    ),
    mainPanel(
      uiOutput("fileViewer")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$loadButton, {
    # Check if a valid URL is provided
    if (!grepl("^https?://", input$urlInput)) {
      shinyjs::alert("Please enter a valid URL starting with http:// or https://.")
      return()
    }

    # Download the file from the URL
    file_extension <- tolower(tools::file_ext(input$urlInput))
    if (file_extension == "csv" && input$fileType == "CSV") {
      data <- read.csv(input$urlInput)
      output$fileViewer <- renderTable({
        data
      })
      output$downloadCSV <- downloadHandler(
        filename = function() {
          paste("downloaded_data.csv")
        },
        content = function(file) {
          write.csv(data, file)
        }
      )
    } else if (file_extension == "png" && input$fileType == "PNG") {
      output$fileViewer <- renderText({c('<img src="',input$urlInput,'">')})
      # output$fileViewer <- renderImage({
      #   list(src = input$urlInput, contentType = "image/png")
      # }, deleteFile = FALSE)
    } else {
      shinyjs::alert("Invalid file type or file format.")
    }
  })
}

shinyApp(ui, server)
library("tools")
file_ext("https://adminweb06.ices.dk/api/blob/2015_had-iceg/report/biomass.png")

jsonlite::read_json("https://adminweb06.ices.dk/api/dir/ices_cat_3_template", simplifyVector = TRUE)


rmarkdown::knitr_options_html()



########################

library(shiny)
library(shinyAce)

ui <- fluidPage(
  titlePanel("R Code with Syntax Highlighting"),
  
  sidebarLayout(
    sidebarPanel(
      # No sidebar content needed for this example
    ),
    
    mainPanel(
      aceEditor(
        outputId = "code",
        value = "### ------------------------------------------------------------------------ ###
### Apply rfb rule ####
### ------------------------------------------------------------------------ ###

## Before: data/idx.csv
##         data/advice_history.csv
##         data/length_data.rds
## After:  model/advice.rds

library(TAF)
library(cat3advice)
library(dplyr)

mkdir(\'model\')

### ------------------------------------------------------------------------ ###
### load data ####
### ------------------------------------------------------------------------ ###",
        mode = "r",
        theme = "github",
        fontSize = 14,
        height = "500px",
        readOnly = TRUE
      )
    )
  )
)

server <- function(input, output) {
  # No server logic needed for this example
}

shinyApp(ui = ui, server = server)


getAceModes()
getAceThemes()
basename(stocklist$gitHubUrl)
jsonlite::read_json(paste0("https://adminweb06.ices.dk/api/dir/","2022_cod.27.47d20_assessment"), simplifyVector = TRUE)
