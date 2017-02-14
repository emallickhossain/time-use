# ------------- UI file --------------------------------------------------------
ui <- fluidPage(
  
  title = 'Time Use Explorer',
  
  fluidRow(
    column(3, selectInput(inputId = 'activityCode1', label = 'Select first activity', 
                          choices = activities, selected = NULL, multiple = FALSE)),
    column(3, radioButtons(inputId = 'group1', label = 'Select grouping', 
                           choices = c('Age', 'Gender', 'Metro'), 
                           selected = 'Gender')),
    column(3, selectInput(inputId = 'activityCode2', label = 'Select second activity', 
                          choices = activities, selected = NULL, multiple = FALSE)),
    column(3, radioButtons(inputId = 'group2', label = 'Select grouping', 
                           choices = c('Age', 'Gender', 'Metro'), 
                           selected = 'Gender')),
  hr(),
  column(6, plotlyOutput('plot1')),
  column(6, plotlyOutput('plot2'))
  )
)

# ------------- Server file ----------------------------------------------------
server <- function(input, output) {
  source("helpers.R")
  
  output$plot1 <- renderPlotly({  
    code <- which(activities == input$activityCode1)
    groupName <- input$group1
    plotGroup(code, groupName)
    })
  
  output$plot2 <- renderPlotly({  
    code <- which(activities == input$activityCode2)
    groupName <- input$group2
    plotGroup(code, groupName)
  })
}

# --------------- Running Shiny ------------------------------------------------
shinyApp(ui = ui, server = server)
