# ------------- Helper Functions -----------------------------------------------
# ------------ Loading libraries and data --------------------------------------
library(shiny)
library(plotly)
library(data.table)
load(file = "./timeUseActivity.rda")

# ------------ Defining functions to store in memory ---------------------------
activities <- levels(timeUse$TRTIER1P)
plots <- list()

codeColumn <- function(code) {
  if (nchar(code) %in% c(5, 6)) {
    codeLevel <- quote(TRCODEP)
  } else if (nchar(code) %in% c(3, 4)) {
    codeLevel <- quote(TRTIER2P)
  } else if (nchar(code) %in% c(1, 2)) {
    codeLevel <- quote(TRTIER1P)
  }
  return(codeLevel)
}

category <- function(groupName) {
  if (groupName == c("Gender")) {
    lineCat <- c("TESEX")
  } else if (groupName == c("Age")) {
    lineCat <- c("ageGroup")
  } else if (groupName == c("Age", "Gender")) {
    lineCat <- c("TESEX", "ageGroup")
  } else if (groupName == c("Metro")) {
    lineCat <- c("GTMETSTA")
  } else if (groupName == c("Employment")) {
    lineCat <- c("TELFS")
  } else if (groupName == c("Gender", "Metro")) {
    lineCat <- c("GTMETSTA", "TESEX")
  } else if (groupName == c("Age", "Metro")) {
    lineCat <- c("GTMETSTA", "ageGroup")
  } else {
    stop("Aggregation not implemented yet.")
  }
  group <- c("TUYEAR", lineCat)
  return(list(lineCat = lineCat, group = group))
}

plotGroup <- function(code, groupName, title = NULL) {
  title <- ifelse(is.null(title), activities[as.numeric(substr(code, 1, 2))], title)
  
  # Matching input code with tier level of activity
  code <- ifelse(nchar(code) %% 2 == 0, as.character(code), paste0("0", code))
  substring(code, 1, 2) <- ifelse(substr(code, 1, 2) == "18", "17", "")
  substring(code, 1, 2) <- ifelse(substr(code, 1, 2) == "50", "18", "")
  codeLevel <- codeColumn(code)
  
  # Getting relevant aggregations
  temp <- category(groupName)
  lineCat <- temp$lineCat
  group <- temp$group
  
  # Aggregating to appropriate activity level
  tempData <- timeUse[as.numeric(eval(codeLevel)) == as.numeric(code),
                      .(weeklyHoursLevel = sum(weeklyHours)),
                      by = .(TUCASEID, TEWHERE, GEMETSTA, GTMETSTA, PEEDUCA, 
                             PEHSPNON, PTDTRACE, TEAGE, TELFS, TESCHENR, 
                             TESCHLVL, TESEX, TESPEMPNOT, TRCHILDNUM,
                             TRDPFTPT, TRERNWA, TRYHHCHILD, TUDIARYDAY, 
                             TUFNWGTP, TUYEAR, home, ageGroup)]
  
  # Weighted average hours per week spent on activity.
  graphData <- tempData[, .(meanTime = weighted.mean(weeklyHoursLevel, 
                                                     w = TUFNWGTP)),
                        by = eval(group)]
  
  plots <-  graphData %>% plot_ly(x = ~TUYEAR, y = ~meanTime, type = "scatter",
                                  mode = "lines", color = as.formula(paste0("~factor(", lineCat, ")"))) %>%
    layout(title = title,
           xaxis = list(title = "Year"),
           yaxis = list(title = "Weekly Hours"),
           legend = list(orientation = 'h', x = 0.15, y = 0.95))
  return(plots)
}

# ------------- Server file ----------------------------------------------------
server <- function(input, output) {
  source("./helpers.R")
  
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

# ------------- UI file --------------------------------------------------------
ui <- fluidPage(
  
  title = 'Time Use Explorer',
  
  fluidRow(
    column(3, selectInput(inputId = 'activityCode1', label = 'Select first activity', 
                          choices = activities, selected = NULL, multiple = FALSE)),
    column(3, radioButtons(inputId = 'group1', label = 'Select grouping', 
                           choices = c('Age', 'Gender', 'Metro', 'Employment'), 
                           selected = 'Gender')),
    column(3, selectInput(inputId = 'activityCode2', label = 'Select second activity', 
                          choices = activities, selected = NULL, multiple = FALSE)),
    column(3, radioButtons(inputId = 'group2', label = 'Select grouping', 
                           choices = c('Age', 'Gender', 'Metro', 'Employment'), 
                           selected = 'Gender')),
    hr(),
    column(6, plotlyOutput('plot1')),
    column(6, plotlyOutput('plot2'))
  )
)

# --------------- Running Shiny ------------------------------------------------
shinyApp(ui = ui, server = server)
