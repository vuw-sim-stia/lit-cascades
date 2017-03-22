#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- shinyUI(absolutePanel(
  width=1600,
  height=1600,
  id="h337",
  # Application title
  #titlePanel("TIC Analysis Prototype"),
  includeScript("https://ajax.googleapis.com/ajax/libs/jquery/3.1.0/jquery.min.js"),
  includeScript("heatmap.min.js"),
  includeScript("FileSaver.min.js"),
  includeScript("h337.js"),
  navbarPage("Character co-occurrence in English literature - a distant reading prototype",
             tabPanel("Statistics",
                      sidebarPanel(selectInput('textSelect','Select text:',
                                               c('Great Expectations'='greatexpectations','David Copperfield'='davidcopperfield','Martin Chuzzlewit'='chuzzlewit')
                                               ))),
             tabPanel("Network Visualisation",
                                      htmlOutput("networkvisualisation"))
  )
))

server <- function(input, output) {
  
  addResourcePath("locpath", "/Users/tomgoldfinch/Documents/Research/lit-cascades/output")
  
  output$networkvisualisation <- renderUI({
    tagList(tags$iframe(
      src=paste("locpath/",input$textSelect,"_dynamic-network.html",sep=""), 
      width=1000, height=1000))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

