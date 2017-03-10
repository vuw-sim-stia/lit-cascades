
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

ui <- shinyUI(absolutePanel(
  width=1600,
  height=1600,
  tags$link(
    href=paste0("http://fonts.googleapis.com/css?",
                "family=Source+Sans+Pro:300,600,300italic"),
    rel="stylesheet", type="text/css"),
  tags$style(type="text/css",
             "body {font-family: 'Source Sans Pro'}"),
  navbarPage("Character co-occurrence in English literature - a distant reading prototype",
             tabPanel("Home"),
             
             tabPanel("Statistics",
                      sidebarPanel(
                        helpText("From the below options, select a text and range of nodes to examine; alternately, you can hover over the graph to display various interactive methods."),
                        selectInput('textSelect', 'Select Text:', list('Charles Dickens' = c('Bleak House'='bleakhouse','David Copperfield'='davidcopperfield','Great Expectations'='greatexpectations','Martin Chuzzlewit'='chuzzlewit','Our Mutual Friend'='ourmutualfriend','Pickwick Papers'='pickwick'), 'Anthony Trollope' = c('Phineas Finn'='phineasfinn', 'Small House'='smallhouse'))),
                        sliderInput('noderange', label = 'Range of Nodes Within Text:', min = 1, max = 400, value = c(1, 400)),
                        uiOutput('characterrange'),
                        tableOutput('statistics')),
                      
                      mainPanel(tabsetPanel(
                        tabPanel("Entropy",
                                 fluidRow(column(width = 12,
                                                 plotlyOutput("plot_entropy", height = 800)))),
                        tabPanel("First and Last Character Appearances",
                                 fluidRow(class = "smallcharapp", column(width = 12,
                                                                         plotlyOutput("plot_character_appearance_first_last", height = 800)))),
                        tabPanel("All Character Appearances", 
                                 fluidRow(column(width = 12,
                                                 plotlyOutput("plot_character_appearance_all", height = 800))))))),
             
             tabPanel("Network Visualisation",
                      sidebarPanel(
                        helpText("The Network Visualisation provides a dynamic method of viewing how character co-occurence develops in a text over time.  Click on an edge to reveal the characters linking two nodes; click on a node for more information on the node, characters, and a link to the text content in a separate window.")),
                      mainPanel(
                        tabsetPanel(
                          tabPanel("Bleak House",
                                   htmlOutput("networkvisualisation_bleakhouse")),
                          tabPanel("David Copperfield",
                                   htmlOutput("networkvisualisation_davidcopperfield")),
                          tabPanel("Great Expectations",  
                                   htmlOutput("networkvisualisation_greatexpectations")),
                          tabPanel("Martin Chuzzlewit",
                                   htmlOutput("networkvisualisation_chuzzlewit")),
                          tabPanel("Our Mutual Friend",
                                   htmlOutput("networkvisualisation_ourmutualfriend")),
                          tabPanel("Pickwick Papers",
                                   htmlOutput("networkvisualisation_pickwick")),
                          tabPanel("Phineas Finn",
                                   htmlOutput("networkvisualisation_phineasfinn")),
                          tabPanel("Small House",
                                   htmlOutput("networkvisualisation_smallhouse"))))))))

server <- function(input, output) {
  
  # dataframes for Shiny - todo rewrite into generic
  # Dickens
  bleakhouse_entropy <- read.csv(file = "www/output/bleakhouse_gutenberg_entropy_csv.txt", sep = ",")
  bleakhouse_character_first_last <- read.csv(file = "www/output/bleakhouse_gutenberg_first_last_character_appearance_csv.txt", sep = ",")
  bleakhouse_character_all <- read.csv(file = "www/output/bleakhouse_gutenberg_character_frequency_csv.txt", sep = ",")
  bleakhouse_stats <- read.csv2(file='www/output/bleakhouse_netstat.csv', header = T)
  
  davidcopperfield_entropy <- read.csv(file = "www/output/davidcopperfield_gutenberg_entropy_csv.txt", sep = ",")
  davidcopperfield_character_first_last <- read.csv(file = "www/output/davidcopperfield_gutenberg_first_last_character_appearance_csv.txt", sep = ",")
  davidcopperfield_character_all <- read.csv(file = "www/output/davidcopperfield_gutenberg_character_frequency_csv.txt", sep = ",")
  davidcopperfield_stats <- read.csv2(file='www/output/davidcopperfield_netstat.csv', header = T)
  
  greatexpectations_entropy <- read.csv(file = "www/output/greatexpectations_gutenberg_entropy_csv.txt", sep = ",")
  greatexpectations_character_first_last <- read.csv(file = "www/output/greatexpectations_gutenberg_first_last_character_appearance_csv.txt", sep = ",")
  greatexpectations_character_all <- read.csv(file = "www/output/greatexpectations_gutenberg_character_frequency_csv.txt", sep = ",")
  greatexpectations_stats <- read.csv2(file='www/output/greatexpectations_netstat.csv', header = T)
  
  chuzzlewit_entropy <- read.csv(file = "www/output/chuzzlewit_gutenberg_entropy_csv.txt", sep = ",")
  chuzzlewit_character_first_last <- read.csv(file = "www/output/chuzzlewit_gutenberg_first_last_character_appearance_csv.txt", sep = ",")
  chuzzlewit_character_all <- read.csv(file = "www/output/chuzzlewit_gutenberg_character_frequency_csv.txt", sep = ",")
  chuzzlewit_stats <- read.csv2(file='www/output/chuzzlewit_netstat.csv', header = T)
  
  ourmutualfriend_entropy <- read.csv(file = "www/output/ourmutualfriend_gutenberg_entropy_csv.txt", sep = ",")
  ourmutualfriend_character_first_last <- read.csv(file = "www/output/ourmutualfriend_gutenberg_first_last_character_appearance_csv.txt", sep = ",")
  ourmutualfriend_character_all <- read.csv(file = "www/output/ourmutualfriend_gutenberg_character_frequency_csv.txt", sep = ",")
  
  pickwick_entropy <- read.csv(file = "www/output/pickwick_gutenberg_entropy_csv.txt", sep = ",")
  pickwick_character_first_last <- read.csv(file = "www/output/pickwick_gutenberg_first_last_character_appearance_csv.txt", sep = ",")
  pickwick_character_all <- read.csv(file = "www/output/pickwick_gutenberg_character_frequency_csv.txt", sep = ",")
  
  # Trollope
  phineasfinn_entropy <- read.csv(file = "www/output/phineasfinn_gutenberg_entropy_csv.txt", sep = ",")
  phineasfinn_character_first_last <- read.csv(file = "www/output/phineasfinn_gutenberg_first_last_character_appearance_csv.txt", sep = ",")
  phineasfinn_character_all <- read.csv(file = "www/output/phineasfinn_gutenberg_character_frequency_csv.txt", sep = ",")
  
  smallhouse_entropy <- read.csv(file = "www/output/smallhouse_gutenberg_entropy_csv.txt", sep = ",")
  smallhouse_character_first_last <- read.csv(file = "www/output/smallhouse_gutenberg_first_last_character_appearance_csv.txt", sep = ",")
  smallhouse_character_all <- read.csv(file = "www/output/smallhouse_gutenberg_character_frequency_csv.txt", sep = ",")
  
  
  #resource path for network visulisations
  addResourcePath("locpath", "/Users/mlr/Documents/git-projects/lit-cascades/src/TLit/www/output")
  
  #dynamic slider range for characters
  
  output$characterrange <- renderUI ({
    
    if (input$textSelect == "chuzzlewit") {charrange <- chuzzlewit_character_all}
    if (input$textSelect == "greatexpectations") {charrange <- greatexpectations_character_all}
    if (input$textSelect == "davidcopperfield") {charrange <- davidcopperfield_character_all}
    if (input$textSelect == "bleakhouse") {charrange <- bleakhouse_character_all}
    if (input$textSelect == "pickwick") {charrange <- pickwick_character_all}
    if (input$textSelect == "ourmutualfriend") {charrange <- ourmutualfriend_character_all}
    if (input$textSelect == "phineasfinn") {charrange <- phineasfinn_character_all}
    if (input$textSelect == "smallhouse") {charrange <- smallhouse_character_all}
    
    sliderInput('charrange', 'Number of Character Appearances', min = min(charrange$Appearances), max = max(charrange$Appearances), value = c(min(charrange$Appearances), max(charrange$Appearances)))
    
  })
  
  #statistics
  
  output$statistics <- renderTable ({
    
    if (input$textSelect == "chuzzlewit") {stats <- chuzzlewit_stats}
    if (input$textSelect == "greatexpectations") {stats <- greatexpectations_stats}
    if (input$textSelect == "davidcopperfield") {stats <- davidcopperfield_stats}
    if (input$textSelect == "bleakhouse") {stats <- bleakhouse_stats}
    if (input$textSelect == "pickwick") {stats <- pickwick_stats}
    if (input$textSelect == "ourmutualfriend") {stats <- ourmutualfriend_stats}
    if (input$textSelect == "phineasfinn") {stats <- phineasfinn_stats}
    if (input$textSelect == "smallhouse") {stats <- smallhouse_stats}
    
    tmpstat <- c('diameter(g)','min(degd)','max(degd)','mean(degd)','edge_density(g)','modularity(wtc)')
    tmpexpl <- c('?','?','?','?','?','?')
    stats <- cbind(tmpstat, stats, tmpexpl)
    names(stats) <- c('Statistic', 'Value', 'Details')
    stats
    
  })
  
  #entropy plot
  output$plot_entropy <- renderPlotly({
    
    if (input$textSelect == "chuzzlewit") {plotname_ent <- chuzzlewit_entropy}
    if (input$textSelect == "greatexpectations") {plotname_ent <- greatexpectations_entropy}
    if (input$textSelect == "davidcopperfield") {plotname_ent <- davidcopperfield_entropy}
    if (input$textSelect == "bleakhouse") {plotname_ent <- bleakhouse_entropy}
    if (input$textSelect == "pickwick") {plotname_ent <- pickwick_entropy}
    if (input$textSelect == "ourmutualfriend") {plotname_ent <- ourmutualfriend_entropy}
    if (input$textSelect == "phineasfinn") {plotname_ent <- phineasfinn_entropy}
    if (input$textSelect == "smallhouse") {plotname_ent <- smallhouse_entropy}
    
    ggplotly(ggplot(plotname_ent, aes(x = Node, y = Entropy)) + geom_point() + geom_line(linetype = 2, size=.2) + scale_x_continuous (limits = c(input$noderange), minor_breaks = seq(0 , 400, 1), breaks = seq(0, 400, 10), expand = c(0.02,0)) + scale_y_continuous (limits = c(0,5), minor_breaks = seq(0, 5, 0.25), breaks = seq(0, 5, 0.5)))
    
  })
  
  #plot for character first and last appearances
  
  plotname_char_first_last <- reactive({
    
    if (input$textSelect == "chuzzlewit") {plotname_char_first_last <- chuzzlewit_character_first_last}
    if (input$textSelect == "greatexpectations") {plotname_char_first_last <- greatexpectations_character_first_last}
    if (input$textSelect == "davidcopperfield") {plotname_char_first_last <- davidcopperfield_character_first_last}
    if (input$textSelect == "bleakhouse") {plotname_char_first_last <- bleakhouse_character_first_last}
    if (input$textSelect == "pickwick") {plotname_char_first_last <- pickwick_character_first_last}
    if (input$textSelect == "ourmutualfriend") {plotname_char_first_last <- ourmutualfriend_character_first_last}
    if (input$textSelect == "phineasfinn") {plotname_char_first_last <- phineasfinn_character_first_last}
    if (input$textSelect == "smallhouse") {plotname_char_first_last <- smallhouse_character_first_last}
    
    if(!is.null(input$charrange)){
      filteredData <- plotname_char_first_last %>%
        filter(Appearances >= input$charrange[1], Appearances <= input$charrange[2])}
    filteredData
    
  })
  
  output$plot_character_appearance_first_last <- renderPlotly({
    
    ggplotly(ggplot(plotname_char_first_last(), aes(x = Node, y = Character, group = Type, col = Type)) + theme(legend.position = c(300,3)) + geom_point() + scale_x_continuous (limits = c(input$noderange), minor_breaks = seq(0 , 400, 1), breaks = seq(0, 400, 10), expand = c(0.02,0)))
    
  })
  
  #plot for all character appearances
  
  plotname_char_all <- reactive({
    
    if (input$textSelect == "chuzzlewit") {plotname_char_all <- chuzzlewit_character_all}
    if (input$textSelect == "greatexpectations") {plotname_char_all <- greatexpectations_character_all}
    if (input$textSelect == "davidcopperfield") {plotname_char_all <- davidcopperfield_character_all}
    if (input$textSelect == "bleakhouse") {plotname_char_all <- bleakhouse_character_all}
    if (input$textSelect == "pickwick") {plotname_char_all <- pickwick_character_all}
    if (input$textSelect == "ourmutualfriend") {plotname_char_all <- ourmutualfriend_character_all}
    if (input$textSelect == "phineasfinn") {plotname_char_all <- phineasfinn_character_all}
    if (input$textSelect == "smallhouse") {plotname_char_all <- smallhouse_character_all}
    
    if(!is.null(input$charrange)){
      filteredData <- plotname_char_all %>%
        filter(Appearances >= input$charrange[1], Appearances <= input$charrange[2])}
    filteredData
    
  })
  
  output$plot_character_appearance_all <- renderPlotly({
    
    ggplotly(ggplot(plotname_char_all(), aes(x = Node, y = Character)) + geom_point() + scale_x_continuous (limits = c(input$noderange), minor_breaks = seq(0 , 400, 1), breaks = seq(0, 400, 10), expand = c(0.02,0)))
    
  }) 
  
  #network visualisations    
  output$networkvisualisation_greatexpectations <- renderUI({
    tagList(tags$iframe(src=paste("locpath/greatexpectations_dynamic-network.html",sep=""), width=1000, height=800))
    #netpath <- paste("file:///Users/mlr/Documents/git-projects/lit-cascades/output/",input$textSelect,"_dynamic-network.html",sep="")
  })
  
  output$networkvisualisation_davidcopperfield <- renderUI({
    tagList(tags$iframe(src=paste("locpath/davidcopperfield_dynamic-network.html",sep=""), width=1000, height=800))
    #netpath <- paste("file:///Users/mlr/Documents/git-projects/lit-cascades/output/",input$textSelect,"_dynamic-network.html",sep="")
  })
  
  output$networkvisualisation_chuzzlewit <- renderUI({
    tagList(tags$iframe(src=paste("locpath/chuzzlewit_dynamic-network.html",sep=""), width=1000, height=800))
    #netpath <- paste("file:///Users/mlr/Documents/git-projects/lit-cascades/output/",input$textSelect,"_dynamic-network.html",sep="")
  })
  
  output$networkvisualisation_bleakhouse <- renderUI({
    tagList(tags$iframe(src=paste("locpath/bleakhouse_dynamic-network.html",sep=""), width=1000, height=800))
    #netpath <- paste("file:///Users/mlr/Documents/git-projects/lit-cascades/output/",input$textSelect,"_dynamic-network.html",sep="")
  })
  
  output$networkvisualisation_pickwick <- renderUI({
    tagList(tags$iframe(src=paste("locpath/pickwick_dynamic-network.html",sep=""), width=1000, height=800))
    #netpath <- paste("file:///Users/mlr/Documents/git-projects/lit-cascades/output/",input$textSelect,"_dynamic-network.html",sep="")
  })
  
  output$networkvisualisation_ourmutualfriend <- renderUI({
    tagList(tags$iframe(src=paste("locpath/ourmutualfriend_dynamic-network.html",sep=""), width=1000, height=800))
    #netpath <- paste("file:///Users/mlr/Documents/git-projects/lit-cascades/output/",input$textSelect,"_dynamic-network.html",sep="")
  })
  
  output$networkvisualisation_phineasfinn <- renderUI({
    tagList(tags$iframe(src=paste("locpath/phineasfinn_dynamic-network.html",sep=""), width=1000, height=800))
    #netpath <- paste("file:///Users/mlr/Documents/git-projects/lit-cascades/output/",input$textSelect,"_dynamic-network.html",sep="")
  })
  
  output$networkvisualisation_smallhouse <- renderUI({
    tagList(tags$iframe(src=paste("locpath/smallhouse_dynamic-network.html",sep=""), width=1000, height=800))
    #netpath <- paste("file:///Users/mlr/Documents/git-projects/lit-cascades/output/",input$textSelect,"_dynamic-network.html",sep="")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)