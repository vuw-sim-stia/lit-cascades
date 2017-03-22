
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
  height=1000,
  navbarPage("Towards a Computational Literary Science",
             tabPanel("Home",
                      titlePanel(
                        h1(strong("Towards a Computational",br("Literary Science")))),
                      sidebarLayout(
                      sidebarPanel(
                        h4("Team"),
                          p("Led by Markus Luczak-Roesch and Adam Grener this interdisciplinary research project was supported under the spearheading digital futures theme, which is part of the areas of strategic distictiveness of Victoria University of Wellington."),
                        h5("Markus Luczak-Roesch"),
                          p("Markus Luczak-Roesch is a Senior Lecturer in Information Systems at the School for Information Management, Victoria Business School, Victoria University of Wellington. Before joining Victoria Markus worked as a Senior Research Fellow on the prestigious EPSRC programme grant",a(href="sociam.org",target="_blank","SOCIAM - The Theory and Practice of Social Machines"),"at the University of Southampton, Electronics and Computer Science (UK, 2013-2016). A computer scientist by education, Markus investigates the formal properties of information in socio-technical systems and human factors of information and computing systems.", br("More information:",a(href="http://markus-luczak.de",target="_blank","http://markus-luczak.de"))),
                        h5("Adam Grener"),
                          p("Adam Grener is Lecturer in the English Programme at Victoria University of Wellington. His main area of research is the nineteenth-century British novel, though he also has interest in the history of the novel, narrative theory, and computational approaches to literature. His work has appeared in the journals Genre, Narrative, and Modern Philology, and he is the co-editor of a special issue of Genre, “Narrative Against Data in the Victorian Novel,” set to appear in March 2017. He is completing a book on realist aesthetics and the history of probabilistic thought.", br("More information:",a(href="http://www.victoria.ac.nz/seftms/about/staff/adam-grener",target="-blank","http://www.victoria.ac.nz/seftms/about/staff/adam-grener"))),
                        h5("Research Assistants"),
                          p("Tom Goldfinch", br("Emma Fenton"))),
                      mainPanel(
                        br(),
                          img(src='net5.png', height = 500, width = 1000, align = "center"),
                        h4("Project Abstract"),
                          p("Data-driven analysis has emerged as a growing methodology, if not sub-discipline within literary studies. This approach, broadly described as “distant reading”, has harnessed available technology to open new avenues for how we understand literary texts, both individually and in the aggregate. Whereas traditional literary scholarship is generally grounded in the interpretation of the specific language of a text or body of texts, macroanalytic approaches have offered new ways of seeing texts. This interdisciplinary research project at the Victoria University of Wellington is an attempt to theorise the relationship between macroanalytic and microanalytic (distant and close) readings of individual works."),
                        h5(a(href="https://vuw-sim-stia.github.io/computational-literary-science/", target='_blank',"View the Project on GitHub"))
                        ))),
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
  addResourcePath("locpath", "/Users/tomgoldfinch/Documents/Research/lit-cascades/src/TLit/www/output")
  
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
    #netpath <- paste("file:///Users/tomgoldfinch/Documents/Research/lit-cascades/output/",input$textSelect,"_dynamic-network.html",sep="")
  })
  
  output$networkvisualisation_davidcopperfield <- renderUI({
    tagList(tags$iframe(src=paste("locpath/davidcopperfield_dynamic-network.html",sep=""), width=1000, height=800))
    #netpath <- paste("file:///Users/tomgoldfinch/Documents/Research/lit-cascades/output/",input$textSelect,"_dynamic-network.html",sep="")
  })
  
  output$networkvisualisation_chuzzlewit <- renderUI({
    tagList(tags$iframe(src=paste("locpath/chuzzlewit_dynamic-network.html",sep=""), width=1000, height=800))
    #netpath <- paste("file:///Users/tomgoldfinch/Documents/Research/lit-cascades/output/",input$textSelect,"_dynamic-network.html",sep="")
  })
  
  output$networkvisualisation_bleakhouse <- renderUI({
    tagList(tags$iframe(src=paste("locpath/bleakhouse_dynamic-network.html",sep=""), width=1000, height=800))
    #netpath <- paste("file:///Users/tomgoldfinch/Documents/Research/lit-cascades/output/",input$textSelect,"_dynamic-network.html",sep="")
  })
  
  output$networkvisualisation_pickwick <- renderUI({
    tagList(tags$iframe(src=paste("locpath/pickwick_dynamic-network.html",sep=""), width=1000, height=800))
    #netpath <- paste("file:///Users/tomgoldfinch/Documents/Research/lit-cascades/output/",input$textSelect,"_dynamic-network.html",sep="")
  })
  
  output$networkvisualisation_ourmutualfriend <- renderUI({
    tagList(tags$iframe(src=paste("locpath/ourmutualfriend_dynamic-network.html",sep=""), width=1000, height=800))
    #netpath <- paste("file:///Users/tomgoldfinch/Documents/Research/lit-cascades/output/",input$textSelect,"_dynamic-network.html",sep="")
  })
  
  output$networkvisualisation_phineasfinn <- renderUI({
    tagList(tags$iframe(src=paste("locpath/phineasfinn_dynamic-network.html",sep=""), width=1000, height=800))
    #netpath <- paste("file:///Users/tomgoldfinch/Documents/Research/lit-cascades/output/",input$textSelect,"_dynamic-network.html",sep="")
  })
  
  output$networkvisualisation_smallhouse <- renderUI({
    tagList(tags$iframe(src=paste("locpath/smallhouse_dynamic-network.html",sep=""), width=1000, height=800))
    #netpath <- paste("file:///Users/tomgoldfinch/Documents/Research/lit-cascades/output/",input$textSelect,"_dynamic-network.html",sep="")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)