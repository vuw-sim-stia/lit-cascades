# This is a Shiny web application to visualise networks of character co-occurrence in literature.
# You can run this application with the demo content from the github repository or populate your own
# data by running the prep.R script first.
#
# Find out more about building and running applications with Shiny here:
#    http://shiny.rstudio.com/
#
# Authors: Markus Luczak-Roesch, Tom Goldfinch

# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements.  See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership.  The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
#                                               "License"); you may not use this file except in compliance
# with the License.  You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
#

library(plyr)
library(shiny)
library(plotly)

allText <- read.csv2(file = "www/output/1000/allTexts.csv")

ui <- shinyUI(absolutePanel(
  width=1600,
  height=1000,
  navbarPage("SATIS Tool Prototype",
             tabPanel("Home",
                      #titlePanel(
                        #h1(strong("Towards a Computational",br("Literary Science")))),
                      sidebarLayout(
                      sidebarPanel(
                        h4("Project Overview"),
                        p("The programme creates network maps of characters in Charles Dickens's novels. Each node (or dot) represents a 1,000 word chunk of the novel. The programme scans the chunks sequentially and identifies which characters appear in each chunk. Two nodes are connected if they share sequential appearances of any character."),
                        p("The Dynamic Flow Character Network shows how the network is created as the novel unfolds from beginning to end."),
                        p("The Static Flow Character Network provides a view of the completed network."),
                        p("The Static Social Network shows how all of the characters are connected--in this visualisation, each node represents a character and not a chunk of the text."),
                        br(),
                        h5(a(href="https://vuw-fair.github.io/dickens-and-data-science/", target='_blank',"View the Research Methodology and Findings")),
                        br(),
                        h5("How to Read the Graphs and Plots"),
                        h6("Node"),
                          p("A node (or circle/dot) on a graph or plot indicates a 1000-word slice of the book. Clicking or hovering over the node will reveal more detail about, such as matched characters, or type of character appearance."),
                        h6("Edge"),
                          p("An edge (or line between two nodes) on a graph or plot indicates a connection between two 1000-word slices of the book due to character co-occurrence. Click or hover over an edge for more detail, such as the matched characters between two nodes.")),
                      mainPanel(
                        img(src='net5.png', height = 700, align = "center")
                        ))),
             tabPanel("Network Visualisations",
                      sidebarPanel(
                        helpText(
                          p("The Dynamic Character Flow Network provides a method of viewing how character co-occurence develops in a text over time.  Click on an edge to reveal the characters linking two nodes; click on a node for more information on the node, characters, and a link to the text content in a separate window; double-click on a node to highlight its connections."),
                          p("The Static Character Flow Network provides a high-level view of all relationships between characters over the course of a text, segmented by time of occurrence."),
                          p("The Static Social Network provides an overview of the relationships between characters over the entirety of a text. Click on a node to highlight it and its connections.")),
                        selectInput('dynSelect', 'Select Text:', list('Charles Dickens' = gsub("_"," ",allText$x)))),
                      mainPanel(
                        tabsetPanel(
                          tabPanel("Dynamic Character Flow Network",
                                   htmlOutput("networkvisualisation_chars")),
                          tabPanel("Static Character Flow Network",
                                   htmlOutput("networkvisualisation_statchars")),
                          tabPanel("Static Social Network",
                                   htmlOutput("networkvisualisation_social"))))),
             tabPanel("Statistics",
                      sidebarPanel(
                        helpText("From the below options, select a text and range of nodes to examine; alternately, you can hover over the graph to display various interactive methods."),
                        #selectInput('textSelect', 'Select Text:', list('Charles Dickens' = c('Bleak House'='bleakhouse','David Copperfield'='davidcopperfield','Great Expectations'='greatexpectations','Martin Chuzzlewit'='chuzzlewit','Our Mutual Friend'='ourmutualfriend','Pickwick Papers'='pickwick'), 'Anthony Trollope' = c('Phineas Finn'='phineasfinn', 'Small House'='smallhouse'))),
                        selectInput('textSelect', 'Select Text:', list('Charles Dickens' = gsub("_"," ",allText$x))),
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
            tabPanel("About",
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
                        h4("Project Abstract"),
                          p("Data-driven analysis has emerged as a growing methodology, if not sub-discipline within literary studies. This approach, broadly described as “distant reading”, has harnessed available technology to open new avenues for how we understand literary texts, both individually and in the aggregate. Whereas traditional literary scholarship is generally grounded in the interpretation of the specific language of a text or body of texts, macroanalytic approaches have offered new ways of seeing texts. This interdisciplinary research project at the Victoria University of Wellington is an attempt to theorise the relationship between macroanalytic and microanalytic (distant and close) readings of individual works."),
                        h5(a(href="https://vuw-fair.github.io/dickens-and-data-science/", target='_blank',"More"))
                        ))))))

server <- function(input, output) {
  #resource path for network visulisations
  addResourcePath("locpath", "./www/output")
  
  loadchars <- reactive({
    paste0("output/1000/",gsub(" ","_",input$dynSelect),"_dynamic-network.html")
  })
  
  loadstatchars <- reactive({
    paste0("output/1000/",gsub(" ","_",input$dynSelect),"_static-network.html")
  })
  
  loadsoc <- reactive({
    paste0("output/1000/",gsub(" ","_",input$dynSelect),"_social-network.html")
  })
  
  #dynamic slider range for characters
  slider <- reactive({
    paste0("www/output/1000/",gsub(" ","_",input$textSelect),"_gutenberg_character_frequency_csv.txt")
  })
  
  output$characterrange <- renderUI ({
    charrange <- read.csv(file = slider(), sep = ",")
    
    sliderInput('charrange', 'Number of Character Appearances', min = min(charrange$Appearances), max = max(charrange$Appearances), value = c(min(charrange$Appearances), max(charrange$Appearances)))
    
  })
  
  #statistics
  stat1 <- reactive({
    paste0("www/output/1000/",gsub(" ","_",input$textSelect),"_netstat.csv")
  })
  
  stat2 <- reactive({
    paste0("www/output/1000/",gsub(" ","_",input$textSelect),"_socnetstat.csv")
  })
  
  output$statistics <- renderTable ({
    stats <- read.csv2(file = stat1(), header = T)
    
    stats2 <- read.csv2(file = stat2(), header = T)
    
    tmpstat <- c('diameter(g)','min(degd)','max(degd)','mean(degd)','edge_density(g)','modularity(wtc)')
    tmpexpl <- c('?','?','?','?','?','?')
    stats <- cbind(tmpstat, stats, stats2, tmpexpl)
    names(stats) <- c('Statistic', 'Dynamic Character Network', 'Social Network','Detail')
    stats
    
  })
  
  #entropy plot
  entrop <- reactive({
    paste0("www/output/1000/",gsub(" ","_",input$textSelect),"_gutenberg_entropy_csv.txt")
  })
  
  output$plot_entropy <- renderPlotly({
    
    plotname_ent <- read.csv(file = entrop(), sep = ',')
    charrange <- read.csv(file = slider(), sep = ",")
    
    ggplotly((ggplot(plotname_ent, aes(x = Node, y = Entropy)) + geom_point() + geom_line(linetype = 2, size=.2) + scale_x_continuous (limits = c(input$noderange), minor_breaks = seq(0 , 400, 1), breaks = seq(0, 400, 10), expand = c(0.02,0)) + scale_y_continuous (limits = c(0,5), minor_breaks = seq(0, 5, 0.25), breaks = seq(0, 5, 0.5))))
    
  })
  
  #plot for character first and last appearances
  
  plotname_char_first_last <- reactive({
    
    plotname_char_first_last <- read.csv(file = paste0("www/output/1000/",gsub(" ","_",input$textSelect),"_gutenberg_first_last_character_appearance_csv.txt"), sep = ',')
    
    if(!is.null(input$charrange)){
      filteredData <- plotname_char_first_last %>%
        filter(Appearances >= input$charrange[1], Appearances <= input$charrange[2])}
    filteredData
    
  })
  
  output$plot_character_appearance_first_last <- renderPlotly({
    
    plotname_char_first_last <- read.csv(file = paste0("www/output/1000/",gsub(" ","_",input$textSelect),"_gutenberg_first_last_character_appearance_csv.txt"), sep = ',')
    plotheight <- nrow(count(unique(plotname_char_first_last$Character)))
    
   ggplotly((ggplot(plotname_char_first_last(), aes(x = Node, y = Character, col = Type)) + theme(legend.position = c(300,3)) + geom_point() + scale_color_manual(values=c("#33CC33","#FF0000")) + scale_x_continuous (limits = c(input$noderange), minor_breaks = seq(0 , 400, 1), breaks = seq(0, 400, 10), expand = c(0.02,0))),height=plotheight*18, width = 1200)
    
  })
  
  #plot for all character appearances
  
  plotname_char_all <- reactive({
    
    plotname_char_all <- read.csv(file = paste0("www/output/1000/",gsub(" ","_",input$textSelect),"_gutenberg_character_frequency_csv.txt"), sep = ',')
    
    if(!is.null(input$charrange)){
      filteredData <- plotname_char_all %>%
        filter(Appearances >= input$charrange[1], Appearances <= input$charrange[2])}
    filteredData
    
  })
  
  output$plot_character_appearance_all <- renderPlotly({
    
    plotname_char_first_last <- read.csv(file = paste0("www/output/1000/",gsub(" ","_",input$textSelect),"_gutenberg_first_last_character_appearance_csv.txt"), sep = ',')
    plotheight <- nrow(count(unique(plotname_char_first_last$Character)))
    
    ggplotly((ggplot(plotname_char_all(), aes(x = Node, y = Character)) + geom_point() + scale_x_continuous (limits = c(input$noderange), minor_breaks = seq(0 , 400, 1), breaks = seq(0, 400, 10), expand = c(0.02,0))),height=plotheight*18, width = 1200)
    
  }) 
  
  output$networkvisualisation_chars <- renderUI({
    tags$iframe(src=loadchars(), width=1000, height=800)
  })
  
  output$networkvisualisation_statchars <- renderUI({
    tags$iframe(src=loadstatchars(), width=1000, height=800)
  })
  
  output$networkvisualisation_social <- renderUI({
    tags$iframe(src=loadsoc(), width=1000, height=800)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)