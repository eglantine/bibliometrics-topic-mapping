library(shiny)
library(arules)
library(arulesViz)
library(jsonlite)
library(plyr)
library(igraph)
library(httr)

# ui.R

shinyUI(fluidPage(theme = "bootstrap.css",
                  
                  tags$div(HTML('
           <div class="navbar navbar-default navbar-fixed-top">
            <div class="container-fluid">
             <div class="navbar-header">
              <a>
               <img src="diamant_white_transparent_128.png" alt="Proxem" class="header-logo"style="padding-top: 10px;">
              </a>
            </div>
            <div id="mainNavbar">
             <ul class="nav navbar-nav">
              <li class="selected" style="font-family:"Source sans Pro";"><a>Bibliographic mapping</a></li>
             </ul>
            </div>
            </div>
           </div>'
                  )),
                  
                  titlePanel("Bibliometrics mapping"),
  sidebarLayout(
    sidebarPanel(
      h3("Title retrieval"),
      
      textInput("query", "Your query term (can be several words)", "bibliometrics"),
      checkboxInput('sample', 'Sample only (100 documents; otherwise maximum is 5000)'),
      checkboxInput('deduplicate', 'Deduplicate titles'),
      
      actionButton('makeSearch', 'Search'),
      
      h3("Visualization"),
      
      sliderInput("support",
                  "Support:",
                  min = 0,  max = 0.1, value = 0.1),
      
      sliderInput("confidence",
                  "Confidence:",
                  min = 0,  max = 0.1,  value = 0.1),
      
      radioButtons("outputFormat", h3("Export"),
                   c("Article titles (csv)"="title", "Topic list (csv)"="topic", "Association rules (GML graph)"="gml"),
                   "none"),      
      
      
      downloadButton("downloadData", "Download")
      
    ),
    
    mainPanel(
      textOutput("resultNumber"),
      
      tabsetPanel(type = "tabs", 
                  tabPanel("Titles", tableOutput("titles")), 
                  tabPanel("Topics", dataTableOutput("contents")), 
                  tabPanel("Rules", verbatimTextOutput("rules")),
                  tabPanel("Graph", plotOutput("graphPlot"))
      )
    )
  )
))
