# SimpleHealthRetreiver
#
# Jacob Saltzman
# 
# ui.R
library(shiny)

shinyUI(navbarPage("SimpleHealthRetriever",
  tabPanel("File Upload",
    sidebarLayout(
      sidebarPanel(
        fileInput('xmlfile',label = "Upload Data File",accept = ".xml"),
        tags$hr(),
        checkboxInput('showall',label="Show all data",value = TRUE),
        conditionalPanel(
          condition = "input.showall == false",
          uiOutput("choose.type")
        ),
        # checkboxInput('unix.time',label = "Use UNIX timestamps",value = FALSE),
        radioButtons('format.choose',label = 'Table Format',choices = c("Wide","Long"),selected = "Wide"),
        downloadButton('downloadbutton',label = 'Download Table'),
        tags$hr(),
        HTML("
          <h4>Instructions</h4>
          1. Open Health App <br>
          2. Export Data via Health Data -> All -> [Export Box] <br>
          3. Upload file named 'export.xml' (NOT 'export_cda.xml') <br>
        ")
        ),
      mainPanel(
        dataTableOutput('health.table')
        )
      )
   ),
  tabPanel("Feature Explorer",
      # Forthcoming!
           ""
   )
  
  ))
