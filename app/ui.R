library(shiny)
library(DT)

shinyUI(
  navbarPage(title = "Visualiser",
             id = "navbar",
             
             tabPanel(title = "Import data",
                      mainPanel(
                        fileInput(inputId = "file",
                                  label = "Upload your text file here"),
                        textInput(inputId = "name1",
                                  label = "Type the name of person 1 "),
                        textInput(inputId = "name2",
                                  label = "Type the name of person 2"),
                        
                        DTOutput("raw_data")
                        )
                      ),
             
             tabPanel(title = "Another tab",
                      mainPanel(plotOutput(
                        outputId = "whatplot_date"
                      ) )
                      )
             )
)