library(shiny)

shinyUI(
  navbarPage(title = "Visualiser",
             id = "navbar",
             
             tabPanel(title = "Import data",
                      mainPanel(title = "Import more data")
                      ),
             
             tabPanel(title = "Another tab",
                      mainPanel(title = "Tab 2")
                      )
             )
)