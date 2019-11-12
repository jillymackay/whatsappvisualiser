library(shiny)
library(DT)

shinyUI(
  navbarPage(title = "Visualise your two-person WhatsApp chats",
             id = "navbar",
             
             tabPanel(title = "Import your chat",
                      sidebarLayout(
                        sidebarPanel(tags$p("First, export your WhatsApp chat (without media) from the app. Upload the .txt file below."),
                                     tags$em("P.S. Some people use emojis or other special characters in their contact details.
                                             If you do this, you may need to rename the file if you're having difficulties uploading the file."),
                          fileInput(inputId = "file",
                                               label = "Upload your text file here"),
                          tags$p("To ensure the visualisations look their prettiest, you have to confirm the speakers' names. 
                                 For example if the 'sender' field reads 'Jane Doe :)' you can type 'Jane' or 'Jane Doe'."),

                                     textInput(inputId = "name1",
                                               label = "Type the name of person 1 "),
                                     textInput(inputId = "name2",
                                               label = "Type the name of person 2")),
                      mainPanel(tags$h2("Chat snapshot"),
                                tags$p("Once your data is uploaded it will appear here. If your table doesn't 
                                      start at the beginning of your chat you may need to re-export your chat from 
                                      WhatsApp, making sure you've loaded the whole chat on your device 
                                      before you export it. "),
                        DTOutput("raw_data")
                        )
                      )),
             
    # -------------- Visualisations -----------------------         

     tabPanel(title = "Visualise your chats",
              fluidPage(
              fluidRow(column(width = 6, 
                              plotOutput(outputId = "whatplot_time")),
                       column(width = 6,  plotOutput(outputId = "whatplot_wordle")),
                       fluidRow(column(width = 12, 
                                       plotOutput(outputId = "whatplot_words")
                                         )))))
    
    
    
    

    
    # ------------- app close brackets---------------
             

             )
)