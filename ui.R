library(shiny)
library(shinythemes)

#if (interactive()) {
  ui<-fluidPage(
    theme = shinytheme("sandstone"),
      tabsetPanel(
        id = "navbar",
          tabPanel(tags$h1("Validation Charts"),
              titlePanel(
                fluidRow( 
                  column(6, offset = 4, "View and download charts")
                  #column(4, offset = 8, img(height = 105, width = 300, src = "logo_pcfruit.jpg"))
                )),
              sidebarLayout( 
                sidebarPanel( actionButton(inputId = "deselectAll",label = "Deselect All"),
                              br(),br(),
                              tableOutput("results"),           
                              checkboxGroupInput("categoryInput", "Category", c("Alcoholic Beverages","Automotive","Durables and Electronics","Electronics","Financial","FMCG","Healthcare","Media","Retail","Services","Telecommunications")),
                              checkboxGroupInput("regionInput","Region",choices = c("Africa", "Asia", "Australia","Eastern Europe", "Japan","Latin America","Middle East","New Zealand","North America","Western Europe"))
          
                ),
              mainPanel(verbatimTextOutput("display1"),
                        verbatimTextOutput("display2"),
                        titlePanel("AE against Share"),
                        plotOutput("share_ae"),
                        titlePanel("EE(TE) against Share"),
                        plotOutput("share_ee"),
                        actionButton(inputId = "go",label = "Download Charts")
                        )
                )
       
          ),
        tabPanel(tags$h1("Validation Figures"),
                 br(),
                 titlePanel(
                   fluidRow( 
                     column(6, offset = 3, "View and download Figures")
                     #column(4, offset = 8, img(height = 105, width = 300, src = "logo_pcfruit.jpg"))
                   )),
                 sidebarLayout( 
                   sidebarPanel( 
                     tableOutput("results1"),
                     selectInput(inputId = "categoryInput1", label = "Category", choices = c("All","Alcoholic Beverages","Automotive","Durables and Electronics","Electronics","Financial","FMCG","Healthcare","Media","Retail","Services","Telecommunications"), selected = c("All"), multiple = FALSE),
                     selectInput(inputId = "subcategoryInput1",label = "subCategory",choices = "All",  multiple = FALSE),
                     selectInput(inputId ="regionInput1",label ="Region",choices = c("All","Africa", "Asia", "Australia","Eastern Europe", "Japan","Latin America","Middle East","New Zealand","North America","Western Europe"), selected = c("All"), multiple = FALSE),
                     selectInput(inputId = "countryInput1",label = "Country",choices = "All",  multiple = FALSE)

                   ),
                   mainPanel(
                     verbatimTextOutput("display3"),
                     verbatimTextOutput("display4"),
                     verbatimTextOutput("display5"),
                     verbatimTextOutput("display6"),
                     titlePanel("Correlation Table"),
                     tableOutput("tab_correl"),
                     #actionButton(inputId = "go1",label = "Download Table")
                     downloadButton('download', 'Download')

                   )
                 )
                 
        )
      )
    
  )
  
    
    

#}