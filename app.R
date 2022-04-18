#### Load librarys
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(caret)
options(scipen = 999)


#### Load Models

Models <- read_rds("Models/Best_mds.RData")

# Define UI for application that draws a histogram
ui <- navbarPage(
    
    # Application title
    "Business Valuation",
    
    # Sidebar with a slider input for number of bins 
    tabPanel("Data Entry",fluidPage(theme = shinytheme("flatly")),
             pageWithSidebar(
                 headerPanel('Input Data'),
                 sidebarPanel(width = 4,
                              
                              textInput('D1', 'Enter new ID',669), 
                              textInput("D2", "enter", 20)
                              # radioGroupButtons(inputId = 'NewVal',label = 'Rate the Movie',choices = 1:5),
                              # 
                              # actionButton("Previous","Previous"),
                              # actionButton("Next","Next"),
                              # hr(),
                              # actionButton("goButton", "Update Table")
                 ),
                 
                 mainPanel(
                     uiOutput("movieID"),
                     #dataTableOutput("movieID"),
                     hr(),
                     dataTableOutput("table")
                 )
             )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
        plotOutput("distPlot")
    )
)

# ui <- fluidPage(
# 
#     # Application title
#     titlePanel("Old Faithful Geyser Data"),
# 
#     # Sidebar with a slider input for number of bins 
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#            plotOutput("distPlot")
#         )
#     )
# )

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
