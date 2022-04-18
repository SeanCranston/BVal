#### Load libraries and set options
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(kableExtra)
library(tidyverse)
library(caret)
options(scipen = 999)


#### Load Models

Models <- read_rds("https://raw.githubusercontent.com/SeanCranston/BVal/main/Models/Best_mds.RData")
#load(mtcars)

#############################################################################
ui <- navbarPage(
    
    # Application title
    "Business Valuation",
    
    # Sidebar with a slider input for number of bins 
    tabPanel("Best Models",fluidPage(theme = shinytheme("flatly")),
             pageWithSidebar(
                 headerPanel('Input Data'),
                 sidebarPanel(width = 4,
                              
                              # sliderInput("mpg", "mpg Limit",
                              #             min = 11, max = 33, value = 20),
                              numericInput('Target.Age', 
                                           'Age',11),
                              numericInput('Target.Employee.Count', 
                                           'Employee Count',8),
                              numericInput('Net.Sales', 
                                           'Net Sales',517768),
                              numericInput('Cost.of.Goods.Sold', 
                                           'COGS',173088),
                              numericInput('Rent', 
                                           'Rent',56400),
                              numericInput('Total.Operating.Expenses', 
                                           'Total Operating Expenses',299015),
                              numericInput('Interest.Expense', 
                                           'Interest Expense',0),
                              numericInput('Inventory.PPA', 
                                           'Inventory PPA',7935),
                              numericInput("Fixed.Assets.PPA", 
                                           "Fixed Assets PPA", 120000),
                              numericInput('Total.Intangibles.PPA', 
                                           'Total Intangibles PPA',101778),
                              numericInput('Sales.Per.Square.Foot', 
                                           'Sales Per Square Foot',226.84),
                              numericInput('Gross.Profit.Margin.before.Operating.and.Net', 
                                           'Gross Profit Margin before Operating and Net',0.6797),
                              numericInput('Operating.Profit.Margin', 
                                           'Operating Profit Margin',0.16015),
                              numericInput('Net.Profit.Margin.after.Gross.and.Operating', 
                                           'Net Profit Margin after Gross and Operating',0.1504),
                              numericInput('Sale.Date', 
                                           'Year',2022)
                 ),
                 
                 mainPanel(
                     uiOutput("Dat"),
                     #dataTableOutput("movieID"),
                     hr(),
                     #plotOutput("distPlot")
                 )
             )
    )
    
    # # Show a plot of the generated distribution
    # mainPanel(
    #     plotOutput("distPlot")
    # )
)


#############################################################################
server <- function(input, output) {
    
    ##### this outputs the a row from movie_data that is to be rated
    #values <- reactiveValues(dum = data.frame(x="test"))
    
    output$Dat <- renderUI({
        observed <- 
            data.frame(
                Target.Age = input$Target.Age,                      
                Target.Employee.Count = input$Target.Employee.Count,
                Net.Sales = input$Net.Sales,
                Cost.of.Goods.Sold = input$Cost.of.Goods.Sold,                 
                Rent = input$Rent,
                Total.Operating.Expenses = input$Total.Operating.Expenses,                   
                Interest.Expense = input$Interest.Expense,
                Inventory.PPA = input$Inventory.PPA,  
                Fixed.Assets.PPA = input$Fixed.Assets.PPA,        
                Total.Intangibles.PPA = input$Total.Intangibles.PPA,  
                Sales.Per.Square.Foot = input$Sales.Per.Square.Foot,
                Gross.Profit.Margin.before.Operating.and.Net = input$Gross.Profit.Margin.before.Operating.and.Net,
                Operating.Profit.Margin = input$Operating.Profit.Margin,
                Net.Profit.Margin.after.Gross.and.Operating = input$Net.Profit.Margin.after.Gross.and.Operating ,                     
                Sale.Date = input$Sale.Date
            )
        
        observed %>% 
            knitr::kable(format = "html") %>%
            kable_styling("striped",full_width = T) %>%
            #add_header_above(c(" " = 2, "95% Confident Interval" = 2)) %>%
            HTML()
        
        pred <- list()
        pred[["Best_Price"]] <- predict(object = Models$Best_Price, newdata = observed)
        # pred$Best_Sales <- predict(Models$Best_Sales, newdata = observed)
        # pred$Best_EBIT <- predict(Models$Best_EBIT, newdata = observed)
        # pred$Best_EBITDA <- predict(Models$Best_EBITDA, newdata = observed)
        #pred <- lapply(Models, predict, observed)
        # 
        # x = data.frame(
        #     Outcome = c("Price", "MVIC/Sales", "MVIC/EBIT","MVIC/EBITDA"),
        #     Estimate = c(pred$Best_Price,pred$Best_Sales,
        #                  pred$Best_EBIT, pred$Best_EBITDA),
        #     Lower_Estimate = NA,
        #     Upper_Estimate = NA)
        # 
        # x %>% 
        #     knitr::kable(format = "html") %>%
        #     kable_styling("striped",full_width = T) %>%
        #     add_header_above(c(" " = 2, "95% Confident Interval" = 2)) %>%
        #     HTML()

    })


}

# Run the application 
shinyApp(ui = ui, server = server)
