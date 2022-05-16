#### Load libraries and set options
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(kableExtra)
library(tidyverse)
library(caret)
library(elasticnet)
library(leaps)
require(leaps)
require(plsRglm)
options(scipen = 999)


#### Load Models

#Models <- read_rds("https://raw.githubusercontent.com/SeanCranston/BVal/main/Models/Best_mds.RData")
Models <-   read_rds("https://raw.githubusercontent.com/SeanCranston/BVal/main/Models/Models_Best.RData")
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
                     uiOutput("Dat_CI"),
                     #dataTableOutput("movieID"),
                     hr(),
                     #plotOutput("distPlot")
                     uiOutput("Dat_PI")
                 )
             )
    ),
    
    navbarMenu("Behind the Scenes",
               tabPanel("PLS Regression"),
               tabPanel("Neural Network")
             # fluidPage(theme = shinytheme("flatly")),
             # # pageWithSidebar(
             # #     headerPanel('Input Data'),
             # #     
             # #     # sidebarPanel(
             # #     #     
             # #     # ),
             # #     # 
             # #     # mainPanel(
             # #     #     hr()
             # #     # )
             # # )
             # 
             # plotOutput(plot(mtcars$mpg,mtcars$disp))
    )
)


#############################################################################
server <- function(input, output) {
    

    output$Dat_CI <- renderUI({
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
        
        # start predictions
        pred <- list()
        pred <- lapply(Models, predict, observed) %>% 
            lapply(., round, digits = 2)
        
        #populate data frame with predictions an CI
        x = data.frame(
            Outcome = c("MVIC/EBIT", "MVIC/EBITDA", "MVIC", "MVIC/Net Sales"),
            Estimate = c(pred$EBIT,
                         pred$EBITDA,
                         paste0("$",format(pred$Price, big.mark = ",")), 
                         pred$Sales),
            Lower_Estimate = c(
                round(max(0,pred$EBIT-2*getTrainPerf(Models$EBIT)$TrainRMSE/sqrt(23)),digits = 2),
                round(max(0,pred$EBITDA-2*getTrainPerf(Models$EBITDA)$TrainRMSE/sqrt(23)),digits = 2),
                paste0("$",format(max(0,pred$Price-2*getTrainPerf(Models$Price)$TrainRMSE/sqrt(23)), big.mark = ",")),
                round(max(0,pred$Sales-2*getTrainPerf(Models$Sales)$TrainRMSE/sqrt(23)),digits = 2)
                
            ),
            Upper_Estimate = c(
                pred$EBIT+2*getTrainPerf(Models$EBIT)$TrainRMSE/sqrt(23),
                pred$EBITDA+2*getTrainPerf(Models$EBITDA)$TrainRMSE/sqrt(23),
                paste0("$",format(pred$Price+2*getTrainPerf(Models$Price)$TrainRMSE/sqrt(23), big.mark = ",")),
                pred$Sales+2*getTrainPerf(Models$Sales)$TrainRMSE/sqrt(23)
                
            ))
        
        x %>%
            knitr::kable(format = "html") %>%
            kable_styling("striped",full_width = T) %>%
            add_header_above(c(" " = 2, "95% Confident Interval (Bounded by 0)" = 2)) %>%
            HTML()
        
    })
    
    output$Dat_PI <- renderUI({
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
        
        # start predictions
        pred <- list()
        pred <- lapply(Models, predict, observed) %>% 
            lapply(., round, digits = 2)
        
        #populate data frame with predictions an CI
        x = data.frame(
            Outcome = c("MVIC/EBIT", "MVIC/EBITDA", "MVIC", "MVIC/Net Sales"),
            Estimate = c(pred$EBIT,
                         pred$EBITDA,
                         paste0("$",format(pred$Price, big.mark = ",")), 
                         pred$Sales),
            Lower_Estimate = c(
                round(max(0,pred$EBIT-2*getTrainPerf(Models$EBIT)$TrainRMSE),digits = 2),
                round(max(0,pred$EBITDA-2*getTrainPerf(Models$EBITDA)$TrainRMSE),digits = 2),
                paste0("$",format(max(0,pred$Price-2*getTrainPerf(Models$Price)$TrainRMSE), big.mark = ",")),
                round(max(0,pred$Sales-2*getTrainPerf(Models$Sales)$TrainRMSE),digits = 2)
                
            ),
            Upper_Estimate = c(
                pred$EBIT+2*getTrainPerf(Models$EBIT)$TrainRMSE,
                pred$EBITDA+2*getTrainPerf(Models$EBITDA)$TrainRMSE,
                paste0("$",format(pred$Price+2*getTrainPerf(Models$Price)$TrainRMSE, big.mark = ",")),
                pred$Sales+2*getTrainPerf(Models$Sales)$TrainRMSE

            ))

        x %>%
            knitr::kable(format = "html") %>%
            kable_styling("striped",full_width = T) %>%
            add_header_above(c(" " = 2, "95% Prediction Interval (Bounded by 0)" = 2)) %>%
            HTML()

    })


}

# Run the application 
shinyApp(ui = ui, server = server)
