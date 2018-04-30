#Stock Analysis
#Statistics and Data Analysis - R Project
#Kevin Le & Katrina Francis
#APP

###IMPORT FILES###
library(shiny) #load shiny package

source('load_csvs.R') #call other R files, these contain functions for our app 
source('two_stock.R')
source('single_stock.R')

###USER INTERFACE###
ui <- navbarPage(title = "The Fried Eggs",
                 #NAVBAR TAB 1 - Stock Analyses
                 tabPanel("Stock Analyses", 
                          sidebarLayout(
                            #SIDEBAR
                            sidebarPanel(
                              #Select between analyzing one stock and analyzing two stocks
                              radioButtons("stocks", label = "One or Two Stock Analysis",
                                           choices = list("One" = 1, "Two" = 2)),
                              
                              #Dropdown menu for single stock
                              conditionalPanel("input.stocks == 1", 
                                               selectizeInput(inputId = "dataset",
                                                              label = "Choose a dataset:",
                                                              choices = list(`Stock Market Indices` = c("S&P 500 (SPX)", "Dow-Jones Industrial Average (DJI)", "NASDAQ (NDQ)"),
                                                                             `Industry Indices/ETFs` = c("Tech Industry (IYW)", "Financial Services (IYF)", "Natural Resources (MXI)", "Consumer Staples (XLP)", "Utilities (XLU)", "Dow Jones Utilities Average (DJU)"),
                                                                             `Publicly Traded Sports Companies` = c("Nike (NKE)", "Dick's Sporting Goods (DKS)", "Footlocker (FL)", "Lululemon (LULU)", "Underarmor (UAA)")), 
                                                              selected = "S&P 500 (SPX)")
                              ),
                              
                              #Two item text input, automatically reveals dropdown menus when the user types
                              conditionalPanel("input.stocks == 2",
                                               selectizeInput(inputId = "datasets",
                                                              label = "Choose two datasets:",
                                                              choices = list(`Stock Market Indices` = c("S&P 500 (SPX)", "Dow-Jones Industrial Average (DJI)", "NASDAQ (NDQ)"),
                                                                             `Industry Indices/ETFs` = c("Tech Industry (IYW)", "Financial Services (IYF)", "Natural Resources (MXI)", "Consumer Staples (XLP)", "Utilities (XLU)", "Dow Jones Utilities Average (DJU)"),
                                                                             `Publicly Traded Sports Companies` = c("Nike (NKE)", "Dick's Sporting Goods (DKS)", "Footlocker (FL)", "Lululemon (LULU)", "Underarmor (UAA)")), 
                                                              multiple = TRUE,
                                                              selected = c("S&P 500 (SPX)", "Dow-Jones Industrial Average (DJI)"),
                                                              options = list(maxItems = 2)
                                               )
                              ),
                              
                              #Select between checking yearly log returns and checking daily log returns
                              radioButtons(inputId = "yearly", label = "Time Interval for Returns",
                                           choices = list("Daily" = FALSE, "Yearly" = TRUE),
                                           selected = TRUE),
                              
                              #Bin number slider input for single stock
                              conditionalPanel('input.stocks == 1 && input.tab_selected != 3', 
                                               sliderInput(inputId = "bins",
                                                           label = "Number of bins:",
                                                           min = 1,
                                                           max = 50,
                                                           value = 30)
                              ),
                              
                              #Two bin number slider input for two stocks
                              conditionalPanel('input.stocks == 2 && input.tab_selected != 3 && !input.dcor',
                                               sliderInput(inputId = "bins1",
                                                           label = "Number of bins for stock 1:",
                                                           min = 1,
                                                           max = 50,
                                                           value = 30),
                                               sliderInput(inputId = "bins2",
                                                           label = "Number of bins for stock 2:",
                                                           min = 1,
                                                           max = 50,
                                                           value = 30)
                              ),
                              
                              #Minimum bin count for hypothesis testing
                              conditionalPanel('!input.dcor && input.tab_selected == 2', 
                                               sliderInput(inputId = "min_bin_count",
                                                           label = "Minimum bin count",
                                                           min = 2,
                                                           max = 5,
                                                           value = 3)),
                              
                              #Chi-squared test for independence view selection
                              conditionalPanel('input.stocks == 2 && !input.dcor && input.tab_selected == 2', 
                                               checkboxInput(inputId = 'showCT', 
                                                             label = 'Show contingency table for the chi-squared test for independence', 
                                                             value = FALSE)),
                              
                              #Distance correlation test for independence view selection
                              conditionalPanel('input.stocks == 2 && input.tab_selected == 2',
                                               checkboxInput(inputId = 'dcor', 
                                                             label = 'Perform a distance correlation test for independence (not recommended for daily data)', 
                                                             value = FALSE)),
                              
                              #If distance correlation test is selected, then this is a required parameter
                              conditionalPanel('input.dcor && input.tab_selected == 2',
                                               sliderInput(inputId = 'replicates',
                                                           label = "Replicates to perform for distance correlation test (Higher values will give a more precise p-value)",
                                                           min = 5,
                                                           max = 300,
                                                           value = 100)),
                              
                              #Select significance level
                              conditionalPanel('input.tab_selected != 1',
                                               sliderInput(inputId = "sig",
                                                           label = "Significance level of confidence intervals",
                                                           min = 0.005,
                                                           max = 0.10,
                                                           value = 0.05)),
                              
                              #Select test type (two-sided, upper bound, lower bound) for confidence intervals
                              conditionalPanel('input.stocks == 1 && input.tab_selected == 2',
                                               radioButtons(inputId = "test_choice", 
                                                            label = "Select test type",
                                                            choices = c("Two-sided", "Upper-bound", "Lower-bound")))
                              
                            ),
                            
                            #MAIN PANEL 
                            mainPanel(
                              tabsetPanel(
                                #CHARTS
                                tabPanel('Charts', value = 1, 
                                         #Display histogram and normality plot for single stock
                                         conditionalPanel("input.stocks == 1",
                                                          plotOutput("histPlot"),
                                                          plotOutput("normPlot")
                                         ),
                                         #Display histograms for two stock
                                         conditionalPanel("input.stocks == 2",
                                                          plotOutput("histPlot1"), plotOutput("histPlot2")
                                         )
                                ),
                                #HYPOTHESIS TESTS
                                tabPanel('Hypothesis Tests', value = 2,
                                         #Display goodness of fit and confidence intervals of mean and variance for single stock
                                         conditionalPanel("input.stocks == 1",
                                                          verbatimTextOutput("goodnessFit"),
                                                          verbatimTextOutput("confidenceIntMean"),
                                                          verbatimTextOutput("confidenceIntVar")
                                         ),
                                         #Display tests for paired t-test mean, independence, and histograms
                                         conditionalPanel("input.stocks == 2",
                                                          verbatimTextOutput("testMeans"),
                                                          conditionalPanel('!input.dcor',verbatimTextOutput("testIndependence")),
                                                          conditionalPanel('!input.dcor',plotOutput("histPlot1_1"), plotOutput("histPlot2_1")),
                                                          conditionalPanel('input.dcor',verbatimTextOutput("advtestIndependence"))
                                         )
                                ),
                                #REGRESSION
                                tabPanel('Regression', value = 3,
                                         conditionalPanel("input.stocks == 1",
                                                          #View regression summary, regression plot, and residual plot for single stock
                                                          verbatimTextOutput("oneSampleRegressionSummary"),
                                                          plotOutput("oneSampleLinearRegression"),
                                                          plotOutput("oneSampleResidualRegression")
                                         ),
                                         conditionalPanel("input.stocks == 2",
                                                          #View regression summar, regression plot, and residual plot for two stock
                                                          verbatimTextOutput("twoSampleRegressionSummary"),
                                                          plotOutput("twoSampleRegressionPlot"),
                                                          plotOutput("twoSampleResidualPlot")
                                         )
                                ),
                                id = "tab_selected"
                              )
                            )
                          )
                 ),
                 
                 #NAVBAR TAB 2 - Sorts Analysis
                 tabPanel('Sports Analysis',
                          sidebarLayout(sidebarPanel(selectizeInput(inputId = "sbdataset",
                                                                    #Select single stock
                                                                    label = "Choose a dataset:",
                                                                    choices = list(`Stock Market Indices` = c("S&P 500 (SPX)", "Dow-Jones Industrial Average (DJI)", "NASDAQ (NDQ)"),
                                                                                   `Industry Indices/ETFs` = c("Tech Industry (IYW)", "Financial Services (IYF)", "Natural Resources (MXI)", "Consumer Staples (XLP)", "Utilities (XLU)", "Dow Jones Utilities Average (DJU)"),
                                                                                   `Publicly Traded Sports Companies` = c("Nike (NKE)", "Dick's Sporting Goods (DKS)", "Footlocker (FL)", "Lululemon (LULU)", "Underarmor (UAA)")), 
                                                                    selected = "S&P 500 (SPX)"),
                                                     
                                                     conditionalPanel('input.sb_tab_selected == 1',
                                                                      #Select bin count
                                                                      sliderInput(inputId = "sbbins",
                                                                                  label = "Number of histogram bins:",
                                                                                  min = 1,
                                                                                  max = 15,
                                                                                  value = 5)),
                                                     
                                                     sliderInput(inputId = "sbconf",
                                                                 #Select confidence level
                                                                 label = "Confidence level",
                                                                 min = 90,
                                                                 max = 100,
                                                                 value = 95)
                          ),
                          
                          mainPanel(tabsetPanel(
                            tabPanel(title = 'General', value = 1,
                                     #In general, show independence test and overlaid histogram
                                     verbatimTextOutput("sbtestIndependence"),
                                     plotOutput("sbHist")),
                            tabPanel(title = "Regression", value = 2,
                                     #Show overlaid regression information
                                     verbatimTextOutput("sbRegressionSummary"),
                                     plotOutput("sbRegressionPlot"),
                                     plotOutput("sbResidualPlot")),
                            id = 'sb_tab_selected')))),
                 
                 tags$head(tags$style(HTML("pre { word-break: normal; white-space: pre-wrap; }")))
                 
)

###DATA PRE-PROCESSING

#Read data from csv -- calls load_csvs.R
dic<-list("S&P 500 (SPX)" = spx, "Dow-Jones Industrial Average (DJI)" = dji, "NASDAQ (NDQ)" = ndq, "Tech Industry (IYW)" = iyw, "Financial Services (IYF)" = iyf, "Natural Resources (MXI)" = mxi, "Consumer Staples (XLP)" = xlp, "Utilities (XLU)" = xlu, "Dow Jones Utilities Average (DJU)" = dju, "Dick's Sporting Goods (DKS)" = dks, "Footlocker (FL)" = fl, "Lululemon (LULU)" = lulu, "Nike (NKE)" = nke, "Underarmor (UAA)" = uaa)

###SERVER LOGIC###
server <- function(input, output, session){
  #Data selection for single stock
  datasetInput <- reactive({
    dataset<-switch(input$dataset,
           "S&P 500 (SPX)" = spx,
           "Dow-Jones Industrial Average (DJI)" = dji,
           "NASDAQ (NDQ)" = ndq,
           "Tech Industry (IYW)" = iyw,
           "Financial Services (IYF)" = iyf,
           "Natural Resources (MXI)" = mxi,
           "Consumer Staples (XLP)" = xlp,
           "Utilities (XLU)" = xlu,
           "Dow Jones Utilities Average (DJU)" = dju,
           "Dick's Sporting Goods (DKS)" = dks,
           "Footlocker (FL)" = fl,
           "Lululemon (LULU)" = lulu,
           "Nike (NKE)" = nke,
           "Underarmor (UAA)" = uaa)
    if(input$yearly) dataset<-get_yearly(dataset)
    dataset$Returns <- log_return(dataset)
    return(dataset)
    })
  
  #Data selection for Super Bowl (Sports Analysis)
  sbdatasetInput <- reactive({
    dataset<-switch(input$sbdataset,
                    "S&P 500 (SPX)" = spx,
                    "Dow-Jones Industrial Average (DJI)" = dji,
                    "NASDAQ (NDQ)" = ndq,
                    "Tech Industry (IYW)" = iyw,
                    "Financial Services (IYF)" = iyf,
                    "Natural Resources (MXI)" = mxi,
                    "Consumer Staples (XLP)" = xlp,
                    "Utilities (XLU)" = xlu,
                    "Dow Jones Utilities Average (DJU)" = dju,
                    "Dick's Sporting Goods (DKS)" = dks,
                    "Footlocker (FL)" = fl,
                    "Lululemon (LULU)" = lulu,
                    "Nike (NKE)" = nke,
                    "Underarmor (UAA)" = uaa)
    dataset<-get_yearly(dataset)
    dataset$Returns <- log_return(dataset)
    return(sb_join(dataset))
  })

  #Data selection for two stocks
  datasetsInput <- reactive({
    dataset1 = dic[[input$datasets[1]]]
    dataset2 = dic[[input$datasets[2]]]
    if(input$yearly){
      dataset1 = get_yearly(dataset1)
      dataset2 = get_yearly(dataset2)
      }
    dataset1$Returns <- log_return(dataset1)
    dataset2$Returns <- log_return(dataset2)
    dataset = join_samples(dataset1, dataset2, input$yearly)
    return(dataset)
    })
  
  ###SINGLE STOCK###
  
  #Histograms
  output$histPlot <- renderPlot({
    dataset <- datasetInput()
    x <- dataset$Returns
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    if(input$tab_selected == 2) bins = bin_data(x, input$bins, input$min_bin_count)$breaks
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = paste("Log returns for",input$dataset),
         main = paste("Histogram for log returns of",input$dataset))
    
  })
  
  #Normal probability plot
  output$normPlot <- renderPlot({
    dataset <- datasetInput()
    qqnorm(dataset$Returns, col = "#75AADB",
           main = "Normality plot for log returns in selected stock data")
  })
  
  #Goodness of fit for normal distribution -- calls single_stock.R
  output$goodnessFit <- renderPrint({
    dataset <- datasetInput()
    if(input$tab_selected == 2){ #only appears for HYPOTHESIS TESTS tab
      results <- goodness_of_fit(dataset, input$bins, input$min_bin_count, input$sig)
    }
  })
  
  #Confidence interval of mean -- calls single_stock.R
  output$confidenceIntMean <- renderPrint({
    dataset <- datasetInput()
    results <- confidence_interval_mean(dataset, input$test_choice, input$sig)
  })
  
  #Confidence interval of variance -- calls single_stock.R
  output$confidenceIntVar <- renderPrint({
    dataset <- datasetInput()
    results <- confidence_interval_var(dataset, input$test_choice, input$sig)
  })
  
  #One stock linear regression plot with least squares regression line drawn
  output$oneSampleLinearRegression <- renderPlot({
    dataset <- datasetInput()
    dataset$Returns <- log_return(dataset)
    y <- dataset$Returns
    length(y) -> n
    x <- c(1:n)
    reg <- lm(y ~ x, data=dataset)
    plot(reg$model[,2], reg$model[,1],
         main = 'Regression data with least-squares regression line',
         xlab = 'Time',
         ylab = input$dataset)
    abline(reg, lty=1, lwd=2)
  })
  
  #One stock linear residual plot
  output$oneSampleResidualRegression <- renderPlot({
    dataset <- datasetInput()
    dataset$Returns <- log_return(dataset)
    y <- dataset$Returns
    length(y) -> n
    x <- c(1:n)
    reg <- lm(y ~ x, data=dataset)
    plot(reg$model[,2], reg$residuals,
         main = 'Residual Plot for Linear Regression',
         xlab = 'Time',
         ylab =  'Regression Residuals')
    abline(h = 0)
  })
  
  #One stock linear regression summary -- includes slope, intercept, R^2, confidence interval for slope, and confidence interval for intercept
  output$oneSampleRegressionSummary <- renderPrint({
    dataset <- datasetInput()
    dataset$Returns <- log_return(dataset)
    y <- dataset$Returns
    length(y) -> n
    x <- c(1:n)
    reg <- lm(y ~ x, data=dataset)
    summary(reg)$r.squared ->r2
    confint(reg, level = (1-input$sig)) -> conf
    cat(paste('Least Squares Regression Formula: y =',signif(reg$coefficients[2],3), 'x +',signif(reg$coefficients[1],3),
              '\nR-squared:',signif(r2,3),'\nwhere y represents',input$datasets[1],'and x represents',input$datasets[2],
              paste('\n\n',100*(1-input$sig),'% Confidence Intervals:\nSlope: [',signif(conf[2,1],3),', ',signif(conf[2,2],3),']\nIntercept: [',signif(conf[1,1],3),', ',signif(conf[1,2],3),']',sep = "")))
  })
  
  ###TWO STOCK###
  
  #Bin first stock data
  bin_me_1 <- reactive({
    x = datasetsInput()$Returns.x
    if(input$tab_selected == 2) bins = bin_data(x, input$bins1, input$min_bin_count)$breaks #calls two_stock.R
    else bins = seq(min(x), max(x), length.out = input$bins1 + 1)
    return(bins)
  })
  
  #Bin second stock data -- calls two_stock.R
  bin_me_2 <- reactive({
    x = datasetsInput()$Returns.y
    if(input$tab_selected == 2) bins = bin_data(x, input$bins2, input$min_bin_count)$breaks #calls two_stock.R
    else bins = seq(min(x), max(x), length.out = input$bins2 + 1)
    return(bins)
  })
  
  #Histogram of log returns for first stock in CHARTS tab
  output$histPlot1 <- renderPlot({
    dataset <- datasetsInput()
    x <- dataset$Returns.x
    bins = bin_me_1()
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = paste("Log returns for",input$datasets[1]),
         main = paste("Histogram for log returns of",input$datasets[1]))
    
  })
  
  #Histogram of log returns for second stock in CHARTS tab
  output$histPlot2 <- renderPlot({
    dataset <- datasetsInput()
    x <- dataset$Returns.y
    bins <- seq(min(x), max(x), length.out = input$bins2 + 1)
    if(input$tab_selected == 2) bins = bin_data(x, input$bins2, input$min_bin_count)$breaks #calls two_stock.R
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = paste("Log returns for",input$datasets[2]),
         main = paste("Histogram for log returns of",input$datasets[2]))
    
  })
  
  #Histogram of log returns for first stock in HYPOTHESIS TESTS tab
  output$histPlot1_1 <- renderPlot({
    dataset <- datasetsInput()
    x <- dataset$Returns.x
    bins = bin_me_1()
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = paste("Log returns for",input$datasets[1]),
         main = paste("Histogram for log returns of",input$datasets[1]))
    
  })
  
  #Histogram of log returns for second stock in HYPOTHESIS TESTS tab
  output$histPlot2_1 <- renderPlot({
    dataset <- datasetsInput()
    x <- dataset$Returns.y
    bins = bins = bin_me_2()
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = paste("Log returns for",input$datasets[2]),
         main = paste("Histogram for log returns of",input$datasets[2]))
    
  })
  
  #Test difference eof means 
  output$testMeans <- renderPrint({
    d<-datasetsInput()
    test_means(d$Returns.x, d$Returns.y, input$sig) #calls two_stock.R
  })
  
  #Test independence (chi-squared contingency)
  output$testIndependence <- renderPrint({
    d<-datasetsInput()
    breaks1 = bin_me_1()
    breaks2 = bin_me_2()
    test_independence(d$Returns.x, d$Returns.y, #calls two_stock.R
                      breaks1 = breaks1, breaks2 = breaks2, 
                      showCT = input$showCT, alpha = input$sig)
  })
  
  #Advanced test for independence (distance correlation)
  output$advtestIndependence <- renderPrint({
    d<-datasetsInput()
    adv_test_independence(d$Returns.x, d$Returns.y, replicates = input$replicates, alpha = input$sig)
  })

  #Store dataset input for two stock regression
  reactive({
    d <- datasetsInput()
    lm(d$Returns.y ~ d$Returns.x) -> lm
    return(lm)
    }) -> tsreg
  
  #Output two sample regression summary
  output$twoSampleRegressionSummary <- renderPrint({
    tsreg()->reg #calls two_stock.R
    summary(reg)$r.squared ->r2
    confint(reg, level = (1-input$sig)) -> conf
    cat(paste('Least Squares Regression Formula: y =',signif(reg$coefficients[2],3), 'x +',signif(reg$coefficients[1],3),
              '\nR-squared:',signif(r2,3),'\nwhere y represents',input$datasets[1],'and x represents',input$datasets[2],
              paste('\n\n',100*(1-input$sig),'% Confidence Intervals:\nSlope: [',signif(conf[2,1],3),', ',signif(conf[2,2],3),']\nIntercept: [',signif(conf[1,1],3),', ',signif(conf[1,2],3),']',sep = "")))
  })
  
  #Output two sample regression plot
  output$twoSampleRegressionPlot <-renderPlot({
    tsreg()->reg #calls two_stock.R
    plot(reg$model[,2], reg$model[,1],
         main = 'Regression data with least-squares regression line',
         xlab = input$datasets[1],
         ylab = input$datasets[2])
    abline(reg, lty=1, lwd=2)
  })
  
  #Output two sample residual plot
  output$twoSampleResidualPlot <- renderPlot({
    tsreg()->reg #calls two_stock.R
    plot(reg$model[,2],reg$residuals,
         main = 'Residual Plots for Linear Regression',
         xlab = input$datasets[1],
         ylab = 'Regression Residuals')
    abline(h = 0)
  })
  
  ###SUPER BOWL SPORTS ANALYSIS###
  
  #Test for independence between stock and super bowl results with chi-squared contingency table
  output$sbtestIndependence <- renderPrint({
    d<-sbdatasetInput()
    sb_test_independence(d, conf = input$sbconf) #calls two_stock.R
  })
  
  #Create overlaid histogram of stock data and super bowl results
  output$sbHist <- renderPlot({
    d<-sbdatasetInput()
    xmin = min(d[,"Returns"])
    xmax = max(d[,"Returns"])
    a = d[d$Winning.Conference == 'AFL','Returns']
    n = d[d$Winning.Conference == 'NFL','Returns']
    bins = seq(xmin, xmax, length.out = input$sbbins + 1)
    hist(d[,"Returns"], breaks = bins, plot = F)->h
    hist(a,  breaks = bins, col=rgb(0,0,1,1/4), ylim = c(0,max(h$counts)),
         xlab = paste("Annual Log Returns for",input$sbdataset),
         main = "Annual Log Returns by League of Super Bowl Victor")
    hist(n, breaks = bins, col=rgb(1,0,0,1/4), add = T)
    legend('topleft',c('NFL Super Bowl Victory','AFL Super Bowl Victory'),
           fill = rgb(1:0,0,0:1,0.4), bty = 'n',
           border = NA)
  })
  
  #Input for regression between super bowl wins and selected stock
  sbreg = reactive({
    d = sbdatasetInput()
    dummy_bool = grepl('NFL', d$Winning.Conference, fixed = TRUE) #dummy variable 
    diff = d$Diff
    lm(d$Returns ~ diff + dummy_bool) -> lm
    return(lm)
  })
  
  #Regression summary of super bowl wins and selected stock
  output$sbRegressionSummary <- renderPrint({
    sbreg()->reg #calls two_stock.R
    summary(reg)$r.squared ->r2
    confint(reg, level = (input$sbconf/100)) -> conf
    cat(paste('Least Squares Regression Formula: y =',signif(reg$coefficients[2],3), 'x_1 +',signif(reg$coefficients[3],3),'x_2 +',signif(reg$coefficients[1],3),
              '\nR-squared:',signif(r2,3)),'\nWhere x_1 is the point difference in the Super Bowl and x_2 is 1 if an NFL team won, or 0 if an AFL team won.',
        paste('\n\n',input$sbconf,'% Confidence Intervals:\nSlope for Point Difference: [',signif(conf[2,1],3),', ',signif(conf[2,2],3),
              ']\nSlope for NFL Indicator: [', signif(conf[3,1],3),', ',signif(conf[3,2],3),
              ']\nIntercept: [',signif(conf[1,1],3),', ',signif(conf[1,2],3),']',sep = ""))
  })
  
  #Regression plot of super bowl wins and selected stock
  output$sbRegressionPlot <-renderPlot({
    sbreg()->reg #calls two_stock.R
    plot(reg$model[reg$model$dummy_bool,2], reg$model[reg$model$dummy_bool,1],
         col = rgb(1,0,0,1),
         main = 'Regression data with least-squares regression line',
         xlab = "Point Difference in Super Bowl",
         ylab = "Annual Log Returns of Selected Index/Stock")
    points(reg$model[!(reg$model$dummy_bool),2], reg$model[!(reg$model$dummy_bool),1], col = rgb(0,0,1,1))
    abline(a = (reg$coefficients[1] + reg$coefficients[3]), b = reg$coefficients[2], lty=1, lwd=2, col = rgb(1,0.25, 0.25, 1))
    abline(a = reg$coefficients[1], b = reg$coefficients[2], lty=1, lwd=2, col = rgb(0.25,0.25, 1, 1))
    legend('bottomright',c('NFL Super Bowl Victory','AFL Super Bowl Victory'),
           fill = rgb(1:0,0,0:1,0.4), bty = 'n',
           border = NA)
    abline(h=0)
  })
  
  #Residual plot of super bowl wins and selected stock
  output$sbResidualPlot <- renderPlot({
    sbreg()->reg #calls two_stock.R
    plot(reg$model[reg$model$dummy_bool,2],reg$residuals[reg$model$dummy_bool],
         main = 'Residual Plots for Linear Regression',
         xlab = "Point Difference in Super Bowl",
         ylab = 'Regression Residuals', col = rgb(1,0,0,1))
    points(reg$model[!(reg$model$dummy_bool),2], reg$residuals[!(reg$model$dummy_bool)], col = rgb(0,0,1,1))
    abline(h=0)
    legend('bottomright',c('NFL Super Bowl Victory','AFL Super Bowl Victory'),
           fill = rgb(1:0,0,0:1,0.4), bty = 'n',
           border = NA)
  })
  
}

  

shinyApp(ui, server)






