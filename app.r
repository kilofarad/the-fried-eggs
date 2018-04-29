#Stock Analysis
#Statistics and Data Analysis - R Project
#runapp.r
#library(shiny)
#setwd(“C:/Users/weloveton/Documents/stockhistograms”)
#runApp(host = “0.0.0.0”, port = 5050)
#
#app.r

library(shiny) #load shiny package

source('load_csvs.R')
source('two_stock.R')

###USER INTERFACE###
ui <- navbarPage(title = "The Fried Eggs",
                 tabPanel("Stock Analyses",
                          sidebarLayout(
                            #SIDEBAR
                            sidebarPanel(
                              radioButtons("stocks", label = "One or Two Stock Analysis",
                                           choices = list("One" = 1, "Two" = 2)),
                              
                              #Select dataset
                              conditionalPanel("input.stocks == 1", 
                                               selectizeInput(inputId = "dataset",
                                                              label = "Choose a dataset:",
                                                              choices = list(`Stock Market Indices` = c("S&P 500 (SPX)", "Dow-Jones Industrial Average (DJI)", "NASDAQ (NDQ)"),
                                                                             `Industry Indices/ETFs` = c("Tech Industry (IYW)", "Financial Services (IYF)", "Natural Resources (MXI)", "Consumer Staples (XLP)", "Utilities (XLU)", "Dow Jones Utilities Average (DJU)"),
                                                                             `Publicly Traded Sports Companies` = c("Nike (NKE)", "Dick's Sporting Goods (DKS)", "Footlocker (FL)", "Lululemon (LULU)", "Underarmor (UAA)")), 
                                                              selected = "S&P 500 (SPX)")
                              ),
                              
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
                              
                              radioButtons(inputId = "yearly", label = "Time Interval for Returns",
                                           choices = list("Daily" = FALSE, "Yearly" = TRUE),
                                           selected = TRUE),
                              
                              conditionalPanel('input.stocks == 1 && input.tab_selected != 3', 
                                               sliderInput(inputId = "bins",
                                                           label = "Number of bins:",
                                                           min = 1,
                                                           max = 50,
                                                           value = 30)
                              ),
                              
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
                              
                              conditionalPanel('!input.dcor && input.tab_selected == 2', 
                                               sliderInput(inputId = "min_bin_count",
                                                           label = "Minimum bin count",
                                                           min = 2,
                                                           max = 5,
                                                           value = 3)),
                              
                              
                              conditionalPanel('input.stocks == 2 && !input.dcor && input.tab_selected == 2', 
                                               checkboxInput(inputId = 'showCT', 
                                                             label = 'Show contingency table for the chi-squared test for independence', 
                                                             value = FALSE)),
                              
                              conditionalPanel('input.stocks == 2 && input.tab_selected == 2',
                                               checkboxInput(inputId = 'dcor', 
                                                             label = 'Perform a distance correlation test for independence (not recommended for daily data)', 
                                                             value = FALSE)),
                              
                              conditionalPanel('input.dcor && input.tab_selected == 2',
                                               sliderInput(inputId = 'replicates',
                                                           label = "Replicates to perform for distance correlation test (Higher values will give a more precise p-value)",
                                                           min = 5,
                                                           max = 300,
                                                           value = 100)),
                              
                              conditionalPanel('input.tab_selected != 1',
                                               sliderInput(inputId = "sig",
                                                           label = "Significance level of confidence intervals",
                                                           min = 0.005,
                                                           max = 0.10,
                                                           value = 0.05)),
                              
                              conditionalPanel('input.stocks == 1 && input.tab_selected == 2',
                                               radioButtons(inputId = "test_choice", 
                                                            label = "Select test type",
                                                            choices = c("Two-sided", "Upper-bound", "Lower-bound")))
                              
                            ),
                            
                            #MAIN PANEL
                            mainPanel(
                              tabsetPanel(
                                tabPanel('Charts', value = 1,
                                         conditionalPanel("input.stocks == 1",
                                                          plotOutput("histPlot"),
                                                          plotOutput("normPlot")
                                         ),
                                         conditionalPanel("input.stocks == 2",
                                                          plotOutput("histPlot1"), plotOutput("histPlot2")
                                         )
                                ),
                                tabPanel('Hypothesis Tests', value = 2,
                                         conditionalPanel("input.stocks == 1",
                                                          verbatimTextOutput("goodnessFit"),
                                                          verbatimTextOutput("confidenceIntMean"),
                                                          verbatimTextOutput("confidenceIntVar")
                                         ),
                                         conditionalPanel("input.stocks == 2",
                                                          verbatimTextOutput("testMeans"),
                                                          conditionalPanel('!input.dcor',verbatimTextOutput("testIndependence")),
                                                          conditionalPanel('!input.dcor',plotOutput("histPlot1_1"), plotOutput("histPlot2_1")),
                                                          conditionalPanel('input.dcor',verbatimTextOutput("advtestIndependence"))
                                         )
                                ),
                                tabPanel('Regression', value = 3,
                                         conditionalPanel("input.stocks == 1",
                                                          verbatimTextOutput("oneSampleRegressionSummary"),
                                                          plotOutput("oneSampleLinearRegression"),
                                                          plotOutput("oneSampleResidualRegression")
                                         ),
                                         conditionalPanel("input.stocks == 2",
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
                 tabPanel('Sports Analysis',
                          sidebarLayout(sidebarPanel(selectizeInput(inputId = "sbdataset",
                                                                    label = "Choose a dataset:",
                                                                    choices = list(`Stock Market Indices` = c("S&P 500 (SPX)", "Dow-Jones Industrial Average (DJI)", "NASDAQ (NDQ)"),
                                                                                   `Industry Indices/ETFs` = c("Tech Industry (IYW)", "Financial Services (IYF)", "Natural Resources (MXI)", "Consumer Staples (XLP)", "Utilities (XLU)", "Dow Jones Utilities Average (DJU)"),
                                                                                   `Publicly Traded Sports Companies` = c("Nike (NKE)", "Dick's Sporting Goods (DKS)", "Footlocker (FL)", "Lululemon (LULU)", "Underarmor (UAA)")), 
                                                                    selected = "S&P 500 (SPX)"),
                                                     
                                                     conditionalPanel('input.sb_tab_selected == 1',
                                                                      sliderInput(inputId = "sbbins",
                                                                                  label = "Number of histogram bins:",
                                                                                  min = 1,
                                                                                  max = 15,
                                                                                  value = 5)),
                                                     
                                                     sliderInput(inputId = "sbconf",
                                                                 label = "Confidence level",
                                                                 min = 90,
                                                                 max = 100,
                                                                 value = 95)
                          ),
                          
                          mainPanel(tabsetPanel(
                            tabPanel(title = 'General', value = 1,
                                     verbatimTextOutput("sbtestIndependence"),
                                     plotOutput("sbHist")),
                            tabPanel(title = "Regression", value = 2,
                                     verbatimTextOutput("sbRegressionSummary"),
                                     plotOutput("sbRegressionPlot"),
                                     plotOutput("sbResidualPlot")),
                            id = 'sb_tab_selected')))),
                 
                 tags$head(tags$style(HTML("pre { word-break: normal; white-space: pre-wrap; }")))
                 
)
###DATA PRE-PROCESSING
#READ DATA FROM CSV
source('load_csvs.R')
dic<-list("S&P 500 (SPX)" = spx, "Dow-Jones Industrial Average (DJI)" = dji, "NASDAQ (NDQ)" = ndq, "Tech Industry (IYW)" = iyw, "Financial Services (IYF)" = iyf, "Natural Resources (MXI)" = mxi, "Consumer Staples (XLP)" = xlp, "Utilities (XLU)" = xlu, "Dow Jones Utilities Average (DJU)" = dju, "Dick's Sporting Goods (DKS)" = dks, "Footlocker (FL)" = fl, "Lululemon (LULU)" = lulu, "Nike (NKE)" = nke, "Underarmor (UAA)" = uaa)

###SERVER LOGIC###
server <- function(input, output, session){
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
  
  #histogram
  output$histPlot <- renderPlot({
    dataset <- datasetInput()
    x <- dataset$Returns
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    if(input$tab_selected == 2) bins = bin_me_daddy(x, input$bins, input$min_bin_count)$breaks
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = paste("Log returns for",input$dataset),
         main = paste("Histogram for log returns of",input$dataset))
    
  })
  
  output$normPlot <- renderPlot({
    dataset <- datasetInput()
    qqnorm(dataset$Returns, col = "#75AADB",
           main = "Normality plot for log returns in selected tech stock data")
  })
  
  output$goodnessFit <- renderPrint({
    dataset <- datasetInput()
    x <- dataset$Returns
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    if(input$tab_selected == 2) p1 = bin_me_daddy(x, input$bins1, input$min_bin_count)

    
    num_rows <- length(p1$counts)
    cumulative_probs <- pnorm(p1$breaks, mean = mean(x), sd = sd(x))
    
    remove_cumulative <- 2
    
    non_cumulative_probs <- cumulative_probs
    
    while(remove_cumulative < num_rows){
      non_cumulative_probs[remove_cumulative] <- cumulative_probs[remove_cumulative] - cumulative_probs[remove_cumulative-1]
      remove_cumulative <- remove_cumulative + 1
    }
    non_cumulative_probs[num_rows] <- 1 - cumulative_probs[num_rows-1]
    non_cumulative_probs <- non_cumulative_probs[-length(non_cumulative_probs)]
    
    a <- chisq.test(p1$counts, p=non_cumulative_probs, simulate.p.value=TRUE)
    
    print(a)
  
  })
  
  output$confidenceIntMean <- renderPrint({
    dataset <- datasetInput()
    test <- input$test_choice
    dataset$Returns <- log_return(dataset)
    dataset$Returns -> dlr
    length(dlr) -> n
    if(test == "Two-sided"){
      mlower <- mean(dlr) + qt(input$sig/2, n-1) * sqrt(var(dlr))
      mupper <- mean(dlr) - qt(input$sig/2, n-1) * sqrt(var(dlr))
      mlower = signif(mlower,3)
      mupper = signif(mupper,3)
      cat(paste((1-input$sig)*100,"% two-sided confidence interval for mean of log returns:"), sep = "")
      cat(paste('\n[',mlower,', ',mupper,']', sep =""))
    }
    else if(test == "Upper-bound"){
      mupper <- mean(dlr) + qt(input$sig, n-1) * sqrt(var(dlr))
      mupper = signif(mupper, 3)
      cat(paste((1-input$sig)*100, "% upper-bound confidence interval for mean of log returns:"), sep="")
      cat(paste('\n[-inifinity, ', mupper,']', sep=""))
    }
    else if(test == "Lower-bound"){
      mlower <- mean(dlr) - qt(input$sig, n-1) * sqrt(var(dlr))
      mlower = signif(mlower, 3)
      cat(paste((1-input$sig)*100, "% lower-bound confidence interval for mean of log returns:"), sep="")
      cat(paste('\n[',mlower, ', +infinity]', sep=""))
    }
  })
  
  output$confidenceIntVar <- renderPrint({
    dataset <- datasetInput()
    test <- input$test_choice
    dataset$Returns <- log_return(dataset)
    dataset$Returns -> dlr
    length(dlr) -> n
    if(test == 'Two-sided'){
      vlower <- ((n-1)*var(dlr))/qchisq(input$sig/2, n-1)
      vupper <- ((n-1)*var(dlr))/qchisq(1-input$sig/2, n-1)
      vlower = signif(vlower,3)
      vupper = signif(vupper,3)
      cat(paste((1-input$sig)*100,"% two-sided confidence interval for variance of log returns:"), sep = "")
      cat(paste('\n[',vlower,', ',vupper,']', sep =""))
    }
    else if(test == 'Upper-bound'){
      vupper <- ((n-1)*var(dlr))/qchisq(1-input$sig, n-1)
      vupper = signif(vupper, 3)
      cat(paste((1-input$sig)*100,"% upper-bound confidence interval for variance of log returns:"), sep = "")
      cat(paste('\n[-infinity, ', vupper,']', sep=""))
    }
    else if(test == 'Lower-bound'){
      vlower <- ((n-1)*var(dlr))/qchisq(input$sig, n-1)
      vlower = signif(vlower, 3)
      cat(paste((1-input$sig)*100,"% lower-bound confidence interval for variance of log returns:"), sep="")
      cat(paste('\n[',vlower, ', +infinity]', sep=""))
    }
  })
  
  slr <- reactive({
    dataset <- datasetInput()
    dataset$Returns <- log_return(dataset)
    y <- dataset$Returns
    length(y) -> n
    x <- c(1:n) 
    plot(y ~ x,bty="l",pch=20)
    regression<-lm(y ~ x)
    return(regression)
  })
  
  output$oneSampleLinearRegression <- renderPlot({
    dataset <- datasetInput()
    dataset$Returns <- log_return(dataset)
    y <- dataset$Returns
    length(y) -> n
    x <- c(1:n)
    reg <- lm(y ~ x, data=dataset)
    #summary(reg) -> summary
    #print(summary)
    plot(reg$model[,2], reg$model[,1],
         main = 'Regression data with least-squares regression line',
         xlab = 'Time',
         ylab = input$dataset)
    abline(reg, lty=1, lwd=2)
    #summary(linearMod)
  })
  
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
  })
  
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
  
  bin_me_1 <- reactive({
    x = datasetsInput()$Returns.x
    if(input$tab_selected == 2) bins = bin_me_daddy(x, input$bins1, input$min_bin_count)$breaks
    else bins = seq(min(x), max(x), length.out = input$bins1 + 1)
    return(bins)
  })
  
  bin_me_2 <- reactive({
    x = datasetsInput()$Returns.y
    if(input$tab_selected == 2) bins = bin_me_daddy(x, input$bins2, input$min_bin_count)$breaks
    else bins = seq(min(x), max(x), length.out = input$bins2 + 1)
    return(bins)
  })
  
  output$histPlot1 <- renderPlot({
    dataset <- datasetsInput()
    x <- dataset$Returns.x
    bins = bin_me_1()
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = paste("Log returns for",input$datasets[1]),
         main = paste("Histogram for log returns of",input$datasets[1]))
    
  })
  
  output$histPlot2 <- renderPlot({
    dataset <- datasetsInput()
    x <- dataset$Returns.y
    bins <- seq(min(x), max(x), length.out = input$bins2 + 1)
    if(input$tab_selected == 2) bins = bin_me_daddy(x, input$bins2, input$min_bin_count)$breaks
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = paste("Log returns for",input$datasets[2]),
         main = paste("Histogram for log returns of",input$datasets[2]))
    
  })
  
  output$histPlot1_1 <- renderPlot({
    dataset <- datasetsInput()
    x <- dataset$Returns.x
    bins = bin_me_1()
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = paste("Log returns for",input$datasets[1]),
         main = paste("Histogram for log returns of",input$datasets[1]))
    
  })
  
  output$histPlot2_1 <- renderPlot({
    dataset <- datasetsInput()
    x <- dataset$Returns.y
    bins = bins = bin_me_2()
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = paste("Log returns for",input$datasets[2]),
         main = paste("Histogram for log returns of",input$datasets[2]))
    
  })
  
  output$testMeans <- renderPrint({
    d<-datasetsInput()
    test_means(d$Returns.x, d$Returns.y, input$sig)
  })
  
  output$testIndependence <- renderPrint({
    d<-datasetsInput()
    breaks1 = bin_me_1()
    breaks2 = bin_me_2()
    test_independence(d$Returns.x, d$Returns.y, 
                      breaks1 = breaks1, breaks2 = breaks2, 
                      showCT = input$showCT, alpha = input$sig)
  })
  
  output$advtestIndependence <- renderPrint({
    d<-datasetsInput()
    adv_test_independence(d$Returns.x, d$Returns.y, replicates = input$replicates, alpha = input$sig)
  })

  reactive({
    d <- datasetsInput()
    lm(d$Returns.y ~ d$Returns.x) -> lm
    return(lm)
    }) -> tsreg
  
  output$twoSampleRegressionSummary <- renderPrint({
    tsreg()->reg
    summary(reg)$r.squared ->r2
    confint(reg, level = (1-input$sig)) -> conf
    cat(paste('Least Squares Regression Formula: y =',signif(reg$coefficients[2],3), 'x +',signif(reg$coefficients[1],3),
              '\nR-squared:',signif(r2,3),'\nwhere y represents',input$datasets[1],'and x represents',input$datasets[2],
              paste('\n\n',100*(1-input$sig),'% Confidence Intervals:\nSlope: [',signif(conf[2,1],3),', ',signif(conf[2,2],3),']\nIntercept: [',signif(conf[1,1],3),', ',signif(conf[1,2],3),']',sep = "")))
  })
  
  output$twoSampleRegressionPlot <-renderPlot({
    tsreg()->reg
    plot(reg$model[,2], reg$model[,1],
         main = 'Regression data with least-squares regression line',
         xlab = input$datasets[1],
         ylab = input$datasets[2])
    abline(reg, lty=1, lwd=2)
  })
  
  output$twoSampleResidualPlot <- renderPlot({
    tsreg()->reg
    plot(reg$model[,2],reg$residuals,
         main = 'Residual Plots for Linear Regression',
         xlab = input$datasets[1],
         ylab = 'Regression Residuals')
    abline(h = 0)
  })
  
  output$sbtestIndependence <- renderPrint({
    d<-sbdatasetInput()
    sb_test_independence(d, conf = input$sbconf)
  })
  
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
  
  sbreg = reactive({
    d = sbdatasetInput()
    dummy_bool = grepl('NFL', d$Winning.Conference, fixed = TRUE)
    diff = d$Diff
    lm(d$Returns ~ diff + dummy_bool) -> lm
    return(lm)
  })
  
  output$sbRegressionSummary <- renderPrint({
    sbreg()->reg
    summary(reg)$r.squared ->r2
    confint(reg, level = (input$sbconf/100)) -> conf
    cat(paste('Least Squares Regression Formula: y =',signif(reg$coefficients[2],3), 'x_1 +',signif(reg$coefficients[3],3),'x_2 +',signif(reg$coefficients[1],3),
              '\nR-squared:',signif(r2,3)),'\nWhere x_1 is the point difference in the Super Bowl and x_2 is 1 if an NFL team won, or 0 if an AFL team won.',
        paste('\n\n',input$sbconf,'% Confidence Intervals:\nSlope for Point Difference: [',signif(conf[2,1],3),', ',signif(conf[2,2],3),
              ']\nSlope for NFL Indicator: [', signif(conf[3,1],3),', ',signif(conf[3,2],3),
              ']\nIntercept: [',signif(conf[1,1],3),', ',signif(conf[1,2],3),']',sep = ""))
  })
  
  output$sbRegressionPlot <-renderPlot({
    sbreg()->reg
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
  
  output$sbResidualPlot <- renderPlot({
    sbreg()->reg
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






