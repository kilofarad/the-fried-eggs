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
ui <- pageWithSidebar(
  
  #TITLE
  headerPanel("Stock Analyses"),
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
    
    radioButtons("yearly", label = "Time Interval for Returns",
                choices = list("Daily" = FALSE, "Yearly" = TRUE)),
                               
    conditionalPanel('input.stocks == 1', 
                     sliderInput(inputId = "bins",
                                 label = "Number of histogram bins:",
                                 min = 1,
                                 max = 50,
                                 value = 30)
                     ),
    
    conditionalPanel('input.stocks == 2',
                     sliderInput(inputId = "bins1",
                                 label = "Number of histogram bins:",
                                 min = 1,
                                 max = 50,
                                 value = 30),
                     sliderInput(inputId = "bins2",
                                 label = "Number of histogram bins:",
                                 min = 1,
                                 max = 50,
                                 value = 30)
                     ),
    
    conditionalPanel('!input.dcor',
                     checkboxInput(inputId = 'merge_bins', 
                                   label = 'Merge bins dynamically to help meet minimum bin value requirements of the chi-squared test', 
                                   value = TRUE)),
    
    conditionalPanel('input.merge_bins && !input.dcor', 
                     sliderInput(inputId = "min_bin_count",
                                 label = "Minimum bin count",
                                 min = 2,
                                 max = 5,
                                 value = 3),
                     checkboxInput(inputId = 'show_merged_bins', 
                                   label = 'Show the merged bins on the histogram(s)',
                                   value = FALSE)
                     ),
    
    conditionalPanel('input.stocks == 2 && !input.dcor', 
                     checkboxInput(inputId = 'showCT', 
                                   label = 'Show contingency table for the chi-squared test for independence', 
                                   value = FALSE)),
    
    checkboxInput(inputId = 'dcor', 
                  label = 'Perform a distance correlation test for independence (more computationally expensive)', 
                  value = FALSE),
    
    conditionalPanel('input.dcor',
                     sliderInput(inputId = 'replicates',
                                 label = "Replicates to perform for dcor (Higher values will give a more precise p-value, but is very computationally expensive)",
                                 min = 5,
                                 max = 200,
                                 value = 10)
                    ),
    
    
    
    sliderInput(inputId = "sig",
                label = "Significance level of confidence intervals",
                min = 0.01,
                max = 0.99,
                value = 0.05),
  
    conditionalPanel('input.stocks ==1',
                     radioButtons(inputId = "test_choice", 
                                                     label = "Select test type",
                                                     choices = c("Two-sided", "Upper-bound", "Lower-bound")
                                  )
                     )
    
  ),
  
  #MAIN PANEL
  mainPanel(
    conditionalPanel("input.stocks == 1", 
                     plotOutput("histPlot"),
                     plotOutput("normPlot"),
                     verbatimTextOutput("goodnessFit"),
                     verbatimTextOutput("confidenceIntMean"),
                     verbatimTextOutput("confidenceIntVar")
                     ),
    
    conditionalPanel("input.stocks == 2",
                     plotOutput("histPlot1"),
                     plotOutput("histPlot2"),
                     verbatimTextOutput("testMeans"),
                     verbatimTextOutput("testIndependence")
                     )
  )
  
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
    if(input$merge_bins) if(input$show_merged_bins) bins = bin_me_daddy(x, input$bins, input$min_bin_count)$breaks
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
    dataset$Returns <- log_return(dataset)
    dataset$Returns -> dlr
    print(ks.test(dlr, 'pnorm', mean(dlr), var(dlr)**0.5))
    #dataset$Returns -> observed
    #pnorm(dataset$Returns) -> expected
    #cat("Goodness of Fit for Normality Results:")
    #print(chisq.test(x = observed, p = expected))
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
  
  output$linearRegression <- renderPlot({
    dataset <- datasetInput()
    dataset$Returns <- log_return(dataset)
    y <- dataset$Returns
    length(y) -> n
    x <- c(1:n) 
    plot(y ~ x,bty="l",pch=20)
    regression<-lm(y ~ x)
    
    abline(regression, lty=1, lwd=2)
    plot(resid(regression))
    
    qqnorm(resid(regression))
    qqline(resid(regression))
    
    plot(fitted(regression),resid(regression))
    plot(fitted(regression),rstandard(regression))
    
    print("Intercept and Slope Estimates:")
    print(regression)
  })
  
  output$histPlot1 <- renderPlot({
    dataset <- datasetsInput()
    x <- dataset$Returns.x
    bins <- seq(min(x), max(x), length.out = input$bins1 + 1)
    if(!input$dcor) if(input$merge_bins) if(input$show_merged_bins) bins = bin_me_daddy(x, input$bins1, input$min_bin_count)$breaks
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = paste("Log returns for",input$datasets[1]),
         main = paste("Histogram for log returns of",input$datasets[1]))
    
  })
  
  output$histPlot2 <- renderPlot({
    dataset <- datasetsInput()
    x <- dataset$Returns.y
    bins <- seq(min(x), max(x), length.out = input$bins2 + 1)
    if(!input$dcor) if(input$merge_bins) if(input$show_merged_bins) bins = bin_me_daddy(x, input$bins2, input$min_bin_count)$breaks
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = paste("Log returns for",input$datasets[2]),
         main = paste("Histogram for log returns of",input$datasets[2]))
    
  })
  
  output$testMeans <- renderPrint({
    d<-datasetsInput()
    test_means(d$Returns.x, d$Returns.y, input$sig, yearly = input$yearly)
    })
  
  output$testIndependence <- renderPrint({
    d<-datasetsInput()
    if(input$dcor){
      adv_test_independence(d$Returns.x, d$Returns.y, alpha = input$sig)
    }
    else{
    breaks1 = input$bins1
    breaks2 = input$bins2
    test_independence(d$Returns.x, d$Returns.y, 
                      yearly=input$yearly, 
                      breaks1 = breaks1, breaks2 = breaks2, 
                      merge_bins = input$merge_bins, mbc=input$min_bin_count, 
                      showCT = input$showCT, alpha = input$sig)}
  })

  
  output$twoSampleRegressionSummary <- renderPrint({cat("Hello World")})
  
  #output$twoSampleRegressionPlot <-renderPlot({})
  #output$twoSampleResidualPlot <- renderPlot({})
}

  

shinyApp(ui, server)






