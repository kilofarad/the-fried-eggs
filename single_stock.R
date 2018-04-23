#Single Stock Analysis
#Statistics and Data Analysis - R Project
runapp.r
library(shiny)
#setwd(“C:/Users/weloveton/Documents/stockhistograms”)
#runApp(host = “0.0.0.0”, port = 5050)

app.r

library(shiny) #load shiny package
###USER INTERFACE###
ui <- pageWithSidebar(
  
  #TITLE
  headerPanel("Tech Company Stocks"),
  #SIDEBAR
  sidebarPanel(
    
    #Select dataset
    selectInput(inputId = "dataset",
                label = "Choose a dataset:",
                choices = c("S&P 500 (SPX)",
                            "Dow-Jones Industrial Average (DJI)",
                            "NASDAQ (NDQ)",
                            "Tech Industry (IYW)",
                            "Financian Services (IYF)",
                            "Natural Resources (MXI)",
                            "Consumer Staples (XLP)",
                            "Utilities (XLU)",
                            "Dow Jones Utilities Average (DJU)",
                            "Nike (NKE)",
                            "Adidas (ADS)",
                            "Puma (PUM)",
                            "Underarmor (UAA)")),
    
    sliderInput(inputId = "bins",
                label = "Number of histogram bins:",
                min = 1,
                max = 50,
                value = 30),
    
    sliderInput(inputId = "sig",
                label = "Significance level of confidence intervals",
                min = 0.01,
                max = 0.99,
                value = 0.05)
  ),
  
  #MAIN PANEL
  mainPanel(
    
    #Output histogram
    
    plotOutput("histPlot"),
    plotOutput("normPlot"),
    verbatimTextOutput("goodnessFit"),
    verbatimTextOutput("confidenceIntMean"),
    verbatimTextOutput("confidenceIntVar")
  )
  
)
###DATA PRE-PROCESSING
#READ DATA FROM CSV
#General
sp <- read.csv('spx.csv') -> sp
dj_ind <- read.csv('dji.csv')
nasdaq <- read.csv('ndq.csv') 

#Non-Sport 
tech <- read.csv('tech.csv') 
finance <- read.csv('fin.csv') 
nat_resources <- read.csv('nat.csv')
consumer_staples <- read.csv('stp.csv') 
utilities <- read.csv('uti.csv') 
dj_util <- read.csv('dju.csv')

#Sport
nike <- read.csv('nke.csv')
adidas <- read.csv('ads_eur.csv') 
puma <- read.csv('pum_eur.csv')
underarmor <- read.csv('uaa.csv')
lululemon <- read.csv('lulu.csv') 

###SERVER LOGIC###
server <- function(input, output, session){
  datasetInput <- reactive({
    switch(input$dataset,
           "S&P 500 (SPX)" = sp,
           "Dow-Jones Industrial Average (DJI)" = dj_ind,
           "NASDAQ (NDQ)" = nasdaq,
           "Tech Industry (IYW)" = tech,
           "Financian Services (IYF)" = finance,
           "Natural Resources (MXI)" = nat_resources,
           "Consumer Staples (XLP)" = consumer_staples,
           "Utilities (XLU)" = utilities,
           "Dow Jones Utilities Average (DJU)" = dj_util,
           "Nike (NKE)" = nike,
           "Adidas (ADS)" = adidas,
           "Puma (PUM)" = puma,
           "Underarmor (UAA)" = underarmor)
  })
  
  
  #histogram
  output$histPlot <- renderPlot({
    dataset <- datasetInput()
    x <- dataset$high
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Highs in tech stock data",
         main = "Histogram of highs in selected tech stock data")
    
  })
  
  log_return <- function(symbol){
    open = symbol[,'open']
    close = symbol[,'close']
    return(log(close/open))
  }
  
  output$normPlot <- renderPlot({
    dataset <- datasetInput()
    dataset$log_return <- log_return(dataset)
    qqnorm(dataset$log_return, col = "#75AADB",
           main = "Normality plot for log returns in selected tech stock data")
  })
  
  output$goodnessFit <- renderPrint({
    dataset <- datasetInput()
    dataset$log_return <- log_return(dataset)
    dataset$log_return -> observed
    pnorm(dataset$log_return) -> expected
    print("Goodness of Fit for Normality Results:")
    print(chisq.test(x = observed, p = expected))
  })
  
  output$confidenceIntMean <- renderPrint({
    dataset <- datasetInput()
    dataset$log_return <- log_return(dataset)
    dataset$log_return -> dlr
    length(dlr) -> n
    mlower <- mean(dlr) + qt(input$sig/2, n-1) * sqrt(var(dlr))
    mupper <- mean(dlr) - qt(input$sig/2, n-1) * sqrt(var(dlr))
    print("Significance level:")
    print(input$sig)
    print("Confidence interval for mean of log returns given significance level:")
    print(c(mlower,mupper))
    
  })
  
  output$confidenceIntVar <- renderPrint({
    dataset <- datasetInput()
    dataset$log_return <- log_return(dataset)
    dataset$log_return -> dlr
    length(dlr) -> n
    vlower <- ((n-1)*var(dlr))/qchisq(input$sig/2, n-1)
    vupper <- ((n-1)*var(dlr))/qchisq(1-input$sig/2, n-1)
    print("Significance level:")
    print(input$sig)
    print("Confidence interval for variance of log returns given significance level:")
    print(c(vlower,vupper))
  })
  
  output$linearRegression <- renderPlot({
    dataset <- datasetInput()
    dataset$log_return <- log_return(dataset)
    y <- dataset$log_return
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
  
}
shinyApp(ui, server)






