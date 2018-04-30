
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


confidence_interval_mean<-function(symbol, test_type, significance){
  dlr <- log_return(symbol)
  length(dlr) -> n
  if(test_type == "Two-sided"){
    mlower <- mean(dlr) + qt(significance/2, n-1) * sqrt(var(dlr))
    mupper <- mean(dlr) - qt(significance/2, n-1) * sqrt(var(dlr))
    mlower = signif(mlower,3)
    mupper = signif(mupper,3)
    cat(paste((1-significance)*100,"% two-sided confidence interval for mean of log returns:"), sep = "")
    cat(paste('\n[',mlower,', ',mupper,']', sep =""))
  }
  else if(test_type == "Upper-bound"){
    mupper <- mean(dlr) + qt(significance, n-1) * sqrt(var(dlr))
    mupper = signif(mupper, 3)
    cat(paste((1-significance)*100, "% upper-bound confidence interval for mean of log returns:"), sep="")
    cat(paste('\n[-inifinity, ', mupper,']', sep=""))
  }
  else if(test_type == "Lower-bound"){
    mlower <- mean(dlr) - qt(significance, n-1) * sqrt(var(dlr))
    mlower = signif(mlower, 3)
    cat(paste((1-significance)*100, "% lower-bound confidence interval for mean of log returns:"), sep="")
    cat(paste('\n[',mlower, ', +infinity]', sep=""))
  }
  
}

confidence_interval_var<-function(symbol, test_type, significance){
  dlr <- log_return(symbol)
  length(dlr) -> n
  print(significance)
  if(test_type == 'Two-sided'){
    vlower <- ((n-1)*var(dlr))/qchisq(1-significance/2, n-1)
    vupper <- ((n-1)*var(dlr))/qchisq(significance/2, n-1)
    vlower = signif(vlower,3)
    vupper = signif(vupper,3)
    cat(paste((1-significance)*100,"% two-sided confidence interval for variance of log returns:"), sep = "")
    cat(paste('\n[',vlower,', ',vupper,']', sep =""))
  }
  else if(test_type == 'Upper-bound'){
    vupper <- ((n-1)*var(dlr))/qchisq(1-significance, n-1)
    vupper = signif(vupper, 3)
    cat(paste((1-significance)*100,"% upper-bound confidence interval for variance of log returns:"), sep = "")
    cat(paste('\n[-infinity, ', vupper,']', sep=""))
  }
  else if(test_type == 'Lower-bound'){
    vlower <- ((n-1)*var(dlr))/qchisq(significance, n-1)
    vlower = signif(vlower, 3)
    cat(paste((1-significance)*100,"% lower-bound confidence interval for variance of log returns:"), sep="")
    cat(paste('\n[',vlower, ', +infinity]', sep=""))
  }
}

goodness_of_fit <-function(symbol, bin_count, min_bin, tab_selected, significance){
  x <- symbol$Returns
  bins <- seq(min(x), max(x), length.out = bin_count + 1)
  if(tab_selected == 2) {
    binned_stocks = bin_me_daddy(x, bin_count, min_bin)
  }
  num_rows <- length(binned_stocks$counts)
  cumulative_probs <- pnorm(binned_stocks$breaks, mean = mean(x), sd = sd(x))
  cumulative_probs[1] <- 0
  cumulative_probs[num_rows+1] <- 1
  
  non_cumulative_probs = diff(cumulative_probs)
  
  goodnessOfFit<- chisq.test(binned_stocks$counts, p=non_cumulative_probs, simulate.p.value=FALSE)
  cat(paste('Goodness of Fit Results for Normal Distribution:\n'),sep="")
  print(goodnessOfFit)
  if(goodnessOfFit$p.value <= significance){
    cat(paste('\n',goodnessOfFit$p.value," <= ",significance," so, the null hypothesis is rejected. There is sufficient evidence that the data does not fit a normal distribution."),sep="")
  }
  else{
    cat(paste('\n',goodnessOfFit$p.value," > ",significance," so, we fail to reject the null hypothesis. There is not sufficient evidence that the data does not fit a normal distribution."),sep="")
  }
}
