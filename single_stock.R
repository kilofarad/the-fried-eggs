#Stock Analysis
#Statistics and Data Analysis - R Project
#Kevin Le & Katrina Francis
#SINGLE_STOCK

library(shiny) #load shiny package

source('load_csvs.R')
source('two_stock.R')

#Confidence interval of means function assuming unknown population variance
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

#Confidence interval of variance function assuming unknown population variance
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

#Chi-squared goodness of fit test for normal distribution
#To perform chi-goodness of fit, first had to bin the stock data and use frequencies of each bin for the observed
goodness_of_fit <-function(symbol, bin_count, min_bin, significance){
  x <- symbol$Returns
  #Bin data
  bins <- seq(min(x), max(x), length.out = bin_count + 1)

  binned_stocks = bin_data(x, bin_count, min_bin) #calls two-stock.R
  
  num_rows <- length(binned_stocks$counts)
  
  #Get expected frequencies
  cumulative_probs <- pnorm(binned_stocks$breaks, mean = mean(x), sd = sd(x))
  cumulative_probs[1] <- 0
  cumulative_probs[num_rows+1] <- 1
  
  #Take the marginal probability of each point, instead of the cumulative probability at each point
  non_cumulative_probs = diff(cumulative_probs)
  
  #Run test
  goodnessOfFit<- chisq.test(binned_stocks$counts, p=non_cumulative_probs, simulate.p.value=FALSE)
  
  #Output results
  cat(paste('Chi-Squared Goodness of Fit Results for Normal Distribution:'),sep="")
  cat(paste('\nSelected bin count: ', bin_count), sep="")
  cat(paste('\nProcessed bin count (combine bins with low frequencies): ', num_rows,'\n'),sep="")
  cat(paste('\nData Source: ', goodnessOfFit$data.name), sep="")
  cat(paste('\nX-squared: ',goodnessOfFit$statistic,'   p-value: ', goodnessOfFit$p.value),sep="")
  if(goodnessOfFit$p.value <= significance){
    cat(paste('\n\n',goodnessOfFit$p.value," <= ",significance," so, the null hypothesis is rejected. There is sufficient evidence that the data does not fit a normal distribution."),sep="")
  }
  else{
    cat(paste('\n\n',goodnessOfFit$p.value," > ",significance," so, we fail to reject the null hypothesis. There is not sufficient evidence that the data does not fit a normal distribution."),sep="")
  }
}
