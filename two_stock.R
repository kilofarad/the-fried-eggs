#Stock Analysis
#Statistics and Data Analysis - R Project
#Kevin Le & Katrina Francis
#TWO_STOCK

require(energy)
source('load_csvs.R')

#turn daily returns into yearly returns
get_yearly <- function(df){ 
  aggregate(list(df$Open,df$Week), by=list(df$Year), FUN=function(x){x[1]})->yearly_open
  aggregate(list(df$Close, df$Week), by=list(df$Year), FUN=function(x){tail(x,n=1)})->yearly_close
  colnames(yearly_close)<-c('Year', "Close", "CWeek")
  colnames(yearly_open)<-c('Year',"Open","OWeek")
  yearly_close = yearly_close[yearly_close$CWeek %in% c("52","53"),c("Year","Close")]
  yearly_open = yearly_open[yearly_open$OWeek %in% c("00","01"),c("Year", "Open")]
  return(merge(yearly_open,yearly_close))
}

#Calculate log returns
log_return <- function(symbol){
  open = symbol[,'Open']
  close = symbol[,'Close']
  return(log(close/open))
}

#Merge samples
join_samples <- function(s1, s2, yearly){ 
  if(yearly) merge(s1, s2, by="Year")
  else merge(s1, s2, by="Date")
}

#Bin stock data, this allows us to use observed frequencies in other functinons as the frequencies of each bin range
bin_data <- function(log_returns, init_bin_count = 30, min_bin_value = 3){
  bins <- seq(min(log_returns), max(log_returns), length.out = init_bin_count + 1)
  hist(log_returns, breaks = bins, plot = FALSE) -> h
  b = c()
  for(x in h$counts) b = append(b, x>min_bin_value)
  if(all(b)) return(h)
  
  new_breaks = c(min(log_returns))
  running_count = 0
  for(i in seq(init_bin_count)){
    running_count = running_count + h$counts[i]
    if(running_count >= min_bin_value){
      new_breaks = append(new_breaks, h$breaks[i+1])
      running_count = 0
    }
  }
  
  if(tail(h$breaks, n=1) %in% new_breaks) new_breaks = replace(new_breaks, length(new_breaks), tail(h$breaks, n=1))
  if(max(log_returns) > tail(new_breaks, n = 1)) new_breaks = replace(new_breaks, length(new_breaks), max(log_returns))
  hist(log_returns, breaks = new_breaks, plot = TRUE)
}

#Test difference of means
test_means <- function(s1, s2, alpha){
  t.test(s1, s2,paired=TRUE, conf.level = 1-alpha)->t
  cat(paste("Results of", t$method,'\n'))
  cat(paste('P-value: ', round(t$p.value,3)))
  if(t$p.value>alpha) cat('\nThus, we do not reject the null hypothesis that the true difference between the two means is zero.')
  else cat('\nThus, we reject the null hypothesis that the true difference between mean is zero.')
  cat(paste('\n\n', (1-alpha)*100, '% confidence interval for the difference between the mean log returns: \n[', sep=''))
  cat(paste(signif(t$conf.int[1],3), ', ', signif(t$conf.int[2],3), ']', sep=''))
}

#Test for independence using chi-squared contingency table
test_independence <- function(s1, s2, breaks1=2, breaks2=2, showCT = FALSE, alpha = 0.05){
  Stock1 = cut(s1, breaks=breaks1)
  Stock2 = cut(s2, breaks=breaks2)
  table(Stock1, Stock2) -> ContigencyTable
  if(showCT) print(ContigencyTable)
  chisq.test(ContigencyTable)->ch
  cat(paste("Results of", ch$method,'\n'))
  cat(paste('P-value: ', round(ch$p.value,3)))
  if(ch$p.value>alpha) cat('\nThus, we do not reject the null hypothesis that the log returns for the two stocks are independent.')
  else cat('\nThus, we reject the null hypothesis that the log returns for the two stocks are independent.')
}

#Test for independence using distance correlation test
adv_test_independence <- function(s1, s2, replicates = 10, alpha = 0.05){
  dcor.test(s1, s2, R=replicates)->d
  cat(paste('Distance correlation coefficient:',d$statistic,'\n'))
  cat(paste('P-value:',signif(d$p.value,2)))
  if(d$p.value>alpha) cat('\nThus, we do not reject the null hypothesis that the log returns for the two stocks are independent.')
  else cat('\nThus, we reject the null hypothesis that the log returns for the two stocks are independent.')
}

#Merge stock data with super bowl data
sb_join <- function(dataset){
  merge(dataset, superbowl)
}

#Test for independence of super bowl data and stock data using chi-squared contingency
sb_test_independence <- function(dataset, conf){
  alpha = 1-conf/100
  cat('Can the outcome of the Superbowl predict the outcome of the stock market?\n\nSome investors believe that the market will be bearish (negative yearly returns) if a team from the original AFL wins, while it will be bullish (positive yearly returns) if a team from the original NFL wins.\n')
  c(min(dataset$Returns)-1e-5,0,max(dataset$Returns))->br
  cut(dataset$Returns, br, labels = list('Bearish','Bullish')) -> dataset$BorB
  table(dataset$BorB, dataset$Winning.Conference) -> ContingencyTable
  cat('\nFor the chosen stock/index:')
  print(ContingencyTable)
  chisq.test(ContingencyTable)->c
  cat(paste("\nChi-squared contingency table test (", c$method,')\n',sep=''))
  cat(paste('P-value: ', round(c$p.value,3)))
  if(c$p.value<alpha) cat(paste('\n\nThe results of this test indicate that we can state with ', conf, '% confidence that the annual returns for the selected stock/index and the outcome of the SuperBowl are not independent.', sep = ''))
  else cat(paste('\n\nThe results of this test indicate that we cannot conclude with ', conf, '% confidence that the annual returns for the selected stock/index and the outcome of the SuperBowl are not independent.', sep = ''))
}