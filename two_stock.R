#setwd("~/Documents/nicolaus copernicus/hmwk/r/the-fried-eggs")
require(energy)
source('load_csvs.R')

get_yearly <- function(df){ #turn daily returns into yearly returns
  aggregate(list(df$Open,df$Week), by=list(df$Year), FUN=function(x){x[1]})->yearly_open
  aggregate(list(df$Close, df$Week), by=list(df$Year), FUN=function(x){tail(x,n=1)})->yearly_close
  colnames(yearly_close)<-c('Year', "Close", "CWeek")
  colnames(yearly_open)<-c('Year',"Open","OWeek")
  yearly_close = yearly_close[yearly_close$CWeek %in% c("52","53"),c("Year","Close")]
  yearly_open = yearly_open[yearly_open$OWeek %in% c("00","01"),c("Year", "Open")]
  return(merge(yearly_open,yearly_close))
}

log_return <- function(symbol){ #Calculate log returns
  open = symbol[,'Open']
  close = symbol[,'Close']
  return(log(close/open))
}

join_samples <- function(s1, s2, yearly){ 
  if(yearly) merge(s1, s2, by="Year")
  else merge(s1, s2, by="Date")
}

test_means <- function(s1, s2, alpha, yearly = TRUE){
  t.test(s1, s2,paired=TRUE, conf.level = 1-alpha)->t
  cat(paste("Results of", t$method,'\n'))
  cat(paste('P-value: ', round(t$p.value,3)))
  if(t$p.value>alpha) cat('\nThus, we do not reject the null hypothesis that the true difference between the two means is zero.')
  else cat('\nThus, we reject the null hypothesis that the true difference between mean is zero.')
  cat(paste('\n\n', (1-alpha)*100, '% confidence interval for the difference between the mean log returns: \n[', sep=''))
  cat(paste(signif(t$conf.int[1],3), ', ', signif(t$conf.int[2],3), ']', sep=''))
}

test_independence <- function(s1, s2, yearly = TRUE, breaks1=2, breaks2=2, merge_bins = TRUE, mbc=NaN, showCT = FALSE, alpha = 0.05){
  if(merge_bins){
    breaks1 = bin_me_daddy(s1, breaks1, mbc)$breaks
    breaks2 = bin_me_daddy(s2, breaks2, mbc)$breaks
  }
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

adv_test_independence <- function(s1, s2, replicates = 10, alpha = 0.05){
  dcor.test(s1, s2, R=replicates)->d
  cat(paste('Distance correlation coefficient:',d$statistic,'\n'))
  cat(paste('P-value:',signif(d$p.value,2)))
  if(d$p.value>alpha) cat('\nThus, we do not reject the null hypothesis that the log returns for the two stocks are independent.')
  else cat('\nThus, we reject the null hypothesis that the log returns for the two stocks are independent.')
}

two_sample_regression <- function(sy, sx, yearly=TRUE, anova = FALSE){
  lm <- lm(sy ~ sx)
  summary(lm)->summary
  print(summary)
  if(anova) print(anova(lm))
}

bin_me_daddy <- function(log_returns, init_bin_count = 30, min_bin_value = 3){
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

sb_join <- function(dataset){
  merge(dataset, superbowl)
}

sb_test_independence <- function(dataset, alpha){
  c(min(dataset$Returns)-1e-5,0,max(dataset$Returns))->br
  cut(dataset$Returns, br, labels = list('Bearish','Bullish')) -> dataset$BorB
  table(dataset$BorB, dataset$Winning.Conference) -> ContingencyTable
  print(ContingencyTable)
  chisq.test(ContingencyTable)->c
  cat(paste("Chi-squared contingency table test (", c$method,')\n',sep=''))
  cat(paste('P-value: ', round(c$p.value,3)))
}

#test_means(dji, spx, 0.05, yearly=FALSE)
#test_independence(spx, dji, yearly=FALSE, breaks1 = 2, breaks2 = 2)
#adv_test_independence(uaa, lulu, yearly=TRUE, replicates = 100)
#two_sample_regression(uaa, spx, yearly=FALSE, anova = FALSE)
