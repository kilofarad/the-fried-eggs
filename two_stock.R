setwd("~/Documents/nicolaus copernicus/hmwk/r/the-fried-eggs")
require(energy)
source('load_csvs.R')

get_yearly <- function(df){
  aggregate(list(df$Open,df$Week), by=list(df$Year), FUN=function(x){x[1]})->yearly_open
  aggregate(list(df$Close, df$Week), by=list(df$Year), FUN=function(x){tail(x,n=1)})->yearly_close
  colnames(yearly_close)<-c('Year', "Close", "CWeek")
  colnames(yearly_open)<-c('Year',"Open","OWeek")
  yearly_close = yearly_close[yearly_close$CWeek %in% c("52","53"),c("Year","Close")]
  yearly_open = yearly_open[yearly_open$OWeek %in% c("00","01"),c("Year", "Open")]
  return(merge(yearly_open,yearly_close))
}

log_return <- function(symbol){
  open = symbol[,'Open']
  close = symbol[,'Close']
  return(log(close/open))
}

join_samples <- function(s1, s2, yearly){
  if(yearly){
    sample1 = get_yearly(s1)
    sample1$Returns = log_return(sample1)
    sample2 = get_yearly(s2)
    sample2$Returns = log_return(sample2)
    merge(sample1, sample2, by='Year')->m
  }
  else{
    s1$Returns = log_return(s1)
    s2$Returns = log_return(s2)
    merge(s1, s2, by="Date")->m
  }
  return(m)
}

test_means <- function(s1, s2, alpha, yearly = TRUE){
  m = join_samples(s1, s2, yearly)
  t.test(m$Returns.x, m$Returns.y,paired=TRUE, conf.level = 1-alpha)->t
  cat("Results of paired t-test:\n")
  cat(paste('P-value: ', round(t$p.value,3)))
  if(t$p.value>alpha) cat('\nThus, we do not reject the null hypothesis that the true difference between the two means is zero.')
  else cat('\nThus, we reject the null hypothesis that the true difference between mean is zero.')
  cat(paste('\n\n', (1-alpha)*100, '% confidence interval for the difference between the mean log returns: \n[', sep=''))
  cat(paste(signif(t$conf.int[1],3), ', ', signif(t$conf.int[2],3), ']', sep=''))
}

test_independence <- function(s1, s2, yearly = TRUE, breaks1=2, breaks2=2){
  m = join_samples(s1,s2, yearly)
  cut1 = cut(m$Returns.x, breaks=breaks1)
  cut2 = cut(m$Returns.y, breaks=breaks2)
  chisq.test(cut1, cut2)->chisq
  print(chisq)
}

adv_test_independence <- function(s1, s2, yearly = TRUE, replicates = 199){
  m = join_samples(s1,s2, yearly)
  dcor.test(m$Returns.x, m$Returns.y, R=replicates)->d
  print(d)
}

two_sample_regression <- function(sy, sx, yearly=TRUE, anova = FALSE){
  m = join_samples(sx, sy, yearly)
  lm <- lm(m$Returns.y ~ m$Returns.x)
  summary(lm)->summary
  print(summary)
  if(anova) print(anova(lm))
}

#test_means(dji, spx, 0.05, yearly=FALSE)
test_independence(spx, dji, yearly=TRUE, breaks1 = 2, breaks2 = 2)
#adv_test_independence(uaa, lulu, yearly=TRUE, replicates = 100)
#two_sample_regression(uaa, spx, yearly=FALSE, anova = FALSE)
