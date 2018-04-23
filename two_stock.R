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

get_returns <- function(symbol, abs_returns=FALSE, percent_returns=FALSE){
  open=symbol[,'Open']
  close=symbol[,'Close']
  if(abs_returns) symbol$Returns = (close/open)
  if(percent_returns) symbol$Returns = (close-open)/open
  else symbol$Returns = log(close/open)
  return(symbol)
}

join_samples <- function(s1, s2, yearly, abs_returns, percent_returns){
  if(yearly){
    sample1 = get_yearly(s1)
    sample1 = get_returns(sample1, abs_returns = abs_returns, percent_returns = percent_returns)
    sample2 = get_yearly(s2)
    sample2 = get_returns(sample2, abs_returns = abs_returns, percent_returns = percent_returns)
    merge(sample1, sample2, by='Year')->m
  }
  else{
    sample1 = get_returns(s1, abs_returns = abs_returns, percent_returns = percent_returns)
    sample2 = get_returns(s2, abs_returns = abs_returns, percent_returns = percent_returns)
    merge(sample1, sample2, by="Date")->m
  }
  return(m)
}

test_means <- function(s1, s2, yearly = TRUE, abs_returns = FALSE, percent_returns = FALSE){
  m = join_samples(s1,s2, yearly, abs_returns, percent_returns)
  t.test(m$Returns.x, m$Returns.y,paired=TRUE)->t
  print(t)
}

test_independence <- function(s1, s2, yearly = TRUE, abs_returns = FALSE, percent_returns = FALSE, breaks1=2, breaks2=2){
  m = join_samples(s1,s2, yearly, abs_returns, percent_returns)
  cut1 = cut(m$Returns.x, breaks=breaks1)
  cut2 = cut(m$Returns.y, breaks=breaks2)
  chisq.test(cut1, cut2)->chisq
  print(chisq)
}

two_sample_regression <- function(sy, sx, abs_returns = FALSE, percent_returns = FALSE, yearly=TRUE){
  m = join_samples(sx, sy, yearly, abs_returns, percent_returns)
  lm <- lm(m$Returns.y ~ m$Returns.x)
  summary(lm)->summary
  print(summary)
}

test_means(dji, spx, yearly=FALSE)
test_independence(uaa, lulu, yearly=FALSE, breaks1 = 3, breaks2 = 2)
two_sample_regression(uaa, spx, yearly=FALSE)