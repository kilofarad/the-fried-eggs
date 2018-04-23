temp = list.files(pattern='[.]csv')
for (i in 1:length(temp)) {
  name<-gsub("*.csv","",temp[i])
  print(name)
  assign(name, read.csv(temp[i]))
  temp2<-get(name)
  if("Date" %in% names(temp2)){
    temp2$"Date"<-as.Date(temp2$"Date")
    temp2$"Year"<-format(temp2$Date,"%Y")
    temp2$"Week"<-format(temp2$Date,"%U")
    assign(name, temp2)
  }
}