library(jsonlite)
library(curl)
setwd('d:\\orthoresearch\\scrape\\woman\\17')


names<-read.csv('unknowns.csv',stringsAsFactors=FALSE)

names1<-names[1:1000,]
names2<-names[1001:2000,]
names3<-names[2001:3000,]
names4<-names[3001:4000,]
names5<-names[4001:5000,]
names6<-names[5001:6000,]

namestry<-data.frame(names=names,stringsAsFactors=FALSE)

for (i in 1001:2000){
  try<-fromJSON(paste0("https://api.genderize.io/?name=",namestry$names[i]))
  if try$gender=="NULL"{
    namestry$gender[i]<-"unknown"
    namestry$probability[i]<-"unknown"
  }
  else
    namestry$gender[i]<-try$gender[1]
    namestry$probability[i]<-try$probability[1]
}
              