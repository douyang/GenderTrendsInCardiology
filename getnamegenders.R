install.packages(c("jsonlite", "curl", "stringr")

library(jsonlite)
library(curl)
library(stringr)

setwd('C:\\Users\\David\\Dropbox\\StatisticalAnalysis\\Female Cardiologists')

names<-read.csv('unknowns.csv',stringsAsFactors=FALSE)
names <- names[!str_detect( names$names, "[?]"),]
str(names)

#names1<-names[1:1000]
#names2<-names[1001:2000,]
#names3<-names[2001:3000,]
#names4<-names[3001:4000,]
#names5<-names[4001:5000,]
#names6<-names[5001:6000,]
#names7<-names[6001:7000,]
#names8<-names[7001:8000,]
#names9<-names[8001:9000,]
#names10<-names[9001:10000,]


namestry<-data.frame(names=names,stringsAsFactors=FALSE)
namestry$gender <- "empty"
namestry$probability <- "empty"
namestry$count <- "empty"
str(namestry)


Sys.sleep(60*60*4)

#for (i in 7001:9674){
for (i in 7904:8904){

#for (i in 985:1000){
  
  #Sys.sleep(10)
  try<-fromJSON(paste0("https://api.genderize.io/?name=",names[i]))
  print(i)
  if(is.null(try$gender)){
    namestry$gender[i]<-"unknown"
    namestry$probability[i]<-"unknown"
    namestry$count[i]<-"unknown"    

  }  else {
    namestry$gender[i]<-try$gender[1]
    namestry$probability[i]<-try$probability[1] 
    namestry$count[i]<-try$count[1] 
} }

#head(namestry,3000)

write.csv(namestry,"UnknownsPart11.csv")


              