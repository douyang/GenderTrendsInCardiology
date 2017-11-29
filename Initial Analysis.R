#setwd('D:\\orthoresearch\\scrape')
setwd('C:\\Users\\David\\Dropbox\\StatisticalAnalysis\\Female Cardiologists')

library(ggplot2)
library(stringr)

data <- read.csv("AllThreeJournals.txt", stringsAsFactors = FALSE)

str(data)
qplot(data$Year,binwidth = 1)
summary(data$Year)


table(data$Type1)

36096 + 4769 + 5884 + 28 + 80 + 39 + 5 + 7237 + 19 + 359 + 58 + 3 + 610

smallData <- data[data$Type1 %in% c("Journal Article", "Case Reports", "Clinical Trial", "Clinical Trial  Phase I", "Clinial Trial  Phase II", 
"Clinical Trial  Phase III", "Clinical Trial  Phase IV", "Comparative Study", "Controlled Clinical Trial", "Evaluation Studies", "Guideline"),]
str(smallData)

summary(smallData)
sd(smallData$numAuthor, na.rm = TRUE)

ayo <- smallData
ayo<-ayo[!is.na(ayo$numAuthor),]
ayo$ym<-paste(ayo$Year,ayo$Month)

ayo$briefJournal <- ""
ayo[ayo$Journal == "European heart journal",]$briefJournal  <- "Eur Heart J"
ayo[ayo$Journal == "Journal of the American College of Cardiology",]$briefJournal  <- "JACC"
ayo[ayo$Journal == "Circulation",]$briefJournal  <- "Circ"

ayo$ref<-paste(ayo$firstAuthorL," et al. \"",ayo$Title,'\" ',ayo$briefJournal," (",ayo$Year,")",sep="")


authorid<-data.frame(first=c(ayo$firstAuthorF,ayo$srAuthorF,ayo$midAuthorF1,ayo$midAuthorF2,ayo$midAuthorF3,ayo$midAuthorF4,ayo$midAuthorF5,ayo$midAuthorF6,ayo$midAuthorF7,ayo$midAuthorF8,ayo$midAuthorF9,ayo$midAuthorF10,ayo$midAuthorF11,ayo$midAuthorF12,ayo$midAuthorF13,ayo$midAuthorF14,ayo$midAuthorF15),init=c(ayo$firstAuthorI,ayo$srAuthorI,ayo$midAuthorI1,ayo$midAuthorI2,ayo$midAuthorI3,ayo$midAuthorI4,ayo$midAuthorI5,ayo$midAuthorI6,ayo$midAuthorI7,ayo$midAuthorI8,ayo$midAuthorI9,ayo$midAuthorI10,ayo$midAuthorI11,ayo$midAuthorI12,ayo$midAuthorI13,ayo$midAuthorI14,ayo$midAuthorI15),last=c(ayo$firstAuthorL,ayo$srAuthorL,ayo$midAuthorL1,ayo$midAuthorL2,ayo$midAuthorL3,ayo$midAuthorL4,ayo$midAuthorL5,ayo$midAuthorL6,ayo$midAuthorL7,ayo$midAuthorL8,ayo$midAuthorL9,ayo$midAuthorL10,ayo$midAuthorL11,ayo$midAuthorL12,ayo$midAuthorL13,ayo$midAuthorL14,ayo$midAuthorL15),stringsAsFactors=FALSE)
authorid<-authorid[!is.na(authorid$first)&!is.na(authorid$last),]
authorid$faF<-unlist(lapply(strsplit(authorid$first," "), '[[',1))

authorid$class<-""
authorid$class[nchar(authorid$faF)==1 & nchar(authorid$init==1)]<-"NoF_1In"
authorid$class[nchar(authorid$faF)==1 & nchar(authorid$init)==2]<-"NoF_2In"
authorid$class[nchar(authorid$faF)>1 & nchar(authorid$init)==1]<-"1F_1In"
authorid$class[nchar(authorid$faF)>1 & nchar(authorid$init)>1]<-"2F_2In"
authorid$matchID4<-NA
authorid$matchID4[authorid$class=="2F_2In"]<-paste(authorid$first[authorid$class=="2F_2In"],authorid$last[authorid$class=="2F_2In"])
authorid$matchID3<-NA
authorid$matchID3[authorid$class %in% c("2F_2In","1F_1In")]<-paste(authorid$faF[authorid$class %in% c("2F_2In","1F_1In")],authorid$last[authorid$class %in% c("2F_2In","1F_1In")])
authorid$matchID2<-NA
authorid$matchID2[authorid$class %in% c("2F_2In","NoF_2In")]<-paste(authorid$init[authorid$class %in% c("2F_2In","NoF_2In")],authorid$last[authorid$class %in% c("2F_2In","NoF_2In")])
authorid$matchID1<-paste(substring(authorid$init,1,1),authorid$last)

authorid$uniqueID<-NA
authorid$uniqueID[authorid$class=="2F_2In"]<-paste(authorid$first[authorid$class=="2F_2In"],authorid$last[authorid$class=="2F_2In"])
authorid$uniqueIDclass[authorid$class=="2F_2In"]<-"2F_2In"
authorid$uniquedup[authorid$class=="2F_2In"]<-0

class4<-authorid[authorid$class=="2F_2In",]
class4<-class4[!duplicated(class4$matchID4),]

match3<-function(name,match4list){
  if (name %in% match4list$matchID3) c(match4list$matchID4[match4list$matchID3==name][1],"2F_2In",length(match4list$matchID4[match4list$matchID3==name]))
  else c(name,"1F_1In",0)
}

authorid$uniqueID[authorid$class=="1F_1In"]<-sapply(authorid$matchID3[authorid$class=="1F_1In"],function (c) match3(c,class4)[1])
authorid$uniqueIDclass[authorid$class=="1F_1In"]<-sapply(authorid$matchID3[authorid$class=="1F_1In"],function (c) match3(c,class4)[2])
authorid$uniquedup[authorid$class=="1F_1In"]<-sapply(authorid$matchID3[authorid$class=="1F_1In"],function (c) match3(c,class4)[3])

match2<-function(name,match4list){
  if (name %in% match4list$matchID2) c(match4list$matchID4[match4list$matchID2==name][1],"2F_2In",length(match4list$matchID4[match4list$matchID2==name]))
  else c(NA,NA,0)
}

authorid$uniqueID[authorid$class=="NoF_2In"]<-sapply(authorid$matchID2[authorid$class=="NoF_2In"],function (c) match2(c,class4)[1])
authorid$uniqueIDclass[authorid$class=="NoF_2In"]<-sapply(authorid$matchID2[authorid$class=="NoF_2In"],function (c) match2(c,class4)[2])
authorid$uniquedup[authorid$class=="NoF_2In"]<-sapply(authorid$matchID2[authorid$class=="NoF_2In"],function (c) match2(c,class4)[3])

class3<-authorid[authorid$class=="1F_1In",]
class3<-class3[!duplicated(class3$uniqueID),]

match1<-function(name,match3list){
  if (name %in% match3list$matchID1) c(match3list$uniqueID[match3list$matchID1==name][1],match3list$uniqueIDclass[match3list$matchID1==name][1],length(match3list$uniqueID[match3list$matchID1==name]))
  else c(NA,NA,0)
}

authorid$uniqueID[authorid$class=="NoF_1In"]<-sapply(authorid$matchID1[authorid$class=="NoF_1In"],function (c) match1(c,class4)[1])


authorid$uniqueIDclass[authorid$class=="NoF_1In"]<-sapply(authorid$matchID1[authorid$class=="NoF_1In"],function (c) match1(c,class4)[2])
authorid$uniquedup[authorid$class=="NoF_1In"]<-sapply(authorid$matchID1[authorid$class=="NoF_1In"],function (c) match1(c,class4)[3])
authorid<-authorid[!is.na(authorid$uniqueID),]





aid<-authorid[,c("first","init","last","uniqueID")]
aid<-aid[!is.na(aid$uniqueID),]
aid$fil<-paste(aid$first,aid$init,aid$last)
aid$faF<-unlist(lapply(strsplit(aid$uniqueID," "),'[[',1))
length(aid$faF)

length(unique(aid$uniqueID))

sort(table(aid$uniqueID))

freqtable <- as.data.frame(table(aid$uniqueID))
str(freqtable)
summary(freqtable)
sd(freqtable$Freq)

dim(aid) #number of authors identified
sum(!is.na(aid$gender)) #number with gender
sum(aid$gender=="female",na.rm=TRUE) #%female
sum(aid$gender=="female",na.rm=TRUE)/sum(!is.na(aid$gender)) #^

#first names - 120723
length(unique(aid$faF))
#unique first names - 12661
write.csv(unique(aid$faF),'uniquefirsts.csv')

sort(table(aid$faF))

















####################################### Run up to this time #########################################

nameMF<-read.csv("namegenders.csv",stringsAsFactors=FALSE)
nameMF$confidence<-as.numeric(nameMF$confidence)
nameMF<-nameMF[nameMF$confidence>=0.6 & !is.na(nameMF$confidence),]
nameMF<-nameMF[,c("name","gender")]
#unique matched first names - 7949



aid<-merge(aid,nameMF,by.x="faF",by.y="name",all.x=TRUE)

aidmatch<-aid[,c("fil","uniqueID","gender")]
aidmatch<-aidmatch[!duplicated(aidmatch$fil),]

#aidmatch is no duplicates master key

aidmatchF<-aidmatch
aidmatchS<-aidmatch
aidmatchM1<-aidmatch
aidmatchM2<-aidmatch
aidmatchM3<-aidmatch
aidmatchM4<-aidmatch
aidmatchM5<-aidmatch
aidmatchM6<-aidmatch
aidmatchM7<-aidmatch
aidmatchM8<-aidmatch
aidmatchM9<-aidmatch
aidmatchM10<-aidmatch
aidmatchM11<-aidmatch
aidmatchM12<-aidmatch
aidmatchM13<-aidmatch
aidmatchM14<-aidmatch
aidmatchM15<-aidmatch

names(aidmatchF)<-c("ffil","funiqueID","fgender")
names(aidmatchS)<-c("sfil","suniqueID","sgender")
names(aidmatchM1)<-c("mfil1","muniqueID1","mgender1")
names(aidmatchM1)<-c('mfil1','muniqueID1','mgender1')
names(aidmatchM2)<-c('mfil2','muniqueID2','mgender2')
names(aidmatchM3)<-c('mfil3','muniqueID3','mgender3')
names(aidmatchM4)<-c('mfil4','muniqueID4','mgender4')
names(aidmatchM5)<-c('mfil5','muniqueID5','mgender5')
names(aidmatchM6)<-c('mfil6','muniqueID6','mgender6')
names(aidmatchM7)<-c('mfil7','muniqueID7','mgender7')
names(aidmatchM8)<-c('mfil8','muniqueID8','mgender8')
names(aidmatchM9)<-c('mfil9','muniqueID9','mgender9')
names(aidmatchM10)<-c('mfil10','muniqueID10','mgender10')
names(aidmatchM11)<-c('mfil11','muniqueID11','mgender11')
names(aidmatchM12)<-c('mfil12','muniqueID12','mgender12')
names(aidmatchM13)<-c('mfil13','muniqueID13','mgender13')
names(aidmatchM14)<-c('mfil14','muniqueID14','mgender14')
names(aidmatchM15)<-c('mfil15','muniqueID15','mgender15')


#ayo<-ayo
#ayo$funiqueID[is.na(ayo$funiqueID)]<-""
#ayo$suniqueID[is.na(ayo$suniqueID)]<-""


#get ayo each entry first mid author etc
ayo$ffil<-paste(ayo$firstAuthorF,ayo$firstAuthorI,ayo$firstAuthorL)
ayo$sfil<-paste(ayo$srAuthorF,ayo$srAuthorI,ayo$srAuthorL)
ayo$mfil1<-paste(ayo$midAuthorF1,ayo$midAuthorI1,ayo$midAuthorL1)
ayo$mfil2<-paste(ayo$midAuthorF2,ayo$midAuthorI2,ayo$midAuthorL2)
ayo$mfil3<-paste(ayo$midAuthorF3,ayo$midAuthorI3,ayo$midAuthorL3)
ayo$mfil4<-paste(ayo$midAuthorF4,ayo$midAuthorI4,ayo$midAuthorL4)
ayo$mfil5<-paste(ayo$midAuthorF5,ayo$midAuthorI5,ayo$midAuthorL5)
ayo$mfil6<-paste(ayo$midAuthorF6,ayo$midAuthorI6,ayo$midAuthorL6)
ayo$mfil7<-paste(ayo$midAuthorF7,ayo$midAuthorI7,ayo$midAuthorL7)
ayo$mfil8<-paste(ayo$midAuthorF8,ayo$midAuthorI8,ayo$midAuthorL8)
ayo$mfil9<-paste(ayo$midAuthorF9,ayo$midAuthorI9,ayo$midAuthorL9)
ayo$mfil10<-paste(ayo$midAuthorF10,ayo$midAuthorI10,ayo$midAuthorL10)
ayo$mfil11<-paste(ayo$midAuthorF11,ayo$midAuthorI11,ayo$midAuthorL11)
ayo$mfil12<-paste(ayo$midAuthorF12,ayo$midAuthorI12,ayo$midAuthorL12)
ayo$mfil13<-paste(ayo$midAuthorF13,ayo$midAuthorI13,ayo$midAuthorL13)
ayo$mfil14<-paste(ayo$midAuthorF14,ayo$midAuthorI14,ayo$midAuthorL14)
ayo$mfil15<-paste(ayo$midAuthorF15,ayo$midAuthorI15,ayo$midAuthorL15)
ayo$mfil<-ayo[,71:85]


ayo<-merge(ayo,aidmatchF,by.x="ffil",by.y="ffil",all.x=TRUE)
ayo<-merge(ayo,aidmatchS,by.x="sfil",by.y="sfil",all.x=TRUE)
ayo<-merge(ayo,aidmatchM1,by.x='mfil1',by.y='mfil1',all.x=TRUE)
ayo<-merge(ayo,aidmatchM2,by.x='mfil2',by.y='mfil2',all.x=TRUE)
ayo<-merge(ayo,aidmatchM3,by.x='mfil3',by.y='mfil3',all.x=TRUE)
ayo<-merge(ayo,aidmatchM4,by.x='mfil4',by.y='mfil4',all.x=TRUE)
ayo<-merge(ayo,aidmatchM5,by.x='mfil5',by.y='mfil5',all.x=TRUE)
ayo<-merge(ayo,aidmatchM6,by.x='mfil6',by.y='mfil6',all.x=TRUE)
ayo<-merge(ayo,aidmatchM7,by.x='mfil7',by.y='mfil7',all.x=TRUE)
ayo<-merge(ayo,aidmatchM8,by.x='mfil8',by.y='mfil8',all.x=TRUE)
ayo<-merge(ayo,aidmatchM9,by.x='mfil9',by.y='mfil9',all.x=TRUE)
ayo<-merge(ayo,aidmatchM10,by.x='mfil10',by.y='mfil10',all.x=TRUE)
ayo<-merge(ayo,aidmatchM11,by.x='mfil11',by.y='mfil11',all.x=TRUE)
ayo<-merge(ayo,aidmatchM12,by.x='mfil12',by.y='mfil12',all.x=TRUE)
ayo<-merge(ayo,aidmatchM13,by.x='mfil13',by.y='mfil13',all.x=TRUE)
ayo<-merge(ayo,aidmatchM14,by.x='mfil14',by.y='mfil14',all.x=TRUE)
ayo<-merge(ayo,aidmatchM15,by.x='mfil15',by.y='mfil15',all.x=TRUE)


ayo$uniqueIDs<-ayo[,c(seq(87,119,2))]


aidtime<-aidmatch[!is.na(aidmatch$gender),]
aidtime<-aidtime[-1]
aidtime<-aidtime[!duplicated(aidtime$uniqueID),]
#str(aidtime) 111365 authors, 50437 unique authors, 31095 unique authors with gender match
length(aidmatch$uniqueID[!duplicated(aidmatch$uniqueID)])


aidtime$firstCount<-sapply(aidtime$uniqueID,function(c)sum(ayo$funiqueID==c,na.rm=TRUE))
aidtime$lastCount<-sapply(aidtime$uniqueID,function(c)sum(ayo$suniqueID==c,na.rm=TRUE))
aidtime$midCount<-sapply(aidtime$uniqueID,function(c)sum(ayo$muniqueID1==c|ayo$muniqueID2==c|ayo$muniqueID3==c|ayo$muniqueID4==c|ayo$muniqueID5==c|ayo$muniqueID6==c|ayo$muniqueID7==c|ayo$muniqueID8==c|ayo$muniqueID9==c|ayo$muniqueID10==c|ayo$muniqueID11==c|ayo$muniqueID12==c|ayo$muniqueID13==c|ayo$muniqueID14==c,na.rm=TRUE))                    
aidtime$allCount<-aidtime$firstCount+aidtime$lastCount+aidtime$midCount

aidtime$firstCount15<-sapply(aidtime$uniqueID,function(c)sum(ayo$funiqueID[ayo$Year>2010]==c,na.rm=TRUE))
aidtime$lastCount15<-sapply(aidtime$uniqueID,function(c)sum(ayo$suniqueID[ayo$Year>2010]==c,na.rm=TRUE))
aidtime$midCount15<-sapply(aidtime$uniqueID,function(c)sum(ayo$Year>2010&(ayo$muniqueID1==c|ayo$muniqueID2==c|ayo$muniqueID3==c|ayo$muniqueID4==c|ayo$muniqueID5==c|ayo$muniqueID6==c|ayo$muniqueID7==c|ayo$muniqueID8==c|ayo$muniqueID9==c|ayo$muniqueID10==c|ayo$muniqueID11==c|ayo$muniqueID12==c|ayo$muniqueID13==c|ayo$muniqueID14==c),na.rm=TRUE))                    
aidtime$allCount15<-aidtime$firstCount15+aidtime$lastCount15+aidtime$midCount15


aidtimeF<-aidtime[aidtime$gender=="female",]
aidtimeM<-aidtime[aidtime$gender=="male",]

head(aidtimeF[order(-aidtimeF$allCount),],25)
head(aidtimeM[order(-aidtimeM$allCount),],25)

write.csv(head(aidtimeF[order(-aidtimeF$allCount),],25),'topfemale.csv')
write.csv(head(aidtimeM[order(-aidtimeM$allCount),],25),'topmale.csv')

write.csv(head(aidtimeF[order(-aidtimeF$allCount15),],25),'topfemale15.csv')
write.csv(head(aidtimeM[order(-aidtimeM$allCount15),],25),'topmale15.csv')



#ayo15<-ayo[ayo$Year=="2015",]
#ayo15$faF<-unlist(lapply(strsplit(ayo15$firstAuthorF," "), '[[',1))
#ayo15<-merge(ayo15,nameMF,by.x="faF",by.y="name",all.x=TRUE)


dim(aidtime[aidtime$allCount==0,])
#aidtime<-aidtime[aidtime$allCount!=0,]

#aidtime15<-aidtime[aidtime$allCount15!=0,]
library(epicalc)
use(aidtime)
avgpub<-tableStack(vars=c(firstCount,midCount,lastCount,allCount),iqr='none',by=gender)
#use(aidtime15)
#avgpub15<-tableStack(vars=c(firstCount15,midCount15,lastCount15,allCount15),iqr='none',by=gender)
write.csv(avgpub,'avgpub.csv')
#write.csv(avgpub15,'avgpub15.csv')

aidtime$midonly<-cut(aidtime$firstCount+aidtime$lastCount,breaks=c(-1,0.9,1.9,2.9,10000),labels=c("0","1","2","3 or more"))
#aidtime15$midonly<-cut(aidtime15$firstCount15+aidtime15$lastCount15,breaks=c(-1,0.9,1.9,2.9,10000),labels=c("0","1","2","3 or more"))
use(aidtime)
tableStack(vars=midonly,by=gender)
write.csv(tableStack(vars=midonly,by=gender),'midonly.csv')

#use(aidtime15)
#tableStack(vars=midonly,by=gender)

table(aidtime$midonly,aidtime$gender)
table(aidtime15$midonly,aidtime$gender)

aidtime$firstfirstY<-NA
aidtime$firstfirstY[aidtime$firstCount>0]<-sapply(aidtime$uniqueID[aidtime$firstCount>0],function(c) min(ayo$Year[ayo$funiqueID==c],na.rm=TRUE))
aidtimefirstfirstM<-NA
aidtime$firstfirstM[aidtime$firstCount>0]<-apply(rbind(aidtime$uniqueID[aidtime$firstCount>0],aidtime$firstfirstY[aidtime$firstCount>0]),2,function(c) min(ayo$Month[ayo$funiqueID==c[1] & ayo$Year==c[2]],na.rm=TRUE))

aidtime$lastfirstY<-NA
aidtime$lastfirstM<-NA
aidtime$lastfirstY[aidtime$firstCount>0]<-sapply(aidtime$uniqueID[aidtime$firstCount>0],function(c) max(ayo$Year[ayo$funiqueID==c],na.rm=TRUE))
aidtime$lastfirstM[aidtime$firstCount>0]<-apply(rbind(aidtime$uniqueID[aidtime$firstCount>0],aidtime$lastfirstY[aidtime$firstCount>0]),2,function(c) max(ayo$Month[ayo$funiqueID==c[1] & ayo$Year==c[2]],na.rm=TRUE))

aidtime$firstlastY<-NA
aidtime$firstlastM<-NA
aidtime$firstlastY[aidtime$lastCount>0]<-sapply(aidtime$uniqueID[aidtime$lastCount>0],function(c) min(ayo$Year[ayo$suniqueID==c],na.rm=TRUE))
aidtime$firstlastM[aidtime$lastCount>0]<-apply(rbind(aidtime$uniqueID[aidtime$lastCount>0],aidtime$firstlastY[aidtime$lastCount>0]),2,function(c) min(ayo$Month[ayo$suniqueID==c[1] & ayo$Year==c[2]],na.rm=TRUE))

aidtime$lastlastY<-NA
aidtime$lastlastM<-NA
aidtime$lastlastY[aidtime$lastCount>0]<-sapply(aidtime$uniqueID[aidtime$lastCount>0],function(c) max(ayo$Year[ayo$suniqueID==c],na.rm=TRUE))
aidtime$lastlastM[aidtime$lastCount>0]<-apply(rbind(aidtime$uniqueID[aidtime$lastCount>0],aidtime$lastlastY[aidtime$lastCount>0]),2,function(c) max(ayo$Month[ayo$suniqueID==c[1] & ayo$Year==c[2]],na.rm=TRUE))

#aidtime$first2first<-NA
#aidtime<-aidtime[aidtime$firstCount>0,]
#fltrue<-aidtime$firstCount>0&aidtime$lastCount>0
#aidtime$first2first[fltrue]<-(aidtime$firstlastY[fltrue]-aidtime$firstfirstY[fltrue])*12+(aidtime$firstlastM[fltrue]-aidtime$firstfirstM[fltrue])
#aidtime$first2first[aidtime$first2first<0]<-NA
#aidtime$first2first[(aidtime$firstlastY==aidtime$firstfirstY)&(aidtime$firstlastM==aidtime$firstfirstM)]<-NA

aidtime$ff2ll<-(aidtime$lastlastY-aidtime$firstfirstY)+(aidtime$lastlastM-aidtime$firstfirstM)/12
aidtime$fl2ll<-(aidtime$lastlastY-aidtime$firstlastY)+(aidtime$lastlastM-aidtime$firstlastM)/12
aidtime$ff2lf<-(aidtime$lastfirstY-aidtime$firstfirstY)+(aidtime$lastfirstM-aidtime$firstfirstM)/12
aidtime$fl2lf<-(aidtime$lastfirstY-aidtime$firstlastY)+(aidtime$lastfirstM-aidtime$firstlastM)/12

aidtime$firstpY<-apply(rbind(aidtime$firstlastY,aidtime$firstfirstY),2,function(c) min(c,na.rm=TRUE))
  
aidtime$firstpYR<-cut(aidtime$firstpY,breaks=c(0,1994.9,1999.9,2004.9,2009.9,2020),labels=c("1978-1994","1995-1999","2000-2004","2005-2009","2010-2015"))

aidtime$f2l<-apply(rbind(aidtime$ff2ll,aidtime$fl2ll,aidtime$ff2lf,aidtime$fl2lf),2,function(c) max(c,na.rm=TRUE))

aidtimef2l<-aidtime[aidtime$firstpY>1999.9 & aidtime$firstpY<2011.1,]
aidtimef2l$active<-aidtimef2l$f2l
aidtimef2l$active<-cut(aidtimef2l$active,breaks=c(-.1,1.99,2.99,3.99,4.99,20),labels=c("1 Yr or Less", "2 Yr", "3 Yr", "4 Yr", "5 Yr or More"))
aidtimef2l$active5<-aidtimef2l$f2l>4.99


aidtime$f2l5<-FALSE
aidtime$f2l5[aidtime$first2last>60]<-TRUE

aidtime$firstfirstYRG<-paste(aidtime$firstfirstYR,aidtime$gender)

write.csv(table(aidtime$firstfirstYRG,aidtime$f2f5),"yrg5f2f.csv")



ayo$yeargrp<-cut(ayo$Year,breaks=c(0,1994.9,1999.9,2004.9,2009.9,2020),labels=c("1978-1994","1995-1999","2000-2004","2005-2009","2010-2015"))


ayo$yeargrp<-cut(ayo$Year,breaks=c(0,1994.9,1999.9,2004.9,2009.9,2020),labels=c("1978-1994","1995-1999","2000-2004","2005-2009","2010-2015"))
write.csv(table(ayo$fgender,ayo$yeargrp),"fgenderyr.csv")
write.csv(table(ayo$sgender,ayo$yeargrp),"sgenderyr.csv")
write.csv(
  
  table(ayo$fgender,ayo$yeargrp)+table(ayo$sgender,ayo$yeargrp)+table(ayo$mgender1,ayo$yeargrp)+
            table(ayo$mgender2,ayo$yeargrp)+
            table(ayo$mgender3,ayo$yeargrp)+
            table(ayo$mgender4,ayo$yeargrp)+
            table(ayo$mgender5,ayo$yeargrp)+
            table(ayo$mgender6,ayo$yeargrp)+
            table(ayo$mgender7,ayo$yeargrp)+
            table(ayo$mgender8,ayo$yeargrp)+
            table(ayo$mgender9,ayo$yeargrp)+
            table(ayo$mgender10,ayo$yeargrp)+
            table(ayo$mgender11,ayo$yeargrp)+
            table(ayo$mgender12,ayo$yeargrp)+
            table(ayo$mgender13,ayo$yeargrp)+
            table(ayo$mgender14,ayo$yeargrp)+
            table(ayo$mgender15,ayo$yeargrp)
,"allgenderyr.csv")


tableall<-table(ayo$fgender,ayo$yeargrp)+table(ayo$sgender,ayo$yeargrp)+table(ayo$mgender1,ayo$yeargrp)+
  table(ayo$mgender2,ayo$yeargrp)+
  table(ayo$mgender3,ayo$yeargrp)+
  table(ayo$mgender4,ayo$yeargrp)+
  table(ayo$mgender5,ayo$yeargrp)+
  table(ayo$mgender6,ayo$yeargrp)+
  table(ayo$mgender7,ayo$yeargrp)+
  table(ayo$mgender8,ayo$yeargrp)+
  table(ayo$mgender9,ayo$yeargrp)+
  table(ayo$mgender10,ayo$yeargrp)+
  table(ayo$mgender11,ayo$yeargrp)+
  table(ayo$mgender12,ayo$yeargrp)+
  table(ayo$mgender13,ayo$yeargrp)+
  table(ayo$mgender14,ayo$yeargrp)

write.csv(tableall,"allgenderyr.csv")


tablemid<-table(ayo$mgender1,ayo$yeargrp)+
  table(ayo$mgender2,ayo$yeargrp)+
  table(ayo$mgender3,ayo$yeargrp)+
  table(ayo$mgender4,ayo$yeargrp)+
  table(ayo$mgender5,ayo$yeargrp)+
  table(ayo$mgender6,ayo$yeargrp)+
  table(ayo$mgender7,ayo$yeargrp)+
  table(ayo$mgender8,ayo$yeargrp)+
  table(ayo$mgender9,ayo$yeargrp)+
  table(ayo$mgender10,ayo$yeargrp)+
  table(ayo$mgender11,ayo$yeargrp)+
  table(ayo$mgender12,ayo$yeargrp)+
  table(ayo$mgender13,ayo$yeargrp)+
  table(ayo$mgender14,ayo$yeargrp)
write.csv(tablemid,"midgenderyr.csv")

sum(sum(!is.na(ayo$fgender))
    ,sum(!is.na(ayo$mgender1))
    ,sum(!is.na(ayo$mgender2))
    ,sum(!is.na(ayo$mgender3))
    ,sum(!is.na(ayo$mgender4))
    ,sum(!is.na(ayo$mgender5))
    ,sum(!is.na(ayo$mgender6))
    ,sum(!is.na(ayo$mgender7))
    ,sum(!is.na(ayo$mgender8))
    ,sum(!is.na(ayo$mgender9))
    ,sum(!is.na(ayo$mgender10))
    ,sum(!is.na(ayo$mgender11))
    ,sum(!is.na(ayo$mgender12))
    ,sum(!is.na(ayo$mgender13))
    ,sum(!is.na(ayo$mgender14))
    ,sum(!is.na(ayo$mgender15))
    ,sum(!is.na(ayo$sgender))
)    

ayofirst<-data.frame(uniqueID=ayo$funiqueID,Year=ayo$Year,Month=ayo$Month,pos='first')
ayomid1<-data.frame(uniqueID=ayo$muniqueID1,Year=ayo$Year,Month=ayo$Month,pos='mid')
ayomid2<-data.frame(uniqueID=ayo$muniqueID2,Year=ayo$Year,Month=ayo$Month,pos='mid')
ayomid3<-data.frame(uniqueID=ayo$muniqueID3,Year=ayo$Year,Month=ayo$Month,pos='mid')
ayomid4<-data.frame(uniqueID=ayo$muniqueID4,Year=ayo$Year,Month=ayo$Month,pos='mid')
ayomid5<-data.frame(uniqueID=ayo$muniqueID5,Year=ayo$Year,Month=ayo$Month,pos='mid')
ayomid6<-data.frame(uniqueID=ayo$muniqueID6,Year=ayo$Year,Month=ayo$Month,pos='mid')
ayomid7<-data.frame(uniqueID=ayo$muniqueID7,Year=ayo$Year,Month=ayo$Month,pos='mid')
ayomid8<-data.frame(uniqueID=ayo$muniqueID8,Year=ayo$Year,Month=ayo$Month,pos='mid')
ayomid9<-data.frame(uniqueID=ayo$muniqueID9,Year=ayo$Year,Month=ayo$Month,pos='mid')
ayomid10<-data.frame(uniqueID=ayo$muniqueID10,Year=ayo$Year,Month=ayo$Month,pos='mid')
ayomid11<-data.frame(uniqueID=ayo$muniqueID11,Year=ayo$Year,Month=ayo$Month,pos='mid')
ayomid12<-data.frame(uniqueID=ayo$muniqueID12,Year=ayo$Year,Month=ayo$Month,pos='mid')
ayomid13<-data.frame(uniqueID=ayo$muniqueID13,Year=ayo$Year,Month=ayo$Month,pos='mid')
ayomid14<-data.frame(uniqueID=ayo$muniqueID14,Year=ayo$Year,Month=ayo$Month,pos='mid')
ayolast<-data.frame(uniqueID=ayo$suniqueID,Year=ayo$Year,Month=ayo$Month,pos='last')

ayoauths<-rbind(ayofirst,ayomid1,ayomid2,ayomid3,ayomid4,ayomid5,ayomid6,ayomid7,ayomid8,ayomid9,ayomid10,ayomid11,ayomid12,ayomid13,ayomid14,ayolast)
ayoauths$uniqueID<-as.character(ayoauths$uniqueID)
ayoauths$YM<-ayoauths$Year*12+ayoauths$Month
ayoauths<-ayoauths[!is.na(ayoauths$uniqueID),]
aidtimeR<-data.frame(uniqueID=aidtime$uniqueID,gender=aidtime$gender,stringsAsFactors=FALSE)
aidtimeR$minpub<-sapply(aidtimeR$uniqueID,function(c) min(ayoauths$YM[ayoauths$uniqueID==c],na.rm=TRUE))
aidtimeR<-aidtimeR[aidtimeR$minpub>24000&aidtimeR$minpub<=24120,]
aidtimeR$maxpub<-sapply(aidtimeR$uniqueID,function(c) max(ayoauths$YM[ayoauths$uniqueID==c],na.rm=TRUE))
aidtimeR$morethan5<-(aidtimeR$maxpub-aidtimeR$minpub)>=60
aidtimeR$onlyone<-aidtimeR$maxpub==aidtimeR$minpub
use(aidtimeR)
tableStack(vars=morethan5,by=gender)
write.csv(tableStack(vars=c(morethan5,onlyone),by=gender),'morethan5.csv')

aidtimeR$surv5<-aidtimeR$maxpub-aidtimeR$minpub
aidtimeR$surv5[aidtimeR$surv5>=60]<-60
aidtimeR$status<-2
aidtimeR$status[aidtimeR$surv5==60]<-1
aidtimeR$gender<-relevel(factor(aidtimeR$gender),ref="female")
aidtime.surv<-survfit(Surv(surv5,status)~gender,data=aidtimeR)

library(GGally)
library(ggplot2)
p<-ggsurv(aidtime.surv)+xlab("Time (Months)")+scale_color_manual(values=c("#0000FF", "#FF0000"))+ scale_y_continuous(limits = c(0, 1.00))
+scale_colour_discrete(name='Gender',labels=c('Female','Male'))




