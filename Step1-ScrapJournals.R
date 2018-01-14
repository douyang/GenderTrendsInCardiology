#setwd('D:\\orthoresearch\\scrape')
setwd('C:\\Users\\David\\Dropbox\\StatisticalAnalysis\\Female Cardiologists')
#install.packages("RISmed")
library(RISmed)

#searchjournals<-c('"Spine J"[Journal]','"Spine (Phila Pa 1976)" [Journal]','"J Neurosurg Spine"[Journal]','"Eur Spine J"[Journal]','"J Spinal Disord Tech"[Journal]','"Clin Spine Surg"[Journal]')

searchjournals<-c('"J Am Coll Cardiol"[Journal]','"Circulation" [Journal]','"Eur Heart J"[Journal]')


####  
search<-searchjournals[1]
query<-EUtilsSummary(search,retmax=32000)
Spinerecords<-EUtilsGet(query)

authorlist<-Author(Spinerecords)	

impAuthor<-function(authorlist){
  authors<-data.frame(matrix(ncol=52,nrow=length(authorlist)))
  colnames(authors)<-c('firstAuthorL','firstAuthorF','firstAuthorI','srAuthorL','srAuthorF','srAuthorI'
                       ,'midAuthorL1','midAuthorF1','midAuthorI1',
                       'midAuthorL2','midAuthorF2','midAuthorI2',
                       'midAuthorL3','midAuthorF3','midAuthorI3',
                       'midAuthorL4','midAuthorF4','midAuthorI4',
                       'midAuthorL5','midAuthorF5','midAuthorI5',
                       'midAuthorL6','midAuthorF6','midAuthorI6',
                       'midAuthorL7','midAuthorF7','midAuthorI7',
                       'midAuthorL8','midAuthorF8','midAuthorI8',
                       'midAuthorL9','midAuthorF9','midAuthorI9',
                       'midAuthorL10','midAuthorF10','midAuthorI10',
                       'midAuthorL11','midAuthorF11','midAuthorI11',
                       'midAuthorL12','midAuthorF12','midAuthorI12',
                       'midAuthorL13','midAuthorF13','midAuthorI13',
                       'midAuthorL14','midAuthorF14','midAuthorI14',
                       'midAuthorL15','midAuthorF15','midAuthorI15'
                       ,'numAuthor')
  for (i in 1:length(authorlist)){
    authors[i,]$firstAuthorL<-head(authorlist[[i]],n=1)$LastName
    authors[i,]$firstAuthorF<-head(authorlist[[i]],n=1)$ForeName
    authors[i,]$firstAuthorI<-head(authorlist[[i]],n=1)$Initials
    
    authors[i,]$srAuthorL<-tail(authorlist[[i]],n=1)$LastName
    authors[i,]$srAuthorF<-tail(authorlist[[i]],n=1)$ForeName
    authors[i,]$srAuthorI<-tail(authorlist[[i]],n=1)$Initials
    authors[i,]$numAuthor<-tail(authorlist[[i]],n=1)$order
    if (!is.na(tail(authorlist[[i]],n=1))&tail(authorlist[[i]],n=1)$order>2){
    for (j in 2:min(c(dim(authorlist[[i]])[1]-1),15)){

      authors[i,][[paste("midAuthorL",as.character(j-1),sep="")]]<-authorlist[[i]][j,]$LastName
      authors[i,][[paste("midAuthorF",as.character(j-1),sep="")]]<-authorlist[[i]][j,]$ForeName
      authors[i,][[paste("midAuthorI",as.character(j-1),sep="")]]<-authorlist[[i]][j,]$Initials
    }
    }
    
  }
  authors
}
authors<-impAuthor(authorlist)

typelist<-PublicationType(Spinerecords)

type<-function(typelist){
  types<-data.frame(matrix(ncol=3,nrow=length(typelist)))
  colnames(types)<-c('type1','type2','type3')
  for (i in 1:length(typelist)){
    types[i,]$type1<-array(unlist(typelist[[i]][1]))
    types[i,]$type2<-array(unlist(typelist[[i]][2]))
    types[i,]$type3<-array(unlist(typelist[[i]][3]))
    
  }
  types
}

pubtype<-type(typelist)

Spine<-data.frame('PMID'=PMID(Spinerecords)
                  ,'Journal'=Title(Spinerecords)
                  ,'Title'=ArticleTitle(Spinerecords)
                  ,'Country'=Country(Spinerecords)
                  ,'firstAuthorL'=authors$firstAuthorL
                  ,'firstAuthorF'=authors$firstAuthorF
                  ,'firstAuthorI'=authors$firstAuthorI
                  ,'srAuthorL'=authors$srAuthorL
                  ,'srAuthorF'=authors$srAuthorF
                  ,'srAuthorI'=authors$srAuthorI
                  ,'numAuthor'=authors$numAuthor
                  ,'midAuthorL1'=authors$midAuthorL1,'midAuthorF1'=authors$midAuthorF1,'midAuthorI1'=authors$midAuthorI1,
                  'midAuthorL2'=authors$midAuthorL2,'midAuthorF2'=authors$midAuthorF2,'midAuthorI2'=authors$midAuthorI2,
                  'midAuthorL3'=authors$midAuthorL3,'midAuthorF3'=authors$midAuthorF3,'midAuthorI3'=authors$midAuthorI3,
                  'midAuthorL4'=authors$midAuthorL4,'midAuthorF4'=authors$midAuthorF4,'midAuthorI4'=authors$midAuthorI4,
                  'midAuthorL5'=authors$midAuthorL5,'midAuthorF5'=authors$midAuthorF5,'midAuthorI5'=authors$midAuthorI5,
                  'midAuthorL6'=authors$midAuthorL6,'midAuthorF6'=authors$midAuthorF6,'midAuthorI6'=authors$midAuthorI6,
                  'midAuthorL7'=authors$midAuthorL7,'midAuthorF7'=authors$midAuthorF7,'midAuthorI7'=authors$midAuthorI7,
                  'midAuthorL8'=authors$midAuthorL8,'midAuthorF8'=authors$midAuthorF8,'midAuthorI8'=authors$midAuthorI8,
                  'midAuthorL9'=authors$midAuthorL9,'midAuthorF9'=authors$midAuthorF9,'midAuthorI9'=authors$midAuthorI9,
                  'midAuthorL10'=authors$midAuthorL10,'midAuthorF10'=authors$midAuthorF10,'midAuthorI10'=authors$midAuthorI10,
                  'midAuthorL11'=authors$midAuthorL11,'midAuthorF11'=authors$midAuthorF11,'midAuthorI11'=authors$midAuthorI11,
                  'midAuthorL12'=authors$midAuthorL12,'midAuthorF12'=authors$midAuthorF12,'midAuthorI12'=authors$midAuthorI12,
                  'midAuthorL13'=authors$midAuthorL13,'midAuthorF13'=authors$midAuthorF13,'midAuthorI13'=authors$midAuthorI13,
                  'midAuthorL14'=authors$midAuthorL14,'midAuthorF14'=authors$midAuthorF14,'midAuthorI14'=authors$midAuthorI14,
                  'midAuthorL15'=authors$midAuthorL15,'midAuthorF15'=authors$midAuthorF15,'midAuthorI15'=authors$midAuthorI15
                  
                  ,'Type1'=pubtype$type1
                  ,'Type2'=pubtype$type2
                  ,'Type3'=pubtype$type3
                  ,'Year'=YearPubmed(Spinerecords)
                  ,'Month'=MonthPubmed(Spinerecords)
                  )

write.table(Spine,'JACC.txt',sep="\t")

####  
search<-searchjournals[2]
query<-EUtilsSummary(search,retmax=32000)
Spinerecords<-EUtilsGet(query)

authorlist<-Author(Spinerecords)

impAuthor<-function(authorlist){
  authors<-data.frame(matrix(ncol=52,nrow=length(authorlist)))
  colnames(authors)<-c('firstAuthorL','firstAuthorF','firstAuthorI','srAuthorL','srAuthorF','srAuthorI'
                       ,'midAuthorL1','midAuthorF1','midAuthorI1',
                       'midAuthorL2','midAuthorF2','midAuthorI2',
                       'midAuthorL3','midAuthorF3','midAuthorI3',
                       'midAuthorL4','midAuthorF4','midAuthorI4',
                       'midAuthorL5','midAuthorF5','midAuthorI5',
                       'midAuthorL6','midAuthorF6','midAuthorI6',
                       'midAuthorL7','midAuthorF7','midAuthorI7',
                       'midAuthorL8','midAuthorF8','midAuthorI8',
                       'midAuthorL9','midAuthorF9','midAuthorI9',
                       'midAuthorL10','midAuthorF10','midAuthorI10',
                       'midAuthorL11','midAuthorF11','midAuthorI11',
                       'midAuthorL12','midAuthorF12','midAuthorI12',
                       'midAuthorL13','midAuthorF13','midAuthorI13',
                       'midAuthorL14','midAuthorF14','midAuthorI14',
                       'midAuthorL15','midAuthorF15','midAuthorI15'
                       ,'numAuthor')
  for (i in 1:length(authorlist)){
    authors[i,]$firstAuthorL<-head(authorlist[[i]],n=1)$LastName
    authors[i,]$firstAuthorF<-head(authorlist[[i]],n=1)$ForeName
    authors[i,]$firstAuthorI<-head(authorlist[[i]],n=1)$Initials
    
    authors[i,]$srAuthorL<-tail(authorlist[[i]],n=1)$LastName
    authors[i,]$srAuthorF<-tail(authorlist[[i]],n=1)$ForeName
    authors[i,]$srAuthorI<-tail(authorlist[[i]],n=1)$Initials
    authors[i,]$numAuthor<-tail(authorlist[[i]],n=1)$order
    if (!is.na(tail(authorlist[[i]],n=1))&tail(authorlist[[i]],n=1)$order>2){
      for (j in 2:min(c(dim(authorlist[[i]])[1]-1),15)){
        
        authors[i,][[paste("midAuthorL",as.character(j-1),sep="")]]<-authorlist[[i]][j,]$LastName
        authors[i,][[paste("midAuthorF",as.character(j-1),sep="")]]<-authorlist[[i]][j,]$ForeName
        authors[i,][[paste("midAuthorI",as.character(j-1),sep="")]]<-authorlist[[i]][j,]$Initials
      }
    }
    
  }
  authors
}
authors<-impAuthor(authorlist)

typelist<-PublicationType(Spinerecords)

type<-function(typelist){
  types<-data.frame(matrix(ncol=3,nrow=length(typelist)))
  colnames(types)<-c('type1','type2','type3')
  for (i in 1:length(typelist)){
    types[i,]$type1<-array(unlist(typelist[[i]][1]))
    types[i,]$type2<-array(unlist(typelist[[i]][2]))
    types[i,]$type3<-array(unlist(typelist[[i]][3]))
    
  }
  types
}

pubtype<-type(typelist)

Spine<-data.frame('PMID'=PMID(Spinerecords)
                  ,'Journal'=Title(Spinerecords)
                  ,'Title'=ArticleTitle(Spinerecords)
                  ,'Country'=Country(Spinerecords)
                  ,'firstAuthorL'=authors$firstAuthorL
                  ,'firstAuthorF'=authors$firstAuthorF
                  ,'firstAuthorI'=authors$firstAuthorI
                  ,'srAuthorL'=authors$srAuthorL
                  ,'srAuthorF'=authors$srAuthorF
                  ,'srAuthorI'=authors$srAuthorI
                  ,'numAuthor'=authors$numAuthor
                  ,'midAuthorL1'=authors$midAuthorL1,'midAuthorF1'=authors$midAuthorF1,'midAuthorI1'=authors$midAuthorI1,
                  'midAuthorL2'=authors$midAuthorL2,'midAuthorF2'=authors$midAuthorF2,'midAuthorI2'=authors$midAuthorI2,
                  'midAuthorL3'=authors$midAuthorL3,'midAuthorF3'=authors$midAuthorF3,'midAuthorI3'=authors$midAuthorI3,
                  'midAuthorL4'=authors$midAuthorL4,'midAuthorF4'=authors$midAuthorF4,'midAuthorI4'=authors$midAuthorI4,
                  'midAuthorL5'=authors$midAuthorL5,'midAuthorF5'=authors$midAuthorF5,'midAuthorI5'=authors$midAuthorI5,
                  'midAuthorL6'=authors$midAuthorL6,'midAuthorF6'=authors$midAuthorF6,'midAuthorI6'=authors$midAuthorI6,
                  'midAuthorL7'=authors$midAuthorL7,'midAuthorF7'=authors$midAuthorF7,'midAuthorI7'=authors$midAuthorI7,
                  'midAuthorL8'=authors$midAuthorL8,'midAuthorF8'=authors$midAuthorF8,'midAuthorI8'=authors$midAuthorI8,
                  'midAuthorL9'=authors$midAuthorL9,'midAuthorF9'=authors$midAuthorF9,'midAuthorI9'=authors$midAuthorI9,
                  'midAuthorL10'=authors$midAuthorL10,'midAuthorF10'=authors$midAuthorF10,'midAuthorI10'=authors$midAuthorI10,
                  'midAuthorL11'=authors$midAuthorL11,'midAuthorF11'=authors$midAuthorF11,'midAuthorI11'=authors$midAuthorI11,
                  'midAuthorL12'=authors$midAuthorL12,'midAuthorF12'=authors$midAuthorF12,'midAuthorI12'=authors$midAuthorI12,
                  'midAuthorL13'=authors$midAuthorL13,'midAuthorF13'=authors$midAuthorF13,'midAuthorI13'=authors$midAuthorI13,
                  'midAuthorL14'=authors$midAuthorL14,'midAuthorF14'=authors$midAuthorF14,'midAuthorI14'=authors$midAuthorI14,
                  'midAuthorL15'=authors$midAuthorL15,'midAuthorF15'=authors$midAuthorF15,'midAuthorI15'=authors$midAuthorI15
                  
                  ,'Type1'=pubtype$type1
                  ,'Type2'=pubtype$type2
                  ,'Type3'=pubtype$type3
                  ,'Year'=YearPubmed(Spinerecords)
                  ,'Month'=MonthPubmed(Spinerecords)
)

write.table(Spine,'Circ.txt',sep="\t")

####  
search<-searchjournals[3]
query<-EUtilsSummary(search,retmax=32000)
Spinerecords<-EUtilsGet(query)

authorlist<-Author(Spinerecords)

impAuthor<-function(authorlist){
  authors<-data.frame(matrix(ncol=52,nrow=length(authorlist)))
  colnames(authors)<-c('firstAuthorL','firstAuthorF','firstAuthorI','srAuthorL','srAuthorF','srAuthorI'
                       ,'midAuthorL1','midAuthorF1','midAuthorI1',
                       'midAuthorL2','midAuthorF2','midAuthorI2',
                       'midAuthorL3','midAuthorF3','midAuthorI3',
                       'midAuthorL4','midAuthorF4','midAuthorI4',
                       'midAuthorL5','midAuthorF5','midAuthorI5',
                       'midAuthorL6','midAuthorF6','midAuthorI6',
                       'midAuthorL7','midAuthorF7','midAuthorI7',
                       'midAuthorL8','midAuthorF8','midAuthorI8',
                       'midAuthorL9','midAuthorF9','midAuthorI9',
                       'midAuthorL10','midAuthorF10','midAuthorI10',
                       'midAuthorL11','midAuthorF11','midAuthorI11',
                       'midAuthorL12','midAuthorF12','midAuthorI12',
                       'midAuthorL13','midAuthorF13','midAuthorI13',
                       'midAuthorL14','midAuthorF14','midAuthorI14',
                       'midAuthorL15','midAuthorF15','midAuthorI15'
                       ,'numAuthor')
  for (i in 1:length(authorlist)){
    authors[i,]$firstAuthorL<-head(authorlist[[i]],n=1)$LastName
    authors[i,]$firstAuthorF<-head(authorlist[[i]],n=1)$ForeName
    authors[i,]$firstAuthorI<-head(authorlist[[i]],n=1)$Initials
    
    authors[i,]$srAuthorL<-tail(authorlist[[i]],n=1)$LastName
    authors[i,]$srAuthorF<-tail(authorlist[[i]],n=1)$ForeName
    authors[i,]$srAuthorI<-tail(authorlist[[i]],n=1)$Initials
    authors[i,]$numAuthor<-tail(authorlist[[i]],n=1)$order
    if (!is.na(tail(authorlist[[i]],n=1))&tail(authorlist[[i]],n=1)$order>2){
      for (j in 2:min(c(dim(authorlist[[i]])[1]-1),15)){
        
        authors[i,][[paste("midAuthorL",as.character(j-1),sep="")]]<-authorlist[[i]][j,]$LastName
        authors[i,][[paste("midAuthorF",as.character(j-1),sep="")]]<-authorlist[[i]][j,]$ForeName
        authors[i,][[paste("midAuthorI",as.character(j-1),sep="")]]<-authorlist[[i]][j,]$Initials
      }
    }
    
  }
  authors
}
authors<-impAuthor(authorlist)

typelist<-PublicationType(Spinerecords)

type<-function(typelist){
  types<-data.frame(matrix(ncol=3,nrow=length(typelist)))
  colnames(types)<-c('type1','type2','type3')
  for (i in 1:length(typelist)){
    types[i,]$type1<-array(unlist(typelist[[i]][1]))
    types[i,]$type2<-array(unlist(typelist[[i]][2]))
    types[i,]$type3<-array(unlist(typelist[[i]][3]))
    
  }
  types
}

pubtype<-type(typelist)

Spine<-data.frame('PMID'=PMID(Spinerecords)
                  ,'Journal'=Title(Spinerecords)
                  ,'Title'=ArticleTitle(Spinerecords)
                  ,'Country'=Country(Spinerecords)
                  ,'firstAuthorL'=authors$firstAuthorL
                  ,'firstAuthorF'=authors$firstAuthorF
                  ,'firstAuthorI'=authors$firstAuthorI
                  ,'srAuthorL'=authors$srAuthorL
                  ,'srAuthorF'=authors$srAuthorF
                  ,'srAuthorI'=authors$srAuthorI
                  ,'numAuthor'=authors$numAuthor
                  ,'midAuthorL1'=authors$midAuthorL1,'midAuthorF1'=authors$midAuthorF1,'midAuthorI1'=authors$midAuthorI1,
                  'midAuthorL2'=authors$midAuthorL2,'midAuthorF2'=authors$midAuthorF2,'midAuthorI2'=authors$midAuthorI2,
                  'midAuthorL3'=authors$midAuthorL3,'midAuthorF3'=authors$midAuthorF3,'midAuthorI3'=authors$midAuthorI3,
                  'midAuthorL4'=authors$midAuthorL4,'midAuthorF4'=authors$midAuthorF4,'midAuthorI4'=authors$midAuthorI4,
                  'midAuthorL5'=authors$midAuthorL5,'midAuthorF5'=authors$midAuthorF5,'midAuthorI5'=authors$midAuthorI5,
                  'midAuthorL6'=authors$midAuthorL6,'midAuthorF6'=authors$midAuthorF6,'midAuthorI6'=authors$midAuthorI6,
                  'midAuthorL7'=authors$midAuthorL7,'midAuthorF7'=authors$midAuthorF7,'midAuthorI7'=authors$midAuthorI7,
                  'midAuthorL8'=authors$midAuthorL8,'midAuthorF8'=authors$midAuthorF8,'midAuthorI8'=authors$midAuthorI8,
                  'midAuthorL9'=authors$midAuthorL9,'midAuthorF9'=authors$midAuthorF9,'midAuthorI9'=authors$midAuthorI9,
                  'midAuthorL10'=authors$midAuthorL10,'midAuthorF10'=authors$midAuthorF10,'midAuthorI10'=authors$midAuthorI10,
                  'midAuthorL11'=authors$midAuthorL11,'midAuthorF11'=authors$midAuthorF11,'midAuthorI11'=authors$midAuthorI11,
                  'midAuthorL12'=authors$midAuthorL12,'midAuthorF12'=authors$midAuthorF12,'midAuthorI12'=authors$midAuthorI12,
                  'midAuthorL13'=authors$midAuthorL13,'midAuthorF13'=authors$midAuthorF13,'midAuthorI13'=authors$midAuthorI13,
                  'midAuthorL14'=authors$midAuthorL14,'midAuthorF14'=authors$midAuthorF14,'midAuthorI14'=authors$midAuthorI14,
                  'midAuthorL15'=authors$midAuthorL15,'midAuthorF15'=authors$midAuthorF15,'midAuthorI15'=authors$midAuthorI15
                  
                  ,'Type1'=pubtype$type1
                  ,'Type2'=pubtype$type2
                  ,'Type3'=pubtype$type3
                  ,'Year'=YearPubmed(Spinerecords)
                  ,'Month'=MonthPubmed(Spinerecords)
)

write.table(Spine,'EuroHeartJ.txt',sep="\t")





###### END HERE FOR NOW #####
#### JUST THREE JOURNALS ####






###
search<-searchjournals[4]
query<-EUtilsSummary(search,retmax=32000)
Spinerecords<-EUtilsGet(query)

authorlist<-Author(Spinerecords)

impAuthor<-function(authorlist){
  authors<-data.frame(matrix(ncol=52,nrow=length(authorlist)))
  colnames(authors)<-c('firstAuthorL','firstAuthorF','firstAuthorI','srAuthorL','srAuthorF','srAuthorI'
                       ,'midAuthorL1','midAuthorF1','midAuthorI1',
                       'midAuthorL2','midAuthorF2','midAuthorI2',
                       'midAuthorL3','midAuthorF3','midAuthorI3',
                       'midAuthorL4','midAuthorF4','midAuthorI4',
                       'midAuthorL5','midAuthorF5','midAuthorI5',
                       'midAuthorL6','midAuthorF6','midAuthorI6',
                       'midAuthorL7','midAuthorF7','midAuthorI7',
                       'midAuthorL8','midAuthorF8','midAuthorI8',
                       'midAuthorL9','midAuthorF9','midAuthorI9',
                       'midAuthorL10','midAuthorF10','midAuthorI10',
                       'midAuthorL11','midAuthorF11','midAuthorI11',
                       'midAuthorL12','midAuthorF12','midAuthorI12',
                       'midAuthorL13','midAuthorF13','midAuthorI13',
                       'midAuthorL14','midAuthorF14','midAuthorI14',
                       'midAuthorL15','midAuthorF15','midAuthorI15'
                       ,'numAuthor')
  for (i in 1:length(authorlist)){
    authors[i,]$firstAuthorL<-head(authorlist[[i]],n=1)$LastName
    authors[i,]$firstAuthorF<-head(authorlist[[i]],n=1)$ForeName
    authors[i,]$firstAuthorI<-head(authorlist[[i]],n=1)$Initials
    
    authors[i,]$srAuthorL<-tail(authorlist[[i]],n=1)$LastName
    authors[i,]$srAuthorF<-tail(authorlist[[i]],n=1)$ForeName
    authors[i,]$srAuthorI<-tail(authorlist[[i]],n=1)$Initials
    authors[i,]$numAuthor<-tail(authorlist[[i]],n=1)$order
    if (!is.na(tail(authorlist[[i]],n=1))&tail(authorlist[[i]],n=1)$order>2){
      for (j in 2:min(c(dim(authorlist[[i]])[1]-1),15)){
        
        authors[i,][[paste("midAuthorL",as.character(j-1),sep="")]]<-authorlist[[i]][j,]$LastName
        authors[i,][[paste("midAuthorF",as.character(j-1),sep="")]]<-authorlist[[i]][j,]$ForeName
        authors[i,][[paste("midAuthorI",as.character(j-1),sep="")]]<-authorlist[[i]][j,]$Initials
      }
    }
    
  }
  authors
}
authors<-impAuthor(authorlist)

typelist<-PublicationType(Spinerecords)

type<-function(typelist){
  types<-data.frame(matrix(ncol=3,nrow=length(typelist)))
  colnames(types)<-c('type1','type2','type3')
  for (i in 1:length(typelist)){
    types[i,]$type1<-array(unlist(typelist[[i]][1]))
    types[i,]$type2<-array(unlist(typelist[[i]][2]))
    types[i,]$type3<-array(unlist(typelist[[i]][3]))
    
  }
  types
}

pubtype<-type(typelist)

Spine<-data.frame('PMID'=PMID(Spinerecords)
                  ,'Journal'=Title(Spinerecords)
                  ,'Title'=ArticleTitle(Spinerecords)
                  ,'Country'=Country(Spinerecords)
                  ,'firstAuthorL'=authors$firstAuthorL
                  ,'firstAuthorF'=authors$firstAuthorF
                  ,'firstAuthorI'=authors$firstAuthorI
                  ,'srAuthorL'=authors$srAuthorL
                  ,'srAuthorF'=authors$srAuthorF
                  ,'srAuthorI'=authors$srAuthorI
                  ,'numAuthor'=authors$numAuthor
                  ,'midAuthorL1'=authors$midAuthorL1,'midAuthorF1'=authors$midAuthorF1,'midAuthorI1'=authors$midAuthorI1,
                  'midAuthorL2'=authors$midAuthorL2,'midAuthorF2'=authors$midAuthorF2,'midAuthorI2'=authors$midAuthorI2,
                  'midAuthorL3'=authors$midAuthorL3,'midAuthorF3'=authors$midAuthorF3,'midAuthorI3'=authors$midAuthorI3,
                  'midAuthorL4'=authors$midAuthorL4,'midAuthorF4'=authors$midAuthorF4,'midAuthorI4'=authors$midAuthorI4,
                  'midAuthorL5'=authors$midAuthorL5,'midAuthorF5'=authors$midAuthorF5,'midAuthorI5'=authors$midAuthorI5,
                  'midAuthorL6'=authors$midAuthorL6,'midAuthorF6'=authors$midAuthorF6,'midAuthorI6'=authors$midAuthorI6,
                  'midAuthorL7'=authors$midAuthorL7,'midAuthorF7'=authors$midAuthorF7,'midAuthorI7'=authors$midAuthorI7,
                  'midAuthorL8'=authors$midAuthorL8,'midAuthorF8'=authors$midAuthorF8,'midAuthorI8'=authors$midAuthorI8,
                  'midAuthorL9'=authors$midAuthorL9,'midAuthorF9'=authors$midAuthorF9,'midAuthorI9'=authors$midAuthorI9,
                  'midAuthorL10'=authors$midAuthorL10,'midAuthorF10'=authors$midAuthorF10,'midAuthorI10'=authors$midAuthorI10,
                  'midAuthorL11'=authors$midAuthorL11,'midAuthorF11'=authors$midAuthorF11,'midAuthorI11'=authors$midAuthorI11,
                  'midAuthorL12'=authors$midAuthorL12,'midAuthorF12'=authors$midAuthorF12,'midAuthorI12'=authors$midAuthorI12,
                  'midAuthorL13'=authors$midAuthorL13,'midAuthorF13'=authors$midAuthorF13,'midAuthorI13'=authors$midAuthorI13,
                  'midAuthorL14'=authors$midAuthorL14,'midAuthorF14'=authors$midAuthorF14,'midAuthorI14'=authors$midAuthorI14,
                  'midAuthorL15'=authors$midAuthorL15,'midAuthorF15'=authors$midAuthorF15,'midAuthorI15'=authors$midAuthorI15
                  
                  ,'Type1'=pubtype$type1
                  ,'Type2'=pubtype$type2
                  ,'Type3'=pubtype$type3
                  ,'Year'=YearPubmed(Spinerecords)
                  ,'Month'=MonthPubmed(Spinerecords)
)

write.table(Spine,'spine17ESJ.txt',sep="\t")



search<-searchjournals[5]
query<-EUtilsSummary(search,retmax=32000)
Spinerecords<-EUtilsGet(query)

authorlist<-Author(Spinerecords)

impAuthor<-function(authorlist){
  authors<-data.frame(matrix(ncol=52,nrow=length(authorlist)))
  colnames(authors)<-c('firstAuthorL','firstAuthorF','firstAuthorI','srAuthorL','srAuthorF','srAuthorI'
                       ,'midAuthorL1','midAuthorF1','midAuthorI1',
                       'midAuthorL2','midAuthorF2','midAuthorI2',
                       'midAuthorL3','midAuthorF3','midAuthorI3',
                       'midAuthorL4','midAuthorF4','midAuthorI4',
                       'midAuthorL5','midAuthorF5','midAuthorI5',
                       'midAuthorL6','midAuthorF6','midAuthorI6',
                       'midAuthorL7','midAuthorF7','midAuthorI7',
                       'midAuthorL8','midAuthorF8','midAuthorI8',
                       'midAuthorL9','midAuthorF9','midAuthorI9',
                       'midAuthorL10','midAuthorF10','midAuthorI10',
                       'midAuthorL11','midAuthorF11','midAuthorI11',
                       'midAuthorL12','midAuthorF12','midAuthorI12',
                       'midAuthorL13','midAuthorF13','midAuthorI13',
                       'midAuthorL14','midAuthorF14','midAuthorI14',
                       'midAuthorL15','midAuthorF15','midAuthorI15'
                       ,'numAuthor')
  for (i in 1:length(authorlist)){
    authors[i,]$firstAuthorL<-head(authorlist[[i]],n=1)$LastName
    authors[i,]$firstAuthorF<-head(authorlist[[i]],n=1)$ForeName
    authors[i,]$firstAuthorI<-head(authorlist[[i]],n=1)$Initials
    
    authors[i,]$srAuthorL<-tail(authorlist[[i]],n=1)$LastName
    authors[i,]$srAuthorF<-tail(authorlist[[i]],n=1)$ForeName
    authors[i,]$srAuthorI<-tail(authorlist[[i]],n=1)$Initials
    authors[i,]$numAuthor<-tail(authorlist[[i]],n=1)$order
    if (!is.na(tail(authorlist[[i]],n=1))&tail(authorlist[[i]],n=1)$order>2){
      for (j in 2:min(c(dim(authorlist[[i]])[1]-1),15)){
        
        authors[i,][[paste("midAuthorL",as.character(j-1),sep="")]]<-authorlist[[i]][j,]$LastName
        authors[i,][[paste("midAuthorF",as.character(j-1),sep="")]]<-authorlist[[i]][j,]$ForeName
        authors[i,][[paste("midAuthorI",as.character(j-1),sep="")]]<-authorlist[[i]][j,]$Initials
      }
    }
    
  }
  authors
}
authors<-impAuthor(authorlist)

typelist<-PublicationType(Spinerecords)

type<-function(typelist){
  types<-data.frame(matrix(ncol=3,nrow=length(typelist)))
  colnames(types)<-c('type1','type2','type3')
  for (i in 1:length(typelist)){
    types[i,]$type1<-array(unlist(typelist[[i]][1]))
    types[i,]$type2<-array(unlist(typelist[[i]][2]))
    types[i,]$type3<-array(unlist(typelist[[i]][3]))
    
  }
  types
}

pubtype<-type(typelist)

Spine<-data.frame('PMID'=PMID(Spinerecords)
                  ,'Journal'=Title(Spinerecords)
                  ,'Title'=ArticleTitle(Spinerecords)
                  ,'Country'=Country(Spinerecords)
                  ,'firstAuthorL'=authors$firstAuthorL
                  ,'firstAuthorF'=authors$firstAuthorF
                  ,'firstAuthorI'=authors$firstAuthorI
                  ,'srAuthorL'=authors$srAuthorL
                  ,'srAuthorF'=authors$srAuthorF
                  ,'srAuthorI'=authors$srAuthorI
                  ,'numAuthor'=authors$numAuthor
                  ,'midAuthorL1'=authors$midAuthorL1,'midAuthorF1'=authors$midAuthorF1,'midAuthorI1'=authors$midAuthorI1,
                  'midAuthorL2'=authors$midAuthorL2,'midAuthorF2'=authors$midAuthorF2,'midAuthorI2'=authors$midAuthorI2,
                  'midAuthorL3'=authors$midAuthorL3,'midAuthorF3'=authors$midAuthorF3,'midAuthorI3'=authors$midAuthorI3,
                  'midAuthorL4'=authors$midAuthorL4,'midAuthorF4'=authors$midAuthorF4,'midAuthorI4'=authors$midAuthorI4,
                  'midAuthorL5'=authors$midAuthorL5,'midAuthorF5'=authors$midAuthorF5,'midAuthorI5'=authors$midAuthorI5,
                  'midAuthorL6'=authors$midAuthorL6,'midAuthorF6'=authors$midAuthorF6,'midAuthorI6'=authors$midAuthorI6,
                  'midAuthorL7'=authors$midAuthorL7,'midAuthorF7'=authors$midAuthorF7,'midAuthorI7'=authors$midAuthorI7,
                  'midAuthorL8'=authors$midAuthorL8,'midAuthorF8'=authors$midAuthorF8,'midAuthorI8'=authors$midAuthorI8,
                  'midAuthorL9'=authors$midAuthorL9,'midAuthorF9'=authors$midAuthorF9,'midAuthorI9'=authors$midAuthorI9,
                  'midAuthorL10'=authors$midAuthorL10,'midAuthorF10'=authors$midAuthorF10,'midAuthorI10'=authors$midAuthorI10,
                  'midAuthorL11'=authors$midAuthorL11,'midAuthorF11'=authors$midAuthorF11,'midAuthorI11'=authors$midAuthorI11,
                  'midAuthorL12'=authors$midAuthorL12,'midAuthorF12'=authors$midAuthorF12,'midAuthorI12'=authors$midAuthorI12,
                  'midAuthorL13'=authors$midAuthorL13,'midAuthorF13'=authors$midAuthorF13,'midAuthorI13'=authors$midAuthorI13,
                  'midAuthorL14'=authors$midAuthorL14,'midAuthorF14'=authors$midAuthorF14,'midAuthorI14'=authors$midAuthorI14,
                  'midAuthorL15'=authors$midAuthorL15,'midAuthorF15'=authors$midAuthorF15,'midAuthorI15'=authors$midAuthorI15
                  
                  ,'Type1'=pubtype$type1
                  ,'Type2'=pubtype$type2
                  ,'Type3'=pubtype$type3
                  ,'Year'=YearPubmed(Spinerecords)
                  ,'Month'=MonthPubmed(Spinerecords)
)

write.table(Spine,'spine17JSD.txt',sep="\t")

####  
search<-searchjournals[6]
query<-EUtilsSummary(search,retmax=32000)
Spinerecords<-EUtilsGet(query)

authorlist<-Author(Spinerecords)

impAuthor<-function(authorlist){
  authors<-data.frame(matrix(ncol=52,nrow=length(authorlist)))
  colnames(authors)<-c('firstAuthorL','firstAuthorF','firstAuthorI','srAuthorL','srAuthorF','srAuthorI'
                       ,'midAuthorL1','midAuthorF1','midAuthorI1',
                       'midAuthorL2','midAuthorF2','midAuthorI2',
                       'midAuthorL3','midAuthorF3','midAuthorI3',
                       'midAuthorL4','midAuthorF4','midAuthorI4',
                       'midAuthorL5','midAuthorF5','midAuthorI5',
                       'midAuthorL6','midAuthorF6','midAuthorI6',
                       'midAuthorL7','midAuthorF7','midAuthorI7',
                       'midAuthorL8','midAuthorF8','midAuthorI8',
                       'midAuthorL9','midAuthorF9','midAuthorI9',
                       'midAuthorL10','midAuthorF10','midAuthorI10',
                       'midAuthorL11','midAuthorF11','midAuthorI11',
                       'midAuthorL12','midAuthorF12','midAuthorI12',
                       'midAuthorL13','midAuthorF13','midAuthorI13',
                       'midAuthorL14','midAuthorF14','midAuthorI14',
                       'midAuthorL15','midAuthorF15','midAuthorI15'
                       ,'numAuthor')
  for (i in 1:length(authorlist)){
    authors[i,]$firstAuthorL<-head(authorlist[[i]],n=1)$LastName
    authors[i,]$firstAuthorF<-head(authorlist[[i]],n=1)$ForeName
    authors[i,]$firstAuthorI<-head(authorlist[[i]],n=1)$Initials
    
    authors[i,]$srAuthorL<-tail(authorlist[[i]],n=1)$LastName
    authors[i,]$srAuthorF<-tail(authorlist[[i]],n=1)$ForeName
    authors[i,]$srAuthorI<-tail(authorlist[[i]],n=1)$Initials
    authors[i,]$numAuthor<-tail(authorlist[[i]],n=1)$order
    if (!is.na(tail(authorlist[[i]],n=1))&tail(authorlist[[i]],n=1)$order>2){
      for (j in 2:min(c(dim(authorlist[[i]])[1]-1),15)){
        
        authors[i,][[paste("midAuthorL",as.character(j-1),sep="")]]<-authorlist[[i]][j,]$LastName
        authors[i,][[paste("midAuthorF",as.character(j-1),sep="")]]<-authorlist[[i]][j,]$ForeName
        authors[i,][[paste("midAuthorI",as.character(j-1),sep="")]]<-authorlist[[i]][j,]$Initials
      }
    }
    
  }
  authors
}
authors<-impAuthor(authorlist)

typelist<-PublicationType(Spinerecords)

type<-function(typelist){
  types<-data.frame(matrix(ncol=3,nrow=length(typelist)))
  colnames(types)<-c('type1','type2','type3')
  for (i in 1:length(typelist)){
    types[i,]$type1<-array(unlist(typelist[[i]][1]))
    types[i,]$type2<-array(unlist(typelist[[i]][2]))
    types[i,]$type3<-array(unlist(typelist[[i]][3]))
    
  }
  types
}

pubtype<-type(typelist)

Spine<-data.frame('PMID'=PMID(Spinerecords)
                  ,'Journal'=Title(Spinerecords)
                  ,'Title'=ArticleTitle(Spinerecords)
                  ,'Country'=Country(Spinerecords)
                  ,'firstAuthorL'=authors$firstAuthorL
                  ,'firstAuthorF'=authors$firstAuthorF
                  ,'firstAuthorI'=authors$firstAuthorI
                  ,'srAuthorL'=authors$srAuthorL
                  ,'srAuthorF'=authors$srAuthorF
                  ,'srAuthorI'=authors$srAuthorI
                  ,'numAuthor'=authors$numAuthor
                  ,'midAuthorL1'=authors$midAuthorL1,'midAuthorF1'=authors$midAuthorF1,'midAuthorI1'=authors$midAuthorI1,
                  'midAuthorL2'=authors$midAuthorL2,'midAuthorF2'=authors$midAuthorF2,'midAuthorI2'=authors$midAuthorI2,
                  'midAuthorL3'=authors$midAuthorL3,'midAuthorF3'=authors$midAuthorF3,'midAuthorI3'=authors$midAuthorI3,
                  'midAuthorL4'=authors$midAuthorL4,'midAuthorF4'=authors$midAuthorF4,'midAuthorI4'=authors$midAuthorI4,
                  'midAuthorL5'=authors$midAuthorL5,'midAuthorF5'=authors$midAuthorF5,'midAuthorI5'=authors$midAuthorI5,
                  'midAuthorL6'=authors$midAuthorL6,'midAuthorF6'=authors$midAuthorF6,'midAuthorI6'=authors$midAuthorI6,
                  'midAuthorL7'=authors$midAuthorL7,'midAuthorF7'=authors$midAuthorF7,'midAuthorI7'=authors$midAuthorI7,
                  'midAuthorL8'=authors$midAuthorL8,'midAuthorF8'=authors$midAuthorF8,'midAuthorI8'=authors$midAuthorI8,
                  'midAuthorL9'=authors$midAuthorL9,'midAuthorF9'=authors$midAuthorF9,'midAuthorI9'=authors$midAuthorI9,
                  'midAuthorL10'=authors$midAuthorL10,'midAuthorF10'=authors$midAuthorF10,'midAuthorI10'=authors$midAuthorI10,
                  'midAuthorL11'=authors$midAuthorL11,'midAuthorF11'=authors$midAuthorF11,'midAuthorI11'=authors$midAuthorI11,
                  'midAuthorL12'=authors$midAuthorL12,'midAuthorF12'=authors$midAuthorF12,'midAuthorI12'=authors$midAuthorI12,
                  'midAuthorL13'=authors$midAuthorL13,'midAuthorF13'=authors$midAuthorF13,'midAuthorI13'=authors$midAuthorI13,
                  'midAuthorL14'=authors$midAuthorL14,'midAuthorF14'=authors$midAuthorF14,'midAuthorI14'=authors$midAuthorI14,
                  'midAuthorL15'=authors$midAuthorL15,'midAuthorF15'=authors$midAuthorF15,'midAuthorI15'=authors$midAuthorI15
                  
                  ,'Type1'=pubtype$type1
                  ,'Type2'=pubtype$type2
                  ,'Type3'=pubtype$type3
                  ,'Year'=YearPubmed(Spinerecords)
                  ,'Month'=MonthPubmed(Spinerecords)
)

write.table(Spine,'spine17CSS.txt',sep="\t")
