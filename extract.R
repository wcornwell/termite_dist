library(pdftools)
library(stringr)
library(tidyverse)
a<-pdftools::pdf_data("B377 vol. 2.pdf")
aa<-a[22:424]


get_species<-function(aa){
   aa$laggy<-aa$y-lag(aa$y)
   aa$laggy_lead<-lag(aa$laggy)
   bb<-filter(aa,(laggy>=28&laggy<=29)|(laggy_lead>=28&laggy_lead<=29)) #take first two words after a break of that size
   bb %>% group_by(y) %>% summarize(species=paste(text,collapse=" "))->cc
   return(bb)
}

dat<-lapply(aa,get_species)
dat2<-bind_rows(setNames(dat, seq_along(dat)), .id = "id") #keeps page numbers in the data frame

dat2 %>% group_by(id,y) %>% summarize(species=paste(text,collapse=" "),x=x) -> spp #off by one mistakes

#df<-filter(spp,id==385)

fix_off_by_one_error<-function(df){
   df<-arrange(df,id,y)
   k<-1
   df$match<-1:nrow(df)
   for(i in 2:nrow(df)){
      if(df$y[i]==df$y[i-1]+1){
         df$match[i]<-letters[k]
         df$match[i-1]<-letters[k]
         k<-k+1
      }
   }
   df<-fix(df)
   return(df)
}

fix<-function(df){
   df %>% arrange(x) %>%
   group_by(id,match) %>% 
      summarize(species=paste(species,collapse=" ")) ->out
}

df <- fix_off_by_one_error(spp)

   
get_dist<-function(aa){
  dists<-filter(aa,text=="Distribution:")
  filter(aa,y%in%dists$y) %>%
  group_by(y) %>% summarize(species=paste(text,collapse=" "))->cc
  return(cc)
}

xx<-bind_rows(lapply(a,get_dist))



