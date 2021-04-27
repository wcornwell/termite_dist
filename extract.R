library(pdftools)
library(stringr)
library(tidyverse)
a<-pdftools::pdf_data("B377 vol. 2.pdf")

all<-bind_rows(setNames(a, seq_along(a)), .id = "id")


get_species<-function(aa){
   aa$laggy<-aa$y-lag(aa$y)
   aa$laggy_lead<-lag(aa$laggy)
   bb<-filter(aa,(laggy>=28&laggy<=29)|(laggy_lead>=28&laggy_lead<=29)) #take first two words after 
   bb %>% group_by(y) %>% summarize(species=paste(text,collapse=" "))->cc
   return(bb)
}

dat<-lapply(a,get_species)
dat2<-bind_rows(setNames(dat, seq_along(dat)), .id = "id")



get_dist<-function(aa){
  dists<-filter(aa,text=="Distribution:")
  filter(aa,y%in%dists$y) %>%
  group_by(y) %>% summarize(species=paste(text,collapse=" "))->cc
  return(cc)
}

xx<-bind_rows(lapply(a,get_dist))



