library(pdftools)
library(stringr)
library(tidyverse)
a<-pdftools::pdf_data("B377 vol. 2.pdf")
aa<-a[22:424]

#bg<-aa[[400]]

get_species<-function(aa){
   aa$laggy<-aa$y-lag(aa$y)
   aa$laggy_lead<-lag(aa$laggy)
   bb<-filter(aa,(laggy>=28&laggy<=29)|(laggy_lead>=28&laggy_lead<=29)) #take first two words after a break of that size
   bb %>% group_by(y) %>% summarize(species=paste(text,collapse=" "))->cc
   return(bb)
}


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
      summarize(species=paste(species,collapse=" "),y=y[1]) %>%
      distinct(species,id,y,match)->out
}

fix_problems<-function(df){
   df$species<-str_trim(df$species)
   df2<-filter(df,str_to_sentence(species)==species)
   df4<-filter(df2,sapply(strsplit(species, " "), length)==2)
   df5<-distinct(df4,species,id,y)
   df5$id<-as.numeric(df5$id)
   df5<-arrange(df5,id,y)   
   return(df5)
}

get_dist<-function(species_df,text_df){
   text_df<-arrange(text_df,id,y,x)
   species_df<-arrange(species_df,id,y)   
   species_df$dist<-as.character("")
   for(i in 1:nrow(species_df)){
      index<-which(text_df$id==species_df$id[i] & text_df$y==species_df$y[i])[1]
      print(i)
      print(index)
     for (j in 1:10000){
        if(!is.na(text_df$text[index+j]) & text_df$text[index+j]=="Distribution:"){
           start<-index+j
           for(k in 1:50){
              if(grepl("\\.",text_df$text[start+k])){ 
              end<-index+j+k
              break
              }
           }
           break
        }
     }
      species_df$dist[i]<-paste(text_df$text[start:end],collapse=" ")
      start<-NA
      end<-NA
      index<-NA
   }
   return(species_df)
}

dat<-lapply(aa,get_species)
dat2<-bind_rows(setNames(dat, seq_along(dat)), .id = "id") #keeps page numbers in the data frame
dat2 %>% group_by(id,y) %>% summarize(species=paste(text,collapse=" "),x=x) -> spp #off by one mistakes
df <- fix_off_by_one_error(spp)
df3<-fix_problems(df)
text_df<-bind_rows(setNames(aa, seq_along(aa)), .id = "id") 
text_df$id<-as.numeric(text_df$id)
full_df<-get_dist(df3,text_df)

full_df %>%
   arrange(id) %>%
   rename(page=id) %>%
   filter(species!="Species inquirendae")

write_csv(full_df,"book2.csv")

