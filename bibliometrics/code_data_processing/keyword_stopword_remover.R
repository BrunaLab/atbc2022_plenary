keyword_stopword_remover <- function(keywords) {
  library(tidyverse)
  library(stringr)
  library(stopwords)
  
# remove stopwords --------------------------------------------------------
  


  keywords<-keywords %>% mutate(final=singular) %>% 
  filter(!(final %in% stopwords(source = "snowball")))
  
keywords$final<-gsub(" or ", " ",keywords$final)   
keywords$final<-gsub(" and ", " ",keywords$final)   
keywords$final<-gsub(" the ", " ",keywords$final)   
keywords$final<-gsub(" with ", " ",keywords$final) 
keywords$final<-gsub(" vs ", " ",keywords$final) 
keywords$final<-gsub(" of ", " ",keywords$final)   




keywords<-keywords %>% mutate_all(trimws)    



return(keywords)
}