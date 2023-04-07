# putting together and cleaning up the various WOS archives - all together

# load libraries ----------------------------------------------------------

library(refsplitr)
library(tidyverse)
library(countrycode)
library(opencage)
library(usethis)


complete_data<-read_csv("./bibliometrics/data_clean/complete_data.csv") %>% 
  mutate(pub_cat=as.factor(pub_cat)) %>% 
  mutate(jrnl_cat=as.factor(jrnl_cat)) %>% 
  mutate(index = row_number()) 




all_refs_count<-complete_data %>%
  group_by(DI,TI) %>% 
  count() %>% 
  filter(n>1) %>% 
  arrange(desc(n))

all_refs<-complete_data %>% 
  distinct(DI,TI,.keep_all = TRUE) 

all_refs_count<-complete_data %>%
  group_by(TI) %>% 
  count() %>% 
  filter(n>1) %>% 
  arrange(desc(n))

complete_data<-complete_data %>% 
  distinct(TI,.keep_all = TRUE) 


summary(complete_data$jrnl_cat)
summary(complete_data$pub_cat) 

complete_data %>% count(pub_cat,jrnl_cat)
 
nocat<-complete_data %>% filter(is.na(pub_cat))
nocat %>% count(pub_cat)
tropical_terms = c("tropical", "tropics", "amazon","congo","amazonia","bci","la selva")
nocat<-nocat %>% 
mutate(pub_cat = case_when(
  (is.na(pub_cat) & str_detect(TI, paste(tropical_terms, collapse = '|'))) ~ "tropical",
  (is.na(pub_cat) & str_detect(AB, paste(tropical_terms, collapse = '|'))) ~ "tropical",
  (is.na(pub_cat) & str_detect(DE, paste(tropical_terms, collapse = '|'))) ~ "tropical",
  TRUE ~ as.character(pub_cat)
  )
  ) 
nocat %>% count(pub_cat)



diversity_nsf_fail <- diversity_nsf %>%
  filter(str_detect(title, paste(diversity_terms, collapse = '|'))) %>%
  filter(!str_detect(title, paste(dei_terms_diversity, collapse = '|')))
 
 