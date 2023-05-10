

names(complete_data)

complete_data$AF
data<-complete_data %>% filter(PY>1990)

# Take the KW column and split it up, arrange as a column
authors<-data %>% 
  select(refID,AU) %>% 
  drop_na(AU) %>%
  mutate(author = strsplit(AU, "\n")) %>% 
  unnest(author) %>%
  group_by(refID) %>%
  tally() %>% 
  arrange(desc(n))
  
range(authors$n)
mean(authors$n)
sd(authors$n)
hist(authors$n)
authors_slim<-authors %>% slice_tail(n=42100)
ggplot(authors_slim, aes(x=n))+
  geom_histogram(color="darkblue", fill="lightblue", binwidth=1)+
  theme_classic()




  mutate(row = row_number()) %>%
  spread(row, to) %>% 
  ungroup() %>% 
  select(-kw_original) %>% 
  #   separate(kw_original,c(LETTERS[seq( from = 1, to = 20 )]), sep = ";") %>% 
  pivot_longer(!refID, names_to = "letter", values_to = "kw_original") %>% 
  select(-letter) %>% 
  drop_na(kw_original) %>% 
  mutate(kw_original=trimws(kw_original)) %>% 
  mutate(kw_original=gsub("\n"," ",kw_original)) %>% 
  # mutate(kw_original=str_replace('\\n"', '')) %>% 
  mutate(kw_original=tolower(kw_original))






analysis_refs<-complete_data %>%  
  filter(SO!="rbt") %>% 
  filter(SO!="trop_ecol") %>% 
  filter(PY>1968) %>% 
  filter(PY<2021) 
unique(merged_refs$SO)



# load libraries ----------------------------------------------------------
library(tidyverse)
library(stopwords)
library(ngram)
library(tidytext)
library(igraph)
library(tidystringdist)
library(SemNetCleaner)
library(SemNeT)

# Load reference records, clean keywords and text of DE and titles -----------------

complete_data<-read_csv("./bibliometrics/data_clean/complete_data.csv") %>% 
  mutate(TI=gsub(" - "," ",TI)) %>%
  replace_na(list(pub_cat = "temperate")) %>% 
  mutate(DE = gsub("\n"," ",DE)) %>% 
  mutate(DE = gsub("\"","",DE)) %>% 
  mutate(DE = gsub("<bold>","",DE)) %>% 
  mutate(DE = gsub("</bold>","",DE)) %>% 
  mutate(DE = gsub("- ","-",DE)) %>% 
  mutate(DE = gsub("r  theory *","r* theory",DE)) %>% 
  mutate(DE = gsub("&#8208","-",DE)) %>%
  mutate(DE = gsub("&#8211","-",DE)) %>%
  mutate(DE = gsub("&#8217","'",DE)) %>%
  mutate(DE = gsub("&amp","&",DE)) 
unique(complete_data$SO) 
