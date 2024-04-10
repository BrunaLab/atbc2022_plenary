
#TODO: delete this title # 0‐226‐53236‐4/0‐226‐53237‐2

# load libraries ----------------------------------------------------------

library(tidyverse)
library(stopwords)
# library(ngram)
# library(tidytext)
# library(igraph)
# library(tidystringdist)

#TODO: How to deal with COsta Rica, La Selva, etc. being split in TW analysis? treat only bigrams?

# Load keyword records  -----------------
complete_data<-read_csv("./bibliometrics/data_clean/complete_data_clean.csv") %>%
  mutate(TI = gsub(" - ", "-", TI)) %>% 
    mutate(refID=paste(refID,PY,sep="-")) %>% 
    relocate(refID,.before=1) %>% 
  # filter(SO!="rbt") %>% 
  # filter(SO!="trop_ecol") %>%
  mutate(pub_cat=as.factor(pub_cat)) %>% 
  mutate(jrnl_cat=as.factor(jrnl_cat)) %>% 
  mutate(index = row_number())

# complete_data %>% group_by(refID,TI) %>% 
#   tally() %>% filter(n>1)



# analysis - title words --------------------------------------------------

analysis_data<-complete_data


# clean up the titles ----------------------------------------------------


# analysis_data<-analysis_data %>% select(refID,jrnl_cat,SO,PY,TI) %>% 

analysis_data<-analysis_data %>% select(refID,PY,TI) %>% 
  # drop_na(TI) %>% 
  mutate(TI=gsub(" - "," ",TI)) %>%
  mutate(TI=gsub("- "," ",TI)) %>%
  mutate(TI=gsub(" -"," ",TI)) %>%
  mutate(TI=gsub("-"," ",TI)) %>% 
  mutate(TI=gsub("trade offs","tradeoffs",TI)) %>% 
  mutate(TI=gsub("trade off","tradeoff",TI)) %>% 
  # mutate(TI=gsub(".1."," ",TI)) %>%
  # mutate(TI=gsub(".2."," ",TI)) %>%
  # mutate(TI=gsub(".3."," ",TI)) %>%
  # mutate(TI=gsub(".4."," ",TI)) %>%
  mutate(TI=gsub("  "," ",TI)) %>%
mutate(TI=gsub(":","",TI)) %>% 
  mutate(TI=gsub(",","",TI)) %>% 
  mutate(TI=gsub(";","",TI))

# %>% 
#   mutate(TI=gsub("species diversity","species-diversity",TI)) %>% 
#   mutate(TI=gsub("tropical forest","tropical-forest",TI)) %>% 
#   mutate(TI=gsub("dry forest","dry-forest",TI)) %>% 
#   mutate(TI=gsub("rain forest","rain-forest",TI)) %>% 
#   mutate(TI=gsub("seed forest","seed-forest",TI))



# convert to ascii --------------------------------------------------------
library(stringi)
analysis_data$TI<-analysis_data$TI<-stri_trans_general(str = analysis_data$TI,id = "Latin-ASCII")



# 2word Titlewords. -------------------------------------------------------

# Some of the Words are being split up and counted as different keywords, so that
# they lose their meaning, eg "La Selva" -> 'La' and 'Selva' 
# Some of these are countries, so consider converting to ISO code and then converting back after split


# library(countrycode)
# analysis_data<-analysis_data %>%
#   mutate(TI2=countrycode(TI, origin = 'country.name', destination = 'iso3c'))


analysis_data <- analysis_data %>% 
  mutate(TI = gsub("los tuxtlas", "lostuxtlas", TI)) %>% 
  mutate(TI = gsub("burkina faso", "burkinafaso", TI)) %>% 
  mutate(TI = gsub("new zealand", "newzealand", TI)) %>% 
  mutate(TI = gsub("new york", "newyork", TI)) %>% 
  mutate(TI = gsub("isla verde", "islaverde", TI)) %>% 
  mutate(TI = gsub("cape verde", "capeverde", TI)) %>% 
  mutate(TI = gsub("united states", "usa", TI)) %>% 
  mutate(TI = gsub("ivory coast", "ivorycoast", TI)) %>% 
  mutate(TI = gsub("el salvador", "elsalvador", TI)) %>% 
  mutate(TI = gsub("ivory coast", "ivorycoast", TI)) %>% 
  mutate(TI = gsub("us virgin islands", "usvi", TI)) %>% 
  mutate(TI = gsub("south africa", "southafrica", TI)) %>% 
  mutate(TI = gsub("republic of congo", "congo", TI)) %>% 
  mutate(TI = gsub("sierra nevada", "sierranevada", TI)) %>% 
  mutate(TI = gsub("costa rica", "costarica", TI)) %>% 
  mutate(TI = gsub("new jersey", "newjersey", TI)) %>% 
  mutate(TI = gsub("united kingdom", "uk", TI)) %>% 
  mutate(TI = gsub("mariana islands", "marianaislands", TI)) %>% 
  mutate(TI = gsub("cocos island", "cocosisland", TI)) %>% 
  mutate(TI = gsub("brunei darussalam", "bruneidarussalam", TI)) %>% 
  mutate(TI = gsub("mexico city", "mexicocity", TI)) %>% 
  mutate(TI = gsub("north carolina", "northcarolina", TI)) %>% 
  mutate(TI = gsub("south carolina", "southcarolina", TI)) %>% 
  mutate(TI = gsub("american samoa", "americansamoa", TI)) %>% 
  mutate(TI = gsub("la selva", "laselva", TI)) %>% 
  mutate(TI = gsub("jena experiment", "jenaexperiment", TI)) %>% 
  mutate(TI = gsub("cedar creek", "cedar creek", TI)) %>% 
  mutate(TI = gsub("las cruces", "lascruces", TI)) %>% 
  mutate(TI = gsub("palo verde", "paloverde", TI)) %>% 
  mutate(TI = gsub("cocha cashu", "cochacashu", TI)) %>% 
  mutate(TI = gsub("janzen-connell", "janzenconnell", TI)) %>% 
  mutate(TI = gsub("lotka-volterra", "lotkavolterra", TI)) %>% 
  mutate(TI = gsub("ornstein-uhlenbeck", "ornsteinuhlenbeck", TI)) %>% 
  mutate(TI = gsub("rain forest", "rainforest", TI)) %>% 
  mutate(TI = gsub("cormack-jolly-seber", "cormackjollyseber", TI)) 






# parse titles and clean --------------------------------------------------
tw<-analysis_data %>% 
  select(refID,PY,TI) %>% 
  # slice(1:50) %>% 
  drop_na(TI) %>% 
  rename(DE=TI) %>% 
  mutate(to = strsplit(DE, " |\n")) %>%
  unnest(to) %>%
  select(-DE) %>% 
  group_by(refID) %>%
  mutate(row = row_number()) %>%
  spread(row, to) %>%
  ungroup() %>%
  #   separate(kw_original,c(LETTERS[seq( from = 1, to = 20 )]), sep = ";") %>%
  pivot_longer(!refID:PY, names_to = "letter", values_to = "original") %>%
  select(-letter) %>%
  drop_na(original) %>%
  mutate(original = trimws(original)) %>%
  # mutate(original = gsub("\n", " ", kw_original)) %>% # none of these, can delete?
  mutate(original = tolower(original)) %>% 
  mutate(original = trimws(original)) %>% 
  drop_na(original) %>% 
  filter(original!="") %>% 
  mutate(original = gsub('"',"",original)) %>% 
  mutate(original = gsub("'","",original)) %>% 
  mutate(original = gsub('[(]',"",original)) %>% 
  mutate(original = gsub('[).]',"",original)) %>% 
  mutate(original = gsub('[)]',"",original)) 
  



tw_summary<-tw %>% 
  group_by(original) %>% 
  tally() %>% 
  arrange(desc(n)) 
tw_summary



# /
# +/
# °
# –
# remove stopwords, punctuation, etc. ----------------------------------------



tw_summary<-tw_summary %>% 
  mutate(original=str_remove_all(original, "<sup>")) %>% 
  mutate(original=str_remove_all(original, "</sup>")) %>% 
  mutate(original=str_remove_all(original, "<inf>")) %>% 
  mutate(original=str_remove_all(original, "</inf>")) %>% 
  mutate(original=str_remove_all(original, "[?]")) %>% 
  mutate(original=str_remove_all(original, "[*]")) %>% 
  mutate(original=str_remove_all(original, "[’]")) %>% 
  mutate(original=str_remove_all(original, "\\[")) %>% 
  mutate(original=str_remove_all(original, "\\]")) %>% 
  mutate(original=str_remove_all(original, "—")) %>% 
  mutate(original=str_remove_all(original, "″")) %>% 
  mutate(original=str_remove_all(original, "[×]")) %>% 
  mutate(original=str_remove_all(original, "[‘]")) %>% 
  mutate(original=str_remove_all(original, "[¿]")) %>% 
  mutate(original=str_remove_all(original, "[“]")) %>% 
  mutate(original=str_remove_all(original, "[”]")) %>% 
  mutate(original=str_remove_all(original, "[〉]")) %>% 
  mutate(original=str_remove_all(original, "&amp")) %>% 
  mutate(original=str_remove_all(original, "&gt")) %>% 
  mutate(original=str_remove_all(original, "&lt")) %>% 
  mutate(original=str_remove_all(original, "=")) %>% 
  mutate(original=str_remove_all(original, "\\+")) %>% 
  filter(!str_detect(original, '\\$')) %>% # removes all with $
  filter(!str_detect(original, '\\£')) %>%  # removes all with £
  filter(!str_detect(original, '\\£')) %>%  # removes all with £
  filter(!str_detect(original, '\\,')) %>%  # removes all with £
  mutate(original=trimws(original)) 
tw_summary



tw_summary<-tw_summary %>% 
  filter(!(original %in% stopwords(source = "snowball"))) %>%  # deletes the stopwords
  # mutate(original=gsub('[[:punct:] ]+',' ',original)) %>% 
  mutate(original=trimws(original)) 
tw_summary



source("./bibliometrics/code_data_processing/keyword_editor.R")
tw_summary<-keyword_editor(tw_summary) 
tw_summary




tw_summary<-tw_summary %>% 
  mutate(n=as.integer(n)) %>% 
  group_by(original,edited) %>% 
  summarize(n=sum(n)) %>% 
  relocate(n,.after="edited") %>% 
  arrange(desc(n)) 
tw_summary


tw_summary<-tw_summary %>% 
  filter(edited!="") 
write_csv(tw_summary, "./bibliometrics/data_intermediate/titlewords_edited.csv")

summary(tw_summary$original==tw_summary$edited)


source("./bibliometrics/code_data_processing/keyword_depluralizer.R")
tw_summary<-keyword_depluralizer(tw_summary)
write_csv(tw_summary, "./bibliometrics/data_intermediate/titlewords_no_plurals.csv")

summary(tw_summary$edited==tw_summary$singular)



tw_summary<-tw_summary %>% 
mutate(n=as.integer(n)) %>% 
  group_by(original,edited,singular) %>% 
  summarize(n=sum(n)) %>% 
  relocate(n,.after="singular") %>% 
  arrange(desc(n)) 
tw_summary


tw_summary %>% 
  select(-n) %>% 
  ungroup() %>% 
  mutate_all(as.factor) %>% 
  summarise_at(vars(original, edited,singular), n_distinct,na.rm = TRUE)


source("./bibliometrics/code_data_processing/keyword_stopword_remover.R")
tw_summary<-keyword_stopword_remover(tw_summary)

write_csv(tw_summary, "./bibliometrics/data_intermediate/titlewords_no_stopwords.csv")



tw_summary %>% 
  select(-n) %>% 
  ungroup() %>% 
  mutate_all(as.factor) %>% 
  summarise_at(vars(original, edited,singular), n_distinct,na.rm = TRUE)




source("./bibliometrics/code_data_processing/keyword_s_word_finder.R")
s_words<-keyword_s_word_finder(tw_summary)
s_words
write_csv(s_words, file = "./bibliometrics/data_intermediate/title_s_words.csv")



tw_summary<-tw_summary %>% 
  # group_by(kw_final) %>% 
  # tally() %>%
  drop_na(final) %>% 
  mutate(n_char=nchar(final)) %>% 
  arrange(n_char) %>% 
  filter(n_char>2) %>% 
  distinct(original,edited,singular,final)

names(tw)
names(tw_summary)




tw_clean<-left_join(tw,tw_summary,by="original") 



pub_data<-complete_data %>% select(refID,PY,SO,pub_cat_2,jrnl_cat)


pub_data %>% arrange(refID)
tw_clean<-left_join(tw_clean,pub_data,by=c("refID","PY"))

tw_clean %>% drop_na(final) %>% group_by(refID,final) %>% tally() %>% filter(n>1) 


tw_summary_final<-tw_clean %>% 
  group_by(final) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  drop_na(final)



write_csv(tw_summary_final,"./bibliometrics/data_clean/tw_summary_clean.csv")
write_csv(tw_clean,"./bibliometrics/data_clean/tw_clean.csv")


