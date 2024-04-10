
# load libraries ----------------------------------------------------------

library(tidyverse)
# library(stringr)
# library(stopwords)
# library(ngram)
# library(tidytext)
# library(igraph)
# library(janitor)
# library(tidystringdist)
# # tutorial
# https://www.youtube.com/watch?v=FbznaCOXbcU


#TODO: 2x to make sure there are no dupes due to abbreviation

# Load reference records, clean keywords and text of DE and titles -----------------

complete_data <- read_csv("./bibliometrics/data_clean/complete_data_clean.csv") %>%
  mutate(TI = gsub(" - ", "-", TI)) %>% 
  mutate(refID=paste(refID,PY,sep="-")) %>% 
  relocate(refID,.before=1)

unique(complete_data$SO)
unique(complete_data$pub_cat)
unique(complete_data$jrnl_cat)


titles<-complete_data %>% select(refID,TI)

# split TW and arrange as column ----------------------------------------
tw_all<-titles %>% drop_na(TI) %>%
  rename(original = TI) %>%
  mutate(original = gsub("<bold>", "", original)) %>%
  mutate(original = gsub("</bold>", "", original)) %>%
  mutate(original = gsub("</bold>", "", original)) %>%
  mutate(original = gsub("<inf>", "", original)) %>%
  mutate(original = gsub("</inf>", "", original)) %>%
  mutate(original = gsub("<sup>", "", original)) %>%
  mutate(original = gsub("</sup>", "", original)) %>%
  mutate(original = gsub("&#8208", "-", original)) %>%
  mutate(original = gsub("&#8211", "-", original)) %>%
  mutate(original = gsub("&#8217", "'", original)) %>%
  mutate(original = gsub("&amp", "&", original)) %>%
  mutate(original = gsub("[^[:alnum:]]", " ",original)) %>% 
  # 
  # 
  # #   some of the key words are seperated by ",". This complicates things because some key words are also in the
  # # format "biomass, microbial" (instead of microbial biomass"). Also have "STRI, Panama" or "Montana, USA" But easier to split, i think.
  # mutate(original = gsub(",", " ", original)) %>%
  # mutate(original = gsub("\\*", " ", original)) %>%
  # mutate(original = gsub(":", " ", original)) %>%
  # mutate(original = gsub(";", " ", original)) %>% # some had 2 ; separating kw
  # mutate(original = gsub("-", " ", original)) %>%
  # mutate(original = gsub("–", " ", original)) %>%
  # mutate(original = gsub("—", " ", original)) %>%
  # mutate(original = gsub("=", " ", original)) %>%
  # mutate(original = gsub("×", " ", original)) %>%
  # mutate(original = gsub("/", " ", original)) %>%
  # mutate(original = gsub("\\[", " ", original)) %>%
  # mutate(original = gsub("\\]", " ", original)) %>%
  # mutate(original = gsub("?", " ", original)) %>%
  # mutate(original = gsub("+", " ", original)) %>%
  # mutate(original = gsub("\\)", " ", original)) %>%
  # mutate(original = gsub("\\(", " ", original)) %>%
  # mutate(original = gsub("\n", " ", original)) %>%
  # mutate(original = gsub("[<]", " ", original)) %>%
  # mutate(original = gsub("[>]", " ", original)) %>%
  # most efficient way to split withoutknowing number of columns
  # https://stackoverflow.com/questions/33288695/how-to-use-tidyrseparate-when-the-number-of-needed-variables-is-unknown
  mutate(to = strsplit(original, " ")) %>%
  unnest(to) %>%
  group_by(original) %>%
  mutate(row = row_number()) %>%
  spread(row, to) %>%
  ungroup() %>%
  select(-original) %>%
  #   separate(original,c(LETTERS[seq( from = 1, to = 20 )]), sep = ";") %>%
  pivot_longer(!refID, names_to = "letter", values_to = "original") %>%
  select(-letter) %>%
  drop_na(original) %>%
  mutate(original = trimws(original)) %>%
  mutate(original = gsub("\n", " ", original)) %>% # none of these, can delete?
  # mutate(original=str_replace('\\n"', '')) %>%
  mutate(original = tolower(original)) %>% 
  mutate(original = trimws(original)) %>% 
  filter(nchar(original)>0) 
  

tw_all
write_csv(tw_all, "./bibliometrics/data_intermediate/tw_all.csv")
# write_csv(tw_all, "./bibliometrics/data_intermediate/keywords_tropical.csv")

tw_all %>% summarize(n_distinct(tw_all))


dupes<-get_dupes(tw_all)
tw_all<-distinct_all(tw_all)

# ID and remove the kw that didnt split up--------------------------------------
unsplit_tw <-tw_all %>% 
  select(refID,original) %>% 
  mutate(nchar=nchar(original)) %>% 
  arrange(desc(nchar)) %>%
  filter((nchar<3))
# |
#            str_detect(original, 'special feature')|
#            str_detect(original, 'funding was provided')|
#            str_detect(original, 'international journal of tropical')|
#            str_detect(original, 'funding was provided')|
#            str_detect(original, 'este trabajo')
#   )


# remove them 
# 
# tw_all<-anti_join(tw_all,unsplit_kw,by="refID") 
# tw_all<-tw_all %>% mutate_all(trimws)
# rm(complete_data)

# editing keywords --------------------------------------------------------



tw_summary <- tw_all %>%
  group_by(original) %>%
  tally() %>%
  arrange(desc(n))
tw_summary




source("./bibliometrics/code_data_processing/keyword_editor.R")
tw_summary<-keyword_editor(tw_summary)
write_csv(tw_summary, "./bibliometrics/data_intermediate/tw_edited.csv")


# tw_summary <- tw_summary %>%
#   filter(!str_detect(edited, "funding was provided by grants from the academy")) %>%
#   filter(!str_detect(edited, "este trabajo es parte del trabajo doctoral de la autora")) %>%
#   filter(!str_detect(edited, "atom percent excess")) %>%
#   filter(!str_detect(edited, "fruit census fruit trap collection")) # need to fix this one
# 
tw_summary <- tw_summary %>% filter(nchar(edited) > 0)
tw_summary<-tw_summary %>% mutate_all(trimws)

tw_summary %>%
  group_by(edited) %>%
  tally() %>%
  arrange(desc(n))


# remove plurals ----------------------------------------------------------

source("./bibliometrics/code_data_processing/keyword_depluralizer.R")
tw_summary<-keyword_depluralizer(tw_summary)
write_csv(tw_summary, "./bibliometrics/data_intermediate/tw_no_plurals.csv")

tw_summary %>% summarise_at(vars(edited,singular), n_distinct, na.rm = TRUE)
# 
# tw_summary %>%
#   group_by(singular) %>%
#   tally() %>%
#   arrange(desc(n))


# summary(keywords$original==keywords$final)

# temporary save/reload point ---------------------------------------------
# 
# write_csv(keywords, file = "./bibliometrics/data_intermediate/kw_int.csv")
# # keywords<-read_csv("./bibliometrics/data_intermediate/kw_int.csv")



source("./bibliometrics/code_data_processing/keyword_stopword_remover.R")
tw_summary<-keyword_stopword_remover(tw_summary)
write_csv(tw_summary, "./bibliometrics/data_intermediate/tw_no_stopwords.csv")



tw_summary %>% summarise_at(vars(edited,singular,final), n_distinct, na.rm = FALSE)

short_tw<-tw_summary %>% 
  mutate(nchar_final=nchar(final)) %>% 
  arrange(desc(nchar_final)) %>%
  filter((nchar_final<5))
short_tw

non_num<-tw_summary %>% 
  mutate(non_num=str_detect(final, "[^[:alnum:]]")) %>% 
  filter(non_num==TRUE) %>% 
  mutate(non_num2=str_extract(final, "[^[:alnum:]]")) %>% 
  mutate(nchar=nchar(non_num2))

unique(non_num$non_num2)

# final check for plurals ------------------------------------------------------------
# look for last s

source("./bibliometrics/code_data_processing/keyword_s_word_finder.R")
s_words<-keyword_s_word_finder(tw_summary)
s_words
write_csv(s_words, file = "./bibliometrics/data_intermediate/title_s_words.csv")

# write_csv(keywords, file = "./bibliometrics/data_intermediate/kw_int_no_plurals.csv")
# keywords<-read_csv("./bibliometrics/data_intermediate/kw_int_no_plurals.csv")


tw_summary %>%
  select(final) %>%
  group_by(final) %>%
  tally() %>%
  arrange(desc(n))
tw_summary


tw_all<-left_join(tw_all,tw_summary) %>% select(-n)
tw_all_summary<-tw_all %>% 
  group_by(final) %>% 
  tally() %>% 
  arrange(desc(n))
tw_all_summary

# complete_data <- read_csv("./bibliometrics/data_clean/complete_data_clean.csv") %>% 
#   relocate(refID,.before=1)
pub_cat<-complete_data %>% select(refID,pub_cat,jrnl_cat,SO,PY) 

# %>% 
#   group_by(refID) %>% 
#   tally() %>% 
#   arrange(desc(n))



tw_all<-left_join(tw_all,pub_cat)

dupes<-get_dupes(tw_all)
tw_all<-distinct_all(tw_all)


write_csv(tw_all, "./bibliometrics/data_clean/titlewords.csv")
