
# load libraries ----------------------------------------------------------

library(tidyverse)
library(stopwords)
# library(ngram)
library(tidytext)
# library(igraph)
# library(tidystringdist)

# Load keyword records  -----------------
tw<-read_csv("./bibliometrics/data_clean/tw_clean.csv") %>% 
  filter(SO!="rbt") %>% 
  # filter(SO!="trop_ecol") %>%
  mutate(pub_cat=as.factor(pub_cat_2)) %>% 
  mutate(jrnl_cat=as.factor(jrnl_cat)) %>% 
  mutate(index = row_number()) 

unique(tw$SO)
summary(tw$pub_cat_2)
summary(tw$pub_cat_2)

uncat<-tw %>% filter(is.na(pub_cat_2))
unique(uncat$SO)


# tw %>% drop_na(final) %>% group_by(refID,final) %>% tally() %>% filter(n>1) 



# analysis - title words --------------------------------------------------

# top ttitle words --------------------------------------------------------


top_tw<-tw %>% 
  drop_na(final) %>% 
  group_by(final) %>%
  tally() %>% 
  arrange(desc(n))
top_tw

top_tw_trop<-tw %>%
  drop_na(final) %>% 
  filter(jrnl_cat=="tropical") %>% 
  group_by(final) %>%
  tally() %>% 
  arrange(desc(n))
top_tw_trop

top_tw_global<-tw %>%
  drop_na(final) %>% 
  filter(jrnl_cat=="general") %>% 
  group_by(final) %>%
  tally() %>% 
  arrange(desc(n))
top_tw_global




# attempte to parse ngrams from title (not sep by ; like kw are) ----------
# https://www.tidytextmining.com/ngrams.html
ngram_data<-tw %>% filter(jrnl_cat=="tropical")
ngram_data<-tw %>% filter(jrnl_cat=="general")
ngram_data<-tw %>% 
  select(refID,PY,SO,pub_cat_2,jrnl_cat,final) %>%  
  arrange(refID,PY,SO) %>% 
  group_by(refID,PY,SO) %>% mutate(word=row_number()) %>% 
  relocate(word,.before=final) %>% 
  mutate(place="place") %>% 
  relocate(place,.before=word) 
# %>% 
#   replace_na(list(final="-"))
ngram_data<-ngram_data %>% replace_na(list(final= "deleteme")) %>% 
  pivot_wider(names_from = c(place,word),
                               values_from = final,
                               values_fn = list, values_fill = list("deleteme")
                               ) %>%
  ungroup()

last_col<-ngram_data %>% select(last_col())
last_col<-last(names(ngram_data))
ngram_data<- ngram_data %>% 
  unite("tw", place_1:last_col, na.rm = FALSE, remove = TRUE, sep= " ") %>% 
  ungroup()

# ngram_data$tw<-gsub(" NA "," deleteme ",ngram_data$tw)
# ngram_data$tw<-gsub("NA ","deleteme ",ngram_data$tw)




tw_bigrams <- ngram_data %>% 
  select(tw) %>% 
  # slice(1:100) %>% 
  unnest_tokens(bigram, tw, token = "ngrams", n = 2) %>% 
  count(bigram, sort = TRUE) 
bigrams_separated <- tw_bigrams %>% 
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated  %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  arrange(desc(n)) %>% 
  filter(str_detect("deleteme",word1,negate=TRUE)) %>% 
  filter(str_detect("deleteme",word2,negate=TRUE)) %>% 
  slice(1:1000)
bigrams_filtered

group1<-bigrams_filtered %>% arrange(desc(word1)) 

group1<- bigrams_filtered %>% group_by(word1) %>% tally() %>% arrange(desc(n))

bigrams_filtered %>% filter(word1=="plant")
bigrams_filtered %>% filter(word1=="population")
bigrams_filtered %>% filter(word1=="forest")
group1 %>% group_by(word2) %>% tally() %>% arrange(desc(n))


tw_trigrams <-ngram_data %>% 
  select(tw) %>% 
  # slice(1:10) %>% 
  unnest_tokens(trigram, tw, token = "ngrams", n = 3) %>% 
  count(trigram, sort = TRUE) 
trigrams_separated <- tw_trigrams %>% 
  separate(trigram, c("word1", "word2","word3"), sep = " ")
trigrams_filtered <- trigrams_separated  %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word3 %in% stop_words$word) %>% 
  arrange(desc(n)) %>% 
  filter(str_detect("deleteme",word1,negate=TRUE)) %>% 
  filter(str_detect("deleteme",word2,negate=TRUE)) %>% 
  filter(str_detect("deleteme",word3,negate=TRUE))

%>% 
  slice(1:1000)
trigrams_filtered


tw_fourgrams <-ngram_data %>% 
  select(tw) %>% 
  # slice(1:10) %>% 
  unnest_tokens(fourgram, tw, token = "ngrams", n = 4) %>% 
  count(fourgram, sort = TRUE) 
fourgrams_separated <- tw_fourgrams %>% 
  separate(fourgram, c("word1", "word2","word3","word4"), sep = " ")
fourgrams_filtered <- fourgrams_separated  %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word3 %in% stop_words$word) %>% 
  filter(!word4 %in% stop_words$word) %>% 
  filter(str_detect("deleteme",word1,negate=TRUE)) %>% 
  filter(str_detect("deleteme",word2,negate=TRUE)) %>% 
  filter(str_detect("deleteme",word3,negate=TRUE)) %>% 
  filter(str_detect("deleteme",word4,negate=TRUE)) %>% 
  arrange(desc(n)) %>% 
  slice(1:1000)
fourgrams_filtered

tw_fivegrams <-tw %>% 
  select(tw) %>% 
  # slice(1:10) %>% 
  unnest_tokens(fivegram, tw, token = "ngrams", n = 5) %>% 
  count(fivegram, sort = TRUE) 
fivegrams_separated <- tw_fivegrams %>% 
  separate(fivegram, c("word1", "word2","word3","word4","word5"), sep = " ")
fivegrams_filtered <- fivegrams_separated  %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word3 %in% stop_words$word) %>% 
  filter(!word4 %in% stop_words$word) %>% 
  filter(!word5 %in% stop_words$word) %>% 
  arrange(desc(n)) %>% 
  slice(1:1000)
fivegrams_filtered


tw_sixgrams <-tw %>% 
  select(tw) %>% 
  # slice(1:10) %>% 
  unnest_tokens(sixgram, tw, token = "ngrams", n = 6) %>% 
  count(sixgram, sort = TRUE) 
sixgrams_separated <- tw_sixgrams %>% 
  separate(sixgram, c("word1", "word2","word3","word4","word5","word6"), sep = " ")
sixgrams_filtered <- sixgrams_separated  %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word3 %in% stop_words$word) %>% 
  filter(!word4 %in% stop_words$word) %>% 
  filter(!word5 %in% stop_words$word) %>% 
  filter(!word6 %in% stop_words$word) %>% 
  arrange(desc(n)) %>% 
  slice(1:1000)
sixgrams_filtered




bigram_graph <- bigrams_filtered %>%
  filter(n > 40) %>%
  graph_from_data_frame()


library(ggraph)
set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

set.seed(2020)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()




# 
# tw_summary<-tw %>% 
#   group_by(original) %>% 
#   tally() %>% 
#   arrange(desc(n)) 
# %>% 
#   # mutate(edited=gsub('[[:punct:] ]+',' ',edited)) %>% 
#   filter(!(edited %in% stopwords(source = "snowball"))) %>%  # deletes the stopwords
#   mutate(edited=tolower(edited)) %>% 
#   mutate(edited=trimws(edited)) %>% 
#   filter(edited!="") %>% 
#   drop_na(edited)


# 
# # TODO: remove numbers?
# 
# short_tw<-tw %>% 
#   # slice(1:50) %>% 
#   filter((nchar(edited)<3)==TRUE)
# 
# unique_tw<-tw %>%  
#   select(edited) %>% 
#   group_by(edited) %>% 
#   tally() %>% 
#   arrange(desc(n))
# 
# 
# tw_summary<-tw %>% group_by(tw) %>% summarize(n=n()) %>% arrange(desc(n))

# 
# write_csv(tw_summary,"./bibliometrics/data_raw/tw_summary.csv")
# 
# 
# write_csv(unique_tw,"./bibliometrics/data_raw/unique_tw.csv")

foo<-tw %>%
  group_by(tw) %>%
  tally() %>% 
  arrange(desc(n)) %>% 
  slice(1:50) %>% 
  print(n=50)

# 
# top title words
# overall
# overall trop vs. general
# overall trop vs. general by decade



foo<-tw %>%
  mutate(decade=(floor(PY/10)*10)) %>% 
  # group_by(PY,article_cat,tw) %>%
  group_by(decade,tw) %>%
  tally() %>% 
  arrange(desc(n)) %>% 
  slice(1:10) %>% 
  print(n=50)














tw<-analysis_data %>% select(refID,jrnl_cat,SO,PY,TI) %>% 
  rename(tw=TI) %>% 
  mutate(tw=gsub(" - "," ",tw)) %>% 
  separate(tw,c(LETTERS[seq( from = 1, to = 60 )]), sep = " ") %>% 
  pivot_longer(!refID:PY, names_to = "letter", values_to = "tw") %>% 
  select(-letter) %>% 
  drop_na(tw) %>% 
  mutate(tw=trimws(tw)) %>% 
  filter(!(tw %in% stopwords(source = "snowball"))) %>%  # deletes the stopwords
  mutate(tw=tolower(tw)) %>% 
  mutate(tw=gsub("\n"," ",tw))



pubs_with_kw_tw<-analysis_data %>% select(refID,jrnl_cat,SO,PY,TI,DE) %>% 
  drop_na(TI) %>%
  drop_na(DE) %>%
  rename(tw=TI) %>% 
rename(kw=DE)

tw_both<-pubs_with_kw_tw %>% 
  # select(-kw) %>% 
  mutate(tw=gsub(":","",tw)) %>% 
  mutate(tw=gsub(",","",tw)) %>% 
  mutate(tw=gsub(";","",tw)) %>% 
  mutate(tw=gsub("species diversity","species-diversity",tw)) %>% 
  mutate(tw=gsub("tropical forest","tropical-forest",tw)) %>% 
  mutate(tw=gsub("dry forest","dry-forest",tw)) %>% 
  mutate(tw=gsub("rain forest","rain-forest",tw)) %>% 
  mutate(tw=gsub("seed forest","seed-forest",tw)) %>% 
  separate(tw,c(LETTERS[seq( from = 1, to = 60 )]), sep = " ") %>% 
  pivot_longer(!refID:PY, names_to = "letter", values_to = "tw") %>% 
  select(-letter) %>% 
  drop_na(tw) %>% 
  mutate(tw=trimws(tw)) %>% 
  mutate(tw=gsub("\n"," ",tw)) %>% 
  filter(!(tw %in% stopwords(source = "snowball"))) %>%  # deletes the stopwords
  mutate(tw=tolower(tw))
# tw_both$tw<-gsub("\n"," ",tw_both$tw)

kw_both<-pubs_with_kw_tw %>% 
  select(-tw) %>% 
  separate(kw,c(LETTERS[seq( from = 1, to = 20 )]), sep = ";") %>% 
  pivot_longer(!refID:PY, names_to = "letter", values_to = "kw") %>% 
  select(-letter) %>% 
  drop_na(kw) %>% 
  mutate(kw=trimws(kw)) %>% 
  mutate(kw=gsub("\n"," ",kw)) %>% 
  mutate(kw=tolower(kw))
# kw_both$kw<-gsub("\n"," ",kw_both$kw)


# together<-full_join(tw_both,kw_both)
# no_kw<-together %>% filter(is.na(kw))
# both<-together %>% filter(is.na(kw))
# summary(together$tw==together$kw)
# 
# kw$kw<-gsub("\n"," ",kw$kw)

# unique(together$kw)
# unique(together$tw)
kw_bitr<-kw_both %>% 
  filter(SO=="bitr") %>% 
  drop_na(kw) %>% 
  group_by(jrnl_cat,kw) %>%
  tally() %>% 
  arrange(desc(n))
kw_bitr

tw_bitr<-tw_both %>% 
  filter(SO=="bitr") %>% 
  drop_na(tw) %>% 
  group_by(jrnl_cat,tw) %>%
  tally() %>% 
  arrange(desc(n))
tw_bitr

kw_bitr
tw_bitr

kw_bitr_2join<-kw_both %>% 
  # filter(SO=="bitr") %>% 
  drop_na(kw) %>% 
  rename(tw_kw=kw)

tw_bitr_2join<-tw_both %>% 
  # filter(SO=="bitr") %>% 
  drop_na(tw) %>% 
  rename(tw_kw=tw)

joint_tw_kw<-bind_rows(kw_bitr_2join,tw_bitr_2join) 

joint_tw_kw_global<-joint_tw_kw %>% 
  filter(jrnl_cat=="global") %>% 
  group_by(tw_kw) %>%
  tally() %>% 
  arrange(desc(n))
joint_tw_kw_tropical<-joint_tw_kw %>% 
  filter(jrnl_cat=="tropical") %>% 
  group_by(tw_kw) %>%
  tally() %>% 
  arrange(desc(n))
joint_tw_kw_global
joint_tw_kw_tropical


joint_tw_kw




top_kw_trop<-kw_refined %>%
  filter(jrnl_cat=="tropical") %>% 
  group_by(kw) %>%
  tally() %>% 
  arrange(desc(n))
top_kw_trop<-top_kw_trop %>% slice(1:30)

top_kw_global<-kw_refined %>%
  filter(jrnl_cat=="global") %>% 
  group_by(kw) %>%
  tally() %>% 
  arrange(desc(n))
top_kw_global<-top_kw_global %>% slice(1:30)


# top ttitle words --------------------------------------------------------


top_tw<-tw %>% 
  group_by(tw) %>%
  tally() %>% 
  arrange(desc(n))
top_tw

top_tw_trop<-tw %>%
  filter(jrnl_cat=="tropical") %>% 
  group_by(tw) %>%
  tally() %>% 
  arrange(desc(n))
top_tw_trop

top_tw_global<-tw %>%
  filter(jrnl_cat=="general") %>% 
  group_by(tw) %>%
  tally() %>% 
  arrange(desc(n))
top_tw_global




# attempte to parse ngrams from title (not sep by ; like kw are) ----------
# https://www.tidytextmining.com/ngrams.html
ngram_data<-analysis_data %>% filter(jrnl_cat=="tropical")
ngram_data<-analysis_data %>% filter(jrnl_cat=="general")
ngram_data<-analysis_data

tw<-ngram_data %>% select(refID,jrnl_cat,SO,PY,TI) %>% 
  drop_na(TI) %>% 
  rename(tw=TI) %>% 
  mutate(tw=gsub(" - "," ",tw)) 

tw_bigrams <-tw %>% 
  select(tw) %>% 
  # slice(1:10) %>% 
  unnest_tokens(bigram, tw, token = "ngrams", n = 2) %>% 
  count(bigram, sort = TRUE) 
bigrams_separated <- tw_bigrams %>% 
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated  %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  arrange(desc(n)) %>% 
  slice(1:1000)
bigrams_filtered

tw_trigrams <-tw %>% 
  select(tw) %>% 
  # slice(1:10) %>% 
  unnest_tokens(trigram, tw, token = "ngrams", n = 3) %>% 
  count(trigram, sort = TRUE) 
trigrams_separated <- tw_trigrams %>% 
  separate(trigram, c("word1", "word2","word3"), sep = " ")
trigrams_filtered <- trigrams_separated  %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word3 %in% stop_words$word) %>% 
  arrange(desc(n)) %>% 
  slice(1:1000)
trigrams_filtered


tw_fourgrams <-tw %>% 
  select(tw) %>% 
  # slice(1:10) %>% 
  unnest_tokens(fourgram, tw, token = "ngrams", n = 4) %>% 
  count(fourgram, sort = TRUE) 
fourgrams_separated <- tw_fourgrams %>% 
  separate(fourgram, c("word1", "word2","word3","word4"), sep = " ")
fourgrams_filtered <- fourgrams_separated  %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word3 %in% stop_words$word) %>% 
  filter(!word4 %in% stop_words$word) %>% 
  arrange(desc(n)) %>% 
  slice(1:1000)
fourgrams_filtered

tw_fivegrams <-tw %>% 
  select(tw) %>% 
  # slice(1:10) %>% 
  unnest_tokens(fivegram, tw, token = "ngrams", n = 5) %>% 
  count(fivegram, sort = TRUE) 
fivegrams_separated <- tw_fivegrams %>% 
  separate(fivegram, c("word1", "word2","word3","word4","word5"), sep = " ")
fivegrams_filtered <- fivegrams_separated  %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word3 %in% stop_words$word) %>% 
  filter(!word4 %in% stop_words$word) %>% 
  filter(!word5 %in% stop_words$word) %>% 
  arrange(desc(n)) %>% 
  slice(1:1000)
fivegrams_filtered


tw_sixgrams <-tw %>% 
  select(tw) %>% 
  # slice(1:10) %>% 
  unnest_tokens(sixgram, tw, token = "ngrams", n = 6) %>% 
  count(sixgram, sort = TRUE) 
sixgrams_separated <- tw_sixgrams %>% 
  separate(sixgram, c("word1", "word2","word3","word4","word5","word6"), sep = " ")
sixgrams_filtered <- sixgrams_separated  %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word3 %in% stop_words$word) %>% 
  filter(!word4 %in% stop_words$word) %>% 
  filter(!word5 %in% stop_words$word) %>% 
  filter(!word6 %in% stop_words$word) %>% 
  arrange(desc(n)) %>% 
  slice(1:1000)
sixgrams_filtered




bigram_graph <- bigrams_filtered %>%
  filter(n > 40) %>%
  graph_from_data_frame()


library(ggraph)
set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

set.seed(2020)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()



