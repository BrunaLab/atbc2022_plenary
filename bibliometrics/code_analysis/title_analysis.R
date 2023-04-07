
# load libraries ----------------------------------------------------------

library(tidyverse)
library(stopwords)
library(ngram)
library(tidytext)
library(igraph)
library(tidystringdist)

# Load keyword records  -----------------
complete_data<-read_csv("./bibliometrics/data_clean/complete_data.csv") %>% 
  filter(SO!="rbt") %>% 
  # filter(SO!="trop_ecol") %>%
  mutate(TI=gsub(" - "," ",TI)) %>% 
  mutate(pub_cat=as.factor(pub_cat)) %>% 
  mutate(jrnl_cat=as.factor(jrnl_cat)) %>% 
  mutate(index = row_number()) 

unique(complete_data$SO)
summary(complete_data$pub_cat)
summary(complete_data$pub_cat)




# analysis - title words --------------------------------------------------
analysis_data<-complete_data %>% filter(PY>=1976)
summary(is.na(analysis_data$TI))
hist(analysis_data$PY)



pub_year<-analysis_data %>% 
  group_by(jrnl_cat,PY) %>% 
  tally() %>%
  filter(PY>2007)



ggplot(analysis_data, aes(x=PY,color=jrnl_cat)) +
  geom_histogram(fill="white", alpha=0.5)+
  facet_grid(. ~ jrnl_cat)+
  theme_classic()
 



# how many papers per year?
analysis_data %>% group_by(SO,PY) %>% summarize(n_distinct(index)) %>% arrange(PY)



analysis_data %>% group_by(jrnl_cat) %>% count() %>% arrange(desc(n))
# analysis_data %>% group_by(jrnl_cat,PY) %>% summarise_at(vars(jrnl_cat,PY), count, na.rm = TRUE)



jrnl_yrs<-analysis_data %>% group_by(SO,PY,jrnl_cat) %>% tally() %>% arrange (jrnl_cat,SO,PY)
jrnl_yrs$SO <- factor(jrnl_yrs$SO, levels = unique(jrnl_yrs$SO[order(jrnl_yrs$jrnl_cat, jrnl_yrs$SO)]))
jrnl_yrs$PY <- factor(jrnl_yrs$PY, levels = unique(jrnl_yrs$PY[order(jrnl_yrs$jrnl_cat, jrnl_yrs$PY)]))

ggplot(jrnl_yrs, aes(x = PY, y = SO, fill=jrnl_cat, alpha=n)) +
  geom_tile(color = "black")+
  scale_fill_manual(values=c(tropical="red2", global="navyblue")) +
  theme_bw()

jrnl_count<-analysis_data %>% 
  group_by(SO,PY) %>% 
  tally() %>% 
  mutate(SO=as.factor(SO))

p <- ggplot(jrnl_count, aes(PY, n)) + geom_point()
p <- p + facet_wrap(vars(SO))
p



# clean up the titles ----------------------------------------------------


analysis_data<-analysis_data %>% select(refID,jrnl_cat,SO,PY,TI,DE) %>% 
  # drop_na(TI) %>% 
  mutate(TI=gsub(" - "," ",TI)) %>%
  mutate(TI=gsub("- "," ",TI)) %>%
  mutate(TI=gsub(" -"," ",TI)) %>%
  mutate(TI=gsub(".1."," ",TI)) %>%
  mutate(TI=gsub(".2."," ",TI)) %>%
  mutate(TI=gsub(".3."," ",TI)) %>%
  mutate(TI=gsub(".4."," ",TI)) %>%
  mutate(TI=gsub("  "," ",TI)) %>%
mutate(TI=gsub(":","",TI)) %>% 
  mutate(TI=gsub(",","",TI)) %>% 
  mutate(TI=gsub(";","",TI)) %>% 
  mutate(TI=gsub("species diversity","species-diversity",TI)) %>% 
  mutate(TI=gsub("tropical forest","tropical-forest",TI)) %>% 
  mutate(TI=gsub("dry forest","dry-forest",TI)) %>% 
  mutate(TI=gsub("rain forest","rain-forest",TI)) %>% 
  mutate(TI=gsub("seed forest","seed-forest",TI))



# parse titles and clean --------------------------------------------------


tw<-analysis_data %>% 
  drop_na(TI) %>% 
  rename(tw=TI) %>%   
  separate(tw,c(LETTERS[seq( from = 1, to = 60 )]), sep = " ") %>% 
  pivot_longer(!refID:PY, names_to = "letter", values_to = "tw") %>% 
  select(-letter) %>% 
  drop_na(tw) %>% 
  mutate(tw=trimws(tw)) %>% 
  mutate(tw=gsub("\n"," ",tw)) %>%
  mutate(tw=gsub('[[:punct:] ]+',' ',tw)) %>% 
  filter(!(tw %in% stopwords(source = "snowball"))) %>%  # deletes the stopwords
  mutate(tw=tolower(tw)) %>% 
  mutate(tw=trimws(tw)) %>% 
  filter(tw!="") %>% 
  drop_na(tw)

# TODO: remove numbers?

short_tw<-tw %>% 
  # slice(1:50) %>% 
  filter((nchar(tw)<3)==TRUE)

unique_tw<-tw %>%  
  select(tw) %>% 
  distinct() %>% 
  arrange()


tw_summary<-tw %>% group_by(tw) %>% summarize(n=n()) %>% arrange(desc(n))


write_csv(tw_summary,"./bibliometrics/data_raw/tw_summary.csv")


write_csv(unique_tw,"./bibliometrics/data_raw/unique_tw.csv")

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

