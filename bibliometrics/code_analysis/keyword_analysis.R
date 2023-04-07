
# load libraries ----------------------------------------------------------

library(tidyverse)
library(stopwords)
library(ngram)
library(tidytext)
library(igraph)
library(janitor)
library(tidystringdist)
complete_data<-read_csv("./bibliometrics/data_clean/complete_data.csv")


bitr<-complete_data %>% 
  filter(SO=="bitr") %>% 
  filter(is.na(DE)) %>% 
  filter(PY>1991) %>% 
  arrange(desc(PY)) %>% 
  relocate(DI,DE,.before=1) 

analysis_refs<-complete_data %>% 
  relocate(refID,source,SO,PY,DI,DE,AU,.before=1)

names(analysis_refs)

unique(analysis_refs$jrnl_cat)


analysis_refs %>% group_by(jrnl_cat,SO,pub_cat) %>% tally()




# how much data to we have? -----------------------------------------------

# how many are missing

jrnl_artciles<-analysis_refs %>% 
  filter(PY>1990) %>% 
  group_by(SO) %>% 
  summarize(tot=n()) %>%
  arrange(desc(tot))

jrnl_no_kw<-analysis_refs %>% 
  filter(is.na(DE)==TRUE) %>% 
  filter(PY>1990) %>% 
  group_by(SO) %>% 
  summarize(n=n()) %>%
  arrange(desc(n)) 
  
jrnl_summs<-left_join(jrnl_artciles,jrnl_no_kw) %>% 
  mutate(perc=n/tot*100) %>% 
  arrange(desc(perc)) 


# coverage - overall
jrnl_yrs<-analysis_refs %>% 
  filter(is.na(DE)==TRUE) %>% 
  filter(PY>1990) %>% 
  # filter(SO!="amnat") %>% 
  group_by(SO,PY,jrnl_cat) %>% 
  tally() %>% 
  arrange (jrnl_cat,SO,PY)
jrnl_yrs$SO <- factor(jrnl_yrs$SO, levels = unique(jrnl_yrs$SO[order(jrnl_yrs$jrnl_cat, jrnl_yrs$SO)]))
jrnl_yrs$PY <- factor(jrnl_yrs$PY, levels = unique(jrnl_yrs$PY[order(jrnl_yrs$jrnl_cat, jrnl_yrs$PY)]))

ggplot(jrnl_yrs, aes(x = PY, y = SO, fill=jrnl_cat, alpha=n)) +
  geom_tile(color = "black")+
  scale_fill_manual(values=c(tropical="red2", general="navyblue")) +
  theme_bw()



# coverage - key words
jrnl_yrs<-analysis_refs %>% 
  drop_na(DE) %>% 
  group_by(SO,PY,jrnl_cat) %>% 
  tally() %>% 
  arrange (jrnl_cat,SO,PY)
jrnl_yrs$SO <- factor(jrnl_yrs$SO, levels = unique(jrnl_yrs$SO[order(jrnl_yrs$jrnl_cat, jrnl_yrs$SO)]))
jrnl_yrs$PY <- factor(jrnl_yrs$PY, levels = unique(jrnl_yrs$PY[order(jrnl_yrs$jrnl_cat, jrnl_yrs$PY)]))

ggplot(jrnl_yrs, aes(x = PY, y = SO, fill=jrnl_cat, alpha=n)) +
  geom_tile(color = "black")+
  scale_fill_manual(values=c(tropical="red2", general="navyblue")) +
  theme_bw()

jrnl_count<-analysis_refs %>% 
  group_by(SO,PY) %>% 
  tally() %>% 
  mutate(SO=as.factor(SO))

p <- ggplot(jrnl_count, aes(PY, n)) + geom_point()
p <- p + facet_wrap(vars(SO))
p









  
  foo<-str_split_fixed(foo$DE,";",n=20) 
  foo<-foo %>% as_tibble() 

# Load keyword records  -----------------

kw_refined<-read_csv("./keyword_analysis/kw_refined.csv") %>% 
  rename(kw=kw_refined) 
  # mutate(TI=gsub(" - "," ",TI))
unique(kw_refined$SO)



range(kw_refined$PY)
kw_refined %>% distinct(refID) %>% count(DI,jrnl_cat)


top_kw<-kw_refined %>% 
  group_by(kw) %>%
  tally() %>% 
  arrange(desc(n))
top_kw


top_kw_trop<-kw_refined %>% 
  filter(jrnl_cat=="tropical") %>% 
  group_by(kw) %>%
  tally() %>% 
  arrange(desc(n))
top_kw_trop %>% slice(1:20)

top_kw_global<-kw_refined %>% 
  filter(jrnl_cat=="global") %>% 
  group_by(kw) %>%
  tally() %>% 
  arrange(desc(n))
top_kw_global %>% slice(1:20)


ecoevo_trop_DI<-read_csv("./bibliometrics/data_clean/ecoevo_trop.csv")  %>%
  mutate_all(as.character) %>% 
  mutate_all(tolower) %>% 
  mutate_all(trimws) %>% 
  select(DI,PY,SO,refID) %>% 
  drop_na() %>% 
  # mutate(PY=as.numeric(PY)) %>% 
  mutate(SO = case_when(
    SO ==  "biotropica" ~ "bitr",
    SO ==  "evolution" ~ "evol",
    SO ==  "journal of animal ecology" ~ "jae",
    SO ==  "ecology" ~ "ecology",
    SO ==  "journal of ecology" ~ "jecol",  
    SO ==  "journal of applied ecology" ~ "jappecol",  
    SO ==  "journal of tropical ecology" ~ "jte",
    SO ==  "tropical ecology" ~ "trop_ecol",
    SO ==  "american naturalist" ~ "amnat",
    SO ==  "revista de biologia tropical" ~ "rbt",
    SO ==  "tropical conservation science" ~ "tcs",
    TRUE ~ as.character(SO))) %>% 
  mutate(pub_cat="tropical") %>% 
  tibble() %>% 
  mutate_all(as.character) %>% 
  mutate(DI2=gsub(" ","",DI)) %>% 
  mutate(DI=substr(DI, start = 1, stop = 12))



kw_global<-kw_refined %>% 
  filter(jrnl_cat=="global") %>% 
  mutate_all(as.character) %>% 
  mutate(DI=substr(DI, start = 1, stop = 12))
kw_global2<-semi_join(kw_global,ecoevo_trop_DI,by="DI")

kw<-kw_refined %>% filter(!is.na(jrnl_cat))
# https://stackoverflow.com/questions/66030942/tidytext-clustering
kw_cluster<-kw %>% 
  count(jrnl_cat, SO,kw, sort = TRUE) %>%
  cast_sparse(SO, kw, n)


class(kw_cluster)
dim(kw_cluster)

kfit <- kmeans(kw_cluster, centers = 4)

enframe(kfit$cluster, value = "cluster") %>%
  # separate(name, into = c("jrnl_type"), sep = "_") %>%
  count(name, cluster) %>%
  arrange(cluster)


# topic models ------------------------------------------------------------


# https://www.tidytextmining.com/topicmodeling.html
# library(topicmodels)
# data("AssociatedPress")
unique(kw_refined$SO)
kw<-kw_refined %>% filter(!is.na(jrnl_cat))


kw_refined %>% count(SO)

# kw_dtm<-kw %>% 
#   count(SO,kw) %>% 
#   rename(document=SO,term=kw, count=n) %>% 
#   cast_dtm(document,term,count)
kw_dtm<-kw %>% 
  count(jrnl_cat,kw) %>% 
  rename(document=jrnl_cat,term=kw, count=n) %>% 
  cast_dtm(document,term,count)
library(topicmodels)
# set a seed so that the output of the model is predictable
kw_lda <- LDA(kw_dtm, k = 4, control = list(seed = 1234))
kw_lda

kw_topics <- tidy(kw_lda, matrix = "beta")
kw_topics

kw_top_terms <- kw_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)
kw_top_terms


kw_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()



beta_wide <- kw_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>% 
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_wide %>%
  slice(1:30) %>% 
  select(term,log_ratio) %>% 
  arrange(log_ratio) %>% 
  mutate(term = reorder(term,log_ratio)) %>%
  ggplot(aes(log_ratio, term)) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered()


kw_documents <- tidy(kw_lda, matrix = "gamma")
kw_documents

tidy(kw_dtm) %>%
  filter(document == "tropical") %>%
  arrange(desc(count))


tidy(kw_dtm) %>%
  filter(document == "global") %>%
  arrange(desc(count))


# kw wordclouds -----------------------------------------------------------


# wordclouds
library(wordcloud)
set.seed(1234) # for reproducibility 
wordcloud(
  words = kw_comp$all_non_trop_pubs, 
  # words = kw_trop$kw, 
  freq = kw_comp$n.x, 
  min.freq = 1,
  max.words=50, 
  random.order=FALSE, 
  # rot.per=0.35,
  colors=brewer.pal(8, "Dark2"))
# 
# 
# setdiff(kw_trop$kw,kw_not$kw)
# setdiff(kw_not$kw,kw_trop$kw)
# intersect(kw_trop$kw,kw_not$kw)


# analysis - title words --------------------------------------------------



# clean up the titles ----------------------------------------------------

range(merged_refs$PY,na.rm=TRUE)
merged_refs<-merged_refs %>% select(refID,jrnl_cat,SO,PY,TI) %>% 
  # drop_na(TI) %>% 
  mutate(TI=gsub(" - "," ",TI)) %>%
mutate(TI=gsub(":","",TI)) %>% 
  mutate(TI=gsub(",","",TI)) %>% 
  mutate(TI=gsub(";","",TI)) %>% 
  mutate(TI=gsub("species diversity","species-diversity",TI)) %>% 
  mutate(TI=gsub("tropical forest","tropical-forest",TI)) %>% 
  mutate(TI=gsub("dry forest","dry-forest",TI)) %>% 
  mutate(TI=gsub("rain forest","rain-forest",TI)) %>% 
  mutate(TI=gsub("seed forest","seed-forest",TI))



# parse titles and clean --------------------------------------------------


tw<-merged_refs %>% 
  drop_na(TI) %>% 
  rename(tw=TI) %>%   
  separate(tw,c(LETTERS[seq( from = 1, to = 60 )]), sep = " ") %>% 
  pivot_longer(!refID:PY, names_to = "letter", values_to = "tw") %>% 
  select(-letter) %>% 
  drop_na(tw) %>% 
  mutate(tw=trimws(tw)) %>% 
  mutate(tw=gsub("\n"," ",tw)) %>% 
  filter(!(tw %in% stopwords(source = "snowball"))) %>%  # deletes the stopwords
  mutate(tw=tolower(tw))





tw<-merged_refs %>% select(refID,jrnl_cat,SO,PY,TI) %>% 
  
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



pubs_with_kw_tw<-merged_refs %>% select(refID,jrnl_cat,SO,PY,TI) %>% 
  drop_na(TI) %>% 
  rename(tw=TI) 

tw_both<-pubs_with_kw_tw %>% 
  select(-kw) %>% 
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




top_kw_trop<-kw %>%
  filter(jrnl_cat=="tropical") %>% 
  group_by(kw) %>%
  tally() %>% 
  arrange(desc(n))
top_kw_trop<-top_kw_trop %>% slice(1:30)

top_kw_global<-kw %>%
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


top_tw_global<-tw %>%
  filter(jrnl_cat=="global") %>% 
  group_by(tw) %>%
  tally() %>% 
  arrange(desc(n))
top_tw_global




# attempte to parse ngrams from title (not sep by ; like kw are) ----------
# https://www.tidytextmining.com/ngrams.html
ngram_data<-merged_refs %>% filter(jrnl_cat=="tropical")
ngram_data<-merged_refs %>% filter(jrnl_cat=="global")
ngram_data<-merged_refs

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

