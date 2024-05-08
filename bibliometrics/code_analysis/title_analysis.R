
# load libraries ----------------------------------------------------------

library(tidyverse)
library(stopwords)
# library(ngram)
library(tidytext)
library(here)
library(kableExtra)
library(cowplot)
library(janitor)
# library(igraph)
# library(tidystringdist)

# Load keyword records  -----------------

tw_all<-read_csv(here("bibliometrics","data_clean","tw_clean.csv")) %>% 
  mutate(pub_cat=as.factor(pub_cat_2)) %>% 
  mutate(jrnl_cat=as.factor(jrnl_cat)) %>% 
  mutate(index = row_number()) %>% 
  mutate(pub_cat_2=as.factor(pub_cat_2))

unique(tw_all$SO)

# select journals ---------------------------------------------------------
tw<-tw_all 

rbt<-tw_all %>% filter(SO=="rbt") %>% group_by(final) %>% tally() %>% arrange(desc(n))

# golfo
# isla
# especies
# quinntana and roo separated , make quintanaroo
# bosque
# bosques
# sotobosque
# murcielago
# insectos

# %>% 
#   filter(SO!="rbt") %>% 
#   filter(SO!="trec")

unique(tw$SO)


# articles for TW analysis ------------------------------------------------

tw_articles<-tw %>% 
  group_by(refID,SO,PY,jrnl_cat) %>% 
  tally() %>% 
  arrange (jrnl_cat,SO,PY) %>% 
  group_by(SO,PY) %>% 
  tally() %>% 
  mutate(SO=as.factor(SO))

p <- ggplot(tw_articles, aes(PY, n)) + geom_point()
p <- p + facet_wrap(vars(SO))
p

# how many tw do we have? -----------------------------------------------

jrnl_yrs<-tw %>% group_by(SO,PY,jrnl_cat) %>% tally() %>% arrange (jrnl_cat,SO,PY)
jrnl_yrs$SO <- factor(jrnl_yrs$SO, levels = unique(jrnl_yrs$SO[order(jrnl_yrs$jrnl_cat, jrnl_yrs$SO)]))
jrnl_yrs$PY <- factor(jrnl_yrs$PY, levels = unique(jrnl_yrs$PY[order(jrnl_yrs$jrnl_cat, jrnl_yrs$PY)]))



# this function helps scale how many ticks to print (every nth tick mark)
every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}


unique(as.factor(jrnl_yrs$jrnl_cat))
coverage_plot<-ggplot(jrnl_yrs, aes(x = PY, y = SO, fill=jrnl_cat, alpha=n)) +
  coord_flip()+
  geom_tile(color = "black")+
  theme_bw()+
  scale_x_discrete(breaks = every_nth(n = 2))+
  # scale_x_discrete(guide = guide_axis(n.dodge=2))+
  scale_fill_manual(values=c(tropical="darkgreen", general="black")) +
  theme(axis.text.y = element_text(angle = 0, hjust = 0, vjust = 0))
coverage_plot
  

jrnl_count<-tw %>% 
  group_by(SO,PY) %>% 
  tally() %>% 
  mutate(SO=as.factor(SO))

p <- ggplot(jrnl_count, aes(PY, n)) + geom_point()
p <- p + facet_wrap(vars(SO))
p


# first / last year by journal category ------------------------------------------
jrnl_years_last<-tw %>% 
  group_by(SO,PY) %>% 
  tally() %>% 
  mutate(SO=as.factor(SO)) %>% 
  group_by(SO) %>% 
  slice_max(PY) %>% 
  rename(yr_last=PY,,n_last=n)

jrnl_years_first<-tw %>% 
  group_by(SO,PY) %>% 
  tally() %>% 
  mutate(SO=as.factor(SO)) %>% 
  group_by(SO) %>% 
  slice_min(PY) %>% 
  rename(yr_first=PY,n_first=n)

jrnl_yrs<-full_join(jrnl_years_first,jrnl_years_last,by="SO")

yr_last<-kw %>% select(PY) %>% max() 
yr_first<-kw %>% select(PY) %>% min() 



# tw in each pub cat over time --------------------------------------




cat_years<-tw %>% 
  group_by(pub_cat_2,PY) %>% 
  tally()
# %>% 
#   mutate(SO=as.factor(SO))


p_cat_yrs <- ggplot(cat_years, aes(PY, n,shape = pub_cat_2,color = pub_cat_2)) + geom_point()
p_cat_yrs <- p_cat_yrs + facet_wrap(vars(pub_cat_2))


p_cat_yrs





# how many tw by cat? -----------------------------------------------------


tw %>% group_by(SO) %>% tally()
tw %>% group_by(jrnl_cat) %>% tally()
tw %>% group_by(pub_cat_2) %>% tally()
tw %>% group_by(jrnl_cat,pub_cat_2) %>% tally()

# uncategorized refs ------------------------------------------------------


uncat<-tw %>% filter(is.na(pub_cat_2))
unique(uncat$SO)


# tw %>% drop_na(final) %>% group_by(refID,final) %>% tally() %>% filter(n>1) 

# TODO: review and clean those with numbers in final KW, many are dates

# analysis - title words --------------------------------------------------

system_list<-read_csv(here("bibliometrics","code_analysis","system.csv"), col_names = FALSE) %>% rename("system"="X1")

# top title words --------------------------------------------------------


top_tw<-tw %>% 
  drop_na(final) %>% 
  group_by(final) %>%
  tally() %>% 
  arrange(desc(n))
# 
top_tw_trop<-tw %>%
  drop_na(final) %>%
  filter(jrnl_cat=="tropical") %>%
  group_by(final) %>%
  tally() %>%
  arrange(desc(n)) %>% 
  mutate(perc=n/sum(n)*100)
top_tw_trop
top_tw_trop_50<-top_tw_trop %>% slice(1:50)
# 
# 
# top_tw_global<-tw %>%
#   drop_na(final) %>% 
#   filter(jrnl_cat=="general") %>% 
#   group_by(final) %>%
#   tally() %>% 
#   arrange(desc(n))
# top_tw_global
# 
# top_tw_global_50<-top_tw_global %>% slice(1:50)



# SIMPLIFIED ANALYSIS FOR COMMENTARY
# 
# # TROPICAL
# tw_trop<-tw %>% 
#   drop_na(final) %>% 
#   filter(jrnl_cat=="tropical") %>% 
#   group_by(final) %>% 
#   summarise(n=n()) %>% 
#   mutate(perc=n/sum(n)*100) %>% 
#   arrange(desc(perc)) %>% 
#   mutate(cat="TT", rank=row_number())
# tw_trop
# # 
# tw_trop %>% filter(str_detect(final,"temperate"))
# tw_gen_trop %>% filter(str_detect(final,"tropical"))

# GENERAL-GENERAL
# tw_gen_gen<-tw %>% 
#   drop_na(final) %>% 
#   filter(jrnl_cat=="general"& pub_cat_2 == "general") %>% 
#   group_by(final) %>% 
#   summarise(n=n()) %>% 
#   mutate(perc=n/sum(n)*100) %>% 
#   arrange(desc(perc)) %>% 
#   mutate(cat="GG", rank=row_number())


# tw_gen_gen %>% filter(str_detect(final,"tropical"))
# tw_gen_gen %>% filter(str_detect(final,"temperate"))

# GENERAL-TROPICAL
# tw_gen_trop<-tw %>% 
#   drop_na(final) %>% 
#   filter(jrnl_cat=="general"& pub_cat_2 == "tropical") %>% 
#   group_by(final) %>% 
#   summarise(n=n()) %>% 
#   mutate(perc=n/sum(n)*100) %>% 
#   arrange(desc(perc)) %>% 
#   mutate(cat="GT", rank=row_number())
# tw_gen_trop
# 
# tw_gen_trop %>% filter(str_detect(final,"temperate"))
# tw_gen_trop %>% filter(str_detect(final,"tropical"))


trop_refs <-tw %>% 
  drop_na(final) %>% 
  filter(pub_cat_2 == "tropical") %>% 
  distinct(refID) %>% 
  tally()

tw_trop <-tw %>% 
  drop_na(final) %>% 
  filter(pub_cat_2 == "tropical") %>% 
  distinct(refID,final) %>% 
  group_by(final) %>% 
  tally(n_distinct(refID)) %>%
  arrange(desc(n)) %>% 
  mutate(perc=n/trop_refs$n*100) %>% 
  mutate(cat="tropical") %>% 
  mutate(rank=row_number())
tw_trop




# GENERAL-TROPICAL

gen_refs <-tw %>% 
  drop_na(final) %>% 
  filter(pub_cat_2 == "general") %>% 
  distinct(refID) %>% 
  tally()

tw_non<-tw %>% 
  drop_na(final) %>% 
  filter(pub_cat_2 == "general") %>% 
  distinct(refID,final) %>% 
  group_by(final) %>% 
  tally(n_distinct(refID)) %>%
  arrange(desc(n)) %>% 
  mutate(perc=n/gen_refs$n*100) %>% 
  mutate(cat="non-tropical") %>% 
  mutate(rank=row_number())

tw_non

tw_trop


rankings_tw<-bind_rows(tw_non,tw_trop)

rankings_tw <- rankings_tw %>%
  mutate(system = if_else((final %in% system_list$system == TRUE), "Y", "N")) %>%
  mutate(system = as.factor(system)) %>% 
  group_by(cat) %>% 
  mutate(rank_perc = rank(desc(perc), ties.method = "random")) %>% 
  filter(rank_perc<=50) 

in_both_tw <- rankings_tw %>%
  group_by(final) %>%
  summarise(n2 = n()) %>%
  filter(n2 > 1) %>%
  mutate(both = TRUE)

plot_tw <- full_join(rankings_tw, in_both_tw, by = "final") %>% 
  select(-n2) %>% 
  replace_na(list(
    "both" = FALSE
  )) %>% 
  group_by(cat) %>%
  arrange(cat, desc(n)) %>%
  mutate(rank_perc = rank(desc(n),  ties.method = c("random"))) 




Trop_tw<-plot_tw %>% 
  filter(cat=="tropical") %>% 
  mutate(TitleWord = paste(final, " (", rank, ")", sep = ""))  %>% 
  mutate(TitleWord = fct_reorder(TitleWord, perc)) 

NonTrop_tw<-plot_tw %>%  
  filter(cat=="non-tropical") %>% 
  mutate(TitleWord = paste(final, " (", rank, ")", sep = ""))  %>% 
  mutate(TitleWord = fct_reorder(TitleWord, perc)) 

update_geom_defaults("text", list(size = 2))


tw1_trop_bar <- Trop_tw %>%   # This trick update the factor levels
  ggplot(aes(x=TitleWord, y=perc,fill = factor(system))) +
  # ggplot(aes(x=final, y=perc_pubs_wth_kw))+
  geom_bar(stat="identity", color="black") +
  scale_fill_manual(values = c("darkgray","lightgray"))+
  scale_y_continuous(limits = c(-3, 30), breaks = seq(0, 20, by = 1))+
  ylab("Articles with Word in Title (%)")+
  # ggtitle("Tropics")+
  # ylim(0, 4)+
  # scale_fill_manual(values = c("white","navy"))+
  coord_flip() +
  geom_text(
    data = Trop_tw, aes(
      x = TitleWord, y = -.1, 
      label = TitleWord, 
      # color = factor(system),
      # size= 6,
      # fontface = "bold"),
      fontface = ifelse(system == "Y", "bold", "plain"),
    ),
    hjust = "right", vjust = 0, nudge_x = 0, nudge_y = 0.08
  ) +
  scale_color_manual(values = c("black","navy"))+
  # scale_y_discrete(expand = c(0, 2),limits = c(0, 4)) +
  theme_classic() + theme(
    panel.border = element_blank(), panel.grid.major = element_blank(),
    axis.line.y = element_line(color = "black", size = 0.0, lineend = "square"),
    axis.line.x = element_line(color = "black", size = 0.5, lineend = "square"),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), # sets colors of axes
    # plot.title = element_text(hjust = 0.8, vjust =-103.5, face = "bold", size = 22, color="navy"), # Sets title size, style, location
    axis.title.x = element_text(colour = "black", size = 10, vjust=-3,hjust = 0.6), # sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
    axis.title.y = element_blank(),
    # axis.title.y = element_text(colour = "black", size = 10, angle = 0, vjust = 0.5), # sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
    legend.position = "none",
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(colour = "black", size = 8),
    axis.text.y = element_blank(),
    # axis.text.y = element_text(colour = "black", size = 10), # sets size and style of labels on axes
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  )

tw1_trop_bar

# nontrop
tw1_nontrop_bar <- NonTrop_tw %>%   # This trick update the factor levels
  ggplot(aes(x=TitleWord, y=perc,fill = factor(system))) +
  # ggplot(aes(x=final, y=perc_pubs_wth_kw))+
  geom_bar(stat="identity", color="black") +
  scale_fill_manual(values = c("darkgray","lightgray"))+
  scale_y_continuous(limits = c(-3, 20), breaks = seq(0, 20, by = 1))+
  ylab("Articles with Word in Title (%)")+
  # ggtitle("Non-Tropical")+
  # ylim(0, 4)+
  # scale_fill_manual(values = c("white","navy"))+
  coord_flip() +
  geom_text(
    data = NonTrop_tw, aes(
      x = TitleWord, y = -.1, 
      label = TitleWord, 
      color = factor(system),
      # size= 6,
      # fontface = "bold"),
      fontface = ifelse(system == "Y", "bold", "plain"),
    ),
    hjust = "right", vjust = 0, nudge_x = 0, nudge_y = 0.08
  ) +
  scale_color_manual(values = c("black","navy"))+
  # scale_y_discrete(expand = c(0, 2),limits = c(0, 4)) +
  theme_classic() + theme(
    panel.border = element_blank(), panel.grid.major = element_blank(),
    axis.line.y = element_line(color = "black", size = 0.0, lineend = "square"),
    axis.line.x = element_line(color = "black", size = 0.5, lineend = "square"),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), # sets colors of axes
    # plot.title = element_text(hjust = 0.8, vjust =-103.5, face = "bold", size = 22, color="navy"), # Sets title size, style, location
    axis.title.x = element_text(colour = "black", size = 10, vjust=-3,hjust = 0.6), # sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
    axis.title.y = element_blank(),
    # axis.title.y = element_text(colour = "black", size = 10, angle = 0, vjust = 0.5), # sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
    legend.position = "none",
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(colour = "black", size = 8),
    axis.text.y = element_blank(),
    # axis.text.y = element_text(colour = "black", size = 10), # sets size and style of labels on axes
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  )

tw1_nontrop_bar








tw_fig<-plot_grid(tw1_trop_bar, tw1_nontrop_bar,
                       nrow = 1,
                       # labels = "AUTO",
                  labels=(c("a. Tropics", "b. Non-Tropical")),
                       label_size = 12
                       # align = "h"
)
tw_fig

ggsave("tw_fig.jpeg", 
       path = "./manuscript/figures", 
       dpi=700,
       width = 10,
       height = 6,
       units = c("in")
)

# as venn diagram ---------------------------------------------------------


top_n<-plot_tw %>% filter(rank_perc<50)

# Generate 3 sets of 200 words
trop <- top_n %>% filter(cat=="tropical") %>% ungroup() %>% select(final) %>% rename(trop=final) 
trop<-as.vector(unlist(trop))

not <- top_n %>% filter(cat!="tropical") %>% ungroup() %>% select(final) %>% rename(not=final) 
not <-as.vector(unlist(not))
library("ggvenn") 
A = list("trop"=trop,"not"=not)
venn_kw<-ggvenn(A,
                show_elements = TRUE, 
                label_sep = "\n",
                auto_scale = TRUE
)
venn_kw


ggsave("venn_tw.jpeg", 
       path = "./manuscript/figures", 
       dpi=700,
       width = 10,
       height = 6,
       units = c("in")
)





# 
# 
# 
# # GENERAL-TROPICAL
# tw_trop<-tw %>% 
#   drop_na(final) %>% 
#   filter(pub_cat_2 == "tropical") %>% 
#   group_by(refID,final) %>%
#   summarise(n=n()) %>% 
#   mutate(perc=n/sum(n)*100) %>% 
#   arrange(desc(perc)) %>% 
#   mutate(cat="Tropical", rank=row_number())
# tw_trop
# 
# 
# 
# single_tw_long<-bind_rows(tw_gen_trop,tw_gen_gen,tw_trop) 


# %>% 
#   group_by(cat)
# %>% 
#   slice(1:50) 
# single_tw_wide<-single_tw_long %>% 
#   pivot_wider(
#     names_from=cat,
#     values_from=c(n,perc,rank)
#     )
# single_tw_wide

# NGRAMS

extract_ngram_data <- function(tw) {
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
  return(ngram_data)
}

ngram_data<-extract_ngram_data(tw)
ngram_data

# ngram_data_trop<-ngram_data %>% filter(jrnl_cat=="tropical")
# ngram_data_gen_gen<-ngram_data %>% filter(jrnl_cat=="general" & pub_cat_2=="general")
# ngram_data_gen_trop<-ngram_data %>% filter(jrnl_cat=="general" & pub_cat_2=="tropical")

ngram_data_gen<-ngram_data %>% filter(pub_cat_2=="general")
ngram_data_trop<-ngram_data %>% filter(pub_cat_2=="tropical")


# ngrams -----------------------------------------------------------------


# to do by cat filter first

generate_bigrams <- function(ngram_data) {
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
    filter(str_detect("deleteme",word2,negate=TRUE)) 
    # slice(1:1000)
    
    
  bigrams_filtered<-bigrams_filtered %>% 
    # filter(nchar(word1)<4) %>%
    mutate(word1_first=substring(word1, 1, 1)) %>% 
    mutate(word2_first=substring(word2, 1, 1)) %>% 
    mutate(number=case_when(
      (str_detect(word1_first,".*[0-9].*")) == TRUE ~ "number",
      (str_detect(word2_first,".*[0-9].*")) == TRUE ~ "number",
      TRUE ~ "keep")) %>% 
    filter(number=="keep") %>% 
    select(-number,-word1_first,-word2_first) %>% 
    mutate(perc=n/sum(n)*100)
  return(bigrams_filtered)
}

  
  # bigrams as % of titles

generate_bigrams <- function(ngram_data) {
  
  n_titles<-ngram_data %>% summarize(n=n_distinct(refID))
  
  tw_bigrams <- ngram_data %>% 
    select(tw,refID) %>% 
    # slice(1:100) %>% 
    unnest_tokens(bigram, tw, token = "ngrams", n = 2) %>% 
    distinct(refID,bigram)
  
  # %>% 
  #   count(refID,bigram, sort = TRUE)
  bigrams_separated <- tw_bigrams %>% 
    separate(bigram, c("word1", "word2"), sep = " ")
  bigrams_filtered <- bigrams_separated  %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word) %>% 
    # arrange(desc(n)) %>% 
    filter(str_detect("deleteme",word1,negate=TRUE)) %>% 
    filter(str_detect("deleteme",word2,negate=TRUE)) 
  # slice(1:1000)
  
  
  bigrams_filtered<-bigrams_filtered %>% 
    # filter(nchar(word1)<4) %>%
    mutate(word1_first=substring(word1, 1, 1)) %>% 
    mutate(word2_first=substring(word2, 1, 1)) %>% 
    mutate(number=case_when(
      (str_detect(word1_first,".*[0-9].*")) == TRUE ~ "number",
      (str_detect(word2_first,".*[0-9].*")) == TRUE ~ "number",
      TRUE ~ "keep")) %>% 
    filter(number=="keep") %>% 
    select(-number,-word1_first,-word2_first)  %>% 
    group_by(word1,word2) %>% 
    summarize(n=n()) %>% 
    arrange(desc(n)) %>% 
    mutate(perc=n/n_titles$n*100)
  return(bigrams_filtered)
}



bigrams<-generate_bigrams(ngram_data)  

bigrams_count<- bigrams %>% 
  unite("bigram",word1:word2, sep = " ") %>% 
  tally()

write_csv(bigrams_count,"./bibliometrics/data_clean/bigrams_count.csv")

NonTrop_bigrams<-generate_bigrams(ngram_data_gen) %>% 
  mutate(cat="non-tropical") 

Trop_bigrams<-generate_bigrams(ngram_data_trop) %>% 
  mutate(cat="tropical") 


NonTrop_bigrams<-NonTrop_bigrams %>%  
  mutate(bigram=paste(word1,word2,sep=" ")) %>% 
  ungroup() %>% 
  mutate(rank_perc = rank(desc(perc), ties.method = "random")) 
# %>%
#   mutate(bigram = paste(bigram, " (", rank_perc, ")", sep = "")) 

Trop_bigrams<-Trop_bigrams %>% 
  mutate(bigram=paste(word1,word2,sep=" ")) %>% 
  ungroup() %>% 
  mutate(rank_perc = rank(desc(perc), ties.method = "random")) 
# %>%
#   mutate(bigram = paste(bigram, " (", rank_perc, ")", sep = ""))   

#################


# NonTrop_bigrams$bigram
# Trop_bigrams$bigram


rankings_pub<-bind_rows(Trop_bigrams,NonTrop_bigrams)

rankings_pub <- rankings_pub %>%
  mutate(system = if_else((bigram %in% system_list$system == TRUE), "Y", "N")) %>%
  mutate(system = as.factor(system)) %>% 
  group_by(cat) %>% 
  mutate(rank_perc = rank(desc(perc), ties.method = "random")) %>% 
  filter(rank_perc<=50) 

in_both <- rankings_pub %>%
  group_by(bigram) %>%
  summarise(n2 = n()) %>%
  filter(n2 > 1) %>%
  mutate(both = TRUE)

plot_tw <- full_join(rankings_pub, in_both, by = "bigram") %>% 
  select(-n2) %>% 
  replace_na(list(
    "both" = FALSE
  )) %>% 
group_by(cat) %>%
  arrange(cat, desc(n)) %>%
  mutate(rank_perc = rank(desc(n),  ties.method = c("random"))) 
  



Trop_bigrams2<-plot_tw %>% 
  filter(cat=="tropical") %>% 
  mutate(bigram = paste(bigram, " (", rank_perc, ")", sep = ""))  %>% 
  mutate(bigram = fct_reorder(bigram, perc)) 

NonTrop_bigrams2<-plot_tw %>%  
  filter(cat=="non-tropical") %>% 
  mutate(bigram = paste(bigram, " (", rank_perc, ")", sep = ""))  %>% 
  mutate(bigram = fct_reorder(bigram, perc)) 



update_geom_defaults("text", list(size = 2))


tw_trop_bar <- Trop_bigrams2 %>%   # This trick update the factor levels
  ggplot(aes(x=bigram, y=perc,fill = factor(both))) +
  # ggplot(aes(x=final, y=perc_pubs_wth_kw))+
  geom_bar(stat="identity", color="black") +
  scale_fill_manual(values = c("darkgray","lightgray"))+
  scale_y_continuous(limits = c(-1.5, 4), breaks = seq(0, 4, by = .5))+
  ylab("Articles with Bi-gram in Title (%)")+
  # ggtitle("Tropics")+
  # ylim(0, 4)+
  # scale_fill_manual(values = c("white","navy"))+
  coord_flip() +
  geom_text(
    data = Trop_bigrams2, aes(
      x = bigram, y = -.1, 
      label = bigram, 
      color = factor(system),
      # size= 6,
      # fontface = "bold"),
      fontface = ifelse(system == "Y", "bold", "plain"),
    ),
    hjust = "right", vjust = 0, nudge_x = 0, nudge_y = 0.08
  ) +
  scale_color_manual(values = c("black","navy"))+
  # scale_y_discrete(expand = c(0, 2),limits = c(0, 4)) +
  theme_classic() + theme(
    panel.border = element_blank(), panel.grid.major = element_blank(),
    axis.line.y = element_line(color = "black", size = 0.0, lineend = "square"),
    axis.line.x = element_line(color = "black", size = 0.5, lineend = "square"),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), # sets colors of axes
    # plot.title = element_text(hjust = 0.8, vjust =-103.5, face = "bold", size = 22, color="navy"), # Sets title size, style, location
    axis.title.x = element_text(colour = "black", size = 10, vjust=-3,hjust = 0.6), # sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
    axis.title.y = element_blank(),
    # axis.title.y = element_text(colour = "black", size = 10, angle = 0, vjust = 0.5), # sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
    legend.position = "none",
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(colour = "black", size = 8),
    axis.text.y = element_blank(),
    # axis.text.y = element_text(colour = "black", size = 10), # sets size and style of labels on axes
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  )

tw_trop_bar

# nontrop
tw_nontrop_bar <- NonTrop_bigrams2 %>%   # This trick update the factor levels
  ggplot(aes(x=bigram, y=perc,fill = factor(both))) +
  # ggplot(aes(x=final, y=perc_pubs_wth_kw))+
  geom_bar(stat="identity", color="black") +
  scale_fill_manual(values = c("darkgray","lightgray"))+
  scale_y_continuous(limits = c(-1.5, 4), breaks = seq(0, 4, by = .5))+
  ylab("Articles with Bi-gram in Title (%)")+
  # ggtitle("Non-Tropical")+
  # ylim(0, 4)+
  # scale_fill_manual(values = c("white","navy"))+
  coord_flip() +
  geom_text(
    data = NonTrop_bigrams2, aes(
      x = bigram, y = -.1, 
      label = bigram, 
      color = factor(system),
      # size= 6,
      # fontface = "bold"),
      fontface = ifelse(system == "Y", "bold", "plain"),
    ),
    hjust = "right", vjust = 0, nudge_x = 0, nudge_y = 0.08
  ) +
  scale_color_manual(values = c("black","navy"))+
  # scale_y_discrete(expand = c(0, 2),limits = c(0, 4)) +
  theme_classic() + theme(
    panel.border = element_blank(), panel.grid.major = element_blank(),
    axis.line.y = element_line(color = "black", size = 0.0, lineend = "square"),
    axis.line.x = element_line(color = "black", size = 0.5, lineend = "square"),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), # sets colors of axes
    # plot.title = element_text(hjust = 0.8, vjust =-103.5, face = "bold", size = 22, color="navy"), # Sets title size, style, location
    axis.title.x = element_text(colour = "black", size = 10, vjust=-3,hjust = 0.6), # sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
    axis.title.y = element_blank(),
    # axis.title.y = element_text(colour = "black", size = 10, angle = 0, vjust = 0.5), # sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
    legend.position = "none",
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(colour = "black", size = 8),
    axis.text.y = element_blank(),
    # axis.text.y = element_text(colour = "black", size = 10), # sets size and style of labels on axes
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  )

tw_nontrop_bar








bigrams_fig<-plot_grid(tw_trop_bar, tw_nontrop_bar,
                 nrow = 1,
                 labels=(c("A) Tropics", "B) Non-Tropical")),
                 # labels = "AUTO",
                 label_size = 12,
                 align = "v"
)
bigrams_fig

ggsave("bigrams_fig.jpeg", 
       path = "./manuscript/figures", 
       dpi=700,
       width = 11,
       height = 6,
       units = c("in")
)






# bigram as venn diagram ---------------------------------------------------



top_n<-plot_tw %>% filter(rank_perc<50)

# Generate 3 sets of 200 words
trop <- top_n %>% filter(cat=="tropical") %>% ungroup() %>% select(bigram) %>% rename(trop=bigram) 
trop<-as.vector(unlist(trop))

not <- top_n %>% filter(cat!="tropical") %>% ungroup() %>% select(bigram) %>% rename(not=bigram) 
not <-as.vector(unlist(not))
library("ggvenn") 
A = list("trop"=trop,"not"=not)
venn_bigram<-ggvenn(A,
                show_elements = TRUE, 
                text_size = 4,
                label_sep = "\n",
                auto_scale = TRUE
)
venn_bigram


ggsave("venn_bigram.jpeg", 
       path = "./manuscript/figures", 
       dpi=700,
       width = 10,
       height = 6,
       units = c("in")
)






# 
# GGbigrams<-generate_bigrams(ngram_data_gen_gen) %>% 
#   mutate(cat="GG", rank=row_number())
# GGbigrams
# 
# 
# TTbigrams<-generate_bigrams(ngram_data_trop)%>% 
#   mutate(cat="TT", rank=row_number())
# TTbigrams 

# group1<-bigrams_filtered %>% arrange(desc(word1)) 
# 
# GTbigrams<-generate_bigrams(ngram_data_gen_trop) %>% 
#   mutate(cat="GT", rank=row_number())
# GTbigrams
# 
# 
# 
# bigrams_long<-bind_rows(GGbigrams,TTbigrams,GTbigrams) %>% 
#   unite("bigram", word1:word2,sep= " ", remove=TRUE)
# 
# bigrams_long<-bind_rows(NonTrop_bigrams,Trop_bigrams) %>% 
#    unite("bigram", word1:word2,sep= " ", remove=TRUE) %>%
#   group_by(cat) %>% 
#   arrange(desc(n)) %>% 
#   mutate(rank=row_number()) 
# 
# glimpse(bigrams_long)
# %>% 
#   group_by(cat)
# %>% 
#   slice(1:50) 
# bigrams_wide<-bigrams_long %>% 
#   pivot_wider(
#     names_from=cat,
#     values_from=c(n,perc,rank)
#   )
# bigrams_wide

# group1<- bigrams_filtered %>% group_by(word1,word2) %>% tally() %>% arrange(desc(n))
# 
# bigrams_long %>% filter(str_detect(bigram,"plant"))
# bigrams_long %>% filter(str_detect(bigram,"population"))
# bigrams_long %>% filter(str_detect(bigram,"forest"))
# bigrams_long %>% filter(str_detect(bigram,"forest")) %>% group_by(bigram) %>% arrange(rank)
# bigrams_long %>% filter(str_detect(bigram,"forest")) %>% group_by(bigram)%>% arrange(bigram)
# 

# Trigrams



generate_trigrams <- function(ngram_data) {
  
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
    
    
  trigrams_filtered<-trigrams_filtered %>% 
    # filter(nchar(word1)<4) %>%
    mutate(word1_first=substring(word1, 1, 1)) %>% 
    mutate(word2_first=substring(word2, 1, 1)) %>% 
    mutate(word3_first=substring(word3, 1, 1)) %>% 
    mutate(number=case_when(
      (str_detect(word1_first,".*[0-9].*")) == TRUE ~ "number",
      (str_detect(word2_first,".*[0-9].*")) == TRUE ~ "number",
      (str_detect(word3_first,".*[0-9].*")) == TRUE ~ "number",
      TRUE ~ "keep")) %>% 
    filter(number=="keep") %>% 
    select(-number,-word1_first,-word2_first,-word3_first) %>% 
    mutate(perc=n/sum(n)*100)
      # slice(1:1000)
  return(trigrams_filtered)

}

trigrams<-generate_trigrams(ngram_data)


NonTrop_trigrams<-generate_trigrams(ngram_data_gen) %>% 
  mutate(cat="non-tropical")
NonTrop_trigrams

Trop_trigrams<-generate_trigrams(ngram_data_trop) %>% 
  mutate(cat="tropical")
Trop_trigrams


trigrams_long<-bind_rows(NonTrop_trigrams,Trop_trigrams) %>% 
  unite("trigram", word1:word3,sep= " ", remove=TRUE) %>%
  group_by(cat) %>% 
  arrange(desc(n)) %>% 
  mutate(rank=row_number()) 


# tables ------------------------------------------------------------------

# top tw

single_tw_long<-bind_rows(tw_trop,tw_non) %>% filter(rank<51)  

single_tw_both<-single_tw_long %>% 
group_by(cat) %>% 
  arrange(rank) %>% 
  # slice(1:50) %>% 
group_by(final,.drop=FALSE) %>% 
  tally() %>% 
  mutate(cat2=if_else(n>1,"Both",NA)) %>% 
  drop_na(cat2) %>% 
  select(-n)


  
  
single_tw_wide<-single_tw_long %>% 
  left_join(single_tw_both,by="final") %>% 
  filter(rank<51) %>% 
  mutate(cat2=if_else(is.na(cat2),cat,cat2)) %>% 
  pivot_wider(names_from = cat2,
              values_from = final) 



tw_both<-single_tw_wide %>% ungroup() %>% select(Both) %>% arrange(Both) %>% unique()
tw_tropical<-single_tw_wide %>% ungroup() %>% select(tropical) %>% arrange(tropical)
tw_nontropical<-single_tw_wide %>% ungroup() %>% select(`non-tropical`) %>% arrange(`non-tropical`)

counter<-max(nrow(tw_both),nrow(tw_tropical),nrow(tw_nontropical))
add_tw<-as.data.frame(rep(NA,(counter-nrow(tw_both))))
names(add_tw)<-"Both"
tw_both<-bind_rows(tw_both,add_tw)


table_tw<-bind_cols(tw_both,tw_tropical,tw_nontropical) %>%
  remove_empty("rows") %>% 
  mutate(across(c(Both,tropical,`non-tropical`), 
                ~ case_when(is.na(.)==TRUE~ "", 
                            is.na(.)==FALSE ~.)))



# Bigrams

bigrams_long<-bind_rows(NonTrop_bigrams,Trop_bigrams) %>% 
  unite("bigram", word1:word2,sep= " ", remove=TRUE) %>%
  group_by(cat) %>% 
  arrange(desc(n)) %>% 
  mutate(rank=row_number()) 


bigrams_both<-bigrams_long %>% 
  group_by(cat) %>% 
  arrange(rank) %>% 
  slice(1:50) %>% 
  group_by(bigram,.drop=FALSE) %>% 
  tally() %>% 
  mutate(cat2=if_else(n>1,"Both",NA)) %>% 
  drop_na(cat2) %>% 
  select(-n)
  
bigrams_wide<-bigrams_long %>% 
  left_join(bigrams_both,by="bigram") %>% 
  filter(rank<51) %>% 
  mutate(cat2=if_else(is.na(cat2),cat,cat2)) %>% 
 pivot_wider(names_from = cat2,
               values_from = bigram) 



bigrams_both<-bigrams_wide %>% ungroup() %>% select(Both) %>% arrange(Both)%>% unique()
bigrams_tropical<-bigrams_wide %>% ungroup() %>% select(tropical) %>% arrange(tropical)%>% unique()
bigrams_nontropical<-bigrams_wide %>% ungroup() %>% select(`non-tropical`) %>% arrange(`non-tropical`)%>% unique()


counter<-max(nrow(bigrams_both),nrow(bigrams_tropical),nrow(bigrams_nontropical))
add_bigrams<-as.data.frame(rep(NA,(counter-nrow(bigrams_both))))
names(add_bigrams)<-"Both"
bigrams_both<-bind_rows(bigrams_both,add_bigrams)

table_bigrams<-bind_cols(bigrams_both,bigrams_tropical,bigrams_nontropical)

table_bigrams<-table_bigrams %>% 
  remove_empty("rows") %>% 
  mutate(across(c(Both,tropical,`non-tropical`), 
                ~ case_when(is.na(.)==TRUE~ "", 
                            is.na(.)==FALSE ~.)))



  kable(table_bigrams,
      digits = 2,
      align = "ccc",
      format = "latex",
      row.names = FALSE,
      escape = TRUE,
      booktabs = T,
      linesep = "", # removes the blank line after every 5 lines
      caption = "Text Caption"
) %>%
  kable_styling(
    bootstrap_options = c("hover"),
    # full_width = F,
    # latex_options="scale_down",
    font_size = 12,
    position = "center"
  )

  
  
















# 
# GGtrigrams<-generate_trigrams(ngram_data_gen_gen) %>% 
#   mutate(cat="GG", rank=row_number())
# GGtrigrams
# 
# TTtrigrams<-generate_trigrams(ngram_data_trop) %>% 
#   mutate(cat="TT", rank=row_number())
# TTtrigrams
# 
# GTtrigrams<-generate_trigrams(ngram_data_gen_trop) %>% 
#   mutate(cat="GT", rank=row_number())
# GTtrigrams
# 
# 
# 
# 
# trigrams_long<-bind_rows(GGtrigrams,TTtrigrams,GTtrigrams) %>% 
#   unite("trigram", word1:word3,sep= " ", remove=TRUE)

# --------------------




# four-grams --------------------------------------------------------------


generate_fourgrams <- function(ngram_data) {
  
  
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
   
}






fourgrams<-generate_fourgrams(ngram_data)

GGfourgrams<-generate_fourgrams(ngram_data_gen_gen) %>% 
mutate(cat="GG", rank=row_number())
GGfourgrams


TTfourgrams<-generate_fourgrams(ngram_data_trop)%>% 
  mutate(cat="TT", rank=row_number())
TTfourgrams

GTfourgrams<-generate_fourgrams(ngram_data_gen_trop) %>% 
  mutate(cat="GT", rank=row_number())
GTfourgrams


fourgrams_long<-bind_rows(GGfourgrams,TTfourgrams,GTfourgrams) %>% 
  unite("fourgram", word1:word4,sep= " ", remove=TRUE)



####### figure
# 
# plot_data<-bigrams_long %>%
#   mutate(cat=paste(jrnl_cat,pub_cat_2,sep="-")) %>% 
#   mutate(system=if_else((final%in%system$value==TRUE), "Y","N")) %>% 
#   filter(rank_perc<=30)


plot_data<-bigrams_long %>% 
           group_by(cat) %>% 
           slice(1:20) %>% 
           filter(cat!="GT")

TT<-plot_data %>% filter(cat=="TT") %>% mutate(bigram = paste("(", rank,") ",bigram, sep="")) 
GG<-plot_data %>% filter(cat=="GG") %>% mutate(bigram = paste(bigram," (",rank,")", sep="")) 

bigrams_fig<-ggplot(data=plot_data, aes(x=cat, y=rank,group=bigram))+
  # geom_line(arrow = arrow(angle = 12, ends = "both", type = "closed"),linetype = "solid",linewidth=0.7, color="darkgray")+
  geom_line(linetype = "dashed",linewidth=0.7, color="darkgray") +
  geom_point(size=4, aes(color=cat))+
  # geom_label(aes(label = bigram))+
  # ,nudge_x = 1)+
  geom_text(data=TT, aes(x=cat, y=rank,label=bigram
                         # ,color=factor(system),
                         # fontface = "bold"),
                         # fontface = ifelse(system =="Y", "bold", "plain")
                          ),
             hjust = "left", vjust = 0, nudge_x = 0.15, nudge_y = -0.1)+
  geom_text(data=GG, aes(x=cat, y=rank,label=bigram
                         # ,color=factor(system)
                         ),
             hjust = "right", vjust = 0, nudge_x = -0.15, nudge_y = -0.1)+
  scale_x_discrete(expand = c(4, 0))+
  # geom_text(data=GT, aes(x=cat, y=rank_perc,label=final,color=factor(system)),
  #            hjust = "right", vjust = 0, nudge_x = -0.02)+
  scale_y_reverse()+
  xlab("Journal Category")+
  ylab("Keyword\nRank")+ 
  # scale_y_continuous(limit=c(20, 0))+
  scale_colour_manual(values=c("darkslategray","#000066"))
bigrams_fig

bigrams_fig<-bigrams_fig + theme_classic() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                             axis.line.y = element_line(color="black", size = 0.0, lineend="square"),
                                             axis.line.x = element_line(color="black", size = 0.5, lineend="square"),
                                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), #sets colors of axes
                                             plot.title = element_text(hjust=0.05, vjust=-1.8, face="bold", size=22),        #Sets title size, style, location
                                             axis.title.x=element_text(colour="black", size = 10, vjust=-2),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
                                             axis.title.y=element_text(colour="black", size = 10,angle = 0, vjust=0.5),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
                                             legend.position = "none",
                                             axis.ticks = element_blank(),
                                             axis.text.x=element_text(colour="black", size = 8),                              #sets size and style of labels on axes
                                             axis.text.y=element_text(colour="black", size = 0),                              #sets size and style of labels on axes
                                             plot.margin = unit(c(0,2,2,1), "cm"))
bigrams_fig

########## trigrams fig



plot_data<-trigrams_long %>% 
  group_by(cat) %>% 
  slice(1:20) %>% 
  filter(cat!="GT")

TT<-plot_data %>% filter(cat=="TT") %>% mutate(trigram = paste("(", rank,") ",trigram, sep="")) 
GG<-plot_data %>% filter(cat=="GG") %>% mutate(trigram = paste(trigram," (",rank,")", sep="")) 

trigrams_fig<-ggplot(data=plot_data, aes(x=cat, y=rank,group=trigram))+
  # geom_line(arrow = arrow(angle = 12, ends = "both", type = "closed"),linetype = "solid",linewidth=0.7, color="darkgray")+
  geom_line(linetype = "dashed",linewidth=0.7, color="darkgray") +
  geom_point(size=4, aes(color=cat))+
  # geom_label(aes(label = bigram))+
  # ,nudge_x = 1)+
  geom_text(data=TT, aes(x=cat, y=rank,label=trigram
                         # ,color=factor(system),
                         # fontface = "bold"),
                         # fontface = ifelse(system =="Y", "bold", "plain")
  ),
  hjust = "left", vjust = 0, nudge_x = 0.1, nudge_y = -0.1)+
  geom_text(data=GG, aes(x=cat, y=rank,label=trigram
                         # ,color=factor(system)
  ),
  hjust = "right", vjust = 0, nudge_x = -0.1, nudge_y = -0.1)+
  scale_x_discrete(expand = c(4, 0))+
  # geom_text(data=GT, aes(x=cat, y=rank_perc,label=final,color=factor(system)),
  #            hjust = "right", vjust = 0, nudge_x = -0.02)+
  scale_y_reverse()+
  xlab("Journal Category")+
  ylab("Keyword\nRank")+ 
  # scale_y_continuous(limit=c(20, 0))+
  scale_colour_manual(values=c("darkslategray","#000066"))
bigrams_fig

trigrams_fig<-trigrams_fig + theme_classic() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                                   axis.line.y = element_line(color="black", size = 0.0, lineend="square"),
                                                   axis.line.x = element_line(color="black", size = 0.5, lineend="square"),
                                                   panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), #sets colors of axes
                                                   plot.title = element_text(hjust=0.05, vjust=-1.8, face="bold", size=22),        #Sets title size, style, location
                                                   axis.title.x=element_text(colour="black", size = 10, vjust=-2),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
                                                   axis.title.y=element_text(colour="black", size = 10,angle = 0, vjust=0.5),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
                                                   legend.position = "none",
                                                   axis.ticks = element_blank(),
                                                   axis.text.x=element_text(colour="black", size = 8),                              #sets size and style of labels on axes
                                                   axis.text.y=element_text(colour="black", size = 0),                              #sets size and style of labels on axes
                                                   plot.margin = unit(c(0,2,2,1), "cm"))
trigrams_fig





########## four fig



plot_data<-fourgrams_long %>% 
  group_by(cat) %>% 
  slice(1:20) %>% 
  filter(cat!="GT")

TT<-plot_data %>% filter(cat=="TT") %>% mutate(fourgram = paste("(", rank,") ",fourgram, sep="")) 
GG<-plot_data %>% filter(cat=="GG") %>% mutate(fourgram = paste(fourgram," (",rank,")", sep="")) 

fourgrams_fig<-ggplot(data=plot_data, aes(x=cat, y=rank,group=fourgram))+
  # geom_line(arrow = arrow(angle = 12, ends = "both", type = "closed"),linetype = "solid",linewidth=0.7, color="darkgray")+
  geom_line(linetype = "dashed",linewidth=0.7, color="darkgray") +
  geom_point(size=4, aes(color=cat))+
  # geom_label(aes(label = bigram))+
  # ,nudge_x = 1)+
  geom_text(data=TT, aes(x=cat, y=rank,label=fourgram
                         # ,color=factor(system),
                         # fontface = "bold"),
                         # fontface = ifelse(system =="Y", "bold", "plain")
  ),
  hjust = "left", vjust = 0, nudge_x = 0.1, nudge_y = -0.1)+
  geom_text(data=GG, aes(x=cat, y=rank,label=fourgram
                         # ,color=factor(system)
  ),
  hjust = "right", vjust = 0, nudge_x = -0.1, nudge_y = -0.1)+
  # geom_text(data=GT, aes(x=cat, y=rank_perc,label=final,color=factor(system)),
  #            hjust = "right", vjust = 0, nudge_x = -0.02)+
  scale_x_discrete(expand = c(4, 0))+
  scale_y_reverse()+
  xlab("Journal Category")+
  ylab("Keyword\nRank")+ 
  # scale_y_continuous(limit=c(20, 0))+
  scale_colour_manual(values=c("darkslategray","#000066"))
fourgrams_fig

fourgrams_fig<-fourgrams_fig + theme_classic() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                                     axis.line.y = element_line(color="black", size = 0.0, lineend="square"),
                                                     axis.line.x = element_line(color="black", size = 0.5, lineend="square"),
                                                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), #sets colors of axes
                                                     plot.title = element_text(hjust=0.05, vjust=-1.8, face="bold", size=22),        #Sets title size, style, location
                                                     axis.title.x=element_text(colour="black", size = 10, vjust=-2),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
                                                     axis.title.y=element_text(colour="black", size = 10,angle = 0, vjust=0.5),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
                                                     legend.position = "none",
                                                     axis.ticks = element_blank(),
                                                     axis.text.x=element_text(colour="black", size = 8),                              #sets size and style of labels on axes
                                                     axis.text.y=element_text(colour="black", size = 0),                              #sets size and style of labels on axes
                                                     plot.margin = unit(c(0,2,2,1), "cm"))
fourgrams_fig



































































# top TW by journal -------------------------------------------------------

articles_per_jrnlcat<-tw_subset %>% 
  drop_na(final) %>% 
  group_by(refID,SO,pub_cat_2) %>% 
  slice(1) %>% 
  group_by(SO,pub_cat_2) %>% 
  summarise(n_pubs=n_distinct(refID))
  
  
top_tw_byJrnl<-tw_subset %>% 
  drop_na(final) %>% 
  group_by(final,SO,pub_cat_2) %>%
  tally() %>% 
  group_by(SO,pub_cat_2) %>% 
  arrange(desc(n)) %>% 
  mutate(rank=row_number()) %>% 
  arrange(SO,pub_cat_2,desc(n))


top_tw_byJrnl_avgRank<-top_tw_byJrnl %>% 
  group_by(pub_cat_2,final) %>% 
  summarize(avg_rank=mean(rank)) %>% 
  arrange(final,pub_cat_2,avg_rank) %>% 
  filter(avg_rank<200)

  
  top_tw_byJrnl<-left_join(top_tw_byJrnl,articles_per_jrnlcat) %>% 
    mutate(perc=n/n_pubs*100)
  
  
  n_temp<-top_tw %>% filter(final=="temperate") %>% ungroup() %>% summarize(sum(n))

  n_trop<-top_tw %>% filter(final=="tropical") %>% ungroup() %>% summarize(sum(n))
  
  
  
  n_trop/n_temp
  
  
  # --------
  
  
  # Top Keywords by Journal (pub cat/jrnl cat)
  top_tw_jrnl_2<-tw %>% 
    drop_na(final) %>% 
    group_by(jrnl_cat,pub_cat_2,SO,final) %>% 
    tally() %>% 
    arrange(jrnl_cat,SO,pub_cat_2,desc(n)) %>% 
    group_by(pub_cat_2,SO) %>% 
    # slice_head(n=100) %>% 
    arrange(desc(jrnl_cat),SO,pub_cat_2,desc(n)) %>% 
    left_join(pubs_per_jrnl_pub_cat) %>% 
    group_by(jrnl_cat,SO,pub_cat_2) %>% 
    mutate(perc_pubs_wth_kw=(n/n_pubs*100)) %>% 
    group_by(pub_cat_2,SO) %>% 
    arrange(jrnl_cat,SO,pub_cat_2,desc(n)) %>% 
    mutate(rank_perc = row_number()) %>% 
    # arrange(rank_perc,jrnl_cat,desc(pub_cat_2))
    arrange(rank_perc,desc(pub_cat_2),jrnl_cat)
  top_tw_jrnl_2
  
  
  
  # DONT RANK BY JOURNAL
  
  
  # Top Keywords by pub cat/jrnl cat
  top_tw_jrnl_3<-tw %>% 
    drop_na(final) %>% 
    group_by(jrnl_cat,pub_cat_2,final) %>% 
    tally() %>% 
    arrange(jrnl_cat,pub_cat_2,desc(n)) %>% 
    group_by(pub_cat_2) %>% 
    # slice_head(n=100) %>% 
    arrange(desc(jrnl_cat),pub_cat_2,desc(n)) %>% 
    left_join(pubs_per_pub_cat) %>% 
    group_by(jrnl_cat,pub_cat_2) %>% 
    mutate(perc_pubs_wth_kw=(n/n_pubs*100)) %>% 
    group_by(jrnl_cat,pub_cat_2) %>% 
    arrange(jrnl_cat,pub_cat_2,desc(n)) %>% 
    mutate(rank_perc = row_number()) %>% 
    # arrange(rank_perc,jrnl_cat,desc(pub_cat_2))
    arrange(rank_perc,desc(pub_cat_2),jrnl_cat)
  top_tw_jrnl_3
  
  
  
  # average word ranking by jrnsal and article type
  
  avg_rank<-top_tw_jrnl_2 %>% 
    group_by(final,jrnl_cat, pub_cat_2) %>% 
    summarize(avg_rank=mean(rank_perc)) %>% 
    arrange(final)
  avg_rank  
  
  
  avg_rank_wide<-avg_rank  %>% 
    pivot_wider(
      names_from = c(jrnl_cat,pub_cat_2),
      values_from = c(avg_rank)
    ) %>% 
    replace_na(list("tropical_tropical" = 0,
                    "general_general" = 0,
                    "general_tropical" = 0))
  # ggplot(avg_rank_wide, aes(x=general_general, y=tropical_tropical)) + 
  #   geom_point(size=6) 
  
  
  
  
  # plot_data<-avg_rank %>% mutate(cat=paste(jrnl_cat,pub_cat_2,sep="-")) %>% 
  #   filter(cat!="general-tropical") 
  
  top_tw_jrnl_3_fig<-top_tw_jrnl_3 %>% 
    mutate(cat=paste(jrnl_cat,pub_cat_2,sep="-")) %>% 
    mutate(system=if_else((final%in%system_list$system==TRUE), "Y","N")) %>% 
    filter(rank_perc<=30)
  
  
  
  
  TT<-top_tw_jrnl_3_fig %>% 
    filter(cat=='tropical-tropical') %>% 
    select(final,cat,rank_perc,system) %>% 
    # mutate(final=paste(rank_perc,final,sep=": "))
    mutate(final=paste("(",rank_perc,") ", final,sep=""))
  
  GG<-top_tw_jrnl_3_fig %>% 
    filter(cat=='general-general') %>% 
    select(final,cat,rank_perc,system) %>% 
    mutate(final=paste(final," (",rank_perc,")", sep=""))
  
  GT<-top_tw_jrnl_3_fig %>% 
    filter(cat=='general-tropical') %>% 
    select(final,cat,rank_perc,system) %>% 
    mutate(final=paste(final," (",rank_perc,")", sep=""))
  
  plot_data <- 
    top_tw_jrnl_3_fig %>% 
    filter(cat!="general-tropical")
  
  in_both<-plot_data %>% group_by(final) %>% summarise(n2=n()) %>% filter(n2>1) %>% mutate(both=TRUE)
  plot_data<-full_join(plot_data,in_both, by="final") %>% select(-n2)
  
  # %>% 
  # filter(cat!="general-general") 
  # filter(cat!="tropical-tropical") %>% 
  
  
  # CHANGE SO THAT WORDS IN COMMON have fiklled circles, all others are color
  
  
  GGTT_fig<-ggplot(data=plot_data, aes(x=cat, y=rank_perc,group=final))+
    # geom_line(arrow = arrow(angle = 12, ends = "both", type = "closed"),linetype = "solid",linewidth=0.7, color="darkgray")+
    geom_line(linetype = "dashed",linewidth=0.7, color="darkgray") +
    # geom_point(size=4, aes(color=both))+
    # geom_label(aes(label = final),nudge_x = 1,)+
    geom_text(data=TT, aes(x=cat, y=rank_perc,label=final,color=factor(system),
                           # fontface = "bold"),
                           fontface = ifelse(system =="Y", "bold", "plain")),
              hjust = "left", vjust = 0, nudge_x = 0.04, nudge_y = -0.1)+
    geom_text(data=GG, aes(x=cat, y=rank_perc,label=final,color=factor(system)),
              hjust = "right", vjust = 0, nudge_x = -0.04, nudge_y = -0.1)+
    # geom_text(data=GT, aes(x=cat, y=rank_perc,label=final,color=factor(system)),
    #            hjust = "right", vjust = 0, nudge_x = -0.02)+
    scale_y_reverse()+
    xlab("Journal Category")+
    ylab("Keyword\nRank")+ 
    # scale_y_continuous(limit=c(20, 0))+
    scale_colour_manual(values=c("darkslategray","#000066"))
  GGTT_fig
  
  GGTT_fig<-GGTT_fig + theme_classic() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                               axis.line.y = element_line(color="black", size = 0.0, lineend="square"),
                                               axis.line.x = element_line(color="black", size = 0.5, lineend="square"),
                                               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), #sets colors of axes
                                               plot.title = element_text(hjust=0.05, vjust=-1.8, face="bold", size=22),        #Sets title size, style, location
                                               axis.title.x=element_text(colour="black", size = 10, vjust=-2),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
                                               axis.title.y=element_text(colour="black", size = 10,angle = 0, vjust=0.5),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
                                               legend.position = "none",
                                               axis.text.x=element_text(colour="black", size = 8),                              #sets size and style of labels on axes
                                               axis.text.y=element_text(colour="black", size = 0),                              #sets size and style of labels on axes
                                               plot.margin = unit(c(0,2,2,1), "cm"))
  GGTT_fig
  
  
  
  
# N-GRAMS -----------------------------------------------------------------


# attempte to parse ngrams from title (not sep by ; like kw are) ----------
# https://www.tidytextmining.com/ngrams.html

ngram_data<-tw_subset %>% filter(jrnl_cat=="tropical")
ngram_data<-tw_subset %>% filter(jrnl_cat=="general")
ngram_data<-tw_subset %>% 
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



# bigrams -----------------------------------------------------------------


# to do by cat filter first

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

group1<- bigrams_filtered %>% group_by(word1,word2) %>% tally() %>% arrange(desc(n))

bigrams_filtered %>% filter(word1=="plant")
bigrams_filtered %>% filter(word1=="population")
bigrams_filtered %>% filter(word1=="forest")
bigrams_filtered %>% filter(word1=="forest") %>% group_by(word1,word2)%>% arrange(desc(n))
bigrams_filtered %>% filter(word1=="forest") %>% group_by(word1,word2)%>% arrange(word2)




# bigrams tropical --------------------------------------------------------

unique(ngram_data$pub_cat_2)
tw_bigrams <- ngram_data %>% 
  # filter(pub_cat_2=="general") %>% 
  filter(jrnl_cat=="general") %>% 
  # filter(jrnl_cat=="tropical") %>% 
  filter(pub_cat_2=="tropical") %>% 
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

group1<-bigrams_filtered %>% arrange(desc(word1))  %>% mutate(perc=n/sum(n)*100) %>% arrange(desc(perc))
group1
group1<- bigrams_filtered %>% group_by(word1,word2) %>% tally() %>% arrange(desc(n))
group1

# tri-grams ---------------------------------------------------------------



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
  filter(str_detect("deleteme",word3,negate=TRUE)) %>% 
  slice(1:1000)
trigrams_filtered


# four-grams --------------------------------------------------------------


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


# five-grams --------------------------------------------------------------


tw_fivegrams <-ngram_data %>% 
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
  filter(str_detect("deleteme",word1,negate=TRUE)) %>% 
  filter(str_detect("deleteme",word2,negate=TRUE)) %>% 
  filter(str_detect("deleteme",word3,negate=TRUE)) %>% 
  filter(str_detect("deleteme",word4,negate=TRUE)) %>% 
  filter(str_detect("deleteme",word5,negate=TRUE)) %>% 
  arrange(desc(n)) %>% 
  slice(1:1000)
fivegrams_filtered

# six-grams ---------------------------------------------------------------



tw_sixgrams <-ngram_data %>% 
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
  filter(str_detect("deleteme",word1,negate=TRUE)) %>% 
  filter(str_detect("deleteme",word2,negate=TRUE)) %>% 
  filter(str_detect("deleteme",word3,negate=TRUE)) %>% 
  filter(str_detect("deleteme",word4,negate=TRUE)) %>% 
  filter(str_detect("deleteme",word5,negate=TRUE)) %>% 
  filter(str_detect("deleteme",word6,negate=TRUE)) %>% 
  arrange(desc(n)) %>% 
  slice(1:1000)
sixgrams_filtered



# graphing N-grams --------------------------------------------------------


library(igraph)
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
# tw_summary<-tw_subset %>% 
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
# short_tw<-tw_subset %>% 
#   # slice(1:50) %>% 
#   filter((nchar(edited)<3)==TRUE)
# 
# unique_tw<-tw_subset %>%  
#   select(edited) %>% 
#   group_by(edited) %>% 
#   tally() %>% 
#   arrange(desc(n))
# 
# 
# tw_summary<-tw_subset %>% group_by(tw) %>% summarize(n=n()) %>% arrange(desc(n))

# 
# write_csv(tw_summary,"./bibliometrics/data_raw/tw_summary.csv")
# 
# 
# write_csv(unique_tw,"./bibliometrics/data_raw/unique_tw.csv")

foo<-tw_subset %>%
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



foo<-tw_subset %>%
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
  filter(!(tw_subset %in% stopwords(source = "snowball"))) %>%  # deletes the stopwords
  mutate(tw=tolower(tw)) %>% 
  mutate(tw=gsub("\n"," ",tw))



pubs_with_tw_tw<-analysis_data %>% select(refID,jrnl_cat,SO,PY,TI,DE) %>% 
  drop_na(TI) %>%
  drop_na(DE) %>%
  rename(tw=TI) %>% 
rename(kw=DE)

tw_both<-pubs_with_tw_tw %>% 
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
  filter(!(tw_subset %in% stopwords(source = "snowball"))) %>%  # deletes the stopwords
  mutate(tw=tolower(tw))
# tw_both$tw<-gsub("\n"," ",tw_both$tw)

tw_both<-pubs_with_tw_tw %>% 
  select(-tw) %>% 
  separate(kw,c(LETTERS[seq( from = 1, to = 20 )]), sep = ";") %>% 
  pivot_longer(!refID:PY, names_to = "letter", values_to = "kw") %>% 
  select(-letter) %>% 
  drop_na(kw) %>% 
  mutate(kw=trimws(kw)) %>% 
  mutate(kw=gsub("\n"," ",kw)) %>% 
  mutate(kw=tolower(kw))
# tw_both$kw<-gsub("\n"," ",tw_both$kw)


# together<-full_join(tw_both,tw_both)
# no_kw<-together %>% filter(is.na(kw))
# both<-together %>% filter(is.na(kw))
# summary(together$tw==together$kw)
# 
# kw$kw<-gsub("\n"," ",kw$kw)

# unique(together$kw)
# unique(together$tw)
tw_bitr<-tw_both %>% 
  filter(SO=="bitr") %>% 
  drop_na(kw) %>% 
  group_by(jrnl_cat,kw) %>%
  tally() %>% 
  arrange(desc(n))
tw_bitr

tw_bitr<-tw_both %>% 
  filter(SO=="bitr") %>% 
  drop_na(tw) %>% 
  group_by(jrnl_cat,tw) %>%
  tally() %>% 
  arrange(desc(n))
tw_bitr

tw_bitr
tw_bitr

tw_bitr_2join<-tw_both %>% 
  # filter(SO=="bitr") %>% 
  drop_na(kw) %>% 
  rename(tw_kw=kw)

tw_bitr_2join<-tw_both %>% 
  # filter(SO=="bitr") %>% 
  drop_na(tw) %>% 
  rename(tw_kw=tw)

joint_tw_kw<-bind_rows(tw_bitr_2join,tw_bitr_2join) 

joint_tw_tw_global<-joint_tw_kw %>% 
  filter(jrnl_cat=="global") %>% 
  group_by(tw_kw) %>%
  tally() %>% 
  arrange(desc(n))
joint_tw_tw_tropical<-joint_tw_kw %>% 
  filter(jrnl_cat=="tropical") %>% 
  group_by(tw_kw) %>%
  tally() %>% 
  arrange(desc(n))
joint_tw_tw_global
joint_tw_tw_tropical


joint_tw_kw




top_tw_trop<-tw_refined %>%
  filter(jrnl_cat=="tropical") %>% 
  group_by(kw) %>%
  tally() %>% 
  arrange(desc(n))
top_tw_trop<-top_tw_trop %>% slice(1:30)

top_tw_global<-tw_refined %>%
  filter(jrnl_cat=="global") %>% 
  group_by(kw) %>%
  tally() %>% 
  arrange(desc(n))
top_tw_global<-top_tw_global %>% slice(1:30)


# top ttitle words --------------------------------------------------------


top_tw<-tw_subset %>% 
  group_by(tw) %>%
  tally() %>% 
  arrange(desc(n))
top_tw

top_tw_trop<-tw_subset %>%
  filter(jrnl_cat=="tropical") %>% 
  group_by(tw) %>%
  tally() %>% 
  arrange(desc(n))
top_tw_trop

top_tw_global<-tw_subset %>%
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

tw_bigrams <-tw_subset %>% 
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

tw_trigrams <-tw_subset %>% 
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


tw_fourgrams <-tw_subset %>% 
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

tw_fivegrams <-tw_subset %>% 
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


tw_sixgrams <-tw_subset %>% 
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



