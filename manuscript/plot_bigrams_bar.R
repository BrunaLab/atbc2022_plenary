barplot_words <- function() {
library(tidyverse)
# library(stopwords)
# library(ngram)
# library(tidytext)
# library(igraph)
# library(janitor)
# library(tidystringdist)
library(here)
library(cowplot)




tw_all<-read_csv(here("bibliometrics","data_clean","tw_clean.csv")) %>% 
  mutate(pub_cat=as.factor(pub_cat_2)) %>% 
  mutate(jrnl_cat=as.factor(jrnl_cat)) %>% 
  mutate(index = row_number()) %>% 
  mutate(pub_cat_2=as.factor(pub_cat_2)) %>% 
  inner_join(titles_analyzed) %>% 
  relocate(title,.after=SO) %>% 
  mutate(title=as.factor(title)) %>% 
  drop_na(final) 

tw<-tw_all %>% filter(PY>=start_yr) %>% 
  filter(PY<=end_yr)




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


library(stopwords)
# library(ngram)
library(tidytext)

bigrams<-generate_bigrams(ngram_data) 
# bigrams_count<- bigrams %>% 
#   unite("bigram",word1:word2, sep = " ") %>% 
#   tally()
# 
# write_csv(bigrams_count,"./bibliometrics/data_clean/bigrams_count.csv")



Trop_bigrams<-generate_bigrams(ngram_data_trop) %>% 
  mutate(cat="tropical") %>% 
  mutate(bigram=paste(word1,word2,sep=" ")) %>% 
  ungroup() %>% 
  mutate(rank_perc = rank(desc(perc), ties.method = "random")) %>% 
  mutate(system = if_else((bigram %in% system_list$system == TRUE), "Y", "N")) %>%
  mutate(system = as.factor(system)) %>% 
  filter(rank_perc<=cutoff)


system_list$system
NonTrop_bigrams<-generate_bigrams(ngram_data_gen) %>% 
  mutate(cat="non-tropical") %>% 
  mutate(bigram=paste(word1,word2,sep=" ")) %>% 
  mutate(bigram=paste(word1,word2,sep=" ")) %>% 
  ungroup() %>% 
  mutate(rank_perc = rank(desc(perc), ties.method = "random")) %>% 
  mutate(system = if_else((bigram %in% system_list$system == TRUE), "Y", "N")) %>%
  mutate(system = as.factor(system)) %>% 
  filter(rank_perc<=cutoff)

#################



plot_term <- full_join(Trop_bigrams, NonTrop_bigrams)



trop_term <- plot_term %>%
  filter(cat == "tropical") %>%
  select(bigram, cat, rank_perc, perc,system) %>%
  mutate(original=bigram) %>%
  mutate(bigram = paste(bigram, " - ", rank_perc, " ", sep = "")) %>% 
  mutate(bigram = fct_reorder(bigram, perc))

nontrop_term <- plot_term %>%
  filter(cat == "non-tropical") %>%
  select(bigram, cat, rank_perc, perc,system) %>%
  mutate(original=bigram) %>%
  mutate(bigram = paste(bigram, " - ", rank_perc, " ", sep = "")) %>% 
  mutate(bigram = fct_reorder(bigram, perc)) 

# three keyword list - trop, nontrop, overlap -----------------------------
# 
# in_both <- rankings_pub %>%
#   filter(rank_perc<=cutoff) %>% 
#   group_by(bigram) %>%
#   summarise(n2 = n()) %>%
#   filter(n2 > 1) %>%
#   mutate(both = TRUE) %>% 
#   mutate(original=bigram)
# 
# unique_trop_term<-trop_term %>% filter(rank_perc<=cutoff) %>% anti_join(in_both,by="original") 
# unique_nontrop_term<-nontrop_term %>% filter(rank_perc<=cutoff) %>% anti_join(in_both,by="original") 
# 
# in_both_term<- inner_join(trop_term,in_both,by="original") %>% 
#   inner_join(nontrop_term,in_both,by="original") %>% 
#   mutate(bigram.x=paste(bigram.x, " (",rank_perc.y,")",sep="")) %>% 
#   rename(rank_perc_trop=rank_perc.x,
#          rank_perc_non=rank_perc.y,
#          system=system.x
#   ) %>% 
#   select(-system.y,-bigram.y) %>% 
#   filter(is.na(original)==FALSE) %>% 
#   filter(rank_perc_trop<=cutoff) %>% 
#   filter(rank_perc_non<=cutoff)

# unique_trop_term
# unique_nontrop_term
# in_both_term




axis_max<-max((max(nontrop_term$perc)),
              (max(trop_term$perc)))*
  1.06


GeomLabel$default_aes$size

update_geom_defaults("text", list(size = 2))
# https://stackoverflow.com/questions/25061822/ggplot-geom-text-font-size-control

breaks_vec<-seq(0, axis_max, by = .5)
min_x<- -1.3
max_x<-axis_max



kw_trop_bar <- trop_term %>%   # This trick update the factor levels
  ggplot(aes(x=bigram, y=perc,fill = factor(system))) +
  geom_bar(stat="identity", color="black",size=0.2) +
  scale_fill_manual(values = c("white","slateblue4"))+
  # scale_y_continuous(limits = c(min_x, 6), breaks = seq(0, 6, by = .5))+
  scale_y_continuous(limits = c(min_x, max_x), breaks = breaks_vec)+
  ylab("Articles with Keyword (%)")+
  # ggtitle("Tropics")+
  # ylim(0, 4)+
  # scale_fill_manual(values = c("white","navy"))+
  coord_flip() +
  geom_text(
    data = trop_term, aes(
      x = bigram, y = -.12, 
      label = bigram, 
      color = factor(system),
      # fontface = "bold"),
      fontface = ifelse(system == "Y", "bold", "plain"),
    ),
    hjust = "right", vjust = 0, nudge_x = 0, nudge_y = 0.08
  ) +
  scale_color_manual(values = c("black","midnightblue"))+
  # scale_y_discrete(expand = c(0, 2),limits = c(0, 4)) +
  theme_classic() + theme(
    panel.border = element_blank(), panel.grid.major = element_blank(),
    axis.line.y = element_line(color = "black", size = 0.0, lineend = "square"),
    axis.line.x = element_line(color = "black", size = 0.5, lineend = "square"),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), # sets colors of axes
    plot.title = element_text(hjust = 0.8, vjust =-103.5, face = "bold", size = 22, color="navy"), # Sets title size, style, location
    axis.title.x = element_text(colour = "black", size = 10, vjust=-3,hjust = 0.6), # sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
    axis.title.y = element_blank(),
    # axis.title.y = element_text(colour = "black", size = 10, angle = 0, vjust = 0.5), # sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
    legend.position = "none",
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(colour = "black", size = 8),
    axis.text.y = element_blank(),
    # axis.text.y = element_text(colour = "black", size = 10), # sets size and style of labels on axes
    plot.margin = unit(c(1, 0.4, .5, .4), "cm")
  )

kw_trop_bar



kw_notrop_bar <- nontrop_term %>%   # This trick update the factor levels
  ggplot(aes(x=bigram, y=perc,fill = factor(system))) +
  geom_bar(stat="identity", color="black",size=0.2) +
  scale_fill_manual(values = c("white","slateblue4"))+
  # scale_y_continuous(limits = c(min_x, 6), breaks = seq(0, 6, by = .5))+
  scale_y_continuous(limits = c(min_x, max_x), breaks = breaks_vec)+
  # scale_y_continuous(limits = c(min_x, axis_max), n.breaks=10,minor_breaks=2)+
  ylab("Articles with Keyword (%)")+
  # ggtitle("Non-tropical")+
  # ylim(0, 4)+
  # scale_fill_manual(values = c("white","navy"))+
  coord_flip() +
  geom_text(
    data = nontrop_term, aes(
      x = bigram, y = -.12, 
      label = bigram, 
      color = factor(system),
      # size= 6,
      # fontface = "bold"),
      fontface = ifelse(system == "Y", "bold", "plain"),
    ),
    hjust = "right", vjust = 0, nudge_x = 0, nudge_y = 0.08
  ) +
  scale_color_manual(values = c("black","midnightblue"))+
  # scale_y_discrete(expand = c(0, 2),limits = c(0, 4)) +
  theme_classic() + theme(
    panel.border = element_blank(), panel.grid.major = element_blank(),
    axis.line.y = element_line(color = "black", size = 0.0, lineend = "square"),
    axis.line.x = element_line(color = "black", size = 0.5, lineend = "square"),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), # sets colors of axes
    plot.title = element_text(hjust = 0.8, vjust =-103.5, face = "bold", size = 22, color="navy"), # Sets title size, style, location
    axis.title.x = element_text(colour = "black", size = 10, vjust=-3,hjust = 0.6), # sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
    axis.title.y = element_blank(),
    # axis.title.y = element_text(colour = "black", size = 10, angle = 0, vjust = 0.5), # sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
    legend.position = "none",
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(colour = "black", size = 8),
    axis.text.y = element_blank(),
    # axis.text.y = element_text(colour = "black", size = 10), # sets size and style of labels on axes
    plot.margin = unit(c(1, 0.4, .5, .4), "cm")
  )

kw_notrop_bar

terms_fig<-plot_grid(kw_trop_bar, kw_notrop_bar,
                     nrow = 1,
                     # labels = "AUTO",
                     labels=(c("a. 'Tropical' Articles","b. 'Non-tropical' Articles")),
                     label_size = 10,
                     align = "v"
)

terms_fig


# for KW
ggsave("bigram_fig.jpeg", 
       path = "./manuscript/figures", 
       dpi=700,
       width = 10,
       height = 7,
       units = c("in")
)


}