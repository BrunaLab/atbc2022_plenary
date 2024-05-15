# # Prep the file of ALL keywords and titlewords for use in BITR MS.
# 
# 
# 
# if (!dir.exists("./data_ms")) {
#   dir.create("./data_ms")
# } else {
#   print(" ") # placeholder
# }
# 
# urlfile_plants<-("https://raw.githubusercontent.com/BrunaLab/HeliconiaSurveys/master/data/survey_archive/HDP_survey.csv")
# ha_plants<-read_csv(url(urlfile_plants))
# write_csv(ha_plants, here("manuscript_files", "MetadataS1", "data_downloaded","ha_plants.csv"))
# 
# urlfile_plots<-"https://raw.githubusercontent.com/BrunaLab/HeliconiaSurveys/master/data/survey_archive/HDP_plots.csv"
# ha_plots<-read_csv(url(urlfile_plots))
# write_csv(ha_plots,here("manuscript_files", "MetadataS1", "data_downloaded","ha_plots.csv"))
# 
# # This is to save the info on the version of the data sets used in the paper 
# urlfile_v_plots<-"https://raw.githubusercontent.com/BrunaLab/HeliconiaSurveys/master/data/survey_archive/HDP_plots_version_info.txt"
# version_plots<-read_lines(url(urlfile_v_plots))
# 
# urlfile_v_survey<-"https://raw.githubusercontent.com/BrunaLab/HeliconiaSurveys/master/data/survey_archive/HDP_survey_version_info.txt"
# version_survey<-read_lines(url(urlfile_v_survey))
# 
# version_survey$file<-"survey"
# names(version_survey)<-c("version", "date", "file")
# version_plots$file<-"plots"
# names(version_plots)<-c("version", "date", "file")
# versions_for_ms<-bind_rows(as_tibble(version_survey),
#                            as_tibble(version_plots)) %>% 
#   relocate(file,.before=1)
# write_csv(versions_for_ms,here("manuscript_files", "MetadataS1", "data_downloaded","versions_for_ms.csv"))
# 

# system info -------------------------------------------------------------


system_list<-read_csv(here("bibliometrics","code_analysis","system.csv"), col_names = TRUE) %>% 
  filter(geo==TRUE)


# titles analyzed ---------------------------------------------------------

titles_ms<-
  read_csv(here("bibliometrics","code_analysis","titles.csv"), col_names = TRUE) %>% 
  filter(SO!="rbt") 

write_csv(titles_ms,here("manuscript","data_ms","titles_ms.csv")) 

# title words -------------------------------------------------------------


tw_ms<-read_csv(here("bibliometrics","data_clean","tw_clean.csv")) %>% 
  inner_join(titles_ms) %>% 
  relocate(title,.after=SO) %>% 
  mutate(title=as.factor(title)) %>% 
  mutate(pub_cat=as.factor(pub_cat_2)) %>% 
  mutate(jrnl_cat=as.factor(jrnl_cat)) %>% 
  mutate(index = row_number()) %>% 
  mutate(pub_cat_2=as.factor(pub_cat_2)) %>% 
  mutate(system = if_else((final %in% system_list$system == TRUE), "Y", "N")) %>%
  mutate(system = as.factor(system)) %>% 
  mutate(system = if_else((final %in% system_list$system == TRUE), "Y", "N")) %>%
  mutate(system = as.factor(system)) 

write_csv(tw_ms,here("manuscript","data_ms","tw_ms.csv")) 

# keywords ----------------------------------------------------------------


kw_ms<-read_csv(here("bibliometrics","data_clean","keywords.csv")) %>% 
  inner_join(titles_ms) %>%
  relocate(title,.after=SO) %>%
  mutate(title=as.factor(title)) %>%
  mutate(system = if_else((final %in% system_list$system == TRUE), "Y", "N")) %>%
  mutate(system = as.factor(system)) %>% 
  # filter(PY>=start_yr) %>% 
  # filter(PY<=end_yr) %>% 
  mutate(system = if_else((final %in% system_list$system == TRUE), "Y", "N")) %>%
  mutate(system = as.factor(system)) 

write_csv(kw_ms,here("manuscript","data_ms","kw_ms.csv")) 



# ngram prep --------------------------------------------------------------


extract_ngram_data <- function(tw) {
  ngram_data<-tw_ms %>% 
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

ngram_data<-extract_ngram_data(tw_ms)


ngram_data_gen<-ngram_data %>% filter(pub_cat_2=="general")
ngram_data_trop<-ngram_data %>% filter(pub_cat_2=="tropical")



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


# bigrams - all pubs

bigrams<-generate_bigrams(ngram_data)  
bigrams

write_csv(bigrams,here("manuscript","data_ms","all_bigrams.csv")) 

bigrams_count<- bigrams %>% 
  unite("bigram",word1:word2, sep = " ") %>% 
  tally()



# bigrams: NON-tropical
NonTrop_bigrams<-generate_bigrams(ngram_data_gen) %>% 
  mutate(cat="non-tropical")  %>%  
  mutate(bigram=paste(word1,word2,sep=" ")) %>% 
  ungroup() %>% 
  mutate(rank_perc = rank(desc(perc), ties.method = "random")) 


# bigrams: tropical
Trop_bigrams<-generate_bigrams(ngram_data_trop) %>% 
  mutate(cat="tropical")  %>% 
  mutate(bigram=paste(word1,word2,sep=" ")) %>% 
  ungroup() %>% 
  mutate(rank_perc = rank(desc(perc), ties.method = "random")) 

## all bigrams together
rankings_pub<-bind_rows(Trop_bigrams,NonTrop_bigrams)


rankings_pub <- rankings_pub %>%
  mutate(system = if_else((bigram %in% system_list$system == TRUE), "Y", "N")) %>%
  mutate(system = as.factor(system)) %>% 
  group_by(cat) %>% 
  mutate(rank_perc = rank(desc(perc), ties.method = "random")) 


write_csv(rankings_pub,here("manuscript","data_ms","ranked_bigrams.csv")) 

