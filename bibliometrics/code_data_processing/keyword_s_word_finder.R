keyword_s_word_finder <- function(keywords) {
  library(tidyverse)
  library(stringr)
  
  unique_kw_summary <- keywords %>%
    group_by(singular) %>%
    tally() %>% 
    arrange(desc(n))
  
  # final check for plurals ------------------------------------------------------------
  # look for last s
  
  s_words<-unique_kw_summary %>%
    # separate(singular,c("first", "second","third"), " ", extra = "merge") %>% 
    separate(singular,c("first", "last_word"), " ", fill = "left") %>% 
    mutate(last_no_s=str_sub(last_word, 1,str_length(last_word)-1)) %>%  
    relocate(last_no_s,.after = "last_word") %>% 
    # mutate(last_word=case_when(
    #  (is.na(third)==TRUE & is.na(second)==TRUE)~first,
    #  (is.na(third)==TRUE & is.na(second)!=TRUE)~second,
    #  (is.na(second)!=TRUE & is.na(third)!=TRUE)~third,
    #  TRUE~as.factor("ugh")
    #  )) %>%
    filter(nchar(last_word)>5) %>% 
    # arrange(desc(nchar(last_word))) %>% 
    # slice(1:1) %>% 
    mutate(final_letter=str_sub(last_word, - 1, - 1)) %>% 
    mutate(final_2letter=str_sub(last_word, - 2, - 1)) %>%  
    mutate(final_3letter=str_sub(last_word, - 3, - 1)) %>%  
    filter(str_detect(last_word,"oides")!=TRUE)  %>% 
    filter(str_detect(last_word,"spides")!=TRUE)  %>% 
    filter(str_detect(last_word,"ormes")!=TRUE)  %>% 
    filter(str_detect(last_word,"yops")!=TRUE)  %>% 
    filter(str_detect(last_word,"ales")!=TRUE)  %>%
    filter(str_detect(last_word,"cites")!=TRUE)  %>%
    filter(str_detect(last_word,"aes")!=TRUE)  %>% 
    # filter(str_detect(last_word,"dae")!=TRUE)  %>% 
    filter(str_detect(last_word,"termes")!=TRUE)  %>% 
    filter(str_detect(last_word,"ichos")!=TRUE)  %>% 
    filter(str_detect(last_word,"otheres")!=TRUE)  %>% 
    filter(str_detect(last_word,"iops")!=TRUE)  %>% 
    filter(str_detect(last_word,"icans")!=TRUE)  %>% 
    filter(str_detect(last_word,"poda")!=TRUE)  %>% 
    filter(str_detect(last_word,"arians")!=TRUE)  %>% 
    filter(str_detect(last_word,"ulidas")!=TRUE)  %>% 
    filter(str_detect(last_word,"podes")!=TRUE)  %>%
    filter(str_detect(last_word,"cetes")!=TRUE)  %>% 
    filter(str_detect(last_word,"monas")!=TRUE)  %>% 
    filter(str_detect(last_word,"chos")!=TRUE)  %>% 
    filter(str_detect(last_word,"frons")!=TRUE)  %>% 
    filter(str_detect(last_word,"cytes")!=TRUE)  %>% 
    filter(str_detect(last_word,"ipes")!=TRUE)  %>% 
    filter(str_detect(last_word,"gens")!=TRUE)  %>% 
    filter(str_detect(last_word,"tans")!=TRUE)  %>% 
    filter(str_detect(last_word,"ideae")!=TRUE)  %>% 
    filter(final_letter=="s") %>% 
    filter(final_2letter!="ss") %>% 
    filter(final_2letter!="is") %>% 
    filter(final_2letter!="us") %>% 
    filter(final_2letter!="ys") %>% 
    filter(final_3letter!="ies") %>%
    distinct(last_word, .keep_all=TRUE)
  
  s_words
  
  
  return(s_words)
}