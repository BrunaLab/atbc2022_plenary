keyword_splitter <- function(complete_data) {
  library(tidyverse)
  
  
  keywords_all <- complete_data %>%
    select(refID, DE) %>%
    drop_na(DE) %>%
    rename(original = DE) %>%
    # mutate(DE = gsub("\n", " ", DE)) %>% 
    # mutate(DE = gsub("\"", "", DE)) %>%
    mutate(original = gsub("<bold>", "", original)) %>%
    mutate(original = gsub("</bold>", "", original)) %>%
    mutate(original = gsub("</bold>", "", original)) %>%
    mutate(original = gsub("&#8208", "-", original)) %>%
    mutate(original = gsub("&#8211", "-", original)) %>%
    mutate(original = gsub("&#8217", "'", original)) %>%
    mutate(original = gsub("&amp", "&", original)) %>% 
    #   some of the key words are seperated by ",". This complicates things because some key words are also in the
    # format "biomass, microbial" (instead of microbial biomass"). Also have "STRI, Panama" or "Montana, USA" But easier to split, i think.
    mutate(original = gsub(",", ";", original)) %>%
    mutate(original = gsub("; ;", ";", original)) %>% # some had 2 ; separating kw
    mutate(original = gsub(" ;", ";", original)) %>% #eliminate spaces
    mutate(original = gsub("; ", ";", original)) %>% #eliminate spaces
    mutate(original = gsub("- ", "-", original)) %>%
    mutate(original = gsub(" -", "-", original)) %>%
    # most efficient way to split withoutknowing number of columns
    # https://stackoverflow.com/questions/33288695/how-to-use-tidyrseparate-when-the-number-of-needed-variables-is-unknown
    mutate(to = strsplit(original, ";")) %>%
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
    mutate(original = trimws(original))
  
  return(keywords_all)
}