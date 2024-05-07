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
    # mutate(original = gsub(", usa", " (usa)", original)) %>% #trying to deal with USA problem
    mutate(original = gsub("new york-usa ", "new york (usa) ", original)) %>% #trying to deal with USA problem
    mutate(original = gsub("savannah river site (aiken, south carolina usa)", "savannah river site, aiken, south carolina (usa)", original)) %>%
    mutate(original = gsub("new england,usa", "new england (usa)", original)) %>% #trying to deal with USA problem
    mutate(original = gsub("virginia usa", "virginia; usa;", original)) %>% #trying to deal with USA problem
    mutate(original = gsub("montana usa","montana;usa;",original)) %>% #trying to deal with USA problem
    mutate(original = gsub("washington usa","washington;usa;",original)) %>% #trying to deal with USA problem
    mutate(original = gsub("western great plains usa","western great plains;usa;",original)) %>% #trying to deal with USA problem
    mutate(original = gsub("wisconsin usa","wisconsin;usa;",original)) %>% #trying to deal with USA problem
    mutate(original = gsub("utah usa","utah;usa;",original)) %>% #trying to deal with USA problem
    mutate(original = gsub("texas usa","texas;usa;",original)) %>% #trying to deal with USA problem
    mutate(original = gsub(" usa ", " ; usa ; ", original)) %>%
    # mutate(original = gsub(", usa", "-usa", original)) %>% #trying to deal with USA problem
    # mutate(original = gsub(" usa ", "-usa", original)) %>% #trying to deal with USA problem
    # mutate(original = gsub(" usa)", "-usa)", original)) %>% #trying to deal with USA problem
    # mutate(original = ifelse((str_detect(original, " \\(usa\\)") == TRUE),paste(original,"-usa",sep=""),original)) %>%
    # mutate(original = gsub(" (usa)", "-usa", original)) %>% #trying to deal with USA problem
    # mutate(original = gsub(" (usa) ", "-usa", original)) %>% #trying to deal with USA problem
    mutate(original = gsub(",", ";", original)) %>%
    mutate(original = gsub("; ;", ";", original)) %>% # some had 2 ; separating kw
    mutate(original = gsub(" ;", ";", original)) %>% #eliminate spaces
    mutate(original = gsub("; ", ";", original)) %>% #eliminate spaces
    mutate(original = gsub("- ", "-", original)) %>%
    mutate(original = gsub(" -", "-", original)) %>%
    # mutate(original = ifelse((str_detect(original, " \\(usa\\)") == TRUE),paste(original,"-usa ",sep=""),original)) %>%
    mutate(original = ifelse((str_detect(original, " \\(usa\\)") == TRUE),paste(original,";usa",sep=""),original)) %>%
    mutate(original = ifelse((str_detect(original, "usa\\)") == TRUE),paste(original,";usa",sep=""),original)) %>%
    mutate(original = ifelse((str_detect(original, " \\(usa\\)") == TRUE),gsub("usa","",original),original)) %>%
    mutate(original = ifelse((str_detect(original, "usa\\)") == TRUE),gsub("usa","",original),original)) %>%
    mutate(original = ifelse((str_detect(original, "\\(\\)") == TRUE),gsub("\\(\\)","",original),original)) %>%
    mutate(original = ifelse((str_detect(original, " usa;") == TRUE),gsub(";usa;","",original),original)) %>%
    # the 5 lines above pulls out the usa and adds it to the end as its own keyword, which is important because that doesn't
    # for example split up "california (usa) grasslands" as 3 kw: califonira, grasslands, usa
    mutate(original = gsub("artificial zaps. cameroon", "artificial gaps;cameroon", original)) %>%
    mutate(original = gsub("katanga \\(dem. rep. congo\\)", "katanga;drc", original)) %>%
    mutate(original = gsub("congo brazzaville", "congo;brazzaville", original)) %>%
    mutate(original = gsub("bwindi-uganda", "bwindi;uganda", original)) %>%
    mutate(original = gsub("carbon côte d'ivoire", "carbon;ivory coast", original)) %>%
    mutate(original = gsub("yasuní national park \\(ecuador\\)", "yasuni;ecuador", original)) %>%
      # TODO: can we split the (usa) out with replacing ( with;?)
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
  
  keywords_all<-keywords_all %>% distinct(refID,original) %>% 
    filter(original!="")
  
  
  
  return(keywords_all)
}