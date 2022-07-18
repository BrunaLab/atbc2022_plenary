# putting together and cleaning up the various WOS archives - all together

# load libraries ----------------------------------------------------------

library(refsplitr)
library(tidyverse)
library(countrycode)
library(tictoc)
library(opencage)
library(usethis)


# read in wos with refsplitr ----------------------------------------------
 
source("./bibliometrics/references_read_updated.r")
 wos2_refs <- references_read_updated(data = "./bibliometrics/wos_files",
                              dir = T,
                              include_all = TRUE) 
 
 wos2_refs <- wos2_refs %>% 
   mutate(source="wos") %>% 
   mutate(SO = tolower(SO)) %>% 
   mutate_all(trimws) %>% 
   mutate(SO = case_when(
     SO ==  "biotropica" ~ "bitr",
     SO ==  "evolution" ~ "evol",
     SO ==  "journal of applied ecology" ~ "jae",
     SO ==  "journal of ecology" ~ "jecol",
     SO ==  "tropical ecology" ~ "trop_ecol",
     SO ==  "american naturalist" ~ "amnat",
     SO ==  "the american naturalist" ~ "amnat",
     SO ==  "journal of tropical ecology" ~ "jte",
     SO ==  "journal of animal ecology" ~ "jae",
     SO ==  "the journal of animal ecology" ~ "jae",
     SO ==  "revista de biología tropical" ~ "rbt",
     SO ==  "revista de biologia tropical" ~ "rbt",
     SO ==  "tropical conservation science" ~ "tcs",
     TRUE ~ as.character(SO))) %>% 
   filter(SO!="current science") %>% 
   mutate_all(as.character) %>% 
   mutate(BP=if_else(is.na(BP), AR, BP)) %>% 
   mutate(EP=if_else(is.na(EP), AR, EP))
 
 
 unique(wos2_refs$SO)
 
 write_csv(wos2_refs,"./bibliometrics/data_clean/wos2_refs.csv")
 
 