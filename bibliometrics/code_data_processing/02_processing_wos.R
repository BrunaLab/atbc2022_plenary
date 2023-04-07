# putting together and cleaning up the various WOS archives - all together

# load libraries ----------------------------------------------------------

library(refsplitr)
library(tidyverse)
library(countrycode)
library(opencage)
library(usethis)
library(janitor)


# load_edited_refsplitr ----------------------------------------------
 
source("./bibliometrics/references_read_updated.r")


# load and process data ---------------------------------------------------



wos_refs_all <- references_read_updated(data = "./bibliometrics/data_raw/wos",
                              dir = T,
                              include_all = TRUE) %>% 
  as_tibble() %>% 
  mutate_all(as.character) %>% 
  write_csv("./bibliometrics/data_intermediate/wos_refs_all.csv")


# 
wos_refs_ee <- references_read_updated(data = "./bibliometrics/data_raw/wos_tropical_in_ee",
                                       dir = T,
                                       include_all = TRUE) %>%
  filter(PY<2023) %>%
  # mutate(PY=as.double(PY)) %>%
  mutate(pub_cat="tropical") %>%
  as_tibble() %>%
  drop_na(DI) %>%
  mutate_all(as.character) %>%
  write_csv("./bibliometrics/data_intermediate/wos_refs_trop_in_ee.csv")


wos_doi_check <- references_read_updated(data = "./bibliometrics/data_raw/doi_check_wos/",
                                       dir = T,
                                       include_all = TRUE) %>% 
  filter(PY<2023) %>% 
  # mutate(PY=as.double(PY)) %>% 
  as_tibble() %>% 
  drop_na(DI) %>% 
  mutate_all(as.character) %>% 
  write_csv("./bibliometrics/data_intermediate/wos_doi_check.csv")


# compare_df_cols(wos_refs_all,wos_refs_ee)
# wos_trop_ee_original <- read_csv("./bibliometrics/data_raw/ecoevo_trop.csv") %>% 
#   mutate(pub_cat="tropical") %>% 
#   as_tibble() %>% 
#   drop_na(DI) %>% 
#   mutate(refID=as.character(refID))


wos_refs_ee<-wos_refs_ee %>% mutate_all(as.character)
wos_refs_all<-wos_refs_all %>% mutate_all(as.character)
wos_doi_check<-wos_doi_check %>% mutate_all(as.character)



# 10.1111/btp.13050
# 10.1111/btp.12598
# wos_refs<-bind_rows(wos_refs_all,wos_refs_ee,wos_doi_check) %>% 
wos_refs<-bind_rows(wos_refs_all,wos_doi_check) %>% 
# wos_refs<-full_join(wos_refs_all,wos_refs_ee) %>% 
#   full_join(wos_doi_check) %>% 
   mutate(source="wos") %>% 
   mutate(SO = tolower(SO)) %>% 
   mutate_all(trimws) %>% 
   mutate(SO = case_when(
     SO ==  "journal of evolutionary biology" ~ "jeb",
     SO ==  "biotropica" ~ "bitr",
     SO ==  "evolution" ~ "evol",
     # SO ==  "journal of applied ecology" ~ "jae",
     SO ==  "journal of ecology" ~ "jecol",
     SO ==  "tropical ecology" ~ "trec",
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
   mutate(EP=if_else(is.na(EP), AR, EP)) %>% 
   mutate(jrnl_cat = case_when(
     SO ==  "bitr"~"tropical",
     SO ==  "evol"~"general",
     SO ==  "jae"~"general",
     SO ==  "ecology"~"general",
     SO ==  "jeb"~"general",
     SO ==  "jecol"~"general",  
     SO ==  "jte"~"tropical",
     SO ==  "trec"~"tropical",
     SO ==  "amnat"~"general",
     SO ==  "rbt"~"tropical",
     SO ==  "tcs"~"tropical",
     TRUE ~ as.character(SO))) %>% 
   mutate(pub_cat = case_when(
     SO ==  "bitr"~"tropical",
     SO ==  "jte"~"tropical",
     SO ==  "trec"~"tropical",
     SO ==  "rbt"~"tropical",
     SO ==  "tcs"~"tropical",
     TRUE ~ as.character("general"))) %>% 
  relocate(refID,pub_cat,jrnl_cat,.before=1)
 
 unique(wos_refs$SO)
 
 write_csv(wos_refs,"./bibliometrics/data_intermediate/wos_refs.csv")
 
 wos_refs %>% filter(DI=="10.1111/btp.13050") %>% select(DE)
 # 10.1111/btp.13050
 
 
 # # these are the duplicates. select the row that *isn't* tagged as tropical
 # wos_refs_dupes<-
 #   wos_refs %>% 
 #   get_dupes(DI,SO,PY,BP,PG) %>% 
 #   arrange(DI,SO,PY,BP,PG,pub_cat) %>%
 #   group_by(DI,SO,PY,BP,PG) %>% 
 #   drop_na(DI) %>% 
 #   filter(row_number()>1) %>% 
 #   select(refID)
 # 
 # # remove them with an antijoin
 # 
 # wos_refs<-wos_refs %>% 
 #   anti_join(wos_refs_dupes) %>% 
 #   relocate(DI,.before=2)
 #   
 # 
 # # again, this time without the DI
 # wos_refs_dupes<-
 #   wos_refs %>% 
 #   get_dupes(SO,PY,BP,AU) %>% 
 #   group_by(SO,PY,BP,AU) %>% 
 #   filter(row_number()==2) 
 # 
 # # remove them with an antijoin
 # 
 # wos_refs<-wos_refs %>% 
 #   anti_join(wos_refs_dupes) %>% 
 #   relocate(DI,.before=2)
 # 
 # 
 # # again, this time based on AU and TI
 # wos_refs_dupes<-
 #   wos_refs %>% 
 #   get_dupes(SO,PY,AU,TI) %>% 
 #   group_by(SO,PY,AU,TI)  %>% 
 #   filter(row_number()==2) 
 # 
 # # remove them with an antijoin
 # 
 # wos_refs<-wos_refs %>% 
 #   anti_join(wos_refs_dupes) %>% 
 #   relocate(DI,.before=2)
 # 
 # # again, this time based on AU and TI
 # wos_refs_dupes<-
 #   wos_refs %>% 
 #   get_dupes(AU,TI) %>% 
 #   group_by(AU,TI)  %>% 
 #   filter(row_number()==2) 
 # 
 # 
 # # remove them with an antijoin
 # 
 # wos_refs<-wos_refs %>% 
 #   anti_join(wos_refs_dupes) %>% 
 #   relocate(DI,.before=2)
 # 
 # 
 # 
 # 
 # # again, this time based on AU and TI
 # wos_refs_dupes<-
 #   wos_refs %>% 
 #   get_dupes(TI,BP) %>% 
 #   group_by(TI,BP)  %>% 
 #   filter(row_number()==2) 
 # 
 # # remove them with an antijoin
 # 
 # wos_refs<-wos_refs %>% 
 #   anti_join(wos_refs_dupes) %>% 
 #   relocate(DI,.before=2)
 # 
 # 
 # # again, this time based on AU and TI
 # wos_refs_dupes<-
 #   wos_refs %>% 
 #   get_dupes(TI,AU) %>% 
 #   group_by(TI,AU)  %>% 
 #   filter(row_number()==2) 
 # 
 # 
 # # remove them with an antijoin
 # 
 # wos_refs<-wos_refs %>% 
 #   anti_join(wos_refs_dupes) %>% 
 #   relocate(DI,.before=2)
 # 
 # 
 # 
 # 
 # 
 # 
 # wos_refs<-wos_refs %>%
 #   relocate(refID,DI,pub_cat,jrnl_cat,.before=1)
 # 
 # unique(wos_refs$SO)
 # unique(wos_refs$pub_cat)
 # unique(wos_refs$jrnl_cat)
 # 
 # 
 # # write_csv(wos_refs,"./bibliometrics/data_clean/wos_refs.csv")
 # # 
 # 
 # 
 # # 
 # # refs_ee <- read_csv("./bibliometrics/data_clean/ecoevo_trop.csv")
 # #         
 # # a<-as_tibble(all_data$DI)
 # # b<-as_tibble(refs_ee$DI)
 # # 
 # # setdiff(x, y)
 # # setequal(x, y)
 # # int<-intersect(b,a)
 # # diff<-setdiff(b, a) %>% as_tibble()
 # # names(diff)<-"DI"
 # # 
 # # not_in_all<-semi_join(refs_ee,diff)
 # #                     
 # #   filter(PY!=2023) %>% 
 # #   # mutate(PY=as.double(PY)) %>% 
 # #   mutate(pub_cat="tropical") %>% 
 # #   as_tibble() %>% 
 # #   drop_na(DI) %>% 
 # #   mutate(refID=as.character(refID))
 # # compare_df_cols(wos_refs_all,wos_refs_ee)
 # # 