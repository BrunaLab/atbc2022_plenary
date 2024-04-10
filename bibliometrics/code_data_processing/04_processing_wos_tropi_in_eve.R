# putting together and cleaning up the various WOS archives - all together

# load libraries ----------------------------------------------------------

# library(refsplitr)
library(tidyverse)
library(countrycode)
library(opencage)
library(usethis)


complete_data<-read_csv("./bibliometrics/data_clean/complete_data.csv") %>% 
  mutate(pub_cat=as.factor(pub_cat)) %>% 
  mutate(jrnl_cat=as.factor(jrnl_cat)) %>% 
  mutate(index = row_number()) 


trop_countries<- read_csv("./bibliometrics/code_data_processing/tropical-countries-2023.csv") %>% 
  mutate(country=tolower(country))


# extract tropical field stations?
# data from https://academic.oup.com/bioscience/article/66/2/164/2468674
# txt <- pdf_text("./bibliometrics/code_data_processing/Table S3 - Global database on biological field stations.pdf")

all_refs_count<-complete_data %>%
  group_by(DI,TI) %>% 
  count() %>% 
  filter(n>1) %>% 
  arrange(desc(n))

all_refs<-complete_data %>% 
  distinct(DI,TI,.keep_all = TRUE) 

all_refs_count<-complete_data %>%
  group_by(TI) %>% 
  count() %>% 
  filter(n>1) %>% 
  arrange(desc(n))

complete_data<-complete_data %>% 
  distinct(TI,.keep_all = TRUE) 


summary(complete_data$jrnl_cat)
summary(complete_data$pub_cat) 

complete_data %>% count(pub_cat,jrnl_cat)
 

tropical_terms = c("tropical", 
                   "tropics", 
                   "amazon",
                   "congo",
                   "andean",
                   "andes",
                   "cerrado",
                   "caatinga",
                   "caatinga",
                   "hawaii",
                   "puerto rico",
                   "chamela",
                   "tuxtlas",
                   "nouragues",
                   "kruger",
                   # "cuba",
                   # "malaysia",
                   "amazonia",
                   "bci",
                   "rainforest",
                   "rain forest",
                   "dry forest",
                   "la selva",
                   "atlantic forest",
                   "neotropic",
                   "paleotropic",
                   "manu national",
                   "yasuni",
                   # "costa rica",
                   # "kenya",
                   # "tanzania",
                   # "cameroon",
                   # "nigeria",
                   # "belize",
                   # "venezuela",
                   # "colombia",
                   # "peru",
                   "serengeti",
                   # "ecuador",
                   # "panama",
                   "bornean",
                   "borneo",
                   "barro colorado",
                   "janzen-connell",
                   # "el salvador",
                   # "guatemala",
                   # "honduras",
                   "galapagos",
                   # "nicaragua",
                   "bolivian",
                   # "french guiana",
                   # "guyana",
                   # "paraguay",
                   # "suriname",
                   # "aruba",
                   # "bahamas",
                   "virgin islands",
                   "cayman islands",
                   # "dominica",
                   "western ghats",
                   # "dominican republic",
                   # "grenada",
                   # "guadeloupe",
                   # "haiti",
                   # "jamaica",
                   # "martinique",
                   "netherlands antilles",
                   # "saint lucia",
                   "trinidad",
                   # "angola",
                   # "equatorial guinea",
                   # "gabon",
                   # "zambia",
                   # "burundi",
                   # "comoros",
                   # "ethiopia",
                   # "kenya",
                   # "madagascar",
                   # "malawi",
                   # "mauritius",
                   # Mayotte
                   # "mozambique",
                   "reunion",
                   # "rwanda",
                   # "seychelles",
                   # "uganda",
                   # "benin",
                   # "burkina faso",
                   # "ivory coast",
                   # "ghana",
                   # "philippines",
                   # "singapore",
                   # "thailand",
                   # "vietnam",
                   # "indonesia",
                   "papua new",
                   "indian ", 
                   "australian wet",
                   "african savanna",
                   "african",
                   # "togo",
                   "brunei",
                   "burma", 
                   # "myanmar",
                   # "cambodia",
                   "brazilian",
                   "saint maarten",
                   "heliconia",
                   "myrmecophyte",
                   "guam",
                   "east timor"
                   )

# anguilla
# Antigua and Barbuda
# Barbados
# Liberia"
# Mali"
# Mauritania"
# Niger
# Nigeria
# Saint Helena
# São tomé and Principe
# Senegal
# Sierra Leone
# Montserrat
# Sudan
# Somalia
# djibouti
# eritrea
# Turks and Cacaos Islands
# United States Virgin Islands
# Saint Vincent and the Grenadines

# 
temperate_terms = c("temperate rain forest",
                   "canada",
                   "canadian",
                   " usa ",
                   " france ",
                   "england",
                   "germany",
                   "finland",
                   "sweden",
                   "eastern decid",
                   "norway",
                   "united kingdom",
                   "arctic ",
                   "polar",
                   "boreal",
                   "netherlands",
                   "austria",
                   "belgium",
                   "great basin",
                   "new zealand"
                   )
#   


full_trop_countries<-trop_countries %>% filter(fullyTropical==TRUE) %>% select(country)
full_trop_countries<-pull(full_trop_countries, country)
tropical_terms<-c(full_trop_countries,tropical_terms)

complete_data_clean<-complete_data %>% select(pub_cat,jrnl_cat,PY,AF,AB,TI,SO,DE,refID,DI)  
  complete_data_clean<-complete_data_clean %>% 
   mutate(pub_cat_2 = case_when(
    # (is.na(pub_cat) & str_detect(TI, paste(tropical_terms, collapse = '|'))) ~ "tropical",
    # (is.na(pub_cat) & str_detect(AB, paste(tropical_terms, collapse = '|'))) ~ "tropical",
    # (is.na(pub_cat) & str_detect(DE, paste(tropical_terms, collapse = '|'))) ~ "tropical",
    
    (jrnl_cat=="general" & str_detect(TI, paste(tropical_terms, collapse = '|'))) ~ "tropical",
    (jrnl_cat=="general" & str_detect(AB, paste(tropical_terms, collapse = '|'))) ~ "tropical",
    (jrnl_cat=="general" & str_detect(DE, paste(tropical_terms, collapse = '|'))) ~ "tropical",
    #TODO: NOTE THERE ARE SOME WITH TROPICAL TERMS CLASSIFIED AS GENERAL!!!!
    # str_detect(TI, paste(temperate_terms, collapse = '|'))==TRUE ~ "general",
    # str_detect(AB, paste(temperate_terms, collapse = '|'))==TRUE ~ "general",
    # str_detect(DE, paste(temperate_terms, collapse = '|'))==TRUE ~ "general",
    TRUE ~ as.character(pub_cat)
    )
    )
  
complete_data_clean %>% count(jrnl_cat,pub_cat,pub_cat_2)

tropswitch<-complete_data_clean %>% filter(pub_cat=="tropical" & pub_cat_2=="general")
general<-complete_data_clean %>% filter(is.na(pub_cat) & pub_cat_2=="general")
general<-complete_data_clean %>% filter(pub_cat_2=="general")
nas<-complete_data_clean %>% filter(is.na(pub_cat) & is.na(pub_cat_2))


# read csv of ecoevotrop --------------------------------------------------



trop_ee<-read_csv("./bibliometrics/original_data_summer2022/data_clean/ecoevo_trop.csv") %>% 
  select(DI,SO,PY) %>% 
  mutate_all(tolower) %>% 
  mutate_all(as.character) %>% 
  mutate(trop="trop2") %>% 
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
  mutate(source="ee")
unique(trop_ee$SO)
unique(trop_ee$DI)
unique(complete_data_clean$SO)
unique(complete_data_clean$DI)

complete_data_clean_join<-complete_data_clean %>% 
  mutate_all(as.character) %>% 
  select(DI,TI,PY,SO,pub_cat,pub_cat_2,refID) %>% 
  mutate(source="clean")
na_to_trop<-left_join(trop_ee,complete_data_clean_join,by=c("DI","SO","PY")) %>% 
  filter(is.na(pub_cat_2)) %>% 
  select(-source.x,-source.y) %>% 
  mutate(PY=as.double(PY))


complete_data_clean<-left_join(complete_data_clean,na_to_trop) %>% 
  mutate(pub_cat_2 = case_when(
  trop=="trop2" ~ "tropical",
  TRUE ~ as.character(pub_cat_2)
)) %>% 
  select(-trop) %>% 
  replace_na(list(pub_cat_2="general")) %>%
  relocate(pub_cat_2,.after="pub_cat")


complete_data_clean %>% count(jrnl_cat,pub_cat,pub_cat_2)

complete_data_clean <- complete_data_clean %>%
  group_by(refID,TI) %>% 
  slice_head(n=1)

# %>% 
#   group_by(refID,TI) %>% 
#   tally() %>% arrange(desc(n)) %>% 
#   filter(n>1)

write_csv(complete_data_clean,"./bibliometrics/data_clean/complete_data_clean.csv")
  # library(countrycode)
  # 
  # nocat$code<-countrycode(nocat$AB, origin = 'country.name', destination = 'iso3c')

 
 