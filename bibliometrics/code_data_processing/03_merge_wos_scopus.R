
# WOS: Author keywords are included in records of articles from 1991 forward.

# load libraries ----------------------------------------------------------

library(janitor)
library(refsplitr)
library(tidyverse)
library(countrycode)
library(tictoc)
library(opencage)
library(usethis)



# load data ---------------------------------------------------------------

wos<-read_csv("./bibliometrics/data_intermediate/wos_refs.csv") %>% 
  mutate_all(as.character) %>%
  mutate_all(tolower) %>% 
  relocate(source,.before=1) %>% 
  relocate(SO,.before=3) %>% 
  remove_empty(c("cols","rows"))
# WOS Field Tags
# https://images.webofknowledge.com/images/help/WOS/hs_wos_fieldtags.html
# BP = beginning page
# AR = article no.

# how many have no BP?
# wos %>% filter(is.na(BP)) %>% select(BP,AR) %>% arrange(AR)
wos %>% filter(is.na(BP)) %>% select(BP)


scopus<-read_csv("./bibliometrics/data_intermediate/scopus_refs.csv") %>% 
  mutate(source="scopus") %>% 
  relocate(source,.before=1) %>% 
  mutate_all(as.character) %>% 
  mutate_all(tolower) %>% 
  relocate(SO,.before=3) %>% 
  remove_empty(c("cols","rows"))
# 
# compare_df_cols(wos,scopus)
# 
# names_s<-as_tibble(names(scopus))
# names_w<-as_tibble(names(wos))
# scopus_unique_cols<-anti_join(names_s,names_w)
# unique(scopus$`article-number`)
# 
# scopus$AU
# wos$RI


# bid the two datasets ----------------------------------------------------


all_data<-bind_rows(scopus,wos) %>% 
  remove_empty(c("cols","rows")) %>% 
  select(names(wos)) %>% 
  relocate(DI,SO,PY,BP,DE,.before=2)


# split into with and without DOI

# with
all_data_doi<-all_data %>% drop_na(DI)

# without
all_data_no_doi<-all_data %>% filter(is.na(DI))


# remove duplicates -------------------------------------------------------


all_data_doi_slim<-all_data_doi %>% 
  # filter(is.na(DE)) %>% 
  distinct(DI,SO,PY,BP,DE, .keep_all = TRUE) %>% 
  arrange(DI,desc(source)) %>% 
  # filter(DI=="10.1111/btp.13050"|DI=="10.1086/283903") %>% 
# https://stackoverflow.com/questions/53975397/group-collapse-rows-without-summarising-to-fill-in-nas
# https://stackoverflow.com/questions/54924985/r-collapse-and-auto-fill-blanks-in-rows
# https://stackoverflow.com/questions/54924985/r-collapse-and-auto-fill-blanks-in-rows
  # arrange(DI,source,SO,PY,BP) %>%
  # group_by(DI,SO,PY,BP) %>%
  group_by(DI,SO,PY,DE) %>%
  summarise_all(list(~ if(all(is.na(.))) NA else .[!is.na(.)]))


all_data_doi_slim <- all_data_doi_slim %>%  
  group_by(DI,SO,PY,BP) %>% 
  arrange(desc(source)) %>% 
  slice(1)

# find duplicates
dupes_all_data_doi<- all_data_doi_slim %>% 
  relocate(BP,.before = DE) %>% 
  get_dupes(DI) 

# exclude them from the original dupes_all_data_doi
all_data_doi_slim<-anti_join(all_data_doi_slim,dupes_all_data_doi)

#trim the dupes down
dupes_all_data_doi<- dupes_all_data_doi %>%
  # drop_na(DI) %>% 
  group_by(DI) %>% 
  arrange(DI,desc(source)) %>% 
  slice(1)

# then add them back in
all_data_doi_slim<-bind_rows(all_data_doi_slim, dupes_all_data_doi)

# Any duplicates left?
all_data_doi_slim %>% get_dupes(DI)

# what database from?
all_data_doi_slim %>%group_by(source) %>% tally()

# no keywords
all_data_doi_slim %>% filter(is.na(DE)) %>% 
  group_by(source,decade = (round(as.numeric(PY)/10)*10)) %>% 
  tally()



### NO DOI
all_data_no_doi %>% 
  group_by(source) %>% 
  filter(is.na(DE)) %>% 
  tally()


all_data_no_doi_slim<-all_data_no_doi %>% 
  distinct(SO,PY,BP,DE, .keep_all = TRUE) %>% 
  remove_empty(c("cols","rows")) %>% 
  arrange(SO,PY,BP,DE,VL) %>% 
  # https://stackoverflow.com/questions/53975397/group-collapse-rows-without-summarising-to-fill-in-nas
  # https://stackoverflow.com/questions/54924985/r-collapse-and-auto-fill-blanks-in-rows
  # https://stackoverflow.com/questions/54924985/r-collapse-and-auto-fill-blanks-in-rows
  group_by(SO,PY,BP,DE,VL) %>%
  summarise_all(list(~ if(all(is.na(.))) NA else .[!is.na(.)])) 

all_data_no_doi_slim <- all_data_no_doi_slim %>%  
  group_by(SO,PY,BP,DE,VL) %>% 
  arrange(desc(source)) %>% 
  slice(1)

# find duplicates
dupes_all_data_no_doi<- all_data_no_doi_slim %>% 
  # relocate(BP,.before = DE) %>% 
  get_dupes(TI) 

# exclude them from the original dupes_all_data_doi
all_data_no_doi_slim<-anti_join(all_data_no_doi_slim,dupes_all_data_no_doi)

#trim the dupes down
dupes_all_data_no_doi<- dupes_all_data_no_doi %>%
  # drop_na(DI) %>% 
  group_by(TI) %>% 
  arrange(TI,desc(source)) %>% 
  slice(1)

# then add them back in
all_data_no_doi_slim<-bind_rows(all_data_no_doi_slim, dupes_all_data_no_doi)

# Any duplicates left?
all_data_no_doi_slim %>% get_dupes(TI)

# what database from?
all_data_no_doi_slim %>%group_by(source) %>% tally()

# no keywords
all_data_no_doi_slim %>% filter(is.na(DE)) %>% 
  group_by(source,decade = (round(as.numeric(PY)/10)*10)) %>% 
  tally()


all_data_slim<-bind_rows(all_data_no_doi_slim,all_data_doi_slim)
all_data_slim %>% get_dupes(SO,PY,BP,TI)

all_data_slim<-ungroup(all_data_slim)


all_data_slim<-all_data_slim %>% 
  distinct(SO,PY,BP,TI, .keep_all = TRUE) %>% 
  remove_empty(c("cols","rows")) %>% 
  arrange(SO,PY,BP,TI) %>% 
  # https://stackoverflow.com/questions/53975397/group-collapse-rows-without-summarising-to-fill-in-nas
  # https://stackoverflow.com/questions/54924985/r-collapse-and-auto-fill-blanks-in-rows
  # https://stackoverflow.com/questions/54924985/r-collapse-and-auto-fill-blanks-in-rows
  group_by(SO,PY,BP,TI) %>%
  summarise_all(list(~ if(all(is.na(.))) NA else .[!is.na(.)])) 


all_data_slim <- all_data_slim %>%  
  group_by(SO,PY,TI) %>% 
  arrange(desc(source)) %>% 
  slice(1)

# find duplicates
dupes_all_data_slim<- all_data_slim %>% 
  relocate(BP,.before = DE) %>% 
  get_dupes(SO,PY,TI) 

# exclude them from the original dupes_all_data_doi
all_data_slim<-anti_join(all_data_slim,dupes_all_data_slim)

#trim the dupes down
dupes_all_data_slim<- dupes_all_data_slim %>%
  group_by(SO,PY,TI) %>% 
  arrange(SO,PY,TI,desc(source)) %>% 
  slice(1)

# then add them back in
all_data_slim<-bind_rows(all_data_slim, dupes_all_data_slim)




unique(all_data_slim$DT)

all_data_slim<-all_data_slim %>% 
  filter(DT!="editorial material") %>% 
  filter(DT!="article; early access") %>% 
  filter(DT!="review; early access") %>% 
  filter(DT!="item about an individual") %>% 
  filter(DT!="biographical-item") %>% 
  filter(DT!="correction") %>% 
  filter(DT!="article; proceedings paper") %>% 
  filter(DT!="letter") %>% 
  filter(DT!="ed") %>% 
  filter(DT!="article; proceedings paper") %>% 
  filter(DT!="tb") %>% 
  filter(DT!="undefined") %>% 
  filter(DT!="bk") %>% 
  filter(DT!="er") %>% 
  filter(DT!="cp") %>% 
  filter(DT!="le") %>% 
  filter(DT!="sh") %>% 
  mutate(DT = case_when(
    DT ==  "article" ~ "ar",
    DT ==  "review" ~ "re",
    DT == 'note'~ "no",
    DT == 'article; data paper'~ "ar",
    TRUE ~ as.character(DT))) 

unique(all_data_slim$DT)

# Filter out problematic titles
all_data_slim<-all_data_slim %>%
  filter(TI!="biological flora of british isles") %>% 
  filter(TI!="editorial") %>%
  filter(TI!="erratum") %>% 
  filter(TI!="corrigendum") %>% 
  filter(TI!="instruction to authors") %>% 
  filter(TI!="journal of ecology news") %>% 
  filter(TI!="plant ecology") %>% 
  filter(TI!="recent publications of interest") %>% 
  filter(TI!="animal ecology") %>% 
  filter(TI!="comments on evolutionary literature") %>% 
  filter(TI!="editorial: current status of the journal of evolutionary biology") %>% 
  filter(TI!="european society for evolutionary biology 2nd congress") %>% 
  filter(TI!="genetic terminology") %>% 
  filter(TI!="laboratory and field ecology") %>% 
  filter(TI!="european society for evolutionary biology 2nd congress roma, italy – september 3–7, 1989") %>% 
  filter(TI!="erratum: suppression of sex-ratio meiotic drive and the maintenance of y-chromosome polymorphism in drosophila (evolution (february 1999) 53 (164-174))") %>% 
  filter(TI!="editor's note") %>% 
  filter(TI!="editorial: current status of the journal of evolutionary biology") 




# extract any that are missing DOI  ---------------------------------------


# Author Keywords
# Author keywords are included in WOS records of articles from 1991 forward.
# https://images.webofknowledge.com/images/help/WOS/hp_full_record.html
DOI_to_check<-all_data_slim %>% 
  filter(is.na(DE)) %>% 
  filter(as.numeric(PY)>1990) %>% 
  # select(DI,SO,PY) %>% 
  select(DI) %>% 
  drop_na(DI)
DOI_to_check<-DOI_to_check %>% 
  mutate(SO = case_when(
    SO ==  "bitr"~"biotropica",
    SO ==  "evol"~"evolution",
    SO ==  "jae"~"journal of animal ecology",
    SO ==  "ecology"~"ecology",
    SO ==  "jeb"~"journal of environmental biology",
    SO ==  "jecol"~"journal of ecology",  
    SO ==  "jte"~"journal of tropical ecology",
    SO ==  "trec"~"tropical ecology",
    SO ==  "amnat"~"american naturalist",
    SO ==  "rbt"~"revista de biologia tropical",
    SO ==  "tcs"~"tropical conservation science",
    TRUE ~ as.character(SO))) %>% 
  # mutate(string=paste("(DO=",DI," AND SO=", SO," AND PY=",PY,") OR ",sep="" )) 
  mutate(string=paste("(DO=",DI,") OR ",sep="" )) 

write_csv(DOI_to_check,"./bibliometrics/data_intermediate/doi_check_in_wos3.csv")













# save the final dataset --------------------------------------------------



write_csv(all_data_slim,"./bibliometrics/data_clean/complete_data.csv")













# 
# 
# # again, this time based on Titles
# dupes<-all_data_slim %>% 
#   get_dupes(TI) %>% 
#   arrange(TI,desc(source))
# 
# %>% 
#   group_by(TI)  %>% 
#   filter(row_number()==2) 
# 
# 
# # remove them with an antijoin
# 
# complete_data<-complete_data %>% 
#   anti_join(complete_data_dupes) %>% 
#   relocate(DI,.before=2)
# 
# 
# # Filter out problematic titles
# complete_data<-complete_data %>%
#   filter(TI!="biological flora of british isles") %>% 
#   filter(TI!="editorial") %>% 
#   filter(TI!="instruction to authors") %>% 
#   filter(TI!="journal of ecology news") %>% 
#   filter(TI!="plant ecology") %>% 
#   filter(TI!="recent publications of interest") %>% 
#   filter(TI!="animal ecology") %>% 
#   filter(TI!="comments on evolutionary literature") %>% 
#   filter(TI!="editorial: current status of the journal of evolutionary biology") %>% 
#   filter(TI!="european society for evolutionary biology 2nd congress") %>% 
#   filter(TI!="genetic terminology") %>% 
#   filter(TI!="laboratory and field ecology") 
# 
# 
# # Filter out problematic authors
# 
# to_delete<-complete_data %>% 
#   filter(AU=="[anonymous]"| 
#            AU=="kareiva [ed], p"|
#            str_detect(TI, "editor")|
#            str_detect(TI, "commentary on"))
# 
# 
# # remove them with an antijoin
# 
# complete_data<-complete_data %>% 
#   anti_join(to_delete)
# 
# 
# # restrict to article,review, note   
# 
# complete_data<-complete_data %>% 
#   filter(TI!="editorial") %>% 
#   filter(TI!="instruction to authors") %>% 
#   filter(TI!="journal of ecology news") %>% 
#   filter(TI!="guest editor's note") %>% 
#   filter(TI!="plant ecology") %>% 
#   filter(TI!="recent publications of interest") %>% 
#   filter(TI!="animal ecology") %>% 
#   filter(TI!="comments on evolutionary literature") %>% 
#   filter(TI!="editorial: current status of the journal of evolutionary biology") %>% 
#   filter(TI!="european society for evolutionary biology 2nd congress") %>% 
#   filter(TI!="genetic terminology") %>% 
#   filter(TI!="laboratory and field ecology") 
# 
# 



write_csv(complete_data,"./bibliometrics/data_clean/complete_data.csv")

# bind up all the files into a single one









# 
# 
# 
# 
# 
# 
# 
# 
# 
# wos_long<-wos %>% 
#   replace_na(list(DI = "no_doi")) %>% 
#   relocate(refID,DI,SO,PY,BP,.before=1) %>% 
#   pivot_longer(source:AR,
#                names_to = "code",
#                values_to = "value_wos")
# 
# names(scopus_long)
# scopus_long<-scopus %>% 
#   replace_na(list(DI = "no_doi")) %>% 
#   relocate(refID,DI,SO,PY,BP,.before=1) %>% 
#   pivot_longer(source:AU,
#                names_to = "code",
#                values_to = "value_scopus")
# 
# all_long<-full_join(scopus_long,wos_long,by=c("DI","SO","PY","BP")) %>% 
#   mutate(refID.x=if_else(is.na(refID.x),refID.y,refID.x)) %>% 
#   select(-refID.y) %>% 
#   rename("refID"="refID.x") %>% 
#   mutate(code.x=if_else(is.na(code.x),code.y,code.x)) %>% 
#   select(-code.y) %>% 
#   rename("code"="code.x") %>% 
#   mutate(value_scopus=if_else(is.na(value_scopus),value_wos,value_scopus)) %>% 
#   select(-value_wos) %>% 
#   rename("value"="value_scopus")
# 
# all_long %>% 
#   group_by(DI,SO,PY,BP,code) %>% 
#   slice(1)
#   pivot_wider(names_from = "code", values_from = value)
# 
# 
# all_long_wos<-left_join(wos_long,scopus_long) %>% 
#   mutate(value=if_else(is.na(value_wos),value_scopus,value_wos)) %>% 
#   select(-value_scopus,-value_wos) %>% 
#   select(-value_scopus,-value_wos) %>% 
#   pivot_wider(names_from = "code", values_from = "value")
# 
# 
# 
# all_long_scopus<-left_join(scopus_long,wos_long) %>% 
#   mutate(value=if_else(is.na(value_scopus),value_wos,value_scopus)) %>% 
#   select(-value_scopus,-value_wos) %>% 
#   pivot_wider(names_from = "code", values_from = "value")
# 
# 
# # all_data<-bind_rows(wos,scopus)
# # colnames(all_data)
# 
# 
# scopus_no_DE<-scopus %>% filter(is.na(DE))
# 
# wos_no_DE<-wos %>% filter(is.na(DE))
# 
# # join them up
# all_data<-full_join(wos,scopus)



# dupe DOI
dupe_doi<- all_data_slim %>% 
  # drop_na(DI) %>% 
  get_dupes(DI,SO,PY,BP)
# unique DOI
unique_doi<-anti_join(all_data,dupe_doi)
  
(nrow(unique_doi)+nrow(dupe_doi))==nrow(all_data)

# unique DOI with NO keywords
DOI_no_kw<-unique_doi %>% drop_na(DE) 
# unique DOI with keywords
DOI_with_kw<-anti_join(unique_doi,DOI_no_kw)



# dupe_doi that are NA in DOI
dupe_doi_naDOI<-dupe_doi %>% filter(is.na(DI))



# dupe_doi with a DOI
dupe_doi_withDOI<-dupe_doi %>% filter(!is.na(DI)) %>% 
# keep one of the duplicate
  get_dupes(DI,SO,PY) %>%
  arrange(DI,SO,PY) %>% 
  group_by(DI,SO,PY) %>% 
  slice(1) %>% 
  filter(is.na(DI))


# dupe_doi without DE
dupe_doi_NA<-dupe_doi_NA %>% filter(is.na(DE))
# keep one of the duplicates

# dupe doi not NA
dupe_doi_NA<-dupe_doi %>% filter(!is.na(DI))
# dupe doi not NA with DE
dupe_doi_NA<-dupe_doi_NA %>% filter(!is.na(DE))
# keep one of the duplicates

# dupe doi not NA without DE
dupe_doi_NA<-dupe_doi_NA %>% filter(is.na(DE))
  # keep one of the duplicates


dupes_with_DOI_with_DE<- all_data_with_DOI %>% 
  get_dupes(DI,SO,PY) %>% 
  filter(is.na(DE)==FALSE)

dupes_with_DOI_NO_DE<- all_data_with_DOI %>% 
  get_dupes(DI,SO,PY) %>% 
  filter(is.na(DE))



# find duplicates
dupes_1<- all_data %>% 
  get_dupes(DI,SO,PY) %>%
  drop_na(DI) %>% 
  arrange(DI,SO,PY) %>% 
  group_by(DI,SO,PY) %>% 
  slice(1) %>% 
  filter(is.na(DI))

dupes_2<- all_data %>% 
  get_dupes(DI,SO,PY) %>% 
  arrange(DI,SO,PY) %>% 
  group_by(DI,SO,PY) %>% 
  slice(2) %>% 
  filter(is.na(DI))


dupes_3<- all_data %>% 
  get_dupes(DI,SO,PY) %>% 
  arrange(DI,SO,PY) %>% 
  group_by(DI,SO,PY) %>% 
  slice(3) %>% 
  filter(is.na(DI))


# remove duplicate rows ---------------------------------------------------


scopus_di_dupes<-
  scopus %>% get_dupes(DI,SO,PY,BP) %>% arrange(DI,SO,PY,BP)
wos_di_dupes<-
  wos %>% get_dupes(DI,SO,PY,BP) %>% arrange(DI,SO,PY,BP)

wos<-wos %>% group_by(DI,SO,PY,BP) %>% slice(1)
scopus<-scopus %>% group_by(DI,SO,PY,BP) %>% slice(1)



# unique to each dataset --------------------------------------------------


wos_unique<-anti_join(wos,scopus,by=c("DI","SO","PY","BP"))
scopus_unique<-anti_join(scopus,wos,by=c("DI","SO","PY","BP"))

# get the Source, PY, and DOI nos. of all articles. 
# wosDI<-wos %>% select(DI,SO,PY)
# 
# wosDI<-wos %>% select(DI,SO,PY,VL,BP)%>% 
#   mutate_all(as.character)
# 
# scopusDI<-scopus %>% select(DI,SO,PY,VL,BP) %>% 
#   mutate_all(as.character)
# 

# 
wosDI<-wos %>% select(DI)
scopusDI<-scopus %>% select(DI) 
# 
# # the articles with DOI in common in two databases
common_DI<-intersect(scopusDI,wosDI)

# 2x check that they numbers add up
nrow(scopus)
nrow(scopus_unique)
nrow(wos)
nrow(wos_unique)

nrow(wos)==nrow(common_DI)+nrow(wos_unique)
nrow(scopus)==nrow(common_DI)+nrow(scopus_unique)


# common_DIcount<-common_DI %>% count(SO,PY,VL,BP) %>% filter(n>1)
# common_DIcount<-common_DI %>% count(SO,PY)%>% filter(n>1)
# common_DIcount<-common_DI %>% count(SO)%>% filter(n>1)

# remove these from BOTH and process seperately. 
# Could be data are better from one than the other
# Unique to each
# wos_unique<-anti_join(wos,common_DI)
# wos_unique %>% filter(is.na(DI))

# scopus_unique<-anti_join(scopus,common_DI)
# names(scopus_unique)

# in common

# separate those with and without NA in DI
wos_common_noNA<-semi_join(wos,common_DI)%>% 
  mutate(source="wos",.before=1) %>% 
  relocate(source,.before=1) %>% 
  filter(!is.na(DI)) 



# unique(wos_common_noNA$SO)
# 
# wos_common_NA<-semi_join(wos,common_DI)%>% 
#   mutate(source="wos",.before=1) %>% 
#   relocate(source,.before=1) %>% 
#   filter(is.na(DI)) 
# unique(wos_common_NA$SO)

scopus_common_noNA<-semi_join(scopus,common_DI)%>% 
  mutate(source="scopus",.before=1) %>% 
  relocate(source,.before=1) %>% 
  filter(!is.na(DI))




foo<-full_join(scopus_common_noNA,wos_common_noNA,by=c("DI","SO","PY","BP"))
foo$tag=(is.na(foo$DE.x)==TRUE & is.na(foo$DE.y)==TRUE)
foo<-foo %>% filter(tag==FALSE) 
  

# ALL GO TO WOS EXCEPT THOSE WITHOUT DE IN WOS
# DIs to add back to scopus

back_to_scopus<-wos_common_noNA %>% 
  filter(is.na(DE)==TRUE) %>% 
  ungroup() %>% 
  select(DI)  


back_to_wos<-wos_common_noNA %>% 
  filter(is.na(DE)==FALSE) %>% 
  ungroup() %>% 
  select(DI)  

back_to_wos<-semi_join(wos_common_noNA,back_to_wos)


# these are the onl;y scopus to keep
# 
# scopus_common_noNA<-semi_join(scopus_common_noNA,back_to_scopus,by="DI")
# 
# wos_common_noNA
# # some of these have NA keywords for the WOS record...only send the ones with a 
# # is.na==FALSE in DE to WOS

wos_unique<-bind_rows(wos_unique,back_to_wos) 
write_csv(wos_unique,"./bibliometrics/data_clean/wos_unique_refs.csv")

# some of these have NA keywords for the SCOPUS record...only send the ones with a 
# is.na==FALSE in DE to SCOPUS


scopus_common_noNA<-semi_join(scopus_common_noNA,back_to_scopus,by="DI")
scopus_unique<-bind_rows(scopus_unique,scopus_common_noNA)
# scopus_unique<-scopus_unique %>% rename("OI"="orcid")
write_csv(scopus_unique,"./bibliometrics/data_clean/scopus_unique_refs.csv")
# any DI in common between scopus and wos? --------------------------------





names(scopus_unique)
names(wos_unique)
# columns in common
intersect(names(wos_unique), names(scopus_unique))

#in wos but not scopus
setdiff(names(wos_unique), names(scopus_unique))

# in scopus but not wos
setdiff(names(scopus_unique),names(wos_unique))

summary(as.factor(wos_unique$PD))
summary(as.factor(scopus_unique$author_count))



complete_data<-bind_rows(wos_unique,
                         select(scopus_unique, any_of(names(wos_unique))))
# 
# complete_data<-select(wos_unique, any_of(names(scopus_unique))) %>%
#   bind_rows(select(scopus_unique, any_of(names(wos_unique))))
nrow(wos_unique)+nrow(scopus_unique)==nrow(complete_data)
names(complete_data)
# some of these have NA keywords for the SCOPUS record...send those to wos
# foo<-full_join(scopus_common_noNA,wos_common_noNA,by=c("DI","SO","PY","BP"))
# foo$tag=(is.na(foo$DE.x)==TRUE & is.na(foo$DE.y)==TRUE) 
# foo<-foo %>% filter(tag==FALSE) %>% 
#   select(DE.x,DE.y)
# # 
# scopus_common_NA<-semi_join(scopus,common_DI)%>% 
#   mutate(source="scopus",.before=1) %>% 
#   relocate(source,.before=1) %>% 
#   filter(is.na(DI))
# 
# 
# 
# # it looks like the duplications are mostlky rbt. exclude  
# wos_common_dupes1<-wos_common_noNA %>% count(DI,SO,PY,VL,BP,EP) %>% arrange(desc(n))
# wos_common_noNA<-wos_common_noNA %>% distinct(DI,SO,PY,VL,BP,EP, .keep_all=TRUE) 
# 
# wos_common_dupes2<-wos_common_NA %>% count(DI,SO,PY,VL,BP,EP) %>% filter(n>1)
# wos_common_NA<-wos_common_NA %>% distinct(SO,PY,VL,BP,EP,.keep_all=TRUE)
# 
# scopus_common_dupes1<-scopus_common_noNA %>% count(DI,SO,PY,VL,BP,EP) %>% arrange(desc(n))
# scopus_common_noNA<-scopus_common_noNA %>% distinct(DI,SO,PY,VL,BP,EP,.keep_all=TRUE) 
# unique(scopus_common_noNA$SO)
# scopus_common_dupes2<-scopus_common_NA %>% count(DI,SO,PY,VL,BP,EP) %>% filter(n>1)
# scopus_common_NA<-scopus_common_NA %>% distinct(DI,SO,PY,VL,BP,EP,.keep_all=TRUE) 
# 
# foo<-scopus_common_NA %>% filter(VL==51) %>% filter(BP==1)
# 

# 
# wos_common_long<-wos_unique %>% 
#   distinct(DI,SO,PY,VL,BP,.keep_all=TRUE) %>% 
#   relocate(source,DI,.before=1) %>% 
#   pivot_longer(cols = filename:refID,
#                names_to = "code",
#                values_to = "value") %>% 
#   distinct() 
  

# 
# 
# scopus_common_long<-scopus_unique %>% 
#   distinct(DI,SO,PY,VL,BP,.keep_all=TRUE) %>% 
#   relocate(source,DI,.before=1) %>% 
#   pivot_longer(cols = refID:AU,
#                names_to = "code",
#                values_to = "value") %>% 
#   distinct()
# 
# # common_refs_long<-bind_rows(wos_common_long,scopus_common_long) %>% 
# #   select(DI,source,code,value) %>% 
# #   distinct(DI,code,value,.keep_all=TRUE) %>% 
# #   pivot_wider(names_from = c("code"),
# #               values_from = c("source","value"))
# 
# common_refs_long<-full_join(wos_common_long,scopus_common_long) %>% 
#   relocate(source,.after=value) %>% 
#   filter(!is.na(value)) %>% 
#   distinct(DI,code,value,.keep_all=TRUE) %>%
#   arrange(DI,code,value) %>% 
#   mutate(code=paste(code,source,sep="_")) %>% 
#   select(-source) %>% 
#   arrange(DI,code) %>% 
#   pivot_wider(names_from = code,values_from = value) 
# 
# common_refs_long[,which(unlist(lapply(common_refs_long, function(common_refs_long) !all(is.na(common_refs_long)))))]
# common_refs_long$refID_wos
# common_refs_clean<-common_refs_long %>% 
#   unite(filename,filename_wos,filename_scopus,sep="-",na.rm = TRUE,remove=TRUE) %>% 
#   unite(refID,refID_wos,refID_scopus,sep="-",na.rm = TRUE,remove=TRUE) %>% 
#   mutate(AB=if_else(is.na(AB_scopus),AB_wos,AB_scopus)) %>% 
#   relocate(AB,.before=1) %>% 
#   select(-AB_wos,-AB_scopus) %>% 
#   mutate(AF=if_else(is.na(AF_scopus),AF_wos,AF_scopus)) %>% 
#   relocate(AF,.before=1) %>% 
#   select(-AF_wos,-AF_scopus) %>% 
#   mutate(AU=if_else(is.na(AU_scopus),AU_wos,AU_scopus)) %>% 
#   relocate(AU,.before=1) %>% 
#   select(-AU_wos,-AU_scopus) %>% 
#   mutate(C1=if_else(is.na(C1_scopus),C1_wos,C1_scopus)) %>% 
#   relocate(C1,.before=1) %>% 
#   select(-C1_wos,-C1_scopus) %>% 
#   mutate(DE=if_else(is.na(DE_scopus),DE_wos,DE_scopus)) %>% 
#   relocate(DE,.before=1) %>% 
#   select(-DE_wos,-DE_scopus) %>% 
#   mutate(DT=if_else(is.na(DT_scopus),DT_wos,DT_scopus)) %>% 
#   relocate(DT,.before=1) %>% 
#   select(-DT_wos,-DT_scopus) %>% 
#   mutate(EP=if_else(is.na(EP_scopus),EP_wos,EP_scopus)) %>% 
#   relocate(EP,.before=1) %>% 
#   select(-EP_wos,-EP_scopus) %>% 
#   mutate(IS=if_else(is.na(IS_scopus),IS_wos,IS_scopus)) %>% 
#   relocate(IS,.before=1) %>% 
#   select(-IS_wos,-IS_scopus) %>% 
#   mutate(SN=if_else(is.na(SN_scopus),SN_wos,SN_scopus)) %>% 
#   relocate(SN,.before=1) %>% 
#   select(-SN_wos,-SN_scopus) %>% 
#   mutate(TC=if_else(is.na(TC_scopus),TC_wos,TC_scopus)) %>% 
#   relocate(TC,.before=1) %>% 
#   select(-TC_wos,-TC_scopus) %>% 
#   mutate(TI=if_else(is.na(TI_scopus),TI_wos,TI_scopus)) %>% 
#   relocate(TI,.before=1) %>% 
#   select(-TI_wos,-TI_scopus) %>% 
#   mutate(UT=if_else(is.na(UT_scopus),UT_wos,UT_scopus)) %>% 
#   relocate(UT,.before=1) %>% 
#   select(-UT_wos,-UT_scopus) %>% 
#   rename(BP=BP_wos,RP=RP_wos) %>% 
#   relocate(filename,
#            refID,
#            AB,
#            AF,
#            AU,
#            BP,
#            C1,
#            .before=1)
# names<-names(common_refs_clean) 
# names<-gsub("_scopus","",names)
# names<-gsub("_wos","",names)
# names(common_refs_clean)<-names
# 
# 
# 
# # are there duplicated columns?
# duplicated_columns <- duplicated(as.list(common_refs_clean))
# colnames(common_refs_clean[duplicated_columns])
# common_refs_clean<-common_refs_clean[!duplicated_columns]
# 

# 
# 
# 
# common_with_NA<-bind_rows(scopus_common_NA,wos_common_NA) %>% 
#   group_by(SO,PY,VL,BP,EP,AB) %>% 
#   group_by(SO,PY,VL,BP,EP,AB) %>% 
#   mutate(DI=paste("no_doi_",cur_group_id(),sep="")) %>% 
#   relocate(DI,.before=1) %>% 
#   distinct(SO,PY,VL,BP,EP,AB,.keep_all=TRUE)
# 
# common_with_NA
# unique(common_with_NA$source)
# unique(common_NA_long$source)
# 
# common_NA_long<-common_with_NA %>% 
#   distinct(refID,SO,PY,VL,BP,EP,AB,.keep_all=TRUE) %>% 
#   relocate(source,.before=1) %>% 
#   pivot_longer(cols = refID:AR,
#                names_to = "code",
#                values_to = "value") %>% 
#   # relocate(source,.after=value) %>% 
#   # filter(!is.na(value)) %>% 
#   distinct(code,value,.keep_all=TRUE) %>%
#   # unite(code,code,source,sep="_",remove=TRUE) %>% 
#   relocate(DI,.before=1) %>% 
#   arrange(DI,code) %>% 
#   distinct(code,value,.keep_all=TRUE) %>%
#   pivot_wider(names_from = c(code,source),values_from = value) 
# names(common_NA_long)
# # 
# # {common_NA_long} %>%
# #   dplyr::group_by(DI, code) %>%
# #   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
# #   dplyr::filter(n > 1L)
# 
# common_NA_clean<-common_NA_long
# common_NA_clean[,which(unlist(lapply(common_NA_clean, function(common_NA_clean) !all(is.na(common_NA_clean)))))]
# # %>% 
# #   select(sort(names(common_NA_clean)))
# names(common_NA_clean)
# common_NA_clean<-common_NA_clean %>%
#   unite(filename,filename_wos,filename_scopus,sep="-",na.rm = TRUE,remove=TRUE) %>% 
#   unite(refID,refID_wos,refID_scopus,sep="-",na.rm = TRUE,remove=TRUE) %>% 
#   mutate(AB=if_else(is.na(AB_scopus),AB_wos,AB_scopus)) %>% 
#   relocate(AB,.before=1) %>% 
#   select(-AB_wos,-AB_scopus) %>% 
#   mutate(AF=if_else(is.na(AF_scopus),AF_wos,AF_scopus)) %>% 
#   relocate(AF,.before=1) %>% 
#   select(-AF_wos,-AF_scopus) %>% 
#   mutate(AU=if_else(is.na(AU_scopus),AU_wos,AU_scopus)) %>% 
#   relocate(AU,.before=1) %>% 
#   select(-AU_wos,-AU_scopus) %>% 
#   mutate(C1=if_else(is.na(C1_scopus),C1_wos,C1_scopus)) %>% 
#   relocate(C1,.before=1) %>% 
#   select(-C1_wos,-C1_scopus) %>% 
#   mutate(DE=if_else(is.na(DE_scopus),DE_wos,DE_scopus)) %>% 
#   relocate(DE,.before=1) %>% 
#   select(-DE_wos,-DE_scopus) %>% 
#   mutate(DT=if_else(is.na(DT_scopus),DT_wos,DT_scopus)) %>% 
#   relocate(DT,.before=1) %>% 
#   select(-DT_wos,-DT_scopus) %>% 
#   mutate(EP=if_else(is.na(EP_scopus),EP_wos,EP_scopus)) %>% 
#   relocate(EP,.before=1) %>% 
#   select(-EP_wos,-EP_scopus) %>% 
#   mutate(IS=if_else(is.na(IS_scopus),IS_wos,IS_scopus)) %>% 
#   relocate(IS,.before=1) %>% 
#   select(-IS_wos,-IS_scopus) %>% 
#   mutate(SN=if_else(is.na(SN_scopus),SN_wos,SN_scopus)) %>% 
#   relocate(SN,.before=1) %>% 
#   select(-SN_wos,-SN_scopus) %>% 
#   mutate(TC=if_else(is.na(TC_scopus),TC_wos,TC_scopus)) %>% 
#   relocate(TC,.before=1) %>% 
#   select(-TC_wos,-TC_scopus) %>% 
#   mutate(TI=if_else(is.na(TI_scopus),TI_wos,TI_scopus)) %>% 
#   relocate(TI,.before=1) %>% 
#   # select(-Z9_wos,-Z9_scopus) %>% 
#   # mutate(Z9=if_else(is.na(Z9_scopus),Z9_wos,Z9_scopus)) %>% 
#   # select(-Z9_wos,-Z9_scopus) %>%
#   mutate(OI=if_else(is.na(OI_scopus),OI_wos,OI_scopus)) %>% 
#   select(-OI_wos,-OI_scopus) %>% 
#   mutate(author_count=if_else(is.na(author_count_scopus),author_count_wos,author_count_scopus)) %>% 
#   select(-author_count_wos,-author_count_scopus) %>% 
#   mutate(UT=if_else(is.na(UT_scopus),UT_wos,UT_scopus)) %>% 
#   relocate(UT,.before=1) %>% 
#   select(-UT_wos,-UT_scopus) %>% 
#   mutate(CR=if_else(is.na(CR_scopus),CR_wos,CR_scopus)) %>% 
#   relocate(CR,.before=1) %>% 
#   select(-CR_wos,-CR_scopus) %>% 
#   mutate(FN=if_else(is.na(FN_scopus),FN_wos,FN_scopus)) %>% 
#   relocate(FN,.before=1) %>% 
#   select(-FN_wos,-FN_scopus) %>% 
#   mutate(FU=if_else(is.na(FU_scopus),FU_wos,FU_scopus)) %>% 
#   relocate(FU,.before=1) %>% 
#   select(-FU_wos,-FU_scopus) %>% 
#   mutate(GA=if_else(is.na(GA_scopus),GA_wos,GA_scopus)) %>% 
#   relocate(GA,.before=1) %>% 
#   select(-GA_wos,-GA_scopus) %>% 
#   mutate(ID=if_else(is.na(ID_scopus),ID_wos,ID_scopus)) %>% 
#   relocate(ID,.before=1) %>% 
#   select(-ID_wos,-ID_scopus) %>% 
#   mutate(J9=if_else(is.na(J9_scopus),J9_wos,J9_scopus)) %>% 
#   relocate(J9,.before=1) %>% 
#   select(-J9_wos,-J9_scopus) %>% 
#   mutate(Z9=if_else(is.na(Z9_scopus),Z9_wos,Z9_scopus)) %>% 
#   relocate(Z9,.before=1) %>% 
#   select(-Z9_wos,-Z9_scopus) %>%
#   mutate(RI=if_else(is.na(RI_wos),RI_scopus,RI_wos)) %>% 
#   relocate(RI,.before=1) %>% 
#   select(-RI_wos,-RI_scopus) %>%
#   mutate(RP=if_else(is.na(RP_scopus),RP_wos,RP_scopus)) %>% 
#   relocate(RP,.before=1) %>% 
#   select(-RP_wos,-RP_scopus) %>%
#   mutate(SC=if_else(is.na(SC_scopus),SC_wos,SC_scopus)) %>% 
#   relocate(SC,.before=1) %>% 
#   select(-SC_wos,-SC_scopus) %>% 
#   # mutate(UT=if_else(is.na(UT_scopus),UT_wos,UT_scopus)) %>% 
#   # relocate(UT,.before=1) %>% 
#   # select(-UT_wos,-UT_scopus) %>% 
#   # rename(
#   #   # RP=RP_wos,
#   #        BP=BP_scopus) %>% 
#   relocate(filename,
#            refID,
#            AB,
#            AF,
#            AU,
#            # BP,
#            C1,
#            .before=1)
# names<-names(common_NA_clean) 
# names<-gsub("_scopus","",names)
# names<-gsub("_wos","",names)
# names(common_NA_clean)<-names
# 
# # are there duplicated columns?
# duplicated_columns <- duplicated(as.list(common_NA_clean))
# colnames(common_NA_clean[duplicated_columns])
# common_NA_clean<-common_NA_clean[!duplicated_columns]
# 
# 
# common_refs_clean_all<-bind_rows(common_refs_clean,common_NA_clean)
# common_refs_clean_all
# common_refs_clean_all[,which(unlist(lapply(common_refs_clean_all, function(common_refs_clean_all) !all(is.na(common_refs_clean_all)))))]
# 
# 
# # are there duplicated columns?
# duplicated_columns <- duplicated(as.list(common_refs_clean_all))
# colnames(common_refs_clean_all[duplicated_columns])
# common_refs_clean_all<-common_refs_clean_all[!duplicated_columns]




