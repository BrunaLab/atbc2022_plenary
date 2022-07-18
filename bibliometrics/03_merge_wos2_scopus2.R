
# WOS: Author keywords are included in records of articles from 1991 forward.

# load libraries ----------------------------------------------------------

library(janitor)
library(refsplitr)
library(tidyverse)
library(countrycode)
library(tictoc)
library(opencage)
library(usethis)



# any DI in common between scopus and wos? --------------------------------

wos<-read_csv("./bibliometrics/data_clean/wos2_refs.csv") %>% 
  mutate_all(as.character)
  
wos %>% filter(is.na(BP)) %>% select(BP,AR) %>% arrange(AR)
wosDI<-wos2_refs %>% select(DI,SO,PY)


scopus<-read_csv("./bibliometrics/data_clean/scopus2_refs.csv") %>% 
  mutate(source="scopus") %>% 
  relocate(source,.before=1) %>% 
  mutate_all(as.character)

wosDI<-wos %>% select(DI,SO,PY,VL,BP)%>% 
  mutate_all(as.character)

scopusDI<-scopus %>% select(DI,SO,PY,VL,BP) %>% 
  mutate_all(as.character)

common_DI<-intersect(scopusDI,wosDI)
common_DIcount<-common_DI %>% count(DI,SO,PY,VL,BP)%>% filter(n>1)

# remove these from BOTH and process seperately. COPuld be data are better from one than the other
# Unique to each
wos_unique<-anti_join(wos,common_DI)
scopus_unique<-anti_join(scopus,common_DI)
names(scopus_unique)

write_csv(wos_unique,"./bibliometrics/data_clean/wos_unique_refs.csv")
write_csv(scopus_unique,"./bibliometrics/data_clean/scopus_unique_refs.csv")
# in common
# separate those with and without NA in DI
wos_common_noNA<-semi_join(wos,common_DI)%>% 
  mutate(source="wos",.before=1) %>% 
  relocate(source,.before=1) %>% 
  filter(!is.na(DI))

wos_common_NA<-semi_join(wos,common_DI)%>% 
  mutate(source="wos",.before=1) %>% 
  relocate(source,.before=1) %>% 
  filter(is.na(DI)) 

scopus_common_noNA<-semi_join(scopus,common_DI)%>% 
  mutate(source="scopus",.before=1) %>% 
  relocate(source,.before=1) %>% 
  filter(!is.na(DI))

scopus_common_NA<-semi_join(scopus,common_DI)%>% 
  mutate(source="scopus",.before=1) %>% 
  relocate(source,.before=1) %>% 
  filter(is.na(DI))


# it looks like the duplications are mostlky rbt. exclude  
wos_common_dupes1<-wos_common_noNA %>% count(DI,SO,PY,VL,BP,EP) %>% filter(n>1)
wos_common_noNA<-wos_common_noNA %>% distinct(DI,SO,PY,VL,BP,EP, .keep_all=TRUE)

wos_common_dupes2<-wos_common_NA %>% count(DI,SO,PY,VL,BP,EP) %>% filter(n>1)
wos_common_NA<-wos_common_NA %>% distinct(SO,PY,VL,BP,EP,.keep_all=TRUE)

scopus_common_dupes1<-scopus_common_noNA %>% count(DI,SO,PY,VL,BP,EP) %>% filter(n>1)
scopus_common_noNA<-scopus_common_noNA %>% distinct(DI,SO,PY,VL,BP,EP,.keep_all=TRUE) 

scopus_common_dupes2<-scopus_common_NA %>% count(DI,SO,PY,VL,BP,EP) %>% filter(n>1)
scopus_common_NA<-scopus_common_NA %>% distinct(DI,SO,PY,VL,BP,EP,.keep_all=TRUE) 

foo<-scopus_common_NA %>% filter(VL==51) %>% filter(BP==1)



wos_common_long<-wos_common_noNA %>% 
  distinct(DI,SO,PY,VL,BP,.keep_all=TRUE) %>% 
  relocate(source,DI,.before=1) %>% 
  pivot_longer(cols = filename:refID,
               names_to = "code",
               values_to = "value") %>% 
  distinct() 
  



scopus_common_long<-scopus_common_noNA %>% 
  distinct(DI,SO,PY,VL,BP,.keep_all=TRUE) %>% 
  relocate(source,DI,.before=1) %>% 
  pivot_longer(cols = refID:AU,
               names_to = "code",
               values_to = "value") %>% 
  distinct()

# common_refs_long<-bind_rows(wos_common_long,scopus_common_long) %>% 
#   select(DI,source,code,value) %>% 
#   distinct(DI,code,value,.keep_all=TRUE) %>% 
#   pivot_wider(names_from = c("code"),
#               values_from = c("source","value"))

common_refs_long<-full_join(wos_common_long,scopus_common_long) %>% 
  relocate(source,.after=value) %>% 
  filter(!is.na(value)) %>% 
  distinct(DI,code,value,.keep_all=TRUE) %>%
  arrange(DI,code,value) %>% 
  mutate(code=paste(code,source,sep="_")) %>% 
  select(-source) %>% 
  arrange(DI,code) %>% 
  pivot_wider(names_from = code,values_from = value) 

common_refs_long[,which(unlist(lapply(common_refs_long, function(common_refs_long) !all(is.na(common_refs_long)))))]
common_refs_long$refID_wos
common_refs_clean<-common_refs_long %>% 
  unite(filename,filename_wos,filename_scopus,sep="-",na.rm = TRUE,remove=TRUE) %>% 
  unite(refID,refID_wos,refID_scopus,sep="-",na.rm = TRUE,remove=TRUE) %>% 
  mutate(AB=if_else(is.na(AB_scopus),AB_wos,AB_scopus)) %>% 
  relocate(AB,.before=1) %>% 
  select(-AB_wos,-AB_scopus) %>% 
  mutate(AF=if_else(is.na(AF_scopus),AF_wos,AF_scopus)) %>% 
  relocate(AF,.before=1) %>% 
  select(-AF_wos,-AF_scopus) %>% 
  mutate(AU=if_else(is.na(AU_scopus),AU_wos,AU_scopus)) %>% 
  relocate(AU,.before=1) %>% 
  select(-AU_wos,-AU_scopus) %>% 
  mutate(C1=if_else(is.na(C1_scopus),C1_wos,C1_scopus)) %>% 
  relocate(C1,.before=1) %>% 
  select(-C1_wos,-C1_scopus) %>% 
  mutate(DE=if_else(is.na(DE_scopus),DE_wos,DE_scopus)) %>% 
  relocate(DE,.before=1) %>% 
  select(-DE_wos,-DE_scopus) %>% 
  mutate(DT=if_else(is.na(DT_scopus),DT_wos,DT_scopus)) %>% 
  relocate(DT,.before=1) %>% 
  select(-DT_wos,-DT_scopus) %>% 
  mutate(EP=if_else(is.na(EP_scopus),EP_wos,EP_scopus)) %>% 
  relocate(EP,.before=1) %>% 
  select(-EP_wos,-EP_scopus) %>% 
  mutate(IS=if_else(is.na(IS_scopus),IS_wos,IS_scopus)) %>% 
  relocate(IS,.before=1) %>% 
  select(-IS_wos,-IS_scopus) %>% 
  mutate(SN=if_else(is.na(SN_scopus),SN_wos,SN_scopus)) %>% 
  relocate(SN,.before=1) %>% 
  select(-SN_wos,-SN_scopus) %>% 
  mutate(TC=if_else(is.na(TC_scopus),TC_wos,TC_scopus)) %>% 
  relocate(TC,.before=1) %>% 
  select(-TC_wos,-TC_scopus) %>% 
  mutate(TI=if_else(is.na(TI_scopus),TI_wos,TI_scopus)) %>% 
  relocate(TI,.before=1) %>% 
  select(-TI_wos,-TI_scopus) %>% 
  mutate(UT=if_else(is.na(UT_scopus),UT_wos,UT_scopus)) %>% 
  relocate(UT,.before=1) %>% 
  select(-UT_wos,-UT_scopus) %>% 
  rename(BP=BP_wos,RP=RP_wos) %>% 
  relocate(filename,
           refID,
           AB,
           AF,
           AU,
           BP,
           C1,
           .before=1)
names<-names(common_refs_clean) 
names<-gsub("_scopus","",names)
names<-gsub("_wos","",names)
names(common_refs_clean)<-names



# are there duplicated columns?
duplicated_columns <- duplicated(as.list(common_refs_clean))
colnames(common_refs_clean[duplicated_columns])
common_refs_clean<-common_refs_clean[!duplicated_columns]





common_with_NA<-bind_rows(scopus_common_NA,wos_common_NA) %>% 
  group_by(SO,PY,VL,BP,EP,AB) %>% 
  group_by(SO,PY,VL,BP,EP,AB) %>% 
  mutate(DI=paste("no_doi_",cur_group_id(),sep="")) %>% 
  relocate(DI,.before=1) %>% 
  distinct(SO,PY,VL,BP,EP,AB,.keep_all=TRUE)

common_with_NA
unique(common_with_NA$source)
unique(common_NA_long$source)

common_NA_long<-common_with_NA %>% 
  distinct(refID,SO,PY,VL,BP,EP,AB,.keep_all=TRUE) %>% 
  relocate(source,.before=1) %>% 
  pivot_longer(cols = refID:AR,
               names_to = "code",
               values_to = "value") %>% 
  # relocate(source,.after=value) %>% 
  # filter(!is.na(value)) %>% 
  distinct(code,value,.keep_all=TRUE) %>%
  # unite(code,code,source,sep="_",remove=TRUE) %>% 
  relocate(DI,.before=1) %>% 
  arrange(DI,code) %>% 
  distinct(code,value,.keep_all=TRUE) %>%
  pivot_wider(names_from = c(code,source),values_from = value) 
names(common_NA_long)
# 
# {common_NA_long} %>%
#   dplyr::group_by(DI, code) %>%
#   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
#   dplyr::filter(n > 1L)

common_NA_clean<-common_NA_long
common_NA_clean[,which(unlist(lapply(common_NA_clean, function(common_NA_clean) !all(is.na(common_NA_clean)))))]
# %>% 
#   select(sort(names(common_NA_clean)))
names(common_NA_clean)
common_NA_clean<-common_NA_clean %>%
  unite(filename,filename_wos,filename_scopus,sep="-",na.rm = TRUE,remove=TRUE) %>% 
  unite(refID,refID_wos,refID_scopus,sep="-",na.rm = TRUE,remove=TRUE) %>% 
  mutate(AB=if_else(is.na(AB_scopus),AB_wos,AB_scopus)) %>% 
  relocate(AB,.before=1) %>% 
  select(-AB_wos,-AB_scopus) %>% 
  mutate(AF=if_else(is.na(AF_scopus),AF_wos,AF_scopus)) %>% 
  relocate(AF,.before=1) %>% 
  select(-AF_wos,-AF_scopus) %>% 
  mutate(AU=if_else(is.na(AU_scopus),AU_wos,AU_scopus)) %>% 
  relocate(AU,.before=1) %>% 
  select(-AU_wos,-AU_scopus) %>% 
  mutate(C1=if_else(is.na(C1_scopus),C1_wos,C1_scopus)) %>% 
  relocate(C1,.before=1) %>% 
  select(-C1_wos,-C1_scopus) %>% 
  mutate(DE=if_else(is.na(DE_scopus),DE_wos,DE_scopus)) %>% 
  relocate(DE,.before=1) %>% 
  select(-DE_wos,-DE_scopus) %>% 
  mutate(DT=if_else(is.na(DT_scopus),DT_wos,DT_scopus)) %>% 
  relocate(DT,.before=1) %>% 
  select(-DT_wos,-DT_scopus) %>% 
  mutate(EP=if_else(is.na(EP_scopus),EP_wos,EP_scopus)) %>% 
  relocate(EP,.before=1) %>% 
  select(-EP_wos,-EP_scopus) %>% 
  mutate(IS=if_else(is.na(IS_scopus),IS_wos,IS_scopus)) %>% 
  relocate(IS,.before=1) %>% 
  select(-IS_wos,-IS_scopus) %>% 
  mutate(SN=if_else(is.na(SN_scopus),SN_wos,SN_scopus)) %>% 
  relocate(SN,.before=1) %>% 
  select(-SN_wos,-SN_scopus) %>% 
  mutate(TC=if_else(is.na(TC_scopus),TC_wos,TC_scopus)) %>% 
  relocate(TC,.before=1) %>% 
  select(-TC_wos,-TC_scopus) %>% 
  mutate(TI=if_else(is.na(TI_scopus),TI_wos,TI_scopus)) %>% 
  relocate(TI,.before=1) %>% 
  # select(-Z9_wos,-Z9_scopus) %>% 
  # mutate(Z9=if_else(is.na(Z9_scopus),Z9_wos,Z9_scopus)) %>% 
  # select(-Z9_wos,-Z9_scopus) %>%
  mutate(OI=if_else(is.na(OI_scopus),OI_wos,OI_scopus)) %>% 
  select(-OI_wos,-OI_scopus) %>% 
  mutate(author_count=if_else(is.na(author_count_scopus),author_count_wos,author_count_scopus)) %>% 
  select(-author_count_wos,-author_count_scopus) %>% 
  mutate(UT=if_else(is.na(UT_scopus),UT_wos,UT_scopus)) %>% 
  relocate(UT,.before=1) %>% 
  select(-UT_wos,-UT_scopus) %>% 
  mutate(CR=if_else(is.na(CR_scopus),CR_wos,CR_scopus)) %>% 
  relocate(CR,.before=1) %>% 
  select(-CR_wos,-CR_scopus) %>% 
  mutate(FN=if_else(is.na(FN_scopus),FN_wos,FN_scopus)) %>% 
  relocate(FN,.before=1) %>% 
  select(-FN_wos,-FN_scopus) %>% 
  mutate(FU=if_else(is.na(FU_scopus),FU_wos,FU_scopus)) %>% 
  relocate(FU,.before=1) %>% 
  select(-FU_wos,-FU_scopus) %>% 
  mutate(GA=if_else(is.na(GA_scopus),GA_wos,GA_scopus)) %>% 
  relocate(GA,.before=1) %>% 
  select(-GA_wos,-GA_scopus) %>% 
  mutate(ID=if_else(is.na(ID_scopus),ID_wos,ID_scopus)) %>% 
  relocate(ID,.before=1) %>% 
  select(-ID_wos,-ID_scopus) %>% 
  mutate(J9=if_else(is.na(J9_scopus),J9_wos,J9_scopus)) %>% 
  relocate(J9,.before=1) %>% 
  select(-J9_wos,-J9_scopus) %>% 
  mutate(Z9=if_else(is.na(Z9_scopus),Z9_wos,Z9_scopus)) %>% 
  relocate(Z9,.before=1) %>% 
  select(-Z9_wos,-Z9_scopus) %>%
  mutate(RI=if_else(is.na(RI_wos),RI_scopus,RI_wos)) %>% 
  relocate(RI,.before=1) %>% 
  select(-RI_wos,-RI_scopus) %>%
  mutate(RP=if_else(is.na(RP_scopus),RP_wos,RP_scopus)) %>% 
  relocate(RP,.before=1) %>% 
  select(-RP_wos,-RP_scopus) %>%
  mutate(SC=if_else(is.na(SC_scopus),SC_wos,SC_scopus)) %>% 
  relocate(SC,.before=1) %>% 
  select(-SC_wos,-SC_scopus) %>% 
  # mutate(UT=if_else(is.na(UT_scopus),UT_wos,UT_scopus)) %>% 
  # relocate(UT,.before=1) %>% 
  # select(-UT_wos,-UT_scopus) %>% 
  # rename(
  #   # RP=RP_wos,
  #        BP=BP_scopus) %>% 
  relocate(filename,
           refID,
           AB,
           AF,
           AU,
           # BP,
           C1,
           .before=1)
names<-names(common_NA_clean) 
names<-gsub("_scopus","",names)
names<-gsub("_wos","",names)
names(common_NA_clean)<-names

# are there duplicated columns?
duplicated_columns <- duplicated(as.list(common_NA_clean))
colnames(common_NA_clean[duplicated_columns])
common_NA_clean<-common_NA_clean[!duplicated_columns]


common_refs_clean_all<-bind_rows(common_refs_clean,common_NA_clean)
common_refs_clean_all
common_refs_clean_all[,which(unlist(lapply(common_refs_clean_all, function(common_refs_clean_all) !all(is.na(common_refs_clean_all)))))]


# are there duplicated columns?
duplicated_columns <- duplicated(as.list(common_refs_clean_all))
colnames(common_refs_clean_all[duplicated_columns])
common_refs_clean_all<-common_refs_clean_all[!duplicated_columns]

write_csv(common_refs_clean_all,"./bibliometrics/data_clean/common_refs.csv")

# bind up all the files into a single one



# now refsplitr -----------------------------------------------------------


# 1) wos_unique_refs_authors_prelim
#   refs unique to WOS in output format from authors_clean()

head(wos_unique_refs_authors_prelim)



wos_unique_refs<-read_csv("./bibliometrics/data_clean/wos_unique_refs.csv") 
unique(wos_unique_refs$SO)
# wos_unique_refs<-wos_unique_refs %>% 
#   filter(SO!="rbt")
# unique()

wos_unique_refs_authors<-authors_clean(wos_unique_refs)
wos_unique_refs_authors_prelim<-wos_unique_refs_authors$prelim
wos_unique_refs_authors_review<-wos_unique_refs_authors$review

write_rds(wos_unique_refs_authors,"./bibliometrics/data_clean/wos_unique_refs_authors.rds")
write_csv(wos_unique_refs_authors_prelim,"./bibliometrics/data_clean/wos_unique_refs_authors_prelim.csv")
write_csv(wos_unique_refs_authors_review,"./bibliometrics/data_clean/wos_unique_refs_authors_review.csv")

  
  
  
  
  
# 2) common_refs_authors_prelim
#   read in from both scopus AND wos. removed from the scopus and wos files and
#   cleaned up separately, then processed with authors_clean() 

# load "wos_refs_raw": original search after processed by refernces_read() ---
common_refs<-read_csv("./bibliometrics/data_clean/common_refs.csv") 
names(common_refs2)
names(common_refs2)
head(common_refs_authors_prelim)
names(common_refs_authors_prelim)
names(common_refs)


# %>% 
#   filter(refID!=47764) 
# 
# Remove Duplicate Column Names
duplicated_names <- duplicated(colnames(common_refs))
common_refs<-common_refs[!duplicated_names]

common_refs<-common_refs %>% 
  mutate(AF=if_else(is.na(AF), AU,AF)) %>% 
  mutate(AU=if_else(is.na(AU), AF,AU)) %>% 
  filter(!is.na(AF)) %>% 
  filter(!is.na(AU)) 

# common_refs<-common_refs %>% filter(SO!="rbt")
# 
# names(common_refs)
# 
#   select(filename=filename_wos,AB,AF,AU,BP,C1,CR,DE,DI,
#          EM,EP,FN,FU,PD_wos,PG_wos,PT_wos,PU_wos,PY_wos,RI_wos,OI_wos,PM_wos,
#          RP_wos,SC_wos,SN,TC,TI,UT,VL_wos,WC_wos,Z9_wos,refID) %>% 
# 
# # 
# %>% 
#   mutate(EM='---@---') %>% 
#   mutate(RP=C1) %>% 
#   mutate(RI=C1) %>% 

# common_refs2<-common_refs %>% slice(1:8000)
 common_refs2<-common_refs %>% slice(8000:12000)
# common_refs2<-common_refs %>% slice(12000:12000)
common_refs_authors<-authors_clean(common_refs)
common_refs_authors_prelim<-common_refs_authors$prelim
common_refs_authors_review<-common_refs_authors$review

write_rds(common_refs_authors,"./bibliometrics/data_clean/common_refs_authors.rds")
write_csv(common_refs_authors_prelim,"./bibliometrics/data_clean/common_refs_authors_prelim.csv")
write_csv(common_refs_authors_review,"./bibliometrics/data_clean/common_refs_authors_review.csv")



#  3) `scopus_unique_refs`
#   Unique to scopus. 
#   Need to put in format that looks like output from authors_clean()
head(scopus_unique_refs)
names(scopus_unique_refs)
names(scopus2_authors_affils)

scopus2_authors_affils<-read_csv("./bibliometrics/data_clean/scopus2_authors_affils.csv") %>% 
  unite("address",university:country, sep = ", ", remove=FALSE, na.rm=TRUE) %>% 
  relocate(refID,.before=1) %>% 
  mutate_all(as.character)

scopus2_unique<-read_csv("./bibliometrics/data_clean/scopus_unique_refs.csv") %>% 
  mutate_all(as.character)

# names(scopus_unique_refs)
# scopus_unique_refs$refID
# scopus_unique$refID

scopus2_authors_prelim<-inner_join(scopus2_authors_affils,
                                   scopus_unique,
                                   by=c("SO","PY","refID")) %>% 
  arrange(refID,author_count.x) %>% 
  relocate(author_count.x,author_count.x,.after=1) %>% 
  rename(author_count=author_count.x) %>% 
  group_by(authid) %>% 
  fill(c(city,country), .direction=c("down")) %>% 
  ungroup()


scopus2_authors_affils
scopus2_authors_prelim %>% count(country) %>% arrange(desc(n))
scopus2_authors_prelim %>% select(author_count,author_count.y) %>% arrange(desc(n))
# still need to find any with missing affils and fill in with autid from author_affils
scopus2_authors_prelim %>% count(SO) %>% arrange(desc(n))
write_csv(scopus2_authors_prelim,"./bibliometrics/data_clean/scopus2_authors_prelim.csv")
