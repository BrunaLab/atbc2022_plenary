
# SEE IMPORTANT NOTES ON 265 and 295


library(tidyverse)
# read & standardize: SCOPUS papers ---------------------------------------

scopus_papers <- list.files(path='./bibliometrics/data_raw/scopus/papers',
                            full.names = TRUE)

scopus_papers2 <- data.frame(filename=scopus_papers,
                             PY = scopus_papers, 
                             # SO = scopus_papers, 
                             csv_id = as.character(1:length(scopus_papers))) 
scopus_papers2 <- scopus_papers2 %>% # next line is id for joining
  mutate(PY=str_sub(PY, start=-8L, end=-5L)) %>% 
  # mutate(SO=str_sub(SO, start=-21L, end=-17L)) %>% 
  # mutate(SO = case_when(
  #   SO ==  "/bitr" ~ "bitr",
  #   SO ==  "ology" ~ "ecology",
  #   SO ==  "/evol" ~ "evol",
  #   SO ==  "s/jae" ~ "jae",
  #   # SO ==  "s/jecol" ~ "jecol",
  #   SO ==  "s/jte" ~ "jte",
  #   # SO ==  "rs/amna" ~ "amnat",
  # SO ==  "ors/rbt" ~ "rbt",
  # TRUE ~ as.character(SO))) %>% 
  mutate(filename=str_sub(filename, start=26L)) 
scopus_papers
scopus_papers <- scopus_papers %>% 
  lapply(read_csv,col_types = cols(.default = "c")) %>% # read all the files at once
  bind_rows(.id = "csv_id") %>% # bind all tables into one object, and give id for each
  # left_join(scopus_authors2)
  # lapply(read_csv,col_types = cols(.default = "c")) %>% 
  # bind_rows(.filename = scopus_papers) %>% 
  rename_all(~str_replace_all(.,"prism","")) %>% 
  rename_all(~str_replace_all(.,"dc","")) %>% 
  rename_all(~str_replace_all(.,"\\:","")) %>% 
  mutate(PY=str_sub(coverDate, 1, 4)) %>% 
  rename("SO"="publicationName",
         "AB"="description",
         "DI"="doi",
         "DT"="subtype",
         "VL"="volume",
         "article_type_long"="subtypeDescription",
         "author_count"='author-count.@total',
         "IS"='issueIdentifier',
         "SN"="issn",
         "EI"="eIssn",
         "DE"="authkeywords",
         "PM"="pubmed-id",
         "TC"="citedby-count",
         "TI"="title",
         # "UT"="url",
         "fund_acr"="fund-acr",
         "fund_no"="fund-no",
         "fund_sp"="fund-sponsor",
         "UT"="identifier",
  ) %>% 
  mutate(SO=tolower(SO)) %>% 
  mutate(SO = case_when(
    SO ==  "biotropica" ~ "bitr",
    SO ==  "evolution" ~ "evol",
    SO == 'evolution; international journal of organic evolution'~ "evol",
    SO ==  "journal of applied ecology" ~ "jape",
    SO ==  "journal of ecology" ~ "jecol",
    SO ==  "j. ecol." ~ "jecol",
    SO ==  "american naturalist" ~ "amnat",
    SO ==  "the american naturalist" ~ "amnat",
    SO ==  "journal of tropical ecology" ~ "jte",
    SO ==  "journal of animal ecology" ~ "jae",
    SO ==  "the journal of animal ecology" ~ "jae",
    SO ==  "revista de biología tropical" ~ "rbt",
    SO ==  "revista de biologia tropical" ~ "rbt",
    SO ==  "journal of evolutionary biology" ~ "jeb",
    SO ==  "tropical conservation science" ~ "tcs",
    SO ==  "tropical ecology" ~ "trec",
    TRUE ~ as.character(SO))) %>% 
  distinct() %>% 
  filter(if_any(everything(), is.na)) %>% 
  filter(!is.na(url)) %>% 
  # mutate(pub_number = row_number()) %>% 
  mutate(pageRange=str_replace_all(pageRange,"�","")) %>% 
  mutate(pageRange=str_replace_all(pageRange,"E-","E")) %>% 
  separate(pageRange,c("BP","EP"),remove=FALSE,sep="-",extra="merge") %>% 
  unite(FU,fund_acr,fund_sp,fund_no, sep="-",na.rm=TRUE,remove = TRUE) %>% 
  unite(refID,csv_id,entry_number,sep="-",na.rm=TRUE,remove=FALSE) %>% 
  mutate(DE=str_replace_all(DE,"\\|",";")) %>% 
  select(-"@_fa",
         -"coverDisplayDate",
         -"aggregationType",
         -"author-count.@limit",
         -"openaccess",
         -"freetoread.value.$",
         -"freetoreadLabel.value.$",
         # -"pii",
         -'author-count.$',
         -"coverDate",
         # -"error",
         -"eid",
         -"url",
         -"pageRange",
         -article_type_long,
  ) %>% 
  # mutate_all(tolower) %>% 
  mutate_all(trimws)


scopus_papers<-scopus_papers %>% distinct(DI,TI,.keep_all = TRUE)
rm(scopus_papers2)

write_csv(scopus_papers,"./bibliometrics/data_intermediate/scopus_papers.csv")

# names(scopus_papers)
# unique(scopus_papers$SO)
# names(scopus_papers)
# head(scopus_papers)
# unique(scopus_papers$DE)[999]

# unique(scopus_papers$journal)


scopus_refs<-scopus_papers %>% 
  group_by(SO,PY) %>% 
  tally() 


scopus_papers %>% 
  group_by(SO) %>% 
  tally() 




# read & standardize - SCOPUS affiliations --------------------------------


scopus_affils <- list.files(path='./bibliometrics/data_raw/scopus/affils',
                            full.names = TRUE) %>% 
  lapply(read_csv,col_types = cols(.default = "c")) %>% 
  bind_rows %>% 
  select(-"@_fa") %>% 
  mutate(affilname =str_replace_all(affilname, "\\,", "")) %>% 
  select(-'affiliation-url',
         # -"entry_number"
         ) %>% 
  distinct() %>%
  mutate(C1=paste(affilname,`affiliation-city`,`affiliation-country`,sep=", ")) %>% 
  # mutate(C1=paste("[", C1,"]",sep="")) %>% 
  relocate(C1, .after = afid) %>% 
  rename("university" = "affilname",
         "city" = "affiliation-city",
         "country" = "affiliation-country") %>% 
  # mutate_all(tolower) %>% 
  mutate_all(trimws)



# read & standardize: SCOPUS authors --------------------------------------

scopus_authors <- list.files(path='./bibliometrics/data_raw/scopus/authors',
                             full.names = TRUE)

scopus_authors2 <- data.frame(PY = scopus_authors, 
                              SO = scopus_authors, 
                              csv_id = as.character(1:length(scopus_authors))) 
scopus_authors2 <- scopus_authors2 %>% # next line is id for joining
  mutate(PY=str_sub(PY, start=-8L, end=-5L)) %>% 
  mutate(SO=str_sub(SO, start=-24L, end=-18L)) %>% 
  mutate(SO = case_when(
    SO ==  "rs/bitr" ~ "bitr",
    SO ==  "rs/evol" ~ "evol",
    SO ==  "rs/jane" ~ "jae",  
    SO ==  "s/jecol" ~ "jecol",  
    SO ==  "ors/rbt" ~ "rbt",  
    SO ==  "ors/jte" ~ "jte",
    SO ==  "s/amnat" ~ "amnat",
    SO ==  "ors/jeb" ~ "jeb",
    SO ==  "ors/tcs" ~ "tcs",
    SO ==  "rs/trec" ~ "trec",  
    TRUE ~ as.character(SO)))%>% 
  # mutate_all(tolower) %>% 
  mutate_all(trimws)


scopus_authors2 %>% 
  group_by(SO) %>% 
  tally() 


scopus_authors <- scopus_authors %>% 
  lapply(read_csv,col_types = cols(.default = "c")) %>% # read all the files at once
  bind_rows(.id = "csv_id") %>% # bind all tables into one object, and give id for each
  mutate(author_key = dplyr::row_number()) %>% 
  left_join(scopus_authors2) %>% # join month column created earlier
  select(-"@_fa",
         -"afid.@_fa") %>% 
  rename("author_order"="@seq",
         "afid"="afid.$",
         "first_name"="given-name",
         "author_url"="author-url") %>% 
  relocate(author_key,SO,PY,entry_number, .before="author_order") %>% 
  mutate(first_name = str_replace_all(first_name, "\\. " ,"")) %>%
  mutate(first_name = str_replace_all(first_name, "\\." ,"")) %>%
  mutate(initials = str_replace_all(initials, "\\. " ,"")) %>%
  mutate(initials = str_replace_all(initials, "\\." ,"")) %>%
  mutate(last_init = paste(surname, initials, sep=" ")) %>% 
  mutate(authname = paste(surname, first_name,sep= ", ")) %>% 
  relocate(authid,authname,last_init, .after="author_key") %>% 
  rename("author_count"="author_order") %>% 
  # mutate_all(tolower) %>% 
  mutate_all(trimws) %>% 
  unite(refID,csv_id,entry_number,sep="-",na.rm=TRUE,remove=FALSE)

scopus_authors
write_csv(scopus_authors,"./bibliometrics/data_intermediate/scopus_authors.csv")
rm(scopus_authors2)


names(scopus_affils)
# add the affiliations to authors -----------------------------------------

# scopus_authors_affils<-scopus_authors %>% left_join(scopus_affils)

# Add the name to C1 to make it consistent with WOS
# excludes secondary/current
scopus_authors_affils<-scopus_authors %>% 
  left_join(scopus_affils) %>% 
  select(-author_url) %>% 
  relocate(SO,.before="PY") %>% 
  mutate(first_name = str_replace_all(first_name, "\\. " ,"\\.")) %>% 
  mutate(last_init = paste(surname, initials, sep=" ")) %>% 
  mutate(authname = paste(surname, first_name,sep= ", ")) %>% 
  mutate(C1 = paste("[",authname,"] ", C1 ,sep= "")) %>% 
  distinct(SO,PY,entry_number,authid,afid,.keep_all = TRUE)%>% 
  # mutate_all(tolower) %>% 
  mutate_all(trimws) %>% 
  mutate(C1=gsub("Second Author, ","No Inst Given, ",C1)) 

# unique(scopus_authors_affils$C1)

# are there any remaining?
scopus_authors_affils %>% filter(is.na(author_key)) 

write_csv(scopus_authors_affils,"./bibliometrics/data_intermediate/scopus_authors_affils.csv")

# add the authors and affiliations to papers -------------------------------

names(scopus_authors_affils)
# author_affils in WIDE FORMAT (authors for each paper)
scopus_authors_affils_wide<-scopus_authors_affils %>% 
  select(
    -"university", 
    -"city", 
    -"country", 
    -"last_init", 
    -"authid",
    #       -"C1", 
    - "afid", 
    -"surname" ,
    -"first_name" ,
    #       -"entry_number",
    -author_key,
    -authname,
    #        # -SO,
    #        # -PY,
    -"initials") %>%
  pivot_wider(names_from = author_count, 
              id_cols=refID,
              values_from = C1,
              names_prefix = "C") %>% 
  # BE SURE TO CHECK HOW MANY C COLUMNS AND EDIT BELOW
  unite("C1", C1:C100, remove = TRUE,na.rm=TRUE, sep="./") %>% 
  mutate(C1 = str_replace_all(C1, " \\[na, na, na]" ," \\[missing]"))
# head(scopus_authors_affils_wide$C1,40)
names(scopus_authors_affils_wide)

scopus_article_authors_wide<-scopus_authors_affils %>% 
  distinct() %>% 
  ungroup() %>% 
  select(-"C1", 
         -"university", 
         -"city", 
         -"country", 
         -"last_init", 
         -"authid",
         - "afid", 
         -"surname" ,
         -"first_name" ,
         # -"entry_number",
         # -authname,
         -author_key,
         # -SO,
         # -PY,
         -"initials") %>%
  mutate(orcid=paste(authname,"/",orcid,";",sep="")) %>% 
  pivot_wider(names_from = author_count,
              values_from = c("authname","orcid"),
              id_cols=refID,
              # names_prefix = c("AF","OI")
              ) %>%
  rename_with(~ gsub("authname_", "AF", .x, fixed = TRUE)) %>% 
  rename_with(~ gsub("orcid_", "OI", .x, fixed = TRUE)) %>% 
  # CHECK HOW MANY AUTHOR COLUMNS
  unite("AF", AF1:AF100, remove = TRUE,na.rm=TRUE, sep="; ") %>% 
  unite("OI", OI1:OI100, remove = TRUE,na.rm=TRUE, sep="") 
# mutate(C1 = str_replace_all(C1, " \\[NA, NA, NA]" ," \\[MISSING]"))
# head(scopus_allauthors_wide$AF,40)
names(scopus_article_authors_wide)




scopus_papers_complete <- scopus_papers %>% 
  left_join(scopus_authors_affils_wide) %>% 
  left_join(scopus_article_authors_wide) %>% 
  rename("filename"="csv_id")


# final tweaks to make it possible to use refsplitr on scopus -------------
names(scopus_papers_complete)
scopus_papers_complete<-scopus_papers_complete %>% 
 # mutate(AU=if_else(is.na(AU), AF, AU)) %>% 
  mutate(AU=AF) %>%
  # mutate(refID=gsub("scopus_", "", refID)) %>% 
  mutate(AF=gsub("\\.", "", AF)) %>% 
  mutate(AU=gsub("\\.", "", AU)) %>% 
  mutate(AF=gsub("\\;", "\n", AF)) %>% 
  mutate(AU=gsub("\\;", "\n", AU)) %>% 
  mutate(AF=gsub("\n ", "\n", AF)) %>% 
  mutate(AU=gsub("\n ;", "\n", AU)) %>% 
  # mutate(orcid=paste0(creator, " /", orcid)) %>% # authors clean requires orcid be in wos format "[name]/orcid" 
  relocate(SO,PY,AF,C1,DI,TI,VL,BP,EP,.after="filename")

# remove the last semicolon from every orcid id (last character)
scopus_papers_complete<-scopus_papers_complete %>% 
  mutate(OI=str_sub(OI,end=-2))
# scopus_papers_complete$EM<-NA
  # mutate(refID=as.numeric(refID)*1000) 

scopus_papers_complete<-scopus_papers_complete %>% mutate(jrnl_cat = case_when(
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
    TRUE ~ as.character(NA))) %>% 
  relocate(refID,pub_cat,jrnl_cat,.before=1)


write_csv(scopus_papers_complete,"./bibliometrics/data_intermediate/scopus_refs.csv")
# save csv ----------------------------------------------------------------





























# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #   
# # 
# # scopus_papers_complete %>% 
# #   group_by(refID) %>% 
# #   tally() %>% 
# #   arrange(desc(n)) %>% 
# #   filter(n>1)
# 
# 
# 
# # these are the ones in merged _refs with no authors or database d --------
# 
# 
# 
# refs_noDB<-SD_refs2 %>% 
#   rename_all(~str_replace_all(.,"prism","")) %>% 
#   rename_all(~str_replace_all(.,"dc","")) %>% 
#   rename_all(~str_replace_all(.,"\\:","")) %>% 
#   mutate(PY=str_sub(coverDate, 1, 4)) %>% 
#   rename("SO"="publicationName",
#          "AB"="description",
#          "DI"="doi",
#          "DT"="subtype",
#          "VL"="volume",
#          "article_type_long"="subtypeDescription",
#          "author_count"='author-count.@total',
#          "IS"='issueIdentifier',
#          "SN"="issn",
#          "EI"="eIssn",
#          "DE"="authkeywords",
#          "PM"="pubmed-id",
#          "TC"="citedby-count",
#          "TI"="title",
#          # "UT"="url",
#          "fund_acr"="fund-acr",
#          "fund_no"="fund-no",
#          "fund_sp"="fund-sponsor",
#          "UT"="identifier",
#   ) %>% 
#   mutate(SO=tolower(SO)) %>% 
#   mutate(SO = case_when(
#     SO ==  "biotropica" ~ "bitr",
#     SO ==  "evolution" ~ "evol",
#     SO ==  "journal of applied ecology" ~ "jae",
#     SO ==  "journal of ecology" ~ "jecol",
#     SO ==  "j. ecol." ~ "jecol",
#     SO ==  "american naturalist" ~ "amnat",
#     SO ==  "the american naturalist" ~ "amnat",
#     SO ==  "journal of tropical ecology" ~ "jte",
#     SO ==  "journal of animal ecology" ~ "jae",
#     SO ==  "the journal of animal ecology" ~ "jae",
#     SO ==  "revista de biología tropical" ~ "rbt",
#     SO ==  "revista de biologia tropical" ~ "rbt",
#     TRUE ~ as.character(SO))) %>% 
#   distinct() %>% 
#   filter(if_any(everything(), is.na)) %>% 
#   filter(!is.na(url)) %>% 
#   mutate(pub_number = row_number()) %>% 
#   separate(pageRange,c("BP","EP"),remove=FALSE) %>% 
#   mutate(FU=paste(fund_acr,fund_sp,fund_no, sep="-")) %>% 
#   mutate(refID=paste("scopus",pub_number,sep="_")) %>% 
#   mutate(DE=str_replace_all(DE,"\\|",";")) %>% 
#   select(-"@_fa",
#          -"coverDisplayDate",
#          -"aggregationType",
#          -"author-count.@limit",
#          -"openaccess",
#          -"freetoread.value.$",
#          -"freetoreadLabel.value.$",
#          # -"pii",
#          -'author-count.$',
#          -"coverDate",
#          # -"error",
#          -"eid",
#          -"url",
#          -"pageRange",
#          -article_type_long,
#   ) 
# refs_noDB
# 
# # now the affiliations
# SD_authors2
# 
# 
# affil_noDB <-SD_affils2%>% 
#   mutate(affilname =str_replace_all(affilname, "\\,", "")) %>% 
#   select(-'affiliation-url',-"entry_number") %>% 
#   distinct() %>%
#   mutate(C1=paste(affilname,`affiliation-city`,`affiliation-country`,sep=", ")) %>% 
#   # mutate(C1=paste("[", C1,"]",sep="")) %>% 
#   relocate(C1, .after = afid) %>% 
#   rename("INST" = "affilname",
#          "CITY" = "affiliation-city",
#          "COUNTRY" = "affiliation-country")
# 
# 
# # now the authors
# names(SD_authors2)
# head(SD_authors2)
# 
# 
# au_noDB <- SD_authors2 %>% # next line is id for joining
#   mutate(author_key = dplyr::row_number()) %>% 
#   # left_join(scopus_authors2) %>% # join month column created earlier
#   select(-"@_fa",
#          -"afid.@_fa",
#          - "author-url") %>% 
#   rename("author_order"="@seq",
#          "afid"="afid.$",
#          "first_name"="given-name") %>% 
#   relocate(author_key,entry_number, .before="author_order")
# 
# 
# 
# 
# # scopus_authors_affils<-scopus_authors %>% full_join(scopus_affils)
# # Add the name to C1 to make it consistent with WOS
# # excludes secondary/current
# affil_noDB<-au_noDB %>% 
#   left_join(affil_noDB) %>% 
#   # relocate(SO,.before="PY") %>% 
#   mutate(first_name = str_replace_all(first_name, "\\. " ,"\\.")) %>% 
#   mutate(last_init = paste(surname, initials, sep=" ")) %>% 
#   mutate(authname = paste(surname, first_name,sep= ", ")) %>% 
#   mutate(C1 = paste("[",authname,"] ", C1 ,sep= "")) %>% 
#   rename("author_count"="author_order") %>% 
#   distinct(entry_number,authid,afid,.keep_all = TRUE)
# unique(affil_merged_refs_noDB$C1)
# names(au_merged_refs_noDB)
# # are there any remaining?
# affil_noDB %>% filter(is.na(author_key)) 
# 
# names(au_noDB)
# # add the authors and affiliations to papers -------------------------------
# names(affil_noDB)
# 
# # author_affils in WIDE FORMAT (authors for each paper)
# affil_noDB_wide<-affil_noDB %>% 
#   # select(
#   #   -"INST", 
#   #   -"CITY", 
#   #   -"COUNTRY", 
#   #   -"last_init", 
#   #   -"authid",
#   #   #       -"C1", 
#   #   - "afid", 
#   #   -"surname" ,
#   #   -"first_name" ,
#   #   #       -"entry_number",
#   #   -author_key,
#   #   -authname,
#   #   #        # -SO,
#   #   #        # -PY,
#   #   -"initials") %>%
#   pivot_wider(names_from = author_count, 
#               values_from = C1,
#               names_prefix = "C") %>% 
#   unite("C1", C1:C15, remove = TRUE,na.rm=TRUE, sep="./") %>% 
#   mutate(C1 = str_replace_all(C1, " \\[NA, NA, NA]" ," \\[MISSING]"))
# # head(scopus_authors_affils_wide$C1,40)
# affil_noDB_wide<-affil_noDB_wide %>% select(-'@_fa')
# 
# au_noBD_wide<-affil_noDB %>% 
#   distinct() %>% 
#   ungroup() %>% 
#   # select(-"C1", 
#   #        -"INST", 
#   #        -"CITY", 
#   #        -"COUNTRY", 
#   #        -"last_init", 
#   #        -"authid",
#   #        - "afid", 
#   #        -"surname" ,
#   #        -"first_name" ,
#   #        # -"entry_number",
#   #        # -authname,
#   #        -author_key,
#   #        # -SO,
#   #        # -PY,
#   #        -"initials") %>%
#   pivot_wider(names_from = author_count,
#               values_from = authname,
#               names_prefix = "AF") %>% 
#   unite("AF", AF1:AF15, remove = TRUE,na.rm=TRUE, sep="; ") 
# # mutate(C1 = str_replace_all(C1, " \\[NA, NA, NA]" ," \\[MISSING]"))
# # head(scopus_allauthors_wide$AF,40)
# au_noBD_wide<-au_noBD_wide%>% select(-'@_fa')
# 
# 
# 
# # final tweaks to make it possible to use refsplitr on scopus -------------
# 
# au_noBD_wide<-au_noBD_wide %>% 
#   # mutate(AU=if_else(is.na(AU), AF, AU)) %>% 
#   mutate(AU=AF) %>% 
#   
#   mutate(AF=gsub("\\.", "", AF)) %>% 
#   mutate(AU=gsub("\\.", "", AU)) %>% 
#   mutate(AF=gsub("\\;", "\n", AF)) %>% 
#   mutate(AU=gsub("\\;", "\n", AU)) %>% 
#   mutate(AF=gsub("\n ", "\n", AF)) %>% 
#   mutate(AU=gsub("\n ;", "\n", AU)) %>% 
#   mutate(refID=as.numeric(refID)*1000) 
# 
# write_csv(au_noBD_wide,"./bibliometrics/data_raw/au_noBD_wide.csv")