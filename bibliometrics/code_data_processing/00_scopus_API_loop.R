library(rscopus)
library(tidyverse)


# NOTE API SEARCH DOES NOT GIVE "INDEXED KEYWORDS" (only author ketywords) so 
# will have to get those with NA in ketywords and search for them on WOS

scopus_api_search <- function(issn, date_range, jrnl) {
  year <- seq_along(date_range)
  for (j in year) {
    
    c <- " AND PUBYEAR = "
    
  
    # query_string <- paste0("EXACTSRCTITLE(Biotropica) ", 
  #                 # "AND DOCTYPE(ar)")
  #                 "AND DOCTYPE(ar) ",
  #                 "AND PUBYEAR = 1999")
  #   
    # query_string <-paste0(issn, c, date_range[j],"AND KEY(TROPICAL)",sep = "")
    query_string <-paste0(issn, c, date_range[j],sep = "")
    # query_string<-"DOI(10.1002/ecy.3594)"
    scopus_data <- rscopus::scopus_search(query_string,
      view = "COMPLETE",
      # max_count = 5,
      api_key = "1c46380453a72efcc8b15d447371e646")
    
    
    # query_string <- paste0("eid(2-s2.0-0024266051)")
    
    scopus_data_raw <- gen_entries_to_df(scopus_data$entries)
    scopus_papers <- scopus_data_raw$df
    # jae_papers$`prism:publicationName`
    papers <- paste("./bibliometrics/data_raw/scopus/papers/", jrnl,"_papers_",date_range[j], ".csv", sep = "")
    write_csv(scopus_papers, papers)

    scopus_affiliations <- scopus_data_raw$affiliation
    affils <- paste("./bibliometrics/data_raw/scopus/affils/", jrnl,"_affils_",date_range[j], ".csv", sep = "")
    write_csv(scopus_affiliations, affils)

    scopus_authors <- scopus_data_raw$author
    authors <- paste("./bibliometrics/data_raw/scopus/authors/", jrnl,"_authors_",date_range[j], ".csv", sep = "")
    write_csv(scopus_authors, authors)
    
  }
}


# OECOLOGIA (just to test lkw search)-----------------------------------------

# ISSN:0029-8549
# E-ISSN:1432-1939
# Scopus: 1968 on
# date_range <- seq(2001,2003)
# issn<-'ISSN(0029-8549)'
# jrnl<-'oecol'
# scopus_api_search(issn,date_range, jrnl)


# JANIMECOL ---------------------------------------------------------------

# ISSN:0021-8790 Searched 1982-2022
# E-ISSN:1365-2656
# Scopus: 1982 on
date_range <- seq(1982,2022)
issn<-'ISSN(0021-8790)'
jrnl<-'jane'
scopus_api_search(issn,date_range, jrnl)


# J Evol Biol -------------------------------------------------------------

# ISSN: 1010-061X
# EISSN: 1420-9101 searched 1988-2022
date_range <- seq(1988,2022)
issn<-'ISSN(1420-9101)'
jrnl<-'jeb'
scopus_api_search(issn,date_range,jrnl)


# Evolution ---------------------------------------------------------------

# ISSN 15585646
# ISSN 00143820 search this one 1971-1978, 1982-2022
date_range <- seq(1982,2022)
# date_range <- seq(1971,1978)
issn<-'ISSN(00143820)'
jrnl<-'evol'
scopus_api_search(issn,date_range, jrnl)


# J Trop Ecol -------------------------------------------------------------

# ISSN:0266-4674 searched 1985-2022
# E-ISSN:1469-7831
date_range <- seq(1985,2022)
issn<-'ISSN(0266-4674)'
jrnl<-'jte'
scopus_api_search(issn,date_range, jrnl)

# 10.1002/ecy.3594 indexed keywords not ok
# 10.1002/ecy.3713 authoir keywords ok
# ECOLOGY -----------------------------------------------------------------

# ISSN: 00129658 searched 1982-2022
date_range <- seq(2022,2022)
issn<-'ISSN(00129658)'
jrnl<-'ecology'
scopus_api_search(issn,date_range, jrnl)


# Journal of Ecology ------------------------------------------------------

# 00220477 searched 1982-2022
date_range <- seq(1982,2022)
issn<-'ISSN(00220477)'
jrnl<-'jecol'
scopus_api_search(issn,date_range, jrnl)


# Biotropica --------------------------------------------------------------

# 00063606 searched 1988-2022
date_range <- seq(1988,2022)
issn<-'ISSN(00063606)'
jrnl<-'bitr'
scopus_api_search(issn,date_range, jrnl)


# Trop Cons Science---------------------------------------------------------

# ISSN:1940-0829 searched 2010-2022

date_range <- seq(2010,2022)
# fewer available from 1969-1995
issn<-'ISSN(19400829)'
jrnl<-'tcs'
scopus_api_search(issn,date_range, jrnl)


# Tropical Ecology --------------------------------------------------------


# 00030147  searched 1985-2022
# ISSN:0564-3295
date_range <- seq(1994,2008)
issn<-'ISSN(0564-3295)'
jrnl<-'trec'
scopus_api_search(issn,date_range, jrnl)


# AmNat  ------------------------------------------------------------------

# 00030147  searched 1982-2022
# EISSN:1537-5323
date_range <- seq(1982,2022)
issn<-'ISSN(00030147)'
jrnl<-'amnat'
scopus_api_search(issn,date_range, jrnl)


# Rev Biol Trop -----------------------------------------------------------

# 00347744

date_range <- seq(1996,2022)
# fewer available from 1969-1995
issn<-'ISSN(00347744)'
jrnl<-'rbt'
scopus_api_search(issn,date_range, jrnl)



# a <- 'ISSN(The American Naturalist)'
# a <- "EXACTSRCTITLE(American Naturalist)"
# a <- "EXACTSRCTITLE(Ecology)"
# a <- "EXACTSRCTITLE(Journal of Ecology)"
# a <- "EXACTSRCTITLE(Journal of Animal Ecology)"
# a <- "EXACTSRCTITLE(ecologyution)"
# b <- " AND DOCTYPE(ar OR no OR re OR dp)"
# b <- " AND (DOCTYPE(ar) OR  DOCTYPE(no) OR DOCTYPE(re) OR  DOCTYPE(dp))"
# b <- " AND DOCTYPE(ar OR re)"



# api1: 38c1ea28aed25f40f11034d20557ccde
# api2: 8e204bc721cb41c0251c8846351342b0
# api3: c253aa47dd592442b1d5ad7ded7b0514
# api4: 8d8d7b628fae6e1a5a04db969b0bca93
# abc6828fe082f034bb608b168f0340e6 = jeb2023
# 	2670632e6722467324869014c4d053ce feb2 2023
# 1c46380453a72efcc8b15d447371e646 



























# api loop for keywords by DOI --------------------------------------------

library(rscopus)
library(tidyverse)
DOI<-read.csv("./bibliometrics/data_raw/scopus_no_kw.csv")
DOI<-as.vector(DOI$DI)
kw_list<-list()
for (j in 1:length(DOI)) {
  a <- "DOI("
  b<-")"
  #   DOI<-"10.1111/1365-2745.12194"
  # j=1   j=2     j=3
  
  query_string <-paste0(a, DOI[j],b,sep = "")
  # 
  scopus_data <- rscopus::scopus_search(query_string,
                                        view = "COMPLETE",
                                        # max_count = 5,
                                        api_key = "eef216ea695006c2039f8da2a1c27c09")

  scopus_data_raw <- gen_entries_to_df(scopus_data$entries)
  scopus_papers <- scopus_data_raw$df
   if (is.null(scopus_data[[1]][[1]]$authkeywords)){
  kw_output<-NA
     }else{
  kw_output<- scopus_data[[1]][[1]]$authkeywords
   }
  
  dat <- data.frame(kw_output)
  dat$DOI <- DOI[j]  # maybe you want to keep track of which iteration produced it?
  kw_list[[j]] <- dat # add it to your list
  
} 




# big_data <- dplyr::bind_rows(kw_list)
big_data = do.call(rbind, kw_list)   
write_csv(big_data,"./bibliometrics/data_clean/scopus_no_kw_recovery.csv")

names(big_data)
final_kw<-big_data %>% filter(!is.na(kw_output))
write_csv(final_kw,"./bibliometrics/data_clean/final_kw.csv")






# api loop for keywords by TI and PY  --------------------------------------------

library(rscopus)
library(tidyverse)
DOI<-read.csv("./bibliometrics/data_raw/refs_no_db.csv")
DOI<-as.vector(DOI$ti5)
# DOI<-DOI
SD<-list()
for (j in 1:length(DOI)) {
  a <- ""
  b<-""
  
# DOI<-"DOI(10.1111/1365-2745.12194)"
#   #   DOI <- "DOI(10.1111/1365-2745.12194) and PUBYEAR=1991\""
#   DOI <- "ISSN(00063606) AND PUBYEAR>1991 AND PUBYEAR<1991"
#   DOI <- "KEY(\"cryoelectron microscopy\") AND PUBYEAR > 2005 AND PUBYEAR < 2016\""
#   DOI<-"TITLE(\"short-term activity cycle in ants\") AND PUBYEAR = 1991"
# DOI<-"TITLE(\"short-term activity cycles in ants\") and PUBYEAR = 1991 AND SRCTITLE(\"American Naturalist\")"
# j=1   j=2     j=3
#   
  query_string <-paste0(a, DOI[j],b,sep = "")
  # 
  scopus_data <- rscopus::scopus_search(query_string,
                                        view = "COMPLETE",
                                        api_key = "35605c9cbb3dfa13435f1336d10ff4e3")
  
  
  # 
  # 
  dat <- gen_entries_to_df(scopus_data$entries)
  # scopus_papers <- scopus_data_raw$df

  
  dat$item <- j  # maybe you want to keep track of which iteration produced it?
  SD[[j]] <- dat # add it to your list

} 

write_rds(SD,"./bibliometrics/data_raw/SD.rds")
rm(SD_authors,SD_authors2)

SD_refs<-tibble()
SD_refs2<-tibble()
SD_authors<-tibble()
SD_authors2<-tibble()
SD_affils<-tibble()
SD_affils2<-tibble()
for (j in 1:length(SD)) {
  # j=1
  # j=2
  
  
  
  SD_refs<-bind_rows(SD[j][[1]]$df)
  SD_refs2<-bind_rows(SD_refs,SD_refs2)
  
  SD_authors<-bind_rows(SD[j][[1]]$author)
  SD_authors2<-bind_rows(SD_authors,SD_authors2)
  
  SD_affils<-bind_rows(SD[j][[1]]$affiliation)
  SD_affils2<-bind_rows(SD_affils,SD_affils2)
} 

as.data.frame(do.call(cbind, SD))
# big_data <- dplyr::bind_rows(SD)
big_data = do.call(rbind, SD)   
write_csv(big_data,"./bibliometrics/data_clean/scopus_no_kw_recovery.csv")

names(big_data)
final_kw<-big_data %>% filter(!is.na(kw_output))
write_csv(final_kw,"./bibliometrics/data_clean/final_kw.csv")