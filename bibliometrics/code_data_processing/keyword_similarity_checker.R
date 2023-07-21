kw_similarity_checker <- function(DataToClean) {
  library(RecordLinkage)
  CHECKFILE <- DataToClean
  NamesList <- sapply(CHECKFILE$final, agrep, CHECKFILE$final, value = TRUE)
  NamesDF <- data.frame(
    Name1 = rep(names(NamesList), lapply(NamesList, length)),
    Name2 = unlist(NamesList)
  )
  
  # summary(NamesDF)
  # str(NamesDF)
  
  # Create a column to which you will add a logical condition telling you if the names are an EXACT match
  NamesDF$match <- NA
  NamesDF$match <- NamesDF$Name1 == NamesDF$Name2
  # match2<-ifelse(NamesDF$match=="TRUE",1,0) #convert TRUE/FALSEto 0/1
  # NamesDF<-cbind(NamesDF,match2)
  # head(NamesDF,40)
  # str(NamesDF)
  NamesDF <- arrange(NamesDF, Name1, Name2) # organize in alphabetica order
  NamesDF <- filter(NamesDF, match == FALSE) # THIS DELETES ALL NAMES THAT ARE 100% MATCH
  head(NamesDF)
  # Convert to chr
  NamesDF$Name1 <- as.character(NamesDF$Name1)
  NamesDF$Name2 <- as.character(NamesDF$Name2)
  str(NamesDF)
  # Because this does all pairwise comparisons, it results in redundancy: "e bruna vs emilio bruna" and "emilio bruna vs e bruna"
  # are in different rows, even though they are the same "comparison". This deletes one of the two
  NamesDF <- NamesDF[!duplicated(t(apply(NamesDF, 1, sort))), ]
  # this arranges them in order from most similar (1 change required) to least similar.
  # look carefully at those with a few changes, as they are likely to be a tiny spelling mistake or difference in intials
  
  # Calclulate the proportional similarity and # changes required to go from one name to another. Package RecordLinkage
  NamesDF$Name_sim <- levenshteinSim(NamesDF$Name1, NamesDF$Name2)
  NamesDF$Name_dist <- levenshteinDist(NamesDF$Name1, NamesDF$Name2)
  
  # Because this does all pairwise comparisons, it results in redundancy: "e bruna vs emilio bruna" and "emilio bruna vs e bruna"
  # are in different rows, even though they are the same "comparison". This deletes one of the two
  # NamesDF <- NamesDF[!duplicated(t(apply(NamesDF, 1, sort))), ]
  # this arranges them in order from most similar (1 change required) to least similar.
  # look carefully at those with a few changes, as they are likely to be a tiny spelling mistake or difference in intials
  
  
  NamesDF$index <- seq.int(nrow(NamesDF)) # adds a column with an index to make it easier to id which row you need'
  NamesDF <- NamesDF %>% select(index, Name1, Name2, Name_sim, Name_dist) # It's kinda ugly, but this rearranges columns (and dumps the "FALSE")
  NamesDF <- arrange(NamesDF, desc(Name_sim))
  NamesDF <- NamesDF %>% filter(Name_sim>0.50)
  # head(NamesDF)
  write_csv(NamesDF, file = "./bibliometrics/data_intermediate/kw_similarity.csv") # export it as a csv file
  
  
  return(NamesDF)
}