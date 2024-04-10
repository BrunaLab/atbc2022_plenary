
# load libraries ----------------------------------------------------------

library(tidyverse)
library(stopwords)
library(ngram)
library(tidytext)
library(igraph)
library(janitor)
library(tidystringdist)
library(here)
library(akc)

# load data ---------------------------------------------------------------

# filter out RBT and TREC

kw<-read_csv(here("bibliometrics","data_clean","keywords.csv")) %>% 
  filter(SO!="rbt") %>% 
  filter(SO!="trec") 

merged_data<-kw %>% 
  select(id=refID, keyword=final,jrnl_cat,pub_cat=pub_cat_2) %>% 
  mutate(cat=paste(jrnl_cat,pub_cat,sep="-"))


set.seed(2)

articles<-merged_data %>% 
  select(id,cat) %>% 
  distinct() %>% 
  group_by(cat) %>% 
  slice_head(n = 1000)

dat <- merged_data %>% 
  inner_join(articles) %>% 
  # filter(cat=="tropical-tropical") %>% 
  group_by(cat,id) %>%
  select(id,keyword) %>% 
  distinct(id,keyword) %>% 
  ungroup() %>% 
  # select(-cat)
  select(-id)
dat


library(widyr)
wordpairs<-dat %>% pairwise_count(keyword,id,sort=TRUE,upper=FALSE)
wordpairs<-dat %>% pairwise_count(keyword,cat,sort=TRUE,upper=FALSE)

# count words co-occurring within sections


wordpairs<-wordpairs %>% rename(V1=item1,
                                V2=item2,
                                weight=n) %>% 
  as_tibble() %>% 
  mutate(V1=gsub(" ","_",V1)) %>% 
  mutate(V2=gsub(" ","_",V2)) 

wordpairs
,
eg=graph_from_data_frame(wordpairs, directed=FALSE)
eg
plot(eg, edge.width=E(eg)$weight, vertex.label.cex=0.5)
vert.attr<-left_join(dat,articles) %>% select(-id)
graph <- graph_from_data_frame(d= elist, vertices= vert.attr)


#######
dendro<-dat %>%
  group_by(cat, keyword) %>%
  tally() %>% 
  arrange(desc(n)) %>% 
  # mutate(n=1)  %>%
  mutate(keyword=gsub(" ","_",keyword)) %>%
  pivot_wider(names_from = keyword,values_from = n) %>%
  mutate(
    across(everything(), ~replace_na(.x, 0))
  )
dendro<-as.data.frame(dendro)
row.names(dendro)<-dendro$cat
dendro<-dendro %>% select(-cat)
# 
# 
dendro
dd <- dist(scale(dendro), method = "euclidean")
hc <- hclust(dd, method = "ward.D2")
plot(hc)
plot(hc, hang = -1, cex = 0.6)
# library(ggdendro)


######

t<-merged_data %>% filter(cat=="tropical-tropical") 
gt<-merged_data %>% filter(cat=="general-tropical")
g<-merged_data %>% filter(cat=="general-general")

merged_data<-gt
merged_data<-t
merged_data<-g
merged_data<-merged_data

grouped_data<-merged_data %>% 
  keyword_group(top=1000, id="cat")

grouped_data %>% 
  keyword_table(top = 10)

grouped_data %>% 
  keyword_vis()



library(widyr)

# count words co-occurring within sections
wordpairs <- t %>% pairwise_count(keyword, id, sort = TRUE)
row_odd <- seq_len(nrow(wordpairs)) %% 2            # Create row indicator
row_odd                                          # Print row indicator
# Next, we can use our dummy to drop all even rows from our data frame
  
wordpairs <- wordpairs[row_odd == 1, ]             # Subset odd rows
wordpairs

wordphi <- t %>% 
  distinct(id,keyword) %>% 
  group_by(id,keyword) %>% 
  mutate(n=n()) %>% arrange(desc(n)) %>% 
  distinct(keyword,n) %>% 
  filter(n<1000) %>% 
  pairwise_cor(keyword, id, sort = TRUE)


# sna for community analysis




install.packages(c("network", "sna", "igraph", "tidygraph", "ggraph", 
                   "intergraph", "remotes"))


library(igraph)

articles<-merged_data %>% 
  select(id,cat) %>% 
  distinct() %>% 
  group_by(cat) %>% 
  slice_head(n = 5)

dat <- merged_data %>% 
  inner_join(articles) %>% 
  group_by(cat,id) %>%
  select(id,keyword) %>% 
  distinct(id,keyword) %>% 
  ungroup() %>% 
  select(-cat)
dat
my_f <-function(x)  do.call(rbind,combn(as.character(x),2,simplify = F))

a <-unique(do.call(rbind,aggregate(keyword~id,dat,my_f)[,2]))
a <-unique(do.call(rbind,aggregate(id~keyword,dat,my_f)[,2]))
as.matrix(as_adj(graph_from_edgelist(a)))
library("sna")
library("network")
net <- as.network(a, directed = FALSE)
class(net)
plot( net )

plot( net , # the network object
      # vertex.cex = degree(net) , # how should nodes (vertices) be scaled
      displaylabels =  TRUE
)


##
library(igraph)
dat <- dat %>% rename(first=id, second=keyword)
net <- graph_from_data_frame(dat, directed=F)
V(net)


wordpairs<-slice_head(wordpairs,n=20)
wordpairs[which(wordpairs$n>1),]
netweighted <- graph_from_edgelist(as.matrix(wordpairs[,1:2]), directed=FALSE)
E(netweighted)$weight <- wordpairs$n
