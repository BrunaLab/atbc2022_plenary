
# load libraries ----------------------------------------------------------

library(tidyverse)
library(stopwords)
# library(ngram)
# library(tidytext)
# library(igraph)
# library(tidystringdist)

# Load keyword records  -----------------
complete_data<-read_csv("./bibliometrics/data_clean/complete_data_clean.csv") %>% 
  filter(SO!="rbt") %>% 
  # filter(SO!="trop_ecol") %>%
  mutate(pub_cat=as.factor(pub_cat)) %>% 
  mutate(jrnl_cat=as.factor(jrnl_cat)) %>% 
  mutate(index = row_number()) 

unique(complete_data$SO)
summary(complete_data$pub_cat)
summary(complete_data$pub_cat)

uncat<-complete_data %>% filter(is.na(pub_cat))
unique(uncat$SO)



# analysis - title words --------------------------------------------------

analysis_data<-complete_data

analysis_data<-complete_data 

# %>% 
#   filter(PY>=1976)

summary(is.na(analysis_data$TI))
hist(analysis_data$PY)



pub_year<-analysis_data %>% 
  group_by(jrnl_cat,PY) %>% 
  tally() 

# %>%
#   filter(PY>2007)



ggplot(analysis_data, aes(x=PY,color=jrnl_cat)) +
  geom_histogram(fill="white", alpha=0.5)+
  facet_grid(. ~ jrnl_cat)+
  theme_classic()




# how many papers per year?
analysis_data %>% group_by(SO,PY) %>% summarize(n_distinct(index)) %>% arrange(PY)



analysis_data %>% group_by(jrnl_cat) %>% count() %>% arrange(desc(n))
# analysis_data %>% group_by(jrnl_cat,PY) %>% summarise_at(vars(jrnl_cat,PY), count, na.rm = TRUE)



jrnl_yrs<-analysis_data %>% group_by(SO,PY,jrnl_cat) %>% tally() %>% arrange (jrnl_cat,SO,PY)
jrnl_yrs$SO <- factor(jrnl_yrs$SO, levels = unique(jrnl_yrs$SO[order(jrnl_yrs$jrnl_cat, jrnl_yrs$SO)]))
jrnl_yrs$PY <- factor(jrnl_yrs$PY, levels = unique(jrnl_yrs$PY[order(jrnl_yrs$jrnl_cat, jrnl_yrs$PY)]))

ggplot(jrnl_yrs, aes(x = PY, y = SO, fill=jrnl_cat, alpha=n)) +
  geom_tile(color = "black")+
  scale_fill_manual(values=c(tropical="red2", global="navyblue")) +
  theme_bw()

jrnl_count<-analysis_data %>% 
  group_by(SO,PY) %>% 
  tally() %>% 
  mutate(SO=as.factor(SO))

p <- ggplot(jrnl_count, aes(PY, n)) + geom_point()
p <- p + facet_wrap(vars(SO))
p

