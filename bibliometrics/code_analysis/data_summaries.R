# load libraries ----------------------------------------------------------

library(tidyverse)
library(stopwords)
library(ngram)
library(tidytext)
library(igraph)
library(janitor)
library(tidystringdist)
library(here)
library(kableExtra)



# Load keyword records  -----------------
# kw<-read_csv(here("bibliometrics","data_clean","keywords.csv")) %>% 
#   filter(SO!="rbt") %>% 
#   filter(SO!="trec") 

# kw_rbt_trec<-read_csv(here("bibliometrics","data_clean","keywords.csv")) %>% 
#   filter(SO=="rbt" | SO=="trec") 

kw<-read_csv(here("bibliometrics","data_clean","keywords.csv"))



# N of publications
n_pubs<-kw %>% summarize(n=n_distinct(refID)) 
n_pubs_trop<-kw %>% filter(pub_cat_2=="tropical") %>% summarize(n=n_distinct(refID)) 
n_pubs_gen<-kw %>% filter(pub_cat_2=="general") %>% summarize(n=n_distinct(refID)) 



# N of journals
jrnls<-kw %>% select(SO) %>% distinct() 
library(xfun)
n_jrnls<-kw %>% select(SO) %>% summarize(n=n_distinct(SO)) 
n_jrnls_trop<-kw %>% filter(jrnl_cat=="tropical") %>% summarize(n=n_distinct(SO)) %>% mutate(n=numbers_to_words(n))
n_jrnls_gen<-kw %>% filter(jrnl_cat=="general") %>% summarize(n=n_distinct(SO)) %>% mutate(n=numbers_to_words(n))


# N of publications by jrnl
n_pubs_jrnl<-kw %>% 
  group_by(SO) %>% 
  summarize(n=n_distinct(refID)) %>% 
  arrange(desc(n))



# time frame: 
yr_last<-kw %>% select(PY) %>% max() 
yr_first<-kw %>% select(PY) %>% min() 

# keywords
kw_orig<-kw %>% select(original) %>% n_distinct()
kw_final<-kw %>% select(final) %>% n_distinct()


# analysis - keywords --------------------------------------------------



pubs_per_pub_cat <- kw %>%
  group_by(pub_cat_2) %>%
  summarise(n_pubs = n_distinct(refID)) %>%
  ungroup() %>%
  arrange(pub_cat_2, desc(n_pubs))
pubs_per_pub_cat

# Top Keywords by pub cat
rankings_pub <- kw %>%
  group_by(pub_cat_2, final) %>%
  tally() %>%
  arrange(pub_cat_2, desc(n)) %>%
  group_by(pub_cat_2) %>%
  # slice_head(n=100) %>%
  arrange(pub_cat_2, desc(n)) %>%
  left_join(pubs_per_pub_cat) %>%
  group_by(pub_cat_2) %>%
  mutate(perc_pubs_wth_kw = (n / n_pubs * 100)) %>%
  group_by(pub_cat_2) %>%
  arrange(pub_cat_2, desc(n)) %>%
  mutate(rank_perc = rank(desc(n), ties.method= "random")) %>%
  # mutate(rank_perc = min_rank(desc(n))) %>%
  # arrange(rank_perc,jrnl_cat,desc(pub_cat_2))
  arrange(desc(pub_cat_2),rank_perc)
rankings_pub

# Identify "system" words

system <- c(
  "mammal",
  "usa",
  "grassland",
  "tropical forest",
  "panama",
  "costa rica",
  "tropical rainforest",
  "bci",
  "bird",
  "drosophila melanogaster",
  "brazil",
  "mexico",
  "tropical dryforest",
  "borneo",
  "cerrado",
  "ecuador",
  "cloud forest",
  "ant",
  "epiphyte",
  "amazonia",
  "secondary forest",
  "tropic",
  "chiroptera",
  "rodent",
  "colombia",
  "atlantic forest",
  "rainforest",
  "puerto rico",
  "savanna",
  "africa",
  "neotropic",
  "amazon",
  "usa",
  "tanzania",
  "venezuela",
  "caribbean"
)
system <- as_tibble(system)

rankings_pub <- rankings_pub %>%
  mutate(system = if_else((final %in% system$value == TRUE), "Y", "N")) %>%
  mutate(system = as.factor(system)) %>% 
  filter(rank_perc<=100)

in_both <- rankings_pub %>%
  group_by(final) %>%
  summarise(n2 = n()) %>%
  filter(n2 > 1) %>%
  mutate(both = TRUE)

plot_kw <- full_join(rankings_pub, in_both, by = "final") %>% 
  select(-n2) %>% 
  replace_na(list(
    "both" = FALSE
    )) %>% 
  rename(cat=pub_cat_2)
  



# venn --------------------------------------------------------------------

kw_both<-plot_kw %>% 
  filter(both==TRUE) %>% 
  select(final,perc_pubs_wth_kw) %>% 
  distinct(final) %>% 
  mutate(cat="both") 
  
  # pivot_wider(names_from = cat,values_from = perc_pubs_wth_kw) %>% 
  # mutate(perc_t=round(tropical,2)) %>% 
  # mutate(perc_g=round(general,2)) %>% 
  # mutate(final=paste(final," (Tr: ",perc_t,", ",sep="")) %>% 
  # mutate(final=paste(final," Non-Tr: ",perc_g,")",sep="")) %>% 
  # mutate(cat="both") 
  # 

kw_trop_only <- plot_kw %>% 
  filter(both==FALSE) %>% 
  filter(cat=="tropical") %>% 
  select(cat,final)
  
   
#   select(final,perc_pubs_wth_kw) %>% 
#   mutate(perc=round(perc_pubs_wth_kw,2)) %>% 
#   mutate(final=paste(final," (",perc,")",sep="")) %>% 
#   mutate(cat="tropical") 

kw_temp_only <- plot_kw %>% 
  filter(both==FALSE) %>% 
  filter(cat=="general") %>% 
  select(cat,final)
  # 
  # select(final,perc_pubs_wth_kw) %>% 
  # mutate(perc=round(perc_pubs_wth_kw,2)) %>% 
  # mutate(final=paste(final," (",perc,")",sep="")) %>% 
  # mutate(cat="non-tropical") 



kw_venn<-bind_rows(kw_both,kw_temp_only,kw_trop_only) %>% mutate(index=row_number()) %>% relocate(index,.before=1) %>% 
  pivot_wider(names_from = cat,values_from = final) %>% 
  kbl() %>%
  kable_classic_2(full_width = F)
  

# CHANGE SO THAT WORDS IN COMMON have fiklled circles, all others are color

# TROP VS. GEN ALL POOLED\

trop_kw <- plot_kw %>%
  filter(cat == "tropical") %>%
  select(final, cat, rank_perc, system) %>%
  # mutate(final=paste(rank_perc,final,sep=": "))
  mutate(final = paste("(", rank_perc, ") ", final, sep = ""))

nontrop_kw <- plot_kw %>%
  filter(cat == "general") %>%
  select(final, cat, rank_perc, system) %>%
  mutate(final = paste(final, " (", rank_perc, ")", sep = ""))



kw_fig <- ggplot(data = plot_kw, aes(x = cat, y = rank_perc, group = final)) +
  # geom_line(arrow = arrow(angle = 12, ends = "both", type = "closed"),linetype = "solid",linewidth=0.7, color="darkgray")+
  geom_line(linetype = "dashed", linewidth = 0.7, color = "darkgray") +
  # geom_point(size=4, aes(color=both))+
  # geom_label(aes(label = final),nudge_x = 1,)+
  geom_text(
    data = trop_kw, aes(
      x = cat, y = rank_perc, label = final, color = factor(system),
      # fontface = "bold"),
      fontface = ifelse(system == "Y", "bold", "plain")
    ),
    hjust = "left", vjust = 0, nudge_x = 0.01, nudge_y = -0.1
  ) +
  geom_text(
    data = nontrop_kw,
    aes(x = cat, y = rank_perc, label = final, color = factor(system),
        fontface = ifelse(system == "Y", "bold", "plain")),
    hjust = "right", vjust = 0, nudge_x = -0.01, nudge_y = -0.1
  ) +
  scale_x_discrete(expand = c(4, 0), guide = guide_axis(n.dodge = 1)) +
  scale_y_reverse() +
  xlab("Article Category") +
  ylab("Keyword\nRank") +
  # scale_y_continuous(limit=c(20, 0))+
  scale_colour_manual(values = c("darkslategray", "#000066"))
kw_fig




kw_fig <- kw_fig + theme_classic() + theme(
  panel.border = element_blank(), panel.grid.major = element_blank(),
  axis.line.y = element_line(color = "black", size = 0.0, lineend = "square"),
  axis.line.x = element_line(color = "black", size = 0.5, lineend = "square"),
  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), # sets colors of axes
  plot.title = element_text(hjust = 0.05, vjust = -1.8, face = "bold", size = 22), # Sets title size, style, location
  axis.title.x = element_text(colour = "black", size = 20, vjust = -2), # sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
  axis.title.y = element_text(colour = "black", size = 20, angle = 0, vjust = 0.5), # sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
  legend.position = "none",
  axis.ticks = element_blank(),
  axis.text.x = element_text(colour = "black", size = 16), # sets size and style of labels on axes
  axis.text.y = element_text(colour = "black", size = 0), # sets size and style of labels on axes
  plot.margin = unit(c(0, 2, 2, 1), "cm")
)
kw_fig







# kw as bar ---------------------------------------------------------------

plot_kw_trop<-plot_kw %>%
  mutate(final=as.factor(final)) %>% 
  filter(cat=="tropical") %>% 
  # select(final, cat, rank_perc, system,perc_pubs_wth_kw) %>%
# mutate(final=paste(rank_perc,final,sep=": "))
  mutate(final = paste(final, " (", rank_perc, ")", sep = "")) %>% 
  mutate(final = fct_reorder(final, perc_pubs_wth_kw)) 
  


plot_kw_not<-plot_kw %>%
  mutate(final=as.factor(final)) %>% 
  filter(cat=="general") %>%
  mutate(final = paste("(", rank_perc, ") ",final, sep = ""))  %>% 
  # select(final, cat, rank_perc, system,perc_pubs_wth_kw) %>%
  mutate(final = fct_reorder(final, perc_pubs_wth_kw)) 
  
  
  # mutate(final = paste(final, " (", rank_perc, ")", sep = "")) %>% 
  
# 
# trop_kw <- plot_kw %>%
#   filter(cat == "tropical") %>%
#   select(final, cat, rank_perc, system,perc_pubs_wth_kw) %>%
#   # mutate(final=paste(rank_perc,final,sep=": "))
#   mutate(final = paste(final, " (", rank_perc, ")", sep = ""))
# 
# nontrop_kw <- plot_kw %>%
#   filter(cat == "general") %>%
#   select(final, cat, rank_perc, system,perc_pubs_wth_kw) %>%
#   mutate(final = paste(final, " (", rank_perc, ")", sep = ""))

GeomLabel$default_aes$size
update_geom_defaults("text", list(size = 6))



kw_trop_bar <- plot_kw_trop %>%   # This trick update the factor levels
  ggplot(aes(x=final, y=perc_pubs_wth_kw,fill = factor(both))) +
  geom_bar(stat="identity", color="black") +
  scale_fill_manual(values = c("white","darkgray"))+
  scale_y_continuous(limits = c(-1, 6), breaks = seq(0, 6, by = .5))+
  ylab("Articles with Keyword (%)")+
  ggtitle("Tropics")+
  # ylim(0, 4)+
  # scale_fill_manual(values = c("white","navy"))+
  coord_flip() +
  geom_text(
    data = plot_kw_trop, aes(
      x = final, y = -.1, 
      label = final, 
      color = factor(system),
      # size= 6,
      # fontface = "bold"),
      fontface = ifelse(system == "Y", "bold", "plain"),
    ),
    hjust = "right", vjust = 0, nudge_x = 0, nudge_y = 0.08
  ) +
  scale_color_manual(values = c("black","navy"))+
  # scale_y_discrete(expand = c(0, 2),limits = c(0, 4)) +
  theme_classic() + theme(
    panel.border = element_blank(), panel.grid.major = element_blank(),
    axis.line.y = element_line(color = "black", size = 0.0, lineend = "square"),
    axis.line.x = element_line(color = "black", size = 0.5, lineend = "square"),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), # sets colors of axes
    plot.title = element_text(hjust = 0.1, vjust =0, face = "bold", size = 22), # Sets title size, style, location
    axis.title.x = element_text(colour = "black", size = 20, vjust=-3,hjust = 0.6), # sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
    axis.title.y = element_blank(),
    # axis.title.y = element_text(colour = "black", size = 20, angle = 0, vjust = 0.5), # sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
    legend.position = "none",
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(colour = "black", size = 16),
    axis.text.y = element_blank(),
    # axis.text.y = element_text(colour = "black", size = 10), # sets size and style of labels on axes
    plot.margin = unit(c(0, 1, 1, 1), "cm")
  )

kw_trop_bar
# 
# 
# kw_not_bar <- plot_kw_not %>%   # This trick update the factor levels
#   ggplot(aes(x=final, y=perc_pubs_wth_kw,fill = factor(system))) +
#   geom_bar(stat="identity") +
#   coord_flip() +
#   theme_classic() + theme(
#     panel.border = element_blank(), panel.grid.major = element_blank(),
#     axis.line.y = element_line(color = "black", size = 0.0, lineend = "square"),
#     axis.line.x = element_line(color = "black", size = 0.5, lineend = "square"),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), # sets colors of axes
#     plot.title = element_text(hjust = 0.05, vjust = -1.8, face = "bold", size = 22), # Sets title size, style, location
#     axis.title.x = element_text(colour = "black", size = 20, vjust = -2), # sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
#     axis.title.y = element_text(colour = "black", size = 20, angle = 0, vjust = 0.5), # sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
#     legend.position = "none",
#     axis.ticks = element_blank(),
#     axis.text.x = element_text(colour = "black", size = 16), # sets size and style of labels on axes
#     axis.text.y = element_text(colour = "black", size = 10), # sets size and style of labels on axes
#     plot.margin = unit(c(0, 2, 2, 1), "cm")
#   )

kw_trop_bar




kw_not_bar <- plot_kw_not %>%   # This trick update the factor levels
  ggplot(aes(x=final, y=perc_pubs_wth_kw,
             fill = factor(both))) +
  geom_bar(stat="identity", color="black") +
  # scale_fill_manual(values = c("white","navy"))+
  scale_fill_manual(values = c("white","darkgray"))+
  ylab("Articles with Keyword (%)")+
  ggtitle("Non-Tropical")+
  geom_text(
      data = plot_kw_not, aes(
        x = final, y = 0.9, 
        label = final, 
        color = factor(system),
        # size= 6,
        # fontface = "bold"),
        fontface = ifelse(system == "Y", "bold", "plain"),
      ),
    hjust = "left", vjust = 0, nudge_x = -0.20, nudge_y = 1
  ) +
  scale_color_manual(values = c("black","navy"))+
  scale_y_reverse(limits = c(6, -1),breaks = seq(0, 6, by = .5))+
  coord_flip() +
  # scale_y_reverse(limits = c(0, 6), breaks = seq(0, 6, by = .5))+
  
  # scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, by = .5))+
  # scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, by = .5))+
  # scale_y_reverse(expand = c(0.3, -0.5))+
  # scale_y_discrete(expand = c(-1.1, 0),limits = c(0, 4)) +
  theme_classic() + theme(
    panel.border = element_blank(), panel.grid.major = element_blank(),
    axis.line.y = element_line(color = "black", size = 0.0, lineend = "square"),
    axis.line.x = element_line(color = "black", size = 0.5, lineend = "square"),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), # sets colors of axes
    plot.title = element_text(hjust = 0.91, vjust = -0.1, face = "bold", size = 22), # Sets title size, style, location
    axis.title.x = element_text(colour = "black", size = 20, vjust = -3), # sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
    axis.title.y = element_blank(),
    # axis.title.y = element_text(colour = "black", size = 20, angle = 0, vjust = 0.5), # sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
    legend.position = "none",
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(colour = "black", size = 16), # sets size and style of labels on axes
    axis.text.y = element_text(colour = "black", size = 0, hjust = 1.0), # sets size and style of labels on axes
    plot.margin = unit(c(0, 2, 2, 2), "cm")
  )

kw_not_bar

kw_trop_bar
kw_not_bar




# evolution jrnls ---------------------------------------------------------


# Load keyword records  -----------------
kw<-read_csv(here("bibliometrics","data_clean","keywords.csv")) %>% 
  filter(SO=="evol") %>% 
  filter(SO!="jeb") 

pubs_per_pub_cat <- kw %>%
  group_by(pub_cat_2) %>%
  summarise(n_pubs = n_distinct(refID)) %>%
  ungroup() %>%
  arrange(pub_cat_2, desc(n_pubs))
pubs_per_pub_cat

# Top Keywords by pub cat
rankings_pub <- kw %>%
  group_by(pub_cat_2, final) %>%
  tally() %>%
  arrange(pub_cat_2, desc(n)) %>%
  group_by(pub_cat_2) %>%
  # slice_head(n=100) %>%
  arrange(pub_cat_2, desc(n)) %>%
  left_join(pubs_per_pub_cat) %>%
  group_by(pub_cat_2) %>%
  mutate(perc_pubs_wth_kw = (n / n_pubs * 100)) %>%
  group_by(pub_cat_2) %>%
  arrange(pub_cat_2, desc(n)) %>%
  # mutate(rank_perc = min_rank(desc(n))) %>%
  mutate(rank_perc = rank(desc(n), ties.method= "random")) %>%
  # arrange(rank_perc,jrnl_cat,desc(pub_cat_2))
  arrange(desc(pub_cat_2),rank_perc)
rankings_pub

# Identify "system" words

system <- c(
  "mammal",
  "usa",
  "grassland",
  "tropical forest",
  "panama",
  "costa rica",
  "tropical rainforest",
  "bci",
  "bird",
  "drosophila melanogaster",
  "drosophila",
  "brazil",
  "mexico",
  "tropical dryforest",
  "borneo",
  "cerrado",
  "ecuador",
  "cloud forest",
  "ant",
  "epiphyte",
  "amazonia",
  "secondary forest",
  "tropic",
  "chiroptera",
  "rodent",
  "colombia",
  "atlantic forest",
  "rainforest",
  "puerto rico",
  "savanna",
  "africa",
  "neotropic",
  "amazon",
  "usa",
  "tanzania",
  "hawaii",
  "island",
  "lizard"
)
system <- as_tibble(system)

rankings_pub <- rankings_pub %>%
  mutate(system = if_else((final %in% system$value == TRUE), "Y", "N")) %>%
  mutate(system = as.factor(system)) %>% 
  filter(rank_perc<=50)

in_both <- rankings_pub %>%
  group_by(final) %>%
  summarise(n2 = n()) %>%
  filter(n2 > 1) %>%
  mutate(both = TRUE)

plot_kw <- full_join(rankings_pub, in_both, by = "final") %>% 
  select(-n2) %>% 
  replace_na(list(
    "both" = FALSE
  )) %>% 
  rename(cat=pub_cat_2)



# CHANGE SO THAT WORDS IN COMMON have fiklled circles, all others are color

# TROP VS. GEN ALL POOLED\

trop_kw <- plot_kw %>%
  filter(cat == "tropical") %>%
  select(final, cat, rank_perc, system) %>%
  # mutate(final=paste(rank_perc,final,sep=": "))
  mutate(final = paste("(", rank_perc, ") ", final, sep = ""))

nontrop_kw <- plot_kw %>%
  filter(cat == "general") %>%
  select(final, cat, rank_perc, system) %>%
  mutate(final = paste(final, " (", rank_perc, ")", sep = ""))



kw_fig <- ggplot(data = plot_kw, aes(x = cat, y = rank_perc, group = final)) +
  # geom_line(arrow = arrow(angle = 12, ends = "both", type = "closed"),linetype = "solid",linewidth=0.7, color="darkgray")+
  geom_line(linetype = "dashed", linewidth = 0.7, color = "darkgray") +
  # geom_point(size=4, aes(color=both))+
  # geom_label(aes(label = final),nudge_x = 1,)+
  geom_text(
    data = trop_kw, aes(
      x = cat, y = rank_perc, label = final, color = factor(system),
      # fontface = "bold"),
      fontface = ifelse(system == "Y", "bold", "plain")
    ),
    hjust = "left", vjust = 0, nudge_x = 0.01, nudge_y = -0.1
  ) +
  geom_text(
    data = nontrop_kw,
    aes(x = cat, y = rank_perc, label = final, color = factor(system),
        fontface = ifelse(system == "Y", "bold", "plain")),
    
    hjust = "right", vjust = 0, nudge_x = -0.01, nudge_y = -0.1
  ) +
  scale_x_discrete(expand = c(4, 0), guide = guide_axis(n.dodge = 1)) +
  scale_y_reverse() +
  xlab("Article Category") +
  ylab("Keyword\nRank") +
  # scale_y_continuous(limit=c(20, 0))+
  scale_colour_manual(values = c("darkslategray", "#000066"))
kw_fig




kw_fig <- kw_fig + theme_classic() + theme(
  panel.border = element_blank(), panel.grid.major = element_blank(),
  axis.line.y = element_line(color = "black", size = 0.0, lineend = "square"),
  axis.line.x = element_line(color = "black", size = 0.5, lineend = "square"),
  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), # sets colors of axes
  plot.title = element_text(hjust = 0.05, vjust = -1.8, face = "bold", size = 22), # Sets title size, style, location
  axis.title.x = element_text(colour = "black", size = 20, vjust = -2), # sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
  axis.title.y = element_text(colour = "black", size = 20, angle = 0, vjust = 0.5), # sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
  legend.position = "none",
  axis.ticks = element_blank(),
  axis.text.x = element_text(colour = "black", size = 16), # sets size and style of labels on axes
  axis.text.y = element_text(colour = "black", size = 0), # sets size and style of labels on axes
  plot.margin = unit(c(0, 2, 2, 1), "cm")
)
kw_fig





















# original point chart ----------------------------------------------------



GT_fig <- ggplot(data = plot_kw, aes(x = cat, y = rank_perc, group = final)) +
  # geom_line(arrow = arrow(angle = 12, ends = "both", type = "closed"),linetype = "solid",linewidth=0.7, color="darkgray")+
  geom_line(linetype = "dashed", linewidth = 0.7, color = "darkgray") +
  geom_point(size=4, aes(color=both))+
  # geom_label(aes(label = final),nudge_x = 1,)+
  geom_text(
    data = plot_kw_trop, aes(
      x = cat, y = rank_perc, label = final, color = factor(system),
      # fontface = "bold"),
      fontface = ifelse(system == "Y", "bold", "plain")
    ),
    hjust = "left", vjust = 0, nudge_x = 0.04, nudge_y = -0.1
  ) +
  geom_text(
    data = plot_kw_not, aes(x = cat, y = rank_perc, label = final, color = factor(system)),
    hjust = "right", vjust = 0, nudge_x = -0.04, nudge_y = -0.1
  ) +
  # geom_text(data=GT, aes(x=cat, y=rank_perc,label=final,color=factor(system)),
  #            hjust = "right", vjust = 0, nudge_x = -0.02)+
  scale_x_discrete(expand = c(2, 0)) +
  scale_y_reverse() +
  xlab("Journal Category") +
  ylab("Keyword\nRank") +
  # scale_y_continuous(limit=c(20, 0))+
  scale_colour_manual(values = c("darkslategray", "white","black","darkblue"))
GT_fig

GT_fig <- GT_fig + theme_classic() + theme(
  panel.border = element_blank(), panel.grid.major = element_blank(),
  axis.line.y = element_line(color = "black", size = 0.0, lineend = "square"),
  axis.line.x = element_line(color = "black", size = 0.5, lineend = "square"),
  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), # sets colors of axes
  plot.title = element_text(hjust = 0.05, vjust = -1.8, face = "bold", size = 22), # Sets title size, style, location
  axis.title.x = element_text(colour = "black", size = 20, vjust = -2), # sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
  axis.title.y = element_text(colour = "black", size = 20, angle = 0, vjust = 0.5), # sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
  legend.position = "none",
  axis.ticks = element_blank(),
  axis.text.x = element_text(colour = "black", size = 16), # sets size and style of labels on axes
  axis.text.y = element_text(colour = "black", size = 0), # sets size and style of labels on axes
  plot.margin = unit(c(0, 2, 2, 1), "cm")
)
GT_fig


















_____________________
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

