# load libraries ----------------------------------------------------------

library(tidyverse)
# library(stopwords)
# library(ngram)
library(tidytext)
# library(igraph)
library(janitor)
# library(tidystringdist)
library(here)


# load data ---------------------------------------------------------------

# filter out RBT and TREC

kw <- read_csv(here("bibliometrics", "data_clean", "keywords.csv")) 

# %>%
#   filter(SO != "rbt") %>%
#   filter(SO != "trec")

# how much data to we have? -----------------------------------------------



jrnl_yrs <- kw %>%
  group_by(SO, PY, jrnl_cat) %>%
  tally() %>%
  arrange(
    jrnl_cat,
    SO,
    PY
  )


jrnl_yrs$SO <- factor(jrnl_yrs$SO, levels = unique(jrnl_yrs$SO[order(jrnl_yrs$jrnl_cat, jrnl_yrs$SO)]))
jrnl_yrs$PY <- factor(jrnl_yrs$PY, levels = unique(jrnl_yrs$PY[order(jrnl_yrs$jrnl_cat, jrnl_yrs$PY)]))

p1 <- ggplot(jrnl_yrs, aes(x = PY, y = SO, fill = jrnl_cat, alpha = n)) +
  geom_tile(color = "black") +
  scale_fill_manual(values = c(tropical = "darkgreen", general = "black")) +
  theme_bw()
p1

jrnl_count <- kw %>%
  group_by(SO, PY) %>%
  tally() %>%
  mutate(SO = as.factor(SO))

p2 <- ggplot(jrnl_count, aes(PY, n)) +
  geom_point()
p2 <- p2 + facet_wrap(vars(SO))
p2


# years of coverage -------------------------------------------------------
kw %>%
  select(SO, PY) %>%
  arrange(SO, PY) %>%
  group_by(SO) %>%
  slice(1) %>%
  arrange(PY)


#
# kw<-read_csv("./bibliometrics/data_clean/keywords.csv") %>%
#   filter(SO!="tcs") %>%
#   filter(SO!="amnat") %>%
#   filter(PY>1991)

# %>%
#   filter(PY>2000)

# Papers per journal

pubs_per_jrnl <- kw %>%
  group_by(jrnl_cat, SO) %>%
  summarise(n_pubs = n_distinct(refID)) %>%
  arrange(desc(n_pubs)) %>%
  arrange(jrnl_cat, desc(n_pubs))

# papers per journal by pub_cat


pubs_per_cat <- kw %>%
  group_by(pub_cat_2) %>%
  summarise(n_pubs = n_distinct(refID)) %>%
  ungroup() %>%
  arrange(desc(pub_cat_2))
pubs_per_cat

pubs_per_pub_cat <- kw %>%
  group_by(jrnl_cat, pub_cat_2) %>%
  summarise(n_pubs = n_distinct(refID)) %>%
  ungroup() %>%
  arrange(jrnl_cat, pub_cat_2, desc(n_pubs))
pubs_per_pub_cat


pubs_per_jrnl_pub_cat <- kw %>%
  group_by(jrnl_cat, pub_cat_2, SO) %>%
  summarise(n_pubs = n_distinct(refID)) %>%
  ungroup() %>%
  arrange(jrnl_cat, SO, pub_cat_2, desc(n_pubs))
pubs_per_jrnl_pub_cat

pubs_per_jrnl_pub_cat_wide<-pubs_per_jrnl_pub_cat %>% 
  filter(jrnl_cat=="general") %>% 
  pivot_wider(
    names_from = pub_cat_2,
    values_from = n_pubs) %>% 
  mutate(perc_trop=tropical/(general+tropical)*100)

overall_perc_trop<-((sum(pubs_per_jrnl_pub_cat_wide$tropical))/sum(pubs_per_jrnl_pub_cat_wide$general))*100
avg_perc_trop<-mean(pubs_per_jrnl_pub_cat_wide$perc_trop)
var_perc_trop<-sd(pubs_per_jrnl_pub_cat_wide$perc_trop)


# Total Distinct Keywords

kw %>% summarize(n_distinct(final))

# top kw overalll

top_kw_overall <- kw %>%
  group_by(final) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  mutate(perc = n / sum(n) * 100) %>%
  mutate(cum_perc = cumsum(perc))
top_kw_overall


# Total Distinct Keywords by Journal

kw_jrnl <- kw %>%
  group_by(SO, pub_cat_2) %>%
  summarize(n_kw = n_distinct(final)) %>%
  arrange(desc(n_kw))
kw_jrnl


# Top Keywords by Journal
top_kw_jrnl <- kw %>%
  group_by(SO, final) %>%
  tally() %>%
  arrange(SO, desc(n))


# Top Keywords by Journal (pub cat/jrnl cat)
top_kw_jrnl_2 <- kw %>%
  group_by(jrnl_cat, pub_cat_2, SO, final) %>%
  tally() %>%
  arrange(jrnl_cat, SO, pub_cat_2, desc(n)) %>%
  group_by(pub_cat_2, SO) %>%
  # slice_head(n=100) %>%
  arrange(desc(jrnl_cat), SO, pub_cat_2, desc(n)) %>%
  left_join(pubs_per_jrnl_pub_cat) %>%
  group_by(jrnl_cat, SO, pub_cat_2) %>%
  mutate(perc_pubs_wth_kw = (n / n_pubs * 100)) %>%
  group_by(pub_cat_2, SO) %>%
  arrange(jrnl_cat, SO, pub_cat_2, desc(n)) %>%
  mutate(rank_perc = row_number()) %>%
  # arrange(rank_perc,jrnl_cat,desc(pub_cat_2))
  arrange(rank_perc, desc(pub_cat_2), jrnl_cat)
top_kw_jrnl_2

words <- top_kw_jrnl_2 %>%
  mutate(source = paste(SO, pub_cat_2, sep = "_")) %>%
  select(jrnl_cat, source, word = final, n)

levels_source <- c(unique(words$source))

plot_words <- words %>%
  bind_tf_idf(word, source, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  mutate(source = factor(source, levels = levels_source))

plot_words %>%
  group_by(source) %>%
  top_n(15, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = source)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf_idf") +
  facet_wrap(~source, ncol = 2, scales = "free") +
  coord_flip()

# DONT RANK BY JOURNAL


# Top Keywords by pub cat/jrnl cat
top_kw_jrnl_3 <- kw %>%
  group_by(jrnl_cat, pub_cat_2, final) %>%
  tally() %>%
  arrange(jrnl_cat, pub_cat_2, desc(n)) %>%
  group_by(pub_cat_2) %>%
  # slice_head(n=100) %>%
  arrange(desc(jrnl_cat), pub_cat_2, desc(n)) %>%
  left_join(pubs_per_pub_cat) %>%
  group_by(jrnl_cat, pub_cat_2) %>%
  mutate(perc_pubs_wth_kw = (n / n_pubs * 100)) %>%
  group_by(jrnl_cat, pub_cat_2) %>%
  arrange(jrnl_cat, pub_cat_2, desc(n)) %>%
  mutate(rank_perc = row_number()) %>%
  # arrange(rank_perc,jrnl_cat,desc(pub_cat_2))
  arrange(rank_perc, desc(pub_cat_2), jrnl_cat)
top_kw_jrnl_3


top_kw_jrnl_3 %>%
  filter(jrnl_cat == "general") %>%
  filter(pub_cat_2 == "general") %>%
  filter(str_detect(final, "temperate"))
top_kw_jrnl_3 %>%
  filter(jrnl_cat == "general") %>%
  filter(pub_cat_2 == "general") %>%
  filter(str_detect(final, "tropical"))
top_kw_jrnl_3 %>%
  filter(jrnl_cat == "general") %>%
  filter(pub_cat_2 == "tropical") %>%
  filter(str_detect(final, "tropical"))
top_kw_jrnl_3 %>%
  filter(jrnl_cat == "tropical") %>%
  filter(pub_cat_2 == "tropical") %>%
  filter(str_detect(final, "tropical"))

top_kw_jrnl_3 %>% filter(str_detect(final, "tropical"))


# average word ranking by jrnsal and article type

avg_rank <- top_kw_jrnl_2 %>%
  group_by(final, jrnl_cat, pub_cat_2) %>%
  summarize(avg_rank = mean(rank_perc)) %>%
  arrange(final)
avg_rank


avg_rank_wide <- avg_rank %>%
  pivot_wider(
    names_from = c(jrnl_cat, pub_cat_2),
    values_from = c(avg_rank)
  ) %>%
  replace_na(list(
    "tropical_tropical" = 0,
    "general_general" = 0,
    "general_tropical" = 0
  ))
#
# ggplot(avg_rank_wide, aes(x=general_general, y=tropical_tropical)) +
#   geom_point(size=6)




###
# plot_data<-avg_rank %>% mutate(cat=paste(jrnl_cat,pub_cat_2,sep="-")) %>%
#   filter(cat!="general-tropical")
system_list<-read_csv(here("bibliometrics","code_analysis","system.csv"), col_names = FALSE) %>% 
  rename("system"="X1")

top_kw_jrnl_3_fig <- top_kw_jrnl_3 %>%
  mutate(cat = paste(jrnl_cat, pub_cat_2, sep = "-")) %>%
  mutate(system = if_else((final %in% system_list$system == TRUE), "Y", "N")) %>%
  filter(rank_perc <= 30) %>% 
  filter(cat != "general-tropical")


in_both <- top_kw_jrnl_3_fig %>%
  group_by(final) %>%
  summarise(n2 = n()) %>%
  filter(n2 > 1) %>%
  mutate(both = "Yes")

plot_data<-full_join(top_kw_jrnl_3_fig, in_both, by = "final") %>% 
  select(-n2) %>% 
  replace_na(list(both = "No")) 


TT <- plot_data %>%
  filter(cat == "tropical-tropical") %>%
  select(final, cat, rank_perc, system, both) %>%
  # mutate(final=paste(rank_perc,final,sep=": "))
  mutate(final = paste("(", rank_perc, ") ", final, sep = ""))

GG <- plot_data %>%
  filter(cat == "general-general") %>%
  select(final, cat, rank_perc, system, both) %>%
  mutate(final = paste(final, " (", rank_perc, ")", sep = ""))

GT <- plot_data %>%
  filter(cat == "general-tropical") %>%
  select(final, cat, rank_perc, system, both) %>%
  mutate(final = paste(final, " (", rank_perc, ")", sep = ""))

plot_data<-plot_data %>% 
group_by(cat) %>%
  mutate(word = reorder(final, rank_perc))



ggplot(plot_data, aes(x=word, y=perc_pubs_wth_kw)) + 
  geom_bar(stat = "identity") +
  coord_flip()+
facet_wrap(~cat, ncol = 2, scales = "free") 


+
plot_data %>%
  ungroup() %>%
  
  aes(x=as.factor(final), fill=as.factor(both)) + 
  geom_bar()
  ggplot(aes(word,fill = both)) +
  geom_bar(show.legend = FALSE) +
  labs(x = NULL, y = "tf_idf") +
  
  coord_flip()

# %>%
# filter(cat!="general-general")
# filter(cat!="tropical-tropical") %>%


# CHANGE SO THAT WORDS IN COMMON have fiklled circles, all others are color


GGTT_fig <- ggplot(data = plot_data, aes(x = cat, y = rank_perc, group = final)) +
  # geom_line(arrow = arrow(angle = 12, ends = "both", type = "closed"),linetype = "solid",linewidth=0.7, color="darkgray")+
  # geom_line(linetype = "dashed", linewidth = 0.7, color = "darkgray") +
  # geom_point(size=4, aes(color=both))+
  # geom_label(aes(label = final),nudge_x = 1,)+
  geom_text(
    data = TT, aes(
      x = cat, y = rank_perc, label = final, color = factor(both),
      # fontface = "bold"),
      fontface = ifelse(both == "Yes", "bold", "plain")
    ),
    hjust = "left", vjust = 0, nudge_x = 0.04, nudge_y = -0.1
  ) +
  geom_text(
    data = GG, aes(x = cat, y = rank_perc, label = final, color = factor(both),
                   fontface = ifelse(both == "Yes", "bold", "plain")),
    hjust = "right", vjust = 0, nudge_x = -0.04, nudge_y = -0.1
  ) +
  # geom_text(data=GT, aes(x=cat, y=rank_perc,label=final,color=factor(system)),
  #            hjust = "right", vjust = 0, nudge_x = -0.02)+
  scale_x_discrete(expand = c(4, 0), guide = guide_axis(n.dodge = 3)) +
  scale_y_reverse() +
  xlab("Journal Category") +
  ylab("Keyword\nRank") +
  # scale_y_continuous(limit=c(20, 0))+
  scale_colour_manual(values = c("darkslategray", "#000066"))
GGTT_fig

GGTT_fig <- GGTT_fig + theme_classic() + theme(
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
GGTT_fig

###


# CHANGE SO THAT WORDS IN COMMON have fiklled circles, all others are color




GGTT_fig <- ggplot(data = plot_data, aes(x = cat, y = rank_perc, group = final)) +
  # geom_line(arrow = arrow(angle = 12, ends = "both", type = "closed"),linetype = "solid",linewidth=0.7, color="darkgray")+
  # geom_line(linetype = "dashed", linewidth = 0.7, color = "darkgray") +
  # geom_point(size=4, aes(color=both))+
  # geom_label(aes(label = final),nudge_x = 1,)+
  geom_text(
    data = TT, aes(
      x = cat, y = rank_perc, label = final, color = factor(system),
      # fontface = "bold"),
      fontface = ifelse(system == "Y", "bold", "plain")
    ),
    hjust = "left", vjust = 0, nudge_x = 0.04, nudge_y = -0.1
  ) +
  geom_text(
    data = GG, aes(x = cat, y = rank_perc, label = final, color = factor(system)),
    hjust = "right", vjust = 0, nudge_x = -0.04, nudge_y = -0.1
  ) +
  # geom_text(data=GT, aes(x=cat, y=rank_perc,label=final,color=factor(system)),
  #            hjust = "right", vjust = 0, nudge_x = -0.02)+
  scale_x_discrete(expand = c(4, 0), guide = guide_axis(n.dodge = 3)) +
  scale_y_reverse() +
  xlab("Journal Category") +
  ylab("Keyword\nRank") +
  # scale_y_continuous(limit=c(20, 0))+
  scale_colour_manual(values = c("darkslategray", "#000066"))
GGTT_fig

GGTT_fig <- GGTT_fig + theme_classic() + theme(
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
GGTT_fig






#############

# gg vs gt

plot_data <-
  top_kw_jrnl_3_fig %>%
  # filter(cat!="general-tropical")
  # %>%
  # filter(cat!="general-general")
  filter(cat != "tropical-tropical")

general_fig <- ggplot(data = plot_data, aes(x = cat, y = rank_perc, group = final)) +
  # geom_line(arrow = arrow(angle = 12, ends = "both", type = "closed"),linetype = "solid",linewidth=0.7, color="darkgray")+
  geom_line(linetype = "solid", linewidth = 0.7, color = "darkgray") +
  geom_point(size = 4, aes(colour = jrnl_cat)) +
  # geom_label(aes(label = final),nudge_x = 1,)+
  # geom_text(data=TT, aes(x=cat, y=rank_perc,label=final,color=factor(system),
  #                        # fontface = "bold"),
  #                        fontface = ifelse(system =="Y", "bold", "plain")),
  #           hjust = "left", vjust = 0, nudge_x = 0.04, nudge_y = -0.1)+
  geom_text(
    data = GG, aes(x = cat, y = rank_perc, label = final, color = factor(system)),
    hjust = "right", vjust = 0, nudge_x = -0.04, nudge_y = -0.1
  ) +
  geom_text(
    data = GT, aes(
      x = cat, y = rank_perc, label = final, color = factor(system),
      # fontface = "bold"),
      fontface = ifelse(system == "Y", "bold", "plain")
    ),
    hjust = "left", vjust = 0, nudge_x = 0.04, nudge_y = -0.1
  ) +
  scale_y_reverse() +
  xlab("Journal Category") +
  ylab("Keyword\nRank") +
  # scale_y_continuous(limit=c(20, 0))+
  scale_colour_manual(values = c("darkslategray", "#000066", "#006633", "#000000"))
general_fig
general_fig <- general_fig + theme_classic() + theme(
  panel.border = element_blank(), panel.grid.major = element_blank(),
  axis.line.y = element_line(color = "black", size = 0.0, lineend = "square"),
  axis.line.x = element_line(color = "black", size = 0.5, lineend = "square"),
  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), # sets colors of axes
  plot.title = element_text(hjust = 0.05, vjust = -1.8, face = "bold", size = 22), # Sets title size, style, location
  axis.title.x = element_text(colour = "black", size = 20, vjust = -2), # sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
  axis.title.y = element_text(colour = "black", size = 20, angle = 0, vjust = 0.5), # sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
  legend.position = "none",
  axis.text.x = element_text(colour = "black", size = 16), # sets size and style of labels on axes
  axis.text.y = element_text(colour = "black", size = 0), # sets size and style of labels on axes
  plot.margin = unit(c(0, 2, 2, 1), "cm")
)
general_fig


###############

####
# plot_data_gg <- plot_data %>% filter(cat=="general-general")
# plot_data_gt1 <- plot_data %>% filter(cat=="general-tropical") %>% mutate(cat="general-tropical1")
# plot_data_gt1.5 <- plot_data %>% filter(cat=="general-tropical") %>% mutate(cat="general-tropical1.5")
# plot_data_gt2 <- plot_data %>% filter(cat=="general-tropical") %>% mutate(cat="general-tropical2")
# plot_data_tt<-plot_data %>% filter(cat=="tropical-tropical")
#
# plot_data2<-bind_rows(plot_data_gg,plot_data_gt1,plot_data_gt1.5,plot_data_gt2,plot_data_tt)
#
# GT<-plot_data2 %>%
#   filter(cat=='general-tropical1.5') %>%
#   select(final,cat,rank_perc) %>%
#   mutate(final=paste("(",rank_perc,") ",final," (",rank_perc,")", sep=""))
#
#
# unique(plot_data2$cat)
# cover.fig.location<-ggplot(data=plot_data2, aes(x=cat, y=rank_perc,group=final))+
#   geom_line(linewidth=0.5) +
#   geom_point(size=4, aes(colour=jrnl_cat))+
#   # geom_label(aes(label = final),nudge_x = 1,)+
#   geom_text(data=TT, aes(x=cat, y=rank_perc,label=final),
#             hjust = "left", vjust = 0, nudge_x = 0.02)+
#   geom_text(data=GG, aes(x=cat, y=rank_perc,label=final),
#             hjust = "right", vjust = 0, nudge_x = -0.02)+
#   geom_text(data=GT, aes(x=cat, y=rank_perc,label=final),
#             hjust = "center", vjust = 0, nudge_x = -0.02)+
#   scale_y_reverse()+
#   xlab("Journal Category")+
#   ylab("Rank")+
#   # scale_y_continuous(limit=c(20, 0))+
#   scale_colour_manual(values=c("#000066","#006633"))
# cover.fig.location
# # +
# #   annotate ("text", x=0.6, y=99, label="B", fontface="bold", size=8, color="black")
#
# cover.fig.location<-cover.fig.location + theme_classic() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#                                                                  axis.line.y = element_line(color="black", size = 0.5, lineend="square"),
#                                                                  axis.line.x = element_line(color="black", size = 0.5, lineend="square"),
#                                                                  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), #sets colors of axes
#                                                                  plot.title = element_text(hjust=0.05, vjust=-1.8, face="bold", size=22),        #Sets title size, style, location
#                                                                  axis.title.x=element_text(colour="black", size = 20, vjust=-2),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
#                                                                  axis.title.y=element_text(colour="black", size = 20, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
#                                                                  legend.position = "none",
#                                                                  axis.text=element_text(colour="black", size = 16),                              #sets size and style of labels on axes
#                                                                  plot.margin = unit(c(0,2,2,1), "cm"))
# cover.fig.location


#####

####



####



#####

###






####

#### as rank abundance plot

lineplot <- ggplot(data = plot_data, aes(x = rank_perc, y = perc_pubs_wth_kw, group = cat)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 2, aes(colour = jrnl_cat)) +
  geom_text(
    data = plot_data, aes(x = rank_perc, y = perc_pubs_wth_kw, label = final),
    hjust = "left", vjust = 0, nudge_x = 0.02
  )
lineplot

#####





avg_rank %>%
  ungroup() %>%
  count(final) %>%
  arrange(desc(n))

# KW20<-unique(top_kw_jrnl_2$final) %>% as_tibble()

top_kw_jrnl_2 <- top_kw_jrnl_2 %>%
  mutate(system = if_else((final %in% systemList$system == TRUE), "Y", "N"))

top_kw_jrnl_2 %>%
  filter(system == "Y") %>%
  group_by(jrnl_cat, pub_cat_2, system) %>%
  tally()

top_kw_jrnl_2 %>%
  group_by(jrnl_cat, pub_cat_2) %>%
  tally()

# TODO: (USA) vs USA in KW - extract





# Top Keywords by Journal as N and %
top_kw_summ <- top_kw_jrnl %>%
  left_join(pubs_per_jrnl, by = "SO") %>%
  mutate(perc_of_kw = n / n_kw * 100) %>%
  arrange(SO, desc(perc_of_kw)) %>%
  group_by(SO) %>%
  mutate(rank_perc_kw = row_number()) %>%
  left_join(pubs_per_jrnl, by = "SO") %>%
  mutate(perc_of_pubs = n / n_pubs * 100) %>%
  arrange(SO, desc(perc_of_pubs)) %>%
  group_by(SO) %>%
  mutate(rank_perc_pubs = row_number()) %>%
  select(-rank_perc_kw) %>%
  rename(rank = rank_perc_pubs)



# Top Keywords by Journal

top20_rank <- top_kw_summ %>%
  filter(rank < 21)

top5 %>% arrange(final)



# Top Keywords by Journal

top_kw_jrnl %>%
  group_by(SO) %>%
  slice_head(n = 3)

# Top Keywords by Journal


# KW by DECADE ------------------------------------------------------------


# Total Distinct Keywords by Decade

binned_kw <- kw %>%
  # select(final, SO,jrnl_cat,pub_cat_2,PY) %>%
  mutate(decade = cut_width(PY, 6)) %>%
  relocate(decade, .before = PY)

# binned_time %>%
#   group_by(decade) %>%
#   summarize(n=n_distinct(final)) %>%
#   arrange(desc(decade))

pubs_per_jrnl_cat_binned <- binned_kw %>%
  group_by(decade, jrnl_cat, pub_cat_2, SO) %>%
  summarise(n_pubs = n_distinct(refID)) %>%
  ungroup() %>%
  arrange(pub_cat_2, jrnl_cat, desc(n_pubs))






# Top Keywords by Journal (pub cat/jrnl cat)
top_kw_jrnl_binned <- binned_kw %>%
  group_by(jrnl_cat, pub_cat_2, SO, decade, final) %>%
  tally() %>%
  arrange(jrnl_cat, pub_cat_2, SO, decade, desc(n)) %>%
  group_by(decade, pub_cat_2, SO) %>%
  slice_head(n = 10) %>%
  arrange(decade, desc(jrnl_cat), SO, pub_cat_2, desc(n)) %>%
  left_join(pubs_per_jrnl_cat_binned) %>%
  mutate(perc_pubs_wth_kw = (n / n_pubs * 100))


top_kw_jrnl_binned

graph_data <- top_kw_jrnl_binned %>%
  filter(decade != "[1983,1989]") %>%
  filter(decade == "(1989,1995]")
# %>%
# filter(SO=="amnat")


ggplot(graph_data, aes(x = final, y = perc_pubs_wth_kw)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  # facet_grid(vars(SO), vars(decade))
  facet_grid(cols = vars(SO))


# figures -----------------------------------------------------------------


fig_data <- kw_table %>%
  group_by(jrnl_cat, pub_cat_2, SO) %>%
  slice(1:20)
# %>% mutate(rank=row_number())


ggplot(fig_data, aes(x = final, y = perc)) +
  geom_bar(stat = "identity") +
  facet_grid(pub_cat_2 ~ SO)




trop_sum <- kw %>%
  filter(pub_cat_2 == "tropical") %>%
  group_by(SO, final) %>%
  tally() %>%
  arrange(desc(n)) %>%
  rename(n_trop = n) %>%
  mutate(trop_rank = row_number()) %>%
  arrange(SO, trop_rank)
trop_sum

avg_rank_trop <- trop_sum %>%
  group_by(final) %>%
  mutate(avg_rank = mean(trop_rank)) %>%
  mutate(median_rank = median(trop_rank)) %>%
  arrange(median_rank)


general_sum <- kw %>%
  filter(pub_cat_2 == "general") %>%
  group_by(SO, final) %>%
  tally() %>%
  arrange(desc(n)) %>%
  mutate(gen_rank = row_number()) %>%
  rename(n_gen = n)
general_sum
#
# NA_sum<-kw %>%
#   filter(is.na(pub_cat_2)) %>%
#   group_by(final) %>%
#   tally() %>%
#   arrange(desc(n)) %>%
#   mutate(NA_rank=row_number()) %>%
#   rename(n_NA=n)
# NA_sum

kw_ranks <- full_join(trop_sum, general_sum, by = "final") %>%
  relocate(n_gen, n_trop, .after = 1) %>%
  relocate(gen_rank, trop_rank, .after = 4)

kw_ranks
# %>%
#   mutate(diff=trop_rank-gen_rank) %>%
#   filter(diff<100 & diff>-100)
# kw similarity -----------------------------------------------------------

#
#
# kw_summary <- keywords %>%
#   group_by(final) %>%
#   tally() %>%
#   arrange(desc(n))
# kw_summary
#
#
#




# kw_summary <- kw_summary %>% mutate_all(trimws)
# foo <- kw_summary %>% slice(500:1500)
# DataToClean<-s_words %>% rename(final=last_word)
# DataToClean<-kw_summary_100
# DataToClean<-kw_summary



# keyword ngrams ----------------------------------------------------------

# gram frequency independet of position in kw

# kw_summary <- keywords %>%
#   group_by(final) %>%
#   tally() %>%
#   arrange(desc(n))
# kw_summary

# TODO: can also do n_grams on the kw themselves



# kw_to_grams
grams_split <- kw_summary %>%
  # filter(str_detect(kw_summary$final,".*[0-9].*")==TRUE)
  mutate(kw_id = row_number(), .before = 1) %>%
  mutate(gram = strsplit(final, " ")) %>%
  unnest(gram) %>%
  group_by(kw_id) %>%
  mutate(row = row_number()) %>%
  pivot_wider(names_from = row, values_from = gram, names_prefix = "gram_") %>%
  # spread(row, gram) %>%
  ungroup()



gram_tally <- kw_summary %>%
  # filter(str_detect(kw_summary$final,".*[0-9].*")==TRUE)
  mutate(kw_id = row_number(), .before = 1) %>%
  mutate(gram = strsplit(final, " ")) %>%
  unnest(gram) %>%
  group_by(gram) %>%
  tally() %>%
  arrange(desc(n)) %>%
  mutate(chars = nchar(gram)) %>%
  filter(chars > 0)
gram_tally
# TODO: TO FIX: co2, n/nitorgen p/phosphorous c/carbon, seedlingtrategy c c 3 4
# compositiondispersal

# most frequent 1st word
gram1 <- kw_summary %>%
  # filter(str_detect(kw_summary$final,".*[0-9].*")==TRUE)
  mutate(kw_id = row_number(), .before = 1) %>%
  mutate(gram = strsplit(final, " ")) %>%
  unnest(gram) %>%
  group_by(kw_id) %>%
  mutate(row = row_number()) %>%
  pivot_wider(names_from = row, values_from = gram, names_prefix = "gram_") %>%
  # spread(row, gram) %>%
  ungroup() %>%
  # filter_at(vars(gram_2:gram_11),all_vars(is.na(.)))
  count(gram_1, sort = TRUE) %>%
  rename(n_gram1 = n)
gram1


# 1st-2nd combos
gram2 <- kw_summary %>%
  # filter(str_detect(kw_summary$final,".*[0-9].*")==TRUE)
  mutate(kw_id = row_number(), .before = 1) %>%
  mutate(gram = strsplit(final, " ")) %>%
  unnest(gram) %>%
  group_by(kw_id) %>%
  mutate(row = row_number()) %>%
  pivot_wider(names_from = row, values_from = gram, names_prefix = "gram_") %>%
  # spread(row, gram) %>%
  ungroup() %>%
  # filter_at(vars(gram_3:gram_11),all_vars(is.na(.)))
  count(gram_1, gram_2, sort = TRUE) %>%
  rename(n_pairs = n)
gram2



# 1st-2nd-third combos
gram3 <- kw_summary %>%
  # filter(str_detect(kw_summary$final,".*[0-9].*")==TRUE)
  mutate(kw_id = row_number(), .before = 1) %>%
  mutate(gram = strsplit(final, " ")) %>%
  unnest(gram) %>%
  group_by(kw_id) %>%
  mutate(row = row_number()) %>%
  pivot_wider(names_from = row, values_from = gram, names_prefix = "gram_") %>%
  # spread(row, gram) %>%
  ungroup() %>%
  drop_na("gram_3") %>%
  filter(gram_3 != "") %>%
  # filter_at(vars(gram_4:gram_11),all_vars(is.na(.)))
  count(gram_1, gram_2, gram_3, sort = TRUE) %>%
  rename(n_trips = n)

gram3

combos <- left_join(gram2, gram1, by = "gram_1") %>%
  relocate(n_gram1, .after = gram_1) %>%
  relocate(n_pairs, .after = gram_2) %>%
  arrange(desc(n_gram1))

trips <- left_join(gram3, combos, by = c("gram_1", "gram_2")) %>%
  relocate(n_gram1, .after = gram_1) %>%
  relocate(n_pairs, .after = gram_2) %>%
  arrange(desc(n_gram1), desc(n_pairs), desc(n_trips))





















select(-original) %>%
  #   separate(original,c(LETTERS[seq( from = 1, to = 20 )]), sep = ";") %>%
  pivot_longer(!refID, names_to = "letter", values_to = "original") %>%
  select(-letter) %>%
  drop_na(original) %>%
  mutate(original = trimws(original)) %>%
  mutate(original = gsub("\n", " ", original)) %>% # none of these, can delete?
  # mutate(original=str_replace('\\n"', '')) %>%
  mutate(original = tolower(original)) %>%
  mutate(original = trimws(original))




# unique_kw <- unique(keywords$final_refined) %>% as_tibble()

library(countrycode)
keywords$code <- countrycode(keywords$final_refined, origin = "country.name", destination = "iso3c")
keywords <- keywords %>%
  mutate(code = case_when(
    (code == "CIV" & final != "ivory coast") ~ as.character(NA),
    (code == "AIA" & final != "anguilla") ~ as.character(NA),
    (code == "ARG" & final != "argentina") ~ as.character(NA),
    (code == "COD" & (final != "dr congo" | final != "katanga dem. rep. congo")) ~ as.character(NA),
    (code == "AIA" & final != "anguilla") ~ as.character(NA),
    (code == "BRA" & str_detect(final, "brazilian") == TRUE) ~ as.character(NA),
    (code == "AUS" & str_detect(final, "autstralian") == TRUE) ~ as.character(NA),
    final == "asclepias syriaca" ~ as.character(NA),
    final == "antiguastrea antingua" ~ as.character(NA),
    TRUE ~ as.character(code)
  ))



kw_summary <- keywords %>%
  group_by(final) %>%
  tally() %>%
  arrange(desc(n))

#
# anguilla bengalensis
# anguilla-anguilla
# anguilla-rostrata
# argentina anserina
# antiguastrea
# st. lawrence river québec canada
# williams lake british columbia canada
# cocos nucifera
# swiss long-term forest research programme lwf
# swiss stone pine
# austrocedrus chilensis
# aristotelia chilensis
# chilesius camposi
# (usa)-> , usa
# la selva, costa rica
# carpobrotus chilensis
# bathygobius cocosensis
# austrian
# dracocephalum austriacum
# dahomey gap
# bahamas mosquitofish
# bolivian
# aedes aegypti
# picea mariana
# monteverde
# bef china
# carpodacus mexicanus
# repalace united states with usa
# echinacea angustifolia
# seychelles warbler
# anopheles gambiae
# lutjanus peru
# heterandria formosa
# pteropus tonganus
# sierra madre oriental
# brazil nut
# canada goose
# gulf of mexico
# ecuadorian
# brazil nut
# brazil-nuts
# leishmania braziliensis
# yukon canada->yukon,canada
# boreal forest (yukon, canada)
# canada jays
# canada lynx
# canada warbler
# grassland.national park (canada)
# killarney provincial park ontario canada


keywords_clean_summary <- keywords %>%
  as_tibble() %>%
  filter(code != FALSE) %>%
  group_by(code, final) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  mutate(nchar = nchar(final)) %>%
  arrange(desc(nchar))
keywords_clean_summary





keywords40 <- keywords_clean_summary %>% filter(nchar > 40)
keywords5 <- keywords_clean_summary %>% filter(nchar < 5)
# keywords$final2<-str_replace(keywords$final2,"[:punct:]", " ")
#
#
#
# keywords_clean_summary<-keywords_clean_summary %>%
#   separate(final,c("final2","final3"), sep = (" [(]"),extra="merge",remove=FALSE) %>%
#
#
# keywords_clean_summary<-keywords_clean_summary %>%
#   separate(final,c("final2","final3"), sep = (" [(]"),extra="merge",remove=FALSE) %>%
#   separate(final2,c("final3","final4"), sep = ("[)]"),extra="merge",remove=FALSE)
#


# language classification -------------------------------------------------


library(textcat)
library(cld2)
# library(cld3)
# https://www.r-bloggers.com/2021/05/language-identification-using-the-fasttext-package-a-benchmark/
# keywords100 <- keywords %>% slice(1:100)
# keywords100 <- keywords100 %>%
#   mutate(
#     textcat = textcat(x = final),
#     # cld3 = cld3::detect_language(text = text),
#     cld2 = cld2::detect_language(text = final, plain_text = TRUE)
#   )

# cld2_mixed=cld2::detect_language_mixed(text = final, plain_text = FALSE))


# extract a spelling dictionary
tokens <- unnest_tokens(tbl = (keywords %>% slice(1:1000)), output = token, input = final)
wordlist <- unique(tokens$token)

# Spell check the words
spelling.errors <- hunspell(wordlist)
spelling.errors <- unique(unlist(spelling.errors))
spelling.sugg <- hunspell_suggest(spelling.errors, dict = dictionary("en_US"))

# Pick the first suggestion
spelling.sugg <- unlist(lapply(spelling.sugg, function(x) x[1]))
spelling.dict <- as.data.frame(cbind(spelling.errors, spelling.sugg))
spelling.dict$spelling.pattern <- paste0("\\b", spelling.dict$spelling.errors, "\\b")

# Write out spelling dictionary
write.csv(
  x = spelling.dict, file = "./bibliometrics/code_analysis/intermediate_data/spelling.dict.csv",
  fileEncoding = "utf8", row.names = F
)

# Parse features
tokens <- unnest_tokens(
  tbl = X, output = token,
  input = feature_response, token = stringr::str_split,
  pattern = "  |\\, |\\.|\\,|\\;"
)
tokens$token <- trimws(tokens$token,
  which = c("both", "left", "right"),
  whitespace = "[ \t\r\n]"
)

# Remove empty features
tokens <- tokens[!tokens$token == "", ]

tokens$corrected <- stri_replace_all_regex(
  str = tokens$token,
  pattern = spelling.dict$spelling.pattern,
  replacement = spelling.dict$spelling.sugg,
  vectorize_all = FALSE
)

# Rename columns
tokens <- tokens %>%
  rename(cue = cue, feature = corrected) %>%
  select(cue, feature)

# Write processed file
write.csv(
  x = tokens, file = "./bibliometrics/code_analysis/intermediate_data/spellchecked.features.csv",
  fileEncoding = "utf8", row.names = F
)


# Lemmatization and Multi-Word Sequences ----------------------------------

# Open the spell checked data
X <- read.csv("./bibliometrics/code_analysis/intermediate_data/spellchecked.features.csv", stringsAsFactors = F)

# Extract the list of updated tokens
tokens <- unnest_tokens(tbl = X, output = word, input = corrected)
cuelist <- unique(tokens$refID)

# Create a dataframe for lemmas
tokens.tagged <- data.frame(
  doc_id = character(),
  token = character(),
  wclass = character(),
  lemma = character(),
  stringsAsFactors = FALSE
)

# Loop over cues and create lemmas + POS tags
for (i in 1:length(cuelist)) {
  temp.tag <- suppressWarnings(
    suppressMessages(
      treetag(c(X$feature[X$cue == cuelist[i]], "NULL"),
        treetagger = "manual", format = "obj",
        TT.tknz = FALSE, lang = "en", doc_id = cuelist[i],
        # These parameters are based on your computer
        TT.options = list(path = "~/downloads/TreeTagger", preset = "en")
      )
    )
  )

  temp.tag <- temp.tag@TT.res %>%
    mutate_if(is.factor, as.character)

  tokens.tagged <- tokens.tagged %>%
    bind_rows(temp.tag %>%
      select(doc_id, token, wclass, lemma))
}

tokens.tagged <- tokens.tagged %>%
  rename(cue = doc_id, feature = token, pos = wclass)

# Clean up unknown lookups
tokens.tagged$lemma[tokens.tagged$lemma == "<unknown>"] <- tokens.tagged$feature[tokens.tagged$lemma == "<unknown>"]
tokens.tagged$lemma[tokens.tagged$lemma == "@card@"] <- tokens.tagged$feature[tokens.tagged$lemma == "@card@"]
tokens.tagged$lemma <- tolower(tokens.tagged$lemma)

# Write processed file
write.csv(
  x = tokens.tagged, file = "../output_data/lemmatized.features.csv",
  fileEncoding = "utf8", row.names = F
)











# spell




# keywords_clean<-keywords_clean %>%
#   mutate(final = case_when(
#   (str_detect(final,", (usa)") == TRUE) ~ ", usa",
#   TRUE ~ as.character(final)))
#   mutate(final=gsub("//(uk)",", uk",final))
#
# keywords_clean<-keywords_clean %>%
#   separate(final,c("final2","final3"),sep=",",remove=FALSE,extra="warn") %>%
#   separate(final3,c("final4","final5"),sep="\\(",remove=FALSE,extra="warn")
keywords_clean_summary <- keywords %>%
  as_tibble() %>%
  group_by(final) %>%
  summarize(n = n())
keywords_clean_summary

# -------------------- check for spelling mistakes ---------------

library(hunspell)

## find the misspelled words
# foo<-final_counts_split %>%  select(original) %>% slice(1:3000)
bad.words <- hunspell(keywords_clean_summary$final)
bad.words <- unique(unlist(bad.words))
sugg.words <- hunspell_suggest(bad.words)
sugg.words <- unlist(lapply(sugg.words, function(x) x[1]))
word.list <- as.data.frame(cbind(bad.words, sugg.words))
#
# freq.word <- count(foo, original)
# names(freq.word)
# names(word.list)
# freq.word <- inner_join(freq.word, word.list, by = c(original = "bad.words")) %>% mutate(sugg.words=tolower(sugg.words))
# freq.word <- freq.word %>% distinct(sugg.words,original) %>% mutate(unique=(sugg.words==original)) %>% filter(unique==FALSE)
# freq.word<- freq.word %>%
#   mutate(sugg.words=gsub(" ","",sugg.words))  %>%
#   mutate(unique=(sugg.words==original)) %>%
#   filter(unique==FALSE) %>%
#   mutate(sugg.words=gsub(" ","",sugg.words))  %>%
#   mutate(unique=(sugg.words==original)) %>%
#   filter(unique==FALSE)


# keywords_clean$final<-str_replace(keywords_clean$final,"species\\) diversity/biodiversity", "species diversity")
# keywords_clean$final<-str_replace(keywords_clean$final,"[:punct:]", "")
# keywords_clean$final<-str_replace(keywords_clean$final,"\\)", "")
# keywords_clean$final<-str_replace(keywords_clean$final,"\\(", "")
# keywords_clean$final<-str_replace(keywords_clean$final,"\\,", "")


keywords_clean_summary <- keywords_clean %>%
  as_tibble() %>%
  group_by(final) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
keywords_clean_summary

keywords <- keywords %>%
  separate(final, c("final2", "final3"), sep = (" [(]"), extra = "merge", remove = FALSE)
keywords <- keywords %>%
  separate(final3, c("final3", "final4"), sep = ("[)]"), extra = "merge", remove = FALSE)

# sort to get rid of abbreviations
keywords$final2 <- str_replace(keywords$final2, "[:punct:]", " ")
keywords$final2 <- str_replace(keywords$final2, "[-]", " ")




keywords <- keywords %>%
  mutate(code = case_when(
    (code == "CIV" & final != "ivory coast") ~ as.character(NA),
    (code == "AIA" & final != "anguilla") ~ as.character(NA),
    (code == "ARG" & final != "argentina") ~ as.character(NA),
    (code == "COD" & (final != "dr congo" | final != "katanga dem. rep. congo")) ~ as.character(NA),
    (code == "AIA" & final != "anguilla") ~ as.character(NA),
    (code == "BRA" & str_detect(final, "brazilian") == TRUE) ~ as.character(NA),
    (code == "AUS" & str_detect(final, "autstralian") == TRUE) ~ as.character(NA),
    final == "asclepias syriaca" ~ as.character(NA),
    final == "antiguastrea antingua" ~ as.character(NA),
    TRUE ~ as.character(code)
  ))

library(countrycode)
keywords_clean_summary
# to avoid countries being split/be consistent re names
# you can convert all to iso code

# kw100<-keywords_clean_summary
kw100 <- keywords_clean_summary %>% slice(1:5000)
library(countrycode)
kw100$code <- countrycode(kw100$final, origin = "country.name", destination = "iso3c")
kw_country <- kw100 %>%
  filter(code != FALSE) %>%
  group_by(final, code) %>%
  mutate(code = case_when(
    (code == "CIV" & final != "ivory coast") ~ as.character(NA),
    (code == "AIA" & final != "anguilla") ~ as.character(NA),
    (code == "ARG" & final != "argentina") ~ as.character(NA),
    (code == "COD" & (final != "dr congo" | final != "katanga dem. rep. congo")) ~ as.character(NA),
    (code == "AIA" & final != "anguilla") ~ as.character(NA),
    (code == "BRA" & str_detect(final, "brazilian") == TRUE) ~ as.character(NA),
    (code == "AUS" & str_detect(final, "autstralian") == TRUE) ~ as.character(NA),
    final == "asclepias syriaca" ~ as.character(NA),
    final == "antiguastrea antingua" ~ as.character(NA),
    TRUE ~ as.character(code)
  ))
# delete "western "
# delete "eastern "
# delete "northern "
# delete "southern "
kw100$code <- countrycode(kw100$final, origin = "country.name", destination = "iso3c")

keywords_clean$code <- countrycode(keywords_clean$final, origin = "country.name", destination = "iso3c")
keywords_clean <- keywords_clean %>%
  mutate(code = case_when(
    (code == "CIV" & final != "ivory coast") ~ as.character(NA),
    (code == "AIA" & final != "anguilla") ~ as.character(NA),
    (code == "ARG" & final != "argentina") ~ as.character(NA),
    (code == "COD" & (final != "dr congo" | final != "katanga dem. rep. congo")) ~ as.character(NA),
    (code == "AIA" & final != "anguilla") ~ as.character(NA),
    (code == "BRA" & str_detect(final, "brazilian") == TRUE) ~ as.character(NA),
    (code == "AUS" & str_detect(final, "australian") == TRUE) ~ as.character(NA),
    final == "asclepias syriaca" ~ as.character(NA),
    final == "antiguastrea antingua" ~ as.character(NA),
    TRUE ~ as.character(code)
  ))


# anguilla bengalensis
# anguilla-anguilla
# anguilla-rostrata
# argentina anserina
# antiguastrea
# st. lawrence river québec canada
# williams lake british columbia canada
# cocos nucifera
# swiss long-term forest research programme lwf
# swiss stone pine
# austrocedrus chilensis
# aristotelia chilensis
# chilesius camposi
# (usa)-> , usa
# la selva, costa rica
# carpobrotus chilensis
# bathygobius cocosensis
# austrian
# dracocephalum austriacum
# dahomey gap
# bahamas mosquitofish
# bolivian
# ecuadorian
# brazil nut
# brazil-nuts
# leishmania braziliensis
# yukon canada->yukon,canada
# boreal forest (yukon, canada)
# canada jays
# canada lynx
# canada warbler
# grassland.national park (canada)
# killarney provincial park ontario canada


keywords_clean_summary <- keywords %>%
  as_tibble() %>%
  group_by(final2) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  filter(is.na(code) == FALSE)
keywords_clean_summary





# lemmatization -----------------------------------------------------------
# install.packages('udpipe')
# https://bnosac.github.io/udpipe/en/
# library(udpipe)
# x <- c(doc_a = "In our last meeting, someone said that we are meeting again tomorrow",
#        doc_b = "It's better to be good at being the best")
# anno <- udpipe(x, "english")
# anno[, c("doc_id", "sentence_id", "token", "lemma", "upos")]


# prior to lemmatizing, there are some that might be worth excluding
# taxonomic:
# ii
# orum
# ae
# arum
# aceae
# oideae
# oides
# rales
# ormes


keywords_clean_summary <- keywords %>%
  as_tibble() %>%
  group_by(final2) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
keywords_clean_summary


library(udpipe)
x <- kw_summary %>%
  select(final) %>%
  mutate(id = row_number()) %>%
  slice(1:3000) %>%
  pull(final)


# x<-keywords_clean_summary %>% select(final)
# x<-unlist(x)
anno <- udpipe(x, "english")
anno$unique <- (anno$token == anno$lemma)
summary(anno$unique)
lemmas <- anno[, c("doc_id", "sentence_id", "token", "lemma", "upos", "unique")]
lemmas <- lemmas %>% arrange(unique)
lemmas

false_checks <- lemmas %>%
  filter(unique == FALSE) %>%
  group_by(token, lemma) %>%
  tally() %>%
  mutate(diff = (nchar(token) - nchar(lemma))) %>%
  arrange(desc(diff))

lemmas_summary <- lemmas %>%
  group_by(lemma, token) %>%
  tally() %>%
  arrange(desc(n)) %>%
  filter(n > 100)


lemmas_summary <- lemmas %>%
  group_by(lemma, token) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(lemma) %>%
  tally() %>%
  arrange(desc(n)) %>%
  filter(n > 1)

# kw comb freq stats: https://bnosac.github.io/udpipe/docs/doc5.html
library(udpipe)
x <- keywords_clean_summary %>%
  select(final) %>%
  mutate(id = row_number())
# %>%
#   slice(1:10000)
# data(brussels_reviews)
# comments <- subset(brussels_reviews, language %in% "es")
# ud_model <- udpipe_download_model(language = "spanish")
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)
x <- udpipe_annotate(ud_model, x = x$final, doc_id = x$id)
x <- as.data.frame(x)
stats <- keywords_rake(
  x = x, term = "lemma", group = "doc_id",
  relevant = x$upos %in% c("NOUN", "ADJ")
)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
library(lattice)
barchart(key ~ rake,
  data = head(subset(stats, freq > 3), 20), col = "cadetblue",
  main = "Keywords identified by RAKE",
  xlab = "Rake"
)


# most ocurring noun
stats <- subset(x, upos %in% c("NOUN"))
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq,
  data = head(stats, 20), col = "cadetblue",
  main = "Most occurring nouns", xlab = "Freq"
)

## most ocurring ADJECTIVES
stats <- subset(x, upos %in% c("ADJ"))
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq,
  data = head(stats, 20), col = "cadetblue",
  main = "Most occurring adjectives", xlab = "Freq"
)

# Nouns / adjectives used in same sentence
cooc <- cooccurrence(
  x = subset(x, upos %in% c("NOUN", "ADJ")),
  term = "lemma",
  # group = c("doc_id", "paragraph_id", "sentence_id"))
  group = c("doc_id")
)
head(cooc, 20)


# Nouns / adjectives which follow one another
cooc <- cooccurrence(x$lemma, relevant = x$upos %in% c("NOUN", "ADJ"), skipgram = 1)
head(cooc)



# TODO: correct
# seed->see
# seedling->seed
# seedseedling->seedseedl
# tree seedling->treeseedling
# wren->w
#



# Tutorial for knowledge classification from raw text
# Huang Tian-Yuan (Hope)
# This tutorial gives an example of how to use akc package to carry out automatic knowledge classification based on raw text.
# https://cran.rstudio.com/web/packages/akc/vignettes/tutorial_raw_text.html

# outdated lemmatization practice -----------------------------------------


# # sample<-c("I like apple, pear, pears, and bannana.")
# #
# #   (keywords_clean$final)
# ##############
# # https://stackoverflow.com/questions/28214148/how-to-perform-lemmatization-in-r
# library(koRpus)
# library(koRpus.lang.en)
# # ~/Dropbox (UFL)/Talks/atbc2022_plenary_talk/bibliometrics/code_analysis/tree_tagger
# # sh /bibliometrics/code_analysis/tree_tagger/install-tagger.sh
# # cd /atbc2022_plenary_talk/bibliometrics/code_analysis/tree_tagger/install-tagger.sh
# # cd /bibliometrics/code_analysis/tree_tagger/install-tagger.sh
# # ./code_analysis/tree_tagger/install-tagger.sh
# tagged.results <- treetag(c("run", "ran", "running"), treetagger="manual", format="obj",
#                           TT.tknz=FALSE , lang="en",
#                           TT.options=list(path="./TreeTagger", preset="en"))
# tagged.results@TT.res
#
# system("./atbc2022_plenary_talk/bibliometrics/code_analysis/tree_tagger/install-tagger.sh")
# tagged.text <- tokenize("./bibliometrics/code_analysis/sample_text.txt", lang="en")
# str(describe(tagged.text))
# describe(tagged.text)[["lttr.distrib"]]
# freq.analysis.res <- freq.analysis(tagged.text, corp.freq=LCC.en)
# taggedText(freq.analysis.res)
#
#
# OR
#
# # https://cran.r-project.org/web/packages/textstem/readme/README.html
#
# library(tm)
# library(lexicon)
# library(wordcloud)
# library(textstem)
#
# sample<-c("I like apple, pear, pears, and bannana.")
#
# (keywords_clean$final)
# final <- keywords_clean[['final']]
# sample<-keywords_clean_summary %>% select(final) %>% slice(1:100)
# sample<-sample[['final']]
# sample<-(sample)
# a<-sapply(c("operates", "operating", "operation", "operational", "operator", "operators", "operative", "operatives"), lemmatize_words)
# sapply(sample, lemmatize_words)
#
# crCorpus_lem <- tm_map(sample, lemmatize_strings)
# inspect(crCorpus_lem[[1]])
# https://rpubs.com/chelseyhill/669117
##############
























# (generalized) linear model
keywords_clean <- keywords_clean %>% as_tibble()
keywords_clean$final <- str_trim(keywords_clean$final)
final_counts <- keywords_clean %>%
  mutate_all(str_trim) %>%
  select(final) %>%
  mutate(final = trimws(final)) %>%
  group_by(final) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  keywords$final <- str_replace(keywords$final, "-", " ")
mutate(to = strsplit(final, " ")) %>%
  unnest(to) %>%
  group_by(final) %>%
  mutate(row = row_number()) %>%
  spread(row, to) %>%
  ungroup() %>%
  select(-n) %>%
  pivot_longer(!final, names_to = "letter", values_to = "original") %>%
  select(-letter) %>%
  drop_na(original) %>%
  mutate(original = gsub("\n", " ", original)) %>%
  # mutate(original=str_replace('\\n"', '')) %>%
  mutate(original = tolower(original)) %>%
  mutate(original = trimws(original))

final_counts$original <- str_replace(final_counts$original, "\\,", "")
# library(textclean)
# final_counts<-final_counts %>%
#   replace_non_ascii(original, replacement = "", remove.nonconverted = TRUE)

final_counts_40 <- final_counts %>%
  group_by(original) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  filter(n > 40)

# final_counts$last_letter<-str_sub(final_counts$original,-1,-1)
# final_counts_pl <- final_counts %>% filter(last_letter=="s")
# final_counts<-final_counts %>% arrange(original,desc(n))


library(SnowballC)
prof.tm <- mutate(final_counts, word.stem = wordStem(original, language = "en"))
prof.tm <- prof.tm %>%
  mutate(stem_chk = original == word.stem) %>%
  filter(stem_chk == FALSE)



library(RecordLinkage)
prof.tm$word_sim <- levenshteinSim(prof.tm$original, prof.tm$word.stem)
prof.tm <- prof.tm %>%
  arrange(desc(word_sim)) %>%
  select(-stem_chk)
prof.tm
# https://books.psychstat.org/textmining/data.html
library(hunspell)

## find the misspelled words
# foo<-final_counts_split %>%  select(original) %>% slice(1:3000)
bad.words <- hunspell(final_counts$original)
bad.words <- unique(unlist(bad.words))
sugg.words <- hunspell_suggest(bad.words)
sugg.words <- unlist(lapply(sugg.words, function(x) x[1]))
word.list <- as.data.frame(cbind(bad.words, sugg.words))

freq.word <- count(foo, original)
names(freq.word)
names(word.list)
freq.word <- inner_join(freq.word, word.list, by = c(original = "bad.words")) %>% mutate(sugg.words = tolower(sugg.words))
freq.word <- freq.word %>%
  distinct(sugg.words, original) %>%
  mutate(unique = (sugg.words == original)) %>%
  filter(unique == FALSE)
freq.word <- freq.word %>%
  mutate(sugg.words = gsub(" ", "", sugg.words)) %>%
  mutate(unique = (sugg.words == original)) %>%
  filter(unique == FALSE) %>%
  mutate(sugg.words = gsub(" ", "", sugg.words)) %>%
  mutate(unique = (sugg.words == original)) %>%
  filter(unique == FALSE)

library(RecordLinkage)
freq.word$word_sim <- levenshteinSim(freq.word$original, freq.word$sugg.words)
freq.word <- freq.word %>% arrange(desc(word_sim))
write_csv(freq.word, "./bibliometrics/data_intermediate/freq.word.csv")

# library("SemNetCleaner")
foo <- final_counts_split %>%
  select(original) %>%
  slice(1:3000)
clean <- textcleaner(
  data = foo, miss = NA,
  partBY = "row", dictionary = "general"
)


final_counts_split <- final_counts %>%
  group_by(original) %>%
  summarize(n = n()) %>%
  arrange(desc(n))


clean <- textcleaner(
  data = open.animals[, -c(1:2)], miss = 99,
  partBY = "row", dictionary = "animals"
)

final_counts %>%
  slice(1:300) %>%
  mutate(final_split = stringr::str_extract_all(
    final_counts$final,
    "(trait)|(forest)|(mangrove)"
  )) %>%
  tidyr::unnest(cols = c(focal_words))

fruits <- c("trait", "forest", "mangrove")

final_counts$final2 <- sapply(stringr::str_extract_all(
  final_counts$final,
  sprintf("(%s)", paste0(fruits, collapse = "|"))
), toString)

rows1 <- floor(nrow(final_counts) / 3)
rows2 <- rows1 * 2
rows3 <- rows1 * 3
cola <- final_counts %>% slice(1:rows1)
colb <- final_counts %>% slice((rows1 + 1):(rows2))
colc <- final_counts %>% slice((rows2 + 1):(rows3))
final_counts <- bind_cols(cola, colb, colc)
write_csv(final_counts, "./bibliometrics/data_intermediate/final_counts.csv")
# rm(keywords_unique)



keywords_unique <- keywords %>%
  select(final) %>%
  unique() %>%
  arrange(final)


write_csv(keywords_unique, "./bibliometrics/data_intermediate/keywords_unique.csv")
# rm(keywords_unique)



kw_similarity <- Name.check(keywords_unique)
Name.check <- function(DataToClean) {
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

  # Calclulate the proportional similarity and # changes required to go from one name to another. Package RecordLinkage
  NamesDF$Name_sim <- levenshteinSim(NamesDF$Name1, NamesDF$Name2)
  NamesDF$Name_dist <- levenshteinDist(NamesDF$Name1, NamesDF$Name2)

  # Because this does all pairwise comparisons, it results in redundancy: "e bruna vs emilio bruna" and "emilio bruna vs e bruna"
  # are in different rows, even though they are the same "comparison". This deletes one of the two
  NamesDF <- NamesDF[!duplicated(t(apply(NamesDF, 1, sort))), ]
  # this arranges them in order from most similar (1 change required) to least similar.
  # look carefully at those with a few changes, as they are likely to be a tiny spelling mistake or difference in intials


  NamesDF$index <- seq.int(nrow(NamesDF)) # adds a column with an index to make it easier to id which row you need'
  NamesDF <- NamesDF %>% select(index, Name1, Name2, Name_sim, Name_dist) # It's kinda ugly, but this rearranges columns (and dumps the "FALSE")
  NamesDF <- arrange(NamesDF, desc(Name_sim))
  # head(NamesDF)
  write_csv(NamesDF, file = "./bibliometrics/data_intermediate/kw_similarity.csv") # export it as a csv file


  return(NamesDF)
}


keywords <- keywords %>%
  # mutate(final=gsub("agroforest","agroforest",final)) %>%
  mutate(article_cat = case_when(
    str_detect(final, "tropics") == TRUE ~ "TRUE",
    str_detect(final, "tropical") == TRUE ~ "TRUE",
    str_detect(final, "bci") == TRUE ~ "TRUE",
    str_detect(final, "pasoh") == TRUE ~ "TRUE",
    str_detect(final, "la selva") == TRUE ~ "TRUE",
    str_detect(final, "ots-oet") == TRUE ~ "TRUE",
    str_detect(final, "bdffp") == TRUE ~ "TRUE",
    str_detect(final, "manu national park") == TRUE ~ "TRUE",
    str_detect(final, "cocha cashu") == TRUE ~ "TRUE",
    str_detect(final, "amazon") == TRUE ~ "TRUE",
    str_detect(final, "bci") == TRUE ~ "TRUE",
    # str_detect(final,"tropic")==TRUE~"TRUE",
    str_detect(final, "afrotrop") == TRUE ~ "TRUE",
    str_detect(final, "rain forest") == TRUE ~ "TRUE",
    str_detect(final, "dry forest") == TRUE ~ "TRUE",
    str_detect(final, "la selva") == TRUE ~ "TRUE",
    str_detect(final, "yasuni") == TRUE ~ "TRUE",
    TRUE ~ as.character("FALSE")
  )) %>%
  arrange(desc(article_cat))

names(keywords)
forest_kw <- keywords_unique %>%
  filter(str_detect(final, "forest") == TRUE)


# pooling keywords  -------------------------------------------------------


names(keywords)
forest_kw <- keywords_unique %>%
  filter(str_detect(final, "forest") == TRUE)

rain_forest_kw <- forest_kw %>%
  filter(
    (
      str_detect(final, "wet") |
        str_detect(final, "rain") |
        str_detect(final, "african forest") |
        str_detect(final, "rain")
    )
  ) %>%
  filter(str_detect(final, "forest drainage") == FALSE) %>%
  filter(str_detect(final, "forested wetlands") == FALSE) %>%
  filter(str_detect(final, "temperate rainforest") == FALSE) %>%
  filter(str_detect(final, "eucalyptus forest") == FALSE) %>%
  filter(str_detect(final, "subtropical") == FALSE) %>%
  filter(str_detect(final, "temperate") == FALSE) %>%
  filter(str_detect(final, "hemiepiphytes lianas pioneers tree growth tropical wet forest") == FALSE) %>%
  filter(str_detect(final, "litter litter nutrient content primary forest tropical rainforest") == FALSE)



temp_rainforest_kw <- forest_kw %>%
  filter(
    (
      str_detect(final, "temperate rainforest") |
        str_detect(final, "temperate rainforest")
      # str_detect(final,"temperate")
    )
  )


# includes temp rainforest
temp_forest_kw <- forest_kw %>%
  filter(
    (
      str_detect(final, "temperate")
    )
  )


subtrop_forest_kw <- forest_kw %>%
  filter(
    (
      str_detect(final, "subtropical")
    )
  )

dry_forest_kw <- forest_kw %>%
  filter(str_detect(final, "dry") == TRUE)

boreal_forest_kw <- forest_kw %>%
  filter(str_detect(final, "boreal") == TRUE)


savanna_kw <- keywords_unique %>%
  filter(str_detect(final, "savanna") == TRUE) %>%
  filter(str_detect(final, "autccology bolivia cerrado gramineae kranz anatomy neotropics savanna") == FALSE) %>%
  filter(str_detect(final, "brazil fire fleshy fruit frugivory fruit shortage reproduction savanna vertebrates") == FALSE) %>%
  filter(str_detect(final, "autccology bolivia cerrado gramineae kranz anatomy neotropics savanna") == FALSE) %>%
  filter(str_detect(final, "facilitation, frailty model, hierarchical model, indirect effect") == FALSE) %>%
  filter(str_detect(final, "cerrado savanna") == FALSE) %>%
  # filter(str_detect(final,"brazilian savanna")==FALSE) %>%
  # filter(str_detect(final,"bolivian savanna")==FALSE) %>%
  filter(str_detect(final, "ivoire grass hyparrhenia diplandra. nitrogen use efficien") == FALSE)

cerrado_kw <- keywords_unique %>%
  filter(str_detect(final, "cerrado") == TRUE) %>%
  filter(str_detect(final, "carvocaraceae cerrado") == FALSE) %>%
  filter(str_detect(final, "aconophora teligera ant araliaceae brazil cerrado didymo") == FALSE) %>%
  filter(str_detect(final, "amazonia-cerrado transition") == FALSE) %>%
  filter(str_detect(final, "autccology bolivia cerrado gramineae kranz anatomy neotropics savanna") == FALSE)



grassland_kw <- keywords_unique %>%
  filter(str_detect(final, "grassland") == TRUE)


usa_kw <- keywords_unique %>%
  filter(str_detect(final, "(usa)") == TRUE) %>%
  # filter(str_detect(final,"usa")==TRUE) %>%
  # filter(str_detect(final,"brazil fire fleshy fruit frugivory fruit shortage reproduction savanna vertebrates")==FALSE) %>%
  # filter(str_detect(final,"autccology bolivia cerrado gramineae kranz anatomy neotropics savanna")==FALSE) %>%
  # filter(str_detect(final,"facilitation, frailty model, hierarchical model, indirect effect")==FALSE) %>%
  # filter(str_detect(final,"cerrado savanna")==FALSE) %>%
  # # filter(str_detect(final,"brazilian savanna")==FALSE) %>%
  # filter(str_detect(final,"bolivian savanna")==FALSE) %>%
  filter(str_detect(final, "usa") == TRUE)


# word association
# https://uc-r.github.io/word_relationships
txt_df <- grassland_kw %>%
  mutate(line = nrow(grassland_kw)) %>%
  relocate(line, .before = 1) %>%
  rename(text = final)

foo <- txt_df %>% unnest_tokens(word, text)
# install.packages("widyr")
library(widyr)
(word_pairs <- foo %>%
  pairwise_count(word, line, sort = TRUE))



(word_cor <- foo %>%
  arrange(line) %>%
  group_by(line) %>%
  filter(n() >= 3) %>%
  pairwise_cor(word, line) %>%
  filter(!is.na(correlation)))






# %>%
#   filter(str_detect(final,"carvocaraceae cerrado")==FALSE) %>%
#   filter(str_detect(final,"aconophora teligera ant araliaceae brazil cerrado didymo")==FALSE) %>%
#   filter(str_detect(final,"amazonia-cerrado transition")==FALSE) %>%
#   filter(str_detect(final,"autccology bolivia cerrado gramineae kranz anatomy neotropics savanna")==FALSE)


# library(SemNetCleaner)
# singularize("grasslands")


# keywords<-keywords %>%
#   mutate(kw_major_cat = case_when(
#     str_detect(final,"dry")==TRUE~"TRUE",






















foo <- keywords %>%
  group_by(article_cat, final) %>%
  summarize(n = n()) %>%
  arrange(desc(n))

# https://cran.r-project.org/web/packages/stringdist/stringdist.pdf

library(stringdist)

parents_name <- keywords_unique %>% slice(1:200)
parents_name <- parents_name$final

person_id <- 1:length(parents_name)

family_id <- vector("integer", length(parents_name))


# Looping through unassigned family ids
while (sum(family_id == 0) > 0) {
  ids <- person_id[family_id == 0]

  dists <- stringdist(parents_name[family_id == 0][1],
    parents_name[family_id == 0],
    method = "lv"
  )

  matches <- ids[dists <= 2]

  family_id[matches] <- max(family_id) + 1
}

result <- data.frame(person_id, parents_name, family_id)

result <- result %>%
  arrange(family_id) %>%
  group_by(family_id) %>%
  mutate(n = n()) %>%
  arrange(desc(n))
result

# abiotic
# biotic
# abundance/range-size relationship
# acer rubrum, betula alleghaniensis, betula papyrifera
# anagrus spp
# anas spp.
# anagrus spp.
# análise de isótopos estáveis
# alnus-crispa
# alnus-rubra
# to find the "tropical" stirngs search ab, ti, de, and other
# https://www.statology.org/r-check-if-column-contains-string/
# https://www.statology.org/str_detect-in-r/


# save these and then consolidate in openrefine
# write_csv(kw,"./bibliometrics/data_raw/kw_27june_to_refine.csv")

# pool kw using open_refine ---------------------------------------------------

# CLEANED IN OPEN REFINE. EXPORT THE REFINED DATASET
# OPEN REFINE PROHECT 2475241490253
# kw_refined<-read_csv("./bibliometrics/keyword_analysis/kw-27june-to-refine-csv.csv")
#
# kw_refined<-read_csv("./bibliometrics/keyword_analysis/kw-27june-to-refine-csv.csv") %>%
#   mutate(kw = case_when(
#     kw ==  "ant plant interactions" ~ "ant-plant interactions",
#     kw ==  "tropical forests" ~ "tropical forest",
#     kw ==  "germination" ~ "seed germination",
#     kw ==  "litter" ~ "litterfall",
#     kw ==  "tropical" ~ "tropics/tropical",
#     kw ==  "tropics" ~ "tropics/tropical",
#     kw ==  "ant plant interactions" ~ "ant-plant interactions",
#     kw ==  "tropical forests" ~ "tropical forest",
#     kw ==  "germination" ~ "seed germination",
#     kw ==  "litter" ~ "litterfall",
#     kw ==  "tropical" ~ "tropics/tropical",
#     kw ==  "tropics" ~ "tropics/tropical",
#     kw ==  "tropical forestsuccession" ~ "tropical forest succession/regeneration",
#     kw ==  "tropical forest succession" ~ "tropical forest succession/regeneration",
#     kw ==  "tropical forest regeneration" ~ "tropical forest succession/regeneration",
#     kw ==  "tropical forests" ~ "tropical forest(s)",
#     kw ==  "tropical forest" ~ "tropical forest(s)",
#     kw ==  "tropical forest fragmentation" ~ "tropical forest fragments/fragmentation",
#     kw ==  "tropical forest fragments" ~ "tropical forest fragments/fragmentation",
#     kw ==  "tropical forest fragment" ~"tropical forest fragments/fragmentation",
#     kw ==  "tropical forest manage-" ~ "tropical forest management",
#     kw ==  "invertebrate herbivory" ~ "invertebrate herbivory/herbivore(s)",
#     kw ==  "invertebrate herbivores" ~ "invertebrate herbivory/herbivore(s)",
#     kw ==  "invertebrate community structure" ~ "invertebrate communities/structure",
#     kw ==  "invertebrate communities" ~ "invertebrate communities/structure",
#     kw ==  "invertebrate predators" ~ " invertebrate predator(s)",
#     kw ==  "invertebrate predator" ~ " invertebrate predator(s)",
#     kw ==  "seed dispersal networks" ~ "seed dispersal network(s)",
#     kw ==  "seed dispersal network" ~ "seed dispersal network(s)",
#     kw ==  "seed dispersal kernels" ~ "seed dispersal kernel(s)",
#     kw ==  "seed dispersal kernel" ~ "seed dispersal kernel(s)",
#     kw ==  "seed-dispersal mutualism" ~ "seed dispersal mutualism(s)",
#     kw ==  "seed dispersal mutualisms" ~ "seed dispersal mutualism(s)",
#     kw ==  "drosophila" ~ "drosophila / d melanogaster",
#     kw ==  "drosophila melanogaster" ~ "drosophila / d melanogaster",
#     kw ==  "dispersal modes" ~ "dispersal mode(s)",
#     kw ==  "dispersal mode" ~ "dispersal mode(s)",
#     kw ==  "dispersal models" ~ "dispersal model(s)",
#     kw ==  "dispersal model" ~ "dispersal model(s)",
#     kw ==  "foraging mode" ~ "foraging mode(s)",
#     kw ==  "foraging models" ~ "foraging model(s)",
#     kw ==  "foraging model" ~ "foraging model(s)",
#     kw ==  "drosophila melanogaster" ~ "drosophila / d melanogaster",
#     kw ==  "cerraddo" ~ "cerrado",
#     kw ==  "path analyses" ~ "path analysis",
#     kw ==  "feather" ~ "feather(s)",
#     kw ==  "feathers" ~ "feather(s)",
#     kw ==  "p ratio" ~ "P ratio(s)",
#     kw ==  "p ratios" ~ "P ratio(s)",
#     kw ==  "x choromosomes" ~ "x choromosomes",
#     TRUE ~ as.character(kw))) %>%
#   mutate(kw = case_when(
#     kw ==  "rain forest" ~ "(tropical) rain forest(s)",
#     kw ==  "diversity" ~ "(species) diversity/biodiversity",
#     kw ==  "species diversity" ~ "(species) diversity/biodiversity",
#     kw ==  "biodiversity" ~ "(species) diversity/biodiversity",
#     kw ==  "tropical rainforest"~"(tropical) rain forest(s)",
#     kw ==  "redundancy analysis (rda)" ~ "redundancy analysis",
#     kw ==  "plant community" ~ "plant communities",
#     kw ==  "pinus plantations"~"pine plantation(s)",
#     kw ==  "pinus plantation"~"pine plantation(s)",
#     kw ==  "lowland tropical forest" ~ "lowland tropical rain forest",
#     kw ==  "vapour pressure deficits" ~ "vapor pressure deficit",
#     kw ==  "megafaunal-dispersal syndrome" ~ "megafaunal dispersal syndrome",
#     kw ==  "amazon" ~ "amazon(ia)",
#     kw ==  "land use" ~ "land-use",
#     kw ==  "ant" ~ "ant(s)",
#     kw ==  "ants" ~ "ant(s)",
#     kw ==  "mammal" ~ "mammalia",
#     kw ==  "mammals" ~ "mammalia",
#     kw ==  "oil-palm" ~ "oil palm",
#     kw ==  "fig" ~ "fig(s)",
#     kw ==  "rodent" ~ "rodentia",
#     kw ==  "bambusoideae" ~ "bambuseae",
#     kw ==  "relative growth" ~ "relative growth (rate)",
#     kw ==  "relative growth rate" ~ "relative growth (rate)",
#     kw ==  "tropical montane cloud forest"~"tropical montane forest",
#     kw ==  "reduced impact logging" ~ "reduced-impact logging",
#     kw ==  "afrotropical" ~ "afrotropics",
#     kw ==  "insectivores" ~ "insectivores/insectivory",
#     kw ==  "insectivory" ~ "insectivores/insectivory",
#     kw ==  "land use history" ~ "land-use history",
#     kw ==  "bird community" ~ "bird communities",
#     kw ==  "agriculture intensification" ~ "agricultural intensification",
#     kw ==  "camera trapping" ~ "camera trap(ping)",
#     kw ==  "camera trap" ~ "camera trap(ping)",
#     kw ==  "leaf litterfall" ~ "leaf litter",
#     kw ==  "liana-tree interaction" ~ "liana-tree interaction (network)",
#     kw ==  "liana-tree interaction network" ~ "liana-tree interaction (network)",
#     kw ==  "canopy openness" ~ "canopy openness/openings",
#     kw ==  "canopy openings" ~ "canopy openness/openings",
#     kw ==  "insect-plant interactions" ~ "plant-insect interaction",
#     kw ==  "thermal performance" ~ "thermal performance (curves)",
#     kw ==  "thermal performance curves" ~ "thermal performance (curves)",
#     kw ==  "atlantic rain forest biome" ~ "atlantic rain forest",
#     kw ==  "atlantic rainforest" ~ "atlantic rain forest",
#     kw ==  "arboreal" ~ "arboreal/arboreality",
#     kw ==  "arboreality" ~ "arboreal/arboreality",
#     kw ==  "resprout" ~ "resprout(ing)",
#     kw ==  "resprouting" ~ "resprout(ing)",
#     kw ==  "land use change" ~ "land-use change",
#     kw ==  "forest canopies" ~ "forest canopy",
#     kw ==  "tropical mountain forests"~"tropical montane forest",
#     kw ==  "decomposition" ~ "decomposition rate",
#     kw ==  "albertine rift eco-region"~"albertine rift region",
#     kw ==  "climatic change" ~ "climate change",
#     kw ==  "neotropical" ~ "neotropics",
#     kw ==  "psittacidae" ~ "psittacids",
#     kw ==  "psittacines" ~ "psittacids",
#     kw ==  "pan troglodytes verus" ~ "pan troglodytes",
#     kw ==  "anura" ~ "anurans",
#     kw ==  "la selva biological research station" ~ "la selva biological station",
#     kw ==  "animal-plant interaction"~"plant-animal interactions",
#     kw ==  "long distance dispersal" ~ "long-distance dispersal",
#     kw ==  "twig-nesting ant species" ~ "twig-nesting ants",
#     kw ==  "tropical mountain cloud forest" ~ "tropical montane cloud forest",
#     kw ==  "life histories" ~ "life history",
#     kw ==  "seasonally dry tropical forest" ~ "tropical dry forest",
#     kw ==  "seasonal dry tropical forest" ~ "tropical dry forest",
#     kw ==  "dry-season flushing" ~ "dry season flushing",
#     kw ==  "photosynthesis rates" ~ "photosynthesis (rates)",
#     kw ==  "photosynthesis" ~ "photosynthesis (rates)",
#     kw ==  "janzen-connell model" ~ "janzen-connell",
#     kw ==  "termitidae" ~ "termite",
#     kw ==  "carbon dioxide (co2)" ~ "carbon dioxide",
#     kw ==  "tropical montane" ~ "tropical montane forest",
#     kw ==  "tropical lowland forests" ~ "tropical lowland rain forest(s)",
#     kw ==  "atlantic rain forest biome"~"atlantic rain forest",
#     kw ==  "symbiotic microbiota" ~ "symbiotic microbes",
#     kw ==  "caribbean sea" ~ "caribbean",
#     kw ==  "post-dispersal predation" ~ "postdispersal seed predation",
#     kw ==  "phyllostomid bats" ~ "phyllostomidae",
#     kw ==  "life table response experiment" ~ "life table response experiments",
#     kw ==  "tropical rainforest" ~ "tropical rain forest(s)",
#     TRUE ~ as.character(kw))) %>%
#   mutate(kw=tolower(kw)) %>%
#   mutate(kw=gsub("\"","",kw)) %>%
#   mutate(kw=gsub("\'","",kw)) %>%
#   mutate(kw=trimws(kw)) %>%
#   arrange(kw)

# open refine project2040771042669
# kw_refined2<-read_csv("./bibliometrics/keyword_analysis/kw28june-csv.csv") %>%
#   mutate(kw_to_refine=gsub("above-ground","aboveground",kw_to_refine)) %>%
#   mutate(kw_to_refine=gsub("below-ground","belowground",kw_to_refine)) %>%
#   mutate(kw_to_refine=gsub("below- ","belowground",kw_to_refine)) %>%
#   mutate(kw_to_refine=gsub("above- ","aboveground",kw_to_refine)) %>%
#   mutate(kw_to_refine = case_when(
#     kw_to_refine ==  "b matrix" ~ "b-matrix",
#     kw_to_refine ==  "type 1 error"~"type-1 error",
#     kw_to_refine ==  "type i error"~"type-1 error",
#     kw_to_refine ==  "c : p ratio"~"cp ratio",
#     kw_to_refine ==  "c : p ratios"~"cp ratio",
#     kw_to_refine ==  "k : p ratio"~"kp ratio",
#     kw_to_refine ==  "k : p ratios"~"kp ratio",
#     kw_to_refine ==  "n : k ratio"~"nk ratio",
#     kw_to_refine ==  "n : k ratios"~"nk ratio",
#     kw_to_refine ==  "n : p ratios"~"np ratios",
#     kw_to_refine ==  "g matrix"~"g-matrix",
#     kw_to_refine ==  "b matrix"~"b-matrix",
#     kw_to_refine ==  "m matrix"~"m-matrix",
#     kw_to_refine ==  "p matrix"~"p-matrix",
#     kw_to_refine ==  "barro colorado island"~"BCI",
#     kw_to_refine ==  "barro colorado-island"~"BCI",
#     kw_to_refine ==  "barro-colorado island"~"BCI",
#     kw_to_refine ==  "burro colorado island"~"BCI",
#     kw_to_refine ==  "site dependence"~"site-dependence",
#     kw_to_refine ==  "site-dependence"~"site-dependence",
#     kw_to_refine ==  "site-dependency"~"site-dependence",
#     kw_to_refine ==  "b-chromosomes"~"b-chromosome",
#     kw_to_refine ==  "f statistics"~"f-statistics",
#     kw_to_refine ==  "np ratio"~"np ratio(s)",
#     kw_to_refine ==  "np ratios"~"np ratio(s)",
#     kw_to_refine ==  "n limitation"~"n-limitation",
#     kw_to_refine ==  "rapid biodiversity assessment protocol"~"rapid biodiversity assessment",
#     kw_to_refine ==  "noninvasive sample"~"noninvasive sample/sampling",
#     kw_to_refine ==  "noninvasive sampling"~"noninvasive sample/sampling",
#     kw_to_refine ==  "road"~"roads",
#     kw_to_refine ==  "varillales"~"varillal",
#     kw_to_refine ==  "palm"~"palm(s)",
#     kw_to_refine ==  "palms"~"palm(s)",
#     kw_to_refine ==  "bird"~"bird(s)",
#     kw_to_refine ==  "birds"~"bird(s)",
#     kw_to_refine ==  "abiotic&#8208"~"abiotic",
#     kw_to_refine == "abundant centre model"~"abundant center model",
#     kw_to_refine == "barley and cereal yellow dwarf virus"~"barley and cereal yellow dwarf viruses",
#     kw_to_refine == "acer opalus ssp granatense"~"acer opalus",
#     kw_to_refine == "acer opalus subsp granatense"~"acer opalus",
#     kw_to_refine == "australian monsoon tropics"~"australian monsoonal tropics",
#     kw_to_refine == "adaptation and trade-off"~"adaptations and trade-offs",
#     kw_to_refine == "biodiversity and ecosystem functioning"~"biodiversity and ecosystem function",
#     kw_to_refine == "alternative stable state"~"alternate stable state",
#     kw_to_refine == "anas plathyrynchos"~"anas platyrhynchos",
#     kw_to_refine == "asymmetric competition"~"asymmetrical competition",
#     kw_to_refine == "binomial mixture model"~"binomial n-mixture model",
#     kw_to_refine == "barley yellow dwarf virus (bydv)"~"barley and cereal yellow dwarf viruses",
#     kw_to_refine == "barley yellow dwarf viruses (bydvs)"~"barley and cereal yellow dwarf viruses",
#     kw_to_refine == "arbuscular mycorrhiza"~"arbuscular myccorrhyiza",
#     kw_to_refine == "aguoti paca"~"agouti paca",
#     kw_to_refine == "anti-predator behavior"~"antipredatory behavior",
#     kw_to_refine == "below-ground process"~"below-ground processes",
#     kw_to_refine == "behavioural tradeoff"~"behavioral trade-off",
#     kw_to_refine == "anti-plant"~"ant-plant",
#     kw_to_refine == "bayesian hierarchical modeling"~"bayesian hierarchical model",
#     kw_to_refine == "behavior genetics"~"behavioral genetics",
#     kw_to_refine == "alternate states"~"alternative states",
#     kw_to_refine == "arciidae"~"arctiidae",
#     kw_to_refine == "area-concentrated search"~"area-concentrated searching",
#     kw_to_refine == "behavioral changes"~"behavioral change",
#     kw_to_refine == "behavioural change"~"behavioral change",
#     kw_to_refine == "age-specific breeding probabilities"~"age-specific breeding probability",
#     kw_to_refine == "alternative reproductive strategies"~"alternative reproductive strategy",
#     kw_to_refine == "anadromous fish"~"anadromous fishes",
#     kw_to_refine == "bialowieza forest"~"biaowiea forest",
#     kw_to_refine == "above and belowground herbivores"~"above- and belowground herbivory",
#     kw_to_refine == "alternate prey"~"alternative prey",
#     kw_to_refine == "arciidae"~"ariidae",
#     kw_to_refine == "anthropogenic stress"~"anthropogenic stressors",
#     kw_to_refine == "biogeochemical model"~"biogeochemical modeling",
#     kw_to_refine == "ant assemblages"~"bat assemblages",
#     kw_to_refine == "ant pollination"~"bat pollination",
#     kw_to_refine == "arboreal ants"~"arboreal plants",
#     kw_to_refine == "allogenic ecosystem engineers"~"autogenic ecosystem engineers",
#     kw_to_refine == "basic reproductive number"~"basic reproductive number r-0",
#     kw_to_refine == "alternative mating strategies"~"alternative mating strategy",
#     kw_to_refine == "above-ground competition"~"above-ground competition cue",
#     kw_to_refine == "agelaia"~"aglaia",
#     kw_to_refine == "agro-ecosystem"~"agroecosystems",
#     kw_to_refine == "akaike information criterion"~"akaikes information criteria",
#     kw_to_refine == "annual grass"~"annual grasses",
#     kw_to_refine == "baetids"~"baetis",
#     kw_to_refine == "bioenergetic model"~"bioenergetic modeling",
#     kw_to_refine == "biodiversity and ecosystem function (bef)"~"biodiversity and ecosystem function",
#     kw_to_refine == "altitudinal migrant"~"altitudinal migration",
#     kw_to_refine == "centre-periphery hypothesis"~"center-periphery hypothesis",
#     kw_to_refine == "borrelia burdgorferi"~"borrelia burgdorferi",
#     kw_to_refine == "blue-green aglae"~"blue-green algae",
#     kw_to_refine == "coastal temperate rain forests"~"coastal temperate rainforest",
#     kw_to_refine == "c-4 grassland"~"c-4 grasslands",
#     kw_to_refine == "brachyramphus marmoratus"~"brachyramphus marmotus",
#     kw_to_refine == "coadapted gene complex"~"coadapted gene complexes",
#     kw_to_refine == "coffee agro-ecosystems"~"coffee agroecosystem",
#     kw_to_refine == "comparative approach"~"comparative approaches",
#     kw_to_refine == "capreolus capreolus"~"capreolus capreolus l",
#     kw_to_refine == "cedar creek natural history area, minnesota"~"cedar creek natural history area, minnesota, usa",
#     kw_to_refine == "branching process"~"branching processes",
#     kw_to_refine == "coffee agroforest"~"coffee agroforestry",
#     kw_to_refine == "bloflies"~"blowflies",
#     kw_to_refine == "carry-over effects"~"carryover effect",
#     kw_to_refine == "colombian amazonia"~"colombian amazon",
#     kw_to_refine == "complementary resource use"~"complementary resources",
#     kw_to_refine == "catastrophic regime shifts"~"catastrophic regime-shift",
#     kw_to_refine == "coefficient of additive genetic variance"~"coefficient of additive genetic variation",
#     kw_to_refine == "coevolutionary arm races"~"coevolutionary arms race",
#     kw_to_refine == "chasmagnathus granulata"~"chasmagnathus granulatus",
#     kw_to_refine == "bodega marine reserve, california"~"bodega marine reserve, california (usa)",
#     kw_to_refine == "body size distribution"~"brood size distribution",
#     kw_to_refine == "coastal marsh"~"coastal marshes",
#     kw_to_refine == "community assembly by trait selection, cats"~"community assembly by trait selection",
#     kw_to_refine == "breeding age"~"breeding range",
#     kw_to_refine == "caesium"~"cesium",
#     kw_to_refine == "community function"~"community functioning",
#     kw_to_refine == "california floristic province, usa"~"california floristic province",
#     kw_to_refine == "chihuahuan desert, new mexico"~"chihuahuan desert, new mexico, usa",
#     kw_to_refine == "compensatory growth"~"compensatory regrowth",
#     kw_to_refine == "climate-growth relation"~"climate-growth relationship",
#     kw_to_refine == "community dynamic model"~"community dynamics modeling",
#     kw_to_refine == "competition for pollination"~"competition for pollinators",
#     kw_to_refine == "GxE interactions(s)"~"GxE interaction(s)",
#     kw_to_refine == "top-down vs. bottom-up control"~"bottom-up vs top-down control",
#     kw_to_refine == "50-ha forest dynamics plot"~"50-ha plot",
#     kw_to_refine == "<bold>g</bold>-matrix"~"g matrix",
#     kw_to_refine == "acacia species"~"acacia",
#     kw_to_refine == "africa, bat reproduction"~"african bat reproduction",
#     kw_to_refine == "age-specific reproduction"~"age-specific reproduction/survival",
#     kw_to_refine == "age-specific reproduction and survival"~"age-specific reproduction/survival",
#     kw_to_refine == "age-specific reproductive success"~"age-specific reproduction/survival",
#     kw_to_refine == "age-specific survival"~"age-specific reproduction/survival",
#     kw_to_refine == "aboveground annual net primary productivity"~"anpp",
#     kw_to_refine == "above ground net primary productivity (anpp)"~"anpp",
#     kw_to_refine == "aboveground net primary production (anpp)"~"anpp",
#     kw_to_refine == "age-dependent mortality demography"~"age-dependent mortality",
#     kw_to_refine == "age-from-stage modeling"~"age-from-stage models",
#     kw_to_refine == "abandoned agricultural lands"~"abandoned agriculture",
#     kw_to_refine == "abandoned cattle pastures"~"abandoned pastures",
#     kw_to_refine == "abandoned pasture"~"abandoned pastures",
#     kw_to_refine == "abandoned farmland"~"abandoned fields",
#     kw_to_refine == "16s rdna"~"16s rdna/rrna",
#     kw_to_refine == "16s"~"16s rdna/rrna",
#     kw_to_refine == "16s rdna sequencing"~"16s rdna/rrna",
#     kw_to_refine == "coevoltion"~"coevolution",
#     kw_to_refine == "16s rrna"~"16s rdna/rrna",
#     kw_to_refine == "16s rrna genes"~"16s rdna/rrna",
#     kw_to_refine == "16s-rrna and its gene sequencing"~"16s rdna/rrna",
#     TRUE ~ as.character(kw_to_refine))) %>%
#   mutate(kw_to_refine=tolower(kw_to_refine)) %>%
#   mutate(kw_to_refine=trimws(kw_to_refine)) %>%
#   rename(kw=original) %>%
#   rename(kw_refined=kw_to_refine)
#
#
#
#
# summary(kw_refined$kw==kw_refined$kw_refined)
#

kw_refined <- left_join(kw_refined, kw_refined2) %>%
  select(-kw) %>%
  filter(kw_refined != "18th&#8211") %>%
  filter(kw_refined != "&#160") %>%
  filter(kw_refined != "&#8208") %>%
  filter(kw_refined != "&#8722") %>%
  filter(kw_refined != "&#946") %>%
  filter(kw_refined != "&#947") %>%
  filter(kw_refined != "co&#8208") %>%
  filter(kw_refined != "non&#8208") %>%
  filter(kw_refined != "wood&#8208") %>%
  filter(kw_refined != "mixed&#8208")

final_kw <- read_csv("./bibliometrics/data_clean/final_kw.csv") %>%
  rename(DI = DOI) %>%
  rename(DE = kw_output) %>%
  relocate(DE, .after = DI)

merged_refs <- read_csv("./bibliometrics/data_clean/merged_refs.csv") %>%
  select(refID, jrnl_cat, SO, PY, DI)

final_kw <- left_join(final_kw, merged_refs) %>%
  relocate((refID:PY), .after = DI) %>%
  separate(DE, c(LETTERS[seq(from = 1, to = 20)]), sep = "\\|") %>%
  pivot_longer(!DI:PY, names_to = "letter", values_to = "DE") %>%
  select(-letter) %>%
  drop_na(DE) %>%
  mutate(DE = tolower(DE)) %>%
  mutate(DE = trimws(DE)) %>%
  rename(kw_refined = DE)

# summary(is.na(kw_refined$kw_refined))

kw_refined <- bind_rows(final_kw, kw_refined)

kw_refined <- write_csv(kw_refined, "./bibliometrics/data_clean/kw_refined.csv")


# clustering --------------------------------------------------------------
# https://cran.r-project.org/web/packages/textmineR/vignettes/b_document_clustering.html
library(textmineR)

kw <- kw_refined %>%
  filter(!is.na(jrnl_cat)) %>%
  # filter(jrnl_cat=="tropical") %>%
  group_by(jrnl_cat) %>%
  sample_n(1000)


kw_dtm <- kw %>%
  count(jrnl_cat, kw_refined) %>%
  rename(document = jrnl_cat, term = kw_refined, count = n)


dtm <- CreateDtm(
  doc_vec = kw_dtm$term, # character vector of documents
  doc_names = kw_dtm$document, # document names
  ngram_window = c(1, 4), # minimum and maximum n-gram length
  stopword_vec = c(
    stopwords::stopwords("en"), # stopwords from tm
    stopwords::stopwords(source = "smart")
  ), # this is the default value
  lower = TRUE, # lowercase - this is the default value
  remove_punctuation = FALSE, # punctuation - this is the default
  remove_numbers = FALSE, # numbers - this is the default
  verbose = FALSE, # Turn off status bar for this demo
  cpus = 2
) # default is all available cpus on the system


tf_mat <- TermDocFreq(dtm)


# TF-IDF and cosine similarity
tfidf <- t(dtm[, tf_mat$term]) * tf_mat$idf

tfidf <- t(tfidf)

csim <- tfidf / sqrt(rowSums(tfidf * tfidf))

csim <- csim %*% t(csim)

cdist <- as.dist(1 - csim)


hc <- hclust(cdist, "ward.D")

clustering <- cutree(hc, 4)

plot(hc,
  main = "Hierarchical clustering of 100 NIH grant abstracts",
  ylab = "", xlab = "", yaxt = "n"
)

rect.hclust(hc, 10, border = "red")


p_words <- colSums(dtm) / sum(dtm)

cluster_words <- lapply(unique(clustering), function(x) {
  rows <- dtm[clustering == x, ]

  # for memory's sake, drop all words that don't appear in the cluster
  rows <- rows[, colSums(rows) > 0]

  colSums(rows) / sum(rows) - p_words[colnames(rows)]
})


# create a summary table of the top 5 words defining each cluster
cluster_summary <- data.frame(
  cluster = unique(clustering),
  size = as.numeric(table(clustering)),
  top_words = sapply(cluster_words, function(d) {
    paste(
      names(d)[order(d, decreasing = TRUE)][1:5],
      collapse = ", "
    )
  }),
  stringsAsFactors = FALSE
)
cluster_summary




kw <- kw_refined %>%
  select(kw_refined) %>%
  distinct(kw_refined)


library(sjmisc)
old_string <- kw$kw_refined
newstring <- group_str(old_string)
newstring <- as_tibble(newstring) %>%
  separate(value, sep = ",", into = c("first", "second", "third"))
# Christensen, A. P., & Kenett, Y. (2019, October 22).
# Semantic Network Analysis (SemNA): A Tutorial on Preprocessing, Estimating,
# and Analyzing Semantic Networks. https://doi.org/10.1037/met0000463
library(SemNetCleaner)
library(SemNeT)
load.dictionaries("general")
kw <- kw_refined %>% select(kw_refined)
clean <- textcleaner(
  data = kw, miss = 99,
  partBY = "row", dictionary = "general"
)










# kw_to_refine_28june<-kw_refined %>%
#   select(kw_refined) %>%
#   group_by(kw_refined) %>%
#   slice(1) %>%
#   mutate(kw_to_refine=kw_refined) %>%
#   rename(original=kw_refined)
#
# write_csv(kw_to_refine_28june, "./bibliometrics/keyword_analysis/kw28june.csv")

# use tidystringdist to continue finding similars

#
#
#
# kw_for_comp<-as_tibble(unique(kw_refined$kw_refined)) %>% rename(kw=value) %>% arrange()
# kw_for_comp2<-kw_for_comp %>%
#   mutate(kw2=str_sub(kw, start=-9L, end=-2L)) %>%
#   mutate(kw3=str_sub(kw, start=-1L, end=-1L)) %>%
#   mutate(kw3=if_else(kw3=="s","s",""))  %>%
#   group_by(kw2) %>%
#   add_count(kw2) %>%
#   separate(kw, sep=" ", into=c("first","second","third")) %>%
#   arrange(first,second,kw3)
#
# write_csv(kw_for_comp2, "./bibliometrics/keyword_analysis/kw_for_comp2.csv")
#
#
# tidy_comb_kw <- tidy_comb_all(kw_for_comp$kw[40000:50000], kw_for_comp$kw[40000:50000])
# # tidy_comb_kw <- tidy_comb_all(kw_for_comp$kw, kw_for_comp$kw)
# kw_string_comp<-tidy_stringdist(tidy_comb_kw)
# kw_string_comp<-kw_string_comp %>%
#   filter(jw<0.05) %>%
#   filter(jw!=0) %>%
#   arrange(jw)
#
#
#
# additional_corrections<-paste("kw_to_refine == \"",kw_string_comp$V1,"\"~\"",kw_string_comp$V2,"\",",sep="") %>% tibble()
# write_csv(additional_corrections, "./bibliometrics/keyword_analysis/additional_corrections.csv")



bitr <- complete_data %>%
  filter(SO == "bitr") %>%
  filter(is.na(DE)) %>%
  filter(PY > 1991) %>%
  arrange(desc(PY)) %>%
  relocate(DI, DE, .before = 1)

analysis_refs <- complete_data %>%
  relocate(refID, source, SO, PY, DI, DE, AU, .before = 1)

names(analysis_refs)

unique(analysis_refs$jrnl_cat)


analysis_refs %>%
  group_by(jrnl_cat, SO, pub_cat) %>%
  tally()




# how much data to we have? -----------------------------------------------

# how many are missing

jrnl_artciles <- analysis_refs %>%
  filter(PY > 1990) %>%
  group_by(SO) %>%
  summarize(tot = n()) %>%
  arrange(desc(tot))

jrnl_no_kw <- analysis_refs %>%
  filter(is.na(DE) == TRUE) %>%
  filter(PY > 1990) %>%
  group_by(SO) %>%
  summarize(n = n()) %>%
  arrange(desc(n))

jrnl_summs <- left_join(jrnl_artciles, jrnl_no_kw) %>%
  mutate(perc = n / tot * 100) %>%
  arrange(desc(perc))


# coverage - overall
jrnl_yrs <- analysis_refs %>%
  filter(is.na(DE) == TRUE) %>%
  filter(PY > 1990) %>%
  # filter(SO!="amnat") %>%
  group_by(SO, PY, jrnl_cat) %>%
  tally() %>%
  arrange(jrnl_cat, SO, PY)
jrnl_yrs$SO <- factor(jrnl_yrs$SO, levels = unique(jrnl_yrs$SO[order(jrnl_yrs$jrnl_cat, jrnl_yrs$SO)]))
jrnl_yrs$PY <- factor(jrnl_yrs$PY, levels = unique(jrnl_yrs$PY[order(jrnl_yrs$jrnl_cat, jrnl_yrs$PY)]))

ggplot(jrnl_yrs, aes(x = PY, y = SO, fill = jrnl_cat, alpha = n)) +
  geom_tile(color = "black") +
  scale_fill_manual(values = c(tropical = "red2", general = "navyblue")) +
  theme_bw()



# coverage - key words
jrnl_yrs <- analysis_refs %>%
  drop_na(DE) %>%
  group_by(SO, PY, jrnl_cat) %>%
  tally() %>%
  arrange(jrnl_cat, SO, PY)
jrnl_yrs$SO <- factor(jrnl_yrs$SO, levels = unique(jrnl_yrs$SO[order(jrnl_yrs$jrnl_cat, jrnl_yrs$SO)]))
jrnl_yrs$PY <- factor(jrnl_yrs$PY, levels = unique(jrnl_yrs$PY[order(jrnl_yrs$jrnl_cat, jrnl_yrs$PY)]))

ggplot(jrnl_yrs, aes(x = PY, y = SO, fill = jrnl_cat, alpha = n)) +
  geom_tile(color = "black") +
  scale_fill_manual(values = c(tropical = "red2", general = "navyblue")) +
  theme_bw()

jrnl_count <- analysis_refs %>%
  group_by(SO, PY) %>%
  tally() %>%
  mutate(SO = as.factor(SO))

p <- ggplot(jrnl_count, aes(PY, n)) +
  geom_point()
p <- p + facet_wrap(vars(SO))
p










foo <- str_split_fixed(foo$DE, ";", n = 20)
foo <- foo %>% as_tibble()

# Load keyword records  -----------------

kw_refined <- read_csv("./keyword_analysis/kw_refined.csv") %>%
  rename(kw = kw_refined)
# mutate(TI=gsub(" - "," ",TI))
unique(kw_refined$SO)



range(kw_refined$PY)
kw_refined %>%
  distinct(refID) %>%
  count(DI, jrnl_cat)


top_kw <- kw_refined %>%
  group_by(kw) %>%
  tally() %>%
  arrange(desc(n))
top_kw


top_kw_trop <- kw_refined %>%
  filter(jrnl_cat == "tropical") %>%
  group_by(kw) %>%
  tally() %>%
  arrange(desc(n))
top_kw_trop %>% slice(1:20)

top_kw_global <- kw_refined %>%
  filter(jrnl_cat == "global") %>%
  group_by(kw) %>%
  tally() %>%
  arrange(desc(n))
top_kw_global %>% slice(1:20)


ecoevo_trop_DI <- read_csv("./bibliometrics/data_clean/ecoevo_trop.csv") %>%
  mutate_all(as.character) %>%
  mutate_all(tolower) %>%
  mutate_all(trimws) %>%
  select(DI, PY, SO, refID) %>%
  drop_na() %>%
  # mutate(PY=as.numeric(PY)) %>%
  mutate(SO = case_when(
    SO == "biotropica" ~ "bitr",
    SO == "evolution" ~ "evol",
    SO == "journal of animal ecology" ~ "jae",
    SO == "ecology" ~ "ecology",
    SO == "journal of ecology" ~ "jecol",
    SO == "journal of applied ecology" ~ "jappecol",
    SO == "journal of tropical ecology" ~ "jte",
    SO == "tropical ecology" ~ "trop_ecol",
    SO == "american naturalist" ~ "amnat",
    SO == "revista de biologia tropical" ~ "rbt",
    SO == "tropical conservation science" ~ "tcs",
    TRUE ~ as.character(SO)
  )) %>%
  mutate(pub_cat = "tropical") %>%
  tibble() %>%
  mutate_all(as.character) %>%
  mutate(DI2 = gsub(" ", "", DI)) %>%
  mutate(DI = substr(DI, start = 1, stop = 12))



kw_global <- kw_refined %>%
  filter(jrnl_cat == "global") %>%
  mutate_all(as.character) %>%
  mutate(DI = substr(DI, start = 1, stop = 12))
kw_global2 <- semi_join(kw_global, ecoevo_trop_DI, by = "DI")

kw <- kw_refined %>% filter(!is.na(jrnl_cat))
# https://stackoverflow.com/questions/66030942/tidytext-clustering
kw_cluster <- kw %>%
  count(jrnl_cat, SO, kw, sort = TRUE) %>%
  cast_sparse(SO, kw, n)


class(kw_cluster)
dim(kw_cluster)

kfit <- kmeans(kw_cluster, centers = 4)

enframe(kfit$cluster, value = "cluster") %>%
  # separate(name, into = c("jrnl_type"), sep = "_") %>%
  count(name, cluster) %>%
  arrange(cluster)


# topic models ------------------------------------------------------------


# https://www.tidytextmining.com/topicmodeling.html
# library(topicmodels)
# data("AssociatedPress")
unique(kw_refined$SO)
kw <- kw_refined %>% filter(!is.na(jrnl_cat))


kw_refined %>% count(SO)

# kw_dtm<-kw %>%
#   count(SO,kw) %>%
#   rename(document=SO,term=kw, count=n) %>%
#   cast_dtm(document,term,count)
kw_dtm <- kw %>%
  count(jrnl_cat, kw) %>%
  rename(document = jrnl_cat, term = kw, count = n) %>%
  cast_dtm(document, term, count)
library(topicmodels)
# set a seed so that the output of the model is predictable
kw_lda <- LDA(kw_dtm, k = 4, control = list(seed = 1234))
kw_lda

kw_topics <- tidy(kw_lda, matrix = "beta")
kw_topics

kw_top_terms <- kw_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta)
kw_top_terms


kw_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  scale_y_reordered()



beta_wide <- kw_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_wide %>%
  slice(1:30) %>%
  select(term, log_ratio) %>%
  arrange(log_ratio) %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(log_ratio, term)) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered()


kw_documents <- tidy(kw_lda, matrix = "gamma")
kw_documents

tidy(kw_dtm) %>%
  filter(document == "tropical") %>%
  arrange(desc(count))


tidy(kw_dtm) %>%
  filter(document == "global") %>%
  arrange(desc(count))


# kw wordclouds -----------------------------------------------------------


# wordclouds
library(wordcloud)
set.seed(1234) # for reproducibility
wordcloud(
  words = kw_comp$all_non_trop_pubs,
  # words = kw_trop$kw,
  freq = kw_comp$n.x,
  min.freq = 1,
  max.words = 50,
  random.order = FALSE,
  # rot.per=0.35,
  colors = brewer.pal(8, "Dark2")
)
#
#
# setdiff(kw_trop$kw,kw_not$kw)
# setdiff(kw_not$kw,kw_trop$kw)
# intersect(kw_trop$kw,kw_not$kw)


# analysis - title words --------------------------------------------------



# clean up the titles ----------------------------------------------------

range(merged_refs$PY, na.rm = TRUE)
merged_refs <- merged_refs %>%
  select(refID, jrnl_cat, SO, PY, TI) %>%
  # drop_na(TI) %>%
  mutate(TI = gsub(" - ", " ", TI)) %>%
  mutate(TI = gsub(":", "", TI)) %>%
  mutate(TI = gsub(",", "", TI)) %>%
  mutate(TI = gsub(";", "", TI)) %>%
  mutate(TI = gsub("species diversity", "species-diversity", TI)) %>%
  mutate(TI = gsub("tropical forest", "tropical-forest", TI)) %>%
  mutate(TI = gsub("dry forest", "dry-forest", TI)) %>%
  mutate(TI = gsub("rain forest", "rain-forest", TI)) %>%
  mutate(TI = gsub("seed forest", "seed-forest", TI))



# parse titles and clean --------------------------------------------------


tw <- merged_refs %>%
  drop_na(TI) %>%
  rename(tw = TI) %>%
  separate(tw, c(LETTERS[seq(from = 1, to = 60)]), sep = " ") %>%
  pivot_longer(!refID:PY, names_to = "letter", values_to = "tw") %>%
  select(-letter) %>%
  drop_na(tw) %>%
  mutate(tw = trimws(tw)) %>%
  mutate(tw = gsub("\n", " ", tw)) %>%
  filter(!(tw %in% stopwords(source = "snowball"))) %>% # deletes the stopwords
  mutate(tw = tolower(tw))





tw <- merged_refs %>%
  select(refID, jrnl_cat, SO, PY, TI) %>%
  rename(tw = TI) %>%
  mutate(tw = gsub(" - ", " ", tw)) %>%
  separate(tw, c(LETTERS[seq(from = 1, to = 60)]), sep = " ") %>%
  pivot_longer(!refID:PY, names_to = "letter", values_to = "tw") %>%
  select(-letter) %>%
  drop_na(tw) %>%
  mutate(tw = trimws(tw)) %>%
  filter(!(tw %in% stopwords(source = "snowball"))) %>% # deletes the stopwords
  mutate(tw = tolower(tw)) %>%
  mutate(tw = gsub("\n", " ", tw))



pubs_with_kw_tw <- merged_refs %>%
  select(refID, jrnl_cat, SO, PY, TI) %>%
  drop_na(TI) %>%
  rename(tw = TI)

tw_both <- pubs_with_kw_tw %>%
  select(-kw) %>%
  mutate(tw = gsub(":", "", tw)) %>%
  mutate(tw = gsub(",", "", tw)) %>%
  mutate(tw = gsub(";", "", tw)) %>%
  mutate(tw = gsub("species diversity", "species-diversity", tw)) %>%
  mutate(tw = gsub("tropical forest", "tropical-forest", tw)) %>%
  mutate(tw = gsub("dry forest", "dry-forest", tw)) %>%
  mutate(tw = gsub("rain forest", "rain-forest", tw)) %>%
  mutate(tw = gsub("seed forest", "seed-forest", tw)) %>%
  separate(tw, c(LETTERS[seq(from = 1, to = 60)]), sep = " ") %>%
  pivot_longer(!refID:PY, names_to = "letter", values_to = "tw") %>%
  select(-letter) %>%
  drop_na(tw) %>%
  mutate(tw = trimws(tw)) %>%
  mutate(tw = gsub("\n", " ", tw)) %>%
  filter(!(tw %in% stopwords(source = "snowball"))) %>% # deletes the stopwords
  mutate(tw = tolower(tw))
# tw_both$tw<-gsub("\n"," ",tw_both$tw)

kw_both <- pubs_with_kw_tw %>%
  select(-tw) %>%
  separate(kw, c(LETTERS[seq(from = 1, to = 20)]), sep = ";") %>%
  pivot_longer(!refID:PY, names_to = "letter", values_to = "kw") %>%
  select(-letter) %>%
  drop_na(kw) %>%
  mutate(kw = trimws(kw)) %>%
  mutate(kw = gsub("\n", " ", kw)) %>%
  mutate(kw = tolower(kw))
# kw_both$kw<-gsub("\n"," ",kw_both$kw)


# together<-full_join(tw_both,kw_both)
# no_kw<-together %>% filter(is.na(kw))
# both<-together %>% filter(is.na(kw))
# summary(together$tw==together$kw)
#
# kw$kw<-gsub("\n"," ",kw$kw)

# unique(together$kw)
# unique(together$tw)
kw_bitr <- kw_both %>%
  filter(SO == "bitr") %>%
  drop_na(kw) %>%
  group_by(jrnl_cat, kw) %>%
  tally() %>%
  arrange(desc(n))
kw_bitr

tw_bitr <- tw_both %>%
  filter(SO == "bitr") %>%
  drop_na(tw) %>%
  group_by(jrnl_cat, tw) %>%
  tally() %>%
  arrange(desc(n))
tw_bitr

kw_bitr
tw_bitr

kw_bitr_2join <- kw_both %>%
  # filter(SO=="bitr") %>%
  drop_na(kw) %>%
  rename(tw_kw = kw)

tw_bitr_2join <- tw_both %>%
  # filter(SO=="bitr") %>%
  drop_na(tw) %>%
  rename(tw_kw = tw)

joint_tw_kw <- bind_rows(kw_bitr_2join, tw_bitr_2join)

joint_tw_kw_global <- joint_tw_kw %>%
  filter(jrnl_cat == "global") %>%
  group_by(tw_kw) %>%
  tally() %>%
  arrange(desc(n))
joint_tw_kw_tropical <- joint_tw_kw %>%
  filter(jrnl_cat == "tropical") %>%
  group_by(tw_kw) %>%
  tally() %>%
  arrange(desc(n))
joint_tw_kw_global
joint_tw_kw_tropical


joint_tw_kw




top_kw_trop <- kw %>%
  filter(jrnl_cat == "tropical") %>%
  group_by(kw) %>%
  tally() %>%
  arrange(desc(n))
top_kw_trop <- top_kw_trop %>% slice(1:30)

top_kw_global <- kw %>%
  filter(jrnl_cat == "global") %>%
  group_by(kw) %>%
  tally() %>%
  arrange(desc(n))
top_kw_global <- top_kw_global %>% slice(1:30)


# top ttitle words --------------------------------------------------------


top_tw <- tw %>%
  group_by(tw) %>%
  tally() %>%
  arrange(desc(n))
top_tw

top_tw_trop <- tw %>%
  filter(jrnl_cat == "tropical") %>%
  group_by(tw) %>%
  tally() %>%
  arrange(desc(n))


top_tw_global <- tw %>%
  filter(jrnl_cat == "global") %>%
  group_by(tw) %>%
  tally() %>%
  arrange(desc(n))
top_tw_global




# attempte to parse ngrams from title (not sep by ; like kw are) ----------
# https://www.tidytextmining.com/ngrams.html
ngram_data <- merged_refs %>% filter(jrnl_cat == "tropical")
ngram_data <- merged_refs %>% filter(jrnl_cat == "global")
ngram_data <- merged_refs

tw <- ngram_data %>%
  select(refID, jrnl_cat, SO, PY, TI) %>%
  drop_na(TI) %>%
  rename(tw = TI) %>%
  mutate(tw = gsub(" - ", " ", tw))

tw_bigrams <- tw %>%
  select(tw) %>%
  # slice(1:10) %>%
  unnest_tokens(bigram, tw, token = "ngrams", n = 2) %>%
  count(bigram, sort = TRUE)
bigrams_separated <- tw_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  arrange(desc(n)) %>%
  slice(1:1000)
bigrams_filtered

tw_trigrams <- tw %>%
  select(tw) %>%
  # slice(1:10) %>%
  unnest_tokens(trigram, tw, token = "ngrams", n = 3) %>%
  count(trigram, sort = TRUE)
trigrams_separated <- tw_trigrams %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ")
trigrams_filtered <- trigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  arrange(desc(n)) %>%
  slice(1:1000)
trigrams_filtered


tw_fourgrams <- tw %>%
  select(tw) %>%
  # slice(1:10) %>%
  unnest_tokens(fourgram, tw, token = "ngrams", n = 4) %>%
  count(fourgram, sort = TRUE)
fourgrams_separated <- tw_fourgrams %>%
  separate(fourgram, c("word1", "word2", "word3", "word4"), sep = " ")
fourgrams_filtered <- fourgrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word) %>%
  arrange(desc(n)) %>%
  slice(1:1000)
fourgrams_filtered

tw_fivegrams <- tw %>%
  select(tw) %>%
  # slice(1:10) %>%
  unnest_tokens(fivegram, tw, token = "ngrams", n = 5) %>%
  count(fivegram, sort = TRUE)
fivegrams_separated <- tw_fivegrams %>%
  separate(fivegram, c("word1", "word2", "word3", "word4", "word5"), sep = " ")
fivegrams_filtered <- fivegrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word) %>%
  filter(!word5 %in% stop_words$word) %>%
  arrange(desc(n)) %>%
  slice(1:1000)
fivegrams_filtered


tw_sixgrams <- tw %>%
  select(tw) %>%
  # slice(1:10) %>%
  unnest_tokens(sixgram, tw, token = "ngrams", n = 6) %>%
  count(sixgram, sort = TRUE)
sixgrams_separated <- tw_sixgrams %>%
  separate(sixgram, c("word1", "word2", "word3", "word4", "word5", "word6"), sep = " ")
sixgrams_filtered <- sixgrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word) %>%
  filter(!word5 %in% stop_words$word) %>%
  filter(!word6 %in% stop_words$word) %>%
  arrange(desc(n)) %>%
  slice(1:1000)
sixgrams_filtered




bigram_graph <- bigrams_filtered %>%
  filter(n > 40) %>%
  graph_from_data_frame()


library(ggraph)
set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

set.seed(2020)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n),
    show.legend = FALSE,
    arrow = a, end_cap = circle(.07, "inches")
  ) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
