# load libraries ----------------------------------------------------------

library(tidyverse)
# library(stopwords)
# library(ngram)
# library(tidytext)
# library(igraph)
# library(janitor)
# library(tidystringdist)
library(here)
library(cowplot)


# load key parameters -----------------------------------------------------


cutoff=75
start_yr<-1990
end_yr<-2022


system <- c(
  "mammal",
  "usa",
  "grassland",
  "tropical forest",
  "panama",
  "costa rica",
  "liana",
  "litter",
  "seedling",
  "forest",
  "tree",
  "tropical wet forest",
  "zooplankton",
  "dipterocarpaceae",
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
  "drosophila",
  "drosophila melanogaster",
  "ant",
  "epiphyte",
  "amazonia",
  "secondary forest",
  "tropic",
  "elephant",
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
  "malaysia",
  "french guiana",
  "hawaii",
  "peru",
  "australia",
  "amphibian",
  "lepidoptera",
  "venezuela",
  "andes",
  "bats",
  "formicidae",
  "la selva",
  "mangrove",
  "india",
  "primate",
  "bolivia",
  "anuran",
  "madagascar",
  "indonesia", 
  "reptile",
  "caribbean",
  "kenya",
  "ficus",
  "hummingbird",
  "african",
  "pacific"
)

system <- as_tibble(system)


# load journals -----------------------------------------------------------



# Journals

title<-c("Journal of Evolutionary Biology",
         "Ecology",
         "Journal of Applied Ecology",
         "Evolution", 
         "Biotropica",
         "Journal of Ecology",
         "Tropical Conservation Science",
         "American Naturalist",
         "Tropical Ecology",
         "Journal of Tropical Ecology",
         "Revista de Biologia Tropical")

SO<-c("jeb", 
      "ecology",
      "jae",
      "evol",
      "bitr",
      "jecol",
      "tcs",
      "amnat",
      "trec",
      "jte",
      "rbt")

jrnl_cat<-c("general", 
            "general",
            "general",
            "general",
            "tropical",
            "general",
            "tropical",
            "general",
            "tropical",
            "tropical",
            "tropical")

titles<-tibble(title,SO,jrnl_cat) 

# EXCLUDE JOURNALS FROM ANALYSIS HERE

titles_analyzed<-titles %>% 
  # filter(SO!="jeb") %>% 
  # filter(SO!="evol") %>% 
  filter(SO!="rbt") 


# Load keyword records  -----------------
kw<-read_csv(here("bibliometrics","data_clean","keywords.csv")) %>% 
  inner_join(titles_analyzed) %>% 
  relocate(title,.after=SO) %>% 
  mutate(title=as.factor(title)) %>% 
  drop_na(final) 

kw<-kw %>% 
  filter(PY>=start_yr) %>% 
  filter(PY<=end_yr)




# titleword processing ----------------------------------------------------

tw_all<-read_csv(here("bibliometrics","data_clean","tw_clean.csv")) %>% 
  mutate(pub_cat=as.factor(pub_cat_2)) %>% 
  mutate(jrnl_cat=as.factor(jrnl_cat)) %>% 
  mutate(index = row_number()) %>% 
  mutate(pub_cat_2=as.factor(pub_cat_2)) %>% 
  inner_join(titles_analyzed) %>% 
  relocate(title,.after=SO) %>% 
  mutate(title=as.factor(title)) %>% 
  drop_na(final) 

tw<-tw_all %>% filter(PY>=start_yr) %>% 
  filter(PY<=end_yr)




# plot -  --------------------------------------------------

barplot_words <- function(dataset, cutoff,min_x,max_x,breaks_vec) {
  pubs_per_pub_cat <- dataset %>%
    group_by(pub_cat_2) %>%
    summarise(n_pubs = n_distinct(refID)) %>%
    ungroup() %>%
    arrange(pub_cat_2, desc(n_pubs))
  pubs_per_pub_cat
  
  # Top Keywords by pub cat
  rankings_pub <- dataset %>%
    group_by(pub_cat_2, final) %>%
    tally() %>%
    arrange(pub_cat_2, desc(n)) %>%
    group_by(pub_cat_2) %>%
    arrange(pub_cat_2, desc(n)) %>%
    left_join(pubs_per_pub_cat) %>%
    group_by(pub_cat_2) %>%
    mutate(perc_pubs_wth_term = (n / n_pubs * 100)) %>%
    group_by(pub_cat_2) %>%
    arrange(pub_cat_2, desc(n)) %>%
    mutate(rank_perc = rank(desc(n),  ties.method = c("random"))) %>%
    arrange(desc(pub_cat_2),rank_perc)
  rankings_pub
  
  # Identify "system" words
  
  rankings_pub <- rankings_pub %>%
    mutate(system = if_else((final %in% system$value == TRUE), "Y", "N")) %>%
    mutate(system = as.factor(system)) %>% 
    filter(rank_perc<=cutoff)
  
  in_both <- rankings_pub %>%
    group_by(final) %>%
    summarise(n2 = n()) %>%
    filter(n2 > 1) %>%
    mutate(both = TRUE)
  
  plot_term <- full_join(rankings_pub, in_both, by = "final") %>% 
    select(-n2) %>% 
    replace_na(list(
      "both" = FALSE
    )) %>% 
    rename(cat=pub_cat_2) 
  
  
  # TROP VS. GEN ALL POOLED
  
  trop_term <- plot_term %>%
    filter(cat == "tropical") %>%
    select(final, cat, rank_perc, system) %>%
    # mutate(final=paste(rank_perc,final,sep=": "))
    mutate(original=final) %>%
    mutate(final = paste("(", rank_perc, ") ", final, sep = ""))
  
  nontrop_term <- plot_term %>%
    filter(cat == "general") %>%
    select(final, cat, rank_perc, system) %>%
    mutate(original=final) %>%
    mutate(final = paste(final, " (", rank_perc, ")", sep = ""))
  
  # three keyword list - trop, nontrop, overlap -----------------------------
  
  in_both <- rankings_pub %>%
    filter(rank_perc<=cutoff) %>% 
    group_by(final) %>%
    summarise(n2 = n()) %>%
    filter(n2 > 1) %>%
    mutate(both = TRUE) %>% 
    mutate(original=final)
  
  unique_trop_term<-trop_term %>% filter(rank_perc<=cutoff) %>% anti_join(in_both,by="original") 
  unique_nontrop_term<-nontrop_term %>% filter(rank_perc<=cutoff) %>% anti_join(in_both,by="original") 
  
  in_both_term<- inner_join(trop_term,in_both,by="original") %>% 
    inner_join(nontrop_term,in_both,by="original") %>% 
    mutate(final.x=paste(final.x, " (",rank_perc.y,")",sep="")) %>% 
    rename(rank_perc_trop=rank_perc.x,
           rank_perc_non=rank_perc.y,
           system=system.x
    ) %>% 
    select(-system.y,-final.y) %>% 
    filter(is.na(original)==FALSE) %>% 
    filter(rank_perc_trop<=cutoff) %>% 
    filter(rank_perc_non<=cutoff)
  
  # unique_trop_term
  # unique_nontrop_term
  # in_both_term
  
  
  plot_term_trop<-plot_term %>%
    mutate(final=as.factor(final)) %>% 
    filter(cat=="tropical") %>% 
    filter(rank_perc<=cutoff) %>% 
    # select(final, cat, rank_perc, system,perc_pubs_wth_term) %>%
    # mutate(final=paste(rank_perc,final,sep=": ")) %>% 
    # mutate(final = paste(final, " (", rank_perc, ")", sep = "")) %>% 
    mutate(final = paste(final, " - ", rank_perc, " ", sep = "")) %>% 
    mutate(final = fct_reorder(final, perc_pubs_wth_term)) 
  
  plot_term_not<-plot_term %>%
    mutate(final=as.factor(final)) %>% 
    filter(cat=="general") %>%
    filter(rank_perc<=cutoff) %>% 
    # mutate(final = paste("(", rank_perc, ") ",final, sep = ""))  %>% 
    mutate(final = paste(final, " - ", rank_perc, " ", sep = "")) %>% 
    # mutate(final = paste(final, " (", rank_perc, ")", sep = "")) %>% 
    # select(final, cat, rank_perc, system,perc_pubs_wth_term) %>%
    mutate(final = fct_reorder(final, perc_pubs_wth_term)) 
  
  
   
   
   
   axis_max<-max((max(plot_term_trop$perc_pubs_wth_term)),
                 (max(plot_term_not$perc_pubs_wth_term)))*
     1.2
     

  GeomLabel$default_aes$size
  
  update_geom_defaults("text", list(size = 2))
  # https://stackoverflow.com/questions/25061822/ggplot-geom-text-font-size-control
  
  
  kw_trop_bar <- plot_term_trop %>%   # This trick update the factor levels
    ggplot(aes(x=final, y=perc_pubs_wth_term,fill = factor(system))) +
    geom_bar(stat="identity", color="black",size=0.2) +
    scale_fill_manual(values = c("white","slateblue4"))+
    # scale_y_continuous(limits = c(min_x, 6), breaks = seq(0, 6, by = .5))+
    scale_y_continuous(limits = c(min_x, max_x), breaks = breaks_vec)+
    ylab("Articles with Keyword (%)")+
    # ggtitle("Tropics")+
    # ylim(0, 4)+
    # scale_fill_manual(values = c("white","navy"))+
    coord_flip() +
    geom_text(
      data = plot_term_trop, aes(
        x = final, y = -.12, 
        label = final, 
        color = factor(system),
        # fontface = "bold"),
        fontface = ifelse(system == "Y", "bold", "plain"),
      ),
      hjust = "right", vjust = 0, nudge_x = 0, nudge_y = 0.08
    ) +
    scale_color_manual(values = c("black","midnightblue"))+
    # scale_y_discrete(expand = c(0, 2),limits = c(0, 4)) +
    theme_classic() + theme(
      panel.border = element_blank(), panel.grid.major = element_blank(),
      axis.line.y = element_line(color = "black", size = 0.0, lineend = "square"),
      axis.line.x = element_line(color = "black", size = 0.5, lineend = "square"),
      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), # sets colors of axes
      plot.title = element_text(hjust = 0.8, vjust =-103.5, face = "bold", size = 22, color="navy"), # Sets title size, style, location
      axis.title.x = element_text(colour = "black", size = 10, vjust=-3,hjust = 0.6), # sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
      axis.title.y = element_blank(),
      # axis.title.y = element_text(colour = "black", size = 10, angle = 0, vjust = 0.5), # sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
      legend.position = "none",
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(colour = "black", size = 8),
      axis.text.y = element_blank(),
      # axis.text.y = element_text(colour = "black", size = 10), # sets size and style of labels on axes
      plot.margin = unit(c(.7, .2, 1, .2), "cm")
    )
  
  kw_trop_bar
  
  
  
  kw_notrop_bar <- plot_term_not %>%   # This trick update the factor levels
    ggplot(aes(x=final, y=perc_pubs_wth_term,fill = factor(system))) +
    geom_bar(stat="identity", color="black",size=0.2) +
    scale_fill_manual(values = c("white","slateblue4"))+
    # scale_y_continuous(limits = c(min_x, 6), breaks = seq(0, 6, by = .5))+
    scale_y_continuous(limits = c(min_x, max_x), breaks = breaks_vec)+
    # scale_y_continuous(limits = c(min_x, axis_max), n.breaks=10,minor_breaks=2)+
    ylab("Articles with Keyword (%)")+
    # ggtitle("Non-tropical")+
    # ylim(0, 4)+
    # scale_fill_manual(values = c("white","navy"))+
    coord_flip() +
    geom_text(
      data = plot_term_not, aes(
        x = final, y = -.12, 
        label = final, 
        color = factor(system),
        # size= 6,
        # fontface = "bold"),
        fontface = ifelse(system == "Y", "bold", "plain"),
      ),
      hjust = "right", vjust = 0, nudge_x = 0, nudge_y = 0.08
    ) +
    scale_color_manual(values = c("black","midnightblue"))+
    # scale_y_discrete(expand = c(0, 2),limits = c(0, 4)) +
    theme_classic() + theme(
      panel.border = element_blank(), panel.grid.major = element_blank(),
      axis.line.y = element_line(color = "black", size = 0.0, lineend = "square"),
      axis.line.x = element_line(color = "black", size = 0.5, lineend = "square"),
      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), # sets colors of axes
      plot.title = element_text(hjust = 0.8, vjust =-103.5, face = "bold", size = 22, color="navy"), # Sets title size, style, location
      axis.title.x = element_text(colour = "black", size = 10, vjust=-3,hjust = 0.6), # sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
      axis.title.y = element_blank(),
      # axis.title.y = element_text(colour = "black", size = 10, angle = 0, vjust = 0.5), # sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
      legend.position = "none",
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(colour = "black", size = 8),
      axis.text.y = element_blank(),
      # axis.text.y = element_text(colour = "black", size = 10), # sets size and style of labels on axes
      plot.margin = unit(c(.7, .2, 1, .2), "cm")
    )
  
  kw_notrop_bar
  
  terms_fig<-plot_grid(kw_trop_bar, kw_notrop_bar,
                          nrow = 1,
                          # labels = "AUTO",
                          labels=(c("a. 'Tropical' Articles","b. 'Non-tropical' Articles")),
                          label_size = 10,
                          align = "v"
  )
  
  terms_fig

return(terms_fig)
}


# for KW
breaks_vec<-seq(0, 6, by = .5)
min_x<- -1.0
max_x<-6
dataset<-kw

kw_fig<-barplot_words(kw,cutoff,min_x,max_x,breaks_vec)
kw_fig
ggsave("kw_fig.jpeg", 
       path = "./manuscript/figures", 
       dpi=700,
       width = 10,
       height = 7,
       units = c("in")
)

# for TW
breaks_vec<-seq(0, 25, by = 2.5)
min_x<- -2.4
max_x=25
dataset=tw
tw_fig<-barplot_words(tw,cutoff,min_x,max_x,breaks_vec)
tw_fig
ggsave("tw_fig.jpeg", 
       path = "./manuscript/figures", 
       dpi=700,
       width = 10,
       height = 7,
       units = c("in")
)
