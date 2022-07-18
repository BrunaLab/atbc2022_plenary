library(tidyverse) 
library(raster)
library(rgdal)
library(geotiff)
library(patchwork)
# https://sedac.ciesin.columbia.edu/data/set/gpw-v4-population-count-adjusted-to-2015-unwpp-country-totals-rev11
# Center for International Earth Science Information Network - CIESIN - Columbia University. 
# 2018. Gridded Population of the World, Version 4 (GPWv4): Population Count Adjusted to Match 
# 2015 Revision of UN WPP Country Totals, Revision 11. Palisades, New York: NASA Socioeconomic 
# Data and Applications Center (SEDAC). https://doi.org/10.7927/H4PN93PB. Accessed DAY MONTH YEAR.
str_name<-"./figs/gpw-v4-population-count-adjusted-to-2015-unwpp-country-totals-rev11_2020_1_deg_asc/gpw_v4_population_count_adjusted_to_2015_unwpp_country_totals_rev11_2020_1_deg.asc"
imported_raster<-raster(str_name)
plot(imported_raster)
pop_df <- imported_raster  %>% 
  rasterToPoints(., spatial = TRUE) %>% 
  as_tibble() %>% 
  rename(lon=x,
         lat=y,
         pop=gpw_v4_population_count_adjusted_to_2015_unwpp_country_totals_rev11_2020_1_deg) %>% 
  mutate(region=ifelse((lat > (-24) & lat < 24), "Tropical","Temperate")) %>% 
  mutate(cumsum=cumsum(pop)) 
  


pop_df_3bins <- pop_df %>% 
  mutate(region=case_when(lat>24 ~ "north_temp",
                          lat<(-24) ~ "south_temp",
                          TRUE ~ "tropical")) %>% 
  group_by(region) %>%
  summarise(n = sum(pop)) %>% 
  mutate(perc=n/sum(n)*100)
# percentage in tropics ---------------------------------------------------

perc_trop<-pop_df_3bins %>% filter(region=="tropical") %>% select(perc)
perc_trop<-pop_df_3bins[3,3] 

# binning figures ---------------------------------------------------------


pop_df_binned <- pop_df %>% 
  group_by(region,group = cut(lat, breaks = seq(-90, 90, 3))) %>%
  summarise(pop = sum(pop)) %>% 
  rename(lat=group)

theme_opts <- list(theme(
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  panel.background = element_blank(),
  axis.line.y.left =element_line(color = "gray42", size=0.8),
  axis.line.x.bottom =element_line(color = "gray42", size=0.8),
  plot.background = element_rect(fill="white"),
  panel.border = element_blank(),
  # axis.text.x = element_blank(),
  # axis.text.y = element_blank(),
  plot.title = element_text(hjust = 0.5,size=28,face="bold",color="black"),
  plot.subtitle = element_text(hjust = 0.5,
  size=24, 
  face="bold", 
  color="navyblue"),
  axis.ticks = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  legend.position="top",
  legend.title=element_blank()))


group.colors <- c(Tropical = "#1AB601", Temperate = "#085D8B")

# plot1<-ggplot(pop_df_binned, aes(x=lat, y=pop, fill=region)) + 
plot1<-ggplot(pop_df, aes(x=lat, y=pop/1000000, fill=region)) + 
  geom_bar(stat = "identity")+
  annotate("text", x=-20, y=250, label= paste(floor(perc_trop),"% (50% by 2050)", sep=""), size=16) +
  scale_fill_manual(values=group.colors)+
  geom_vline(xintercept = 0,color = "gray42", linetype="dashed", size=0.8)+
  # geom_vline(xintercept = -24, linetype="dashed", 
  #            color = "gray42", size=0.8)+
  # geom_vline(xintercept = 24, linetype="dashed", 
  #            color = "gray42", size=0.8)+
  scale_y_continuous(limits = c(0,300), expand = c(0, 0),breaks = seq(0, 300, by = 50)) +
  scale_x_continuous(limits = c(-90,90), breaks = seq(-90, 90, by = 15), expand = c(0, 0)) +
  coord_flip()+
  labs(y ="Population (millions)", x = "Latitude")+
  theme_opts
plot1







