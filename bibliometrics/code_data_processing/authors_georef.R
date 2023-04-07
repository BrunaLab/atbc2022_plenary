
# figure of area by latitude ----------------------------------------------

# Data: hub.worldpop.org/geodata/summary?id=29692
library(tidyverse)
library(raster)

# # ggplot megacities -------------------------------------------------------

# 
library(tidyverse)
library(sf)
library(sp)
library(rnaturalearth)
library(rnaturalearthdata)
library(transformr)
library(gganimate)
library(gifski)

mega<-read_csv("./bibliometrics/data_clean/FINAL_AUTHORS_prelim_georef.csv") %>% 
  rename(lon=long) %>% 
  mutate(jrnl_cat = case_when(
    SO ==  "bitr"~"tropical",
    SO ==  "evol"~"global",
    SO ==  "jae"~"global",
    SO ==  "ecology"~"global",
    SO ==  "jecol"~"global",  
    SO ==  "jte"~"tropical",
    SO ==  "trop_ecol"~"tropical",
    SO ==  "amnat"~"global",
    SO ==  "rbt"~"tropical",
    SO ==  "tcs"~"tropical",
    TRUE ~ as.character(SO))) %>% 
  mutate(pop_size=1000) %>% 
  drop_na(lat)



# 
# mega_wide<-mega %>%
#   pivot_longer(!c(index:lat), names_to = "year", values_to = "popsize") %>%
#   mutate(year=gsub("yr","",year)) %>%
#   mutate(year=as.numeric(year)) %>%
#   mutate(city=as.factor(city)) %>%
#   mutate(ISO=as.factor(ISO)) %>%
#   filter(popsize>10000000) %>%
#   mutate(region=ifelse((lat > (-24)&lat < 24), "Tropical","Temperate")) %>%
#   mutate(year=floor(year)) %>%
#   arrange(year,region,popsize)


target_crs <- st_crs("+proj=moll +x_0=0 +y_0=0 +lat_0=0 +lon_0")
world.trans <- ne_countries(scale = "medium", returnclass = "sf") %>%
  sf::st_transform(crs = target_crs)
class(world.trans)

points_all<-data.frame(mega$lat,mega$lon,mega$PY,mega$jrnl_cat,mega$pop_size) 
points_all<-points_all %>% 
  rename("lat"="mega.lat", 
         "long"="mega.lon",
         "region"="mega.jrnl_cat",
         "popsize"="mega.pop_size",
         "year"="mega.PY") %>% 
  arrange(year,region,popsize)
head(points_all)
coordinates(points_all) <- ~long+lat
proj4string(points_all) <- CRS(paste("+init=epsg:4326"))


pts.trans.all <- st_as_sf(points_all, coords = c("mega_wide.lat", "mega_wide.lon"), 
                          crs = ("+proj=moll +lon_0=0 +x_0=0 +y_0=0"), agr = "constant")

theme_opts <- list(theme(panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.background = element_blank(),
                         plot.background = element_rect(fill="white"),
                         panel.border = element_blank(),
                         axis.line = element_blank(),
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank(),
                         plot.title = element_text(hjust = 0.5,
                                                   size=28, 
                                                   face="bold", 
                                                   color="black"),
                         plot.subtitle = element_text(hjust = 0.5,
                                                      size=24, 
                                                      face="bold", 
                                                      color="navyblue"),
                         axis.ticks = element_blank(),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(),
                         legend.position="none"))

group.colors <- c(Tropical = "#1AB601", Temperate = "#085D8B")
# group.colors <- c(Tropical = "#FFD300", Temperate = "#2D01BC")
mega_map<-ggplot(world.trans) + 
  geom_sf(color = "gray50", fill = "gray50") +
  geom_sf(data = pts.trans.all, aes(fill = region,size=popsize,
                                    group = interaction(region, year)), shape = 21)+
  labs(title="Global Megacities: 2000-2100",
       subtitle="2000") + 
  
  scale_fill_manual(values=group.colors)+
  # labs(title="Jrnl Categories") + 
  # labs(size = 'Pop. Size')+
  # labs(size = 'No of Publications')+
  # labs(fill = ' ')+
  coord_sf(default_crs = target_crs)+
  theme_opts
mega_map

# ANIMATION HELP
# https://www.alexcookson.com/post/2020-10-18-building-an-animation-step-by-step-with-gganimate/
# https://stackoverflow.com/questions/56411604/how-to-make-dots-in-gganimate-appear-and-not-transition
# mega_map +
#   transition_time(year) +
#   labs(title = "YEAR: {frame_time}")

mega_map +
  transition_time(year) +
  shadow_mark(colour = 'black', size = 0.0) +
  # transition_states(
  #   year,
  #   transition_length = 2,
  #   state_length = 1)+
  labs(subtitle = "{round(frame_time, 0)}") ###



# enter_fade()  # I liked the look of this, but fine to leave out
animate(mega_map, 
        # duration = 50, 
        nframes = 10)+
  labs(caption = "YEAR: {round(frame_time, 0)}") ###

anim_save("./mega_map.gif")

