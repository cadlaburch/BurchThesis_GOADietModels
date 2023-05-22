#Author: Catalina Burch
#Date Modified: 5/19/23
#Description: This file creates the figure 1 map 


#Load Libraries
library(here) #for finding working directory
library(readr) #for loading CSV
library(tidyverse)
library(sf)
library(ggsn) #for north arrow


#load data
water <- read_sf(here("data", "10m_physical", "ne_10m_geography_marine_polys.shp"))

land <- read_sf(here("data", "10m_physical", "ne_10m_land.shp"))

INPFC <- read_sf(here("data", "GOA_Shapes", "GOA_Shapes.shp")) %>% st_transform(us, crs = 4326)

Den <- read_sf(here("data", "GOA_Den", "GOA_Den.shp")) %>% st_transform(us, crs = 4326)

#check that crs matches
st_crs(INPFC) #4326

mapdata <- read.csv(here("data/data.csv")) %>% 
  distinct(RLAT, RLONG, Year) #this is to make sure there is only one point for each haul, no overlaps

#convert to sf object
mapdata <- st_as_sf(mapdata, coords = c("RLONG", "RLAT"), crs = 4326)

#create bounding box
box <- c(xmin = -172,
         ymin = 49,
         xmax = -130,
         ymax = 62) %>%
  st_bbox(crs = st_crs(4326))


#plot map
Figure1 <- ggplot() + 
  geom_sf(data = mapdata, size = 0.01, color = "red") +
  geom_sf(data = land, fill = "grey") +
  geom_sf(data = INPFC, fill = "transparent") +
  theme_classic()+
  coord_sf(xlim = c(box$xmin, box$xmax),
           ylim = c(box$ymin, box$ymax),
           expand = F)+
  north(scale = 0.8, symbol = 10,
  y.min = 59, y.max = 61.5,
  x.min = -135, x.max = -131) + 
  labs(y = "Latitude", x = "Longitude") +
  geom_segment(aes(x = -147, xend = -147,
               y = 61, yend = 49), linetype = "dashed", color = "#00B8E7") +
  annotate("text", x = -150, y = 50, label = "<- West", color = "#00B8E7") +
  annotate("text", x = -144, y = 50, label = "East ->", color = "#00B8E7") +
  annotate("text", x = -165, y = 51.8, label = "Shumagin") +
  annotate("text", x = -156.8, y = 53.5, label = "Chirikof") +
  annotate("text", x = -151.5, y = 55.2, label = "Kodiak") +
  annotate("text", x = -144, y = 57.5, label = "Yakutat") +
  annotate("text", x = -137, y = 55, label = "SE") +
  theme(text = element_text(family = "Times New Roman"))

#Export figure
ggsave(plot = Figure1, device = png, path = here("output/Figures"), filename = "Figure1.png", dpi = 300)

  




       