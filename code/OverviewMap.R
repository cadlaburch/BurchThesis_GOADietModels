
#Creating an overview map


water <- read_sf(here("data", "10m_physical",
                      "ne_10m_geography_marine_polys.shp"))

land <- read_sf(here("data", "10m_physical",
                      "ne_10m_land.shp"))

st_crs(land)

mapdata <- read.csv(here("output/Models/ModelFilteredData.csv")) %>% 
  distinct(RLAT, RLONG, Year)

mapdata <- st_as_sf(mapdata, coords = c("RLONG", "RLAT"), crs = 4326)

box <- c(xmin = -172,
         ymin = 52,
         xmax = -130,
         ymax = 62) %>%
  st_bbox(crs = st_crs(4326))


ggplot() + 
  geom_sf(data = land, fill = "grey") +
  geom_sf(data = mapdata, size = 0.01, aes(color = Year)) +
  scale_color_binned()
  theme_void() + 
  coord_sf(xlim = c(box$xmin, box$xmax),
           ylim = c(box$ymin, box$ymax),
           expand = TRUE) 


       