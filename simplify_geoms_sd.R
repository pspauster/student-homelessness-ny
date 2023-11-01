library(tidyverse)
library(sf)
library(RSocrata)

nyc_sd_sf <-read.socrata("https://data.cityofnewyork.us/resource/cuae-wd7h.csv") %>% 
  st_as_sf(wkt = "the_geom")

ggplot()+
  geom_sf(nyc_sd_sf, mapping = aes())

nyc_sd_simple <- nyc_sd_sf %>% 
  st_simplify(dTolerance = 1000, preserveTopology = F) %>% 
  st_set_crs(4326)

ggplot()+
  geom_sf(nyc_sd_simple, mapping = aes())

st_write(nyc_sd_simple, "nyc_sd_simple.geojson", driver = "GeoJSON", append = F)
