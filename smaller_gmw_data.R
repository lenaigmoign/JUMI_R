library(tidyverse)
library(sf)
library(geodata)
library(tmap)

# Senegal borders
contour_sen <- gadm(country = "Senegal", resolution = 1, level = 0, 
                    path = "data/GADM") %>%  
  st_as_sf() 

# Bounding box around senegal
bbox_sen <- contour_sen %>%
  st_bbox() %>%
  st_as_sfc()

# List fiels
gmw_files <- list.files("data/mapme_Senegal/gmw", full.names = TRUE)
dir.create("data/mapme_Senegal/gmw_sen")
gmw_files <- gmw_files[8:10]
# Generate new files croped on Madagascar
for (gmw_file in gmw_files) {
  gmw_file %>%
    st_read() %>% 
    st_make_valid() %>%
    st_intersection(contour_sen) %>% 
    st_write(str_replace(gmw_file, "gmw/", "gmw_sen/"))
}

  
