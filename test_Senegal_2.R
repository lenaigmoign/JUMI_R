setwd("C:/Users/Lenaig MOIGN/Documents/R/AP_Senegal")

#PACKAGE MAPME BIODIVERSITY
install.packages("mapme.biodiversity")

#AUTRES PACKAGES
librairies_req <- c("tidyverse","readxl","writexl",                   
                  "sf","wdpar","tmap","geodata","tidygeocoder","maptiles", "mapme.biodiversity")
manquantes <- !(librairies_req %in% installed.packages())
if (any(manquantes)) install.packages(librairies_req[manquantes])
  
#CHARGEMENT DES LIBRAIRIES
library(tidyverse)
library(geodata)
library(sf)
library(tmap)
library(wdpar)

#TELECHARGEMENT DES DONNEES DU WDPA 
WDPA_Senegal <- wdpa_fetch("Senegal", wait = TRUE, 
                          download_dir = "data/WDPA")
#CHARGEMENT DU CONTOUR 
contour_senegal <- gadm(country = "Senegal", resolution = 1, path = "data/GADM") %>%
st_as_sf()
save(contour_senegal, file = "data/contour_senegal.rds")

#CHARGEMENT DES DONNEES DU WDPA #NON FONCTIONNEL 
WDPA_Senegal <- wdpa_read("C:/Users/Lenaig MOIGN/Documents/R/AP_Senegal/data/WDPA/WDPA_Jun2023_SEN-shapefile.zip")
AP_Senegal <- c("C:/Users/Lenaig MOIGN/Documents/R/AP_Senegal/data/WDPA/WDPA_Jun2023_SEN-shapefile/WDPA_WDOECM_Jun2023_Public_SEN_shp_0/WDPA_WDOECM_Jun2023_Public_SEN_shp-polygons.shp",
                "C:/Users/Lenaig MOIGN/Documents/R/AP_Senegal/data/WDPA/WDPA_Jun2023_SEN-shapefile/WDPA_WDOECM_Jun2023_Public_SEN_shp_1/WDPA_WDOECM_Jun2023_Public_SEN_shp-polygons.shp",
                "C:/Users/Lenaig MOIGN/Documents/R/AP_Senegal/data/WDPA/WDPA_Jun2023_SEN-shapefile/WDPA_WDOECM_Jun2023_Public_SEN_shp_2/WDPA_WDOECM_Jun2023_Public_SEN_shp-polygons.shp")
for (i in 1:length(AP_Senegal)) { 
  AP_Senegal_sf <- st_read(AP_Senegal[i])
  st_write(AP_Senegal_sf, paste0("AP_Senegal", i, ".shp"))
}


#APRES FUSION SUR QGIS
WDPA_Senegal <- wdpa_read("C:/Users/Lenaig MOIGN/Documents/R/AP_Senegal/data/WDPA/WDPA_Jun2023_SEN-shapefile.zip")
AP_Senegal_of <- st_read("C:/Users/Lenaig MOIGN/Documents/R/AP_Senegal/AP_Senegal/AP_Senegal_of.shp")


#RENDU CARTO 
tmap_mode(mode = "view")
tm_shape(contour_senegal) +
tm_borders() + 
tm_shape(AP_Senegal_of) + 
tm_polygons(col = "IUCN_CAT", alpha = 0.6, title = "Catégorie IUCN",
            id = "NAME", 
            popup.vars = c("Type" = "DESIG", 
                            "Catégorie IUCN" = "IUCN_CAT",
                            "Surface déclarée" = "REP_AREA",
                            "Année du statut" = "STATUS_YR"))
#ADITIONAL OPTIONS 
tmap_options(check.and.fix = TRUE)


#VERIFIER LES VALEURS MANQUANTES 
AP_Senegal_of %>%
  st_drop_geometry() %>%
  summarise("Nombre total d'aires protégées" = n(),
            "Catégorie IUCN manquante" = sum(IUCN_CAT == "Not Reported"),
            "Année de création manquante" = sum(STATUS_YR == 0),
            "Gestionnaire manquant" = sum(MANG_AUTH == "Not Reported")) %>%
  pivot_longer(cols = everything(),
               names_to = " ",
               values_to = "Nombre d'aires") %>%
  gt() %>%
  tab_header("Valeurs manquantes dans les données WDPA pour le Sénégal") %>%
  tab_source_note("Source : WDPA (juin 2023)")

##SUPERPOSITION

# On ne garde que les polygones (sans les points)
AP_Senegal_of_poly <- AP_Senegal_of %>% 
  filter(st_geometry_type(.) == "MULTIPOLYGON")

# On ne garde que les parties terrestres
Ap_Senegal_of_poly_terrestres <- AP_Senegal_of_poly %>%
  st_intersection(contour_senegal)

# On calcule le total des surfaces de chaque aire
surface_cumul <- Ap_Senegal_of_poly_terrestres %>%
  mutate(surface = st_area(.)) %>%
  st_drop_geometry() %>% 
  summarise(surface = sum(surface, na.rm = TRUE)) %>%
  mutate(`Type de cumul` = "Somme des surfaces terrestres de chaque aire protégée",
         .before = everything())

library(units)
surface_tot <- Ap_Senegal_of_poly_terrestres %>%
  st_union() %>%
  st_as_sf() %>% 
  mutate(surface = st_area(.)) %>%
  st_drop_geometry() %>% 
  summarise(surface = sum(surface, na.rm = TRUE)) %>%
  mutate(`Type de cumul` = "Emprise totale des aires protégées",
         .before = everything())

compare_surfaces <- surface_cumul %>%
  bind_rows(surface_tot) %>%
  mutate(surface = set_units(surface, "hectares"),
         surface = as.numeric(surface)) %>%
  rename(`Surface (ha)` = surface)

compare_surfaces %>%
  gt() %>%
  fmt_number(columns = `Surface (ha)`, use_seps = TRUE, decimals = 0) %>% 
  tab_header(title = "Superposition des aires WDPA",
             subtitle = "Somme des surfaces d'aires vs. emprise totale") %>%
  tab_source_note("Source : WDPA (juin 20232), calculs des auteurs")

#Error in `stopifnot()`:
#  ℹ In argument: `surface = st_area(.)`.
#Caused by error in `wk_handle.wk_wkb()`:
#  ! Loop 232 is not valid: Edge 0 is degenerate (duplicate vertex)
#Run `rlang::last_trace()` to see where the error occurred.

##ACQUISITION DE DONNEES ENV. ET CALCUL D'INDICATEURS 
library(tidyverse)
library(mapme.biodiversity)
library(sf)
# Constitution d'un portefeuille (voir la documentation)

AP_Senegal_of_poly <- AP_Senegal_of %>%
  rename(original_assetid = assetid)
  filter(st_geometry_type(.)== "MULTIPOLYGON")
AP_Senegal_of_poly <- st_cast(AP_Senegal_of_poly, "POLYGON")

AP_Senegal_of_poly <- init_portfolio(x = AP_Senegal_of_poly, 
                               years = 1980:2020,
                               outdir = "data/mapme_Senegal",
                               add_resources = TRUE,
                               verbose = TRUE)


# Modèle numérique de terrain SRTM de la NASA
AP_Senegal_of_poly <- get_resources(x = AP_Senegal_of_poly , resource = "nasa_srtm")

# Indicateurs d'accessibilité
AP_Senegal_of_poly <- calc_indicators(x = AP_Senegal_of_poly,
                                "traveltime",  stats_accessibility = "mean",
                                engine = "extract")
# Indicateurs de relief de terrain
AP_Senegal_of_poly <- calc_indicators(x = AP_Senegal_of_poly,
                                indicators = c("tri", "elevation"),
                                stats_tri = "mean", stats_elevation = "mean")

#   # On récupère aussi les données de Global Forest Watch sur le couver forestier
# Vahatra_poly <- get_resources(x = Vahatra_poly, 
#                               resources = c("gfw_treecover", "gfw_lossyear"))
#   # Indicateurs de couvert forestier
# Vahatra_poly <- calc_indicators(x = Vahatra_poly,
#                                 indicators = "treecover_area", 
#                                 min_cover = 30, min_size = 1)

save(Vahatra_poly, file = "data/Vahatra_poly.rds")

