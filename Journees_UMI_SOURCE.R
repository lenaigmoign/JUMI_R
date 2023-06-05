
# INSTALLATION DES LIBRAIRIES R 

librairies_req <- c("tidyverse",# Une série de packages pour faciliter la manipulation de données
                    "readxl", # Pour lire les fichiers excel (Carvalho et al. 2018)
                    "writexl",# Pour écrire des fichiers excel 
                    "sf", # Pour faciliter la manipulation de données géographiques
                    "maptiles", # Pour télécharger des fonds de carte 
                    "geodata", # Pour télécharger simplement les frontières administratives
                    "tidygeocoder", # Pour obtenir les coordonnées GPS d'un point à partir de son nom 
                    "tmap",# Pour produire de jolies cartes 
                    "wdpar", # Pour télécharger la base d'aires protégées WDPA
                    "mapme.biodiversity") # Pour importer et analyser des indicateurs de biodiversité
                  
                   
manquantes <- !(librairies_req %in% installed.packages())
if (any(manquantes)) install.packages(librairies_req[manquantes])
  
# CHARGEMENT DES LIBRAIRIES

library(tidyverse)
library(sf)
library(geodata)
library(wdpar)
library(tmap)

# TELECHARGEMENT DES DONNEES DU WDPA 

WDPA_Senegal <- wdpa_fetch("Senegal", wait = TRUE, 
                          download_dir = "data/WDPA")

WDPA_Senegal %>%
  mutate(geom_type = st_geometry_type(.)) %>%  # Ajout d'une colonne pour les catégories de géométries
  group_by(geom_type) %>%  # Tri des données en fonction de leur catégorie  
  summarise(n = n()) # Résumé pour chaque catégorie de géométrie

crs(WDPA_Senegal)

# CREATION D'UN TABLEAU DESCRIPTIF (AP PAR ANNEE DE CREATION, TO DO : AJOUTER NOM de L'AIRE PROTEGE)

AP_Annees <- WDPA_Senegal %>%
  filter(st_geometry_type(.) == "MULTIPOLYGON") %>% # Sélection des polygones 
  mutate(GIS_AREA= st_area(.), # Création d'une colonne avec la superficie de chaque polygone 
         GIS_AREA= units::set_units(GIS_AREA, "km2"))%>% # Conversion de l'unité en km² 
  group_by(STATUS_YR) %>% # Groupement des données par année de création de l'aire protégée 
  summarise(n = n(), # Calcul du nombre d'observations pour chaque année dans une colonne de l'effectif n
            area = sum(GIS_AREA)) %>% # Somme des superficies pour chaque groupe dans une colonne area  
  arrange(STATUS_YR) #Tri des données par ordre croissant

# CONNAITRE LES AIRES PROTEGEES CREES EN 2020 

WDPA_Senegal %>% 
filter(STATUS_YR == 2020) %>% 
select(NAME, DESIG)  

# CONNAITRE LES AIRES PROTEGEES CREES AVANT 1972 (ETAPE 1)

WDPA_Senegal %>% 
filter(STATUS_YR < 1972) %>% 
select(NAME, DESIG) 


# CONNAITRE LES AIRES PROTEGEES CREES AVANT 1972 (ETAPE 2) 
# que celles dont on connaît l'année de création

WDPA_Senegal %>% 
  filter(STATUS_YR < 1972 & STATUS_YR != 0) %>% 
  select(NAME, DESIG) 

# CONNAITRE LES AIRES PROTEGEES CREES AVANT 1972 (ETAPE 3)
#que les polygones 

WDPA_Senegal %>% 
  filter(STATUS_YR < 1972 & STATUS_YR != 0) %>% 
  filter(st_geometry_type(.) == "MULTIPOLYGON") %>%
  select(NAME, DESIG) 

# REGARDER LES CREATIONS d'AIRES PROTEGEES PAR MANDAT PRESIDENTIEL 

# Créer une variable pour définir les périodes

WDPA_mandats <- WDPA_Senegal %>%
  filter(STATUS_YR != 0) %>%
  mutate(Periode = cut(STATUS_YR, breaks = c(1981, 2000, 2012, 2020),
                       labels = c("1981-2000", "2000-2012", "2012-2020"),
                       include.lowest = TRUE)) %>%
  filter(!is.na(Periode))

# Effectif total d'aires protégées sur les 3 mandats 

nrow(WDPA_mandats)

# (FAUX A AVANCER) Afficher le nombre d'observations pour chaque mandat 
result <- WDPA_mandats %>%
  count(Periode, name = "Nombre_Observations")


# (FAUX) Regrouper les aires protégées par période et compter le nombre d'aires par période
aires_par_periode <- WDPA_mandats %>%
  group_by(Periode) %>%
  mutate(Nombre_Aires = n()) %>%
  
aires_par_periode

# TABLEAU SYNTHETIQUE PAR MANDAT 
  nombre_observations <- WDPA_mandats %>%
  count(Periode)


# RENDU CARTOGRAPHIQUE
tmap_mode(mode = "view")
#tm_shape(contour_senegal) +
#tm_borders() + 
tm_shape(WDPA_Senegal) + 
tm_polygons(col = "IUCN_CAT", alpha = 0.6, title = "Catégorie IUCN",
            id = "NAME", 
            popup.vars = c("Type" = "DESIG", 
                            "Catégorie IUCN" = "IUCN_CAT",
                            "Surface déclarée" = "REP_AREA",
                            "Année du statut" = "STATUS_YR"))
# ADITIONAL OPTIONS 
tmap_options(check.and.fix = TRUE)


# VERIFIER LES VALEURS MANQUANTES 
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
  tab_source_note("Source : WDPA (juin 2023)"

## SUPERPOSITION ##

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

## ACQUISITION DE DONNEES ENV. ET CALCUL D'INDICATEURS ##
library(tidyverse)
library(mapme.biodiversity)
library(sf)

#  Constitution d'un portefeuille (voir la documentation)

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

