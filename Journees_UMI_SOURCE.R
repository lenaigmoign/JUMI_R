
# 1. INSTALLATION DES LIBRAIRIES R 

librairies_req <- c("tidyverse",# Une série de packages pour faciliter la manipulation de données
                    "readxl", # Pour lire les fichiers excel (Carvalho et al. 2018)
                    "writexl",# Pour écrire des fichiers excel 
                    "gt",# Pour produire de jolis tableaux
                    "sf", # Pour faciliter la manipulation de données géographiques
                    "maptiles", # Pour télécharger des fonds de carte 
                    "geodata", # Pour télécharger simplement les frontières administratives
                    "tidygeocoder", # Pour obtenir les coordonnées GPS d'un point à partir de son nom 
                    "tmap",# Pour produire de jolies cartes 
                    "wdpar", # Pour télécharger la base d'aires protégées WDPA
                    "mapme.biodiversity") # Pour importer et analyser des indicateurs de biodiversité
                  
                   
manquantes <- !(librairies_req %in% installed.packages())
if (any(manquantes)) install.packages(librairies_req[manquantes])
  
# 2. CHARGEMENT DES LIBRAIRIES NECESSAIRES

library(tidyverse)
library(sf)
library(geodata)
library(wdpar)
library(tmap)
library(gt)
library(magrittr)


# 3. TELECHARGEMENT DES DONNEES DU WDPA 

WDPA_Senegal <- wdpa_fetch("Senegal", wait = TRUE, 
                          download_dir = "data/WDPA")


WDPA_Senegal <- wdpa_read("C:/Users/Lenaig MOIGN/Documents/R/JUMI Officiel/data/WDPA/WDPA_Jun2023_SEN-shapefile.zip") 


# Charger les shapefiles dans une liste
liste_shapefiles <- lapply(chemins_shapefiles, st_read)

# Attribuer un nom commun à la liste
WDPA_Senegal <- "shapefiles"

# Assigner la liste à un nom commun dans l'environnement de travail
assign(WDPA_Senegal, liste_shapefiles)

WDPA_Senegal %>%
  mutate(geom_type = st_geometry_type(.)) %>%  # Ajout d'une colonne pour les catégories de géométries
  group_by(geom_type) %>%  # Tri des données en fonction de leur catégorie  
  summarise(n = n()) # Résumé pour chaque catégorie de géométrie


# 4. TELECHARGEMENT DES DONNEES DU CONTOUR DU SENEGAL 

# On télécharge la version en ligne

contour_senegal <- gadm(country = "Senegal", resolution = 1, level = 0,
                     path = "data/GADM") %>%
st_as_sf()

# On charge la version disponible localement 

save(contour_sen, file = "data/GADM/gadm/contour_sen.rds")
str(contour_sen)

## 5. EXPLORATION DES DONNEES DISPONIBLES SUR LES AIRES PROTEGEES 



# CREATION D'UN TABLEAU SYNTHETIQUE 

AP_Annees <- WDPA_Senegal %>%
  filter(st_geometry_type(.) == "MULTIPOLYGON") %>% # Sélection des polygones 
  mutate(GIS_AREA= st_area(.), # Création d'une colonne avec la superficie de chaque polygone 
         GIS_AREA= units::set_units(GIS_AREA, "km2"))%>% # Conversion de l'unité en km² 
  group_by(STATUS_YR) %>% # Groupement des données par année de création de l'aire protégée 
  summarise(n = n(), # Calcul du nombre d'observations pour chaque année dans une colonne de l'effectif n
            area = sum(GIS_AREA)) %>% # Somme des superficies pour chaque groupe dans une colonne area  
  arrange(STATUS_YR) #Tri des données par ordre croissant %>%

AP_Annees %>%
  select(-c('geometry')) %>%
  gt() %>%
  cols_label(STATUS_YR = "Année de création de l'aire protégée",n = "Effectif total",area = "Superficie en km²") %>%
  tab_header("Synthèse du nombre et de la superficie totale des aires protégées par année de création") %>%
  tab_source_note("Source : Données du WDPA (juin 2023)")


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
  filter(STATUS_YR != 0) %>% # On exclut les AP dont l'année de création n'est pas renseignée
  mutate(Periode = cut(STATUS_YR, breaks = c(1981, 2000, 2012, 2020),
                       labels = c("1981-2000", "2000-2012", "2012-2020"),
                       include.lowest = TRUE)) %>% # On créée des catégories de période correspondant aux mandats
  filter(!is.na(Periode)) # On exclut les valeurs NA correspondant aux années avant 1981 

# Effectif total d'aires protégées sur les 3 mandats 

AP_mandats <- WDPA_mandats %>%
  filter(st_geometry_type(.) == "MULTIPOLYGON" & Periode != "NA") %>% # Sélection des polygones 
  group_by(Periode) %>% # Groupement des données par période de création de l'aire protégée 
  summarise(Effectif_total = n()) %>%  # Calcul du nombre d'observations pour chaque année 
  arrange(Periode) %>% #Tri des données par ordre croissant
  mutate(Nom = c("ABDOU DIOUF", "ABDOULAYE WADE", "MACKY SALL")) 
  
# Création d'un beau tableau (FAUX)

AP_mandats %>%
  gt() %>%
  select(-geometry)
  cols_label(Periode = "Mandat", Effectif_total = "Nombre de création d'aires protégées", Nom = "Président") %>%
  tab_header("Nombre de création d'aires protégées durant les derniers mandats présidentiels au Sénégal") %>%
  tab_source_note("Source : Données du WDPA (juin 2023)")

# Difficultés pour renommer les colonnes 
  
# 7. VERIFIER LES VALEURS MANQUANTES 

WDPA_Senegal %>%
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
  
## 8. PRODUIRE DES CARTES 

# RENDU CARTOGRAPHIQUE (NUMERO 1)

tmap_mode(mode = "view") 
tm_shape(contour_sen) +
tm_borders() + 
tm_shape(WDPA_Senegal) + 
tm_polygons(col = "IUCN_CAT", alpha = 0.6, title = "Catégorie IUCN",
            id = "NAME", 
            popup.vars = c("Type" = "DESIG", 
                            "Catégorie IUCN" = "IUCN_CAT",
                            "Surface déclarée" = "REP_AREA",
                            "Année du statut" = "STATUS_YR"))
tmap_options(check.and.fix = TRUE)

## RENDU CARTOGRAPHIQUE (NUMERO 2)

tmap_mode(mode = "view") 
tm_shape(contour_senegal) +
  tm_borders() + 
  tm_shape(WDPA_Senegal) + 
  tm_polygons(col = "DESIG", alpha = 0.6, title = "Catégories d'aires protégées au Sénégal",
              id = "NAME", 
              popup.vars = c("Type" = "DESIG", 
                             "Catégorie IUCN" = "IUCN_CAT",
                             "Surface déclarée" = "REP_AREA",
                             "Année du statut" = "STATUS_YR"))
tmap_options(check.and.fix = TRUE)


## RENDU CARTOGRAPHIQUE (NUMERO 3) 

# On trie par années de création

tmap_mode(mode = "view") 
tm_shape(contour_sen) +
  tm_borders() + 
  tm_shape(WDPA_Senegal) + 
  tm_polygons(col = "STATUS_YR", alpha = 0.6, title = "Création d'aires protégées au Sénégal",
              id = "NAME", 
              popup.vars = c("Type" = "DESIG", 
                             "Catégorie IUCN" = "IUCN_CAT",
                             "Surface déclarée" = "REP_AREA"))

# On se rend compte qu'il y a les années 0 (NA) invalides donc on les exclut 
# On veut aussi indiquer qu'il s'agit aussi d'années dans la colonne et non de données chiffrées
# Au lieu d'avoir une légende avec des périodes, on souhaite une légende unique de la valeur minimum
# à la valeur max et un degradé de couleur (en foncé les plus anciennes)

library(dplyr)
library(lubridate)
library(tmap)

WDPA_Senegal_poly <- WDPA_Senegal %>% 
  filter(STATUS_YR != 0)  %>% 
  mutate(STATUS_YR = as.Date(STATUS_YR, origin = "1950-01-01"),
        STATUS_YR = year(STATUS_YR)) 

#Soucis au niveau de l'enregistrement en années

tmap_mode(mode = "view") 
tm_shape(contour_sen) +
  tm_borders() + 
  tm_shape(WDPA_Senegal_poly) + 
  tm_polygons(col = "STATUS_YR", alpha = 0.6, title = "Création d'aires protégées au Sénégal",
              id = "NAME", 
              popup.vars = c("Type" = "DESIG", 
                             "Catégorie IUCN" = "IUCN_CAT",
                             "Surface déclarée" = "REP_AREA"))


## RENDU CARTOGRAPHIQUE (NUMERO 4)

# Création d'intersection 

# On ne garde que les polygones (sans les points)

WDPA_Senegal_poly <- WDPA_Senegal %>% 
  filter(st_geometry_type(.) == "MULTIPOLYGON")

# On différencie par géo-traitement les aires terrestres et marines

WDPA_Senegal_poly_terrestre <- st_intersection(WDPA_Senegal_poly,contour_senegal)

st_write(WDPA_Senegal_poly_terrestre, "C:/Users/Lenaig MOIGN/Documents/R/AP_Senegal/JUMI_R/data/WDPA_terrestre.shp")

WDPA_Senegal_poly_marin <- st_difference(WDPA_Senegal_poly, contour_senegal)

# On visualise le résultat 

tmap_mode(mode = "view") 

tm_shape(contour_senegal) +
  tm_borders() + 
  tm_shape(WDPA_Senegal_poly_marin) + 
  tm_polygons(col = "DESIG", alpha = 0.6, title = "Aires marines protégées",
              id = "NAME", 
              popup.vars = c("Type" = "DESIG", 
                             "Catégorie IUCN" = "IUCN_CAT",
                             "Surface déclarée" = "REP_AREA")) + 
  tm_shape(WDPA_Senegal_poly_terrestre) + 
  tm_polygons(col = "DESIG", alpha = 0.6, title = "Aires terrestres protégées",
              id = "NAME", 
              popup.vars = c("Type" = "DESIG", 
                             "Catégorie IUCN" = "IUCN_CAT",
                             "Surface déclarée" = "REP_AREA"))

# Voir l'histoire de palettes 

# 8. PARTIE SUPERPOSITION (A FAIRE)

# On ne garde que les polygones (sans les points)

WDPA_Senegal_poly <- WDPA_Senegal %>% 
  filter(st_geometry_type(.) == "MULTIPOLYGON")

# On calcule le total des surfaces de chaque aire

surface_cumul <- WDPA_Senegal_poly_terrestre %>%
  mutate(surface = st_area(.)) %>%
  st_drop_geometry() %>% 
  summarise(surface = sum(surface, na.rm = TRUE)) %>%
  mutate(`Type de cumul` = "Somme des surfaces terrestres de chaque aire protégée",
         .before = everything())

library(units)

surface_tot <- WDPA_Senegal_poly_terrestre %>%
  st_make_valid() %>%
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

# 9. ACQUISITION DE DONNEES ENV. ET CALCUL D'INDICATEURS

library(tidyverse)
library(mapme.biodiversity)
library(sf)

#  Constitution d'un portefeuille 

WDPA_mapme <- WDPA_Senegal_poly %>% 
  st_cast("POLYGON")

WDPA_mapme <- init_portfolio(x = WDPA_mapme, 
                             years = 1980:2020,
                             outdir = "data/mapme_Senegal",
                             add_resources = TRUE,
                             verbose = TRUE)



# Données de Global Mangrove Watch (GMW) sur la perte/extension de mangroves 
# Données uniquement pour les périodes 1996, 2007-2010, 2015, and 2016

WDPA_mapme <- get_resources(x = WDPA_mapme,resources = "gmw")

available_indicators()
available_indicators(indicator = "mangroves_area")

# Indicateurs GMW

WDPA_mapme <- calc_indicators(WDPA_mapme,
                              indicators = "mangroves_area")


# Données de Global Forest Watch (GFW) sur le couver forestier

WDPA_mapme <- get_resources(x = WDPA_mapme, 
                            resources = c("gfw_treecover", "gfw_lossyear"))
# Indicateurs GFW

WDPA_mapme <- calc_indicators(WDPA_mapme,
                              indicators = "treecover_area",
                              min_cover = 30, min_size = 1)

# Données d'accessibilité de Nelson et al. (2018)

WDPA_mapme <-  get_resources(x = WDPA_mapme, resources = "nelson_et_al",  
                             range_traveltime = "5k_110mio")

?nelson_et_al

#year of 2015

# Indicateurs d'accessibilité

WDPA_mapme <- calc_indicators(x = WDPA_mapme,
                              "traveltime",  stats_accessibility = "mean",
                              engine = "extract")

# Modèle numérique de terrain SRTM de la NASA

WDPA_mapme <- get_resources(x = WDPA_mapme , resources = "nasa_srtm")

# Indicateurs de relief de terrain

WDPA_mapme <- calc_indicators(x = WDPA_mapme,
                              indicators = c("tri", "elevation"),
                              stats_tri = "mean", stats_elevation = "mean")

#ENREGISTREMENT FICHIER RDS

save(WDPA_mapme, file = "data/WDPA_indicators.rds") 


# 10. TRI DES DONNEES POUR FACILITER ANALYSE DES INDICATEURS 


# Tableau 1 : Désimbrication des indicateurs (WDPA_terrain)

WDPA_terrain <- WDPA_mapme %>%
  st_drop_geometry() %>% 
  unnest(c(tri, elevation, mangroves_area)) %>% 
  unnest(c(traveltime,treecover_area))


# Tableau 2 : Agrégation par aires protégées (pondération par la surface)

WDPA_terrain_AP <- WDPA_terrain %>% 
  select(Nom = ORIG_NAME, 
         Surface = REP_AREA, 
         Aire_marine_terrestre = MARINE, 
         Mangrove_surface = mangrove_extent, 
         Couvert_foret = treecover) %>% 
  group_by(Nom) %>% 
  mutate(Mangrove_surface = weighted.mean(Mangrove_surface), Surface, 
         Couvert_foret = weighted.mean(Couvert_foret, Surface), 
         na.rm = TRUE) %>% 
  unique() 


# Tableau 3 : Delta du Saloum ré-implantation de mangrove en 2003 (voir l'évolution? )

WDPA_DS <-  WDPA_terrain %>% 
  select(Nom = NAME, Surface = REP_AREA, Mangrove_Surface = mangrove_extent,Année = year) %>% 
  group_by(Année) %>% 
  filter(Nom == "Delta du Saloum") %>% 
  summarise(Mangrove_surface = weighted.mean(Mangrove_Surface, Surface))


# Tableau 4 : Zones où il y a eu une ré-implantation de mangroves

tableau_restauration <- WDPA_terrain %>%
  select(Nom = ORIG_NAME, 
        Surface = REP_AREA, 
        Année = year, 
        Mangrove_surface = mangrove_extent) %>% 
  group_by(Nom, Année) %>% 
  summarise(Mangrove_surface = weighted.mean(Mangrove_surface, Surface))

#Très bien on touche plus 

# # verif_restauration <- tableau_restauration %>% 
#   filter(Mangrove_surface == 0 & lead(Mangrove_surface) > 1000) 
#   select(Nom, Année)

?wdpa_fetch

#Exo 1 : montrer les années 

WDPA_terrain_AP %>% 
  filter(Couvert_foret != 0 & Mangrove_surface != 0) %>% 
  select(Nom)


## 8. EXPLORATION DES DONNÉES AIRES PROTÉGÉES COUPLÉES AUX INDICATEURS ENVIRONNEMENTAUX

Exercice 1 : quelles sont les aires protégées qui ont connu à la fois une augmentation du couvert forestier et de la surface de mangrove sur les deux dernières décénnies ? 
  
  ```{r}

WDPA_terrain_AP %>% 
  filter(Couvert_foret != 0 & Mangrove_surface != 0) %>%
  select(Nom)

```

Exercice 2 : pour l'AMP de la Somone, afficher sa surface et les indicateurs correspondants 

```{r}

WDPA_Somone <-  WDPA_terrain_AP %>% 
  filter(Nom == "Aire Marine Protégée de la Somone") %>% 
  print()
  
```

Piège : surface en km² alors que surface de la mangrove en Ha, on va donc modifier notre tableau pour convertir les Ha en km² 

```{r}

WDPA_terrain_AP$Surface_ha <- WDPA_terrain_AP$Surface * 100

```

Exercice 3 : créer une colonne présentant le pourcentage de mangrove par rapport à la superficie 

```{r}
WDPA_terrain_AP$Pourcentage_mangrove <- (WDPA_terrain_AP$Mangrove_surface / WDPA_terrain_AP$Surface_ha) * 100

```
Encore valeurs abbérantes à cause de l'affichage de la surface de mangrove, correction : (FAUX)

```{r}
WDPA_terrain_AP %>%
  mutate(Mangrove_surface_Ha = as.numeric(gsub(",", "", Mangrove_surface))) %>%
  mutate(Mangrove_surface_Ha = format(round(Mangrove_surface_Ha), big.mark = ","))

rm(WDPA_terrain_AP)

WDPA_terrain_AP %>%
  mutate(Mangrove_surface = as.numeric(Mangrove_surface),
         Surface_ha = as.numeric(Surface_ha),
         Pourcent_mangrove = ifelse(Surface_ha != 0 & !is.na(Surface_ha), (Mangrove_surface / Surface_ha) * 100, 0))

WDPA_terrain_pivot <- WDPA_terrain %>%
  group_by(ORIG_NAME, year) %>%
  summarise(treecover = mean(treecover, na.rm = TRUE)) %>% 
  pivot_wider(names_from = year, values_from = treecover, values_fill = 0)


WDPA_stats <- WDPA_stats %>%
  mutate(Mangrove_surface = as.numeric(Mangrove_surface),
         Surface_AP_Ha = as.numeric(Surface_AP_Ha),
         Pourcent_mangrove = ifelse(Surface_AP_Ha != 0 & !is.na(Surface_AP_Ha), (Mangrove_surface / Surface_AP_Ha) * 100, 0))



library(mapme.biodiversity)
detach("package:mapme.biodiversity", unload = TRUE)
remove.packages("mapme.biodiversity")
install.packages("remotes")
new <- "https://github.com/fBedecarrats/mapme.biodiversity/tree/gfw_2017_to_2020"
remotes::install_github(new, upgrade = "never")

library(mapme.biodiversity)

?tm_polygons()
?gfw_lossyear

# TEST IMPORTANT DONNEES PERTE FORESTIERE 


