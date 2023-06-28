install.packages("openxlsx")
library(openxlsx)
library(readr)

# install.packages(c("tidyverse","sf","here","jsonlite","leaflet","ggthemes","tmap","tmaptools","mapsf","geojsonsf"))

library(tidyverse)
library(sf)
library(here)
library(jsonlite)
library(leaflet)
library(ggthemes)
library(tmap)
library(tmaptools)
library(mapsf)
library(geojsonsf)



chemin_xlsx <-"/data/donnees_detaillees_ra_1852"

# nom_fichier <- c("animaux.xlsx", "cereales.xlsx", "cultures_diverses.xlsx", "descriptif_detaille_recensement_1852.xlsx", "economie_rurale_1852.xlsx", "sericulture.xlsx")
# nom_fichier_complet <- paste0(getwd(),chemin_xlsx,nom_fichier)

fichier_animaux<-read.xlsx(paste0(getwd(),chemin_xlsx,"/animaux.xlsx"))%>% 
  mutate(detail_loc = str_trim(detail_loc))
fichier_cereales<-read.xlsx(paste0(getwd(),chemin_xlsx,"/cereales.xlsx"))%>% 
  mutate(detail_loc = str_trim(detail_loc))
fichier_cult_div<-read.xlsx(paste0(getwd(),chemin_xlsx,"/cultures_diverses.xlsx"))%>% 
  mutate(detail_loc = str_trim(detail_loc))
fichier_descriptif<-read.xlsx(paste0(getwd(),chemin_xlsx,"/descriptif_detaille_recensement_1852.xlsx"))
fichier_eco_rural<-read.xlsx(paste0(getwd(),chemin_xlsx,"/economie_rurale_1852.xlsx"))%>% 
  mutate(detail_loc = str_trim(detail_loc))
fichier_sericulture<-read.xlsx(paste0(getwd(),chemin_xlsx,"/sericulture.xlsx"))



surfaces_ra1852 <- readr::read_delim(here('data/cultures_diverses_RA1852_recap.csv'), 
                                     delim=';',  
                                     skip = 0) %>% 
  mutate(canton = str_trim(canton)) ## supprimer les espaces en fin et en début de chaine

summary(surfaces_ra1852)
head(surfaces_ra1852)

index_surface_culture<- fichier_descriptif %>%
                        filter(INTITULE1=="nombre d'hectares cultivés en 1852") %>%
                        select(NUMERO.VARIABLE) %>%
                        sapply(as.character) %>%
                        as.vector


# surface_cereales <- fichier_cereales %>% 
#                     filter(detail_loc=="ROMORANTIN") %>%
#                   summarise(across(all_of(index_surface_culture), ~ sum(., na.rm = TRUE))) 

#Pour vérifier la somme des surfaces cultures

sum(fichier_cereales[fichier_cereales$detail_loc=="ROMORANTIN",index_surface_culture])
somme_surface_culture<-rowSums(fichier_cereales[,index_surface_culture])


#même chose avec dplyr
ibrary(sjmisc) 
rotate_df() 
test <- fichier_cereales %>% 
  filter(canton=="ROMORANTIN") %>% 
  select(-c(code_dept,detail_loc,dept)) %>% 
  relocate (canton, .before=1) %>% 
  rotate_df(rn = "canton", cn = TRUE) 

surf_cereales_romoratin <- fichier_descriptif %>% 
  select(NUMERO.VARIABLE,INTITULE1) %>% 
  mutate(NUMERO.VARIABLE=as.character(NUMERO.VARIABLE)) %>% 
  full_join(test, by=c("NUMERO.VARIABLE"="canton")) %>% 
  summarise(surf=sum(ROMORANTIN[INTITULE1=="nombre d'hectares cultivés en 1852"],na.rm=T))



## Partie 2 : API

# Pour commencer, regardons comment marche l'api. Nous vous proposons de consulter le site : <https://geo.api.gouv.fr/decoupage-administratif/communes>


url = "https://geo.api.gouv.fr/communes?nom=%s&codeDepartement=%s&fields=centre&boost=population&limit=1"
url_sans_departement = "https://geo.api.gouv.fr/communes?nom=%s&fields=centre&boost=population&limit=1"

# On utilise la fonction sprintf qui prend une chaine de caractères 'url' et qui va remplacer les %s par les paramètres passées ensuite dans l'ordre. Le premier %s est remplacé par "Toulouse), le second par "31"
url_completed <- sprintf(url, "Toulouse", "31")

# on interroge l'API qui est renvoit un JSON qui est convertie en dataframe grâce à la fonction fromJSON
resultat <-  fromJSON(url_completed)
print(resultat)
print(resultat$centre)


surfaces_ra <- surfaces_ra1852 %>% 
  filter(!is.na(departement))

library(stringi)
#récupérer la liste des codes départements
liste_dep <- fromJSON("https://geo.api.gouv.fr/departements")
liste_dep2 <- liste_dep %>%
  mutate(dep_sans_accent=stri_trans_general(str = nom, 
                                            id = "Latin-ASCII"),
         dep=toupper(dep_sans_accent)) %>%
  select(code, dep)


  
surfaces <- surfaces_ra %>%
  rename(code_canton = code) %>%
  left_join(liste_dep2, by=c("departement" = "dep")) %>%
  mutate(coordonnees_x = 0, coordonnees_y = 0)
  

surfaces$canton <- str_replace_all(string=surfaces$canton, pattern = " ", "%20")

surfaces[surfaces$canton=="LA%20PALISSE","canton"]<-"LAPALISSE"
surfaces[surfaces$canton=="PONTS%20L'EVEQUE","canton"]<-"PONT%20L'EVEQUE"
surfaces[surfaces$canton=="ISSINGEAUX","canton"]<-"YSSINGEAUX"
surfaces[surfaces$canton=="CHALONS%20SUR%20MARNE","canton"]<-"CHALONS%20EN%20CHAMPAGNE"
surfaces[surfaces$canton=="NAPOLEONVILLE","canton"]<-"PONTIVY"
surfaces[surfaces$canton=="BRIEY","code"]<-NA
surfaces[surfaces$canton=="SCHLESTADT","canton"]<-"SELESTAT"
surfaces[surfaces$canton=="CASTEL%20SARRASIN","canton"]<-"CASTEL-SARRAZIN"
surfaces[surfaces$canton=="GRASSE","code"] <- NA
surfaces[surfaces$canton=="NAPOLEON%20VENDEE","canton"]<-"LA%20ROCHE-SUR-YON"
surfaces[surfaces$canton=="REMIRMONT","canton"]<-"REMIREMONT"



  

  
for (i in 1:nrow(surfaces)){
  
    if (!is.na(surfaces[i,"code"])){
      url_completed <- sprintf(url, surfaces[i,"canton"], surfaces[i,"code"])
    } else {
      url_completed <- sprintf(url_sans_departement, surfaces[i,"canton"])
    }
  
  resultat <-  fromJSON(url_completed)
  print(surfaces[i,"canton"])
  print(resultat$centre$coordinates)
  surfaces[i,"coordonnees_x"] <- unlist(resultat$centre$coordinates)[1]
  surfaces[i,"coordonnees_y"] <- unlist(resultat$centre$coordinates)[2]
  
}
                    

# Carte

map_leaflet <- leaflet(surfaces) %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  setView(lng = 2.80, lat = 46.80, zoom = 5) %>%
  addMarkers(~coordonnees_x, ~coordonnees_y,label=~as.character(canton))
map_leaflet



# pour travailler sur des aspects géographiques dont les voronoi il faut passer du dataframe au géo dataframe
surfaces_ra1852_geo <-
  st_as_sf(
    surfaces %>% drop_na(c(coordonnees_x, coordonnees_y)),
    coords = c("coordonnees_x", "coordonnees_y"),
    crs = 4326
  )

surfaces$canton[! (surfaces$canton %in% surfaces_ra1852_geo$canton)]

# # on charge la projection pour le mettre en Lambert-93
surfaces_ra1852_geo <- st_transform(surfaces_ra1852_geo, crs = 2154)

# # On utilise les fonctions de sf
voronoi_surfaces_ra1852 <- surfaces_ra1852_geo %>%
  st_union() %>% ## permet de passer une simple à un seul objet géométrique avec tous les points
  st_voronoi() %>% ## calcul du voronoi
  st_collection_extract()  ## extraction en une liste avec les 363 figures géométriques

# # Visualisation simple
plot(voronoi_surfaces_ra1852)

dep <- sf::read_sf(here('data/dep2021_simplify2.json'), crs=2154) 
### enlever outre-mer
dep_metro <- dep %>% 
  rowwise() %>% 
  filter(nchar(dep)==2) %>% 
  ungroup()

### ne garder que les coutours de la France
dep_union <- st_union(dep_metro)
plot(dep_union)

### ne garder les voronoi uniquement qui sont en intersection avece les contours de la France
voronoi_surfaces_ra1852 = st_intersection(voronoi_surfaces_ra1852, dep_union)
plot(voronoi_surfaces_ra1852)



data_geo <- data_frame(id=1:length(voronoi_surfaces_ra1852))
data_geo<-bind_cols(voronoi_surfaces_ra1852,data_geo)
data_geo <- st_as_sf(data_geo)


surfaces_ra1852_geo <- st_transform(surfaces_ra1852_geo, crs = 2154)
data_carto <- data_geo %>% 
  st_join(surfaces_ra1852_geo)

carte_canton_sans_contrainte <- data_carto %>% 
  ggplot() +
  geom_sf(color="grey",size=.2)+
  geom_sf(data = dep_metro, linetype="dotted", color = "red", fill = NA) +
  theme_map()
carte_canton_sans_contrainte


##################################
#créer contour voronoi sous contrainte de respecter les limites départementales

#ajout des départements non connus
surfaces_ra1852_geo_complet <-surfaces_ra1852_geo
surfaces_ra1852_geo_complet <- surfaces_ra1852_geo_complet %>%
  mutate(code= case_when(departement=="BASSES ALPES" ~ "04"
                         ,departement=="HAUTES ALPES" ~ "05"
                         ,departement=="BOUCHES DU RHONE" ~ "13"
                         ,departement=="CHARENTE INFERIEURE" ~ "17"
                         ,canton=="BASTIA" ~ "2B"
                         ,canton=="AJACCIO" ~ "2A"
                         ,canton=="CALVI" ~ "2B"
                         ,canton=="CORTE" ~ "2B"
                         ,canton=="SARTENE" ~ "2A"
                         ,departement=="COTE D'OR" ~ "21"
                         ,departement=="COTES DU NORD" ~ "22"
                         ,departement=="EURE ET LOIR" ~ "28"
                         ,departement=="HAUTE GARONNE" ~ "31"
                         ,departement=="ILLE ET VILAINE" ~ "35"
                         ,departement=="INDRE ET LOIRE" ~ "37"
                         ,departement=="LOIR ET CHER" ~ "41"
                         ,departement=="HAUTE LOIRE" ~ "43"
                         ,departement=="LOIRE INFERIEURE" ~ "44"
                         ,departement=="LOT ET GARONNE" ~ "47"
                         ,departement=="MAINE ET LOIRE" ~ "49"
                         ,departement=="HAUTE MARNE" ~ "52"
                         ,departement=="MEURTHE" ~ "54"
                         ,departement=="MOSELLE" ~ "54"
                         ,departement=="PAS DE CALAIS" ~ "62"
                         ,departement=="PUY DE DOME" ~ "63"
                         ,departement=="BASSES PYRENEES" ~ "64"
                         ,departement=="HAUTES PYRENEES" ~ "65"
                         ,departement=="PYRENEES ORIENTALES" ~ "66"
                         ,departement=="BAS RHIN" ~ "67"
                         ,departement=="HAUT RHIN" ~ "68"
                         ,departement=="HAUTE SAONE" ~ "70"
                         ,departement=="SAONE ET LOIRE" ~ "71"
                         ,departement=="SEINE" ~ "75"
                         ,departement=="SEINE INFERIEURE" ~ "76"
                         ,departement=="SEINE ET MARNE" ~ "77"
                         ,canton=="CORBEIL" ~ "91"
                         ,canton=="ETAMPES" ~ "91"
                         ,canton=="MANTES" ~ "78"
                         ,canton=="PONTOISE" ~ "95"
                         ,canton=="RAMBOUILLET" ~ "78"
                         ,canton=="VERSAILLES" ~ "78"
                         ,departement=="DEUX SEVRES" ~ "79"
                         ,departement=="TARN ET GARONNE" ~ "82"
                         ,departement=="VAR" ~ "83"
                         ,departement=="HAUTE VIENNE" ~ "87"
                         , TRUE ~code))

# surf_sans_dep <- surfaces_ra1852_geo_complet %>% select (departement, code) %>% filter(is.na(code))

  


#test avec le finistère
# data_finistere <- surfaces_ra1852_geo %>% filter(code=="29")
# voronoi_surfaces_ra1852_finistere <- data_finistere %>%
#   st_union() %>% ## permet de passer une simple à un seul objet géométrique avec tous les points
#   st_voronoi() %>% ## calcul du voronoi
#   st_collection_extract()
# 
# plot(voronoi_surfaces_ra1852_finistere)
# 
# dep_finistere <- dep %>% filter (dep=="29")
# dep_union_finistere <- st_union(dep_finistere)
# 
# plot(dep_union_finistere)
# 
# voronoi_surfaces_ra1852_finistere = st_intersection(voronoi_surfaces_ra1852_finistere, dep_union_finistere)
# plot(voronoi_surfaces_ra1852_finistere)

#voronoi sur chaque departement
distinct_dep <-  unlist(surfaces_ra1852_geo_complet %>% distinct(code))
voronoi_tronque <- vector("list", 1)
for (codeDep in distinct_dep){
  print(codeDep)
  data_dep <- surfaces_ra1852_geo_complet %>% filter(code==codeDep)
  voronoi_surfaces_ra1852_dep <- data_dep %>%
    st_union() %>% ## permet de passer une simple à un seul objet géométrique avec tous les points
    st_voronoi() %>% ## calcul du voronoi
    st_collection_extract()

  # plot(voronoi_surfaces_ra1852_finistere)

  contour_dep <- dep %>% filter (dep==codeDep)
  contour_dep_union <- st_union(contour_dep)

  # plot(dep_union_finistere)

  voronoi_surfaces_ra1852_dep = st_intersection(voronoi_surfaces_ra1852_dep, contour_dep_union)
  # plot(voronoi_surfaces_ra1852_finistere)

  voronoi_tronque <-append(voronoi_tronque,voronoi_surfaces_ra1852_dep)
  
}
voronoi_tronque_2<-voronoi_tronque[-1]

voronoi_tronque_3 <-
  st_as_sfc(
    voronoi_tronque_2,
    coords = c("coordonnees_x", "coordonnees_y"),
    crs = 2154
  )
plot(voronoi_tronque_3)

data_geo_tronque <- data_frame(id=1:length(voronoi_tronque_3))
data_geo_tronque<-bind_cols(voronoi_tronque_3,data_geo_tronque)
data_geo_tronque <- st_as_sf(data_geo_tronque)
data_carto_tronque <- data_geo_tronque %>% 
  st_join(surfaces_ra1852_geo)


carte_canton_avec_contrainte_dep <- data_carto_tronque %>% 
  ggplot() +
  geom_sf(color="grey",size=.2)+
  scale_fill_gradient(name = "Part des terres\nlabourables en % \nde la surface totale", limits = c(0, 100), 
                      low = "white", high = "forestgreen") +
  geom_sf(data = dep_metro, linetype="dotted", color = "red", fill = NA) +
  theme_map()
carte_canton_avec_contrainte_dep



carte_sans_dep <- data_carto %>% 
  ggplot() +
  geom_sf(color="grey",size=.2)+
  scale_fill_gradient(name = "Part des terres\nlabourables en % \nde la surface totale", limits = c(0, 100), 
                      low = "white", high = "forestgreen") +
  theme_map()
carte_sans_dep







