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

                    


