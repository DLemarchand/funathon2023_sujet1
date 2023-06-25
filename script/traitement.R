library(openxlsx)

chemin_xlsx <-"/data/donnees_detaillees_ra_1852"

fichier_animaux<-read.xlsx(paste0(getwd(),chemin_xlsx,"/animaux.xlsx"))