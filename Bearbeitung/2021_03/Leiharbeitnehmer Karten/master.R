################################################################################
# Statische und interaktive Karten (Vorlage)
# Autor: Helge Emmler
# R Version: 4.0.3
# Letztes Update: 22.01.2021
################################################################################

# (1) Präambel----

# Pakete laden
pac <- "pacman" %in% rownames(installed.packages())
if(pac == FALSE) install.packages("pacman"); rm(pac)
library(pacman)
p_load("stringr", "haven", "sp", "rgdal", "leaflet", "styler", 
       "geosphere", "htmltools", "ggplot2", "ggmap", "rgeos", "maptools", 
       "openxlsx","extrafont", "htmlwidgets", "leaflet.extras", "dplyr",
       "scales", "widgetframe", "rmapshaper") # rgeos vor maptools!
loadfonts()

# (2) Definitionen----

# Ebene? 
ebene <- "AMR" # KRS, AMR, LAN
key <- "SN_AMR" # analog zu ebene: AGS, AMR, GEN

# Dateinamen
# Name für statische Kreis-Karte
name_s <- paste0("wsi_aiwa_leiharbeit")
# Name für interaktive Kreis-Karte
name_i <- paste0("wsi_aiwa_leiharbeit")

# Farben für Regionen (zulässig: lila, pink, grau, tuerkis, blau)
col_s <- "tuerkis"
col_i <- "tuerkis"

# (3) Schritte----

# Funktionen einlesen

dir.create("./Output/", showWarnings=F)
dir.create("./Output/Karten", showWarnings=F)

source("./Funktionen/funktionen_karten.R", encoding="UTF-8")

# shapefile Download (nur wenn nicht vorhanden)
if(!("shapefiles" %in% dir("./Shapefiles"))) {
  download_shapefile(shape="amr") # krs_neu, krs_alt (2016), amr (Arbm-reg.)
  download_shapefile(shape="krs_neu") # krs_neu, krs_alt (2016), amr (Arbm-reg.)
}

for(jahr in c(2015, 2016, 2017, 2018, 2020)) {
# Automatische Ausführung:
n <- 1:3#length(dir("./Schritte"))
sapply(dir("./Schritte", full.names=T)[n], source, encoding="UTF-8")
}

source("./Schritte/4_interaktiv_gesamt.R", encoding="UTF-8")

# (4) Session Info----
writeLines(
  capture.output(sessionInfo()), 
  con=paste0("./Session_Info/sessionInfo_",format(Sys.time(), "%y%m%d"), ".txt")
)




