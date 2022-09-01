################################################################################
# neue Karten AIWA
# Autor: Helge Emmler
# R Version: 3.6.3
# Letztes Update: 03.09.2020
################################################################################

# (1) Präambel----

# Paket-Directory einrichten
.libPaths("C:/R/library")

# Pakete laden
pac <- "pacman" %in% rownames(installed.packages())
if(pac == FALSE) install.packages("pacman"); rm(pac)
library(pacman)
p_load("stringr", "haven", "sp", "Hmisc", "rgdal", "leaflet", "styler", 
       "htmltools", "ggplot2", "ggmap", "rgeos", "maptools", "openxlsx", 
       "extrafont", "htmlwidgets", "leaflet.extras", "dplyr",
       "widgetframe") # rgeos vor maptools!

loadfonts()

# (1) Schritte----
n <- 1:5
sapply(dir("./Schritte", full.names=T)[n], source, encoding="UTF-8")

# (2) Session Info----
writeLines(
  capture.output(sessionInfo()), 
  con=paste0("./Session_Info/sessionInfo_",format(Sys.time(), "%y%m%d"), ".txt")
)




