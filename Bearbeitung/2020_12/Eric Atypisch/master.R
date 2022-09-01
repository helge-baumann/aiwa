################################################################################
# Interaktive Karten
# Autor: Helge Emmler
# R Version: 4.0.3
# Letztes Update: 08.12.2020
################################################################################

# (1) Präambel----

# Dateinamen
name_mjh <- "wsi_aiwa_minijobs_haupterwerb"
name_mjn <- "wsi_aiwa_minijobs_nebenerwerb"
name_bef <- "wsi_aiwa_befristet"
name_lei <- "wsi_aiwa_leiharbeit"
name_sol <- "wsi_aiwa_solo_selbstaendige"
name_vol <- "wsi_aiwa_volumina"
  
# Farben (zulässig: lila, pink, grau, tuerkis, blau)
col_mjh <- "lila"
col_mjn <- "pink"
col_bef <- "grau"
col_lei <- "tuerkis"
col_sol <- "blau"
col_vol <- "lila"

# Pakete laden
pac <- "pacman" %in% rownames(installed.packages())
if(pac == FALSE) install.packages("pacman"); rm(pac)
library(pacman)
p_load("stringr", "haven", "sp", "Hmisc", "rgdal", "leaflet", "styler", "geosphere",
       "htmltools", "ggplot2", "ggmap", "rgeos", "maptools", "xlsx", "openxlsx",
       "extrafont", "htmlwidgets", "leaflet.extras", "dplyr", "scales",
       "widgetframe") # rgeos vor maptools!

loadfonts()

# (2) Schritte----
n <- 1:length(dir("./Schritte"))
sapply(dir("./Schritte", full.names=T)[n], source, encoding="UTF-8")

# (3) Session Info----
writeLines(
  capture.output(sessionInfo()), 
  con=paste0("./Session_Info/sessionInfo_",format(Sys.time(), "%y%m%d"), ".txt")
)




