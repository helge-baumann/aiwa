##############################################################
# Titel: Ziel der Analyse
# Autor: 
# Letztes Update: 17.02.2021
##############################################################


# Präambel: Pakete installieren
  if(!("pacman" %in% installed.packages()[,1])) install.packages("pacman")
  library(pacman)

  # p_load checkt, ob Paket installiert ist; wenn ja: direkt laden
  p_load(haven, dplyr, ggplot2, tidyr, zoo, purrr, tibble, here, openxlsx, 
         stringr)

  
# Alle Unterdateien ausführen

  # selbstgeschriebene Funktionen
  source("./Data preparation/Functions/functions.R", encoding="UTF-8")
  
  # alle Unterdateien im UNterordner "Steps" der Reihe nach ausf?hren
  n <- 1:length(dir("./Data preparation/Steps"))
  sapply(dir("./Data preparation/Steps", full.names=T)[n], source, encoding="UTF-8")

  
# sessionInfo (geladene Pakete etc. f?r Fehlersuche)
  dir.create("./Data preparation/sessionInfo()", showWarnings=F)
  writeLines(
    capture.output(sessionInfo()), 
    con=paste0("./Data preparation/sessionInfo()/", format(Sys.time(), "%y%m%d"), ".txt")
  )