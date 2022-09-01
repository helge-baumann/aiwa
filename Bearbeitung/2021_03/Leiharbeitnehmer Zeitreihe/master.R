##############################################################
# Titel: Aggregation Leiharbeitnehmer Arbeitsmarktregionen
# Autor: Helge Emmler
# Letztes Update: 05.03.2021
##############################################################


# Präambel: Pakete installieren
  if(!("pacman" %in% installed.packages()[,1])) install.packages("pacman")
  library(pacman)

  # p_load checkt, ob Paket installiert ist; wenn ja: direkt laden
  p_load(haven, dplyr, ggplot2, tidyr, zoo, purrr, tibble, here, openxlsx,
         stringr)
  
  # here() setzt das Arbeitsverzeichnis an die Stelle des master-files.
  # (Alternative: Projekt anlegen unter File --> New Project)

  # wiss. Notation ändern
  options(scipen=999)
  
# Alle Unterdateien ausführen

  # selbstgeschriebene Funktionen
  source("./Functions/functions.R", encoding="UTF-8")
  
  # alle Unterdateien im UNterordner "Steps" der Reihe nach ausf?hren
  n <- 1:length(dir("./Steps"))
  sapply(dir("./Steps", full.names=T)[n], source, encoding="UTF-8")

  
# sessionInfo (geladene Pakete etc. f?r Fehlersuche)
  dir.create("./sessionInfo()", showWarnings=F)
  writeLines(
    capture.output(sessionInfo()), 
    con=paste0("./sessionInfo()/", format(Sys.time(), "%y%m%d"), ".txt")
  )