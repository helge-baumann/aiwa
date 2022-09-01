pac <- "pacman" %in% rownames(installed.packages())
if(pac == FALSE) install.packages("pacman"); rm(pac)
library(pacman)
p_load("stringr", "haven", "sp", "rgdal", "leaflet", "styler", 
       "geosphere", "htmltools", "ggplot2", "ggmap", "rgeos", "maptools", 
       "openxlsx","extrafont", "htmlwidgets", "leaflet.extras", "dplyr",
       "scales", "widgetframe", "DT", "shinythemes", "shinyWidgets",
       "shinypanels", "shinyjs", "waiter", "purrr") # rgeos vor maptools!

Data <- readRDS(dir("data/", full.names=T)[length(dir("data/", full.names=T))])
source("./Funktionen/funktionen_karten.R")