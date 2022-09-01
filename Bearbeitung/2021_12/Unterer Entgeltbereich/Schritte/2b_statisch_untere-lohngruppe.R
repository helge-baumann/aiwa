# Statische Karte Kreise

# Annahme über Dateipfad siehe funktionen/funktionen_karten.R
geo <- load_shapefile(path=NULL, level=ebene) # KRS, LAN, AMR
geo <- merge(geo, dat, by=key) # dat siehe 1_read.R

# Land
if(ebene %in% c("KRS", "AMR")) {
  geo_land <- load_shapefile(path=NULL, level="LAN")
  geo_land <- fortify(geo_land, region="GEN")
}

# Farbenpalette: Woran geknüpft?
geo$gruppe <- geo$`Gruppe 2020`
colfunc <- colorFactor(farben("tuerkis"), domain=geo$gruppe, ordered=T)
geo$color <- colfunc(geo$gruppe)

# Aufbereitung für ggplot2
geo.merge <- geo[,c(key, "gruppe", "color")]
geo <- fortify(geo, region=key)
geo <- merge(geo, geo.merge, by.x="id", by.y=key)

# Karte zeichnen und speichern (Funktion siehe Funktionen)
karte <- static_map(
  geo,
  titel = "Vollzeitbeschäftigte unter 2.284 Euro, Dez 2020", 
  untertitel = "Anteil Vollzeitbeschäftigte unter 2.284 Euro, in %", 
  caption = "", leg = "") 

# Ländergrenzen
if(ebene %in% c("KRS", "AMR")) {
  
  karte <- karte +
    geom_polygon(data = geo_land, color = "white", size = 0.3, fill=NA)
  
}

pdf(file=paste0("./Output/Karten/", name_s2, ".pdf"), width=4, height=7)
print(karte)
dev.off()

for(b in c("pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf" )) {
  
  ggsave(
    paste0("./Output/Karten/", "test", ".", b),
    plot = karte,
    device = b,
    path = NULL,
    scale = 1,
    width = 4,
    height = 7,
    units = c("in"),
    dpi = 300,
    limitsize = TRUE,
    bg = NULL
  )
}

