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
colfunc <- colorFactor(farben(col_s), domain=geo$gruppe, ordered=T)
geo$color <- colfunc(geo$gruppe)

# Aufbereitung für ggplot2
geo.merge <- geo[,c(key, "gruppe", "color")]
geo <- fortify(geo, region=key)
geo <- merge(geo, geo.merge, by.x="id", by.y=key)

# Karte zeichnen und speichern (Funktion siehe Funktionen)
karte <- static_map(
  geo,
  titel = paste0("Leiharbeit in den Arbeitsmarktregionen ", jahr), 
  untertitel = "Anteil der Beschäftigten in der Leiharbeit* an allen Beschäftigten*, in Prozent", 
  caption = "* bezieht sich auf den Wohnort", leg = "Legendentitel") 

# Ländergrenzen
if(ebene %in% c("KRS", "AMR")) {
  
  karte <- karte +
    geom_polygon(data = geo_land, color = "white", size = 0.3, fill=NA)
  
}

pdf(file=paste0("./Output/Karten/", name_s, "_", jahr, ".pdf"), width=4, height=7)
print(karte)
dev.off()
