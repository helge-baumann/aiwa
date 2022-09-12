# Hintergrunddaten laden -----

# Annahme über Dateipfad siehe funktionen/funktionen_karten.R
geo_base <- load_shapefile(path=NULL, level=ebene)

# Ländergrenzen (für später, um sie einzuzeichnen)
if(ebene %in% c("KRS", "AMR")) {
  geo_land <- load_shapefile(path=NULL, level="LAN")
}

geo_base <- ms_simplify(geo_base, keep_shapes=T)
geo_base <- spTransform(geo_base, CRS("+proj=longlat"))
geo_land <- ms_simplify(geo_land, keep_shapes=T)
geo_land <- spTransform(geo_land, CRS("+proj=longlat"))

geo <- merge(geo_base, dat, by=key) # dat siehe 1_read.R

# Farbenpalette: Woran geknüpft?
colfunc <- colorFactor(farben(col_i), domain=geo$`Gruppe`, ordered=T)
geo$color <- colfunc(geo$`Gruppe`)

geo$GEN[geo$GEN == "Eisenach"] <- "Wartburgkreis (inkl. Eisenach)"
geo$GEN[geo$GEN == "Wartburgkreis"] <- "Wartburgkreis (inkl. Eisenach)"

# Karte zeichnen-------

# Karte
geo$gruppe <- geo$`Gruppe`

fig <- leaflet_basis(geo) %>% 
  leaflet_text("Beschäftigte mit Bruttostundenlohn < 12 Euro, Ende Sept. 2022", size=20, group="2022") %>%
  leaflet_text(lat=55.25, 
               "Anteil aller mindestlohnberechtigten Beschäftigten, in Prozent", size=15) %>%
  leaflet_legend(colors = colfunc(levels(geo$gruppe)),
                 lvl = levels(geo$gruppe),
                 group="2022") %>%
  leaflet_logo() %>%
  leaflet_quelle(Text="Quelle: <br> Statistik der Bundesagentur für Arbeit, <br> Statistisches Bundesamt, <br> SOEP, eigene Berechnungen.") %>%
  leaflet_anmerkung(Text="Mehr Informationen erhalten Sie per Mausklick auf die Kreise!") %>%
  leaflet_polygons(
    colors=colfunc(levels$geo_gruppe),
    bg = colfunc(levels(geo$gruppe)),
    variablen=c("Quote", "Anzahl"),
    headers=c("Beschäftigungsanteil", "Beschäftigte < 12 Euro"),
    labtext ="Beschäftigte < 12 Euro",
    k=c(1,0), # Nachkommastellen
    group="2022", suffix=c(" %", "")
  ) 


# Ländergrenzen (nur bei Kreisen, Bezirken, Arbeitsmarktregionen sinnvoll)
if(ebene %in% c("KRS", "AMR")) {
  for(b in as.character(2022)) {
    fig <- fig %>%
      addPolygons(
        data = geo_land,
        color = "white", weight = 2, smoothFactor = 0.5, 
        opacity = 1.0, fillOpacity = 1.0,
        fill = F,
        group=b
      )
  }
  
}


# Leaflet-Stil anwenden und in HTML überführen----
fig$dependencies <- list(
  htmlDependency(
    name = "leaflet_hbs"
    ,version = "1.0.0"
    # if local file use file instead of href below
    #  with an absolute path
    ,src = paste0(getwd(),"/CSS/Leaflet-Stil/")
    ,stylesheet = "rstudio_leaflet_helge.css"
  )
)

# abspeichern
saveWidget(
  frameableWidget(fig), 
  file = paste0(getwd(), "/Output/Karten/", name_i, ".html"),
  selfcontained=F
)

# mitliefern: WSI-Logo
file.copy(
  from="./CSS/WSI_Abbinder_RGB.jpg",
  to="./Output/Karten"
)



