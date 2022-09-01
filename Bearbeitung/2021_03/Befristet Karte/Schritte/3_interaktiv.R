
# Hintergrunddaten laden -----

# Annahme über Dateipfad siehe funktionen/funktionen_karten.R
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

dat <- dat19 %>% left_join(dat20, by="AGS") 
geo <- merge(geo_base, dat, by=key) # dat siehe 1_read.R

# Farbenpalette: Woran geknüpft?
colfunc <- colorFactor(farben(col_i), domain=geo$`Gruppe 2020`, ordered=T)
geo$color19 <- colfunc(geo$`Gruppe 2019`)
geo$color20 <- colfunc(geo$`Gruppe 2020`)

# Karte zeichnen-------

# Karte
geo$gruppe <- geo$`Gruppe 2020`
geo$color <- geo$color20

fig <- leaflet_basis(geo) %>% 
  leaflet_text("Befristete Einstellungen, Juni 2020", size=20, group="2020") %>%
  leaflet_text(lat=55.25, 
               "Anteil befristeter Einstellungen an allen Einstellungen in der SvB-Kerngruppe, in Prozent", size=15) %>%
  leaflet_legend(colors = colfunc(levels(geo$gruppe)),
                 lvl = levels(geo$gruppe),
                 group="2020") %>%
  leaflet_logo() %>%
  leaflet_quelle(Text="Quelle: <br> Sonderauswertung der <br> Bundesagentur für Arbeit, eigene Berechnungen") %>%
  leaflet_anmerkung(Text="Mehr Informationen erhalten Sie per Mausklick auf die Kreise!") %>%
  leaflet_polygons(
    colors=colfunc(levels(geo$gruppe)),
    bg = colfunc(levels(geo$gruppe)),
    variablen=c("Gesamt (2020)", "Männer (2020)", "Frauen (2020)"),
    headers=c("Gesamt (2020)", "Männer (2020)", "Frauen (2020)"),
    k=1, # Nachkommastellen
    group="2020", suffix="%"
  ) 

geo$gruppe <- geo$`Gruppe 2019`
geo$color <- geo$color19

fig <- fig %>%
  leaflet_text("Befristete Einstellungen, Juni 2019", size=20, group="2019") %>%
  leaflet_polygons(
    labvar = 1, 
    colors=colfunc(levels(geo$gruppe)),
    bg = colfunc(levels(geo$gruppe)),
    variablen=c("Gesamt (2019)", "Männer (2019)", "Frauen (2019)"),
    headers=c("Gesamt (2019)", "Männer (2019)", "Frauen (2019)"),
    k=1, # Nachkommastellen
    group="2019", suffix="%"
  ) %>%
  leaflet_legend(colors = colfunc(levels(geo$gruppe)), group="2019")

# Ländergrenzen (nur bei Kreisen, Bezirken, Arbeitsmarktregionen sinnvoll)
if(ebene %in% c("KRS", "AMR")) {
  fig <- fig %>%
    addPolygons(
      data = geo_land,
      color = "white", weight = 2, smoothFactor = 0.5, 
      opacity = 1.0, fillOpacity = 1.0,
      fill = F,
      group="2020"
    )
  fig <- fig %>%
    addPolygons(
      data = geo_land,
      color = "white", weight = 2, smoothFactor = 0.5, 
      opacity = 1.0, fillOpacity = 1.0,
      fill = F,
      group="2019"
    )
  
}

fig <- fig %>%
  addLayersControl(
    baseGroups=c("2020", "2019"),
    position = "bottomright",
    options = layersControlOptions(collapsed = FALSE)
  )

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



