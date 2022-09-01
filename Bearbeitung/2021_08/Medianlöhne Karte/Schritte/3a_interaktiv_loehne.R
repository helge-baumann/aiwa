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

geo <- merge(geo_base, dat_gesamt, by=key) # dat siehe 1_read.R

# Farbenpalette: Woran geknüpft?
colfunc <- colorFactor(farben(col_i), domain=geo$`Gruppe 2020`, ordered=T)
geo$color16 <- colfunc(geo$`Gruppe 2016`)
geo$color17 <- colfunc(geo$`Gruppe 2017`)
geo$color18 <- colfunc(geo$`Gruppe 2018`)
geo$color19 <- colfunc(geo$`Gruppe 2019`)
geo$color20 <- colfunc(geo$`Gruppe 2020`)


# Karte zeichnen-------

# Karte
geo$gruppe <- geo$`Gruppe 2020`
geo$color <- geo$color20

fig <- leaflet_basis(geo) %>% 
  leaflet_text("Bruttolöhne, Dezember 2020", size=20, group="2020") %>%
  leaflet_text(lat=55.25, 
               "<div style='padding-top: 2%; line-height:90%'>Mittlere monatliche Bruttoarbeitsentgelte der 
  sozialversicherungspflichtig <br>Vollzeitbeschäftigten der Kerngruppe, in Euro</div>", size=15) %>%
  leaflet_legend(colors = colfunc(levels(geo$gruppe)),
                 lvl = levels(geo$gruppe),
                 group="2020") %>%
  leaflet_logo() %>%
  leaflet_quelle(Text="Quelle: <br> Bundesagentur für Arbeit (2020)") %>%
  leaflet_anmerkung(Text="Mehr Informationen erhalten Sie per Mausklick auf die Kreise!") %>%
  leaflet_polygons(
    colors=colfunc(levels$geo_gruppe),
    bg = colfunc(levels(geo$gruppe)),
    variablen=c("Gesamt (2020)", "Männer (2020)", "Frauen (2020)"),
    headers=c("Gesamt (2020)", "Männer (2020)", "Frauen (2020)"),
    k=0, # Nachkommastellen
    group="2020", suffix="Euro"
  ) 


for(i in 1:4) {

geo$gruppe <- geo[[paste0("Gruppe 20", 20-i)]] 
geo$color <- geo[[paste0("color", as.character(20-i))]]

fig <- fig %>% 
  leaflet_text(paste0("Bruttolöhne, Dezember 20", 20-i), size=20, group=as.character(2020-i)) %>%
  #leaflet_text(lat=55.25, 
     #          "<div style='padding-top: 2%; line-height:90%'>Mittlere monatliche Bruttoarbeitsentgelte der 
  #sozialversicherungspflichtig <br>Vollzeitbeschäftigten der Kerngruppe, in Euro</div>", size=15) %>%
  leaflet_legend(colors = colfunc(levels(geo$gruppe)),
                 lvl = levels(geo$gruppe),
                 group=as.character(2020-i)) %>%
  #leaflet_logo() %>%
  #leaflet_quelle(Text="Quelle: <br> Bundesagentur für Arbeit (2020)") %>%
  #leaflet_anmerkung(Text="Mehr Informationen erhalten Sie per Mausklick auf die Kreise!") %>%
  leaflet_polygons(
    colors=colfunc(levels$geo_gruppe),
    bg = colfunc(levels(geo$gruppe)),
    variablen=paste0(c("Gesamt", "Männer", "Frauen"), " (20", 20-i, ")"),
    headers=paste0(c("Gesamt", "Männer", "Frauen"), " (20", 20-i, ")"),
    k=0, # Nachkommastellen
    group=as.character(2020-i), suffix="Euro"
  ) 

}

# Ländergrenzen (nur bei Kreisen, Bezirken, Arbeitsmarktregionen sinnvoll)
if(ebene %in% c("KRS", "AMR")) {
  for(b in as.character(2020:2016)) {
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

fig <- fig %>%
  addLayersControl(
    baseGroups=c("2020", "2019", "2018", "2017", "2016"),
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
  file = paste0(getwd(), "/Output/Karten/", name_i1, ".html"),
  selfcontained=F
)

# mitliefern: WSI-Logo
file.copy(
  from="./CSS/WSI_Abbinder_RGB.jpg",
  to="./Output/Karten"
)



