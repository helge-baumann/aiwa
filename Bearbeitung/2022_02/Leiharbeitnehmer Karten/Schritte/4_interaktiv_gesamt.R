# read data

# Am besten einfach "dat" nennen. 


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

dat <- dat15 %>% left_join(dat16, by="amr") %>% left_join(dat17, by="amr") %>%
  left_join(dat18, by="amr") %>% left_join(dat19, by="amr") %>% left_join(dat20, by="amr") %>%
  left_join(dat21, by="amr") 

geo <- merge(geo_base, dat, by=key) # dat siehe 1_read.R


# Farbenpalette: Woran geknüpft?
colfunc <- colorFactor(farben(col_i), domain=geo$`gruppe 2015`, ordered=T)
geo$color15 <- colfunc(geo$`gruppe 2015`)
geo$color16 <- colfunc(geo$`gruppe 2016`)
geo$color17 <- colfunc(geo$`gruppe 2017`)
geo$color18 <- colfunc(geo$`gruppe 2018`)
geo$color19 <- colfunc(geo$`gruppe 2019`)
geo$color20 <- colfunc(geo$`gruppe 2020`)
geo$color21 <- colfunc(geo$`gruppe 2021`)

# Karte zeichnen-------

# Karte
geo$color <- geo$color15
fig <- leaflet_basis(geo) %>% 
  leaflet_text(paste0("Leiharbeit in den Arbeitsmarktregionen ", "2015"), size=20, group="2015") %>%
  leaflet_text(lat=55.25, 
               "Anteil der Beschäftigten in der Leiharbeit* an allen Beschäftigten*, in Prozent", size=15) %>%
  leaflet_legend(colors = colfunc(levels(geo$`gruppe 2015`)),
                 lvl = levels(geo$`gruppe 2015`)) %>%
  leaflet_logo() %>%
  leaflet_quelle(Text="Quelle: <br> Sonderauswertung der <br> Bundesagentur für Arbeit, eigene Berechnungen") %>%
  leaflet_anmerkung(Text="<br> Mehr Informationen erhalten Sie per Mausklick auf die Länder! <br>*bezieht sich auf den Wohnort") %>%
  leaflet_polygons(
    bg = colfunc(levels(geo$`gruppe 2015`)),
    variablen=c("Gesamt (2015)", "Männer (2015)", "Frauen (2015)"),
    headers=c("Gesamt (2015)", "Männer (2015)", "Frauen (2015)"),
    k=1, # Nachkommastellen,
    group="2015", suffix="%"
  ) 

geo$color <- geo$color16
fig <- fig %>%
  leaflet_text(paste0("Leiharbeit in den Arbeitsmarktregionen ", "2016"), size=20, group="2016") %>%
  leaflet_polygons(
    color=geo$color,
    bg = colfunc(levels(geo$`gruppe 2016`)),
    variablen=c("Gesamt (2016)", "Männer (2016)", "Frauen (2016)"),
    headers=c("Gesamt (2016)", "Männer (2016)", "Frauen (2016)"),
    k=1, # Nachkommastellen,
    group="2016", suffix="%"
  )
geo$color <- geo$color17
fig <- fig %>%
  leaflet_text(paste0("Leiharbeit in den Arbeitsmarktregionen ", "2017"), size=20, group="2017") %>%
  leaflet_polygons(
    color=geo$color,
    bg = colfunc(levels(geo$`gruppe 2017`)),
    variablen=c("Gesamt (2017)", "Männer (2017)", "Frauen (2017)"),
    headers=c("Gesamt (2017)", "Männer (2017)", "Frauen (2017)"),
    k=1, # Nachkommastellen,
    group="2017", suffix="%"
  )


geo$color <- geo$color18
fig <- fig %>%
  leaflet_text(paste0("Leiharbeit in den Arbeitsmarktregionen ", "2018"), size=20, group="2018") %>%
  leaflet_polygons(
    color=geo$color,
    bg = colfunc(levels(geo$`gruppe 2018`)),
    variablen=c("Gesamt (2018)", "Männer (2018)", "Frauen (2018)"),
    headers=c("Gesamt (2018)", "Männer (2018)", "Frauen (2018)"),
    k=1, # Nachkommastellen,
    group="2018", suffix="%"
  )

geo$color <- geo$color19
fig <- fig %>%
  leaflet_text(paste0("Leiharbeit in den Arbeitsmarktregionen ", "2019"), size=20, group="2019") %>%
  leaflet_polygons(
    color=geo$color,
    bg = colfunc(levels(geo$`gruppe 2019`)),
    variablen=c("Gesamt (2019)", "Männer (2019)", "Frauen (2019)"),
    headers=c("Gesamt (2019)", "Männer (2019)", "Frauen (2019)"),
    k=1, # Nachkommastellen,
    group="2019", suffix="%"
  )

geo$color <- geo$color20
fig <- fig %>%
  leaflet_text(paste0("Leiharbeit in den Arbeitsmarktregionen ", "2020"), size=20, group="2020") %>%
  leaflet_polygons(
    color=geo$color,
    bg = colfunc(levels(geo$`gruppe 2020`)),
    variablen=c("Gesamt (2020)", "Männer (2020)", "Frauen (2020)"),
    headers=c("Gesamt (2020)", "Männer (2020)", "Frauen (2020)"),
    k=1, # Nachkommastellen,
    group="2020", suffix="%"
  )

geo$color <- geo$color21
fig <- fig %>%
  leaflet_text(paste0("Leiharbeit in den Arbeitsmarktregionen ", "2021"), size=20, group="2021") %>%
  leaflet_polygons(
    color=geo$color,
    bg = colfunc(levels(geo$`gruppe 2021`)),
    variablen=c("Gesamt (2021)", "Männer (2021)", "Frauen (2021)"),
    headers=c("Gesamt (2021)", "Männer (2021)", "Frauen (2021)"),
    k=1, # Nachkommastellen,
    group="2021", suffix="%"
  )



  
 
  
fig <- fig %>%
  addLayersControl(
    baseGroups=c("2021", "2020", "2019", "2018", "2017", "2016", "2015"),
    position = "bottomright",
    options = layersControlOptions(collapsed = FALSE)
  )
# Ländergrenzen (nur bei Kreisen, Bezirken, Arbeitsmarktregionen sinnvoll)
if(ebene %in% c("KRS", "AMR")) {
  fig <- fig %>%
    addPolygons(
      data = geo_land,
      color = "white", weight = 2, smoothFactor = 0.5, 
      opacity = 1.0, fillOpacity = 1.0,
      fill = F, 
      group=c("2015")
    )
  fig <- fig %>%
    addPolygons(
      data = geo_land,
      color = "white", weight = 2, smoothFactor = 0.5, 
      opacity = 1.0, fillOpacity = 1.0,
      fill = F, 
      group=c("2016")
    )
  fig <- fig %>%
    addPolygons(
      data = geo_land,
      color = "white", weight = 2, smoothFactor = 0.5, 
      opacity = 1.0, fillOpacity = 1.0,
      fill = F, 
      group=c("2017")
    )
  fig <- fig %>%
    addPolygons(
      data = geo_land,
      color = "white", weight = 2, smoothFactor = 0.5, 
      opacity = 1.0, fillOpacity = 1.0,
      fill = F, 
      group=c("2018")
    )
  fig <- fig %>%
    addPolygons(
      data = geo_land,
      color = "white", weight = 2, smoothFactor = 0.5, 
      opacity = 1.0, fillOpacity = 1.0,
      fill = F, 
      group=c("2019")
    )
  fig <- fig %>%
    addPolygons(
      data = geo_land,
      color = "white", weight = 2, smoothFactor = 0.5, 
      opacity = 1.0, fillOpacity = 1.0,
      fill = F, 
      group=c("2020")
    )
  fig <- fig %>%
    addPolygons(
      data = geo_land,
      color = "white", weight = 2, smoothFactor = 0.5, 
      opacity = 1.0, fillOpacity = 1.0,
      fill = F, 
      group=c("2021")
    )
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
  file = paste0(getwd(), "/Output/Karten/", name_i, "", "", ".html"),
  selfcontained=F
)

# mitliefern: WSI-Logo
file.copy(
  from="./CSS/WSI_Abbinder_RGB.jpg",
  to="./Output/Karten"
)



