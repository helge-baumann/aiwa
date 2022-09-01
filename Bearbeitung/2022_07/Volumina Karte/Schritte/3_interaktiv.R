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
  leaflet_text("Geleistete Arbeitsstunden, 2020", size=20, group="2020") %>%
  leaflet_text(lat=55.25, 
               "Standard-Arbeitsvolumen pro Arbeitnehmer/in nach Kreisen und Wirtschaftsbereichen", size=15) %>%
  leaflet_legend(colors = colfunc(levels(geo$gruppe)),
                 lvl = levels(geo$gruppe),
                 group="2020") %>%
  leaflet_logo() %>%
  leaflet_quelle(Text="Quelle: <br> AK Erwerbstätigenrechnung, <br> eigene Berechnungen") %>%
  leaflet_anmerkung(Text="Mehr Informationen erhalten Sie per Mausklick auf die Kreise!") %>%
  leaflet_polygons(
    colors=colfunc(levels$geo_gruppe),
    bg = colfunc(levels(geo$gruppe)),
    variablen=c("Gesamt (2020)", "Verarbeitendes Gewerbe (2020)", "Baugewerbe (2020)", "Öffentliche Dienstleister (2020)"),
    headers=c("Gesamt (2020)", "Verarbeitendes Gewerbe (2020)", "Baugewerbe (2020)", "Öffentliche Dienstleister (2020)"),
    k=1, # Nachkommastellen
    group="2020", suffix="Std."
  ) 



geo$gruppe <- geo$`Gruppe 2019`
geo$color <- geo$color19

fig <- fig %>% 
  leaflet_text("Geleistete Arbeitsstunden, 2019", size=20, group="2019") %>%
  leaflet_text(lat=55.25, 
               "Standard-Arbeitsvolumen pro Arbeitnehmer/in nach Kreisen und Wirtschaftsbereichen", size=15) %>%
  leaflet_legend(colors = colfunc(levels(geo$gruppe)),
                 lvl = levels(geo$gruppe),
                 group="2019") %>%
  leaflet_logo() %>%
  leaflet_quelle(Text="Quelle: <br> AK Erwerbstätigenrechnung, <br> eigene Berechnungen") %>%
  leaflet_anmerkung(Text="Mehr Informationen erhalten Sie per Mausklick auf die Kreise!") %>%
  leaflet_polygons(
    colors=colfunc(levels$geo_gruppe),
    bg = colfunc(levels(geo$gruppe)),
    variablen=c("Gesamt (2019)", "Verarbeitendes Gewerbe (2019)", "Baugewerbe (2019)", "Öffentliche Dienstleister (2019)"),
    headers=c("Gesamt (2019)", "Verarbeitendes Gewerbe (2019)", "Baugewerbe (2019)", "Öffentliche Dienstleister (2019)"),
    k=1, # Nachkommastellen
    group="2019", suffix="Std."
  ) 


geo$gruppe <- geo$`Gruppe 2018`
geo$color <- geo$color18

fig <- fig %>% 
  leaflet_text("Geleistete Arbeitsstunden, 2018", size=20, group="2018") %>%
  leaflet_text(lat=55.25, 
               "Standard-Arbeitsvolumen pro Arbeitnehmer/in nach Kreisen und Wirtschaftsbereichen", size=15) %>%
  leaflet_legend(colors = colfunc(levels(geo$gruppe)),
                 lvl = levels(geo$gruppe),
                 group="2018") %>%
  leaflet_logo() %>%
  leaflet_quelle(Text="Quelle: <br> AK Erwerbstätigenrechnung, <br> eigene Berechnungen") %>%
  leaflet_anmerkung(Text="Mehr Informationen erhalten Sie per Mausklick auf die Kreise!") %>%
  leaflet_polygons(
    colors=colfunc(levels$geo_gruppe),
    bg = colfunc(levels(geo$gruppe)),
    variablen=c("Gesamt (2018)", "Verarbeitendes Gewerbe (2018)", "Baugewerbe (2018)", "Öffentliche Dienstleister (2018)"),
    headers=c("Gesamt (2018)", "Verarbeitendes Gewerbe (2018)", "Baugewerbe (2018)", "Öffentliche Dienstleister (2018)"),
    k=1, # Nachkommastellen
    group="2018", suffix="Std."
  ) 



geo$gruppe <- geo$`Gruppe 2017`
geo$color <- geo$color17

fig <- fig %>%
  leaflet_text("Geleistete Arbeitsstunden, 2017", size=20, group="2017") %>%
  leaflet_polygons(
    labvar = 1, 
    colors=colfunc(levels$geo_gruppe),
    bg = colfunc(levels(geo$gruppe)),
    variablen=c("Gesamt (2017)", "Verarbeitendes Gewerbe (2017)", "Baugewerbe (2017)", "Öffentliche Dienstleister (2017)"),
    headers=c("Gesamt (2017)", "Verarbeitendes Gewerbe (2017)", "Baugewerbe (2017)", "Öffentliche Dienstleister (2017)"),
    k=1, # Nachkommastellen
    group="2017", suffix="Std."
  ) %>%
  leaflet_legend(colors = colfunc(levels(geo$gruppe)), group="2017")

geo$gruppe <- geo$`Gruppe 2016`
geo$color <- geo$color16

fig <- fig %>%
  leaflet_text("Geleistete Arbeitsstunden, 2016", size=20, group="2016") %>%
  leaflet_polygons(
    labvar = 1, 
    colors=colfunc(levels$geo_gruppe),
    bg = colfunc(levels(geo$gruppe)),
    variablen=c("Gesamt (2016)", "Verarbeitendes Gewerbe (2016)", "Baugewerbe (2016)", "Öffentliche Dienstleister (2016)"),
    headers=c("Gesamt (2016)", "Verarbeitendes Gewerbe (2016)", "Baugewerbe (2016)", "Öffentliche Dienstleister (2016)"),
    k=0, # Nachkommastellen
    group="2016", suffix="Std."
  ) %>%
  leaflet_legend(colors = colfunc(levels(geo$gruppe)), group="2016")

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
  file = paste0(getwd(), "/Output/Karten/", name_i, ".html"),
  selfcontained=F
)

# mitliefern: WSI-Logo
file.copy(
  from="./CSS/WSI_Abbinder_RGB.jpg",
  to="./Output/Karten"
)



