# Hintergrunddaten laden -----

# Annahme über Dateipfad siehe funktionen/funktionen_karten.R
geo_base <- load_shapefile(path=NULL, level=ebene)

# Ländergrenzen (für später, um sie einzuzeichnen)
if(ebene %in% c("KRS", "AMR")) {
  geo_land <- load_shapefile(path=NULL, level="LAN")
}

geo_base <- ms_simplify(geo_base, keep_shapes=T)
geo_base <- spTransform(geo_base, CRS("+proj=longlat"))
#geo_land <- ms_simplify(geo_land, keep_shapes=T)
#geo_land <- spTransform(geo_land, CRS("+proj=longlat"))

geo <- merge(geo_base, dat, by=key) # dat siehe 1_read.R

# Farbenpalette: Woran geknüpft?
colfunc <- colorFactor(farben(col_i), domain=geo$Gruppe, ordered=T)
colfunc_m <- colorFactor(farben(col_i), domain=geo$`Gruppe Männer`, ordered=T)
colfunc_f <- colorFactor(farben(col_i), domain=geo$`Gruppe Frauen`, ordered=T)
geo$color_m <- colfunc_m(geo$`Gruppe Männer`)
geo$color_f <- colfunc_f(geo$`Gruppe Frauen`)
geo$color <- colfunc(geo$Gruppe)



# Karte zeichnen-------

# Karte
geo$gruppe <- geo$Gruppe
geo$color <- geo$color

fig <- leaflet_basis(geo) %>% 
  leaflet_text("Solo-Selbstständige in den Bundesländern, 2021", size=20, group="Gesamt") %>%
  leaflet_text(lat=55.25, 
               "<div style='padding-top: 2%; line-height:90%'>Anteil der Solo-Selbstständigen 
               an allen Erwerbstätigen, in Prozent</div>", size=15, group="Gesamt") %>%
  leaflet_legend(colors = colfunc(levels(geo$gruppe)),
                 lvl = levels(geo$gruppe),
                 group="Gesamt") %>%
  leaflet_logo() %>%
  leaflet_quelle(Text="Quelle: <br> Statistisches Bundesamt (2022)") %>%
  leaflet_anmerkung(Text="Mehr Informationen erhalten Sie per Mausklick auf die Länder!") %>%
  leaflet_polygons(
    colors=colfunc(levels(geo$gruppe)),
    bg = colfunc(levels(geo$gruppe)),
    variablen=c("Gesamt", "Männer", "Frauen"),
    headers=c("Gesamt", "Männer", "Frauen"),
    k=1, # Nachkommastellen
    group="Gesamt", suffix="%"
  ) 


geo$gruppe <- geo$`Gruppe Männer`
geo$color <- geo$color_m

fig <- fig %>% 
  leaflet_text("Solo-Selbstständige in den Bundesländern, 2021 (Männer)", size=20, group="Männer") %>%
  leaflet_text(lat=55.25, 
               "<div style='padding-top: 2%; line-height:90%'>Anteil der solo-selbstständigen Männer 
               an allen erwerbstätigen Männern, in Prozent</div>", size=15, group="Männer") %>%
  leaflet_legend(colors = colfunc_m(levels(geo$gruppe)),
                 lvl = levels(geo$gruppe),
                 group="Männer") %>%
  leaflet_polygons(
    colors=colfunc_m(levels(geo$gruppe)),
    bg = colfunc_m(levels(geo$gruppe)),
    variablen=c("Männer", "Frauen", "Gesamt"),
    headers=c("Männer", "Frauen", "Gesamt"),
    k=1, # Nachkommastellen
    group="Männer", suffix="%"
  ) 

geo$gruppe <- geo$`Gruppe Frauen`
geo$color <- geo$color_f

fig <- fig %>% 
  leaflet_text("Solo-Selbstständige in den Bundesländern, 2021 (Frauen)", size=20, group="Frauen") %>%
  leaflet_text(lat=55.25, 
               "<div style='padding-top: 2%; line-height:90%'>Anteil der solo-selbstständigen Frauen
               an allen erwerbstätigen Frauen, in Prozent</div>", size=15, group="Frauen") %>%
  leaflet_legend(colors = colfunc_f(levels(geo$gruppe)),
                 lvl = levels(geo$gruppe),
                 group="Frauen") %>%
  leaflet_polygons(
    colors=colfunc_f(levels(geo$gruppe)),
    bg = colfunc_f(levels(geo$gruppe)),
    variablen=c("Frauen", "Männer", "Gesamt"),
    headers=c("Frauen", "Männer", "Gesamt"),
    k=1, # Nachkommastellen
    group="Frauen", suffix="%"
  ) 


fig <- fig %>%
  addLayersControl(
    baseGroups=c("Gesamt", "Männer", "Frauen"),
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



