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
colfunc <- colorFactor(farben(col_i), domain=geo$`Gruppe 2019`, ordered=T)
geo$color19 <- colfunc(geo$`Gruppe 2019`)



# Karte zeichnen-------

# Karte
geo$gruppe <- geo$`Gruppe 2019`
geo$color <- geo$color19

fig <- leaflet_basis(geo) %>% 
  leaflet_text("Verfügbare Pro-Kopf-Einkommen der privaten Haushalte 2019", size=20, group="2019") %>%
  leaflet_text(lat=55.25, 
               "Verfügbare Einkommen der privaten Haushalte* pro Einwohner, in Euro", size=15) %>%
  leaflet_legend(colors = colfunc(levels(geo$gruppe)),
                 lvl = levels(geo$gruppe),
                 group="2019") %>%
  leaflet_logo() %>%
  leaflet_quelle(Text="Quelle: <br> VGR der Länder") %>%
  leaflet_anmerkung(Text="Verfügbares Einkommen der privaten Haushalte einschließlich privater Organisationen ohne Erwerbszweck <br> (Ausgabenkonzept) pro Einwohner.") %>%
  leaflet_polygons(
    colors=colfunc(levels$geo_gruppe),
    bg = colfunc(levels(geo$gruppe)),
    variablen=c("Verfügbares Einkommen pro Kopf (2019)", "Primäreinkommen pro Kopf (2019)"),
    headers=c("Verfügbares Einkommen pro Kopf (2019)", "Primäreinkommen pro Kopf (2019)"),
    k=0, # Nachkommastellen
    group="2019", suffix=c("Euro", "Euro")
  ) 




# Ländergrenzen (nur bei Kreisen, Bezirken, Arbeitsmarktregionen sinnvoll)
if(ebene %in% c("KRS", "AMR")) {
  
    fig <- fig %>%
      addPolygons(
        data = geo_land,
        color = "white", weight = 2, smoothFactor = 0.5, 
        opacity = 1.0, fillOpacity = 1.0,
        fill = F,
        group="2019"
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
  file = paste0(getwd(), "/Output/Karten/", name_i, ".html"),
  selfcontained=F
)

# mitliefern: WSI-Logo
file.copy(
  from="./CSS/WSI_Abbinder_RGB.jpg",
  to="./Output/Karten"
)



