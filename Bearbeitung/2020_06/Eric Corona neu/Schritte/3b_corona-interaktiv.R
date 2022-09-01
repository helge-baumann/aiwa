FormatDecimal <- function(x, k) { 
  return(format(round(as.numeric(x), k), nsmall=k)) 
} 
# Hintergrunddaten laden -----

# Download unter 
## http://www.geodatenzentrum.de/geodaten/gdz_rahmen.gdz_div?gdz_spr=deu&gdz_akt_zeile=5
## &gdz_anz_zeile=1&gdz_unt_zeile=17&gdz_user_id=0)

# Gebiete bis einschl. 2016:
geo <- readOGR(
  "./Karte/Quellmaterial/vg1000_0101.utm32s.shape.ebenen/vg1000_ebenen", 
  "VG1000_KRS", # Bundesländer: "VG1000_LAN"
  , 
  use_iconv = TRUE, 
  encoding = "UTF-8")

# Daten transformieren
longlatcoor<-spTransform(geo,CRS("+proj=longlat"))

# Ländergrenzen
geo_land <- readOGR(
  "./Karte/Quellmaterial/vg1000_0101.utm32s.shape.ebenen/vg1000_ebenen", 
  "VG1000_LAN", # Bundesländer: "VG1000_LAN"
  , 
  use_iconv = TRUE, 
  encoding = "UTF-8")
geo_land <- geo_land[geo_land$GF > 1,] 

# Daten transformieren
longlatcoor_land<-spTransform(geo_land,CRS("+proj=longlat"))

# Daten herunterladen

#geo$GEN <- str_remove(geo$GEN, fixed(" (Bodensee)"))
longlatcoor <- merge(longlatcoor, dat_co, by="AGS")
#geo <- geo[geo$GF > 1,]

longlatcoor$zahl2.g <- cut2(longlatcoor$zahl2, 
                            cuts=c(0,0.75, 1, 1.25, 1.5, 40000))
longlatcoor$zahl2.g <- factor(longlatcoor$zahl2.g, 
                              labels=c( "bis 0,75", "0,75 bis 1,00", 
                                        "1,00 bis 1,25", "1,25 bis 1,5", "1,5 und mehr"))

# Achtung: Name des Landes muss identisch mit der entsprechenden Bezeichnung 
# im Objekt "longlatcoor" sein; z.B. "GEN" für Länder und "AGS" für Kreise!

#longlatcoor$GEN <- str_remove(longlatcoor$GEN, fixed(" (Bodensee)"))
#longlatcoor <- merge(longlatcoor, dat, by="AGS")
#longlatcoor <- longlatcoor[longlatcoor$GF > 1,]

# Farbpalette
colfunc <- colorRampPalette(c("#dfd5e8", "#431d57")) # für lila

# Farben kategorisch festlegen
longlatcoor$color <- NA
for(i in 1:length(levels(longlatcoor$zahl2.g))) {
  longlatcoor$color[longlatcoor$zahl2.g == levels(longlatcoor$zahl2.g)[i]] <- colfunc(5)[i]
}

longlatcoor$hinweis <- NA # Platzhalter für weitere Hinweise aus Quelldaten

labs <- lapply(seq(nrow(longlatcoor)), function(i) {
  paste0( "<p style='font-weight:bold;'>", "", longlatcoor[["GEN"]][i], "", '<p></p>', 
          "Anstieg der Arbeitslosenquote: ",
          str_replace(FormatDecimal(longlatcoor[["zahl2"]][i], k=1), fixed("."), ","),  " %",
          "<br>",
          "Anstieg der Arbeitslosigkeit: ",
          str_replace(FormatDecimal(longlatcoor[["zahl1"]][i], k=0), fixed("."), ","),  " ",
          
          "", '</p>' ) 
})

# Karte zeichnen-------

zf <- 6 # Zoomfaktor der Karte; Original 5.2 ; optimal 6.2

# Karte
m <- leaflet( 
  longlatcoor, 
  width=596, height=596*1.25, 
  options = leafletOptions(
    zoomControl = F,
    minZoom=zf, maxZoom=zf)
) %>%
  
  setView(lng=10, lat=51, zoom=zf) %>% 
  
  suspendScroll(hoverToWake=T) %>%
  
  addMapPane("polygons", zIndex = 490) %>%
  addMapPane("text", zIndex = 1) %>%
  
  # Titel:----
addLabelOnlyMarkers(
  ~3.8, ~55.25, 
  label =  htmltools::HTML(
    paste(
      "<font face='Univers LT Std'>",
      "Wirkung von Corona auf die Arbeitslosigkeit, Mai 2020", 
      "</font>", 
      sep=""
    )
  ),
  labelOptions = labelOptions(
    noHide = T, 
    direction = 'right', 
    textOnly = T,
    textsize="22px"
    ,style=list("font-family"="Univers LT Std")
  )
) %>%
  
  # Untertitel:
  addLabelOnlyMarkers(
    ~3.8, ~54.4, 
    label =  htmltools::HTML(
      paste(
        "<font face='Univers LT Std'>",
        "Corona-bedingter Anstieg   ", 
        "<br>",
        "der Arbeitslosenquote in  ",
        "<br>",
        "Prozent der Bezugsgröße der ",
        "<br>",
        "Arbeitslosenquote",
        "</font>", 
        sep=""
      )
    ),
    labelOptions = labelOptions(
      noHide = T, direction = 'right', textOnly = T,
      textsize="15px"
      ,style=list(
        "font-family"="Univers LT Std",
        "color"=gray(0.3)
      ) 
    )
  ) %>%
  
  # Legende manuell----

# Farben
addLabelOnlyMarkers(
  ~3.8, ~52.65, 
  label =  htmltools::HTML(
    paste0(
      "<svg width='14.8' height='14.8'>",
      "<rect width='15' height='15' style='fill:",
      colfunc(5)[1],
      ";stroke-width:3;",
      colfunc(5)[1],
      " ' />",
      "</svg>", 
      "</br>",
      "<svg width='14.8' height='14.8'>",
      "<rect width='15' height='15' style='fill:",
      colfunc(5)[2],
      ";stroke-width:3;",
      colfunc(5)[2],
      " ' />",
      "</svg>", 
      "</br>",
      "<svg width='14.8' height='14.8'>",
      "<rect width='15' height='15' style='fill:",
      colfunc(5)[3],
      ";stroke-width:3;",
      colfunc(5)[3],
      " ' />",
      "</svg>", 
      "</br>",
      "<svg width='14.8' height='14.8'>",
      "<rect width='15' height='15' style='fill:",
      colfunc(5)[4],
      ";stroke-width:3;",
      colfunc(5)[4],
      " ' />",
      "</svg>", 
      "</br>",
      "<svg width='14.8' height='14.8'>",
      "<rect width='15' height='15' style='fill:",
      colfunc(5)[5],
      ";stroke-width:3;",
      colfunc(5)[5],
      " ' />",
      "</svg>", 
      "</br>"
    )
  ),
  labelOptions = labelOptions(
    noHide = T, direction = 'auto', textOnly = T,
    textsize="13px"
    ,style=list(
      "font-family"="Univers LT Std",
      "color"=gray(0.3)
    ) 
  )
) %>%
  
  # Legendentitel
  addLabelOnlyMarkers(
    ~3.8, ~53.5, 
    label =  htmltools::HTML(
      paste(
        "<font face='Univers LT Std'>",
        "",
        "</font>", 
        sep=""
      )
    ),
    labelOptions = labelOptions(
      noHide = T, direction = 'auto', textOnly = T,
      textsize="14px"
      ,style=list(
        "font-family"="Univers LT Std",
        "color"=gray(0.3)
      ) 
    )
  ) %>%
  
  # Legendenbeschriftung
  addLabelOnlyMarkers(
    ~4.25, ~52.65, 
    label =  htmltools::HTML(
      paste(
        "<font face='Univers LT Std'>",
        levels(longlatcoor$zahl2.g)[1], 
        "</br>",
        levels(longlatcoor$zahl2.g)[2], 
        "</br>",
        levels(longlatcoor$zahl2.g)[3], 
        "</br>",
        levels(longlatcoor$zahl2.g)[4], 
        "</br>",
        levels(longlatcoor$zahl2.g)[5], 
        "</br>",
        "</font>", 
        sep=""
      )
    ),
    labelOptions = labelOptions(
      noHide = T, direction = 'auto', textOnly = T,
      textsize="13px"
      ,style=list(
        "font-family"="Univers LT Std",
        "color"=gray(0.3)
      ) 
    )
  ) %>%
  
  # WSI-Logo:----
addMarkers(
  ~4.7, ~48.75, 
  icon = list(
    iconUrl = "./Karte/Quellmaterial/WSI_Abbinder_RGB.jpg", 
    iconSize = c(73,50)
  )
) %>%
  
  # Quellenangabe:----
addLabelOnlyMarkers(
  ~3.8, ~47.5, 
  label =  htmltools::HTML(
    paste("<font face='Univers LT Std'>",
          "Quelle:", "<br>", 
          "Eigene Berechnungen  ", "</br>",
          "Daten der Bundesagentur für Arbeit	", 
          sep="")
  ),
  labelOptions = labelOptions(
    noHide = T, direction = 'right', textOnly = T,
    textsize="12px"
    ,style=list(
      "font-family"="Univers LT Std",
      "color"=gray(0.2),
      "line-height"="12px")
  )
) %>%
  
  # Anmerkung :----
addLabelOnlyMarkers(
  ~3.8, ~46.75, 
  label =  htmltools::HTML(
    paste("<font face='Univers LT Std'>",
          "<i> ", "<br>", 
          "</i> ", 
          "</br>",
          "	", 
          sep="")
  ),
  labelOptions = labelOptions(
    noHide = T, direction = 'right', textOnly = T,
    textsize="12px"
    ,style=list(
      "font-family"="Univers LT Std",
      "color"=gray(0.2),
      "line-height"="12px")
  )
) %>%
  
  
  # Polygone einzeichnen:----
addPolygons(
  color=gray(0.5), weight = 0.4, smoothFactor = 0.5, 
  opacity = 1.0, fillOpacity = 0.75,
  fillColor= ~color,
  highlightOptions = highlightOptions(
    color = gray(0.45), weight = 1.5, bringToFront = T, sendToBack = T,
  ), 
  
  # Popups als Unteroption der Polygone:
  popup=paste(
    "<div style='background-color:", 
    colfunc(5)[3],
    " ;'>",
    "<font face='Univers LT Std'>", 
    "<div style='background-color:",
    colfunc(5)[1],
    ";color:white;padding-left:10px;font-weight:bold;'>",
    "",
    paste(
      #longlatcoor$BEZ,
      longlatcoor$GEN,  
      sep=" "
    ), 
    ":", 
    "",
    "</div>", 
    "<div style='background-color:",
    colfunc(5)[3],
    ";color:white;padding-left:5px;height: 5px;'>", "",
    "</div>",
    "<div style='display: inline-block; background-color:",
    colfunc(5)[3],
    ";color:white;padding-left:10px; padding-top: 8px; padding-right: 5px; width:65px;height:60px; margin:5; vertical-align:top;'>",
    "<img src='WSI_Abbinder_RGB.jpg' alt='WSI' width='60'>", "<br>", "",
    "</div>",
    "<div class='verticalLine' style='display: inline-block;  background-color:",
    colfunc(5)[3],
    ";color:white;padding-left:10px; padding-top: 0px; padding-right: 5px ;padding-right: 10px; width:150px;height: 130px; margin:5px;'>",
    " <span style='vertical-align:0px''>Anstieg der Arbeitslosenquote:</span>",
    "<div style=' background-color:white;color:black;padding-left:0px; padding-top: -20px; padding-right: 5px; width:195px;height:16px; '>",
    str_replace(FormatDecimal(longlatcoor$zahl2, k=1), fixed("."), ","),  " %", "<br>",
    paste( 
      longlatcoor$anmerkung, sep=""), 
    "<div style=' background-color:",
    colfunc(5)[3],
    ";color:white;padding-left:0px; padding-top: 8px; padding-right: 5px; width:195px;height:16px; '>",
    "Anstieg der Arbeitslosigkeit:",
    "</div>",
    "<div style=' background-color:white;color:black;padding-left:0px; padding-top: 0px; padding-right: 5px; width:195px;height:16px; '>",
    str_replace(FormatDecimal(longlatcoor$zahl1, k=0), fixed("."), ","),  " ", "<br>",
    paste( 
      longlatcoor$anmerkung, sep=""
    ), 
    "</div>",
    "</font>", 
    "</div>",
    sep=""), 
  label = lapply(labs, HTML), 
  labelOptions = labelOptions(transparent=F, direction="auto", textsize="11px"),
  popupOptions = popupOptions(closeButton=T, closeOnClick=T, keepInView=F, 
                              autoPan=F, zIndex=500, bringToFront=T)
) %>%
  
  
  # Ländergrenzen----
addPolygons(data=longlatcoor_land,
            color="white", weight = 2, smoothFactor = 0.5, 
            opacity = 1.0, fillOpacity = 1.0,
            fill=F)


# Leaflet-Stil anwenden und in HTML überführen----
m$dependencies <- list(
  htmlDependency(
    name = "leaflet_hbs"
    ,version = "1.0.0"
    # if local file use file instead of href below
    #  with an absolute path
    ,src = paste0(getwd(),"/Karte/Quellmaterial/Leaflet-Stil/")
    ,stylesheet = "rstudio_leaflet_helge.css"
  )
)

# abspeichern
saveWidget(
  frameableWidget(m), 
  file = paste0(
    getwd(),
    "/Output/Karten/",
    #Sys.Date(),
    "wsi_covid_ue.html"),
  selfcontained=F
)

# mitliefern: WSI-Logo
file.copy(
  from="./Karte/Quellmaterial/WSI_Abbinder_RGB.jpg",
  to="./Output/Karten"
)



