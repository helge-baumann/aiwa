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
  "VG1000_LAN", # Bundesländer: "VG1000_LAN"
  , 
  use_iconv = TRUE, 
  encoding = "UTF-8")

# Daten transformieren
longlatcoor<-spTransform(geo,CRS("+proj=longlat"))

# Daten herunterladen
dat <- dat_solo

names(dat) <- c("GEN", "zahl1", "zahl2", "zahl3")

longlatcoor$GEN <- str_remove(longlatcoor$GEN, fixed(" (Bodensee)"))
longlatcoor <- merge(longlatcoor, dat, by="GEN")
longlatcoor <- longlatcoor[longlatcoor$GF > 1,]

longlatcoor$zahl3.g <- cut2(longlatcoor$zahl3, 
                            cuts=c(-1,4.4, 4.8, 5.2, 5.6, 100))
longlatcoor$zahl3.g <- factor(longlatcoor$zahl3.g, 
                              labels=c( "bis 4,4", "4,4 bis 4,8", 
                                        "4,8 bis 5,2", "5,2 bis 5,6", "5,6 und mehr"))

# Achtung: Name des Landes muss identisch mit der entsprechenden Bezeichnung 
# im Objekt "longlatcoor" sein; z.B. "GEN" für Länder und "AGS" für Kreise!

#longlatcoor$GEN <- str_remove(longlatcoor$GEN, fixed(" (Bodensee)"))
#longlatcoor <- merge(longlatcoor, dat, by="AGS")
#longlatcoor <- longlatcoor[longlatcoor$GF > 1,]

# Farbpalette
colfunc <- colorRampPalette(farben(col_sol)) # für pink

# Farben kategorisch festlegen
longlatcoor$color <- NA
for(i in 1:length(levels(longlatcoor$zahl3.g))) {
  longlatcoor$color[longlatcoor$zahl3.g == levels(longlatcoor$zahl3.g)[i]] <- colfunc(5)[i]
}

longlatcoor$hinweis <- NA # Platzhalter für weitere Hinweise aus Quelldaten

labs <- lapply(seq(nrow(longlatcoor)), function(i) {
  paste0( "<p style='font-weight:bold;'>", "", longlatcoor[["GEN"]][i], "", '<p></p>', 
          "Gesamt: ",
          str_replace(FormatDecimal(longlatcoor[["zahl3"]][i], k=1), fixed("."), ","),  " %",
          "; Männer: ",
          str_replace(FormatDecimal(longlatcoor[["zahl1"]][i], k=1), fixed("."), ","),  " %",
          "; Frauen: ",
          str_replace(FormatDecimal(longlatcoor[["zahl2"]][i], k=1), fixed("."), ","),  " %", 
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
  ~3.8, ~55.5, 
  label =  htmltools::HTML(
    paste(
      "<font face='Univers LT Std'>",
      "Solo-Selbständige nach Bundesländern, 2019", 
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
    ~3.8, ~54.6, 
    label =  htmltools::HTML(
      paste(
        "<font face='Univers LT Std'>",
        "Prozent der Erwerbstätigen ", 
        "<br>",
        "insgesamt ",
        "</font>", 
        sep=""
      )
    ),
    labelOptions = labelOptions(
      noHide = T, direction = 'right', textOnly = T,
      textsize="14px"
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
        levels(longlatcoor$zahl3.g)[1], 
        "</br>",
        levels(longlatcoor$zahl3.g)[2], 
        "</br>",
        levels(longlatcoor$zahl3.g)[3], 
        "</br>",
        levels(longlatcoor$zahl3.g)[4], 
        "</br>",
        levels(longlatcoor$zahl3.g)[5], 
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
    iconSize = c(80, 60)
  )
) %>%
  
  # Quellenangabe:----
addLabelOnlyMarkers(
  ~3.8, ~47.5, 
  label =  htmltools::HTML(
    paste("<font face='Univers LT Std'>",
          "Quelle:", "<br>", 
          "Sonderauswertung des  ", "</br>",
          "Statistischen Bundesamtes	", 
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
    colfunc(5)[5],
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
    " <span style='vertical-align:0px''>Gesamt:</span>",
    "<div style=' background-color:white;color:black;padding-left:0px; padding-top: -20px; padding-right: 5px; width:195px;height:16px; '>",
    str_replace(FormatDecimal(longlatcoor$zahl3, k=1), fixed("."), ","),  " %", "<br>",
    paste( 
      longlatcoor$anmerkung, sep=""), 
    "<div style=' background-color:",
    colfunc(5)[3],
    ";color:white;padding-left:0px; padding-top: 8px; padding-right: 5px; width:195px;height:16px; '>",
    "Männer:",
    "</div>",
    "<div style=' background-color:white;color:black;padding-left:0px; padding-top: 0px; padding-right: 5px; width:195px;height:16px; '>",
    str_replace(FormatDecimal(longlatcoor$zahl1, k=1), fixed("."), ","),  " %", "<br>",
    paste( 
      longlatcoor$anmerkung, sep=""
    ), 
    "<div style=' background-color:",
    colfunc(5)[3],
    ";color:white;padding-left:0px; padding-top: 8px; padding-right: 5px; width:195px;height:16px; '>",
    "Frauen:",
    "</div>",
    "<div style=' background-color:white;color:black;padding-left:0px; padding-top: 0px; padding-right: 5px; width:195px;height:16px; '>",
    str_replace(FormatDecimal(longlatcoor$zahl2, k=1), fixed("."), ","),  " %", "<br>",
    paste( 
      longlatcoor$anmerkung, sep=""
    ), 
    "</div>",
    "</div>",
    "</font>", 
    "</div>",
    sep=""), 
  label = lapply(labs, HTML), 
  labelOptions = labelOptions(transparent=F, direction="auto", textsize="11px"),
  popupOptions = popupOptions(closeButton=T, closeOnClick=T, keepInView=F, 
                              autoPan=F, zIndex=500, bringToFront=T)
) 

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
    name_sol,
    ".html"),
  selfcontained=F
)

# mitliefern: WSI-Logo
file.copy(
  from="./Karte/Quellmaterial/WSI_Abbinder_RGB.jpg",
  to="./Output/Karten"
)



