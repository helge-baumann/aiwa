
# Basis-Shapefile laden
# ( falls n?tig, Download unter http://www.geodatenzentrum.de/geodaten/gdz_rahmen.gdz_div?gdz_spr=deu&gdz_akt_zeile=5&gdz_anz_zeile=1&gdz_unt_zeile=17&gdz_user_id=0)
geo <- readOGR(
  "./Karte/Quellmaterial/vg1000_0101.utm32s.shape.ebenen/vg1000_ebenen", 
  "VG1000_KRS", # Bundesländer: "VG1000_LAN"
  use_iconv = TRUE, 
  encoding = "UTF-8")
# Wichtige Variablen in diesem Datensatz:
# Allgemeiner Gemeindeschlüssel (AGS); identisch mit Kreiskennziffer
# Name des Kreises (GEN)

#geo$GEN <- str_remove(geo$GEN, fixed(" (Bodensee)"))
geo <- merge(geo, dat_med, by="AGS")
#geo <- geo[geo$GF > 1,]

geo$zahl1.g <- cut2(geo$zahl1, 
                            cuts=c(0,2750,3000,3250,3500, 6000))
geo$zahl1.g <- factor(geo$zahl1.g, 
                              labels=c( "bis 2749", "2750 bis 2999", 
                                        "3000 bis 3249", "3250 bis 3499", "3500 und mehr"))


# Farbpalette entwerfen
# hier bitte das untere und obere Ende der gewünschten Farbpalette eintragen. 
colfunc <- colorRampPalette(c("#d3e3ee", "#00384f"))
# colfunc(n) gibt die Feinheit der Abstufungen an.
#spplot(geo, "zahl", col.regions = colfunc(30))

# jetzt nicht graduell, sondern kategorial (gruppiert)
#spplot(geo, "zahl.g", col.regions = colfunc(length(levels(geo$zahl.g))))

# Jetzt mit ggplot2 / ggmap
geo2 <- fortify(geo, region="AGS")
geo.merge <- geo[,c(1,25,28)]
geo2 <- merge(geo2, geo.merge, by.x="id", by.y="AGS")

ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white")
)

geo2$color <- NA
for(i in 1:length(levels(geo2$zahl1.g))) {
  geo2$color[geo2$zahl1.g == levels(geo2$zahl1.g)[i]] <- colfunc(5)[i]
}

# Karte 1----
check1 <- ggplot(geo2, aes(x=long, y=lat, group=group)) +
  labs(title="Mittlere monatliche Bruttoarbeitsentgelte, Ende 2019",
       subtitle = paste0("Angaben für die sozialver", 
                         "sicherungspflichtig Vollzeit", 
                         "beschäftigten der Kerngruppe, ", 
                         "in Euro")#,
       #caption = paste0("*Anmerkung: Aus Gründen der Verfügbarkeit wurden",
           #             " für Beschäftigte Angaben vom September 2019 verwendet.\n",
             #           "Der Wert für Weiden i. d.Opf. (rot markiert) überschätzt mutmaßlich die tatsächliche Verbreitung.")
       ) +
  geom_polygon(data=geo2 ,aes(fill=zahl1.g), color = "white")+
  geom_polygon(data=geo2[!geo2$id %in% geo2[geo2$hole,]$id,],aes(fill=zahl1.g), color = "white")+
  #geom_polygon(data=geo2[geo2$Kreis == "Weiden i.d.OPf., Stadt",], fill="red", color="white")+
  scale_fill_manual(name="In Euro:", values=colfunc(5)) +
  ditch_the_axes +
  theme(plot.caption=element_text(hjust = 0))
check1

pdf(file=paste0("./Output/Karten/wsi_median_",format(Sys.time(), "%y%m%d"),".pdf"), 
    pointsize=12, width= 8, height = 11, family="Univers LT Std 55")
print(check1)
dev.off()
