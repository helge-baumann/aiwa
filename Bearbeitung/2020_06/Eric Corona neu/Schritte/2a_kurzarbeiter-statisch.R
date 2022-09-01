
# Basis-Shapefile laden
# ( falls n?tig, Download unter http://www.geodatenzentrum.de/geodaten/gdz_rahmen.gdz_div?gdz_spr=deu&gdz_akt_zeile=5&gdz_anz_zeile=1&gdz_unt_zeile=17&gdz_user_id=0)
geo <- readOGR(
  "./Karte/Quellmaterial/vg1000_0101.utm32s.shape.ebenen/vg1000_ebenen", 
  "VG1000_KRS", # Bundesländer: "VG1000_LAN"
  , 
  use_iconv = TRUE, 
  encoding = "UTF-8")
# Wichtige Variablen in diesem Datensatz:
# Allgemeiner Gemeindeschlüssel (AGS); identisch mit Kreiskennziffer
# Name des Kreises (GEN)

# Daten herunterladen
dat_ka <- dat_ges %>% 
  select(AGS, Kreis, Kurzarbeiterquote, Kurzarbeiter)

names(dat_ka) <- c("AGS", "Kreis", "zahl1", "zahl2")

#geo$GEN <- str_remove(geo$GEN, fixed(" (Bodensee)"))
geo <- merge(geo, dat_ka, by="AGS")
#geo <- geo[geo$GF > 1,]

geo$zahl1.g <- cut2(geo$zahl1, 
                            cuts=c(0,25,30,35, 40, 101))
geo$zahl1.g <- factor(geo$zahl1.g, 
                              labels=c( "bis 25", "25 bis 30", 
                                        "30 bis 35", "35 bis 40", "40 und mehr"))

# Farbpalette entwerfen
# hier bitte das untere und obere Ende der gewünschten Farbpalette eintragen. 
colfunc <- colorRampPalette(c("#d3e3ee", "#00384f"))
# colfunc(n) gibt die Feinheit der Abstufungen an.
#spplot(geo, "zahl", col.regions = colfunc(30))

# jetzt nicht graduell, sondern kategorial (gruppiert)
#spplot(geo, "zahl.g", col.regions = colfunc(length(levels(geo$zahl.g))))

# Jetzt mit ggplot2 / ggmap
geo2 <- fortify(geo, region="AGS")
geo.merge <- geo[,c(1,24,27)]
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
  labs(title="Quote angezeigter Kurzarbeit nach Kreisen",
       subtitle = paste0("Angezeigte Kurzarbeit (März + April 2020) in Prozent",
                         " der sozialversicherungspflichtig Beschäftigten*"),
       caption = paste0("*Anmerkung: Aus Gründen der Verfügbarkeit wurden",
                        " für Beschäftigte Angaben vom September 2019 verwendet.\n",
                        "Der Wert für Weiden i. d.Opf. (rot markiert) überschätzt mutmaßlich die tatsächliche Verbreitung.")) +
  geom_polygon(data=geo2 ,aes(fill=zahl1.g), color = "white")+
  geom_polygon(data=geo2[!geo2$id %in% geo2[geo2$hole,]$id,],aes(fill=zahl1.g), color = "white")+
  geom_polygon(data=geo2[geo2$Kreis == "Weiden i.d.OPf., Stadt",], fill="red", color="white")+
  scale_fill_manual(name="In Prozent:", values=colfunc(5)) +
  ditch_the_axes +
  theme(plot.caption=element_text(hjust = 0))
check1

pdf(file=paste0("./Output/Karten/Kurzarbeit_",format(Sys.time(), "%y%m%d"),".pdf"), 
    pointsize=12, width= 8, height = 11, family="Univers LT Std 55")
print(check1)
  dev.off()
