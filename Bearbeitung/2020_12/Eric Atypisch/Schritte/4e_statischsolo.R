

# Basis-Shapefile laden
# ( falls n?tig, Download unter http://www.geodatenzentrum.de/geodaten/gdz_rahmen.gdz_div?gdz_spr=deu&gdz_akt_zeile=5&gdz_anz_zeile=1&gdz_unt_zeile=17&gdz_user_id=0)
geo <- readOGR(
  "./Karte/Quellmaterial/vg1000_0101.utm32s.shape.ebenen/vg1000_ebenen", 
  "VG1000_LAN", # Bundesländer: "VG1000_LAN"
  , 
  use_iconv = TRUE, 
  encoding = "UTF-8")
# Wichtige Variablen in diesem Datensatz:
# Allgemeiner Gemeindeschlüssel (AGS); identisch mit Kreiskennziffer
# Name des Kreises (GEN)

# Daten herunterladen
dat <- dat_solo

names(dat) <- c("GEN", "zahl1", "zahl2", "zahl3")

geo$GEN <- str_remove(geo$GEN, fixed(" (Bodensee)"))
geo <- merge(geo, dat, by="GEN")
geo <- geo[geo$GF > 1,]

geo$zahl3.g <- cut2(geo$zahl3, 
                    cuts=c(-1,4.4, 4.8, 5.2, 5.6, 100))
geo$zahl3.g <- factor(geo$zahl3.g, 
                      labels=c( "bis 4,4", "4,4 bis 4,8", 
                                "4,8 bis 5,2", "5,2 bis 5,6", "5,6 und mehr"))

# Farbpalette entwerfen
# hier bitte das untere und obere Ende der gewünschten Farbpalette eintragen. 
colfunc <- colorRampPalette(farben(col_sol))
# colfunc(n) gibt die Feinheit der Abstufungen an.
#spplot(geo, "zahl", col.regions = colfunc(30))

# jetzt nicht graduell, sondern kategorial (gruppiert)
#spplot(geo, "zahl.g", col.regions = colfunc(length(levels(geo$zahl.g))))

# Jetzt mit ggplot2 / ggmap
geo2 <- fortify(geo, region="GEN")
geo.merge <- geo[,c(1,27)]
geo2 <- merge(geo2, geo.merge, by.x="id", by.y="GEN")

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
for(i in 1:length(levels(geo2$zahl3.g))) {
  geo2$color[geo2$zahl3.g == levels(geo2$zahl3.g)[i]] <- colfunc(5)[i]
}

# Karte 1----
check1 <- ggplot(geo2, aes(x=long, y=lat, group=group)) +
  ggtitle("Solo-Selbständige nach Bundesländern, 2019") +
  geom_polygon(data=geo2 ,aes(fill=zahl3.g), color = "white")+
  geom_polygon(data=geo2[!geo2$id %in% geo2[geo2$hole,]$id,],aes(fill=zahl3.g), color = "white")+
  scale_fill_manual(name="In Prozent:", values=colfunc(5)) +
  theme(text=element_text(family="Univers LT Std 55")) +
  ditch_the_axes 
#check1

pdf(file=paste0("./Output/Karten/", name_sol, ".pdf"), 
    pointsize=12, width=7, height=8)
print(check1)
dev.off()
#dev.off()
