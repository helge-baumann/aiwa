
# Basis-Shapefile laden
# ( falls n?tig, Download unter http://www.geodatenzentrum.de/geodaten/gdz_rahmen.gdz_div?gdz_spr=deu&gdz_akt_zeile=5&gdz_anz_zeile=1&gdz_unt_zeile=17&gdz_user_id=0)
geo <- readOGR(
  "./Karte/Quellmaterial/Arbeitsmarktregionen/ge1000.utm32s.shape/ge1000.utm32s.shape/ge1000/amr1000", "AMR1000",
  encoding="UTF-8", use_iconv=T)
# Wichtige Variablen in diesem Datensatz:
# Allgemeiner Gemeindeschlüssel (AGS); identisch mit Kreiskennziffer
# Name des Kreises (GEN)

# Daten herunterladen
dat <- amr_quoten %>%
  select(amr, Arbeitsmarktregion, Quote_gesamt, Quote_Maenner, Quote_Frauen)

names(dat) <- c("amr", "Region", "zahl1", "zahl2", "zahl3")

geo <- merge(geo, dat, by.x="SN_AMR", by.y="amr")

#geo$GEN <- str_remove(geo$GEN, fixed(" (Bodensee)"))
#geo <- geo[geo$GF > 1,]

geo$zahl1.g <- cut2(geo$zahl1, 
                    cuts=c(0,1.5, 2, 2.5, 100))
geo$zahl1.g <- factor(geo$zahl1.g, 
                      labels=c( "bis 1,5", "1,5 bis 2", 
                                "2 bis 2,5", "2,5 und mehr"))

# Farbpalette entwerfen
# hier bitte das untere und obere Ende der gewünschten Farbpalette eintragen. 
colfunc <- colorRampPalette(farben(col_lei))
# colfunc(n) gibt die Feinheit der Abstufungen an.
#spplot(geo, "zahl", col.regions = colfunc(30))

# jetzt nicht graduell, sondern kategorial (gruppiert)
#spplot(geo, "zahl.g", col.regions = colfunc(length(levels(geo$zahl.g))))

# Jetzt mit ggplot2 / ggmap
geo2 <- fortify(geo, region="SN_AMR")
geo.merge <- geo[,c(1,7)]
geo2 <- merge(geo2, geo.merge, by.x="id", by.y="SN_AMR")

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
  ggtitle("Leiharbeitnehmer nach Arbeitsmarktregionen, Juni 2019") +
  geom_polygon(data=geo2 ,aes(fill=zahl1.g), color = "white")+
  geom_polygon(data=geo2[!geo2$id %in% geo2[geo2$hole,]$id,],aes(fill=zahl1.g), color = "white")+
  scale_fill_manual(name="In Prozent:", values=colfunc(5)) +
  theme(text=element_text(family="sans")) +
  ditch_the_axes 
#check1

pdf(file=paste0("./Output/Karten/", name_lei, ".pdf"), 
    pointsize=12, width=7, height=8)
print(check1)
dev.off()
#dev.off()
