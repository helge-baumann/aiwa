
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
dat_co <- dat_ges %>% 
  select(AGS, Kreis, CoronaEffekt, CoronaQuote)

names(dat_co) <- c("AGS", "Kreis", "zahl1", "zahl2")

#geo$GEN <- str_remove(geo$GEN, fixed(" (Bodensee)"))
geo <- merge(geo, dat_co, by="AGS")
#geo <- geo[geo$GF > 1,]

geo$zahl2.g <- cut2(geo$zahl2, 
                            cuts=c(0,0.75, 1, 1.25, 1.5, 40000))
geo$zahl2.g <- factor(geo$zahl2.g, 
                              labels=c( "bis 0,75", "0,75 bis 1,00", 
                                        "1,00 bis 1,25", "1,25 bis 1,5", "1,5 und mehr"))

# Farbpalette entwerfen
# hier bitte das untere und obere Ende der gewünschten Farbpalette eintragen. 
colfunc <- colorRampPalette(c("#dfd5e8", "#431d57")) # für lila
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
for(i in 1:length(levels(geo2$zahl2.g))) {
  geo2$color[geo2$zahl2.g == levels(geo2$zahl2.g)[i]] <- colfunc(5)[i]
}

# Karte 1----
check1 <- ggplot(geo2, aes(x=long, y=lat, group=group)) +
  labs(title="Wirkung von Corona auf die Arbeitslosigkeit, Mai 2020",
       subtitle = paste0("Corona-bedingter Anstieg der Arbeitslosenquote in",
                         " Prozent der Bezugsgröße der Arbeitslosenquote"),
       caption = paste0("",
                        " ")) +
  geom_polygon(data=geo2 ,aes(fill=zahl2.g), color = "white")+
  geom_polygon(data=geo2[!geo2$id %in% geo2[geo2$hole,]$id,],aes(fill=zahl2.g), color = "white")+
  scale_fill_manual(name="in Prozent:", values=colfunc(5)) +
  ditch_the_axes 
#check1

pdf(file=paste0("./Output/Karten/Corona_",format(Sys.time(), "%y%m%d"),".pdf"), 
    pointsize=12, width= 8, height = 11, family="Univers LT Std 55")
print(check1)
dev.off()

