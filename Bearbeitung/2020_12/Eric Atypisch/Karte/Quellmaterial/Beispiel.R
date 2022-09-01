################################################################################
############ Beispiel statistische Karten in R erzeugen ########################
####################### 18.05.2017 #############################################
################################################################################

# Workspace bereinigen
rm(list=ls())

# Notwendige Pakete Laden
        # Achtung: rgeos muss vor maptools geladen werden.
        # Achtung: Bisher nicht installierte Pakete mit "install.packages("[PAKET]") installieren
x <- c("sp", "Hmisc", "rgdal", "ggplot2", "ggmap", "rgeos", "maptools")
lapply(x, require, character.only = TRUE)

# Arbeitsverzeichnis einrichten
setwd("K:/Karten_R")

# Basis-Shapefile laden
        # ( falls nötig, Download unter http://www.geodatenzentrum.de/geodaten/gdz_rahmen.gdz_div?gdz_spr=deu&gdz_akt_zeile=5&gdz_anz_zeile=1&gdz_unt_zeile=17&gdz_user_id=0)
geo <- readOGR("./vg1000_0101.utm32s.shape.ebenen/vg1000_ebenen", "VG1000_KRS")
        # Wichtige Variablen in diesem Datensatz:
                # Allgemeiner Gemeindeschlüssel (AGS); identisch mit Kreiskennziffer
                # Name des Kreises (GEN)

# für jeden Kreis eine Zufallszahl erzeugen
h <- data.frame(unique(geo$AGS), NA); h[,2] <- runif(dim(h)[1], 0,1);
names(h) <- c("AGS", "zahl"); geo <- merge(geo, h, by="AGS")
        # (das lässt sich sicher eleganter programmieren)

# Farbpalette entwerfen
        # hier bitte das untere und obere Ende der gewünschten Farbpalette eintragen. 
colfunc <- colorRampPalette(c("#f6dae8", "#740042"))
        # colfunc(n) gibt die Feinheit der Abstufungen an.
spplot(geo, "zahl", col.regions = colfunc(30))

# jetzt nicht graduell, sondern kategorial (gruppiert)
geo$zahl.g <- cut2(geo$zahl, cuts=c(0,.2,.4,.6,.8,1))
geo$zahl.g <- factor(geo$zahl.g, labels=c("bis 20%", "20 bis 40%", "40 bis 60%", "60 bis 80%", "80 bis 100%"))

spplot(geo, "zahl.g", col.regions = colfunc(length(levels(geo$zahl.g))))

# Jetzt mit ggplot2 / ggmap
geo2 <- fortify(geo, region="AGS")
geo.merge <- geo[,c(1,24)]
geo2 <- merge(geo2, geo.merge, by.x="id", by.y="AGS")

ditch_the_axes <- theme(
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank()
)

check <- ggplot() +
        geom_polygon(aes(x=long, y=lat, group=group, fill = geo2$zahl), 
                     size=.2,
                     color=gray(0.5), 
                     data=geo2) + scale_fill_gradient(name="Zufallszahl", low="#f6dae8", high="#740042") +
                        ditch_the_axes; check

# jetzt wieder gruppiert
geo2$zahl.g <- cut2(geo2$zahl, cuts=c(0,.2,.4,.6,.8,1))
geo2$zahl.g <- factor(geo2$zahl.g, labels=c("bis 20%", "20 bis 40%", "40 bis 60%", "60 bis 80%", "80 bis 100%"))

check <- ggplot() +
        geom_polygon(aes(x=long, y=lat, group=group, fill = geo2$zahl.g), 
                     size=.2, 
                     color=gray(0.5), 
                     data=geo2) + scale_fill_manual(name="Zufallszahl", values=colfunc(5)) +
                         ditch_the_axes; check

# Bundesländer markieren
geo$land <- substr(geo$AGS, 1, 2)

geo3 <- fortify(geo, region="land")

check <- check + 
        geom_polygon(aes(x=long, y=lat, group=group), 
                     size=1, fill=NA,
                     color="blue", 
                     data=geo3) 

pdf(file="Deutschlandkarte.pdf", pointsize=12)
check
dev.off()
