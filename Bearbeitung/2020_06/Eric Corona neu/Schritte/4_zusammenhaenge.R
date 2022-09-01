# Zusammenhänge zwischen Variablen und Fällen/Tioten? Siehe Mail Eric

# laden----
rki  <- openxlsx::read.xlsx(
  "./Daten/2020-05-15faelle_quoten.xlsx",
  sheet=2, 
  startRow=1, colNames=T,
  cols=1:5
) %>% as_tibble()

dat <- openxlsx::read.xlsx(
  "./Daten/Corona Effekt 20 05 14.xlsx",
  sheet=1, 
  startRow=10, colNames=T,
  cols=c(1:16)
) %>% 
  as_tibble() %>%
  filter(str_detect(X1, "[:digit:]{5} ") %in% T) %>%
  mutate(AGS = str_extract(X1, "[:digit:]{5}")) %>%
  mutate(Kreis = str_remove(X1, "[:digit:]{5} ")) %>%
  select(AGS, Kreis, everything()) %>%
  select(-X1) 

names(dat) <- c("AGS", "Kreis", "CoronaQuote",
                    "CoronaEffekt", "Kurzarbeiter", "ALmaerz", "ALapril",
                    "Bezug", "KAma", "AM2020", "AM2019", "Befr", "Leih", 
                    "Kurzarbeiterquote","SVP", 
                    "Gross",
                     "CoronaQuoteAL")
# manipulieren und mergen----
rki <- rki[!is.na(rki$IdLandkreis),]
rki$IdLandkreis <- as.character(rki$IdLandkreis)

for(i in 1:nrow(rki)) {
  
  if(nchar(rki$IdLandkreis[i]) == 4) {
    
    rki$IdLandkreis[i] <- paste0("0", rki$IdLandkreis[i])
    
  }
  
}

dat <- merge(dat, rki, by.x="AGS", by.y="IdLandkreis", all.y=F)

# untersuchen

# (1) Fälle und Kurzarbeiter
p1 <- ggplot(dat, aes(x=CoronaQuote, y=Kurzarbeiterquote)) + 
  geom_point() +
  geom_smooth() +
  labs(x="Corona-bedingte Zunahme der Arbeitslosigkeit")
ggsave("./Output/ZunahmeAL-Kurzarbeiter.png", dpi=100)

cor.test(dat$CoronaQuote, dat$Kurzarbeiterquote)

# (2) Fälle und Kurzarbeiter
p2 <- ggplot(dat, aes(x=Gross, y=CoronaQuote)) + 
  geom_point() +
  geom_smooth() +
  labs(x="Anteil in Großbetrieben")
ggsave("./Output/Großbetriebe-Corona.png")

cor.test(dat$Gross, dat$CoronaQuote)

# (3) Corona-Quote und März
p3 <- ggplot(dat, aes(x=ALmaerz, y=CoronaQuote)) + 
  geom_point() +
  geom_smooth() +
  labs(x="Arbeitslosigkeit März 2020")
ggsave("./Output/März-Coronaquote.png")

cor.test(dat$ALmaerz, dat$CoronaQuote)

# (1) Fälle und Kurzarbeiter
p4 <- ggplot(dat, aes(x=Quote, y=CoronaQuote)) + 
  geom_point() +
  geom_smooth() +
  labs(x="Corona-Inzidenz pro 100.000 EW")
ggsave("./Output/Corona-Kurzarbeiter.png")

cor.test(dat$Quote, dat$CoronaQuote)
