# AMR

# Vorbereitung ----
dat_leih <- read.xlsx(
  "./Data preparation/Input/Berechnungen Kurztexte Regionales 20 04 22.xlsx",
  sheet = 3, startRow=10, colNames=T, cols=1:7
) %>% 
  as_tibble() %>%
  filter(str_detect(X1, "[:digit:]{5} ") %in% T) %>%
  mutate(AGS = str_extract(X1, "[:digit:]{5}")) %>%
  mutate(Kreis = str_remove(X1, "[:digit:]{5} ")) %>%
  select(AGS, Kreis, everything(), -X1) %>%
  rename("X1" = `1`, "X2" = `2`, "X3" = `3`,
         "X4" = `4`, "X5" = `5`, "X6" = `6`)

amr <- read.xlsx(
  "./Data preparation/Input/Helge Leiharbeiter Regression 19 09 12.xlsx",
  sheet=1, 
  startRow=11, colNames=T, 
 cols=1:23 # AGS, Teenage-Motherhood-Quote
) %>%
  as_tibble() %>%
  mutate(AGS = str_extract(Region, "[:digit:]{5}")) %>%
  mutate(Kreis = str_remove(Region, "[:digit:]{5} ")) %>%
  select(AGS, Kreis, amr, amr_name, amr_styp3, X22)

amr_leih <- left_join(dat_leih, amr, by="AGS")
amr_leih[is.na(amr_leih$amr_name), "amr_name"] <- 
  amr_leih[is.na(amr_leih$amr_name), "Kreis.x"]

amr_summen <- amr_leih %>%
  mutate(amr_name = as_factor(amr_name)) %>%
  group_by(amr_name) %>%
  mutate(X6 = as.numeric(X6)) %>%
  filter(!is.na(X1)) %>%
  summarise(Insgesamt_Beschaeftigte = sum(X1),
            Insgesamt_Leiharbeitnehmer = sum(X2),
            Insgesamt_Maenner = sum(X3),
            Maenner_Leiharbeitnehmer = sum(X4),
            Insgesamt_Frauen = sum(X5),
            Frauen_Leiharbeitnehmer = sum(X6)) 

amr_summen <- 
  left_join(
    amr_summen, 
    unique(amr[,c("amr", "amr_name", "amr_styp3", "X22")]), 
    by="amr_name", 
    all=T) %>%
  rename(Kategorie = X22, Arbeitsmarktregion = amr_name) %>%
  replace(is.na(.), "") 
  
amr_quoten <- amr_summen %>%
  mutate(Gesamt = Insgesamt_Leiharbeitnehmer/Insgesamt_Beschaeftigte*100,
         Männer = Maenner_Leiharbeitnehmer/Insgesamt_Maenner*100,
         Frauen = Frauen_Leiharbeitnehmer/Insgesamt_Frauen*100) %>%
  select(amr, Arbeitsmarktregion, Kategorie, Gesamt, Männer, Frauen )



# Data----
Data[["leiharbeitnehmer"]][["dat"]] <- amr_quoten

Data[["leiharbeitnehmer"]][["title"]] <-
  "Leiharbeit in den Arbeitsmarktregionen, Juni 2019"
Data[["leiharbeitnehmer"]][["subtitle"]] <- 
  "Anteil der Beschäftigten in der Leiharbeit an allen Beschäftigten, in Prozent"
Data[["leiharbeitnehmer"]][["hinweis"]] <- 
  "* bezieht sich auf den Wohnort"
Data[["leiharbeitnehmer"]][["quelle"]] <- 
  "Sonderauswertung der \n Bundesagentur für Arbeit, eigene Berechnungen"

Data[["leiharbeitnehmer"]][["cuts"]][["Gesamt"]] <- 
  c(-1, 1.5, 2, 2.25, 2.6, 6)

Data[["leiharbeitnehmer"]][["cuts"]][["Männer"]] <- 
  c(-1, 2, 2.5, 3, 3.5, 7)

Data[["leiharbeitnehmer"]][["cuts"]][["Frauen"]] <- 
  c(-1, 1, 1.2, 1.4, 1.6, 5)

Data[["leiharbeitnehmer"]][["Einheit"]] <- "%"
Data[["leiharbeitnehmer"]][["Ebene"]] <- "AMR"
Data[["leiharbeitnehmer"]][["Key"]] <- "AMR"
