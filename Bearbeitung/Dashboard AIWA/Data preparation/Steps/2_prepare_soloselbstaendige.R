Data[["soloselbstaendige"]][["dat"]] <-
  read.xlsx(
    "./Data preparation/Input/Solo-Selbständige für 2019 Helge.xlsx",
    sheet = 1, startRow = 3, colNames = T, cols = 1:4
  ) %>%
  as_tibble() %>%
  rename(
    "GEN" = X1, "Gesamt" = Insgesamt
  ) %>%
  select(GEN, Gesamt, everything())

Data[["soloselbstaendige"]][["title"]] <-
  "Soloselbständige 2019"
Data[["soloselbstaendige"]][["subtitle"]] <- 
  "Angaben in %"
Data[["soloselbstaendige"]][["quelle"]] <- 
  "Bundesagentur für Arbeit, 2020"

Data[["soloselbstaendige"]][["cuts"]][["Gesamt"]] <- 
  c(-1, 4.4, 4.6, 5.1, 5.5, 11)

Data[["soloselbstaendige"]][["cuts"]][["Männer"]] <- 
  c(-1, 4.8, 5.1, 5.9, 6.6, 13)

Data[["soloselbstaendige"]][["cuts"]][["Frauen"]] <- 
  c(-1, 3.7, 4.0, 4.3, 4.5, 10)

Data[["soloselbstaendige"]][["Einheit"]] <- "%"
Data[["soloselbstaendige"]][["Ebene"]] <- "LAN"
Data[["soloselbstaendige"]][["Key"]] <- "GEN"

