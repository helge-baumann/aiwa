Data[["bruttoarbeitsentgelte"]][["dat"]] <-
      read.xlsx(
        "./Data preparation/Input/wsi_aiwa_bruttoentgelte.xlsx",
        sheet = 2, startRow = 7, colNames = T, cols = 2:5
      ) %>%
  as_tibble() %>%
  rename(
    "AGS" = X1, "Gesamt" = `1`, "Männer" = `2`, "Frauen" = `3`
  ) %>%
  filter(str_detect(AGS, "[:digit:]{5}") %in% T) %>%
  select(AGS, everything())

Data[["bruttoarbeitsentgelte"]][["title"]] <-
  "Bruttoarbeitsentgelte 2019"
Data[["bruttoarbeitsentgelte"]][["subtitle"]] <- 
  "Mittlere monatliche Arbeitsentgelte"
Data[["bruttoarbeitsentgelte"]][["quelle"]] <- 
  "Bundesagentur für Arbeit, 2020"

Data[["bruttoarbeitsentgelte"]][["cuts"]][["Gesamt"]] <- 
  c(-1, 2900, 3150, 3300, 3600, 5200)

Data[["bruttoarbeitsentgelte"]][["cuts"]][["Männer"]] <- 
  c(-1, 3000, 3250, 3500, 3800, 5700)

Data[["bruttoarbeitsentgelte"]][["cuts"]][["Frauen"]] <- 
  c(-1, 2600, 2800, 2950, 3150, 5000)

Data[["bruttoarbeitsentgelte"]][["Einheit"]] <- "Euro"
Data[["bruttoarbeitsentgelte"]][["Ebene"]] <- "KRS"
Data[["bruttoarbeitsentgelte"]][["Key"]] <- "AGS"

