# read data

# Am besten einfach "dat" nennen. 

# Kreise (401)----

# key sollte AGS lauten!

dat <-
  read.xlsx(
    "./Input/Helge Verfügbare & Primäreinkommen 22 01 19.xlsx",
    sheet = "Tabelle1", startRow = 1, colNames = T, 
    cols=1:5
  ) %>%
  as_tibble() %>%
  rename(AGS = 1, Kreis = 2, 
         `Primäreinkommen pro Kopf (2019)` = 3, `Verfügbares Einkommen pro Kopf (2019)` = 4) %>%
  mutate(AGS = if_else(nchar(AGS) == 5, as.character(AGS), paste0("0", AGS))) %>%
  filter(str_detect(AGS, "[:digit:]{5}") %in% T) %>%
  select(AGS, Kreis, everything()) %>%
  mutate("Gruppe 2019" = cut(`Verfügbares Einkommen pro Kopf (2019)`,
                             breaks=c(-1, 21500, 23000, 24500, 26000, 100000),
                             labels=c("bis unter 21.500", "21.500 bis unter 23.000",
                                      "23.000 bis unter 24.500", "24.500 bis unter 26.000",
                                      "26.000 und mehr"))) 


  
  
