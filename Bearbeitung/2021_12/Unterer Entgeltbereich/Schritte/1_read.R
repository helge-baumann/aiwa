# read data

# Am besten einfach "dat" nennen. 

# Kreise (401)----

# key sollte AGS lauten!

dat <-
  read.xlsx(
    "./Input/Helge Bearbeitung_unterer_entgeltbereich 21 11 16.xlsx",
    sheet = "Geschlecht", startRow = 1, colNames = T, cols = 1:5
  ) %>%
  as_tibble() %>%
  rename(AGS = 1, Kreis = 2, `Gesamt (2020)` = 3, `Männer (2020)` = 4, `Frauen (2020)` = 5) %>%
  mutate(AGS = if_else(nchar(AGS) == 4, paste0(0, AGS), as.character(AGS))) %>%
  select(AGS, Kreis, everything()) %>%
  mutate("Gruppe 2020" = cut(`Gesamt (2020)`,
                             breaks=c(-1, 15, 20, 25, 30, 101),
                             labels=c("bis 15", "15,1 bis 20",
                                      "20,1 bis 25", "25,1 bis 30",
                                      "30,1 und mehr"))) 
