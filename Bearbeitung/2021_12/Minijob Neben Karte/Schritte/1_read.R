# read data

# Am besten einfach "dat" nennen. 

# Kreise (401)----

# key sollte AGS lauten!

dat <-
  read.xlsx(
    "./Input/Helge Nebenjob geringfügig entlohnte Beschäftigte März 2021.xlsx",
    sheet = "Tabelle1", startRow = 1, colNames = T, cols = 1:5
  ) %>%
  as_tibble() %>%
  rename(AGS = 1, Kreis = 2, `Gesamt (2021)` = 3, `Männer (2021)` = 4, `Frauen (2021)` = 5) %>%
  mutate(AGS = if_else(nchar(AGS) == 4, paste0(0, AGS), as.character(AGS))) %>%
  select(AGS, Kreis, everything()) %>%
  mutate("Gruppe 2021" = cut(`Gesamt (2021)`,
                             breaks=c(-1, 5, 7, 9, 11, 101),
                             labels=c("bis 5", "5 bis 7",
                                      "7 bis 9", "9 bis 11",
                                      "11 und mehr"))) 

