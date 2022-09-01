# read data

# Am besten einfach "dat" nennen. 

# Kreise (401)----

# key sollte AGS lauten!

dat20 <-
  read.xlsx(
    "./Input/wsi_aiwa-befristet 21 03 03.xlsx",
    sheet = "2020 Q2", startRow = 10, colNames = T, cols = c(1, 8:10)
  ) %>%
  as_tibble() %>%
  rename(ID = X1, `Gesamt (2020)` = 2, `Männer (2020)` = 3, `Frauen (2020)` = 4) %>%
  filter(str_detect(ID, "[:digit:]{5} ") %in% T) %>%
  mutate(
    AGS = str_extract(ID, "[:digit:]{5}"),
    Kreis = str_remove(ID, "[:digit:]{5} ")
  ) %>%
  select(AGS, Kreis, everything(), -ID) %>%
  mutate("Gruppe 2020" = cut(`Gesamt (2020)`,
                               breaks=c(-1, 30, 33, 36, 39, 101),
                               labels=c("bis 30", "30 bis 33",
                                        "33 bis 36", "36 bis 39",
                                        "39 und mehr"))) 

dat19 <-
  read.xlsx(
    "./Input/wsi_aiwa-befristet 21 03 03.xlsx",
    sheet = "2019 Q2", startRow = 10, colNames = T, cols = c(1, 8:10)
  ) %>%
  as_tibble() %>%
  rename(ID = X1, `Gesamt (2019)` = 2, `Männer (2019)` = 3, `Frauen (2019)` = 4) %>%
  filter(str_detect(ID, "[:digit:]{5} ") %in% T) %>%
  mutate(
    AGS = str_extract(ID, "[:digit:]{5}"),
    Kreis = str_remove(ID, "[:digit:]{5} ")
  ) %>%
  select(AGS, Kreis, everything(), -ID) %>%
  mutate("Gruppe 2019" = cut(`Gesamt (2019)`,
                             breaks=c(-1, 30, 33, 36, 39, 101),
                             labels=c("bis 30", "30 bis 33",
                                      "33 bis 36", "36 bis 39",
                                      "39 und mehr"))) 

i <- 6
dat21 <-
  read.xlsx(
    "./Input/Helge Befristete Minijobber Leiharbeiter 22 01 26.xlsx",
    sheet="Befristete Einstellungen in %", startRow = 2, colNames = T, cols = 1:5
  ) %>%
  as_tibble() %>%
  rename(AGS = 1, Kreis=2,  !!paste0("Gesamt (20", 15+i, ")") := 3, 
         !!paste0("Männer (20", 15+i, ")") := 4, 
         !!paste0("Frauen (20", 15+i, ")") := 5) %>%
  mutate(AGS = if_else(nchar(AGS) == 4, paste0("0", AGS), as.character(AGS))) %>%
  select(AGS, Kreis, everything()) %>%
  mutate(!!paste0("Gruppe 20", 15+i) := cut(get(paste0("Gesamt (20", 15+i, ")")),
                                            breaks=c(-1, 30, 33, 36, 39, 101),
                                            labels=c("bis 30", "30 bis 33",
                                                     "33 bis 36", "36 bis 39",
                                                     "39 und mehr"))) 
