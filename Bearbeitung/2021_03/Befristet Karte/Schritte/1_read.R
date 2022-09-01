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
