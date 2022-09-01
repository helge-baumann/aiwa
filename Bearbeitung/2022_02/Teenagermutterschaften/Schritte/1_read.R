# read data

# Am besten einfach "dat" nennen. 

# Kreise (401)----

# key sollte AGS lauten!

dat <-
  read.xlsx(
    "./Input/Helge Teenagermütterdaten 20 01 19.xlsx",
    sheet = "Tabelle1", startRow = 2, colNames = F, 
    cols=1:5
  ) %>%
  as_tibble() %>%
  rename(AGS = X2, Kreis = X1, 
         `Anteil (2020)` = X3, `Anzahl (2020)` = X4, `SGB-II-Quote (2020)` = X5) %>%
  filter(str_detect(AGS, "[:digit:]{5}") %in% T) %>%
  select(AGS, Kreis, everything()) %>%
  mutate("Gruppe 2020" = cut(`Anteil (2020)`,
                             breaks=c(-1, 5, 7.5, 10, 15, 10000),
                             labels=c("bis 5 \u2030", "5,1 bis 7,5 \u2030",
                                      "7,6 bis 10 \u2030", "10,1 bis 15 \u2030",
                                      "15 und mehr \u2030"))) 


  
  
