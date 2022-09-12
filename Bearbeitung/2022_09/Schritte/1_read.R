# read data

# Am besten einfach "dat" nennen. 

# Kreise (401)----

# key sollte AGS lauten!

dat <-
  read.xlsx(
    "./Input/Helge Mindestlohnreichweite regional 22 09 11.xlsx",
    sheet = "Helge", startRow = 2, colNames = F
  ) %>%
  as_tibble() %>%
  rename(AGS = X1, Kreis = X2, Quote = X3, Anzahl = X4) %>%
  mutate(AGS = as.character(AGS), 
         AGS = if_else(nchar(AGS) == 4, paste0("0", AGS), AGS)) %>%
  mutate("Gruppe" = cut(`Quote`,
                             breaks=c(-1, 15, 17.5, 20, 22.5, 101),
                             labels=c("bis 15%", "15 bis 17,5%",
                                      "17,5 bis 20%", "20 bis 22,5%",
                                      "22,5% und mehr"))) 
  
  
