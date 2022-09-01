# read data

# Am besten einfach "dat" nennen. 

# Kreise (401)----

# key sollte AGS lauten!

dat <-
  read.xlsx(
    "./Input/wsi_aiwa-minjob 21 02 15.xlsx",
    sheet = "30.06.2017", startRow = 10, colNames = T, cols = c(1,5:7)
  ) %>%
  as_tibble() %>%
  rename(ID = X1, `Gesamt (2017)` = 2, `Männer (2017)` = 3, `Frauen (2017)` = 4) %>%
  filter(str_detect(ID, "[:digit:]{5} ") %in% T) %>%
  mutate(
    AGS = str_extract(ID, "[:digit:]{5}"),
    Kreis = str_remove(ID, "[:digit:]{5} ")
  ) %>%
  select(AGS, Kreis, everything(), -ID) %>%
  mutate("Gruppe 2017" = cut(`Gesamt (2017)`,
                             breaks=c(-1, 5, 7, 9, 11, 101),
                             labels=c("bis 5", "5 bis 7",
                                      "7 bis 9", "9 bis 11",
                                      "11 und mehr"))) 

for(i in 3:5) {
  
  datneu <-
    read.xlsx(
      "./Input/wsi_aiwa-minjob 21 02 15.xlsx",
      sheet = paste0("30.06.20", 15+i), startRow = 10, colNames = T, cols = c(1, 5:7)
    ) %>%
    as_tibble() %>%
    rename(ID = X1, !!paste0("Gesamt (20", 15+i, ")") := 2, 
           !!paste0("Männer (20", 15+i, ")") := 3, 
           !!paste0("Frauen (20", 15+i, ")") := 4) %>%
    filter(str_detect(ID, "[:digit:]{5} ") %in% T) %>%
    mutate(
      AGS = str_extract(ID, "[:digit:]{5}"),
      Kreis = str_remove(ID, "[:digit:]{5} ")
    ) %>%
    select(AGS, Kreis, everything(), -ID) %>%
    mutate(!!paste0("Gruppe 20", 15+i) := cut(get(paste0("Gesamt (20", 15+i, ")")),
                                              breaks=c(-1, 5, 7, 9, 11, 101),
                                              labels=c("bis 5", "5 bis 7",
                                                       "7 bis 9", "9 bis 11",
                                                       "11 und mehr"))) 
  
  dat <- left_join(dat, datneu, by="AGS")
  
}

# 2021
i <- 6
datneu <-
  read.xlsx(
    "./Input/Helge Befristete Minijobber Leiharbeiter 22 01 26.xlsx",
    sheet="Nebenjob in %", startRow = 1, colNames = T, cols = 1:5
  ) %>%
  as_tibble() %>%
  rename(AGS = 1, !!paste0("Gesamt (20", 15+i, ")") := 3, 
         !!paste0("Männer (20", 15+i, ")") := 4, 
         !!paste0("Frauen (20", 15+i, ")") := 5) %>%
  mutate(AGS = if_else(nchar(AGS) == 4, paste0("0", AGS), as.character(AGS))) %>%
  select(AGS, Kreis, everything()) %>%
  mutate(!!paste0("Gruppe 20", 15+i) := cut(get(paste0("Gesamt (20", 15+i, ")")),
                                            breaks=c(-1, 5, 7, 9, 11, 101),
                                            labels=c("bis 5", "5 bis 7",
                                                     "7 bis 9", "9 bis 11",
                                                     "11 und mehr")))

dat <- left_join(dat, datneu, by="AGS")


 
 