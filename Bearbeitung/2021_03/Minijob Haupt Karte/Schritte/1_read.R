# read data

# Am besten einfach "dat" nennen. 

# Kreise (401)----

# key sollte AGS lauten!

dat <-
  read.xlsx(
    "./Input/wsi_aiwa-minjob 21 02 15.xlsx",
    sheet = "30.06.2017", startRow = 10, colNames = T, cols = 1:4
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
                                  breaks=c(-1, 7.5, 10, 12.5, 15, 101),
                                  labels=c("bis 7,5", "7,5 bis 10",
                                           "10 bis 12,5", "12,5 bis 15",
                                           "15 und mehr"))) 

for(i in 3:5) {
  
  datneu <-
    read.xlsx(
      "./Input/wsi_aiwa-minjob 21 02 15.xlsx",
      sheet = paste0("30.06.20", 15+i), startRow = 10, colNames = T, cols = 1:4
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
                               breaks=c(-1, 7.5, 10, 12.5, 15, 101),
                               labels=c("bis 7,5", "7,5 bis 10",
                                        "10 bis 12,5", "12,5 bis 15",
                                        "15 und mehr"))) 
  
  dat <- left_join(dat, datneu, by="AGS")
  
}

