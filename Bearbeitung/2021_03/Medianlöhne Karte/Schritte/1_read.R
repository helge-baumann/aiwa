# read data

# Am besten einfach "dat" nennen. 

# Kreise (401)----

# key sollte AGS lauten!

dat <-
  read.xlsx(
    "./Input/wsi_aiwa_median 21 01 21.xlsx",
    sheet = "2016", startRow = 8, colNames = F, cols = 1:5
  ) %>%
  as_tibble() %>%
  rename(Kreis = X1, AGS =X2, `Gesamt (2016)` = 3, `Männer (2016)` = 4, `Frauen (2016)` = 5) %>%
  filter(str_detect(AGS, "[:digit:]{5}") %in% T) %>%
  select(AGS, Kreis, everything()) %>%
  mutate("Gruppe 2016" = cut(`Gesamt (2016)`,
                             breaks=c(-1, 2750, 3000, 3250, 3500, 100000),
                             labels=c("bis 2750", "2751 bis 3000",
                                      "3001 bis 3250", "3251 bis 3500",
                                      "3501 und mehr"))) 

for(i in 2:4) {
  
  datneu <-
    read.xlsx(
      "./Input/wsi_aiwa_median 21 01 21.xlsx",
      sheet = paste0("20", 15+i), startRow = 8, colNames = F, cols = 1:5
    ) %>%
    as_tibble() %>%
    rename(Kreis = X1, AGS =X2, !!paste0("Gesamt (20", 15+i, ")") := 3, 
           !!paste0("Männer (20", 15+i, ")") := 4, 
           !!paste0("Frauen (20", 15+i, ")") := 5) %>%
    filter(str_detect(AGS, "[:digit:]{5}") %in% T) %>%
    select(AGS, Kreis, everything()) %>%
    mutate(!!paste0("Gruppe 20", 15+i) := cut(get(paste0("Gesamt (20", 15+i, ")")),
                                              breaks=c(-1, 2750, 3000, 3250, 3500, 100000),
                                              labels=c("bis 2750", "2751 bis 3000",
                                                       "3001 bis 3250", "3251 bis 3500",
                                                       "3501 und mehr"))) 
  
  dat <- left_join(dat, datneu, by="AGS")
  
}



