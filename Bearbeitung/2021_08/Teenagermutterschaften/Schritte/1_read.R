# read data

# Am besten einfach "dat" nennen. 

# Kreise (401)----

# key sollte AGS lauten!

dat <-
  read.xlsx(
    "./Input/Teeniemütterquote & SGB II Quote 2019.xlsx",
    sheet = "Tabelle1", startRow = 4, colNames = F, 
    cols=1:5
  ) %>%
  as_tibble() %>%
  rename(AGS = X2, Kreis = X1, 
         `Anteil (2019)` = X3, `Anzahl (2019)` = X4, `SGB-II-Quote (2019)` = X5) %>%
  filter(str_detect(AGS, "[:digit:]{5}") %in% T | AGS %in% c("02", "11")) %>%
  select(AGS, Kreis, everything()) %>%
  mutate("Gruppe 2019" = cut(`Anteil (2019)`,
                             breaks=c(-1, 5, 7.5, 10, 15, 10000),
                             labels=c("bis 5 \u2030", "5,1 bis 7,5 \u2030",
                                      "7,6 bis 10 \u2030", "10,1 bis 15 \u2030",
                                      "15 und mehr \u2030"))) 

dat$AGS[dat$AGS == "02"] <- "02000"
dat$AGS[dat$AGS == "11"] <- "11000"
  
  
