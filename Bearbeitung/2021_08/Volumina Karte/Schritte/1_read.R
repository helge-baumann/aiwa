# read data

# Am besten einfach "dat" nennen. 

# Kreise (401)----

# key sollte AGS lauten!

dat <-
  read.xlsx(
    "./Input/wsi_aiwa_volumina 21 03 24.xlsx",
    sheet = "2000 bis 2018", startRow = 8, colNames = F, 
    cols=sort(c(209, 210, 211+c(0, 13, 26), 215 + c(0, 13, 26),
           216+c(0, 13, 26), 220+c(0, 13, 26)))
  ) %>%
  as_tibble() %>%
  rename(AGS = X1, Kreis = X2, 
         `Gesamt (2016)` = X3, `Verarbeitendes Gewerbe (2016)` = X4, `Baugewerbe (2016)` = X5, `Öffentliche Dienstleister (2016)` = X6,
         `Gesamt (2017)` = X7, `Verarbeitendes Gewerbe (2017)` = X8, `Baugewerbe (2017)` = X9, `Öffentliche Dienstleister (2017)` = X10,
         `Gesamt (2018)` = X11, `Verarbeitendes Gewerbe (2018)` = X12, `Baugewerbe (2018)` = X13, `Öffentliche Dienstleister (2018)` = X14
         ) %>%
  filter(str_detect(AGS, "[:digit:]{5}") %in% T | AGS %in% c("02", "11")) %>%
  select(AGS, Kreis, everything()) %>%
  mutate("Gruppe 2016" = cut(`Gesamt (2016)`,
                             breaks=c(-1, 1300, 1325, 1350, 1375, 10000),
                             labels=c("bis 1300", "1301 bis 1325",
                                      "1326 bis 1350", "1351 bis 1375",
                                      "1376 und mehr"))) %>%
  mutate("Gruppe 2017" = cut(`Gesamt (2017)`,
                             breaks=c(-1, 1300, 1325, 1350, 1375, 10000),
                             labels=c("bis 1300", "1301 bis 1325",
                                      "1326 bis 1350", "1351 bis 1375",
                                      "1376 und mehr"))) %>%
  mutate("Gruppe 2018" = cut(`Gesamt (2018)`,
                             breaks=c(-1, 1300, 1325, 1350, 1375, 10000),
                             labels=c("bis 1300", "1301 bis 1325",
                                      "1326 bis 1350", "1351 bis 1375",
                                      "1376 und mehr"))) %>%
  left_join(
    read.xlsx(
  "./Input/Arbeitsstunden (2019) für Helge.xlsx",
  sheet = "Tabelle1", startRow = 8, colNames = F, 
  cols=sort(c(1:3, 7, 8, 12))
) %>%
  as_tibble() %>%
  rename(AGS = X1, Kreis = X2, 
         `Gesamt (2019)` = X3, `Verarbeitendes Gewerbe (2019)` = X4, 
         `Baugewerbe (2019)` = X5, `Öffentliche Dienstleister (2019)` = X6
  ) %>%
  filter(str_detect(AGS, "[:digit:]{5}") %in% T | AGS %in% c("02", "11")) %>%
  select(AGS, Kreis, everything()) %>%
  mutate("Gruppe 2019" = cut(`Gesamt (2019)`,
                             breaks=c(-1, 1300, 1325, 1350, 1375, 10000),
                             labels=c("bis 1300", "1301 bis 1325",
                                      "1326 bis 1350", "1351 bis 1375",
                                      "1376 und mehr"))) 
)

dat$AGS[dat$AGS == "02"] <- "02000"
dat$AGS[dat$AGS == "11"] <- "11000"
  
  
