# read data

# AMR
amr <- xlsx::read.xlsx(
  "./Daten/Helge Leiharbeiter Regression 19 09 12.xlsx",
  sheetIndex=1, 
  startRow=11, header=T, 
  colIndex=1:23, # AGS, Teenage-Motherhood-Quote
  encoding="UTF-8",
  stringsAsFactors=F
) %>%
  as_tibble() %>%
  mutate(AGS = str_extract(Region, "[:digit:]{5}")) %>%
  mutate(Kreis = str_remove(Region, "[:digit:]{5} ")) %>%
  select(AGS, Kreis, amr, amr_name, amr_styp3, NA.)

# Hauptdaten----
dat_mj <- xlsx::read.xlsx(
  "./Daten/Berechnungen Kurztexte Regionales 20 04 22.xlsx",
  sheetIndex=2, 
  startRow=10, header=T, 
  colIndex=1:10, # AGS, Teenage-Motherhood-Quote
  encoding="UTF-8",
  stringsAsFactors=F
) %>% 
  as_tibble() %>%
  filter(str_detect(NA., "[:digit:]{5} ") %in% T) %>%
  mutate(AGS = str_extract(NA., "[:digit:]{5}")) %>%
  mutate(Kreis = str_remove(NA., "[:digit:]{5} ")) %>%
  select(NA., AGS, Kreis, everything())

dat_leih <- xlsx::read.xlsx(
  "./Daten/Berechnungen Kurztexte Regionales 20 04 22.xlsx",
  sheetIndex=3, 
  startRow=10, header=T, 
  colIndex=1:7, # AGS, Teenage-Motherhood-Quote
  encoding="UTF-8",
  stringsAsFactors=F
) %>% 
  as_tibble() %>%
  filter(str_detect(NA., "[:digit:]{5} ") %in% T) %>%
  mutate(AGS = str_extract(NA., "[:digit:]{5}")) %>%
  mutate(Kreis = str_remove(NA., "[:digit:]{5} ")) %>%
  select(NA., AGS, Kreis, everything())

dat_leih[is.na(dat_leih[,2]), 2] <- dat_leih[is.na(dat_leih[,2]), 1]

dat_befr <- xlsx::read.xlsx(
  "./Daten/Berechnungen Kurztexte Regionales 20 04 22.xlsx",
  sheetIndex=4, 
  startRow=10, header=T, 
  colIndex=1:7, # AGS, Teenage-Motherhood-Quote
  encoding="UTF-8",
  stringsAsFactors=F
) %>% 
  as_tibble() %>%
  filter(str_detect(NA., "[:digit:]{5} ") %in% T) %>%
  mutate(AGS = str_extract(NA., "[:digit:]{5}")) %>%
  mutate(Kreis = str_remove(NA., "[:digit:]{5} ")) %>%
  select(NA., AGS, Kreis, everything())

dat_solo <- xlsx::read.xlsx(
  "./Daten/Solo-Selbständige für 2019 Helge.xlsx",
  sheetIndex=1, 
  startRow=3, header=T, 
  colIndex=1:4, 
  encoding="UTF-8",
  stringsAsFactors=F
) %>% 
  as_tibble() %>%
  mutate(GEN = NA.) %>%
  filter(GEN != "Deutschland") %>%
  select(GEN, everything(), -NA.)

dat_median <- xlsx::read.xlsx(
  "./Daten/wsi_aiwa_bruttoentgelte.xlsx",
    sheetIndex=2, 
    startRow=9, header=F, 
    colIndex=1:5, # AGS, Teenage-Motherhood-Quote
    encoding="UTF-8",
    stringsAsFactors=F
  ) %>% 
    as_tibble() %>%
    filter(str_detect(X2, "[:digit:]{5}") %in% T) 
  
dat_av <- openxlsx::read.xlsx(
  "./Daten/Arbeitsvolumen nach Kreisen 20 12 03.xlsx",
  sheet=1, 
  startRow=8, colNames=F, 
  cols=c(1,3,7,8,12)
) %>% 
  as_tibble() 

farben <- function(x) {
  
  if(x == "tuerkis") return(c("#e5f2f3", "#2b5f65"))
  if(x == "grau") return(c("#f9f9f4", "#64635c"))
  if(x == "lila") return(c("#dfd5e8", "#431d57"))
  if(x == "blau") return(c("#d3e3ee", "#00384f"))
  if(x == "pink") return(c("#f6dae8", "#740042"))

  
}
