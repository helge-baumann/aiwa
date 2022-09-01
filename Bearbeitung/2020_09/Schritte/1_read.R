# read data

# Medianeinkommen----
dat_med <- openxlsx::read.xlsx(
  "./Daten/Medianentgelte 2019 Helge Jutta.xlsx",
  sheet=1, 
  startRow=9, colNames=F,
  cols=c(1:5)
) %>% 
  as_tibble() %>%
  mutate(
    AGS = X2,
    Kreis = X1, zahl1 = X3, zahl2 = X4, zahl3 = X5) %>%
  filter(!is.na(X2)) %>%
  select(AGS, Kreis, zahl1, zahl2, zahl3) 

# Soloselbständige
dat_vol <- openxlsx::read.xlsx(
  "./Daten/Standardarbeitsvolumen.xlsx",
  sheet=13, 
  startRow=7, colNames=F,
  cols=c(3, 8, 26)
) %>% 
  as_tibble()

dat_vol$X1[dat_vol$X1 == "02"] <- "02000"
dat_vol$X1[dat_vol$X1 == "11"] <- "11000"

dat_vol <- dat_vol %>% 
  filter(str_detect(X1, "[:digit:]{5}") %in% T) %>%
  mutate(
    AGS = X1,
    Kreis = X2, zahl1 = X3) %>%
  filter(!is.na(X1)) %>%
  select(AGS, Kreis, zahl1) 


