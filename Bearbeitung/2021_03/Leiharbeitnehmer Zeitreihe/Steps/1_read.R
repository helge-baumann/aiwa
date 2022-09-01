# Daten mit read_dta() oder read_sav() einlesen, 
# mit write_sav oder write_dta() aufs Laufwerk schreiben. 

# Daten sollten im Unterordner /Input liegen.

# Leiharbeitnehmer 
dat15 <- leiharbeit_extract(
  "./Input/Leiharbeiter Helge 21 03 02.xlsx", cols=1:7, startRow=11, sheet=1
)

dat16 <- leiharbeit_extract(
  "./Input/Leiharbeiter Helge 21 03 02.xlsx", cols=10:16, startRow=11, sheet=1
)

dat17 <- leiharbeit_extract(
  "./Input/Leiharbeiter Helge 21 03 02.xlsx", cols=1:7, startRow=11, sheet=2
)

dat18 <- leiharbeit_extract(
  "./Input/Leiharbeiter Helge 21 03 02.xlsx", cols=10:16, startRow=11, sheet=2
)

dat20 <- leiharbeit_extract(
  "./Input/Leiharbeiter Helge 21 03 02.xlsx", cols=19:25, startRow=11, sheet=2
)

# Arbeitsmarktregionen
amr_alt <- read.xlsx("./Input/Key_AMR.xlsx", sheet=2) %>% as_tibble() %>%
  mutate(krs15 = str_remove(krs15, "000$"),
         krs15 = if_else(nchar(krs15) == 4, paste0("0", krs15), krs15))
amr_neu <- read.xlsx("./Input/Key_AMR.xlsx", sheet=1) %>% as_tibble() %>%
  mutate(krs15 = str_remove(krs15, "000$"),
         krs15 = if_else(nchar(krs15) == 4, paste0("0", krs15), krs15))

