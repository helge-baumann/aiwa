# Vereinfachte gewichtete Häufigkeitsauszählung mit dplyr (univariat)
dplyr_relfreq <- function(.data, x, gew, round=T) {
  
  erg <- .data %>%
    filter(!is.na({{x}})) %>%
    mutate(x = as_factor({{x}})) %>%
    group_by(x) %>%
    summarise(anteil = sum({{gew}}), n=n()) %>%
    mutate(anteil=(anteil/sum(anteil)*100), n=sum(n)) 
  
  if(round) erg <- erg %>% mutate(anteil=round(anteil))
  
  return(erg)
}

# Vereinfachte bivariate Häufigkeitsauszählung (bivariat)
dplyr_relfreq2 <- function(.data, x, y, gew, round=T) {
  
  erg <- .data %>%
    filter(!is.na({{x}}) & !is.na({{y}})) %>%
    mutate(x = as_factor({{x}}), y = as_factor({{y}})) %>%
    group_by(x, y) %>%
    summarise(anteil = sum({{gew}}), n=n()) %>%
    mutate(anteil=(anteil/sum(anteil)*100), n=sum(n))
  
  if(round) erg <- erg %>% mutate(anteil=round(anteil))
  
  return(erg)
  
}

leiharbeit_extract <- function(x, sheet=NULL, cols=NULL, startRow = NULL) {
  
  read.xlsx(
    x,
    sheet=sheet, cols=cols, startRow=startRow, colNames=F) %>%
    rename("Kreis" = 1, 
           "Gesamt" = 2, "Leiharbeitnehmer Gesamt" = 3,
           "Männer" = 4, "Leiharbeitnehmer Männer" = 5, 
           "Frauen" = 6, "Leiharbeitnehmer Frauen" = 7) %>%
    mutate(AGS = str_extract(Kreis, "[:digit:]{5}"), 
           Kreis = str_remove(Kreis, "[:digit:]{5} ")) %>%
    select(AGS, Kreis, everything()) %>%
    filter(!is.na(Gesamt)) %>%
    as_tibble()
  
}

leiharbeit_aggregate <- function(x) {
  
  x %>% 
    mutate(Arbeitsmarktregion = amr_name, Typ = amr_styp_name) %>%
    group_by(amr, Arbeitsmarktregion, Typ) %>%
    summarise(
      Gesamt = sum(Gesamt), 
      `Leiharbeitnehmer Gesamt` = sum(`Leiharbeitnehmer Gesamt`),
      Männer = sum(Männer), 
      `Leiharbeitnehmer Männer` = sum(`Leiharbeitnehmer Männer`),
      Frauen = sum(Frauen),
      `Leiharbeitnehmer Frauen` = sum(`Leiharbeitnehmer Frauen`)
    ) %>%
    select(Arbeitsmarktregion, Typ, everything())
  
}