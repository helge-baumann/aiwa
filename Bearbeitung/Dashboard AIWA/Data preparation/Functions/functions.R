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
