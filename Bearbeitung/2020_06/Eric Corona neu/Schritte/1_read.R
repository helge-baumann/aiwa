# read data

dat_ges <- openxlsx::read.xlsx(
  "./Daten/2_coronamai.xlsx",
  sheet=2, 
  startRow=30, colNames=F,
  cols=c(1:2, 4, 20, 21)
) %>% 
  as_tibble() %>%
  filter(str_detect(X1, "[:digit:]{5} ") %in% T) %>%
  mutate(AGS = str_extract(X1, "[:digit:]{5}")) %>%
  mutate(Kreis = str_remove(X1, "[:digit:]{5} ")) %>%
  select(AGS, Kreis, everything()) %>%
  select(-X1) 

names(dat_ges) <- c("AGS", "Kreis", "Kurzarbeiter",
                "Kurzarbeiterquote", "CoronaEffekt",
                "CoronaQuote")

