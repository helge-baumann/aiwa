# read data

# Kreise: Bruttolöhne

# key sollte AGS lauten!

i <- 1

dat_gesamt <-
  dat[[as.character(2015+i)]] %>%
  select(AGS, Kreis, Insgesamt, Männer, Frauen) %>%
  rename(!!paste0("Gesamt (20", 15+i, ")") := 3, 
         !!paste0("Männer (20", 15+i, ")") := 4, 
         !!paste0("Frauen (20", 15+i, ")") := 5) %>%
  filter(str_detect(AGS, "[:digit:]{5}") %in% T) %>%
  select(AGS, Kreis, everything()) %>%
  mutate(!!paste0("Gruppe 20", 15+i) := cut(get(paste0("Gesamt (20", 15+i, ")")),
                                            breaks=c(-1, 2750, 3000, 3250, 3500, 100000),
                                            labels=c("bis 2750", "2751 bis 3000",
                                                     "3001 bis 3250", "3251 bis 3500",
                                                     "3501 und mehr"))) 

for(i in 2:5) {
  
  dat_neu <-
    dat[[as.character(2015+i)]] %>%
    select(AGS, Kreis, Insgesamt, Männer, Frauen) %>%
    rename(!!paste0("Gesamt (20", 15+i, ")") := 3, 
           !!paste0("Männer (20", 15+i, ")") := 4, 
           !!paste0("Frauen (20", 15+i, ")") := 5) %>%
    filter(str_detect(AGS, "[:digit:]{5}") %in% T) %>%
    select(AGS, everything(), -Kreis) %>%
    mutate(!!paste0("Gruppe 20", 15+i) := cut(get(paste0("Gesamt (20", 15+i, ")")),
                                              breaks=c(-1, 2750, 3000, 3250, 3500, 100000),
                                              labels=c("bis 2750", "2751 bis 3000",
                                                       "3001 bis 3250", "3251 bis 3500",
                                                       "3501 und mehr"))) 
  
  dat_gesamt <- left_join(dat_gesamt, dat_neu, by="AGS")
  
}

rm(dat_neu)

# Kreise untere Lohngruppe
dat_ul <- dat_ul %>%
  mutate(`Gruppe 2020` =  cut(`Anteil in Prozent`,
                                            breaks=c(-1, 15, 20, 25, 30, 100000),
                                            labels=c("bis 15", "15,1 bis 20",
                                                     "20,1 bis 25", "25,1 bis 30",
                                                     "30,1 und mehr"))) %>%
  filter(str_detect(AGS, "[:digit:]{5}") %in% T)
  

