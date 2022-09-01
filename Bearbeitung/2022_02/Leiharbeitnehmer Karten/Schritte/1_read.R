# read data

# Am besten einfach "dat" nennen. 

# Kreise (401)----
dat15 <- read.xlsx("./Input/2021-03-08_Leiharbeitnehmer_Arbeitsmarktregionen.xlsx",
                   sheet=paste0("Leiharbeitnehmer ", 2015)) %>%
  as_tibble() %>%
  select(1:6) %>%
  rename(Gesamt = 4, Männer = 5, Frauen = 6) %>%
  mutate(`Gesamt (2015)` =Gesamt*100, `Männer (2015)`=Männer*100, `Frauen (2015)`=Frauen*100) %>%
  mutate(`gruppe 2015` = 
           cut(
             `Gesamt (2015)`, 
             breaks=c(-1, 1.7, 2.0, 2.3, 2.6, 101),
             labels=c("bis 1,7", "1,7 bis 2,0", "2,0 bis 2,3", "2,3 bis 2,6", "2,6 und mehr")),
         SN_AMR = amr
  ) %>%
  select(-Gesamt, -Männer, -Frauen)

dat16 <- read.xlsx("./Input/2021-03-08_Leiharbeitnehmer_Arbeitsmarktregionen.xlsx",
                   sheet=paste0("Leiharbeitnehmer ", 2016)) %>%
  as_tibble() %>%
  select(1:6) %>%
  rename(Gesamt = 4, Männer = 5, Frauen = 6) %>%
  mutate(`Gesamt (2016)` =Gesamt*100, `Männer (2016)`=Männer*100, `Frauen (2016)`=Frauen*100) %>%
  mutate(`gruppe 2016` = 
           cut(
             `Gesamt (2016)`, 
             breaks=c(-1, 1.7, 2.0, 2.3, 2.6, 101),
             labels=c("bis 1,7", "1,7 bis 2,0", "2,0 bis 2,3", "2,3 bis 2,6", "2,6 und mehr")),
         SN_AMR = amr
  )%>%
  select(-Gesamt, -Männer, -Frauen)

dat17 <- read.xlsx("./Input/2021-03-08_Leiharbeitnehmer_Arbeitsmarktregionen.xlsx",
                   sheet=paste0("Leiharbeitnehmer ", 2017)) %>%
  as_tibble() %>%
  select(1:6) %>%
  rename(Gesamt = 4, Männer = 5, Frauen = 6) %>%
  mutate(`Gesamt (2017)` =Gesamt*100, `Männer (2017)`=Männer*100, `Frauen (2017)`=Frauen*100) %>%
  mutate(`gruppe 2017` = 
           cut(
             `Gesamt (2017)`,  
             breaks=c(-1, 1.7, 2.0, 2.3, 2.6, 101),
             labels=c("bis 1,7", "1,7 bis 2,0", "2,0 bis 2,3", "2,3 bis 2,6", "2,6 und mehr")),
         SN_AMR = amr
  ) %>%
  select(-Gesamt, -Männer, -Frauen)

dat18 <- read.xlsx("./Input/2021-03-08_Leiharbeitnehmer_Arbeitsmarktregionen.xlsx",
                   sheet=paste0("Leiharbeitnehmer ", 2018)) %>%
  as_tibble() %>%
  select(1:6) %>%
  rename(Gesamt = 4, Männer = 5, Frauen = 6) %>%
  mutate(`Gesamt (2018)` =Gesamt*100, `Männer (2018)`=Männer*100, `Frauen (2018)`=Frauen*100) %>%
  mutate(`gruppe 2018` = 
           cut(
             `Gesamt (2018)`, 
             breaks=c(-1, 1.7, 2.0, 2.3, 2.6, 101),
             labels=c("bis 1,7", "1,7 bis 2,0", "2,0 bis 2,3", "2,3 bis 2,6", "2,6 und mehr")),
         SN_AMR = amr
  ) %>%
  select(-Gesamt, -Männer, -Frauen)


dat19 <- read.csv2("./Input/2020-05-05_Leiharbeitnehmer_Quoten.csv") %>% as_tibble() %>%
  filter(!is.na(amr)) %>%
  mutate(SN_AMR = amr) %>%
  rename(`Gesamt (2019)` = Quote_gesamt, `Männer (2019)`= Quote_Maenner, `Frauen (2019)` = Quote_Frauen) %>%
  mutate(`gruppe 2019` = 
           cut(
             `Gesamt (2019)`, 
             breaks=c(-1, 1.7, 2.0, 2.3, 2.6, 101),
             labels=c("bis 1,7", "1,7 bis 2,0", "2,0 bis 2,3", "2,3 bis 2,6", "2,6 und mehr")),
         SN_AMR = amr
  ) 

dat20 <- read.xlsx("./Input/2021-03-08_Leiharbeitnehmer_Arbeitsmarktregionen.xlsx",
                   sheet=paste0("Leiharbeitnehmer ", 2020)) %>%
  as_tibble() %>%
  select(1:6) %>%
  rename(Gesamt = 4, Männer = 5, Frauen = 6) %>%
  mutate(`Gesamt (2020)` =Gesamt*100, `Männer (2020)`=Männer*100, `Frauen (2020)`=Frauen*100) %>%
  mutate(`gruppe 2020` = 
           cut(
             `Gesamt (2020)`, 
             breaks=c(-1, 1.7, 2.0, 2.3, 2.6, 101),
             labels=c("bis 1,7", "1,7 bis 2,0", "2,0 bis 2,3", "2,3 bis 2,6", "2,6 und mehr")),
         SN_AMR = amr
  ) %>%
  select(-Gesamt, -Männer, -Frauen)

i <- 6
dat21 <- read.xlsx("./Input/Helge Befristete Minijobber Leiharbeiter 22 01 26.xlsx",
                   sheet="Leiharbeiter in %", startRow = 1, colNames = T, cols = 1:5
) %>%
  as_tibble() %>%
  rename(amr = 1, Arbeitsmarktregion = 2, !!paste0("Gesamt (20", 15+i, ")") := 3, 
         !!paste0("Männer (20", 15+i, ")") := 4, 
         !!paste0("Frauen (20", 15+i, ")") := 5) %>%
  select(amr, Arbeitsmarktregion, everything())  %>%
  mutate(`gruppe 2021` = 
           cut(
             `Gesamt (2021)`, 
             breaks=c(-1, 1.7, 2.0, 2.3, 2.6, 101),
             labels=c("bis 1,7", "1,7 bis 2,0", "2,0 bis 2,3", "2,3 bis 2,6", "2,6 und mehr")),
         SN_AMR = amr
  ) 
