# read data

# Am besten einfach "dat" nennen. 

# Kreise (401)----
dat <- read.xlsx("./Input/2021-03-08_Leiharbeitnehmer_Arbeitsmarktregionen.xlsx",
                 sheet=paste0("Leiharbeitnehmer ", jahr)) %>%
  as_tibble() %>%
  select(1:6) %>%
  rename(Gesamt = 4, Männer = 5, Frauen = 6) %>%
  mutate(Gesamt=Gesamt*100, Männer=Männer*100, Frauen=Frauen*100) %>%
  mutate(gruppe = 
           cut(
             Gesamt, 
             breaks=c(-1, 1.7, 2.0, 2.3, 2.6, 101),
             labels=c("bis 1,7", "1,7 bis 2,0", "2,0 bis 2,3", "2,3 bis 2,6", "2,6 und mehr")),
         SN_AMR = amr
  )

