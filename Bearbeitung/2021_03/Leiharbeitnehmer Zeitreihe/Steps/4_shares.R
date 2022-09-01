# Quoten der Leiharbeitnehmer

amr15 <- amr15 %>% 
  mutate(`Quote Gesamt` = round(`Leiharbeitnehmer Gesamt`/Gesamt, digits=3),
         `Quote Männer` = round(`Leiharbeitnehmer Männer`/Männer, digits=3),
         `Quote Frauen` = round(`Leiharbeitnehmer Frauen`/Frauen, digits=3)) %>%
  select(Arbeitsmarktregion, Typ, `Quote Gesamt`, `Quote Männer`, `Quote Frauen`,
         `Leiharbeitnehmer Gesamt`, `Leiharbeitnehmer Männer`, `Leiharbeitnehmer Frauen`)
amr16 <- amr16 %>% 
  mutate(`Quote Gesamt` = round(`Leiharbeitnehmer Gesamt`/Gesamt, digits=3),
         `Quote Männer` = round(`Leiharbeitnehmer Männer`/Männer, digits=3),
         `Quote Frauen` = round(`Leiharbeitnehmer Frauen`/Frauen, digits=3)) %>%
  select(Arbeitsmarktregion, Typ, `Quote Gesamt`, `Quote Männer`, `Quote Frauen`,
         `Leiharbeitnehmer Gesamt`, `Leiharbeitnehmer Männer`, `Leiharbeitnehmer Frauen`)
amr17 <- amr17 %>% 
  mutate(`Quote Gesamt` = round(`Leiharbeitnehmer Gesamt`/Gesamt, digits=3),
         `Quote Männer` = round(`Leiharbeitnehmer Männer`/Männer, digits=3),
         `Quote Frauen` = round(`Leiharbeitnehmer Frauen`/Frauen, digits=3)) %>%
  select(Arbeitsmarktregion, Typ, `Quote Gesamt`, `Quote Männer`, `Quote Frauen`,
         `Leiharbeitnehmer Gesamt`, `Leiharbeitnehmer Männer`, `Leiharbeitnehmer Frauen`)
amr18 <- amr18 %>% 
  mutate(`Quote Gesamt` = round(`Leiharbeitnehmer Gesamt`/Gesamt, digits=3),
         `Quote Männer` = round(`Leiharbeitnehmer Männer`/Männer, digits=3),
         `Quote Frauen` = round(`Leiharbeitnehmer Frauen`/Frauen, digits=3)) %>%
  select(Arbeitsmarktregion, Typ, `Quote Gesamt`, `Quote Männer`, `Quote Frauen`,
         `Leiharbeitnehmer Gesamt`, `Leiharbeitnehmer Männer`, `Leiharbeitnehmer Frauen`)



amr20 <- amr20 %>% 
  mutate(`Quote Gesamt` = round(`Leiharbeitnehmer Gesamt`/Gesamt, digits=3),
         `Quote Männer` = round(`Leiharbeitnehmer Männer`/Männer, digits=3),
         `Quote Frauen` = round(`Leiharbeitnehmer Frauen`/Frauen, digits=3)) %>%
  select(Arbeitsmarktregion, Typ, `Quote Gesamt`, `Quote Männer`, `Quote Frauen`,
         `Leiharbeitnehmer Gesamt`, `Leiharbeitnehmer Männer`, `Leiharbeitnehmer Frauen`)