# Summen Leiharbeit

amr_leih <- left_join(dat_leih, amr, by="AGS")
amr_leih[is.na(amr_leih$amr_name), "amr_name"] <- 
  amr_leih[is.na(amr_leih$amr_name), "Kreis.x"]

amr_summen <- amr_leih %>%
  mutate(amr_name = as_factor(amr_name)) %>%
  group_by(amr_name) %>%
  mutate(X6 = as.numeric(X6)) %>%
  filter(!is.na(X1)) %>%
  summarise(Insgesamt_Beschaeftigte = sum(X1),
            Insgesamt_Leiharbeitnehmer = sum(X2),
            Insgesamt_Maenner = sum(X3),
            Maenner_Leiharbeitnehmer = sum(X4),
            Insgesamt_Frauen = sum(X5),
            Frauen_Leiharbeitnehmer = sum(X6)) 

amr_summen <- 
  left_join(
    amr_summen, 
    unique(amr[,c("amr", "amr_name", "amr_styp3", "NA.")]), 
    by="amr_name", 
    all=T) %>%
  rename(Label = NA., Arbeitsmarktregion = amr_name) %>%
  replace(is.na(.), "")

amr_quoten <- amr_summen %>%
  mutate(Quote_gesamt = Insgesamt_Leiharbeitnehmer/Insgesamt_Beschaeftigte*100,
         Quote_Maenner = Maenner_Leiharbeitnehmer/Insgesamt_Maenner*100,
         Quote_Frauen = Frauen_Leiharbeitnehmer/Insgesamt_Frauen*100) %>%
  select(Arbeitsmarktregion, Quote_gesamt, Quote_Maenner, Quote_Frauen,
         amr, amr_styp3, Label)
