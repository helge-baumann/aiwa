amr_quoten <- amr_summen %>%
  mutate(Quote_gesamt = Insgesamt_Leiharbeitnehmer/Insgesamt_Beschaeftigte*100,
         Quote_Maenner = Maenner_Leiharbeitnehmer/Insgesamt_Maenner*100,
         Quote_Frauen = Frauen_Leiharbeitnehmer/Insgesamt_Frauen*100) %>%
  select(Arbeitsmarktregion, Quote_gesamt, Quote_Maenner, Quote_Frauen,
         amr, amr_styp3, Label)

write.csv2(amr_quoten, 
           paste0("./Output/Daten/",
                  Sys.Date(),
                  "_",
                  "Leiharbeitnehmer_Quoten.csv"))
