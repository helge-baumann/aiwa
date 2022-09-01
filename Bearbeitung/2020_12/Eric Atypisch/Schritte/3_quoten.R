# Quoten
rm(list=c("amr_summen", "amr", "dat_leih", "amr_leih"))

# Leiharbeitnehmer: Länder und gesamt entfernen
amr_quoten <- amr_quoten %>%
  filter(amr != "")

# befristet
befr_quoten <- dat_befr %>%
  mutate(X6 = as.numeric(X6)) %>%
  mutate(Insgesamt = X2/X1*100,
         Maenner = X4/X3*100,
         Frauen = X6/X5*100) %>%
  select(AGS, Kreis, Insgesamt, Maenner, Frauen)

# mj hauptjob
mjh_quoten <- dat_mj %>%
  mutate(Insgesamt = X2/X1*100,
         Maenner = X5/X4*100,
         Frauen = X8/X7*100) %>%
  select(AGS, Kreis, Insgesamt, Maenner, Frauen)

# mj hauptjob
mjn_quoten <- dat_mj %>%
  mutate(X9 = as.numeric(X9)) %>%
  mutate(Insgesamt = X3/X1*100,
         Maenner = X6/X4*100,
         Frauen = X9/X7*100) %>%
  select(AGS, Kreis, Insgesamt, Maenner, Frauen)

