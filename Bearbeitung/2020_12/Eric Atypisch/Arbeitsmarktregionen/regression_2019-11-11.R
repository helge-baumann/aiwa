# Regression

dat_reg <- 
  dat_amr %>%
  transmute(
    leih = Leiharbeiter..WO./Beschäftigte..WO.*100,
    lager = X513.Lagerwirt...Post..Zustellung..Güterumschlag..AO./
      SvB..AO.*100,
    metallohne = 
      (X244.Metallbau.und.Schweißtechnik)/
      SvB..AO.*100,
    gb = SvB.in.Betrieben.mit.ab.200.SvB/SvB..AO.*100,
    alo = Arbeitslose/zivile.Erwerbspersonen*100
  )

reg <- lm(leih ~ ., data=dat_reg)
summary(reg)
