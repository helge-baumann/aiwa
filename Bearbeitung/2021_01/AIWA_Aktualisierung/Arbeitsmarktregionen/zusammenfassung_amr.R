# Zusammenfassung Arbeitsmarktregionen
# 13.09.2019

dat <- read.xlsx(
  "Helge Leiharbeiter Regression 19 09 12.xlsx",
  sheetIndex=1, 
  startRow=11, header=T, 
  colIndex=1:23, # AGS, Teenage-Motherhood-Quote
  encoding="UTF-8"
)

# Summenbildung pro Arbeitsmarktregion
dat_amr <-
  dat %>% 
  group_by(amr) %>%
  summarize_at(vars(Beschäftigte..WO.:zivile.Erwerbspersonen), sum)

# Gruppeninformationen zurückholen (geht vllt einfacher?)
dat_amr <- 
  merge(
    dat_amr, 
    unique(dat[,c("amr", "amr_name", "amr_styp3", "NA.")]), 
    by="amr", 
    all=F)

# Datei speichern (Spaltennamen sind noch verkürzt)
write.xlsx(
  dat_amr,
  file=paste0("Regressiondaten_", format(Sys.time(), "%y%m%d"), ".xlsx"),
  sheetName="AMR",
  row.names=F)

  
rm(dat)
