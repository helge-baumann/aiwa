# Kreise und AMR mergen und aggregieren

dat15 <- dat15 %>% left_join(amr_alt, by=c("AGS" = "krs15")) %>%
  mutate(`Leiharbeitnehmer Frauen` = as.numeric(`Leiharbeitnehmer Frauen`))
dat16 <- dat16 %>% left_join(amr_alt, by=c("AGS" = "krs15")) 
dat17 <- dat17 %>% left_join(amr_neu, by=c("AGS" = "krs15")) 
dat18 <- dat18 %>% left_join(amr_neu, by=c("AGS" = "krs15")) 
dat20 <- dat20 %>% left_join(amr_neu, by=c("AGS" = "krs15")) 
 
