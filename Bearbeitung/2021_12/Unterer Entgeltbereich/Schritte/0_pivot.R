# Rohdaten pivotieren

style <- createStyle(textDecoration="bold")

# 1: Personenmerkmale----
wb <- createWorkbook()

dat <- list()

for(i in 1:5) {
  
  dat[[as.character(2015+i)]] <- read.xlsx(
    "Input/Bundesagentur für Arbeit 2021 Bruttoarbeitsentgelte.xlsx",
    sheet = "8.2", 
    startRow = 11, cols = c(1:3, 3+i), colNames=F
    ) %>% 
    tibble() %>%
    filter(nchar(X1) <= 5) %>%
    rename(AGS = 1, Kreis = 2, Kat = 3, Median = 4) %>%
    pivot_wider(names_from = Kat, values_from = Median) %>%
    arrange(AGS)
  
  dat[[as.character(2015+i)]] <- rbind(
    dat[[as.character(2015+i)]][dat[[as.character(2015+i)]]$AGS %in% c("D", "W"),], 
    dat[[as.character(2015+i)]][str_detect(dat[[as.character(2015+i)]]$AGS, 
                             paste(c(paste0("^0", 1:9), "^10"), collapse="|")),],
    dat[[as.character(2015+i)]][dat[[as.character(2015+i)]]$AGS %in% c("O"),], 
    dat[[as.character(2015+i)]][str_detect(dat[[as.character(2015+i)]]$AGS, 
                             paste(paste0("^1", 1:6), collapse="|")),]
    )
  
  dat[[as.character(2015+i)]][3:ncol(dat[[as.character(2015+i)]])] <- 
    sapply(
      dat[[as.character(2015+i)]][3:ncol(dat[[as.character(2015+i)]])], 
      function(x) round(as.double(x), digits=0))
  
  addWorksheet(wb, as.character(2015+i))
  writeData(wb, as.character(2015+i), dat[[as.character(2015+i)]] )
  bold <- which(nchar(dat[[as.character(2015+i)]][[1]]) <= 2)
  addStyle(wb, as.character(2015+i), style, rows=c(1,bold+1), cols=1:ncol(dat[[as.character(2015+i)]]), gridExpand=T)
  
  
}

saveWorkbook(wb, paste0("Output/", Sys.Date(), "_Medianentgelte2016-2020.xlsx"), overwrite=T)

# 2: Wirtschaftsmerkmale----
wb <- createWorkbook()
  
  dat_wz <- read.xlsx(
    "Input/Bundesagentur für Arbeit 2021 Bruttoarbeitsentgelte.xlsx",
    sheet = "8.3", 
    startRow = 11, cols = c(1:3, 11), colNames=F
  ) %>% 
    tibble() %>%
    filter(nchar(X1) <= 5) %>%
    rename(AGS = 1, Kreis = 2, Kat = 3, Median = 4) %>%
    pivot_wider(names_from = Kat, values_from = Median) %>%
    arrange(AGS)%>%
    left_join( read.xlsx(
      "Input/Bundesagentur für Arbeit 2021 Bruttoarbeitsentgelte.xlsx",
      sheet = "8.4", 
      startRow = 11, cols = c(1:3, 11), colNames=F
    ) %>% 
      tibble() %>%
      filter(nchar(X1) <= 5) %>%
      rename(AGS = 1, Kreis = 2, Kat = 3, Median = 4) %>%
      pivot_wider(names_from = Kat, values_from = Median) %>%
      arrange(AGS))
  
  dat_wz <- rbind(
    dat_wz[dat_wz$AGS %in% c("D", "W"),], 
    dat_wz[str_detect(dat_wz$AGS, paste(c(paste0("^0", 1:9), "^10"), collapse="|")),],
    dat_wz[dat_wz$AGS %in% c("O"),], 
    dat_wz[str_detect(dat_wz$AGS, paste(paste0("^1", 1:6), collapse="|")),]
  )
  
  dat_wz[3:ncol(dat_wz)] <- sapply(dat_wz[3:ncol(dat_wz)],  function(x) round(as.double(x), digits=0))
  
  addWorksheet(wb, "WZ und Tätigkeiten")
  writeData(wb, "WZ und Tätigkeiten", dat_wz )
  bold <- which(nchar(dat_wz[[1]]) <= 2)
  addStyle(wb, "WZ und Tätigkeiten", style, rows=c(1,bold+1), cols=1:ncol(dat_wz), gridExpand=T)
  
  

saveWorkbook(wb, paste0("Output/", Sys.Date(), "_Medianentgelte2020_Branchen.xlsx"), overwrite=T)

# 3: Untere Lohngruppe----
wb <- createWorkbook()

dat_ul <- read.xlsx(
  "Input/Bundesagentur für Arbeit 2021 Bruttoarbeitsentgelte.xlsx",
  sheet = "8.5", 
  startRow = 11, cols = c(1:2, 5), colNames=F
) %>% 
  tibble() %>%
  filter(nchar(X1) <= 5) %>%
  rename(AGS = 1, Kreis = 2, `Anteil in Prozent` = 3) %>%
  arrange(AGS)

dat_ul <- rbind(
  dat_ul[dat_ul$AGS %in% c("D", "W"),], 
  dat_ul[str_detect(dat_ul$AGS, paste(c(paste0("^0", 1:9), "^10"), collapse="|")),],
  dat_ul[dat_ul$AGS %in% c("O"),], 
  dat_ul[str_detect(dat_ul$AGS, paste(paste0("^1", 1:6), collapse="|")),]
)

dat_ul[3:ncol(dat_ul)] <- sapply(dat_ul[3:ncol(dat_ul)],  function(x) round(as.double(x), digits=1))

addWorksheet(wb, "Unterer Lohnbereich")
writeData(wb, "Unterer Lohnbereich", dat_ul )
bold <- which(nchar(dat_ul[[1]]) <= 2)
addStyle(wb, "Unterer Lohnbereich", style, rows=c(1,bold+1), cols=1:ncol(dat_ul), gridExpand=T)

saveWorkbook(wb, paste0("Output/", Sys.Date(), "_Medianentgelte2020_unterer Lohnbereich.xlsx"), overwrite=T)

write.csv2(
  sapply(dat_wz, function(x) sum(is.na(x))), "Output/Missings.csv"
)
