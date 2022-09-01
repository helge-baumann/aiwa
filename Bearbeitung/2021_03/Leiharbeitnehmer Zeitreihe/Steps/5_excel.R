# save to Excel

wb <- createWorkbook()

addWorksheet(wb, sheetName="Leiharbeitnehmer 2015")
writeDataTable(wb, "Leiharbeitnehmer 2015", amr15)

addWorksheet(wb, sheetName="Leiharbeitnehmer 2016")
writeDataTable(wb, "Leiharbeitnehmer 2016", amr16)

addWorksheet(wb, sheetName="Leiharbeitnehmer 2017")
writeDataTable(wb, "Leiharbeitnehmer 2017", amr17)

addWorksheet(wb, sheetName="Leiharbeitnehmer 2018")
writeDataTable(wb, "Leiharbeitnehmer 2018", amr18)

addWorksheet(wb, sheetName="Leiharbeitnehmer 2020")
writeDataTable(wb, "Leiharbeitnehmer 2020", amr20)

saveWorkbook(wb, 
             paste0("./Output/", Sys.Date(), 
                    "_Leiharbeitnehmer_Arbeitsmarktregionen.xlsx"),
             overwrite=T)