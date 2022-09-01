# read data

round_off <- function (x, digits=0) 
{
  posneg = sign(x)
  z = trunc(abs(x) * 10 ^ (digits + 1)) / 10
  z = floor(z * posneg + 0.5) / 10 ^ digits
  return(z)
}



# Kreise: Bruttolöhne

# key sollte AGS lauten!

dat <- read.xlsx(
  "Input/wsi_aiwa_solo_selbstaendige (2020) 21 09 29.xlsx", 
  sheet = "absolut und Quoten",
  rows = 8:24, cols=1:4 ) %>%
  rename(GEN = 1, Gesamt = 4) %>%
  mutate_if(is.numeric, round_off, digits=1) %>%
  mutate(
    Gruppe = cut(Gesamt, right=F,
                 breaks=c(0, 3.8, 4.2, 4.6, 5.0, 11),
                 labels=c("bis unter 3,8%", "3,8 bis unter 4,2%",
                          "4,2 bis unter 4,6%", "4,6 bis unter 5,0%", 
                          "5,0% und mehr")),
    `Gruppe Männer` = cut(Männer, right=F,
                 breaks=c(0, 4, 4.7, 5.4, 6.1, 12),
                 labels=c("bis unter 4,0%", "4,0 bis unter 4,7%",
                          "4,7 bis unter 5,4%", "5,4 bis unter 6,1%", 
                          "6,1% und mehr")),
    `Gruppe Frauen` = cut(Frauen, right=F, 
                          breaks=c(0, 3.2, 3.5, 3.8, 4.1, 11),
                          labels=c("bis unter 3,2%", "3,2 bis unter 3,5%",
                                   "3,5 bis unter 3,8%", "3,8 bis unter 4,1%", 
                                   "4,1% und mehr")),
    
    
    
  )
