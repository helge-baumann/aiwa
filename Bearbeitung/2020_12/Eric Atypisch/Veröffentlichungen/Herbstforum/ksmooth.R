plot(ksmooth(dat_reg$lager, dat_reg$leih, bandwidth=1), type="l",
     xlim=c(0,10),
     )

data.frame(ksmooth(dat_reg$lager, dat_reg$leih)$x,
           ksmooth(dat_reg$lager, dat_reg$leih)$y)

           
