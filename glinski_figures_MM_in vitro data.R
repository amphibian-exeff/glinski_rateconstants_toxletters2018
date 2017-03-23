

##MM for atz#
atzconc <- c(0, 0.712, 3.1684, 10, 50, 75, 100, 125, 150, 200, 250)
atzrate <- c(0, 42.63, 75.22, 120.16, 208.64, 272.69, 663.02, 628.38, 569.18, 578.97, 552.27)
plot(atzconc, atzrate, las=1, pch=16) 
atzmmModel <- nls(atzrate~Vm*atzconc/(Km+atzconc), start = list(Vm=500,Km=200)) 
summary(atzmmModel)
x <- seq(min(atzconc), max(atzconc), length=100)
y <- predict(atzmmModel, list(atzconc=x))
points(x, y, type='l', col='blue')


##MM for DIA##
diaconc <- c(0, 0.712, 3.1684, 10, 50, 75, 100, 125, 150, 200, 250)
diarate <- c(0, 2.47, 8.545, 17.74, 44.69, 76.60, 113.78, 144.18, 132.46, 76.99, 70.46)
plot(diaconc, diarate, las=1, pch=16, col="blue")
diammModel <- nls(diarate~Vm*diaconc/(Km+diaconc), start = list(Vm=75,Km=20)) 
summary(diammModel)
x <- seq(min(diaconc), max(diaconc), length=100)
y <- predict(diammModel, list(diaconc=x))
points(x, y, type='l', col='blue')

##MM for dea##
deaconc <- c(0, 0.712, 3.1684, 10, 50, 75, 100, 125, 150, 200, 250)
dearate <- c(0, 5.05, 13.80, 27.22, 62.42, 85.02, 135.00, 163.14, 157.35, 40.30, 43.30)
plot(deaconc, dearate, las=1, pch=16, col='green')
deammModel <- nls(dearate~Vm*deaconc/(Km+deaconc), start = list(Vm=50,Km=20)) 
summary(deammModel)
x <- seq(min(deaconc), max(deaconc), length=100)
y <- predict(deammModel, list(deaconc=x))
points(x, y, type='l', col='green')

#figure 1
jpeg(paste(micro.graphics,"glinski_fig1.jpg", sep=""),width = 6, height = 4, units = "in",res=300)
  conc <- c(0, 0.712, 3.1684, 10, 50, 75, 100, 125, 150, 200, 250)
  diarate <- c(0, 2.47, 8.545, 17.74, 44.69, 76.60, 113.78, 144.18, 132.46, 76.99, 70.46)
  dearate <- c(0, 5.05, 13.80, 27.22, 62.42, 85.02, 135.00, 163.14, 157.35, 40.30, 43.30)
  plot(conc, atzrate, las=1, pch=16, col="red", xlab = expression(paste("Concentration (", mu, "M)")), ylab=expression(paste("V"['max']* " (pmol min "^" -1","mg" ^" -1","MSP)")))
  points(conc, diarate, pch=16, col="dark green")
  points(conc, dearate, pch=16, col="blue")
  x <- seq(min(atzconc), max(atzconc), length=100)
  y <- predict(atzmmModel, list(atzconc=x))
  points(x, y, type='l', col='red')
  x <- seq(min(diaconc), max(diaconc), length=100)
  y <- predict(diammModel, list(diaconc=x))
  points(x, y, type='l', col='dark green')
  x <- seq(min(deaconc), max(deaconc), length=100)
  y <- predict(deammModel, list(deaconc=x))
  points(x, y, type='l', col='blue')
  legend(0,690, c("ATZ", 'DIA', 'DEA'), col=c('red', 'dark green', 'blue'), pch=16, cex=.75)
dev.off()

#figure 3
##MM for FS##
jpeg(paste(micro.graphics,"glinski_fig3.jpg", sep=""),width = 6, height = 4, units = "in",res=300)
  fsconc <- c(0, 2.89, 10, 50, 75, 100, 125, 150, 200, 250)
  fsrate <- c(0, 35.73, 45.99, 69.81, 72.95, 145.85, 159.43, 145.86, 114.72, 115.80) 
  plot(fsconc, fsrate, xlab = expression(paste("Concentration (", mu, "M)")), 
       ylab=expression(paste("V"['max']* " (pmol min "^" -1","mg" ^" -1","MSP)")), 
       las=1, pch=16, col='blue')
  fsmmModel <- nls(fsrate~Vm*fsconc/(Km+fsconc), start = list(Vm=100,Km=20)) 
  summary(fsmmModel)
  x <- seq(min(fsconc), max(fsconc), length=100)
  y <- predict(fsmmModel, list(fsconc=x))
  points(x, y, type='l', col='blue')
  legend(0,166, c("F. sulfone"), col=c('blue'), pch=16, cex=0.75)
dev.off()

#dont use
##MM for Fip##
fipconc <- c(0, 2.89, 10, 50, 150, 200, 250)
fiprate <- c(0, 76.71, 160.11, 587.46, 972.34, 1184.29, 857.04)
plot(fipconc, fiprate, las=1, pch=16)
fipmmModel <- nls(fiprate~Vm*fipconc/(Km+fipconc), start = list(Vm=800,Km=200)) 
summary(fipmmModel)
x <- seq(min(fipconc), max(fipconc), length=100)
y <- predict(fipmmModel, list(fipconc=x))
points(x, y, type='l', col='blue')

#dont use
fsconc <- c(0, 2.89, 10, 50, 75, 100, 125, 150, 200, 250)
plot(fipconc, fiprate, las=1, pch=16, col='red', xlab = expression(paste("Concentration (", mu, "M)")), 
     ylab=expression(paste("V"['max']* " (pmol min "^" -1","mg" ^" -1","MSP)")))
points(fsconc, fsrate, pch=16, col='blue')
x <- seq(min(fsconc), max(fsconc), length=100)
y <- predict(fsmmModel, list(fsconc=x))
points(x, y, type='l', col='blue')
x <- seq(min(fipconc), max(fipconc), length=100)
y <- predict(fipmmModel, list(fipconc=x))
points(x, y, type='l', col='red')
legend(0,1235, c("Fip", 'F. Sulfone'), col=c('red', 'blue'), pch=16, cex=0.75)



##MM for tdn##
tdnconc <- c(0, 0.6808, 2.89, 10, 50, 75, 100, 125, 150, 200, 250)
tdnrate <- c(0, 69.02, 135.56, 457.30, 943.03, 665.71, 478.25, 598.63, 644.17, 628.45, 374.59)
plot(tdnconc, tdnrate, las=1, pch=16)
tdnmmModel <- nls(tdnrate~Vm*tdnconc/(Km+tdnconc), start = list(Vm=500,Km=200)) 
summary(tdnmmModel)
x <- seq(min(tdnconc), max(tdnconc), length=100)
y <- predict(tdnmmModel, list(tdnconc=x))
points(x, y, type='l', col='blue')

##MM for tdla##
tdlaconc <- c(0, 0.6808, 2.89, 10, 50, 75, 100, 125, 150, 200, 250)
tdlarate <- c(0, -38.39, 48.03, 55.82, 53.55, 103.72, 91.91, 96.93, 144.80, 273.16, 108.30)
plot(tdlaconc, tdlarate, las=1, pch=16)
tdlammModel <- nls(tdlarate~Vm*tdlaconc/(Km+tdlaconc), start = list(Vm=100,Km=20)) 
summary(tdlammModel)
x <- seq(min(tdlaconc), max(tdlaconc), length=100)
y <- predict(tdlammModel, list(tdlaconc=x))
points(x, y, type='l', col='blue')


##MM for tdlb##
tdlbconc <- c(0, 0.6808, 2.89, 10, 50, 75, 100, 125, 150, 200, 250)
tdlbrate <- c(0, -13.63, 47.68, 77.16, 208.95, 339.54, 410.42, 503.50, 595.36, 1708, 271.20)
plot(tdlbconc, tdlbrate, las=1, pch=16)

alt_tdlbrate <- tdlbrate
alt_tdlbrate[10] <- NA
tdlbmmModel <- nls(alt_tdlbrate~Vm*tdlbconc/(Km+tdlbconc), start = list(Vm=500,Km=20)) 
summary(tdlbmmModel)
x <- seq(min(tdlbconc), max(tdlbconc), length=100)
y <- predict(tdlbmmModel, list(tdlbconc=x))
points(x, y, type='l', col='blue')


?pch

#figure 2
jpeg(paste(micro.graphics,"glinski_fig2.jpg", sep=""),width = 6, height = 4, units = "in",res=300)
  tdnconc <- c(0, 0.6808, 2.89, 10, 50, 75, 100, 125, 150, 200, 250)
  plot(tdnconc, tdlbrate, las=1, pch=16, col='blue', xlab = expression(paste("Concentration (", mu, "M)")), 
       ylab=expression(paste("V"['max']* " (pmol min "^" -1","mg" ^" -1","MSP)")))
  points(tdnconc, tdlarate, pch=16, col='dark green')
  points(tdnconc, tdnrate, pch=16, col='red')
  x <- seq(min(tdnconc), max(tdnconc), length=100)
  y <- predict(tdlbmmModel, list(tdlbconc=x))
  points(x, y, type='l', col='blue')
  x <- seq(min(tdlaconc), max(tdlaconc), length=100)
  y <- predict(tdlammModel, list(tdlaconc=x))
  points(x, y, type='l', col='dark green')
  x <- seq(min(tdnconc), max(tdnconc), length=100)
  y <- predict(tdnmmModel, list(tdnconc=x))
  points(x, y, type='l', col='red')
  legend(0,1780, c("TDN", 'TDL A', 'TDL B'), col=c('red', 'dark green', 'blue'), pch=16, cex=0.75)
dev.off()

##ALL metabolites together################################################################
plot(tdnconc, tdlbrate, las=1, pch=18, col='orange', xlab = expression(paste("Substrate Concentration (", mu, "M)")), ylab='Velocity (pmol/min/mg MSP)')
x <- seq(min(tdnconc), max(tdnconc), length=100)
y <- predict(tdlbmmModel, list(tdlbconc=x))
points(x, y, type='l', col='orange')
points(diaconc, diarate, pch=15, col='red')
x <- seq(min(diaconc), max(diaconc), length=100)
y <- predict(diammModel, list(diaconc=x))
points(x, y, type='l', col='red')
points(deaconc, dearate, pch=16, col='blue')
x <- seq(min(deaconc), max(deaconc), length=100)
y <- predict(deammModel, list(deaconc=x))
points(tdlaconc, tdlarate, pch=17, col='dark green')
x <- seq(min(tdlaconc), max(tdlaconc), length=100)
y <- predict(tdlammModel, list(tdlaconc=x))
points(x, y, type='l', col='dark green')
points(fsconc, fsrate, pch=8, col='purple')
x <- seq(min(fsconc), max(fsconc), length=100)
y <- predict(fsmmModel, list(fsconc=x))
points(x, y, type='l', col='purple')
legend(0, 625, c('DIA', 'DEA', 'TDL A', 'TDL B', "F.Sulfone"), col=c('red', 'blue', 'dark green', 'orange', 'purple'), pch=c(15,16,17,18,8), cex=0.75)

