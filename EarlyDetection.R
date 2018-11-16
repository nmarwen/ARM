#How early in the booking horizon can an outlier be detected?
#Fare class Y
genuineEDY <- numeric(0)
fpratesEDY <- numeric(0)
for (i in 1:11){
  dat <- Ybookingsa[,2:(i+20)]
  k <- outlierdet1(dat,3)
  fpratesEDY <- c(fpratesEDY, fpratedists(k,501))
  genuineEDY <- c(genuineEDY, trueoutdet(k,501))
}
par(mfrow = c(2,2))
d = data.frame(x = 11:1, FPRate = fpratesEDY) 
with(d, plot(x, rev(FPRate), type="l", col="black", xlab = "Booking Horizon", ylab="Number of False Positives", xlim = c(BH,0), main = "Fare Class Y", cex=1.2, cex.lab=1.2, cex.axis=1.2))
par(new = T)
with(d, plot(x, rev(FPRate), col = genuineEDY+1, axes=F, xlim = c(BH,0), pch=19, xlab=NA, ylab=NA, cex=1.2))
legend("topleft", legend=c("Outlier Not Detected", "Outlier Detected"), pch=c(19,19), col=c("black", "red"))

#Fare class M
genuineEDM <- numeric(0)
fpratesEDM <- numeric(0)
for (i in 1:BH){
  dat <- Mbookingsa[,2:(i+1)]
  k <- outlierdet1(dat,5)
  fpratesEDM <- c(fpratesEDM, fpratedists(k,501))
  genuineEDM <- c(genuineEDM, trueoutdet(k,501))
}
d = data.frame(x = 30:1, FPRate = fpratesEDM) 
with(d, plot(x, rev(FPRate), type="l", col="black", xlab = "Booking Horizon", ylab="Number of False Positives", xlim = c(BH,0), main = "Fare Class M", cex=1.2, cex.lab=1.2, cex.axis=1.2))
par(new = T)
with(d, plot(x, rev(FPRate), col = genuineEDM+1, axes=F, xlim = c(BH,0), pch=19, xlab=NA, ylab=NA, cex=1.2))
legend("topleft", legend=c("Outlier Not Detected", "Outlier Detected"), pch=c(19,19), col=c("black", "red"))

#Fare class K
genuineEDK <- numeric(0)
fpratesEDK <- numeric(0)
for (i in 1:BH){
  dat <- Kbookingsa[,2:(i+1)]
  k <- outlierdet1(dat,5)
  fpratesEDK <- c(fpratesEDK, fpratedists(k,501))
  genuineEDK <- c(genuineEDK, trueoutdet(k,501))
}
d = data.frame(x = 30:1, FPRate = fpratesEDK) 
with(d, plot(x, rev(FPRate), type="l", col="black", xlab = "Booking Horizon", ylab="Number of False Positives", xlim = c(BH,0), main = "Fare Class K", cex=1.2, cex.lab=1.2, cex.axis=1.2))
par(new = T)
with(d, plot(x, rev(FPRate), col = genuineEDK+1, axes=F, xlim = c(BH,0), pch=19, xlab=NA, ylab=NA, cex=1.2))
legend("bottomleft", legend=c("Outlier Not Detected", "Outlier Detected"), pch=c(19,19), col=c("black", "red"))

#Total Bookings
genuineEDTot <- numeric(0)
fpratesEDTot <- numeric(0)
for (i in 1:BH){
  dat <- Totbookingsa[,2:(i+1)]
  k <- outlierdet1(dat,5)
  fpratesEDTot <- c(fpratesEDTot, fpratedists(k,501))
  genuineEDTot <- c(genuineEDTot, trueoutdet(k,501))
}
d = data.frame(x = 30:1, FPRate = fpratesEDTot) 
with(d, plot(x, rev(FPRate), type="l", col="black", xlab = "Booking Horizon", ylab="Number of False Positives", xlim = c(BH,0), main = "Total Bookings", cex=1.2, cex.lab=1.2, cex.axis=1.2))
par(new = T)
with(d, plot(x, rev(FPRate), col = genuineEDTot+1, axes=F, xlim = c(BH,0), pch=19, xlab=NA, ylab=NA, cex=1.2))
legend("topleft", legend=c("Outlier Not Detected", "Outlier Detected"), pch=c(19,19), col=c("black", "red"))

