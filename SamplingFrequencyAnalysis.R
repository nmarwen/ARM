#Investigate the effect of sampling frequency on run time, 
#detection rates for a single outlier and false positive rates
#for Fare Class Y
times1 <- numeric(0)
fprates1 <- numeric(0)
genuine1 <- numeric(0)
for (i in 1:10){
  dat <- Ybookings[,seq(1, ncol(Ybookings), i)]
  times1 <- c(times1, timerfunc(eucdistsums,dat))
  fprates1 <- c(fprates1, fprate(dat,501))
  genuine1 <- c(genuine1, trueoutdet(eucdistoutlierdet(dat),501))
}
#Plot sampling frequency, times and false positive rates
d = data.frame(x = 1:10, Times = times1, FPRate = fprates1) 
par(mar = c(5,5,2,5))
with(d, plot(x, Times, type="l", col="black", xlab = "Sampling Frequency", ylab="Time to Compute Distances (s)"))
par(new = T)
with(d, plot(x, FPRate, type="l", col = "red", axes=F, xlab=NA, ylab=NA, cex=1.2))
par(new = T)
with(d, plot(x, FPRate, col = genuine1+1, axes=F, pch=19, xlab=NA, ylab=NA, cex=1.2))
axis(side = 4)
mtext(side = 4, line = 3, 'Number of False Positives')
legend("top", legend=c("Time (s)", "False Positives", "Outlier Detected", "Outlier Not Detected"), lty=c(1,1,NA,NA), pch=c(NA,NA,19,19), col=c("black", "red", 1, 2))


#Repeat for fare class M, K and Totals
#Fare Class M
times2 <- numeric(0)
fprates2 <- numeric(0)
genuine2 <- numeric(0)
for (i in 1:10){
  dat <- Mbookings[,seq(1, ncol(Mbookings), i)]
  times2 <- c(times2, timerfunc(eucdistsums,dat))
  fprates2 <- c(fprates2, fprate(dat,501))
  genuine2 <- c(genuine2, trueoutdet(eucdistoutlierdet(dat),501))
}
times2[1] <- times2[1]*60
d = data.frame(x = 1:10, Times = times2, FPRate = fprates2) 
par(mar = c(5,5,2,5))
with(d, plot(x, Times, type="l", col="black", xlab = "Sampling Frequency", ylab="Time to Compute Distances (s)"))
par(new = T)
with(d, plot(x, FPRate, type="l", col = "red", axes=F, xlab=NA, ylab=NA, cex=1.2))
par(new = T)
with(d, plot(x, FPRate, col = genuine2+1, axes=F, pch=19, xlab=NA, ylab=NA, cex=1.2))
axis(side = 4)
mtext(side = 4, line = 3, 'Number of False Positives')
legend("bottomleft", legend=c("Time (s)", "False Positives", "Outlier Detected", "Outlier Not Detected"), lty=c(1,1,NA,NA), pch=c(NA,NA,19,19), col=c("black", "red", 1, 2))

#Fare Class K
times3 <- numeric(0)
fprates3 <- numeric(0)
genuine3 <- numeric(0)
for (i in 1:10){
  dat <- Kbookings[,seq(1, ncol(Kbookings), i)]
  times3 <- c(times3, timerfunc(eucdistsums,dat))
  fprates3 <- c(fprates3, fprate(dat,501))
  genuine3 <- c(genuine3, trueoutdet(eucdistoutlierdet(dat),501))
}
times3[1] <- times3[1]*60
d = data.frame(x = 1:10, Times = times3, FPRate = fprates3) 
par(mar = c(5,5,2,5))
with(d, plot(x, Times, type="l", col="black", xlab = "Sampling Frequency", ylab="Time to Compute Distances (s)"))
par(new = T)
with(d, plot(x, FPRate, type="l", col = "red", axes=F, xlab=NA, ylab=NA, cex=1.2))
par(new = T)
with(d, plot(x, FPRate, col = genuine3+1, axes=F, pch=19, xlab=NA, ylab=NA, cex=1.2))
axis(side = 4)
mtext(side = 4, line = 3, 'Number of False Positives')
legend("top", legend=c("Time (s)", "False Positives", "Outlier Detected", "Outlier Not Detected"), lty=c(1,1,NA,NA), pch=c(NA,NA,19,19), col=c("black", "red", 1, 2))

#Total Bookings
times4 <- numeric(0)
fprates4 <- numeric(0)
genuine4 <- numeric(0)
for (i in 1:10){
  dat <- Totbookings[,seq(1, ncol(Totbookings), i)]
  times4 <- c(times4, timerfunc(eucdistsums,dat))
  fprates4 <- c(fprates4, fprate(dat,501))
  genuine4 <- c(genuine4, trueoutdet(eucdistoutlierdet(dat),501))
}
times4[1] <- times4[1]*60
d = data.frame(x = 1:10, Times = times4, FPRate = fprates4) 
par(mar = c(5,5,2,5))
with(d, plot(x, Times, type="l", col="black", xlab = "Sampling Frequency", ylab="Time to Compute Distances (s)"))
par(new = T)
with(d, plot(x, FPRate, type="l", col = "red", axes=F, xlab=NA, ylab=NA, cex=1.2))
par(new = T)
with(d, plot(x, FPRate, col = genuine4+1, axes=F, pch=19, xlab=NA, ylab=NA, cex=1.2))
axis(side = 4)
mtext(side = 4, line = 3, 'Number of False Positives')
legend("bottomleft", legend=c("Time (s)", "False Positives", "Outlier Detected", "Outlier Not Detected"), lty=c(1,1,NA,NA), pch=c(NA,NA,19,19), col=c("black", "red", 1, 2))

