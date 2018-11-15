#Sampling Frequency Analysis

#############################################################################################################################################################################
#############################################################################################################################################################################

#Euclidean Distance Outlier Detection
times1 <- numeric(0)
fprates1 <- numeric(0)
genuine1 <- numeric(0)
sampfreqs <- c(1,2,3,5,6,10)
for (i in 1:6){
  f <- sampfreqs[i]
  df <- Ybookingsa[,2:31]
  dat <- sapply(seq(1,29,by=f),function(i) rowSums(df[,i:(i+1)]))
  times1 <- c(times1, timerfunc(eucdistoutlierdet(dat)))
  k <- eucdistoutlierdet(dat)
  fprates1 <- c(fprates1, fpratedists(k,501))
  genuine1 <- c(genuine1, trueoutdet(k,501))
}
#Plot sampling frequency, times and false positive rates
par(mfrow = c(2,2))
d = data.frame(x = sampfreqs, Times = times4, FPRate = fprates4) 
par(mar = c(5,5,2,5))
with(d, plot(x, Times, type="l", col="black", xlab = "Sampling Frequency", ylab="Time to Compute Distances (s)", main = "Total Bookings", cex=1.2, cex.lab=1.2, cex.axis=1.2))
par(new = T)
with(d, plot(x, FPRate, type="l", col = "red", axes=F, xlab=NA, ylab=NA, cex=1.2))
par(new = T)
with(d, plot(x, FPRate, col = genuine4+1, axes=F, pch=19, xlab=NA, ylab=NA, cex=1.2))
axis(side = 4, cex.axis = 1.2)
mtext(side = 4, line = 3, 'Number of False Positives', cex = 1.2)
legend("topright", legend=c("Time (s)", "False Positives", "Outlier Not Detected", "Outlier Detected"), lty=c(1,1,NA,NA), pch=c(NA,NA,19,19), col=c("black", "red", 1, 2))
#Repeat for other fare classes
#Fare Class M
times2 <- numeric(0)
fprates2 <- numeric(0)
genuine2 <- numeric(0)
for (i in 1:6){
  f <- sampfreqs[i]
  df <- Mbookingsa[,2:31]
  dat <- sapply(seq(1,29,by=f),function(i) rowSums(df[,i:(i+1)]))
  times2 <- c(times2, timerfunc(eucdistoutlierdet(dat)))
  k <- eucdistoutlierdet(dat)
  fprates2 <- c(fprates2, fpratedists(k,501))
  genuine2 <- c(genuine2, trueoutdet(k,501))
}
#Fare Class K
times3 <- numeric(0)
fprates3 <- numeric(0)
genuine3 <- numeric(0)
for (i in 1:6){
  f <- sampfreqs[i]
  df <- Kbookingsa[,2:31]
  dat <- sapply(seq(1,29,by=f),function(i) rowSums(df[,i:(i+1)]))
  times3 <- c(times3, timerfunc(eucdistoutlierdet(dat)))
  k <- eucdistoutlierdet(dat)
  fprates3 <- c(fprates3, fpratedists(k,501))
  genuine3 <- c(genuine3, trueoutdet(k,501))
}
#Total Bookings
times4 <- numeric(0)
fprates4 <- numeric(0)
genuine4 <- numeric(0)
for (i in 1:6){
  f <- sampfreqs[i]
  df <- Totbookingsa[,2:31]
  dat <- sapply(seq(1,29,by=f),function(i) rowSums(df[,i:(i+1)]))
  times4 <- c(times4, timerfunc(eucdistoutlierdet(dat)))
  k <- eucdistoutlierdet(dat)
  fprates4 <- c(fprates4, fpratedists(k,501))
  genuine4 <- c(genuine4, trueoutdet(k,501))
}


#############################################################################################################################################################################
#############################################################################################################################################################################


#K-Means Clustering
times1 <- numeric(0)
fprates1 <- numeric(0)
genuine1 <- numeric(0)
sampfreqs <- c(1,2,3,5,6,10)
for (i in 1:6){
  f <- sampfreqs[i]
  df <- Ybookingsa[,2:31]
  dat <- sapply(seq(1,29,by=f),function(i) rowSums(df[,i:(i+1)]))
  times1 <- c(times1, timerfunc(outlierdet1(dat,5)))
  k <- outlierdet1(dat,5)
  fprates1 <- c(fprates1, fpratedists(k,501))
  genuine1 <- c(genuine1, trueoutdet(k,501))
}
#Plot sampling frequency, times and false positive rates
par(mfrow = c(2,2))
d = data.frame(x = sampfreqs, Times = times4, FPRate = fprates4) 
par(mar = c(5,5,2,5))
with(d, plot(x, Times, type="l", col="black", xlab = "Sampling Frequency", ylab="Time to Compute Distances (s)", main = "Total Bookings", cex=1.2, cex.lab=1.2, cex.axis=1.2))
par(new = T)
with(d, plot(x, FPRate, type="l", col = "red", axes=F, xlab=NA, ylab=NA, cex=1.2))
par(new = T)
with(d, plot(x, FPRate, col = genuine4+1, axes=F, pch=19, xlab=NA, ylab=NA, cex=1.2))
axis(side = 4, cex.axis = 1.2)
mtext(side = 4, line = 3, 'Number of False Positives', cex = 1.2)
legend("topleft", legend=c("Time (s)", "False Positives", "Outlier Not Detected", "Outlier Detected"), lty=c(1,1,NA,NA), pch=c(NA,NA,19,19), col=c("black", "red", 1, 2))
#Repeat for other fare classes
#Fare Class M
times2 <- numeric(0)
fprates2 <- numeric(0)
genuine2 <- numeric(0)
for (i in 1:6){
  f <- sampfreqs[i]
  df <- Mbookingsa[,2:31]
  dat <- sapply(seq(1,29,by=f),function(i) rowSums(df[,i:(i+1)]))
  times2 <- c(times2, timerfunc(outlierdet1(dat,5)))
  k <- outlierdet1(dat,5)
  fprates2 <- c(fprates2, fpratedists(k,501))
  genuine2 <- c(genuine2, trueoutdet(k,501))
}
#Fare Class K
times3 <- numeric(0)
fprates3 <- numeric(0)
genuine3 <- numeric(0)
for (i in 1:6){
  f <- sampfreqs[i]
  df <- Kbookingsa[,2:31]
  dat <- sapply(seq(1,29,by=f),function(i) rowSums(df[,i:(i+1)]))
  times3 <- c(times3, timerfunc(outlierdet1(dat,5)))
  k <- outlierdet1(dat,5)
  fprates3 <- c(fprates3, fpratedists(k,501))
  genuine3 <- c(genuine3, trueoutdet(k,501))
}
#Total Bookings
times4 <- numeric(0)
fprates4 <- numeric(0)
genuine4 <- numeric(0)
for (i in 1:6){
  f <- sampfreqs[i]
  df <- Totbookingsa[,2:31]
  dat <- sapply(seq(1,29,by=f),function(i) rowSums(df[,i:(i+1)]))
  times4 <- c(times4, timerfunc(outlierdet1(dat,5)))
  k <- outlierdet1(dat,5)
  fprates4 <- c(fprates4, fpratedists(k,501))
  genuine4 <- c(genuine4, trueoutdet(k,501))
}
