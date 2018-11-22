library(boot)

#function to get mean from the data
mean.fun <- function(dat, idx) mean(dat[idx], na.rm = TRUE)


#create a function which processes bookings data to get cumsum of matrix
getcumulativecounts <- function(data){
  n <- nrow(data)
  df <- t(apply(data[,2:31], 1, cumsum))
  dat <- data.frame(X=data[,1],df)
  return(dat)
}


#define a function which obtains confidence intervals by bootstrapping
bootstrapcis <- function(data){
  lci <- numeric(0)
  uci <- numeric(0)
  for (i in 1:ncol(data)){
    bootobject <- boot(data=as.numeric(data[,i]), statistic=mean.fun , R=100) 
    k <- boot.ci(bootobject, conf=0.95, type="basic") 
    lci <- c(lci, k$basic[4])
    uci <- c(uci, k$basic[5])
  }
  return(list(lci,uci))
}


##############################################################################################################################################################################################
##############################################################################################################################################################################################


#compute confidence intervals for each fare class
lciy <- bootstrapcis(getcumulativecounts(Ybookingsa)[,2:31])[[1]]
uciy <- bootstrapcis(getcumulativecounts(Ybookingsa)[,2:31])[[2]]
lcim <- bootstrapcis(getcumulativecounts(Mbookingsa)[,2:31])[[1]]
ucim <- bootstrapcis(getcumulativecounts(Mbookingsa)[,2:31])[[2]]
lcik <- bootstrapcis(getcumulativecounts(Kbookingsa)[,2:31])[[1]]
ucik <- bootstrapcis(getcumulativecounts(Kbookingsa)[,2:31])[[2]]
lcitot <- bootstrapcis(getcumulativecounts(Totbookingsa)[,2:31])[[1]]
ucitot <- bootstrapcis(getcumulativecounts(Totbookingsa)[,2:31])[[2]]
#plot results
par(mfrow = c(2,2))
#Fare Class Y
plot(BH:1,lciy, type="l", xlab="Booking Horizon", ylab = "Bookings", col = "blue", lwd = 2, xlim = c(BH,0), ylim = c(0,180), main="Fare Class Y", cex=1.2, cex.axis=1.2, cex.lab=1.2)
lines(BH:1,uciy, type = "l", col = "blue", lwd = 2, xlim = c(BH,0))
lines(BH:1,cumsum(as.numeric(simdat1a[1,2:31])), col = "red", lwd = 2, xlim = c(BH,0))
legend("topleft", legend=c("Confidence Interval", "Outlier"), lty=c(1,1), col=c("blue", "red"), lwd=2)
#Fare Class M
plot(BH:1,lcim, type="l", xlab="Booking Horizon", ylab = "Bookings", col = "blue", lwd = 2, xlim = c(BH,0), ylim = c(0,180), main="Fare Class M", cex=1.2, cex.axis=1.2, cex.lab=1.2)
lines(BH:1,ucim, type = "l", col = "blue", lwd = 2, xlim = c(BH,0))
lines(BH:1,cumsum(as.numeric(simdat1a[2,2:31])), col = "red", lwd = 2, xlim = c(BH,0))
legend("topleft", legend=c("Confidence Interval", "Outlier"), lty=c(1,1), col=c("blue", "red"), lwd=2)
#Fare Class K
plot(BH:1,lcik, type="l", xlab="Booking Horizon", ylab = "Bookings", col = "blue", lwd = 2, xlim = c(BH,0), ylim = c(0,180), main="Fare Class K", cex=1.2, cex.axis=1.2, cex.lab=1.2)
lines(BH:1,ucik, type = "l", col = "blue", lwd = 2, xlim = c(BH,0))
lines(BH:1,cumsum(as.numeric(simdat1a[3,2:31])), col = "red", lwd = 2, xlim = c(BH,0))
legend("topleft", legend=c("Confidence Interval", "Outlier"), lty=c(1,1), col=c("blue", "red"), lwd=2)
#Total Bookings
plot(BH:1,lcitot, type="l", xlab="Booking Horizon", ylab = "Bookings", col = "blue", lwd = 2, xlim = c(BH,0), ylim = c(0,180), main="Total Bookings", cex=1.2, cex.axis=1.2, cex.lab=1.2)
lines(BH:1,ucitot, type = "l", col = "blue", lwd = 2, xlim = c(BH,0))
lines(BH:1,cumsum(as.numeric(simdat1a[1,2:31]+simdat1a[2,2:31]+simdat1a[3,2:31])), col = "red", lwd = 2, xlim = c(BH,0))
legend("topleft", legend=c("Confidence Interval", "Outlier"), lty=c(1,1), col=c("blue", "red"), lwd=2)


##############################################################################################################################################################################################
##############################################################################################################################################################################################


#define a function which check if a time series lies within the confidence interval
#returns vector of length BH with 1 if outlier at time t, 0 otherwise
bootout1 <- function(data,lci,uci){
  n <- length(data)
  outs <- rep(1,n)
  for (i in 1:n){
    if ((data[i] >= lci) & (data[i] <= uci)){
      outs[i] <- 0
    }
  }
  return(outs)
}
bootout1(Ybookingsa[501,2:31],lciy,uciy)


#define a function which runs through all time series and returns which ones are outside the CI at any point
bootout2 <- function(data,lci,uci){
  N <- nrow(data)
  outs <- numeric(0)
  for (i in 1:N){
    if (1 %in% bootout1(data[i,],lci,uci) == TRUE){
      outs <- c(outs,i)
    }
  }
  return(outs)
}
bootout2(Ybookingsa[,2:31],lciy,uciy)
