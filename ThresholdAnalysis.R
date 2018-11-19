#Threshold Analysis
#Euclidean Distance

#create a function which determines the outliers based on a specified threshold
#where input is the pre-calculated distances rather than raw count data
eucdistoutlierdetthresh <- function(dists,threshold){
  return(which(dists >= threshold))
}

#Look at FP rate and detection rate
th <- seq(2000,10000,50)
fpth1 <- numeric(length=length(th))
genuineth1 <- numeric(length=length(th))
dists1 <- eucdistsums(Ybookingsa)
deft1 <- 0.5*(max(dists1)-min(dists1))
for (i in 1:(length(th))){
  k <- eucdistoutlierdetthresh(dists1,th[i]) 
  fpth1[i] <- fpratedists(k, 501)
  genuineth1[i] <- trueoutdet(k,501)
}
fpth2 <- numeric(length=length(th))
genuineth2 <- numeric(length=length(th))
dists2 <- eucdistsums(Mbookingsa)
deft2 <- 0.5*(max(dists2)-min(dists2))
for (i in 1:(length(th))){
  k <- eucdistoutlierdetthresh(dists2,th[i]) 
  fpth2[i] <- fpratedists(k, 501)
  genuineth2[i] <- trueoutdet(k,501)
}
fpth3 <- numeric(length=length(th))
genuineth3 <- numeric(length=length(th))
dists3 <- eucdistsums(Kbookingsa)
deft3 <- 0.5*(max(dists3)-min(dists3))
for (i in 1:(length(th))){
  k <- eucdistoutlierdetthresh(dists3,th[i]) 
  fpth3[i] <- fpratedists(k, 501)
  genuineth3[i] <- trueoutdet(k,501)
}
fpth4 <- numeric(length=length(th))
genuineth4 <- numeric(length=length(th))
dists4 <- eucdistsums(Totbookingsa)
deft4 <- 0.5*(max(dists4)-min(dists4))
for (i in 1:(length(th))){
  k <- eucdistoutlierdetthresh(dists4,th[i]) 
  fpth4[i] <- fpratedists(k, 501)
  genuineth4[i] <- trueoutdet(k,501)
}
d = data.frame(x = th, FPRate = fpth1) 
par(mar = c(5,5,2,5))
with(d, plot(x, FPRate, pch=19, col=genuineth1+1, xlab = "Threshold", ylab="Number of False Positives"))
par(new = T)
with(d, plot(x, FPRate, type="l", col = "red", axes=F, xlab=NA, ylab=NA, cex=1.2))
t1 <- thresholdfunc(dists1)
abline(v=deft1)
text(3500,50,"Default Threshold")
legend("topright", legend=c("Outlier Not Detected", "Outlier Detected"), pch=c(19,19), col=c(1, 2))


####################################################################################################################################################################
####################################################################################################################################################################


#K-Means (single outlier of type 1 increase)
#algorithm which allows a chnage of threshold as an input
outlierdet1t <- function(data,k,t){
  if (length(unique(data)) <= k){
    return(NA)
  }
  dists <- sqrt(rowSums(data - fitted(kmeans(data, centers=k))) ^ 2)
  output <- as.vector(which(dists >= t))
  return(list(output,dists))
}

#test different thresholds and store results
th <- seq(0,50,0.5)
fpth1 <- numeric(length=length(th))
genuineth1 <- numeric(length=length(th))
for (i in 1:(length(th))){
  k <- outlierdet1t(Totbookingsa[,2:31],5,th[i]) 
  fpth1[i] <- fpratedists(k[[1]], 501)
  genuineth1[i] <- trueoutdet(k[[1]],501)
  deft1 <- 0.5*(max(k[[2]]) - min(k[[2]]))
}
fpth2 <- numeric(length=length(th))
genuineth2 <- numeric(length=length(th))
for (i in 1:(length(th))){
  k <- outlierdet1t(Totbookingsa[,2:31],5,th[i]) 
  fpth2[i] <- fpratedists(k[[1]], 501)
  genuineth2[i] <- trueoutdet(k[[1]],501)
  deft2 <- 0.5*(max(k[[2]]) - min(k[[2]]))
}
fpth3 <- numeric(length=length(th))
genuineth3 <- numeric(length=length(th))
for (i in 1:(length(th))){
  k <- outlierdet1t(Totbookingsa[,2:31],5,th[i]) 
  fpth3[i] <- fpratedists(k[[1]], 501)
  genuineth3[i] <- trueoutdet(k[[1]],501)
  deft3 <- 0.5*(max(k[[2]]) - min(k[[2]]))
}
fpth4 <- numeric(length=length(th))
genuineth4 <- numeric(length=length(th))
for (i in 1:(length(th))){
  k <- outlierdet1t(Totbookingsa[,2:31],5,th[i])
  fpth4[i] <- fpratedists(k[[1]], 501)
  genuineth4[i] <- trueoutdet(k[[1]],501)
  deft4 <- 0.5*(max(k[[2]]) - min(k[[2]]))
}
par(mfrow = c(2,2))
d = data.frame(x = th, FPRate = fpth4) 
par(mar = c(5,5,2,5))
with(d, plot(x, FPRate, pch=19, col=genuineth4+1, xlab = "Threshold", ylab="Number of False Positives", main = "Total Bookings", cex=1.2, cex.lab=1.2, cex.axis=1.2))
par(new = T)
with(d, plot(x, FPRate, type="l", col = "red", axes=F, xlab=NA, ylab=NA, cex=1.2))
abline(v=deft4)
text(35,200,"Default Threshold", cex=1.2)
legend("topright", legend=c("Outlier Not Detected", "Outlier Detected"), pch=c(19,19), col=c(1, 2))