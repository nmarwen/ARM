#K-Means Clustering Outlier Detection


#Choose the number of clusters for k-means clustering
#choose k = 5 (Y), k = 4 (M), k = 4 (K), k = 5 (Tot)
set.seed(12345)
k.max <- 6
wss <- sapply(1:k.max,function(k){kmeans(Ybookingsa[,2:31], k, nstart=50,iter.max = 15 )$tot.withinss})
par(mfrow = c(1,2))
plot(1:k.max, wss, type="b", pch = 19, frame = FALSE, xlab="Number of clusters K", ylab="Total Within-Clusters Sum of Squares", cex=1.2, cex.lab=1.2, cex.axis=1.2)
par(mfrow = c(1,1))


##############################################################################################################################################################################################
##############################################################################################################################################################################################


#Apply k-Means Clustering to whole data set where each time series is a point in a 30-dimensional space
#Consider a single outlier of type 1 only increase
#Use the default threshold described by Deb and Day
outlierdet1 <- function(data,k){
  if (length(unique(data)) <= k){
    return(NA)
  }
  dists <- sqrt(rowSums(data - fitted(kmeans(data, centers=k))) ^ 2)
  threshold <- (max(dists)+min(dists))/2
  output <- as.vector(which(dists >= threshold))
  return(output)
}
outlierdet1(Ybookingsa[,2:31],5)
outlierdet1(Mbookingsa[,2:31],4) 
outlierdet1(Kbookingsa[,2:31],5) 
outlierdet1(Totbookingsa[,2:31],4) 


##############################################################################################################################################################################################
##############################################################################################################################################################################################


#Multiple outliers from the same distribution
#Plot FP and Detection rates against number of outliers
multifpratesY <- numeric(0)
multigenuineY <- numeric(0)
multifpratesM <- numeric(0)
multigenuineM <- numeric(0)
multifpratesK <- numeric(0)
multigenuineK <- numeric(0)
multifpratesTot <- numeric(0)
multigenuineTot <- numeric(0)
for (i in 1:25){
  nplfsim(n=i,BH=30,mu=180,L=0.01,C=200,SLFs=c(1,0.45,0.25),segments1=c(10,10,10),rates1=c(40,50,80),segments2=c(15,10,5),rates2=c(15,50,60),p1=0.85,p2=0.65,s=12345,name="multi")
  multi <- read.csv("multi.csv", header=TRUE, stringsAsFactors = FALSE)
  Ybookingsm <- rbind(Ybookings,multi[seq(1, nrow(multi), 3),])
  Mbookingsm <- rbind(Mbookings,multi[seq(2, nrow(multi), 3),])
  Kbookingsm <- rbind(Kbookings,multi[seq(3, nrow(multi), 3),])
  Totbookingsm <-  Ybookingsm[,2:31] + Mbookingsm[,2:31] + Kbookingsm[,2:31] 
  lab <- rep("Tot",nrow(Totbookingsm))
  Totbookingsm <- cbind(lab, Totbookingsm)
  ky <- outlierdet1(Ybookingsm[,2:31],5)
  km <- outlierdet1(Mbookingsm[,2:31],5)
  kk <- outlierdet1(Kbookingsm[,2:31],5)
  ktot <- outlierdet1(Totbookingsm[,2:31],5)
  gens <- (501):(500+i)
  multifpratesY <- c(multifpratesY, multifprate(ky, gens, (500+i)))
  multigenuineY <- c(multigenuineY, multidetrate(ky, gens))
  multifpratesM <- c(multifpratesM, multifprate(km, gens, (500+i)))
  multigenuineM <- c(multigenuineM, multidetrate(km, gens))
  multifpratesK <- c(multifpratesK, multifprate(kk, gens, (500+i)))
  multigenuineK <- c(multigenuineK, multidetrate(kk, gens))
  multifpratesTot <- c(multifpratesTot, multifprate(ktot, gens, (500+i)))
  multigenuineTot <- c(multigenuineTot, multidetrate(ktot, gens))
}
par(mfrow = c(2,2))
d = data.frame(x = 1:25, GenRate = multigenuineTot, FPRate = multifpratesTot) 
par(mar = c(5,5,2,5))
with(d, plot(x, GenRate, type="l", col="black", xlab = "Number of Outliers", ylab="% of Genuine Outliers Detected", main = "Total Bookings", cex.lab=1.2, cex.axis = 1.2, cex=1.2))
par(new = T)
with(d, plot(x, FPRate, type="l", col = "red", axes=F, xlab=NA, ylab=NA, cex=1.2))
axis(side = 4, cex.axis = 1.2)
mtext(side = 4, line = 3, '% of Observations False Positives')
legend("right", legend=c("Detection Rate", "False Positive Rate"), lty=c(1,1), col=c("black", "red", 1, 2))


##############################################################################################################################################################################################
##############################################################################################################################################################################################


#Multiple outliers from different distributions
multifpratesY <- numeric(0)
multigenuineY <- numeric(0)
multifpratesM <- numeric(0)
multigenuineM <- numeric(0)
multifpratesK <- numeric(0)
multigenuineK <- numeric(0)
multifpratesTot <- numeric(0)
multigenuineTot <- numeric(0)
for (i in 1:25){
  Ybookingsm <- Ybookingsd[1:(500+i),]
  Mbookingsm <- Mbookingsd[1:(500+i),]
  Kbookingsm <- Kbookingsd[1:(500+i),]
  Totbookingsm <-  Totbookingsd[1:(500+i),]
  ky <- outlierdet1(Ybookingsm[,2:31],5)
  km <- outlierdet1(Mbookingsm[,2:31],5)
  kk <- outlierdet1(Kbookingsm[,2:31],5)
  ktot <- outlierdet1(Totbookingsm[,2:31],5)
  gens <- (501):(500+i)
  multifpratesY <- c(multifpratesY, multifprate(ky, gens, (500+i)))
  multigenuineY <- c(multigenuineY, multidetrate(ky, gens))
  multifpratesM <- c(multifpratesM, multifprate(km, gens, (500+i)))
  multigenuineM <- c(multigenuineM, multidetrate(km, gens))
  multifpratesK <- c(multifpratesK, multifprate(kk, gens, (500+i)))
  multigenuineK <- c(multigenuineK, multidetrate(kk, gens))
  multifpratesTot <- c(multifpratesTot, multifprate(ktot, gens, (500+i)))
  multigenuineTot <- c(multigenuineTot, multidetrate(ktot, gens))
}
par(mfrow = c(2,2))
d = data.frame(x = 1:25, GenRate = multigenuineTot, FPRate = multifpratesTot) 
par(mar = c(5,5,2,5))
with(d, plot(x, GenRate, type="l", col="black", xlab = "Number of Outliers", ylab="% of Genuine Outliers Detected", main = "Total Bookings", cex.lab=1.2, cex.axis = 1.2, cex=1.2))
par(new = T)
with(d, plot(x, FPRate, type="l", col = "red", axes=F, xlab=NA, ylab=NA, cex=1.2))
axis(side = 4, cex.axis = 1.2)
mtext(side = 4, line = 3, '% of Observations False Positives')
legend("left", legend=c("Detection Rate", "False Positive Rate"), lty=c(1,1), col=c("black", "red", 1, 2))
