install.packages("TSclust")
library(TSclust)

#create a function which returns a vector of the sum of Euclidean distances for each time series
eucdistsums <- function(data){
  n <- ncol(data)
  N <- nrow(data)
  A <- matrix(rep(0, len=N^2), nrow = N)
  for (i in 1:N){
    for (j in 1:N){
      if (i < j){
        A[i,j] <- diss.EUCL(as.numeric(data[i,2:n]), as.numeric(data[j,2:n]))
      }
    }
  }
  A <- A + t(A)
  return(colSums(A))
}
eucdistsums(Ybookingsa)


#create a function which calculates threshold based on k-means threshold
thresholdfunc <- function(dists){
  return(mean(dists)+3*sqrt(var(dists)))
}
threshold(eucdistsums(Ybookingsa))


#create a function which determines the outliers
eucdistoutlierdet <- function(data){
  dists <- eucdistsums(data)
  return(which(dists >= thresholdfunc(dists)))
}
eucdistoutlierdet(Ybookingsa)


##############################################################################################################################################################################################
##############################################################################################################################################################################################

#Apply algorithm to a single outlier resulting from type 1 increase
eucdistoutlierdet(Ybookingsa)
eucdistoutlierdet(Mbookingsa)
eucdistoutlierdet(Kbookingsa)
eucdistoutlierdet(Totbookingsa)


##############################################################################################################################################################################################
##############################################################################################################################################################################################

#Apply algorithm to a single outlier resulting from type 1 and type 2 increase
eucdistoutlierdet(Ybookingsb)
eucdistoutlierdet(Mbookingsb)
eucdistoutlierdet(Kbookingsb)
eucdistoutlierdet(Totbookingsb)


##############################################################################################################################################################################################
##############################################################################################################################################################################################


#Apply algorithm to multiple outliers from the same distribution
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
  ky <- eucdistoutlierdet(Ybookingsm)
  km <- eucdistoutlierdet(Mbookingsm)
  kk <- eucdistoutlierdet(Kbookingsm)
  ktot <- eucdistoutlierdet(Totbookingsm)
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
d = data.frame(x = 1:25, GenRate = multigenuineM, FPRate = multifpratesY) 
par(mar = c(5,5,2,5))
with(d, plot(x, GenRate, type="l", col="black", xlab = "Number of Outliers", ylab="% of Genuine Outliers Detected", main = "Fare Class Y", cex.lab=1.2, cex.axis = 1.2, cex=1.2))
par(new = T)
with(d, plot(x, FPRate, type="l", col = "red", axes=F, xlab=NA, ylab=NA, cex=1.2))
axis(side = 4, cex.axis = 1.2)
mtext(side = 4, line = 3, '% of Observations False Positives')
legend("right", legend=c("Detection Rate", "False Positive Rate"), lty=c(1,1), col=c("black", "red", 1, 2))


##############################################################################################################################################################################################
##############################################################################################################################################################################################


#Apply algorithm to multiple outliers from different distributions
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
  ky <- eucdistoutlierdet(Ybookingsm)
  km <- eucdistoutlierdet(Mbookingsm)
  kk <- eucdistoutlierdet(Kbookingsm)
  ktot <- eucdistoutlierdet(Totbookingsm)
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



