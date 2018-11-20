#Poisson Arrivals
#Piecewise homogeneous Poisson process arrivals
pharrivals <- function(BH,L,segments,rates){
  if (sum(segments) != BH){
    print("Segments do not sum to length of booking horizon")
  }
  cust <- numeric(0)
  for (i in 1:(length(segments))){
    cust <- c(cust, (rpois(segments[i]/L,rates[i]/(segments[i]/L))))
  }
  return(cust)
}
pharrivals(30,0.01,c(10,10,10),c(20,30,40))
#Generate two types of customer: type 1 and type 2
Cust1 <- pharrivals(BH=30,L=0.01,segments=c(10,10,10),rates=c(20,30,40)) 
Cust2 <- pharrivals(BH=30,L=0.01,segments=c(15,10,5),rates=c(5,35,50)) 
#Save to files
write.csv(Cust1, "Cust1.csv", row.names=F)
write.csv(Cust2, "Cust2.csv", row.names=F)
#Plot the customer arrivals of type 1 and type 2 customers
plot(30:1,cumsum(aggBH(Cust1,30)),type="s", lwd = 2, col = "red", xlim=c(30, 0), ylim = c(0,200), xlab = "Booking Horizon (days)", ylab = "Bookings", cex.lab = 1.2, cex.axis = 1.2, main = "Piecewise Homogeneous Poisson Arrivals")
lines(30:1,cumsum(aggBH(Cust2,30)),type="s", lwd = 2, col = "blue", xlim=c(30, 0))
lines(30:1,cumsum(aggBH(Cust1,30)+aggBH(Cust2,30)),type="s", lwd = 2, col = "black", xlim=c(30, 0))
legend("topleft", c("Customer Type 1 Arrivals","Customer Type 2 Arrivals","Total Arrivals"), col = c("red", "blue", "black"), fill = c("red", "blue", "black"), cex = 1.2)


#######################################################################################################################################################################################################
#######################################################################################################################################################################################################


#Negative Binomial Arrivals
#Homoegenous
par(mfrow = c(1,2))
plot(30:1,cumsum(rpois(30,5)), type="s", lwd = 2, col = 1, xlim=c(30, 0), ylim = c(0,200), xlab = "Booking Horizon (days)", ylab = "Customer Arrivals", cex.lab = 1.2, cex.axis = 1.2, main = "Homoegenous Poisson Arrivals: Expected Mean per day = 5, Expected Variance per day = 5")
plot(30:1,cumsum(rnbinom(30, size=5, p=0.5)), type="s", lwd = 2, col = 1, xlim=c(30, 0), ylim = c(0,200), xlab = "Booking Horizon (days)", ylab = "Customer Arrivals", cex.lab = 1.2, cex.axis = 1.2, main = "Homoegenous Negative Binomial Arrivals: Expected Mean per day = 5, Expected Variance per day = 10")

#Piecewise Homogeneous NB Arrivals
nbharrivals <- function(BH,L,segments,sizes,probs){
  if (sum(segments) != BH){
    print("Segments do not sum to length of booking horizon")
  }
  cust <- numeric(0)
  for (i in 1:(length(segments))){
    cust <- c(cust, rnbinom(segments[i]/L, size=sizes[i]*L, p = probs[i]))
  }
  return(cust)
}
nbharrivals(30,0.01,c(10,10,10),c(2,3,4),c(0.5,0.5,0.5))
Cust1NB <- nbharrivals(30,0.01,c(10,10,10),c(2,3,4),c(0.5,0.5,0.5))
Cust2NB <- nbharrivals(30,0.01,c(15,10,5),c((1/3),3.5,10),c(0.5,0.5,0.5))
#Save to files
write.csv(Cust1NB, "Cust1NB.csv", row.names=F)
write.csv(Cust2NB, "Cust2NB.csv", row.names=F)
#Plot the customer arrivals of type 1 and type 2 customers
par(mfrow = c(1,2))
plot(30:1,cumsum(aggBH(Cust1NB,30)),type="s", lwd = 2, col = "red", xlim=c(30, 0), ylim = c(0,200), xlab = "Booking Horizon (days)", ylab = "Bookings", cex.lab = 1.2, cex.axis = 1.2, main = "Piecewise Homogeneous Negative Binomial Arrivals")
lines(30:1,cumsum(aggBH(Cust2NB,30)),type="s", lwd = 2, col = "blue", xlim=c(30, 0))
lines(30:1,cumsum(aggBH(Cust1NB,30)+aggBH(Cust2NB,30)),type="s", lwd = 2, col = "black", xlim=c(30, 0))
legend("topleft", c("Customer Type 1 Arrivals","Customer Type 2 Arrivals","Total Arrivals"), col = c("red", "blue", "black"), fill = c("red", "blue", "black"), cex = 1.2)


#######################################################################################################################################################################################################
#######################################################################################################################################################################################################


#Rewrite simulation code for different arrivals
NBplfsimulation <- function(BH,mu,L,C,SLFs,segments1,sizes1,probs1,segments2,sizes2,probs2,p1,p2){
  #Check function argument inputs are valid
  if (sum(segments1) != BH){
    print("Segments 1 do not sum to length of booking horizon")
  }
  if (sum(segments2) != BH){
    print("Segments 2 do not sum to length of booking horizon")
  }
  #Generate type 1 customer arrivals
  Cust1 <- nbharrivals(BH,L,segments1,sizes1,probs1)
  #Generate type 2 customer arrivals
  Cust2 <- nbharrivals(BH,L,segments1,sizes1,probs1)
  #Accept or reject bookings based on PLF
  N <- length(Cust1)
  Y <- numeric(N)
  M <- numeric(N)
  K <- numeric(N)
  for (i in 1:N){
    #Cust1 arrivals
    if ((sum(c(Y,M,K)) + Cust1[i]) < SLFs[2]*C){ 
      M[i] <- Cust1[i]
    }
    else if ((sum(c(Y,M,K)) < SLFs[2]*C)){
      M[i] <- SLFs[2]*C - sum(c(Y,M,K))
      #print(c("M",i))
    }
    else if ((sum(c(Y,M,K)) + Cust1[i]) < SLFs[1]*C){
      Y[i] <- round(Cust1[i]*p1)
    }
    else if ((sum(c(Y,M,K)) < SLFs[1]*C)){
      Y[i] <- round((SLFs[1]*C - sum(c(Y,M,K)))*p1)
    }
    #Cust2 arrivals
    if ((sum(c(Y,M,K)) + Cust2[i]) < SLFs[3]*C){
      K[i] <- Cust2[i]
    }
    else if ((sum(c(Y,M,K)) < SLFs[3]*C)){
      K[i] <- SLFs[3]*C - sum(c(Y,M,K))
      #print(c("K",i))
    }
    else if ((sum(c(Y,M,K)) + Cust2[i]) < SLFs[2]*C){
      M[i] <- M[i] + round((Cust2[i])*p2)
    }
    else if ((sum(c(Y,M,K)) < SLFs[2]*C)){
      M[i] <- M[i] + round((SLFs[2]*C - sum(c(Y,M,K)))*p2)
      #print(c("M",i))
    }
  }
  #Return the number of bookings made on each day
  return(matrix(c(aggBH(Y,BH),aggBH(M,BH),aggBH(K,BH)),ncol=3))
}
NBplfsimulation(BH=30,mu=180,L=0.01,C=200,SLFs=c(1,0.45,0.25),segments1=c(10,10,10),sizes1=c(2,3,4),probs1=c(0.5,0.5,0.5),segments2=c(15,10,5),c((1/3),3.5,10),probs2=c(0.5,0.5,0.5),p1=0.75,p2=0.65)

#Write a function that runs the above simulation n times and writes the results to a csv file
NBnplfsim <- function(n,BH,mu,L,C,SLFs,segments1,sizes1,probs1,segments2,sizes2,probs2,p1,p2,s,name){
  k <- NBplfsimulation(BH,mu,L,C,SLFs,segments1,sizes1,probs1,segments2,sizes2,probs2,p1,p2)
  colnames(k) <- c("Y","M","K")
  km <- t(k)
  for (i in 2:n){
    k <- NBplfsimulation(BH,mu,L,C,SLFs,segments1,sizes1,probs1,segments2,sizes2,probs2,p1,p2)
    colnames(k) <- c("Y","M","K")
    km <- rbind(km,t(k))
  }
  #return(km)
  set.seed(s)
  filename <- paste(name, ".csv", sep="") 
  write.csv(km, filename, row.names=T)
}
NBnplfsim(n=500,BH=30,mu=180,L=0.01,C=200,SLFs=c(1,0.45,0.25),segments1=c(10,10,10),sizes1=c(2,3,4),probs1=c(0.5,0.5,0.5),segments2=c(15,10,5),c((1/3),3.5,10),probs2=c(0.5,0.5,0.5),p1=0.85,p2=0.65,s=12345,name="sim02NB")


#######################################################################################################################################################################################################
#######################################################################################################################################################################################################


#import simulation data from csv
simdat2 <- read.csv("sim02NB.csv", header=TRUE, stringsAsFactors = FALSE)
#plot graphs and explore analysis
par(mfrow = c(2,2))
k <- nrow(simdat2)/3
BH <- 30
#plot fare class Y bookings
plot(BH:1,cumsum(unlist(simdat2[1,2:31])),type="s", main="Customer Bookings for Fare Class Y", col = 1, xlim=c(BH, 0), ylim = c(0,120), xlab = "Booking Horizon (days)", ylab = "Bookings", cex=1.2, cex.lab=1.2, cex.axis=1.2)
for (i in 2:k){
  lines(BH:1,cumsum(unlist(simdat2[((3*i)-2),2:31])),type="s",col=i)
}
#plot fare class M bookings
plot(BH:1,cumsum(unlist(simdat2[2,2:31])),type="s", main="Customer Bookings for Fare Class M", col = 1, xlim=c(BH, 0), ylim = c(0,120), xlab = "Booking Horizon (days)", ylab = "Bookings", cex=1.2, cex.lab=1.2, cex.axis=1.2)
for (i in 2:k){
  lines(BH:1,cumsum(unlist(simdat2[((3*i)-1),2:31])),type="s",col=i)
}
#plot fare class K bookings
plot(BH:1,cumsum(unlist(simdat2[3,2:31])),type="s", main="Customer Bookings for Fare Class K", col = 1, xlim=c(BH, 0), ylim = c(0,50), xlab = "Booking Horizon (days)", ylab = "Bookings", cex=1.2, cex.lab=1.2, cex.axis=1.2)
for (i in 2:k){
  lines(BH:1,cumsum(unlist(simdat2[(3*i),2:31])),type="s",col=i)
}
#plot total bookings
plot(BH:1,cumsum(unlist(simdat2[1,2:31]+simdat2[2,2:31]+simdat2[3,2:31])),type="s", main="Total Customer Bookings", col = 1, xlim=c(BH, 0), ylim = c(0,200), xlab = "Booking Horizon (days)", ylab = "Bookings", cex=1.2, cex.lab=1.2, cex.axis=1.2)
for (i in 2:k){
  lines(BH:1,cumsum(unlist(simdat2[((3*i)-2),2:31]+simdat2[((3*i)-1),2:31]+simdat2[(3*i),2:31])),type="s",col=i)
}
par(mfrow = c(1,1))


#######################################################################################################################################################################################################
#######################################################################################################################################################################################################


#Plot bookings and closure times for a single run with class closure times
k <- NBplfsimulation(BH=30,mu=180,L=0.01,C=200,SLFs=c(1,0.45,0.25),segments1=c(10,10,10),sizes1=c(2,3,4),probs1=c(0.5,0.5,0.5),segments2=c(15,10,5),sizes2=c((1/3),3.5,10),probs2=c(0.5,0.5,0.5),p1=0.85,p2=0.65)
plot(BH:1,cumsum(k[,1]), type="s", lwd = 2, main="Customer Bookings", col = 2, xlim=c(BH, 0), ylim = c(0,200), xlab = "Booking Horizon (days)", ylab = "Customers", cex=1.2, cex.lab = 1.2, cex.axis = 1.2)
lines(BH:1,cumsum(k[,2]), type="s", lwd = 2, col = 3, xlim=c(BH, 0))
lines(BH:1,cumsum(k[,3]), type="s", lwd = 2, col = 4, xlim=c(BH, 0))
lines(BH:1,cumsum(k[,1]+k[,2]+k[,3]), type="s", lwd = 2, col = 1, xlim=c(BH, 0))
abline(v=c(18.3,11.6), lwd = 2)
text(16.3,150,"K Closes", cex = 1.2)
text(9.6,150,"M Closes", cex = 1.2)
legend("topleft", legend = c("Y", "M", "K", "Total Customer Bookings"), fill = c(2,3,4,1), col = c(2,3,4,1))
#Separate booking data into fare classes for further analysis
YbookingsNB <- simdat2[seq(1, nrow(simdat2), 3), ]
MbookingsNB <- simdat2[seq(2, nrow(simdat2), 3), ]
KbookingsNB <- simdat2[seq(3, nrow(simdat2), 3), ]
TotbookingsNB <- YbookingsNB[,2:31] + MbookingsNB[,2:31] + KbookingsNB[,2:31] 
lab <- rep("Tot",500)
TotbookingsNB <- cbind(lab, TotbookingsNB)


#######################################################################################################################################################################################################
#######################################################################################################################################################################################################


#Generate an outlier (single outlier, increase in mean and variance of type 1 customers only)
NBnplfsim(n=1,BH=30,mu=180,L=0.01,C=200,SLFs=c(1,0.45,0.25),segments1=c(10,10,10),sizes1=c(3,4,5),probs1=c(0.4,0.4,0.4),segments2=c(15,10,5),sizes2=c((1/3),3.5,10),probs2=c(0.5,0.5,0.5),p1=0.85,p2=0.65,s=12345,name="sim02NBa")
simdat2a <- read.csv("sim02NBa.csv", header=TRUE, stringsAsFactors = FALSE)
par(mfrow = c(2,2))
k <- 500
BH <- 30
plot(BH:1,cumsum(unlist(simdat2[1,2:31])),type="s", main="Customer Bookings for Fare Class Y", col = 1, xlim=c(BH, 0), ylim = c(0,120), xlab = "Booking Horizon (days)", ylab = "Bookings", cex=1.2, cex.lab=1.2, cex.axis=1.2)
for (i in 2:k){
  lines(BH:1,cumsum(unlist(simdat2[((3*i)-2),2:31])),type="s",col=1)
}
lines(BH:1,cumsum(unlist(simdat2a[1,2:31])),type="s",col="red")
plot(BH:1,cumsum(unlist(simdat2[2,2:31])),type="s", main="Customer Bookings for Fare Class M", col = 1, xlim=c(BH, 0), ylim = c(0,120), xlab = "Booking Horizon (days)", ylab = "Bookings", cex=1.2, cex.lab=1.2, cex.axis=1.2)
for (i in 2:k){
  lines(BH:1,cumsum(unlist(simdat2[((3*i)-1),2:31])),type="s",col=1)
}
lines(BH:1,cumsum(unlist(simdat2a[2,2:31])),type="s",col="red")
plot(BH:1,cumsum(unlist(simdat2[3,2:31])),type="s", main="Customer Bookings for Fare Class K", col = 1, xlim=c(BH, 0), ylim = c(0,50), xlab = "Booking Horizon (days)", ylab = "Bookings", cex=1.2, cex.lab=1.2, cex.axis=1.2)
for (i in 2:k){
  lines(BH:1,cumsum(unlist(simdat2[(3*i),2:31])),type="s",col=1)
}
lines(BH:1,cumsum(unlist(simdat2a[3,2:31])),type="s",col="red")
plot(BH:1,cumsum(unlist(simdat2[3,2:31]+simdat2[2,2:31]+simdat2[3,2:31])),type="s", main="Total Customer Bookings", col = 1, xlim=c(BH, 0), ylim = c(0,200), xlab = "Booking Horizon (days)", ylab = "Bookings", cex=1.2, cex.lab=1.2, cex.axis=1.2)
for (i in 2:k){
  lines(BH:1,cumsum(unlist(simdat2[((3*i)-2),2:31]+simdat2[((3*i)-1),2:31]+simdat2[(3*i),2:31])),type="s",col=1)
}
lines(BH:1,cumsum(unlist(simdat2a[1,2:31]+simdat2a[2,2:31]+simdat2a[3,2:31])),type="s",col="red")
par(mfrow = c(1,1))

#Combine outlier into data
YbookingsNBa <- rbind(YbookingsNB,simdat2a[1,])
MbookingsNBa <- rbind(MbookingsNB,simdat2a[2,])
KbookingsNBa <- rbind(KbookingsNB,simdat2a[3,])
TotbookingsNBa <- YbookingsNBa[,2:31] + MbookingsNBa[,2:31] + KbookingsNBa[,2:31] 
lab <- rep("Tot",501)
TotbookingsNBa <- cbind(lab, TotbookingsNBa)

#Run k-means clustering outlier detection (k = 4,3,4,4)
set.seed(12345)
k.max <- 8
wss <- sapply(1:k.max,function(k){kmeans(TotbookingsNBa[,2:31], k, nstart=50,iter.max = 15 )$tot.withinss})
par(mfrow = c(1,1))
plot(1:k.max, wss, type="b", pch = 19, frame = FALSE, xlab="Number of clusters K", ylab="Total Within-Clusters Sum of Squares", cex=1.2, cex.lab=1.2, cex.axis=1.2)
par(mfrow = c(1,1))
outlierdet1(YbookingsNBa[,2:31],4)
outlierdet1(MbookingsNBa[,2:31],3) 
outlierdet1(KbookingsNBa[,2:31],4) 
outlierdet1(TotbookingsNBa[,2:31],4) 


#######################################################################################################################################################################################################
#######################################################################################################################################################################################################


#Rerun k-means clustering when variance is changing
p <- c(0.9,0.7,0.5,0.3,0.1)
fpoutputy <- list()
genoutputy <- list()
fpoutputm <- list()
genoutputm <- list()
fpoutputk <- list()
genoutputk <- list()
fpoutputtot <- list()
genoutputtot <- list()
for (i in 1:length(p)){
  #create empty vectors to store results
  fprates1y <- numeric(0)
  genuine1y <- numeric(0)
  fprates1m <- numeric(0)
  genuine1m <- numeric(0)
  fprates1k <- numeric(0)
  genuine1k <- numeric(0)
  fprates1tot <- numeric(0)
  genuine1tot <- numeric(0)
  #calculate sizes to keep mean constant
  n1 <- (p[i]/(1-p[i]))*c(2,3,4)
  n2 <- (p[i]/(1-p[i]))*c((1/3),3.5,10)
  #run simulation to establish normal behaviour
  NBnplfsim(n=500,BH=30,mu=180,L=0.01,C=200,SLFs=c(1,0.45,0.25),segments1=c(10,10,10),sizes1=n1,probs1=c(p[i],p[i],p[i]),segments2=c(15,10,5),sizes2=n2,probs2=c(p[i],p[i],p[i]),p1=0.85,p2=0.65,s=12345,name="VarTest1")
  #create an outlier
  NBnplfsim(n=1,BH=30,mu=180,L=0.01,C=200,SLFs=c(1,0.45,0.25),segments1=c(10,10,10),sizes1=n1+1,probs1=c(p[i],p[i],p[i]),segments2=c(15,10,5),sizes2=n2,probs2=c(p[i],p[i],p[i]),p1=0.85,p2=0.65,s=12345,name="VarTest1Out")
  #read in data
  VarTest1 <- read.csv("VarTest1.csv", header=TRUE, stringsAsFactors = FALSE)
  YbookingsNBV <- simdat2[seq(1, nrow(VarTest1), 3), ]
  MbookingsNBV <- simdat2[seq(2, nrow(VarTest1), 3), ]
  KbookingsNBV <- simdat2[seq(3, nrow(VarTest1), 3), ]
  TotbookingsNBV <- YbookingsNBV[,2:31] + MbookingsNBV[,2:31] + KbookingsNBV[,2:31] 
  lab <- rep("Tot",500)
  TotbookingsNBV <- cbind(lab, TotbookingsNBV)
  #combine data and split into fare classes
  VarTest1Out <- read.csv("VarTest1Out.csv", header=TRUE, stringsAsFactors = FALSE)
  YbookingsNBaV <- rbind(YbookingsNBV,VarTest1Out[1,])
  MbookingsNBaV <- rbind(MbookingsNBV,VarTest1Out[2,])
  KbookingsNBaV <- rbind(KbookingsNBV,VarTest1Out[3,])
  TotbookingsNBaV <- YbookingsNBaV[,2:31] + MbookingsNBaV[,2:31] + KbookingsNBaV[,2:31] 
  lab <- rep("Tot",501)
  TotbookingsNBaV <- cbind(lab, TotbookingsNBaV)
  #run outlier detection and store resulst
  ky <- outlierdet1(YbookingsNBaV[,2:31],5)
  fprates1y <- c(fprates1y, fpratedists(ky,501))
  genuine1y <- c(genuine1y, trueoutdet(ky,501))
  km <- outlierdet1(MbookingsNBaV[,2:31],5)
  fprates1m <- c(fprates1m, fpratedists(km,501))
  genuine1m <- c(genuine1m, trueoutdet(km,501))
  kk <- outlierdet1(KbookingsNBaV[,2:31],5)
  fprates1k <- c(fprates1k, fpratedists(kk,501))
  genuine1k <- c(genuine1k, trueoutdet(kk,501))
  ktot <- outlierdet1(TotbookingsNBaV[,2:31],5)
  fprates1tot <- c(fprates1tot, fpratedists(ktot,501))
  genuine1tot <- c(genuine1tot, trueoutdet(ktot,501))
  #store results for each p in a list
  fpoutputy[[i]] <- fprates1y
  genoutputy[[i]] <- genuine1y
  fpoutputm[[i]] <- fprates1m
  genoutputm[[i]] <- genuine1m
  fpoutputk[[i]] <- fprates1k
  genoutputk[[i]] <- genuine1k
  fpoutputtot[[i]] <- fprates1tot
  genoutputtot[[i]] <- genuine1tot
}
#separate data
fpoutputy <- unlist(fpoutputy)
genoutputy <- unlist(genoutputy)
fpoutputm <- unlist(fpoutputm)
genoutputm <- unlist(genoutputm)
fpoutputk <- unlist(fpoutputk)
genoutputk <- unlist(genoutputk)
fpoutputtot <- unlist(fpoutputtot)
genoutputtot <- unlist(genoutputtot)
#plot results
#Fare Class Y
par(mfrow = c(2,2))
d = data.frame(x = p, FPRate = fpoutputy)
par(mar = c(5,5,2,5))
with(d, plot(x, FPRate, type="l", col="red", xlab = "p", ylab="Number of False Positives", main = "Fare Class Y", xlim = c(1,0), cex=1.2, cex.lab=1.2, cex.axis=1.2))
par(new = T)
with(d, plot(x, FPRate, col = genoutputy+1, axes=F, xlim = c(1,0), pch=19, xlab=NA, ylab=NA, cex=1.2))
legend("bottomleft", legend=c("Outlier Not Detected", "Outlier Detected"), pch=c(19,19), col=c(1, 2))
#Repeat for other fare classes
#Fare class M
d = data.frame(x = p, FPRate = fpoutputm)
par(mar = c(5,5,2,5))
with(d, plot(x, FPRate, type="l", col="red", xlab = "p", ylab="Number of False Positives", main = "Fare Class M", xlim = c(1,0), cex=1.2, cex.lab=1.2, cex.axis=1.2))
par(new = T)
with(d, plot(x, FPRate, col = genoutputm+1, axes=F, xlim = c(1,0), pch=19, xlab=NA, ylab=NA, cex=1.2))
legend("top", legend=c("Outlier Not Detected", "Outlier Detected"), pch=c(19,19), col=c(1, 2))
#Fare Class K
d = data.frame(x = p, FPRate = fpoutputk)
par(mar = c(5,5,2,5))
with(d, plot(x, FPRate, type="l", col="red", xlab = "p", ylab="Number of False Positives", main = "Fare Class K", xlim = c(1,0), cex=1.2, cex.lab=1.2, cex.axis=1.2))
par(new = T)
with(d, plot(x, FPRate, col = genoutputk+1, axes=F, xlim = c(1,0), pch=19, xlab=NA, ylab=NA, cex=1.2))
legend("topleft", legend=c("Outlier Not Detected", "Outlier Detected"), pch=c(19,19), col=c(1, 2))
#Total Bookings
d = data.frame(x = p, FPRate = fpoutputtot)
par(mar = c(5,5,2,5))
with(d, plot(x, FPRate, type="l", col="red", xlab = "p", ylab="Number of False Positives", main = "Total Bookings", xlim = c(1,0), cex=1.2, cex.lab=1.2, cex.axis=1.2))
par(new = T)
with(d, plot(x, FPRate, col = genoutputtot+1, axes=F, xlim = c(1,0), pch=19, xlab=NA, ylab=NA, cex=1.2))
legend("bottomleft", legend=c("Outlier Not Detected", "Outlier Detected"), pch=c(19,19), col=c(1, 2))


#######################################################################################################################################################################################################
#######################################################################################################################################################################################################


#Rerun Euclidean distance outlier detection for changing variance
#Rerun k-means clustering when variance is changing
p <- c(0.9,0.7,0.5,0.3,0.1)
fpoutputy2 <- list()
genoutputy2 <- list()
fpoutputm2 <- list()
genoutputm2 <- list()
fpoutputk2 <- list()
genoutputk2 <- list()
fpoutputtot2 <- list()
genoutputtot2 <- list()
for (i in 1:length(p)){
  #create empty vectors to store results
  fprates1y <- numeric(0)
  genuine1y <- numeric(0)
  fprates1m <- numeric(0)
  genuine1m <- numeric(0)
  fprates1k <- numeric(0)
  genuine1k <- numeric(0)
  fprates1tot <- numeric(0)
  genuine1tot <- numeric(0)
  #calculate sizes to keep mean constant
  n1 <- (p[i]/(1-p[i]))*c(2,3,4)
  n2 <- (p[i]/(1-p[i]))*c((1/3),3.5,10)
  #run simulation to establish normal behaviour
  NBnplfsim(n=500,BH=30,mu=180,L=0.01,C=200,SLFs=c(1,0.45,0.25),segments1=c(10,10,10),sizes1=n1,probs1=c(p[i],p[i],p[i]),segments2=c(15,10,5),sizes2=n2,probs2=c(p[i],p[i],p[i]),p1=0.85,p2=0.65,s=12345,name="VarTest1")
  #create an outlier
  NBnplfsim(n=1,BH=30,mu=180,L=0.01,C=200,SLFs=c(1,0.45,0.25),segments1=c(10,10,10),sizes1=n1+1,probs1=c(p[i],p[i],p[i]),segments2=c(15,10,5),sizes2=n2,probs2=c(p[i],p[i],p[i]),p1=0.85,p2=0.65,s=12345,name="VarTest1Out")
  #read in data
  VarTest1 <- read.csv("VarTest1.csv", header=TRUE, stringsAsFactors = FALSE)
  YbookingsNBV <- simdat2[seq(1, nrow(VarTest1), 3), ]
  MbookingsNBV <- simdat2[seq(2, nrow(VarTest1), 3), ]
  KbookingsNBV <- simdat2[seq(3, nrow(VarTest1), 3), ]
  TotbookingsNBV <- YbookingsNBV[,2:31] + MbookingsNBV[,2:31] + KbookingsNBV[,2:31] 
  lab <- rep("Tot",500)
  TotbookingsNBV <- cbind(lab, TotbookingsNBV)
  #combine data and split into fare classes
  VarTest1Out <- read.csv("VarTest1Out.csv", header=TRUE, stringsAsFactors = FALSE)
  YbookingsNBaV <- rbind(YbookingsNBV,VarTest1Out[1,])
  MbookingsNBaV <- rbind(MbookingsNBV,VarTest1Out[2,])
  KbookingsNBaV <- rbind(KbookingsNBV,VarTest1Out[3,])
  TotbookingsNBaV <- YbookingsNBaV[,2:31] + MbookingsNBaV[,2:31] + KbookingsNBaV[,2:31] 
  lab <- rep("Tot",501)
  TotbookingsNBaV <- cbind(lab, TotbookingsNBaV)
  #run outlier detection and store resulst
  ky <- eucdistoutlierdet(YbookingsNBaV[,2:31])
  fprates1y <- c(fprates1y, fpratedists(ky,501))
  genuine1y <- c(genuine1y, trueoutdet(ky,501))
  km <- eucdistoutlierdet(MbookingsNBaV[,2:31])
  fprates1m <- c(fprates1m, fpratedists(km,501))
  genuine1m <- c(genuine1m, trueoutdet(km,501))
  kk <- eucdistoutlierdet(KbookingsNBaV[,2:31])
  fprates1k <- c(fprates1k, fpratedists(kk,501))
  genuine1k <- c(genuine1k, trueoutdet(kk,501))
  ktot <- eucdistoutlierdet(TotbookingsNBaV[,2:31])
  fprates1tot <- c(fprates1tot, fpratedists(ktot,501))
  genuine1tot <- c(genuine1tot, trueoutdet(ktot,501))
  #store results for each p in a list
  fpoutputy2[[i]] <- fprates1y
  genoutputy2[[i]] <- genuine1y
  fpoutputm2[[i]] <- fprates1m
  genoutputm2[[i]] <- genuine1m
  fpoutputk2[[i]] <- fprates1k
  genoutputk2[[i]] <- genuine1k
  fpoutputtot2[[i]] <- fprates1tot
  genoutputtot2[[i]] <- genuine1tot
}
#separate data
fpoutputy2 <- unlist(fpoutputy2)
genoutputy2 <- unlist(genoutputy2)
fpoutputm2 <- unlist(fpoutputm2)
genoutputm2 <- unlist(genoutputm2)
fpoutputk2 <- unlist(fpoutputk2)
genoutputk2 <- unlist(genoutputk2)
fpoutputtot2 <- unlist(fpoutputtot2)
genoutputtot2 <- unlist(genoutputtot2)
#plot results
#Fare Class Y
par(mfrow = c(2,2))
d = data.frame(x = p, FPRate = fpoutputy2)
par(mar = c(5,5,2,5))
with(d, plot(x, FPRate, type="l", col="red", xlab = "p", ylab="Number of False Positives", main = "Fare Class Y", xlim = c(1,0), cex=1.2, cex.lab=1.2, cex.axis=1.2))
par(new = T)
with(d, plot(x, FPRate, col = genoutputy2+1, axes=F, xlim = c(1,0), pch=19, xlab=NA, ylab=NA, cex=1.2))
legend("bottomleft", legend=c("Outlier Not Detected", "Outlier Detected"), pch=c(19,19), col=c(1, 2))
#Repeat for other fare classes
#Fare class M
d = data.frame(x = p, FPRate = fpoutputm2)
par(mar = c(5,5,2,5))
with(d, plot(x, FPRate, type="l", col="red", xlab = "p", ylab="Number of False Positives", main = "Fare Class M", xlim = c(1,0), cex=1.2, cex.lab=1.2, cex.axis=1.2))
par(new = T)
with(d, plot(x, FPRate, col = genoutputm2+1, axes=F, xlim = c(1,0), pch=19, xlab=NA, ylab=NA, cex=1.2))
legend("top", legend=c("Outlier Not Detected", "Outlier Detected"), pch=c(19,19), col=c(1, 2))
#Fare Class K
d = data.frame(x = p, FPRate = fpoutputk2)
par(mar = c(5,5,2,5))
with(d, plot(x, FPRate, type="l", col="red", xlab = "p", ylab="Number of False Positives", main = "Fare Class K", xlim = c(1,0), cex=1.2, cex.lab=1.2, cex.axis=1.2))
par(new = T)
with(d, plot(x, FPRate, col = genoutputk2+1, axes=F, xlim = c(1,0), pch=19, xlab=NA, ylab=NA, cex=1.2))
legend("topleft", legend=c("Outlier Not Detected", "Outlier Detected"), pch=c(19,19), col=c(1, 2))
#Total Bookings
d = data.frame(x = p, FPRate = fpoutputtot2)
par(mar = c(5,5,2,5))
with(d, plot(x, FPRate, type="l", col="red", xlab = "p", ylab="Number of False Positives", main = "Total Bookings", xlim = c(1,0), cex=1.2, cex.lab=1.2, cex.axis=1.2))
par(new = T)
with(d, plot(x, FPRate, col = genoutputtot2+1, axes=F, xlim = c(1,0), pch=19, xlab=NA, ylab=NA, cex=1.2))
legend("bottomleft", legend=c("Outlier Not Detected", "Outlier Detected"), pch=c(19,19), col=c(1, 2))

