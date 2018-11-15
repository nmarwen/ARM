#Generate unexpected demand to go with sim01
#Start with inly increasing rate of type 1 arrivals
#rates1: rates1=c(20,30,60) =====> rates1=c(40,50,80)
nplfsim(n=1,BH=30,mu=180,L=0.01,C=200,SLFs=c(1,0.45,0.25),segments1=c(10,10,10),rates1=c(40,50,80),segments2=c(15,10,5),rates2=c(5,35,50),p1=0.85,p2=0.65,s=12345,name="sim02a")
simdat1a <- read.csv("sim02a.csv", header=TRUE, stringsAsFactors = FALSE)
#plot graphs and explore analysis
par(mfrow = c(2,2))
k <- nrow(simdat1)/3
#plot fare class Y bookings
plot(BH:1,cumsum(unlist(simdat1[1,2:31])),type="s", main="Customer Bookings for Fare Class Y", col = 1, xlim=c(BH, 0), ylim = c(0,100), xlab = "Booking Horizon (days)", ylab = "Bookings", cex=1.2, cex.lab=1.2, cex.axis=1.2)
for (i in 2:k){
  lines(BH:1,cumsum(unlist(simdat1[((3*i)-2),2:31])),type="s",col=1)
}
lines(BH:1,cumsum(unlist(simdat1a[1,2:31])),type="s",col="red")
#plot fare class M bookings
plot(BH:1,cumsum(unlist(simdat1[2,2:31])),type="s", main="Customer Bookings for Fare Class M", col = 1, xlim=c(BH, 0), ylim = c(0,100), xlab = "Booking Horizon (days)", ylab = "Bookings", cex=1.2, cex.lab=1.2, cex.axis=1.2)
for (i in 2:k){
  lines(BH:1,cumsum(unlist(simdat1[((3*i)-1),2:31])),type="s",col=1)
}
lines(BH:1,cumsum(unlist(simdat1a[2,2:31])),type="s",col="red")
#plot fare class K bookings
plot(BH:1,cumsum(unlist(simdat1[3,2:31])),type="s", main="Customer Bookings for Fare Class K", col = 1, xlim=c(BH, 0), ylim = c(0,35), xlab = "Booking Horizon (days)", ylab = "Bookings", cex=1.2, cex.lab=1.2, cex.axis=1.2)
for (i in 2:k){
  lines(BH:1,cumsum(unlist(simdat1[(3*i),2:31])),type="s",col=1)
}
lines(BH:1,cumsum(unlist(simdat1a[3,2:31])),type="s",col="red")
#plot total bookings
plot(BH:1,cumsum(unlist(simdat1[1,2:31]+simdat1[2,2:31]+simdat1[3,2:31])),type="s", main="Total Customer Bookings", col = 1, xlim=c(BH, 0), ylim = c(0,200), xlab = "Booking Horizon (days)", ylab = "Bookings", cex=1.2, cex.lab=1.2, cex.axis=1.2)
for (i in 2:k){
  lines(BH:1,cumsum(unlist(simdat1[((3*i)-2),2:31]+simdat1[((3*i)-1),2:31]+simdat1[(3*i),2:31])),type="s",col=1)
}
lines(BH:1,cumsum(unlist(simdat1a[1,2:31]+simdat1a[2,2:31]+simdat1a[3,2:31])),type="s",col="red")
par(mfrow = c(1,1))
#to clear plotting window
plot.new()
frame()
#Combine data frames to include outlier in each fare class
Ybookingsa <- rbind(Ybookings,simdat1a[1,])
Mbookingsa <- rbind(Mbookings,simdat1a[2,])
Kbookingsa <- rbind(Kbookings,simdat1a[3,])
Totbookingsa <- Ybookingsa[,2:31] + Mbookingsa[,2:31] + Kbookingsa[,2:31] 
lab <- rep("Tot",501)
Totbookingsa <- cbind(lab, Totbookingsa)


#####################################################################################################################################################################################################################################################
#####################################################################################################################################################################################################################################################


#Now increase rate of both type 1 and type 2 arrivals
#rates1: rates1=c(20,30,60) =====> rates1=c(40,50,80)
#rates2: rates2=c(5,35,50) =====> rates2=c(15,50,60)
nplfsim(n=1,BH=30,mu=180,L=0.01,C=200,SLFs=c(1,0.45,0.25),segments1=c(10,10,10),rates1=c(40,50,80),segments2=c(15,10,5),rates2=c(15,50,60),p1=0.85,p2=0.65,s=12345,name="sim02b")
simdat1b <- read.csv("sim02b.csv", header=TRUE, stringsAsFactors = FALSE)
#plot graphs and explore analysis
par(mfrow = c(2,2))
k <- nrow(simdat1)/3
#plot fare class Y bookings
plot(BH:1,cumsum(unlist(simdat1[1,2:31])),type="s", main="Customer Bookings for Fare Class Y", col = 1, xlim=c(BH, 0), ylim = c(0,100), xlab = "Booking Horizon (days)", ylab = "Bookings", cex=1.2, cex.lab=1.2, cex.axis=1.2)
for (i in 2:k){
  lines(BH:1,cumsum(unlist(simdat1[((3*i)-2),2:31])),type="s",col=1)
}
lines(BH:1,cumsum(unlist(simdat1b[1,2:31])),type="s",col="red")
#plot fare class M bookings
plot(BH:1,cumsum(unlist(simdat1[2,2:31])),type="s", main="Customer Bookings for Fare Class M", col = 1, xlim=c(BH, 0), ylim = c(0,120), xlab = "Booking Horizon (days)", ylab = "Bookings", cex=1.2, cex.lab=1.2, cex.axis=1.2)
for (i in 2:k){
  lines(BH:1,cumsum(unlist(simdat1[((3*i)-1),2:31])),type="s",col=1)
}
lines(BH:1,cumsum(unlist(simdat1b[2,2:31])),type="s",col="red")
#plot fare class K bookings
plot(BH:1,cumsum(unlist(simdat1[3,2:31])),type="s", main="Customer Bookings for Fare Class K", col = 1, xlim=c(BH, 0), ylim = c(0,35), xlab = "Booking Horizon (days)", ylab = "Bookings", cex=1.2, cex.lab=1.2, cex.axis=1.2)
for (i in 2:k){
  lines(BH:1,cumsum(unlist(simdat1[(3*i),2:31])),type="s",col=1)
}
lines(BH:1,cumsum(unlist(simdat1b[3,2:31])),type="s",col="red")
#plot total bookings
plot(BH:1,cumsum(unlist(simdat1[1,2:31]+simdat1[2,2:31]+simdat1[3,2:31])),type="s", main="Total Customer Bookings", col = 1, xlim=c(BH, 0), ylim = c(0,200), xlab = "Booking Horizon (days)", ylab = "Bookings", cex=1.2, cex.lab=1.2, cex.axis=1.2)
for (i in 2:k){
  lines(BH:1,cumsum(unlist(simdat1[((3*i)-2),2:31]+simdat1[((3*i)-1),2:31]+simdat1[(3*i),2:31])),type="s",col=1)
}
lines(BH:1,cumsum(unlist(simdat1b[1,2:31]+simdat1b[2,2:31]+simdat1b[3,2:31])),type="s",col="red")
par(mfrow = c(1,1))
#to clear plotting window
plot.new()
frame()
#Combine data frames to include outlier in each fare class
Ybookingsb <- rbind(Ybookings,simdat1a[1,])
Mbookingsb <- rbind(Mbookings,simdat1a[2,])
Kbookingsb <- rbind(Kbookings,simdat1a[3,])
Totbookingsb <- Ybookingsb[,2:31] + Mbookingsb[,2:31] + Kbookingsb[,2:31] 
lab <- rep("Tot",501)
Totbookingsb <- cbind(lab, Totbookingsb)


#####################################################################################################################################################################################################################################################
#####################################################################################################################################################################################################################################################


#Generate Multiple Outliers as a result of increased type 1 demand
#introduce 25 outliers e.g 5% of flights are outliers
nplfsim(n=25,BH=30,mu=180,L=0.01,C=200,SLFs=c(1,0.45,0.25),segments1=c(10,10,10),rates1=c(40,50,80),segments2=c(15,10,5),rates2=c(5,35,50),p1=0.85,p2=0.65,s=12345,name="sim02c")
simdat1c <- read.csv("sim02c.csv", header=TRUE, stringsAsFactors = FALSE)
par(mfrow=c(2,2))
k <- nrow(simdat1)/3
plot(BH:1,cumsum(unlist(simdat1[1,2:31])),type="s", main="Customer Bookings for Fare Class Y", col = 1, xlim=c(BH, 0), ylim = c(0,120), xlab = "Booking Horizon (days)", ylab = "Bookings", cex=1.2, cex.lab=1.2, cex.axis=1.2)
for (i in 2:k){
  lines(BH:1,cumsum(unlist(simdat1[((3*i)-2),2:31])),type="s",col=1)
}
for (i in 1:25){
  lines(BH:1,cumsum(unlist(simdat1c[((3*i)-2),2:31])),type="s",col="red")
}
plot(BH:1,cumsum(unlist(simdat1[2,2:31])),type="s", main="Customer Bookings for Fare Class M", col = 1, xlim=c(BH, 0), ylim = c(0,120), xlab = "Booking Horizon (days)", ylab = "Bookings", cex=1.2, cex.lab=1.2, cex.axis=1.2)
for (i in 2:k){
  lines(BH:1,cumsum(unlist(simdat1[((3*i)-1),2:31])),type="s",col=1)
}
for (i in 1:25){
  lines(BH:1,cumsum(unlist(simdat1c[((3*i)-1),2:31])),type="s",col="red")
}
plot(BH:1,cumsum(unlist(simdat1[3,2:31])),type="s", main="Customer Bookings for Fare Class K", col = 1, xlim=c(BH, 0), ylim = c(0,40), xlab = "Booking Horizon (days)", ylab = "Bookings", cex=1.2, cex.lab=1.2, cex.axis=1.2)
for (i in 2:k){
  lines(BH:1,cumsum(unlist(simdat1[(3*i),2:31])),type="s",col=1)
}
for (i in 1:25){
  lines(BH:1,cumsum(unlist(simdat1c[((3*i)),2:31])),type="s",col="red")
}
plot(BH:1,cumsum(unlist(simdat1[1,2:31]+simdat1[2,2:31]+simdat1[3,2:31])),type="s", main="Total Customer Bookings", col = 1, xlim=c(BH, 0), ylim = c(0,200), xlab = "Booking Horizon (days)", ylab = "Bookings", cex=1.2, cex.lab=1.2, cex.axis=1.2)
for (i in 2:k){
  lines(BH:1,cumsum(unlist(simdat1[((3*i)-2),2:31]+simdat1[((3*i)-1),2:31]+simdat1[(3*i),2:31])),type="s",col=1)
}
for (i in 1:25){
  lines(BH:1,cumsum(unlist(simdat1c[((3*i)),2:31]+simdat1c[((3*i)-1),2:31]+simdat1c[((3*i)-2),2:31])),type="s",col="red")
}
par(mfrow = c(1,1))
#to clear plotting window
plot.new()
frame()
Ybookingsc <- rbind(Ybookings,simdat1c[seq(1, nrow(simdat1c), 3),])
Mbookingsc <- rbind(Mbookings,simdat1c[seq(2, nrow(simdat1c), 3),])
Kbookingsc <- rbind(Kbookings,simdat1c[seq(3, nrow(simdat1c), 3),])
Totbookingsc <- Ybookingsc[,2:31] + Mbookingsc[,2:31] + Kbookingsc[,2:31] 
lab <- rep("Tot",525)
Totbookingsc <- cbind(lab, Totbookingsc)


#####################################################################################################################################################################################################################################################
#####################################################################################################################################################################################################################################################


#Generate multiple outliers from different distributions
#Introduce 5 outliers from different distributions
nplfsim(n=5,BH=30,mu=180,L=0.01,C=200,SLFs=c(1,0.45,0.25),segments1=c(10,10,10),rates1=c(40,50,80),segments2=c(15,10,5),rates2=c(5,35,50),p1=0.85,p2=0.65,s=12345,name="sim02d1")
nplfsim(n=5,BH=30,mu=180,L=0.01,C=200,SLFs=c(1,0.45,0.25),segments1=c(10,10,10),rates1=c(30,50,80),segments2=c(15,10,5),rates2=c(15,35,60),p1=0.85,p2=0.65,s=12345,name="sim02d2")
nplfsim(n=5,BH=30,mu=180,L=0.01,C=200,SLFs=c(1,0.45,0.25),segments1=c(10,10,10),rates1=c(40,40,80),segments2=c(15,10,5),rates2=c(5,45,50),p1=0.85,p2=0.65,s=12345,name="sim02d3")
nplfsim(n=5,BH=30,mu=180,L=0.01,C=200,SLFs=c(1,0.45,0.25),segments1=c(10,10,10),rates1=c(50,50,60),segments2=c(15,10,5),rates2=c(5,45,65),p1=0.85,p2=0.65,s=12345,name="sim02d4")
nplfsim(n=5,BH=30,mu=180,L=0.01,C=200,SLFs=c(1,0.45,0.25),segments1=c(10,10,10),rates1=c(40,35,70),segments2=c(15,10,5),rates2=c(15,40,55),p1=0.85,p2=0.65,s=12345,name="sim02d5")
simdat1d1 <- read.csv("sim02d1.csv", header=TRUE, stringsAsFactors = FALSE)
simdat1d2 <- read.csv("sim02d2.csv", header=TRUE, stringsAsFactors = FALSE)
simdat1d3 <- read.csv("sim02d3.csv", header=TRUE, stringsAsFactors = FALSE)
simdat1d4 <- read.csv("sim02d4.csv", header=TRUE, stringsAsFactors = FALSE)
simdat1d5 <- read.csv("sim02d5.csv", header=TRUE, stringsAsFactors = FALSE)
#to clear plotting window
plot.new()
frame()
Ybookingsd <- rbind(Ybookings,simdat1d1[seq(1, nrow(simdat1d1), 3), ],simdat1d2[seq(1, nrow(simdat1d2), 3), ],simdat1d3[seq(1, nrow(simdat1d3), 3), ],simdat1d4[seq(1, nrow(simdat1d4), 3), ],simdat1d5[seq(1, nrow(simdat1d4), 3), ])
Mbookingsd <- rbind(Mbookings,simdat1d1[seq(2, nrow(simdat1d1), 3), ],simdat1d2[seq(2, nrow(simdat1d2), 3), ],simdat1d3[seq(2, nrow(simdat1d3), 3), ],simdat1d4[seq(2, nrow(simdat1d4), 3), ],simdat1d5[seq(2, nrow(simdat1d4), 3), ])
Kbookingsd <- rbind(Kbookings,simdat1d1[seq(3, nrow(simdat1d1), 3), ],simdat1d2[seq(3, nrow(simdat1d2), 3), ],simdat1d3[seq(3, nrow(simdat1d3), 3), ],simdat1d4[seq(3, nrow(simdat1d4), 3), ],simdat1d5[seq(3, nrow(simdat1d4), 3), ])
Totbookingsd <- Ybookingsd[,2:31] + Mbookingsd[,2:31] + Kbookingsd[,2:31] 
lab <- rep("Tot",525)
Totbookingsd <- cbind(lab, Totbookingsd)
plot(BH:1,cumsum(unlist(Ybookingsd[1,2:31])),type="s", main="Customer Bookings for Fare Class Y", col = 1, xlim=c(BH, 0), ylim = c(0,120), xlab = "Booking Horizon (days)", ylab = "Bookings", cex=1.2, cex.lab=1.2, cex.axis=1.2)
for (i in 2:500){
  lines(BH:1,cumsum(unlist(Ybookingsd[i,2:31])),type="s",col=1)
}
for (i in 501:525){
  lines(BH:1,cumsum(unlist(Ybookingsd[i,2:31])),type="s",col="red")
}
plot(BH:1,cumsum(unlist(Mbookingsd[1,2:31])),type="s", main="Customer Bookings for Fare Class M", col = 1, xlim=c(BH, 0), ylim = c(0,120), xlab = "Booking Horizon (days)", ylab = "Bookings", cex=1.2, cex.lab=1.2, cex.axis=1.2)
for (i in 2:500){
  lines(BH:1,cumsum(unlist(Mbookingsd[i,2:31])),type="s",col=1)
}
for (i in 501:525){
  lines(BH:1,cumsum(unlist(Mbookingsd[i,2:31])),type="s",col="red")
}
plot(BH:1,cumsum(unlist(Kbookingsd[1,2:31])),type="s", main="Customer Bookings for Fare Class K", col = 1, xlim=c(BH, 0), ylim = c(0,40), xlab = "Booking Horizon (days)", ylab = "Bookings", cex=1.2, cex.lab=1.2, cex.axis=1.2)
for (i in 2:500){
  lines(BH:1,cumsum(unlist(Kbookingsd[i,2:31])),type="s",col=1)
}
for (i in 501:525){
  lines(BH:1,cumsum(unlist(Kbookingsd[i,2:31])),type="s",col="red")
}
plot(BH:1,cumsum(unlist(Totbookingsd[1,2:31])),type="s", main="Total Customer Bookings", col = 1, xlim=c(BH, 0), ylim = c(0,200), xlab = "Booking Horizon (days)", ylab = "Bookings", cex=1.2, cex.lab=1.2, cex.axis=1.2)
for (i in 2:500){
  lines(BH:1,cumsum(unlist(Totbookingsd[i,2:31])),type="s",col=1)
}
for (i in 501:525){
  lines(BH:1,cumsum(unlist(Totbookingsd[i,2:31])),type="s",col="red")
}
