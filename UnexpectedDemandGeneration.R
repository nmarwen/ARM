#Generate unexpected demand to go with sim01
#Start with inly increasing rate of type 1 arrivals
#rates1: rates1=c(20,30,60) =====> rates1=c(50,60,90)
nplfsim(n=1,BH=30,mu=180,L=0.01,C=200,SLFs=c(1,0.6,0.25),segments1=c(10,10,10),rates1=c(50,60,90),segments2=c(15,10,5),rates2=c(5,35,50),p1=0.95,p2=0.65,s=12345,name="sim02a")
simdat1a <- read.csv("sim02a.csv", header=TRUE, stringsAsFactors = FALSE)
#plot graphs and explore analysis
par(mfrow = c(2,2))
k <- nrow(simdat1)/3
#plot fare class Y bookings
plot(BH:1,cumsum(unlist(simdat1[1,2:31])),type="s", main="Customer Bookings for Fare Class Y", col = 1, xlim=c(BH, 0), ylim = c(0,40), xlab = "Booking Horizon (days)", ylab = "Bookings")
for (i in 2:k){
  lines(BH:1,cumsum(unlist(simdat1[((3*i)-2),2:31])),type="s",col=1)
}
lines(BH:1,cumsum(unlist(simdat1a[1,2:31])),type="s",col="red")
#plot fare class M bookings
plot(BH:1,cumsum(unlist(simdat1[2,2:31])),type="s", main="Customer Bookings for Fare Class M", col = 1, xlim=c(BH, 0), ylim = c(0,140), xlab = "Booking Horizon (days)", ylab = "Bookings")
for (i in 2:k){
  lines(BH:1,cumsum(unlist(simdat1[((3*i)-1),2:31])),type="s",col=1)
}
lines(BH:1,cumsum(unlist(simdat1a[2,2:31])),type="s",col="red")
#plot fare class K bookings
plot(BH:1,cumsum(unlist(simdat1[3,2:31])),type="s", main="Customer Bookings for Fare Class K", col = 1, xlim=c(BH, 0), ylim = c(0,40), xlab = "Booking Horizon (days)", ylab = "Bookings")
for (i in 2:k){
  lines(BH:1,cumsum(unlist(simdat1[(3*i),2:31])),type="s",col=1)
}
lines(BH:1,cumsum(unlist(simdat1a[3,2:31])),type="s",col="red")
#plot total bookings
plot(BH:1,cumsum(unlist(simdat1[1,2:31]+simdat1[2,2:31]+simdat1[3,2:31])),type="s", main="Total Customer Bookings", col = 1, xlim=c(BH, 0), ylim = c(0,200), xlab = "Booking Horizon (days)", ylab = "Bookings")
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


