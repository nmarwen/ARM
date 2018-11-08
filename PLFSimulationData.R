#import simulation data from csv
simdat1 <- read.csv("sim02.csv", header=TRUE, stringsAsFactors = FALSE)
#plot graphs and explore analysis
par(mfrow = c(2,2))
k <- nrow(simdat1)/3
BH <- 30
#plot fare class Y bookings
plot(BH:1,cumsum(unlist(simdat1[1,2:31])),type="s", main="Customer Bookings for Fare Class Y", col = 1, xlim=c(BH, 0), ylim = c(0,40), xlab = "Booking Horizon (days)", ylab = "Bookings")
for (i in 2:k){
  lines(BH:1,cumsum(unlist(simdat1[((3*i)-2),2:31])),type="s",col=i)
}
#plot fare class M bookings
plot(BH:1,cumsum(unlist(simdat1[2,2:31])),type="s", main="Customer Bookings for Fare Class M", col = 1, xlim=c(BH, 0), ylim = c(0,140), xlab = "Booking Horizon (days)", ylab = "Bookings")
for (i in 2:k){
  lines(BH:1,cumsum(unlist(simdat1[((3*i)-1),2:31])),type="s",col=i)
}
#plot fare class K bookings
plot(BH:1,cumsum(unlist(simdat1[3,2:31])),type="s", main="Customer Bookings for Fare Class K", col = 1, xlim=c(BH, 0), ylim = c(0,40), xlab = "Booking Horizon (days)", ylab = "Bookings")
for (i in 2:k){
  lines(BH:1,cumsum(unlist(simdat1[(3*i),2:31])),type="s",col=i)
}
#plot total bookings
plot(BH:1,cumsum(unlist(simdat1[1,2:31]+simdat1[2,2:31]+simdat1[3,2:31])),type="s", main="Total Customer Bookings", col = 1, xlim=c(BH, 0), ylim = c(0,200), xlab = "Booking Horizon (days)", ylab = "Bookings")
for (i in 2:k){
  lines(BH:1,cumsum(unlist(simdat1[((3*i)-2),2:31]+simdat1[((3*i)-1),2:31]+simdat1[(3*i),2:31])),type="s",col=i)
}
par(mfrow = c(1,1))

#Plot with customer arrivals and customer bookings to look at censoring
set.seed(12345)
k <- plfsimulation(BH=30,mu=180,L=0.01,C=200,SLFs=c(1,0.6,0.25),segments1=c(10,10,10),rates1=c(20,30,40),segments2=c(15,10,5),rates2=c(5,35,50),p1=0.95,p2=0.65)
plot(BH:1,cumsum(k[,1]+k[,2]+k[,3]), type="s", main="Customer Arrivals vs Bookings", col = 1, xlim=c(BH, 0), ylim = c(0,220), xlab = "Booking Horizon (days)", ylab = "Customers")
lines(BH:1,cumsum(aggBH(Cust1,30)+aggBH(Cust2,30)), type="s", col = 2)
legend("topleft", legend = c("Total Customer Bookings", "Total Customer Arrivals"), fill = c(1,2), col = c(1,2))
#plot bookings and closure times for a single run 
#use k above 


#Separate booking data into fare classes for further analysis
Ybookings <- simdat1[seq(1, nrow(simdat1), 3), ]
Mbookings <- simdat1[seq(2, nrow(simdat1), 3), ]
Kbookings <- simdat1[seq(3, nrow(simdat1), 3), ]
Totbookings <- Ybookings[,2:31] + Mbookings[,2:31] + Kbookings[,2:31] 
lab <- rep("Tot",501)
Totbookings <- cbind(lab, Totbookings)
