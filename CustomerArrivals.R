#Homogeneous Poisson process arrivals function (BH=30, L=0.01, mu=180)
arrivals <- function(BH,L,mu){
  return(rpois(BH/L,mu/(BH/L)))
}
arrivals(30,0.01,180)


#Inhomogeneous Poisson process arrivals
#with intensity function of the form at + b
ipparrivals <- function(BH, L, a, b){
  ipparrivals1 <- numeric(0)
  for (i in 1:(BH/L)){
    ipparrivals1 <- c(ipparrivals1,rpois(1,((a/2)*(i^2-(i-1)^2) + b*(i - (i-1)))/(BH/L)))
  }
  return(ipparrivals1)
}
ipparrivals(30,0.01,1/3,1)


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


#Define a function for aggregating data into days
aggBH <- function(data,BH){
  k <- length(data)/BH
  if (k%%1 != 0){
    return("vector not an integer multiple of booking horizon")
  }
  else {
    output <- unname(tapply(data, (seq_along(data)-1) %/% k, sum))
    return(output)
  }
}
aggBH(arrivals(30,0.01,180),30)


#Generate two types of customer: type 1 and type 2
Cust1 <- pharrivals(BH=30,L=0.01,segments=c(10,10,10),rates=c(20,30,40)) 
Cust2 <- pharrivals(BH=30,L=0.01,segments=c(15,10,5),rates=c(5,35,50)) 
#Save to files
write.csv(Cust1, "Cust1.csv", row.names=F)
write.csv(Cust2, "Cust2.csv", row.names=F)
#Plot the customer arrivals of type 1 and type 2 customers
plot(30:1,cumsum(aggBH(Cust1,30)),type="s", col = "red", xlim=c(30, 0), ylim = c(0,200), xlab = "Booking Horizon (days)", ylab = "Bookings")
lines(30:1,cumsum(aggBH(Cust2,30)),type="s", col = "green", xlim=c(30, 0))
lines(30:1,cumsum(aggBH(Cust1,30)+aggBH(Cust2,30)),type="s", col = "black", xlim=c(30, 0))
legend("topleft", c("Customer Type 1 Arrivals","Customer Type 2 Arrivals","Total Arrivals"), col = c("red", "green", "black"), fill = c("red", "green", "black"))