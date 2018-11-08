#Import data
setwd("C:/Users/rennien/Box Sync/PhD/Data/Kadatz")
contdata = read.csv("cont.csv", header = TRUE, stringsAsFactors = FALSE)
contdata <- contdata[,-ncol(contdata)]
head(contdata)


#Separate data by airports
airports <- unique(contdata[,1][contdata != ""])
#remove NAs, Total, and date
airports <- airports[1:(length(airports)-3)]
#create sub-dataframes for each airport
which(contdata[,1] == "ZAD") - which(contdata[,1] == "ZAG") 
ncol(contdata) 
K <- length(airports)
for (i in 1:K){
  df <- contdata[(31*i-30):(31*i),2:28] 
  assign(airports[i],df)
}


#Plot booking data for each fare class and totals for eight random airports
sample(1:K,8,replace=FALSE) #28 13 38  1 35 33 26 52
apl <- sort(c(28, 13, 38,  1, 35, 33, 26, 52))
par(mfrow = c(2,4))
airports[apl]
#"ZAG" "LHR" "MPL" "CDG" "VLC" "SVQ" "LPA" "BSL"
plot(seq(12,0,by=-0.5), ZAG[31,3:27], type="s", main="Zagreb Airport (ZAG)", xlab="Booking Horizon (months before departure)", ylab="Number of Passengers", xlim=c(12,0))
for(i in 1:30){
  lines(seq(12,0,by=-0.5), ZAG[i,3:27], type="s", col=i+1)
}
plot(seq(12,0,by=-0.5), LHR[31,3:27], type="s", main="Heathrow Airport (LHR)", xlab="Booking Horizon (months before departure)", ylab="Number of Passengers", xlim=c(12,0))
for(i in 1:30){
  lines(seq(12,0,by=-0.5), LHR[i,3:27], type="s", col=i+1)
}
plot(seq(12,0,by=-0.5), MPL[31,3:27], type="s", main="Montpellier Airport (MPL)", xlab="Booking Horizon (months before departure)", ylab="Number of Passengers", xlim=c(12,0))
for(i in 1:30){
  lines(seq(12,0,by=-0.5), MPL[i,3:27], type="s", col=i+1)
}
plot(seq(12,0,by=-0.5), CDG[31,3:27], type="s", main="Charles de Gaulle Airport (CDG)", xlab="Booking Horizon (months before departure)", ylab="Number of Passengers", xlim=c(12,0))
for(i in 1:30){
  lines(seq(12,0,by=-0.5), CDG[i,3:27], type="s", col=i+1)
}
plot(seq(12,0,by=-0.5), VLC[31,3:27], type="s", main="Valencia Airport (VLC)", xlab="Booking Horizon (months before departure)", ylab="Number of Passengers", xlim=c(12,0))
for(i in 1:30){
  lines(seq(12,0,by=-0.5), VLC[i,3:27], type="s", col=i+1)
}
plot(seq(12,0,by=-0.5), SVQ[31,3:27], type="s", main="Seville Airport (SVQ)", xlab="Booking Horizon (months before departure)", ylab="Number of Passengers", xlim=c(12,0))
for(i in 1:30){
  lines(seq(12,0,by=-0.5), SVQ[i,3:27], type="s", col=i+1)
}
plot(seq(12,0,by=-0.5), LPA[31,3:27], type="s", main="Gran Canaria Airport (LPA)", xlab="Booking Horizon (months before departure)", ylab="Number of Passengers", xlim=c(12,0))
for(i in 1:30){
  lines(seq(12,0,by=-0.5), LPA[i,3:27], type="s", col=i+1)
}
plot(seq(12,0,by=-0.5), BSL[31,3:27], type="s", main="Basel Airport (BSL)", xlab="Booking Horizon (months before departure)", ylab="Number of Passengers", xlim=c(12,0))
for(i in 1:30){
  lines(seq(12,0,by=-0.5), BSL[i,3:27], type="s", col=i+1)
}
par(mfrow = c(1,1))


#Calculate count data from cumulative data
findcounts <- function(data){
  dat <- as.numeric(data)
  return(c(dat[1],diff(dat)))
}
findcounts(ZAG[5,2:27])


#Test for homogeneity in total number of customers
#input as count data
#ignore last column as this contains negative counts (from no-shows)
hpphyptest <- function(data,alpha){
  q <- length(data)
  N <- data[q]
  if (N == 0){
    return(print("no bookings made"))
  }
  Ti <- 12
  TS <- sum(((data - (0.5/Ti)*N)^2)/((0.5/Ti)*N))
  if (TS > qchisq(1-alpha, df=q-1)){
    print("reject null hypothesis")
  }
}
hpphyptest(findcounts(ZAG[31,3:26]),0.05) #rejected
hpphyptest(findcounts(LHR[31,3:26]),0.05) #rejected
hpphyptest(findcounts(MPL[31,3:26]),0.05) #rejected
hpphyptest(findcounts(CDG[31,3:26]),0.05) #rejected 
hpphyptest(findcounts(VLC[31,3:26]),0.05) #rejected
hpphyptest(findcounts(SVQ[31,3:26]),0.05) #rejected
hpphyptest(findcounts(LPA[31,3:26]),0.05) #rejected
hpphyptest(findcounts(BSL[31,3:26]),0.05) #rejected


#Find changepoints to approximate by piecewise homogeneous Poisson process
cpdet(findcounts(ZAG[31,3:26]),0.05)
cpdet(findcounts(LHR[31,3:26]),0.05)
cpdet(findcounts(MPL[31,3:26]),0.05)
cpdet(findcounts(CDG[31,3:26]),0.05)
cpdet(findcounts(VLC[31,3:26]),0.05)
cpdet(findcounts(SVQ[31,3:26]),0.05)
cpdet(findcounts(LPA[31,3:26]),0.05) 
cpdet(findcounts(BSL[31,3:26]),0.05)


