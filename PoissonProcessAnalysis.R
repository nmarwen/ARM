#Find changepoints for a piecewise homogeneous Poisson process
#based on Antoch and Jaruskova hypothesis testing
#data input is individual count data (not cumulative)
#assume all segment lengths are 1
#calculate test statistic
teststatq <- function(data, q){
  dat <- data[1:q]
  N <- sum(dat)
  Ti <- length(dat)
  if (N > 0){
    return(sum(((dat - (1/Ti)*N)^2)/((1/Ti)*N)))
  }
  else {
    return(0)
  }
}


#perform hypothesis test
#TRUE means null hypothesis is rejected
htestq <- function(data,q,alpha){
  return(teststatq(data,q) > qchisq(1-alpha, df=q-1))
}
htestq(aggBH(arrivals(30,0.05,180),30),30,0.05)


#find the first changepoint then stop looking
cpdet1 <- function(data,alpha){
  for (i in 1:length(data)){
    if (htestq(data,i,alpha) == TRUE){
      return(i)
    }
  }
  return(0)
}
cpdet1(hpparrivals,0.05) #returns 0
cpdet1(shpparrivals,0.05) #returns 15


#find multiple changepoints
#output is list of the endpoints of the segments
cpdet <- function(data,alpha){
  n <- length(data)
  cps <- numeric(0)
  #find the first change point
  while (cpdet1(data,alpha) != 0){
    cps <- c(cps,cpdet1(data,alpha)-1)
    data <- data[cpdet1(data,alpha):length(data)]
  }
  return(cumsum(cps))
}
cpdet(shpparrivals,0.05)
cpdet(findcounts(ZAG[31,3:26]),0.05)


#find estimates of the means given the changepoints
#input data is count data rather than cumulative
#input cps is a list of the endpoints of the segments
lambdacps <- function(data,cps){
  n <- length(data)
  k <- length(cps)
  lambdas <- mean(data[1:cps[1]])
  if (k > 1){
    for (i in 2:k){
      lambdas <- c(lambdas,mean(data[(cps[i-1]+1):cps[i]]))
    }
  }
  lambdas <- c(lambdas, mean(data[(cps[k]+1):n]))
  return(lambdas)
}
cps1 <- cpdet(shpparrivals,0.05) 
lambdacps(shpparrivals,cps1) 


#also investigate using changepoint package in R
install.packages("changepoint")
library(changepoint)
cpt.mean(shpparrivals, method="PELT") 
