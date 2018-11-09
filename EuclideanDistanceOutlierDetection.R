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
  return((max(dists)-min(dists))/2)
}
threshold(eucdistsums(Ybookingsa))


#create a function which determines the outliers
eucdistoutlierdet <- function(data){
  dists <- eucdistsums(data)
  return(which(dists >= thresholdfunc(dists)))
}
eucdistoutlierdet(Ybookingsa)


#create a function which times how long it takes to run a function
timerfunc <- function(func,args){
  start.time <- Sys.time()
  func(args)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  return(time.taken)
}
timerfunc(eucdistsums,Ybookings) #50.36682 secs


#calculate rate of false positives
fprate <- function(data,trueout){
  outs <- eucdistoutlierdet(data)
  if (is.na(match(trueout,outs)) == T){
    return(length(outs))
  }
  else {
    return(length(outs)-1)
  }
}
fprate(Ybookings,501)


#check whether geneuine outlier detected (1) or not(0)
trueoutdet <- function(outs,trueout){
  if (is.na(match(trueout,outs)) == T){
    return(0)
  }
  else {
    return(1)
  }
}
outs1 <- eucdistoutlierdet(Ybookingsa)
trueoutdet(outs1,501)


#create a function which determines the outliers based on a specified threshold
#where input is the pre-calculated distances rather than raw count data
eucdistoutlierdetthresh <- function(dists,threshold){
  return(which(dists >= threshold))
}
dists1 <- eucdistsums(Ybookingsa)
t1 <- thresholdfunc(dists1)
eucdistoutlierdetthresh(dists1,t1) 

#define function which returns FP rate based on list of outliers
fpratedists <- function(outs,trueout){
  if ((length(which(outs==trueout))) == 0){
    return(length(outs))
  }
  else {
    return(length(outs)-1)
  }
}
outs1 <- eucdistoutlierdetthresh(dists1,t1)
fpratedists(outs1,501)






