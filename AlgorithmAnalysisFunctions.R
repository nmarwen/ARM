#Usefule functions for analysing the performance of an algorithm

#Detection rate for a single outlier (detected (1) or not(0))
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

#Function for detection rate as % of number of outliers
multidetrate <- function(outs, trueouts){
  ((length(trueouts) - sum(is.na(match(trueouts,outs))))/length(trueouts))
}


##############################################################################################################################################################################################
##############################################################################################################################################################################################


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

#calculate rate of false positives of euclidean distance algorithm
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

#Consider the effect of multiple outliers
#Function for FP rate as % of number of total observations
multifprate <- function(outs, trueouts, n){
  return(sum(is.na(match(trueouts,outs)))/n) ##returns number of outlier which are not genuine
}


##############################################################################################################################################################################################
##############################################################################################################################################################################################


#create a function which times how long it takes to run a function
timerfunc <- function(func){
  start.time <- Sys.time()
  func
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  return(time.taken)
}
timerfunc(eucdistsums(Ybookings)) #50.36682 secs
