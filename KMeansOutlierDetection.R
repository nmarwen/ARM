#Start by clustering repeatedly at each individual time point
#perform cluster analysis at each time point on bookings for each fare class
#choose the number of clusters for k-means clustering
set.seed(12345)
k.max <- 5
wss <- sapply(1:k.max,function(k){kmeans(Ybookingsa[5,2:31], k, nstart=50,iter.max = 15 )$tot.withinss})
plot(1:k.max, wss, type="b", pch = 19, frame = FALSE, xlab="Number of clusters K", ylab="Total within-clusters sum of squares")
#choose k = 2


#create an algorithm that clusters by k-means and detects outliers at a single time point
outlierdet1 <- function(data,k){
  if (length(unique(data)) <= k){
    return(NA)
  }
  dists <- sqrt(rowSums(data - fitted(kmeans(data, centers=k))) ^ 2)
  threshold <- (max(dists)+min(dists))/2
  output <- as.vector(which(dists >= threshold))
  return(output)
}


#define an algorithm that runs along each time point and returns the outliers
outlierdet <- function(data,k){
  n <- ncol(data)
  output <- list()
  #Ybookings[1,2:length(Ybookings)]
  #output <- list(outlierdet1(data[,2],k))
  for (i in 2:n){
    output[[i-1]] <- outlierdet1(data[,i],k)  #append list 
  }
  return(output)
}
outlierdet(Ybookingsa,2)


#Obtain a list of all identified as outliers at anypoint
uniqueoutlierdet <- function(data,k){
  j <- outlierdet(data,k)
  return(unlist(unique(j))[is.na(unlist(unique(j)))==F])
}
uniqueoutlierdet(Ybookingsa,2)


#input data is output from outlierdet function
#trueout is the artificial outlier
earlydettrue <- function(data,trueout){
  N <- length(data)
  for (i in 1:N){
    if (is.na(match(trueout,data[[i]])) == F){
      return(i)
    }
  }
  return(NA)
}
earlydettrue(outlierdet(Ybookingsa,2),501) 
earlydettrue(outlierdet(Ybookingsa,2),211)


#find outliers in fare classes and total bookings
k <- unique(uniqueoutlierdet(Ybookingsa,2))
length(k)
which(k == 501)
k <- unique(uniqueoutlierdet(Mbookingsa,2))
length(k)
which(k == 501)
k <- unique(uniqueoutlierdet(Kbookingsa,2))
length(k)
which(k == 501)
k <- unique(uniqueoutlierdet(Totbookingsa,2))
length(k)
which(k == 501)