#Investigate different versions of Euclidean distance algorithm in terms of computational time

#Calculate Euclidean distance using TSClust package
library(TSclust)
eucdistsums <- function(data){
  data <- as.matrix(data)
  N <- nrow(data)
  A <- matrix(rep(0, len=N^2), nrow = N)
  for (i in 1:N){
    for (j in 1:N){
      if (i < j){
        A[i,j] <- diss.EUCL(as.numeric(data[i,]), as.numeric(data[j,]))
      }
    }
  }
  A <- A + t(A)
  return(colSums(A))
}
eucdistsums(Ybookingsa[,2:31])


#Calculate Euclidean distance 
eucdistsums2 <- function(data){
  data <- as.matrix(data)
  N <- nrow(data)
  A <- matrix(rep(0, len=N^2), nrow = N)
  for (i in 1:N){
    for (j in 1:N){
      if (i < j){
        A[i,j] <- sqrt(sum(((as.numeric(data[i,]) - as.numeric(data[j,]))^2)))
      }
    }
  }
  A <- A + t(A)
  return(colSums(A))
}
eucdistsums2(Ybookingsa[,2:31])


#Calculate squared Euclidean distance 
eucdistsquaredsums <- function(data){
  data <- as.matrix(data)
  N <- nrow(data)
  A <- matrix(rep(0, len=N^2), nrow = N)
  for (i in 1:N){
    for (j in 1:N){
      if (i < j){
        A[i,j] <- sum(((as.numeric(data[i,]) - as.numeric(data[j,]))^2))
      }
    }
  }
  A <- A + t(A)
  return(colSums(A))
}
eucdistsquaredsums(Ybookingsa[,2:31])


######################################################################################################################################################
######################################################################################################################################################


#create a matrix of size 1000x1000
A <- matrix(rpois(1000000,0.4), ncol=1000)
#Time each distance function based on size of data set (100-1000)
nsizes <- seq(100,1000,by=100)
timesED1 <- numeric(0)
timesED2 <- numeric(0)
timesED3 <- numeric(0)
for (i in 1:length(nsizes)){
  timesED1 <- c(timesED1, timerfunc(eucdistsums(A[1:nsizes[i],1:nsizes[i]])))
}
for (i in 1:length(nsizes)){
  timesED2 <- c(timesED2, timerfunc(eucdistsums2(A[1:nsizes[i],1:nsizes[i]])))
}
for (i in 1:length(nsizes)){
  timesED3 <- c(timesED3, timerfunc(eucdistsquaredsums(A[1:nsizes[i],1:nsizes[i]])))
}
#plot results
d <- data.frame(x = nsizes, y1 = timesED1, y2 = timesED2, y3 = timesED3)
p <- ggplot(data = d, aes(x = x)) + geom_line(size=1,aes(y = y1, colour = "TSClust Package Euclidean Distance")) + 
  geom_line(size=1,aes(y = y2, colour = "Euclidean Distance")) + geom_line(size=1,aes(y = y3, colour = "Squared Euclidean Distance")) + 
  scale_colour_manual("", values = c("TSClust Package Euclidean Distance"="red", "Euclidean Distance"="blue", "Squared Euclidean Distance" = "purple")) + 
  labs(x = "Size of Distance Matrix", y="Time to Compute Distances (s)") + 
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22),legend.text=element_text(size=22),plot.background = element_rect(fill = "transparent", color = NA),legend.background = element_rect(color = NA,fill="transparent"),legend.box.background = element_rect(fill = "transparent",color=NA),legend.position=c(0,1),legend.justification=c(0,1),legend.title=element_blank(),legend.key = element_blank())
p
ggsave(p, filename = "CompTimeEucDist.png",  bg = "transparent")

