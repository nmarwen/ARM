install.packages("TSclust")
library(TSclust)

#create a function which returns a vector of the sum of Euclidean distances for each time series
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
eucdistsums2(Ybookingsa[,2:31])


#create a function which calculates threshold based on k-means threshold
thresholdfunc <- function(dists){
  return(mean(dists)+3*sqrt(var(dists)))
}
threshold(eucdistsums(Ybookingsa[,2:31]))


#create a function which determines the outliers
eucdistoutlierdet <- function(data){
  dists <- eucdistsums2(data)
  return(which(dists >= thresholdfunc(dists)))
}
eucdistoutlierdet(Ybookingsa[,2:31])




##############################################################################################################################################################################################
##############################################################################################################################################################################################

#Apply algorithm to a single outlier resulting from type 1 increase
eucdistoutlierdet(Ybookingsa[,2:31])
eucdistoutlierdet(Mbookingsa[,2:31])
eucdistoutlierdet(Kbookingsa[,2:31])
eucdistoutlierdet(Totbookingsa[,2:31])


##############################################################################################################################################################################################
##############################################################################################################################################################################################

#Apply algorithm to a single outlier resulting from type 1 and type 2 increase
eucdistoutlierdet(Ybookingsb[,2:31])
eucdistoutlierdet(Mbookingsb[,2:31])
eucdistoutlierdet(Kbookingsb[,2:31])
eucdistoutlierdet(Totbookingsb[,2:31])


##############################################################################################################################################################################################
##############################################################################################################################################################################################


#Apply algorithm to multiple outliers from the same distribution
#Plot FP and Detection rates against number of outliers
multifpratesY <- numeric(0)
multigenuineY <- numeric(0)
multifpratesM <- numeric(0)
multigenuineM <- numeric(0)
multifpratesK <- numeric(0)
multigenuineK <- numeric(0)
multifpratesTot <- numeric(0)
multigenuineTot <- numeric(0)
for (i in 1:25){
  nplfsim(n=i,BH=30,mu=180,L=0.01,C=200,SLFs=c(1,0.45,0.25),segments1=c(10,10,10),rates1=c(40,50,80),segments2=c(15,10,5),rates2=c(15,50,60),p1=0.85,p2=0.65,s=12345,name="multi")
  multi <- read.csv("multi.csv", header=TRUE, stringsAsFactors = FALSE)
  Ybookingsm <- rbind(Ybookings,multi[seq(1, nrow(multi), 4),])
  Mbookingsm <- rbind(Mbookings,multi[seq(2, nrow(multi), 4),])
  Kbookingsm <- rbind(Kbookings,multi[seq(3, nrow(multi), 4),])
  Totbookingsm <-  rbind(Totbookings,multi[seq(4, nrow(multi), 4),])
  ky <- eucdistoutlierdet(Ybookingsm[,2:31])
  km <- eucdistoutlierdet(Mbookingsm[,2:31])
  kk <- eucdistoutlierdet(Kbookingsm[,2:31])
  ktot <- eucdistoutlierdet(Totbookingsm[,2:31])
  gens <- (501):(500+i)
  multifpratesY <- c(multifpratesY, multifprate(ky, gens, (500+i)))
  multigenuineY <- c(multigenuineY, multidetrate(ky, gens))
  multifpratesM <- c(multifpratesM, multifprate(km, gens, (500+i)))
  multigenuineM <- c(multigenuineM, multidetrate(km, gens))
  multifpratesK <- c(multifpratesK, multifprate(kk, gens, (500+i)))
  multigenuineK <- c(multigenuineK, multidetrate(kk, gens))
  multifpratesTot <- c(multifpratesTot, multifprate(ktot, gens, (500+i)))
  multigenuineTot <- c(multigenuineTot, multidetrate(ktot, gens))
}

d1 = data.frame(x = 1:25, GenRate = multigenuineTot, FPRate = multifpratesTot)
p1 <- ggplot(data = d1, aes(x = x)) + geom_line(size=1,aes(y = GenRate, linetype="Detection Rate")) + 
  geom_line(size=1,aes(y = FPRate*15, linetype = "False Positive Rate")) + 
  scale_colour_manual("", values = c("Detection Rate" = "black", "False Positive Rate" = "red")) + 
  labs(x = "Number of Outliers", y="% of Genuine Outliers Detected") + scale_y_continuous(sec.axis = sec_axis(~./15, name = "% of Observations False Positives")) + 
  theme(axis.text=element_text(size=14),axis.title=element_text(size=14),legend.text=element_text(size=14),plot.background = element_rect(fill = "transparent", color = NA),legend.background = element_rect(color = NA,fill="transparent"),legend.box.background = element_rect(fill = "transparent",color=NA),legend.position=c(0.67,1),legend.justification=c(0,1),legend.title=element_blank(),legend.key = element_blank())
p1
ggsave(p1, filename = "EucDistMulti1Tot.png",  bg = "transparent")



##############################################################################################################################################################################################
##############################################################################################################################################################################################


#Apply algorithm to multiple outliers from different distributions
multifpratesY <- numeric(0)
multigenuineY <- numeric(0)
multifpratesM <- numeric(0)
multigenuineM <- numeric(0)
multifpratesK <- numeric(0)
multigenuineK <- numeric(0)
multifpratesTot <- numeric(0)
multigenuineTot <- numeric(0)
for (i in 1:25){
  Ybookingsm <- Ybookingsd[1:(500+i),]
  Mbookingsm <- Mbookingsd[1:(500+i),]
  Kbookingsm <- Kbookingsd[1:(500+i),]
  Totbookingsm <-  Totbookingsd[1:(500+i),]
  ky <- eucdistoutlierdet(Ybookingsm[,2:31])
  km <- eucdistoutlierdet(Mbookingsm[,2:31])
  kk <- eucdistoutlierdet(Kbookingsm[,2:31])
  ktot <- eucdistoutlierdet(Totbookingsm[,2:31])
  gens <- (501):(500+i)
  multifpratesY <- c(multifpratesY, multifprate(ky, gens, (500+i)))
  multigenuineY <- c(multigenuineY, multidetrate(ky, gens))
  multifpratesM <- c(multifpratesM, multifprate(km, gens, (500+i)))
  multigenuineM <- c(multigenuineM, multidetrate(km, gens))
  multifpratesK <- c(multifpratesK, multifprate(kk, gens, (500+i)))
  multigenuineK <- c(multigenuineK, multidetrate(kk, gens))
  multifpratesTot <- c(multifpratesTot, multifprate(ktot, gens, (500+i)))
  multigenuineTot <- c(multigenuineTot, multidetrate(ktot, gens))
}

d1 = data.frame(x = 1:25, GenRate = multigenuineTot, FPRate = multifpratesTot)
p1 <- ggplot(data = d1, aes(x = x)) + geom_line(size=1,aes(y = GenRate, linetype="Detection Rate")) + 
  geom_line(size=1,aes(y = FPRate*15, linetype = "False Positive Rate")) + 
  scale_colour_manual("", values = c("Detection Rate" = "black", "False Positive Rate" = "red")) + 
  labs(x = "Number of Outliers", y="% of Genuine Outliers Detected") + scale_y_continuous(sec.axis = sec_axis(~./15, name = "% of Observations False Positives")) + 
  theme(axis.text=element_text(size=14),axis.title=element_text(size=14),legend.text=element_text(size=14),plot.background = element_rect(fill = "transparent", color = NA),legend.background = element_rect(color = NA,fill="transparent"),legend.box.background = element_rect(fill = "transparent",color=NA),legend.position=c(0.67,1),legend.justification=c(0,1),legend.title=element_blank(),legend.key = element_blank())
p1
ggsave(p1, filename = "EucDistMulti2Tot.png",  bg = "transparent")


##############################################################################################################################################################################################
##############################################################################################################################################################################################


#Retest on a single outlier but rerun 100 different times (100 different outliers)
detsy <- 0
fpsy <- 0
detsm <- 0
fpsm <- 0
detsk <- 0
fpsk <- 0
detstot <- 0
fpstot <- 0
for (i in 1:100){
  #simulate outlier
  nplfsim(n=1,BH=30,mu=180,L=0.01,C=200,SLFs=c(1,0.45,0.25),segments1=c(10,10,10),rates1=c(40,50,80),segments2=c(15,10,5),rates2=c(5,35,50),p1=0.85,p2=0.65,s=i,name="outtest")
  outtest <- read.csv("outtest.csv", header=TRUE, stringsAsFactors = FALSE)
  #join data to outlier and split by fare class
  daty <- rbind(Ybookings,outtest[1,])
  datm <- rbind(Mbookings,outtest[2,])
  datk <- rbind(Kbookings,outtest[3,])
  dattot <- daty[,2:31] + datm[,2:31] + datk[,2:31] 
  lab <- rep("Tot",501)
  dattot <- cbind(lab, dattot)
  #run outlier detection
  ky <- eucdistoutlierdet(daty[,2:31])
  km <- eucdistoutlierdet(datm[,2:31])
  kk <- eucdistoutlierdet(datk[,2:31])
  ktot <- eucdistoutlierdet(dattot[,2:31])
  #calculate detection rate and false positive rate 
  fpsy = fpsy + fpratedists(ky,501)
  detsy = detsy + trueoutdet(ky,501)
  fpsm = fpsm + fpratedists(km,501)
  detsm = detsm + trueoutdet(km,501)
  fpsk = fpsk + fpratedists(kk,501)
  detsk = detsk + trueoutdet(kk,501)
  fpstot = fpstot + fpratedists(ktot,501)
  detstot = detstot + trueoutdet(ktot,501)
}
fpsy <- fpsy/100 
fpsm <- fpsm/100
fpsk <- fpsk/100
fpstot <- fpstot/100

