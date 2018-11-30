#Threshold Analysis
#Euclidean Distance

#create a function which determines the outliers based on a specified threshold
#where input is the pre-calculated distances rather than raw count data
eucdistoutlierdetthresh <- function(dists,threshold){
  return(which(dists >= threshold))
}

#Look at FP rate and detection rate
th <- seq(2000,10000,50)
fpth1 <- numeric(length=length(th))
genuineth1 <- numeric(length=length(th))
dists1 <- eucdistsums(Ybookingsa[,2:31])
deft1 <- mean(dists1)+3*sqrt(var(dists1))
for (i in 1:(length(th))){
  k <- eucdistoutlierdetthresh(dists1,th[i]) 
  fpth1[i] <- fpratedists(k, 501)
  genuineth1[i] <- trueoutdet(k,501)
}
fpth2 <- numeric(length=length(th))
genuineth2 <- numeric(length=length(th))
dists2 <- eucdistsums(Mbookingsa[,2:31])
deft2 <- mean(dists2)+3*sqrt(var(dists2))
for (i in 1:(length(th))){
  k <- eucdistoutlierdetthresh(dists2,th[i]) 
  fpth2[i] <- fpratedists(k, 501)
  genuineth2[i] <- trueoutdet(k,501)
}
fpth3 <- numeric(length=length(th))
genuineth3 <- numeric(length=length(th))
dists3 <- eucdistsums(Kbookingsa[,2:31])
deft3 <- mean(dists3)+3*sqrt(var(dists3))
for (i in 1:(length(th))){
  k <- eucdistoutlierdetthresh(dists3,th[i]) 
  fpth3[i] <- fpratedists(k, 501)
  genuineth3[i] <- trueoutdet(k,501)
}
fpth4 <- numeric(length=length(th))
genuineth4 <- numeric(length=length(th))
dists4 <- eucdistsums(Totbookingsa[,2:31])
deft4 <- mean(dists4)+3*sqrt(var(dists4))
for (i in 1:(length(th))){
  k <- eucdistoutlierdetthresh(dists4,th[i]) 
  fpth4[i] <- fpratedists(k, 501)
  genuineth4[i] <- trueoutdet(k,501)
}
d1 = data.frame(x = th, FPRate = fpth3, gens = detornot(genuineth3))
p1 <- ggplot(data = d1, aes(x = x)) + geom_line(size=1,aes(y = FPRate)) + 
  geom_point(size=2,aes(y = FPRate, color=gens)) + 
  annotate("text", x = c(4750), y = c(500), label = c("Default Threshold"), size=5) + 
  geom_vline(xintercept=c(deft3)) + 
  scale_colour_manual("", values = c("Outlier Not Detected" = "black", "Outlier Detected" = "red")) + 
  labs(x = "Threshold", y="Number of False Positives") + scale_y_continuous(limits=c(0,500)) + 
  theme(axis.text=element_text(size=14),axis.title=element_text(size=14),legend.text=element_text(size=14),plot.background = element_rect(fill = "transparent", color = NA),legend.background = element_rect(color = NA,fill="transparent"),legend.box.background = element_rect(fill = "transparent",color=NA),legend.position=c(0.7,1),legend.justification=c(0,1),legend.title=element_blank(),legend.key = element_blank())
p1
ggsave(p1, filename = "EDThresholdK.png",  bg = "transparent")



####################################################################################################################################################################
####################################################################################################################################################################


#K-Means (single outlier of type 1 increase)
#algorithm which allows a chnage of threshold as an input
outlierdet1t <- function(data,k,t){
  if (length(unique(data)) <= k){
    return(NA)
  }
  dists <- sqrt(rowSums(data - fitted(kmeans(data, centers=k))) ^ 2)
  output <- as.vector(which(dists >= t))
  return(list(output,dists))
}

#test different thresholds and store results
th <- seq(0,50,0.5)
fpth1 <- numeric(length=length(th))
genuineth1 <- numeric(length=length(th))
for (i in 1:(length(th))){
  k <- outlierdet1t(Ybookingsa[,2:31],5,th[i]) 
  fpth1[i] <- fpratedists(k[[1]], 501)
  genuineth1[i] <- trueoutdet(k[[1]],501)
  deft1 <- 0.5*(max(k[[2]]) - min(k[[2]]))
}
fpth2 <- numeric(length=length(th))
genuineth2 <- numeric(length=length(th))
for (i in 1:(length(th))){
  k <- outlierdet1t(Mbookingsa[,2:31],5,th[i]) 
  fpth2[i] <- fpratedists(k[[1]], 501)
  genuineth2[i] <- trueoutdet(k[[1]],501)
  deft2 <- 0.5*(max(k[[2]]) - min(k[[2]]))
}
fpth3 <- numeric(length=length(th))
genuineth3 <- numeric(length=length(th))
for (i in 1:(length(th))){
  k <- outlierdet1t(Kbookingsa[,2:31],5,th[i]) 
  fpth3[i] <- fpratedists(k[[1]], 501)
  genuineth3[i] <- trueoutdet(k[[1]],501)
  deft3 <- 0.5*(max(k[[2]]) - min(k[[2]]))
}
fpth4 <- numeric(length=length(th))
genuineth4 <- numeric(length=length(th))
for (i in 1:(length(th))){
  k <- outlierdet1t(Totbookingsa[,2:31],5,th[i])
  fpth4[i] <- fpratedists(k[[1]], 501)
  genuineth4[i] <- trueoutdet(k[[1]],501)
  deft4 <- 0.5*(max(k[[2]]) - min(k[[2]]))
}

d1 = data.frame(x = th, FPRate = fpth4, gens = detornot(genuineth4))
p1 <- ggplot(data = d1, aes(x = x)) + geom_line(size=1,aes(y = FPRate)) + 
  geom_point(size=2,aes(y = FPRate, color=gens)) + 
  annotate("text", x = c(13), y = c(500), label = c("Default Threshold"), size=5) + 
  geom_vline(xintercept=c(deft4)) + 
  scale_colour_manual("", values = c("Outlier Not Detected" = "black", "Outlier Detected" = "red")) + 
  labs(x = "Threshold", y="Number of False Positives") + scale_y_continuous(limits=c(0,500)) + 
  theme(axis.text=element_text(size=14),axis.title=element_text(size=14),legend.text=element_text(size=14),plot.background = element_rect(fill = "transparent", color = NA),legend.background = element_rect(color = NA,fill="transparent"),legend.box.background = element_rect(fill = "transparent",color=NA),legend.position=c(0.7,1),legend.justification=c(0,1),legend.title=element_blank(),legend.key = element_blank())
p1
ggsave(p1, filename = "KMThresholdTot.png",  bg = "transparent")
