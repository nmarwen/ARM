#How early in the booking horizon can an outlier be detected?
#Fare class Y
genuineEDY <- numeric(0)
fpratesEDY <- numeric(0)
for (i in 1:11){
  dat <- Ybookingsa[,2:(i+20)]
  k <- outlierdet1(dat,3)
  fpratesEDY <- c(fpratesEDY, fpratedists(k,501))
  genuineEDY <- c(genuineEDY, trueoutdet(k,501))
}
d <- data.frame(x = 11:1, FPRate = rev(fpratesEDY), gens = detornot(genuineEDY))
p <- ggplot(data = d, aes(x = x)) + geom_line(size=1,aes(y = FPRate, linetype = "False Positives")) + 
  geom_point(size=4,aes(y = FPRate,color=gens)) + scale_x_reverse(lim=c(30,0)) +
  scale_colour_manual("", values = c("Outlier Not Detected" = "black", "Outlier Detected" = "red")) + 
  labs(x = "Booking Horizon (DCPs before Departure)", y="Number of False Positives") + 
  scale_y_continuous(limits=c(0,80)) +theme(axis.text=element_text(size=14),axis.title=element_text(size=14),legend.text=element_text(size=14),plot.background = element_rect(fill = "transparent", color = NA),legend.background = element_rect(color = NA,fill="transparent"),legend.box.background = element_rect(fill = "transparent",color=NA),legend.position=c(0,1),legend.justification=c(0,1),legend.title=element_blank(),legend.key = element_blank())
p
ggsave(p, filename = "EarlyDetY.png",  bg = "transparent")

#Fare class M
genuineEDM <- numeric(0)
fpratesEDM <- numeric(0)
for (i in 1:BH){
  dat <- Mbookingsa[,2:(i+1)]
  k <- outlierdet1(dat,5)
  fpratesEDM <- c(fpratesEDM, fpratedists(k,501))
  genuineEDM <- c(genuineEDM, trueoutdet(k,501))
}
d <- data.frame(x = 30:1, FPRate = rev(fpratesEDM), gens = detornot(genuineEDM))
p <- ggplot(data = d, aes(x = x)) + geom_line(size=1,aes(y = FPRate, linetype = "False Positives")) + 
  geom_point(size=4,aes(y = FPRate,color=gens)) + scale_x_reverse(lim=c(30,0)) +
  scale_colour_manual("", values = c("Outlier Not Detected" = "black", "Outlier Detected" = "red")) + 
  labs(x = "Booking Horizon (DCPs before Departure)", y="Number of False Positives") + 
  scale_y_continuous(limits=c(0,80)) +theme(axis.text=element_text(size=14),axis.title=element_text(size=14),legend.text=element_text(size=14),plot.background = element_rect(fill = "transparent", color = NA),legend.background = element_rect(color = NA,fill="transparent"),legend.box.background = element_rect(fill = "transparent",color=NA),legend.position=c(0,1),legend.justification=c(0,1),legend.title=element_blank(),legend.key = element_blank())
p
ggsave(p, filename = "EarlyDetM.png",  bg = "transparent")

#Fare class K
genuineEDK <- numeric(0)
fpratesEDK <- numeric(0)
for (i in 1:BH){
  dat <- Kbookingsa[,2:(i+1)]
  k <- outlierdet1(dat,5)
  fpratesEDK <- c(fpratesEDK, fpratedists(k,501))
  genuineEDK <- c(genuineEDK, trueoutdet(k,501))
}
d <- data.frame(x = 30:1, FPRate = rev(fpratesEDK), gens = detornot(genuineEDK))
p <- ggplot(data = d, aes(x = x)) + geom_line(size=1,aes(y = FPRate, linetype = "False Positives")) + 
  geom_point(size=4,aes(y = FPRate,color=gens)) + scale_x_reverse(lim=c(30,0)) +
  scale_colour_manual("", values = c("Outlier Not Detected" = "black", "Outlier Detected" = "red")) + 
  labs(x = "Booking Horizon (DCPs before Departure)", y="Number of False Positives") + 
  scale_y_continuous(limits=c(0,80)) +theme(axis.text=element_text(size=14),axis.title=element_text(size=14),legend.text=element_text(size=14),plot.background = element_rect(fill = "transparent", color = NA),legend.background = element_rect(color = NA,fill="transparent"),legend.box.background = element_rect(fill = "transparent",color=NA),legend.position=c(0,1),legend.justification=c(0,1),legend.title=element_blank(),legend.key = element_blank())
p
ggsave(p, filename = "EarlyDetK.png",  bg = "transparent")

#Total Bookings
genuineEDTot <- numeric(0)
fpratesEDTot <- numeric(0)
for (i in 1:BH){
  dat <- Totbookingsa[,2:(i+1)]
  k <- outlierdet1(dat,5)
  fpratesEDTot <- c(fpratesEDTot, fpratedists(k,501))
  genuineEDTot <- c(genuineEDTot, trueoutdet(k,501))
}
d <- data.frame(x = 30:1, FPRate = rev(fpratesEDTot), gens = detornot(genuineEDTot))
p <- ggplot(data = d, aes(x = x)) + geom_line(size=1,aes(y = FPRate, linetype = "False Positives")) + 
  geom_point(size=4,aes(y = FPRate,color=gens)) + scale_x_reverse(lim=c(30,0)) +
  scale_colour_manual("", values = c("Outlier Not Detected" = "black", "Outlier Detected" = "red")) + 
  labs(x = "Booking Horizon (DCPs before Departure)", y="Number of False Positives") + 
  scale_y_continuous(limits=c(0,80)) +theme(axis.text=element_text(size=14),axis.title=element_text(size=14),legend.text=element_text(size=14),plot.background = element_rect(fill = "transparent", color = NA),legend.background = element_rect(color = NA,fill="transparent"),legend.box.background = element_rect(fill = "transparent",color=NA),legend.position=c(0,1),legend.justification=c(0,1),legend.title=element_blank(),legend.key = element_blank())
p
ggsave(p, filename = "EarlyDetTot.png",  bg = "transparent")

