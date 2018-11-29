#Sampling Frequency Analysis

#############################################################################################################################################################################
#############################################################################################################################################################################

#Euclidean Distance Outlier Detection
#Fare Class Y
times1 <- numeric(0)
fprates1 <- numeric(0)
genuine1 <- numeric(0)
sampfreqs <- c(1,2,3,5,6,10)
for (i in 1:6){
  f <- sampfreqs[i]
  df <- Ybookingsa[,2:31]
  dat <- sapply(seq(1,29,by=f),function(j) rowSums(df[,j:(j+1)]))
  times1 <- c(times1, timerfunc(eucdistoutlierdet(dat)))
  k <- eucdistoutlierdet(dat)
  fprates1 <- c(fprates1, fpratedists(k,501))
  genuine1 <- c(genuine1, trueoutdet(k,501))
}
#Fare Class M
times2 <- numeric(0)
fprates2 <- numeric(0)
genuine2 <- numeric(0)
for (i in 1:6){
  f <- sampfreqs[i]
  df <- Mbookingsa[,2:31]
  dat <- sapply(seq(1,29,by=f),function(j) rowSums(df[,j:(j+1)]))
  times2 <- c(times2, timerfunc(eucdistoutlierdet(dat)))
  k <- eucdistoutlierdet(dat)
  fprates2 <- c(fprates2, fpratedists(k,501))
  genuine2 <- c(genuine2, trueoutdet(k,501))
}
#Fare Class K
times3 <- numeric(0)
fprates3 <- numeric(0)
genuine3 <- numeric(0)
for (i in 1:6){
  f <- sampfreqs[i]
  df <- Kbookingsa[,2:31]
  dat <- sapply(seq(1,29,by=f),function(j) rowSums(df[,j:(j+1)]))
  times3 <- c(times3, timerfunc(eucdistoutlierdet(dat)))
  k <- eucdistoutlierdet(dat)
  fprates3 <- c(fprates3, fpratedists(k,501))
  genuine3 <- c(genuine3, trueoutdet(k,501))
}
#Total Bookings
times4 <- numeric(0)
fprates4 <- numeric(0)
genuine4 <- numeric(0)
for (i in 1:6){
  f <- sampfreqs[i]
  df <- Totbookingsa[,2:31]
  dat <- sapply(seq(1,29,by=f),function(j) rowSums(df[,j:(j+1)]))
  times4 <- c(times4, timerfunc(eucdistoutlierdet(dat)))
  k <- eucdistoutlierdet(dat)
  fprates4 <- c(fprates4, fpratedists(k,501))
  genuine4 <- c(genuine4, trueoutdet(k,501))
}

#Make ggplot2 plots
d1 = data.frame(x = sampfreqs, Times = times1, FPRate = fprates1, gens = detornot(genuine1))
p1 <- ggplot(data = d1, aes(x = x)) + geom_line(size=1,aes(y = Times, linetype="Time (s)")) + geom_line(size=1,aes(y = FPRate/25, linetype = "False Positives")) + geom_point(size=4,aes(y = FPRate/25,color=gens)) + scale_colour_manual("", values = c("Outlier Not Detected" = "black", "Outlier Detected" = "red", "Time (s)"="red")) + labs(x = "Sampling Frequency", y="Time (s)") + scale_y_continuous(sec.axis = sec_axis(~.*25, name = "Number of False Positives"),limits=c(0,0.8)) +theme(axis.text=element_text(size=14),axis.title=element_text(size=14),legend.text=element_text(size=14),plot.background = element_rect(fill = "transparent", color = NA),legend.background = element_rect(color = NA,fill="transparent"),legend.box.background = element_rect(fill = "transparent",color=NA),legend.position=c(0,1),legend.justification=c(0,1),legend.title=element_blank(),legend.key = element_blank())
ggsave(p1, filename = "SampFreqY.png",  bg = "transparent")

d2 = data.frame(x = sampfreqs, Times = times2, FPRate = fprates2, gens = detornot(genuine2))
p2 <- ggplot(data = d2, aes(x = x)) + geom_line(size=1,aes(y = Times, linetype="Time (s)")) + geom_line(size=1,aes(y = FPRate/25, linetype = "False Positives")) + geom_point(size=4,aes(y = FPRate/25,color=gens)) + scale_colour_manual("", values = c("Outlier Not Detected" = "black", "Outlier Detected" = "red", "Time (s)"="red")) + labs(x = "Sampling Frequency", y="Time (s)") + scale_y_continuous(sec.axis = sec_axis(~.*25, name = "Number of False Positives"),limits=c(0,0.8)) +theme(axis.text=element_text(size=14),axis.title=element_text(size=14),legend.text=element_text(size=14),plot.background = element_rect(fill = "transparent", color = NA),legend.background = element_rect(color = NA,fill="transparent"),legend.box.background = element_rect(fill = "transparent",color=NA),legend.position=c(0,1),legend.justification=c(0,1),legend.title=element_blank(),legend.key = element_blank())
ggsave(p2, filename = "SampFreqM.png",  bg = "transparent")

d3 = data.frame(x = sampfreqs, Times = times3, FPRate = fprates3, gens = detornot(genuine3))
p3 <- ggplot(data = d3, aes(x = x)) + geom_line(size=1,aes(y = Times, linetype="Time (s)")) + geom_line(size=1,aes(y = FPRate/25, linetype = "False Positives")) + geom_point(size=4,aes(y = FPRate/25,color=gens)) + scale_colour_manual("", values = c("Outlier Not Detected" = "black", "Outlier Detected" = "red", "Time (s)"="red")) + labs(x = "Sampling Frequency", y="Time (s)") + scale_y_continuous(sec.axis = sec_axis(~.*25, name = "Number of False Positives"),limits=c(0,0.8)) +theme(axis.text=element_text(size=14),axis.title=element_text(size=14),legend.text=element_text(size=14),plot.background = element_rect(fill = "transparent", color = NA),legend.background = element_rect(color = NA,fill="transparent"),legend.box.background = element_rect(fill = "transparent",color=NA),legend.position=c(0,1),legend.justification=c(0,1),legend.title=element_blank(),legend.key = element_blank())
ggsave(p3, filename = "SampFreqK.png",  bg = "transparent")

d4 = data.frame(x = sampfreqs, Times = times4, FPRate = fprates4, gens = detornot(genuine4))
p4 <- ggplot(data = d4, aes(x = x)) + geom_line(size=1,aes(y = Times, linetype="Time (s)")) + geom_line(size=1,aes(y = FPRate/25, linetype = "False Positives")) + geom_point(size=4,aes(y = FPRate/25,color=gens)) + scale_colour_manual("", values = c("Outlier Not Detected" = "black", "Outlier Detected" = "red", "Time (s)"="red")) + labs(x = "Sampling Frequency", y="Time (s)") + scale_y_continuous(sec.axis = sec_axis(~.*25, name = "Number of False Positives"),limits=c(0,0.8)) +theme(axis.text=element_text(size=14),axis.title=element_text(size=14),legend.text=element_text(size=14),plot.background = element_rect(fill = "transparent", color = NA),legend.background = element_rect(color = NA,fill="transparent"),legend.box.background = element_rect(fill = "transparent",color=NA),legend.position=c(0,1),legend.justification=c(0,1),legend.title=element_blank(),legend.key = element_blank())
ggsave(p4, filename = "SampFreqTot.png",  bg = "transparent")


#############################################################################################################################################################################
#############################################################################################################################################################################


#K-Means Clustering
#Fare Class Y
times1 <- numeric(0)
fprates1 <- numeric(0)
genuine1 <- numeric(0)
sampfreqs <- c(1,2,3,5,6,10)
for (i in 1:6){
  f <- sampfreqs[i]
  df <- Ybookingsa[,2:31]
  dat <- sapply(seq(1,29,by=f),function(i) rowSums(df[,i:(i+1)]))
  times1 <- c(times1, timerfunc(outlierdet1(dat,5)))
  k <- outlierdet1(dat,5)
  fprates1 <- c(fprates1, fpratedists(k,501))
  genuine1 <- c(genuine1, trueoutdet(k,501))
}

#Fare Class M
times2 <- numeric(0)
fprates2 <- numeric(0)
genuine2 <- numeric(0)
for (i in 1:6){
  f <- sampfreqs[i]
  df <- Mbookingsa[,2:31]
  dat <- sapply(seq(1,29,by=f),function(i) rowSums(df[,i:(i+1)]))
  times2 <- c(times2, timerfunc(outlierdet1(dat,5)))
  k <- outlierdet1(dat,5)
  fprates2 <- c(fprates2, fpratedists(k,501))
  genuine2 <- c(genuine2, trueoutdet(k,501))
}
#Fare Class K
times3 <- numeric(0)
fprates3 <- numeric(0)
genuine3 <- numeric(0)
for (i in 1:6){
  f <- sampfreqs[i]
  df <- Kbookingsa[,2:31]
  dat <- sapply(seq(1,29,by=f),function(j) rowSums(df[,j:(j+1)]))
  times3 <- c(times3, timerfunc(outlierdet1(dat,5)))
  k <- outlierdet1(dat,5)
  fprates3 <- c(fprates3, fpratedists(k,501))
  genuine3 <- c(genuine3, trueoutdet(k,501))
}
#Total Bookings
times4 <- numeric(0)
fprates4 <- numeric(0)
genuine4 <- numeric(0)
for (i in 1:6){
  f <- sampfreqs[i]
  df <- Totbookingsa[,2:31]
  dat <- sapply(seq(1,29,by=f),function(i) rowSums(df[,i:(i+1)]))
  times4 <- c(times4, timerfunc(outlierdet1(dat,5)))
  k <- outlierdet1(dat,5)
  fprates4 <- c(fprates4, fpratedists(k,501))
  genuine4 <- c(genuine4, trueoutdet(k,501))
}

#plot results
d1 = data.frame(x = sampfreqs, Times = times1, FPRate = fprates1, gens = detornot(genuine1))
p1 <- ggplot(data = d1, aes(x = x)) + geom_line(size=1,aes(y = Times, linetype="Time (s)")) + geom_line(size=1,aes(y = FPRate/100, linetype = "False Positives")) + geom_point(size=4,aes(y = FPRate/100,color=gens)) + scale_colour_manual("", values = c("Outlier Not Detected" = "black", "Outlier Detected" = "red", "Time (s)"="red")) + labs(x = "Sampling Frequency", y="Time (s)") + scale_y_continuous(sec.axis = sec_axis(~.*100, name = "Number of False Positives"),limits=c(0,1.5)) +theme(axis.text=element_text(size=14),axis.title=element_text(size=14),legend.text=element_text(size=14),plot.background = element_rect(fill = "transparent", color = NA),legend.background = element_rect(color = NA,fill="transparent"),legend.box.background = element_rect(fill = "transparent",color=NA),legend.position=c(0,1),legend.justification=c(0,1),legend.title=element_blank(),legend.key = element_blank())
ggsave(p1, filename = "KMeansSampFreqY.png",  bg = "transparent")

d2 = data.frame(x = sampfreqs, Times = times2, FPRate = fprates2, gens = detornot(genuine2))
p2 <- ggplot(data = d2, aes(x = x)) + geom_line(size=1,aes(y = Times, linetype="Time (s)")) + geom_line(size=1,aes(y = FPRate/100, linetype = "False Positives")) + geom_point(size=4,aes(y = FPRate/100,color=gens)) + scale_colour_manual("", values = c("Outlier Not Detected" = "black", "Outlier Detected" = "red", "Time (s)"="red")) + labs(x = "Sampling Frequency", y="Time (s)") + scale_y_continuous(sec.axis = sec_axis(~.*100, name = "Number of False Positives"),limits=c(0,1.5)) +theme(axis.text=element_text(size=14),axis.title=element_text(size=14),legend.text=element_text(size=14),plot.background = element_rect(fill = "transparent", color = NA),legend.background = element_rect(color = NA,fill="transparent"),legend.box.background = element_rect(fill = "transparent",color=NA),legend.position=c(0,1),legend.justification=c(0,1),legend.title=element_blank(),legend.key = element_blank())
ggsave(p2, filename = "KMeansSampFreqM.png",  bg = "transparent")

d3 = data.frame(x = sampfreqs, Times = times3, FPRate = fprates3, gens = detornot(genuine3))
p3 <- ggplot(data = d3, aes(x = x)) + geom_line(size=1,aes(y = Times, linetype="Time (s)")) + geom_line(size=1,aes(y = FPRate/100, linetype = "False Positives")) + geom_point(size=4,aes(y = FPRate/100,color=gens)) + scale_colour_manual("", values = c("Outlier Not Detected" = "black", "Outlier Detected" = "red", "Time (s)"="red")) + labs(x = "Sampling Frequency", y="Time (s)") + scale_y_continuous(sec.axis = sec_axis(~.*100, name = "Number of False Positives"),limits=c(0,1.5)) +theme(axis.text=element_text(size=14),axis.title=element_text(size=14),legend.text=element_text(size=14),plot.background = element_rect(fill = "transparent", color = NA),legend.background = element_rect(color = NA,fill="transparent"),legend.box.background = element_rect(fill = "transparent",color=NA),legend.position=c(0,1),legend.justification=c(0,1),legend.title=element_blank(),legend.key = element_blank())
ggsave(p3, filename = "KMeansSampFreqK.png",  bg = "transparent")

d4 = data.frame(x = sampfreqs, Times = times4, FPRate = fprates4, gens = detornot(genuine4))
p4 <- ggplot(data = d4, aes(x = x)) + geom_line(size=1,aes(y = Times, linetype="Time (s)")) + geom_line(size=1,aes(y = FPRate/100, linetype = "False Positives")) + geom_point(size=4,aes(y = FPRate/100,color=gens)) + scale_colour_manual("", values = c("Outlier Not Detected" = "black", "Outlier Detected" = "red", "Time (s)"="red")) + labs(x = "Sampling Frequency", y="Time (s)") + scale_y_continuous(sec.axis = sec_axis(~.*100, name = "Number of False Positives"),limits=c(0,1.5)) +theme(axis.text=element_text(size=14),axis.title=element_text(size=14),legend.text=element_text(size=14),plot.background = element_rect(fill = "transparent", color = NA),legend.background = element_rect(color = NA,fill="transparent"),legend.box.background = element_rect(fill = "transparent",color=NA),legend.position=c(0,1),legend.justification=c(0,1),legend.title=element_blank(),legend.key = element_blank())
ggsave(p4, filename = "KMeansSampFreqTot.png",  bg = "transparent")
