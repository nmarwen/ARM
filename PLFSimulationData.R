#import simulation data from csv
simdat1 <- read.csv("sim02.csv", header=TRUE, stringsAsFactors = FALSE)
Ybookings <- simdat1[seq(1, nrow(simdat1), 4), ]
Mbookings <- simdat1[seq(2, nrow(simdat1), 4), ]
Kbookings <- simdat1[seq(3, nrow(simdat1), 4), ]
Totbookings <- simdat1[seq(4, nrow(simdat1), 4), ] 
#plot graphs in ggplot2
library(reshape)
d <- data.frame(x=30:1, y=t(getcumulativecounts(Totbookings)[,2:31]))
df <- melt(d, id = "x")
p <- ggplot(data = df, aes(x = x, y = value, color = variable)) + geom_step() + 
  labs(x = "Booking Horizon (DCPs before Departure)") + scale_x_reverse(lim=c(30,0)) + scale_y_continuous("Bookings", limits = c(0,200)) + 
  theme(axis.text=element_text(size=20),axis.title=element_text(size=20),legend.text=element_text(size=20),plot.background = element_rect(fill = "transparent", color = NA),legend.background = element_rect(color = NA,fill="transparent"),legend.box.background = element_rect(fill = "transparent",color=NA),legend.position="none",legend.justification=c(0,1),legend.title=element_blank(),legend.key = element_blank())
p
ggsave(p, filename = "SLFTot.png",  bg = "transparent")
#Plot with customer arrivals and customer bookings to look at censoring
set.seed(12345)
k <- plfsimulation(BH=30,mu=180,L=0.01,C=200,SLFs=c(1,0.45,0.25),segments1=c(10,10,10),rates1=c(20,30,40),segments2=c(15,10,5),rates2=c(5,35,50),p1=0.85,p2=0.65)
d <- data.frame(x = 30:1, y1 = cumsum(k$Demand), y2 = cumsum(k$Total))
p <- ggplot(data = d, aes(x = x)) + geom_step(size=1,aes(y = y1, colour = "Actual Demand")) + 
  geom_step(size=1,aes(y = y2, colour = "Censored Demand")) + 
  scale_colour_manual("", values = c("Actual Demand"="red", "Censored Demand"="blue"), breaks = c("Actual Demand","Censored Demand")) + 
  labs(x = "Booking Horizon (DCPs before Departure)") + scale_x_reverse(lim=c(30,0)) + 
  scale_y_continuous("Cumulative Demand", limits = c(0,200)) + 
  theme(axis.text=element_text(size=14),axis.title=element_text(size=14),legend.text=element_text(size=14),plot.background = element_rect(fill = "transparent", color = NA),legend.background = element_rect(color = NA,fill="transparent"),legend.box.background = element_rect(fill = "transparent",color=NA),legend.position=c(0,1),legend.justification=c(0,1),legend.title=element_blank(),legend.key = element_blank())
p
ggsave(p, filename = "Censoring.png",  bg = "transparent")
#Plot bookings and closure times for a single run with class closure times
d <- data.frame(x = 30:1, y1 = cumsum(k$Y), y2 = cumsum(k$M), y3 = cumsum(k$K), y4 = cumsum(k$Total))
p <- ggplot(data = d, aes(x = x)) + geom_step(size=1,aes(y = y1, colour = "Fare Class Y")) + 
  geom_step(size=1,aes(y = y2, colour = "Fare Class M")) + geom_step(size=1,aes(y = y3, colour = "Fare Class K")) + 
  geom_step(size=1,aes(y = y4, colour = "Total Bookings")) + 
  annotate("text", x = c(12.5,5.8), y = c(150,150), label = c("K Closes","M Closes"), size=5) + 
  geom_vline(xintercept=c(14.5,7.96)) + 
  scale_colour_manual("", values = c("Fare Class Y"="red", "Fare Class M" = "green", "Fare Class K"="blue", "Total Bookings"="black")) + 
  labs(x = "Booking Horizon (DCPs before Departure)") + scale_x_reverse(lim=c(30,0)) + scale_y_continuous("Bookings", limits = c(0,200)) + 
  theme(axis.text=element_text(size=14),axis.title=element_text(size=14),legend.text=element_text(size=14),plot.background = element_rect(fill = "transparent", color = NA),legend.background = element_rect(color = NA,fill="transparent"),legend.box.background = element_rect(fill = "transparent",color=NA),legend.position=c(0,1),legend.justification=c(0,1),legend.title=element_blank(),legend.key = element_blank())
p
ggsave(p, filename = "Bookings.png",  bg = "transparent")

