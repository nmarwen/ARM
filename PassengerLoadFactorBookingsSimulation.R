plfsimulation <- function(BH,mu,L,C,SLFs,segments1,rates1,segments2,rates2,p1,p2){
  #Check function argument inputs are valid
  if (sum(segments1) != BH){
    print("Segments 1 do not sum to length of booking horizon")
  }
  if (sum(segments2) != BH){
    print("Segments 2 do not sum to length of booking horizon")
  }
  #Generate type 1 customer arrivals
  Cust1 <- pharrivals(BH,L,segments1,rates1) 
  #Generate type 2 customer arrivals
  Cust2 <- pharrivals(BH,L,segments2,rates2) 
  #Accept or reject bookings based on PLF
  N <- length(Cust1)
  Y <- numeric(N)
  M <- numeric(N)
  K <- numeric(N)
  for (i in 1:N){
    #Cust1 arrivals
    if ((sum(c(Y,M,K)) + Cust1[i]) < SLFs[2]*C){ 
      M[i] <- Cust1[i]
    }
    else if ((sum(c(Y,M,K)) < SLFs[2]*C)){
      M[i] <- SLFs[2]*C - sum(c(Y,M,K))
      #print(c("M",i))
    }
    else if ((sum(c(Y,M,K)) + Cust1[i]) < SLFs[1]*C){
      Y[i] <- round(Cust1[i]*p1)
    }
    else if ((sum(c(Y,M,K)) < SLFs[1]*C)){
      Y[i] <- round((SLFs[1]*C - sum(c(Y,M,K)))*p1)
    }
    #Cust2 arrivals
    if ((sum(c(Y,M,K)) + Cust2[i]) < SLFs[3]*C){
      K[i] <- Cust2[i]
    }
    else if ((sum(c(Y,M,K)) < SLFs[3]*C)){
      K[i] <- SLFs[3]*C - sum(c(Y,M,K))
      #print(c("K",i))
    }
    else if ((sum(c(Y,M,K)) + Cust2[i]) < SLFs[2]*C){
      M[i] <- M[i] + round((Cust2[i])*p2)
    }
    else if ((sum(c(Y,M,K)) < SLFs[2]*C)){
      M[i] <- M[i] + round((SLFs[2]*C - sum(c(Y,M,K)))*p2)
      #print(c("M",i))
    }
  }
  #Return the number of bookings made on each day
  return(matrix(c(aggBH(Y,BH),aggBH(M,BH),aggBH(K,BH)),ncol=3))
}
plfsimulation(BH=30,mu=180,L=0.01,C=200,SLFs=c(1,0.6,0.25),segments1=c(10,10,10),rates1=c(20,30,40),segments2=c(15,10,5),rates2=c(5,35,50),p1=0.75,p2=0.65)


#Write a function that runs the above simulation n times and writes the results to a csv file
nplfsim <- function(n,BH,mu,L,C,SLFs,segments1,rates1,segments2,rates2,p1,p2,s,name){
  k <- plfsimulation(BH,mu,L,C,SLFs,segments1,rates1,segments2,rates2,p1,p2)
  colnames(k) <- c("Y","M","K")
  km <- t(k)
  for (i in 2:n){
    k <- plfsimulation(BH,mu,L,C,SLFs,segments1,rates1,segments2,rates2,p1,p2)
    colnames(k) <- c("Y","M","K")
    km <- rbind(km,t(k))
  }
  #return(km)
  set.seed(s)
  filename <- paste(name, ".csv", sep="") 
  write.csv(km, filename, row.names=T)
}
nplfsim(n=500,BH=30,mu=180,L=0.01,C=200,SLFs=c(1,0.4,0.25),segments1=c(10,10,10),rates1=c(20,30,60),segments2=c(15,10,5),rates2=c(5,35,50),p1=0.85,p2=0.65,s=12345,name="sim02")
