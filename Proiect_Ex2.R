f <- function (x,a=1){
  return(a*(sin(x)^2*exp(-x^2*sqrt(x))))
}

g2 <- function(x){
  return(1/(2*pi) * 1/(1+ (x^2)/4))
}


rg2 <- function(n){
  u <- runif(n)
  return(2*tan(pi*u))
}




fmed<-function(x){
  return(x*f(x,3.852985))  # folosita pt a calcula media
}


raport1<-function(x){
  return(f(x)/dexp(x))  
}

raport2<-function(x){
  return(f(x)/(2*g2(x))) 
}

raport3<-function(x){
  return(f(x)/(2*dnorm(x))) 
}

medie_exacta<-integrate(fmed,0,Inf)$value



func1 <- function(dummy, k=1)
{
  while (TRUE)
  { 
    u <- runif(1)
    y <- rexp(1)
    g <- dexp(y)
    if (u <= f(y) / (k* g)) return(y) 
  }
}


freq1 <- function(dummy, k=1)
{
  u <- runif(1)
  y <- rexp(1)
  g1 <- dexp(y)
  if (u <= f(y) / (k* g1)) return(1) 
  return(0)
}

func2 <- function(dummy, k=1)
{
  while (TRUE)
  { 
    u <- runif(1)
    y <- abs(rg2(1))
    g <- 2*g2(y)
    if (u <= f(y) / (k* g)) return(y) 
  }
}

freq2 <- function(dummy, k=1)
{
  while (TRUE)
  { 
    u <- runif(1)
    y <- abs(rg2(1))
    g <- 2*g2(y)
    if (u <= f(y) / (k* g)) return(1) 
    return(0)
  }
}

func3 <- function(dummy, k=1)
{
  while (TRUE)
  { 
    u <- runif(1)
    y <- abs(rnorm(1))
    # while(y<0){
    #   y<-rnorm(1)
    # }
    
    g <- 2*dnorm(y)
    if (u <= f(y) / (k* g)) return(y) 
  }
}

aux3 <- function (nr,rez){
  
  return(mean(rez[1:nr]))
}

freq3<- function(dummy, k=1)
{
  u <- runif(1)
  y <- abs(rnorm(1))
  # while(y<0){
  #   y<-rnorm(1)
  # }
  g1 <- 2*dnorm(y)
  if (u <= f(y) / (k* g1)) return(1) 
  return(0)
}


x<-seq(0,4,0.001)  # intervalul discretizat


sup1<-optimise(raport1,x,maximum=TRUE)
sup2<-optimise(raport2,x,maximum=TRUE)
sup3<-optimise(raport3,x,maximum=TRUE)


acceptate1<-sapply(1:10^5,freq1,sup1$objective)
acceptate2<-sapply(1:10^5,freq2,sup2$objective)
acceptate3<-sapply(1:10^5,freq3,sup3$objective)

rata1<-mean(acceptate1)
rata2<-mean(acceptate2)
rata3<-mean(acceptate3)

constAprox1<-1/((sup1$objective)*rata1)
constAprox2<-1/((sup2$objective)*rata2)
constAprox3<-1/((sup3$objective)*rata3)



rez1<-sapply(1:10^5,func1,sup1$objective)
rez2<-sapply(1:10^5,func2,sup2$objective)
rez3<-sapply(1:10^5,func3,sup3$objective)


#rezultate3<-c()

# for (i in 1:10){
#   
#   rezultate3<-c(rezultate3,mean(sapply(1:10^5,func3,sup3$objective)))
#   
# }
# 
# plot(1:10,rezultate3, type = "o" )
# abline(h = medie_exacta+0.001, col = "red") 
# abline(h = medie_exacta-0.001, col = "red") 
# abline(h = medie_exacta, col = "blue") 
# 
# 
# rezultate3p<-c()
# 
# for (i in 1:10){
#   
#   rezultate3p<-c(rezultate3p,mean(sapply(1:10^6,func3,sup3$objective)))
#   
# }
# 
# plot(1:10,rezultate3p, type = "o" )
# abline(h = medie_exacta+0.001, col = "red") 
# abline(h = medie_exacta-0.001, col = "red") 
# abline(h = medie_exacta, col = "blue") 

# medie1<-mean(rez1)
# 
# medii1<-cumsum(rez1)/seq_along(rez1)
# 
# criteriu1<-which(abs(medii1-medie_exacta)<=0.001)
# 
# print(criteriu1)
# 
# 
# 
# 
# medie2<-mean(rez2)
# 
# medii2<-cumsum(rez2)/seq_along(rez2)
# 
# criteriu2<-which(abs(medii2-medie_exacta)<=0.001)
# 
# print(criteriu2)
# 
# 
# 
# medie3<-mean(rez3)
# 
# medii3<-cumsum(rez3)/seq_along(rez3)
# 
# criteriu3<-which(abs(medii3-medie_exacta)<=0.001)
# 
# print(criteriu3)


hist(rez1,freq=F)
lines(x,f(x,constAprox1),col='red')

hist(rez2,freq=F)
lines(x,f(x,constAprox2),col='red')

hist(rez3,freq=F)
lines(x,f(x,constAprox3),col='red')




#aproximarea mediei
sim_est_medie3<-sapply(1:10^6,freq3,sup3$objective)


est3<-mean(sim_est_medie3)

est_freq3<-cumsum(sim_est_medie3)/seq_along(sim_est_medie3)

est_freq3<-est_freq3[est_freq3!=0]

est_constante_aprox3<-1/(est_freq3*sup3$objective)

fmedAux<-function(x,a){
  return(x*f(x,a))
}

integrare_auxiliara<-function(constanta){
  return (integrate(fmedAux,0,Inf,a=constanta)$value)
}

medii_aprox3<-sapply(est_constante_aprox3,integrare_auxiliara)


medii_acceptate3<-which(abs(medii_aprox3-medie_exacta)<0.001)

reznraprox3<-mean(medii_acceptate3)

rezultate3<-c()
for (i in 1:10){
  
  rezultate3<-c(rezultate3,mean(sapply(1:reznraprox3,func3,sup3$objective)))
  
}

plot(1:10,rezultate3, type = "o" )
abline(h = medie_exacta+0.001, col = "red")
abline(h = medie_exacta-0.001, col = "red")
abline(h = medie_exacta, col = "blue")

# print(est_constante_aprox3[length(est_constante_aprox3)])
# 
# print(est_freq3[length(est_freq3)])
