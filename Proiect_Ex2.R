f <- function (x,a=1){
  return(a*(sin(x)^2*exp(-x^2*sqrt(x))))
}

g2 <- function(x){
  return(1/(2*pi) * 1/(1+ (x^2)/4))
}


rg2 <- function(n){
   # a se completa 
}


fmed<-function(x){
  return(x*f(x,3.852985))  # folosita pt a calcula media
}


raport1<-function(x){
  return(f(x)/dexp(x))  
}

raport2<-function(x){
  return(f(x)/g2(x)) 
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
    while(y<0){
      y<-rnorm(1)
    }
    
    g <- dexp(y)
    if (u <= f(y) / (k* g)) return(y) 
  }
}


freq1 <- function(dummy, k=1)
{
  u <- runif(1)
  y <- rexp(1)
  while(y<0){
    y<-rnorm(1)
  }
  g1 <- dexp(y)
  if (u <= f(y) / (k* g1)) return(1) 
  return(0)
}



func3 <- function(dummy, k=1)
{
  while (TRUE)
  { 
    u <- runif(1)
    y <- rnorm(1)
    while(y<0){
      y<-rnorm(1)
    }
    
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
    y <- rnorm(1)
    while(y<0){
      y<-rnorm(1)
    }
    g1 <- 2*dnorm(y)
    if (u <= f(y) / (k* g1)) return(1) 
    return(0)
}


x<-seq(0,4,0.001)  # intervalul discretizat


sup1<-optimise(raport1,x,maximum=TRUE)
sup2<-optimise(raport2,x,maximum=TRUE)
sup3<-optimise(raport3,x,maximum=TRUE)


acceptate1<-sapply(1:10^5,freq1,sup1$objective)
acceptate3<-sapply(1:10^5,freq3,sup3$objective)

rata1<-mean(acceptate1)
rata3<-mean(acceptate3)

constAprox1<-1/((sup1$objective)*rata1)
constAprox3<-1/((sup3$objective)*rata3)



rez1<-sapply(1:10^5,func1,sup1$objective)
rez3<-sapply(1:10^6,func3,sup3$objective)

medie3<-mean(rez3)

medii3<-cumsum(rez3)/seq_along(rez3)


criteriu<-which(abs(medii3-medie_exacta)<=0.001)

print(criteriu)

hist(rez1,freq=F)

lines(x,f(x,constAprox1),col='red')


hist(rez3,freq=F)

lines(x,f(x,constAprox3),col='red')





