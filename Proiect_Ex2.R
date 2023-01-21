f <- function (x,a=1){
  return(a*(sin(x)^2*exp(-x^2*sqrt(x))))
}
g1 <- function(x){
  return(1/2 *exp(-x))
}

g2 <- function(x){
  return(1/(2*pi) * 1/(1+ (x^2)/4))
}

fmed<-function(x){
  return(x*f(x,3.852985))
}

raport<-function(x){
  return(f(x)/dnorm(x)) 
}

raport1<-function(x){
  return(f(x)/g1(x)) 
}

raport2<-function(x){
  return(f(x)/g2(x)) 
}

#print(integrate(f,0,Inf,3.852985))

medie_exacta<-integrate(fmed,0,Inf)$value
print(medie_exacta)

func1 <- function(dummy, k=2)
{
  while (TRUE)
  { 
    u <- runif(1)
    y <- rnorm(1)
    while(y<0){
      y<-rnorm(1)
    }
    
    g1 <- dnorm(y)
    if (u <= f(y) / (k* g1)) return(y) 
  }
}


alt1 <- function(dummy, k=2)
{
    u <- runif(1)
    y <- rnorm(1)
    while(y<0){
      y<-rnorm(1)
    }
    g1 <- dnorm(y)
    if (u <= f(y) / (k* g1)) return(1) 
    return(0)
}

acceptate<-sapply(1:10^5,alt1,1)

rata<-mean(acceptate)

x<-seq(0,4,0.01)

sup<-optimise(raport,x,maximum=TRUE)
sup1<-optimise(raport1,x,maximum=TRUE)
sup2<-optimise(raport2,x,maximum=TRUE)


plot(x,f(x,3.852985),type='l',col='blue')
lines(x,sup$objective*dnorm(x))
lines(x,f(x),col='red')
lines(x,sup1$objective*g1(x), col="green")
lines(x,sup2$objective*g2(x), col="pink")

rez<-sapply(1:10^5,func1,sup$objective)
medie<-mean(rez)
print(medie)
hist(rez,freq=F)
lines(x,f(x,3.852985))

