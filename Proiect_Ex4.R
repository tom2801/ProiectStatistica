n=1000
no=500
a=3
b=5

#1
#a

repartitie1<-function(x){
  return(pbinom(x,1,1/2))
}

repartitie1bis<-function(x){
  return(pbinom(x,1,1-3/5))
}

sample1 <- function(n){
  X_i <- rbeta(n,1/(1:n), 1/(1:n))
  return(X_i)
}

check.convergence(nmax=1000, M=1000, genXn=sample1, mode="L", probfunc=repartitie1)



#b
sample2 <- function(n, a=3, b=5){
  X_i <- rbeta(n, a/(1:n), b/(1:n))
  return(X_i)
}

check.convergence(nmax=n, M=no, genXn=sample2, mode="L", probfunc=repartitie1)
check.convergence(nmax=n, M=no, genXn=sample2, mode="L", probfunc=repartitie1bis)

#2
sample3 <- function(n){
  multime<-seq_along(1:n)/n
  X_i<-sample(multime,n,replace=TRUE)
  return(X_i)
}


repartitie2<- function(x){
  return (punif(x,0,1))
}

densitate2<-function(x){
  return (dunif(x,0,1))
}

check.convergence(nmax=1000, M=500, genXn=sample3, mode="L", probfunc=repartitie2)
check.convergence(nmax=1000, M=500, genXn=sample3, epsilon=0.05, mode="p",probfunc=repartitie2)


#3

sample4<-function(n){
  rez<-c()
  for (i in rep(n,n)){
    rez<-c(rez,min(runif(i,0,1)))
  }
  return (rez)
  
}


sample4bis<-function(n){
  rez<-c()
  for (i in rep(n,n)){
    rez<-c(rez,max(runif(i,0,1)))
  }
  return (rez-1)
  
}


check.convergence(nmax=1000, M=500, genXn=sample4, mode="as")
check.convergence(nmax=1000, M=500, genXn=sample4bis, mode="as")




# nr <- 10
# 
# sample4 <- function(n){
#   X_i <- c()
#   for (i in 1:n) 
#       X_i <- rgeom (nr, 1/3)
#   return(X_i)
# }
# 
# f_rep <- function(k, nr){
#   #va <- dgeom(10, 1/3)
#   #return(integrate(va,-Inf,x)$value)
#   s=0
#   for(i in 1:k)
#     s=s+factorial(nr)/(factorial(nr-i)*factorial(i)) * (1/3)*i * (1-1/3)^(nr-i)
#   return(s)
# }
# 
# ok <- FALSE
# while (ok==FALSE) {
#   for(m in 1:1000)
#     for(M in 1:1000)
#       if(f_rep(M, nr)-f_rep(m, nr)==1) { 
#           ok <- TRUE 
#           min <- m
#           max <- M
#       }
# }