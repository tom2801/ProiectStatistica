n=1000
no=500
a=3
b=5

#1
#a
sample1 <- function(n){
  X_i <- rbeta(1,1/(1:n), 1/(1:n))
  return(X_i)
}

sol <- check.convergence(nmax=1000, M=500, genXn=sample1, mode="L", densfunc=dbinom(1, 1/2))
print(sol)


#b
sample2 <- function(n, a=3, b=5){
  X_i <- rbeta(1, a/(1:n), b/(1:n))
  return(X_i)
}

check.convergence(nmax=n, M=no, genXn=sample2, mode="L", densfunc=dbinom(1, 1/2))


#2
sample3 <- function(n){
  X_i <- c()
  for (i in 1:n) {
    X_i <- c(X_i, 1/n)
  }
  return(X_i)
}
n=1000

X <- punif(0, 1)
check.convergence(nmax=1000, M=500, genXn=sample3, mode="L", probfunc=X)
check.convergence(nmax=1000, M=500, genXn=sample3, epsilon=0.05, mode="p", densfunc=dunif(0,1))


#3
nr <- 10

sample4 <- function(n){
  X_i <- c()
  for (i in 1:n) 
      X_i <- rgeom (nr, 1/3)
  return(X_i)
}

f_rep <- function(k, nr){
  #va <- dgeom(10, 1/3)
  #return(integrate(va,-Inf,x)$value)
  s=0
  for(i in 1:k)
    s=s+factorial(nr)/(factorial(nr-i)*factorial(i)) * (1/3)*i * (1-1/3)^(nr-i)
  return(s)
}

ok <- FALSE
while (ok==FALSE) {
  for(m in 1:1000)
    for(M in 1:1000)
      if(f_rep(M, nr)-f_rep(m, nr)==1) { 
          ok <- TRUE 
          min <- m
          max <- M
      }
}

check.convergence(nmax=1000, M=500, genXn=sample4, mode="as")

?check.convergence()
?rbinom()
?runif()
?rgeom()