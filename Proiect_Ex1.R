#1

medie<-function(functie,opt,a=0,b=100){ # a si b sunt doar pt cazul opt==2, anume uniforma discreta
  if (opt==1){ #caz continuu
    
    aux<-function(x){
      return(x*functie(x))
    }
    
    return (integrate(aux,-Inf,+Inf)$value)
  }
  
  if(opt==0){ #caz discret
    x<-0:1000
    return(sum(x*functie(x)))
  }
  
  if(opt==2){
    return ((a+b)/2)
  }
  
}


varianta<-function(functie,opt,a=0,b=100){ # de adaugat si aici caz discret
  if (opt==1){ #caz continuu
    aux<-function(x){
      return(x^2*functie(x)) #formula de transport
    }
    med<-medie(functie,opt)
    return(integrate(aux,-Inf,Inf)$value-(med)^2)
  }
  if (opt==0){ # caz discret
    x<-0:1000
    aux<-function(x){
      return (x^2)
    }
    prob<-sapply(x,functie)
    xpatrat<-sapply(x,aux)
    return(sum(xpatrat*prob)-medie(functie,opt)^2)
  }
  if (opt==2){ #caz unif discreta
    n<-b-a+1
    return((n^2-1)/12)
  }
}

#2

f2<-function(functie,opt,a=0,b=100){ # de adaugat caz discret
  if (opt==1){ #caz continuu
    aux<-function(x){
      return (abs(x-medie(functie,opt))^3*functie(x))
    }
    return (integrate(aux,-Inf,+Inf)$value)
  }
  if (opt==0){ #caz discret
    x<-0:1000
    aux<-function(x){
      return (abs(x-medie(functie,opt))^3*functie(x))
    }
    return (sum(sapply(x,aux)))
  }
  
  if(opt==2){ # caz uniforma discreta
    n=b-a+1
    x<-a:b
    aux<-function(x){
      return (abs(x-medie(functie,opt,a,b))^3*1/n)
    }
    return(sum(sapply(x,aux)))
  }
  
}

#3+#5

dim<-c(30,100,1000)



#5
margine<-function(n,functie,opt,a=0,b=100){ 
  
  med<-f2(functie,opt,a,b)
  sigmaCub<-sqrt(varianta(functie,opt,a,b))^3
  return (33/4*(med/(sqrt(n)*sigmaCub)))
  
}


Binomiala<-function(x){
  return(dbinom(x,5,1/3))
}

Geometrica<-function(x){
  return(dgeom(x,1/3))
}

Poisson<-function(x){
  return(dpois(x,1/3))
}

Unifcont<-function(x){
  return (dunif(x,0,1))
}

Exponentiala<-function(x){
  return (dexp(x,1/3))
}

Gamma<-function(x){
  return (dgamma(x,shape=3,rate=4))
}

Beta<-function(x){
  return (dbeta(x,3,4))
}



marginiBin<-c()          
for (n in dim){
  marginiBin<-c(marginiBin,margine(n,Binomiala,0))
}    

marginiGeo<-c()          
for (n in dim){
  marginiGeo<-c(marginiGeo,margine(n,Geometrica,0))
}       

marginiPois<-c()          
for (n in dim){
  marginiPois<-c(marginiPois,margine(n,Poisson,0))
}    

marginiUnifcont<-c()          
for (n in dim){
  marginiUnifcont<-c(marginiUnifcont,margine(n,Unifcont,1))
}

marginiExponentiala<-c()          
for (n in dim){
  marginiExponentiala<-c(marginiExponentiala,margine(n,Exponentiala,1))
}

marginiGamma<-c()          
for (n in dim){
  marginiGamma<-c(marginiGamma,margine(n,Gamma,1))
}

marginiBeta<-c()          
for (n in dim){
  marginiBeta<-c(marginiBeta,margine(n,Beta,1))
}


denumiri<-c('Binomiala', 'Geometrica','Poisson','Unifcont','Exponentiala','Gamma','Beta')




margini30<-list()
margini30[[1]]<-marginiBin[1]
margini30[[2]]<-marginiGeo[1]        
margini30[[3]]<-marginiPois[1]
margini30[[4]]<-marginiUnifcont[1]
margini30[[5]]<-marginiExponentiala[1]          
margini30[[6]]<-marginiGamma[1]
margini30[[7]]<-marginiBeta[1]           

margini100<-list()
margini100[[1]]<-marginiBin[2]
margini100[[2]]<-marginiGeo[2]        
margini100[[3]]<-marginiPois[2]
margini100[[4]]<-marginiUnifcont[2]
margini100[[5]]<-marginiExponentiala[2]          
margini100[[6]]<-marginiGamma[2]
margini100[[7]]<-marginiBeta[2]           

margini1000<-list()
margini1000[[1]]<-marginiBin[3]
margini1000[[2]]<-marginiGeo[3]        
margini1000[[3]]<-marginiPois[3]
margini1000[[4]]<-marginiUnifcont[3]
margini1000[[5]]<-marginiExponentiala[3]          
margini1000[[6]]<-marginiGamma[3]
margini1000[[7]]<-marginiBeta[3]           



margini.data<-data.frame(
  Denumiri=(denumiri),
  Margini30=unlist(margini30),
  Margini100=unlist(margini100),
  Margini1000=unlist(margini1000),
  stringsAsFactors = FALSE
  
)

print (margini.data)    


# vreau sa fac o functie care primeste un x si face cu precizia

# repartitieEmpirica<-function(Zn,x){
#     
# }
# 
# 
# SvZ<-function(esantion,n,sigma,miu){
#    return(sqrt(n)*(mean(esantion)-miu)/sigma)
# }
# 
# precizie<-100
# 
# for (i in c(30,100,1000)){
#    domeniu<-seq(-3,3,0.1)
#    
#    Xn<-rbinom(i,5,1/3)
#    Zn<-SvZ(Xn,i,sqrt(varianta(Binomiala,0)),medie(Binomiala,0))
#    
# }



interval<-seq(-3,3,0.1)

sv<-function(x,sigma,miu,n){
  return(n*(x*sigma/sqrt(n)+miu))
}

#Binomiala
for (i in c(30,100,1000)){
  sigma<-sqrt(varianta(Binomiala,0))
  miu<-medie(Binomiala,0)
  
  plot(interval,abs(pbinom(sv(interval,sigma,miu,i),i*5,1/3)-pnorm(interval)),type='l')
  abline(h=marginiBin[1],col='red')
  abline(h=marginiBin[2],col='blue')
  abline(h=marginiBin[3],col='magenta')
  plot(interval,pnorm(interval),type='l',col='blue')
  lines(interval,pbinom(sv(interval,sigma,miu,i),i*5,1/3),col='red')
  
}


#Geometrica
for (i in c(30,100,1000)){
  sigma<-sqrt(varianta(Geometrica,0))
  miu<-medie(Geometrica,0)
  
  plot(interval,abs(pnbinom(sv(interval,sigma,miu,i),i,1/3)-pnorm(interval)),type='l')
  abline(h=marginiGeo[1],col='red')
  abline(h=marginiGeo[2],col='blue')
  abline(h=marginiGeo[3],col='magenta')
  plot(interval,pnorm(interval),type='l',col='blue')
  lines(interval,pnbinom(sv(interval,sigma,miu,i),i,1/3),col='red')
  
}


#Poission
for (i in c(30,100,1000)){
  sigma<-sqrt(varianta(Poisson,0))
  miu<-medie(Poisson,0)
  
  plot(interval,abs(ppois(sv(interval,sigma,miu,i),i*1/3)-pnorm(interval)),type='l')
  abline(h=marginiPois[1],col='red')
  abline(h=marginiPois[2],col='blue')
  abline(h=marginiPois[3],col='magenta')
  plot(interval,pnorm(interval),type='l',col='blue')
  lines(interval,ppois(sv(interval,sigma,miu,i),i*1/3),col='red')
  
}


#Exponentiala
for (i in c(30,100,1000)){
  sigma<-sqrt(varianta(Exponentiala,1))
  miu<-medie(Exponentiala,1)
  
  plot(interval,abs(pgamma(sv(interval,sigma,miu,i),i,1/3)-pnorm(interval)),type='l')
  abline(h=marginiExponentiala[1],col='red')
  abline(h=marginiExponentiala[2],col='blue')
  abline(h=marginiExponentiala[3],col='magenta')
  plot(interval,pnorm(interval),type='l',col='blue')
  lines(interval,pgamma(sv(interval,sigma,miu,i),i,1/3),col='red')
  
}


#Gamma
for (i in c(30,100,1000)){
  sigma<-sqrt(varianta(Gamma,1))
  miu<-medie(Gamma,1)
  
  plot(interval,abs(pgamma(sv(interval,sigma,miu,i),i*3,4)-pnorm(interval)),type='l')
  abline(h=marginiGamma[1],col='red')
  abline(h=marginiGamma[2],col='blue')
  abline(h=marginiGamma[3],col='magenta')
  plot(interval,pnorm(interval),type='l',col='blue')
  lines(interval,pgamma(sv(interval,sigma,miu,i),i*3,4),col='red')
  
}


comb <- function(n, x) {
  factorial(n) / (factorial(n-x)*factorial(x))
}


IrwinHall <- function(x, n){ #CDF-ul variabilei aleatoare Irwin-Hall (suma de Unif(0,1))
  s<-0
  for(k in 0:n){
    s<-s+(-1)^k*comb(n,k)*sign(x-k)*(x-k)^n
  }
  return(1/2+1/(2*factorial(n))*s)
}

#Unifcont





for (i in c(5,10,15)){ #CDF-ul Irwin-Hall este foarte volatil
  sigma<-sqrt(varianta(Unifcont,1)) #a fost necesara modificarea dimensiunilor esantionului
  miu<-medie(Unifcont,1)
  ih<-IrwinHall(sv(interval,sigma,miu,i),i)
  plot(interval,abs(ih-pnorm(interval)),type='l')
  plot(interval,pnorm(interval),type='l',col='blue')
  lines(interval,ih,col='red')
    }
