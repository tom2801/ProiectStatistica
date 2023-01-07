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

#3

dim<-c(30,100,1000)



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
  return (dunif(x,4,7))
}

Exponentiala<-function(x){
  return (dexp(x,1/3))
}

Gamma<-function(x){
  return (dgamma(x,shape=3,rate=4,scale=1/4))
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