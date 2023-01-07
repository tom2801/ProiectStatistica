#1

medie<-function(functie,opt){ 
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
  
}


varianta<-function(functie,opt){ # de adaugat si aici caz discret
  aux<-function(x){
    return(x*functie(x)) #formula de transport
  }
  med<-medie(functie,opt)
  return(medie(aux,opt)-(med)^2)
}

aux1<-function(x){
  return(dnorm(x,4,3))
}
m<-medie(aux1,1)
print(varianta(aux1,1))


#2

f2<-function(functie,opt){ # de adaugat caz discret
  if (opt==1){
    aux<-function(x){
      return (abs(x-medie(functie,opt))^3*functie(x))
    }
    return (integrate(aux,-Inf,+Inf)$value)
  }
  
}

print(f2(aux1,1))