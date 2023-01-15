#1

alfa<-c(1,1/3,1/2)
lambda<-c(1/20,1/10,1/30)

# alfa<-c(1,1/3,1/2,1/5,1/6,1/7,1/3)
# lambda<-c(1/20,1/30,1/10,1/20,1/45,1/10,1/25)


rbern<-function(alfa){
  if(runif(1)<=alfa)
  {
    return (1)
  }
  return(0)
}

Timp<-function(d,lambda,alfa){
  s<-0
  i<-1
  ok<-1
  while(ok){
    
    if(rbern(alfa[i])){
       s<-s+rexp(1,lambda[i])
    } else{
       ok<-0
       break
    }
    
    i<-i+1
    
    if(i>length(lambda)){
      ok<-0
    }
    
  }
  return(list(s,i))
}

rez<-lapply(1:10^6,Timp,lambda,alfa)

timp_simulat<-unlist(lapply(rez,function(l) l[[1]]))  

medie<-mean(timp_simulat)

etapa<-unlist(lapply(rez,function(l) l[[2]]))
etapa<-etapa-1

x<-seq(0,200,0.01)

hist(timp_simulat,freq=F)
lines(x,dexp(x,1/28.33))

#timp si etapa sunt identic indexati


#2 -pe foaie

#3

p_finalizare<-length(which(etapa==length(alfa)))/length(etapa) # cazuri favorabile pe cazuri totale

finalizat_estimat<-timp_simulat[timp_simulat>=sum(1/lambda)]

p_finalizare_est<-length(finalizat_estimat)/length(timp_simulat)

#4
sigma<-200

finalizat<-timp_simulat[which(etapa==length(alfa))]

finalizat_conditionat<-finalizat[finalizat<=sigma]

p_final_cond<-length(finalizat_conditionat)/length(timp_simulat)


#5

limite<-range(finalizat)

hist(finalizat,freq=F)



#6

prob<-c()

for (i in 2:length(alfa)){
  prob<-c(prob,length(etapa[etapa<i])/length((etapa)))
}

x1<-seq(0,length(alfa)-1,0.01)
# plot(1:(length(alfa)-1),prob)
# 
# lines(x1,dexp(x1,sum(lambda)))



