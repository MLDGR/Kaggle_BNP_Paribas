library(readr)
library(parallel)
setwd("~/Proyectos/Kaggle/BNP/")
train=read_csv("train.csv")
test=read_csv("test.csv")

gower=function(k,data,train){
  train=train[,-c(1,ncol(train))]
  data=unlist(data)
  nominales=NULL
  for(i in 1:ncol(train)){if(is.factor(train[,i]) | is.integer(train[,i])){nominales=c(nominales,i)}}
  cualitativos=setdiff(1:ncol(train),nominales)
  rangos=apply(train[,cualitativos],2,range)[2,]
  train=as.matrix(sapply(1:ncol(train),function(i){
    if(is.factor(train[,i])){
      as.numeric(levels(train[,i]))[train[,i]]
    }else{train[,i]}}))
  
  Sij=rep(times=nrow(train),x=NA)
  for(j in 1:nrow(train)){
    sij=sapply(nominales, function(k){ifelse(train[j,k]==data[k],1,0)})
    sij=c(sij,as.numeric(1-abs(train[j,cualitativos]-data[cualitativos])/rangos))
    Sij[j]=mean(na.omit(sij))
  }
  return(order(Sij,decreasing=T)[1:k])
}

genera=function(perdido,indice,no.na.train){
  perdido=rbind(perdido)
  for(i in 2:(ncol(train)-1)){
    if(is.na(perdido[1,i])){
      funcion=approxfun(density(no.na.train[,i]))
      p=1
      r=0
      y=max(density(no.na.train[,i])$y)*1.1
      while(p>r){
        p=runif(1,min=0,max=1)*y
        if(is.integer(no.na.train[,i]) | is.factor(no.na.train[,i])){
          e=sample(unique(no.na.train[,i]),1)
        }else{
          e=runif(n=1,min = range(no.na.train[,i])[1],max=range(no.na.train[,i])[2])
        }
        r=funcion(e)
      }
      perdido[1,i]=e
    }
  }
  return(perdido)
}


train=cbind(train[,-2],target=train[,2])

for(i in 1:ncol(test)){
  if(is.character(train[,i])){
    levels=unique(c(train[,i],test[,i]))
    train[[i]] <- as.integer(factor(train[[i]], levels=levels))
    test[[i]]  <- as.integer(factor(test[[i]],  levels=levels))
  }
}

load("~/Proyectos/Kaggle/BNP/Particiones/folds.RData")
train=train[folds[[1]],]
test=train[-folds[[1]],]

imputacion=function(train){  
  no.na.train=train[-which(apply(train,1,function(i)any(is.na(i)))),]
  imputados=mclapply(1:nrow(train),function(i){
    cat(i,"de",nrow(train),"\n")
    if(any(is.na(train[i,]))){
      indice=sample(gower(k = 5,data = train[i,],train = no.na.train),1)
      valores=no.na.train[indice,]
      prob=runif(1,min=0,max=1)
      if(prob>0.8){
        sinteticos=apply(rbind(genera(train[i,],indice,no.na.train)[which(is.na(train[i,]))],valores[is.na(train[i,])]),2,median)
        for(j in 1:length(sinteticos)){
          if(is.factor(no.na.train[,match(names(sinteticos)[j],colnames(no.na.train))]) | 
             is.integer(no.na.train[,match(names(sinteticos)[j],colnames(no.na.train))])){
            sinteticos[j]=round(sinteticos[j],digits=0)
          }
        }
        train[i,is.na(train[i,])]=sinteticos
      }
      train[i,is.na(train[i,])]=valores[is.na(train[i,])]
    }
    return(train[i,])
  },mc.preschedule = TRUE, mc.set.seed = TRUE,
  mc.silent = FALSE, mc.cores = getOption("mc.cores", 1L),
  mc.cleanup = TRUE, mc.allow.recursive = TRUE)
  imputados=matrix(unlist(imputados),nrow=length(imputados),ncol=ncol(train),byrow=T) #Hay que cambiar el ncol=133
}

test=imputacion(test)
save(test,file="test.RData")



