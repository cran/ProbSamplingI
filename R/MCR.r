MCR<-function(N,m,yk=NULL,zk=NULL,dk=NULL,type="selec",parameter="total",Ek=NULL,Nc=0.95){
  #Validation of Arguments....................................................................................................
  if(!(type=="selec" | type=="estm" | type=="estm.Ud")){stop("Error, argument in type is not valid \n")}
  if(!(parameter=="total" | parameter=="mean" | parameter=="prop" | parameter=="ratio")){stop("Error, argument in parameter is not valid \n")}
  if(missing(N)){stop("Error, the value of N is absent \n")}else{
  if(!is.wholenumber(N)){stop("Error, N must be a whole number \n")}
  if(N<=1){stop("Error, population size is not valid \n")}}
  if(missing(m)){stop("Error, the value of m is absent \n")}
  if(!is.wholenumber(m)){stop("Error, m must be a whole number \n")}
  if(type=="estm" | type=="estm.Ud"){
    if(missing(yk)){stop("Error, the vector yk is absent  \n")}
    if(length(yk)!=m){stop("Error, the vectors yk have no length m \n")}
    if(parameter=="prop"){if(!is.factor(yk)){stop("Error, yk should be a factor \n")}}
    if(parameter=="ratio"){
      if(missing(zk)){stop("Error, the vector of zk is absent \n")}
      if(length(zk)!=m){stop("Error, the vectors zk have no length m \n")}
      if(length(yk)!=length(zk)){stop("Error, vectors yk and zk are not equal in length \n")}}
  }
  if(type=="estm.Ud"){
    if(missing(dk)){stop("Error, the vector dk is absent \n")}
    if(!is.factor(dk)){stop("Error, dk should be a factor \n")}
    if(length(yk)!=length(dk)){stop("Error, vectors yk and dk are not equal in length \n")}
    if(parameter=="ratio"){if(length(zk)!=length(dk)){stop("vectors zk and dk are not equal in length \n")}}
    if(length(dk)!=m){stop("Error, the vectors dk have no length m \n")}
  }
  if(m<1){stop("Error, sample size is not valid \n")}
  if(!(Nc>0 & Nc<1)){stop("Error, the confidence level is not valid \n")}
  #..........................................................................................................................

  if(type=="selec"){
    if(missing(Ek)){Ek<-runif(m)}
    pk<-rep(1/N,N)
    Pk.acum<-cumsum(pk)
    Li<-c(0.000000001,Pk.acum[-N]+0.000000001)
    Ls<-c(Pk.acum[-N],0.9999999999)
    K<-numeric()
    for(i in 1:m){
      for (j in 1:N){
        if(Ek[i]<=Ls[j] & Ek[i]>=Li[j]){K[i]<-j}
      }
    }
    return(list(Ksel=K))
  }

  if(type=="estm"|type=="estm.Ud"){
    n<-length(yk)
    if(type=="estm"){dk<-as.matrix(rep(1,length(yk)))}
    if(parameter!="ratio"){zk<-rep(1,length(yk))}
    dk<-Dk(dk)
    if(parameter=="prop"){yk<-as.factor(yk);ydk<-PRODM(dk,Dk(yk))}else{ydk<-yk*dk}
    if(parameter=="prop"){zk<-matrix(rep(zk,ncol(Dk(yk))),ncol=ncol(Dk(yk)));zdk<-PRODM(dk,zk)}else{zdk<-zk*dk}
    if(parameter=="total"){
      Estd<-N*apply(ydk,2,mean)
      Vest<-((N^2)/m)*apply(ydk,2,var)
      deff<-(1-(1/N))/(1-(m/N))
    }else{
      tdy.e<-N*apply(ydk,2,mean)
      tdz.e<-N*apply(zdk,2,mean)
      Estd<-tdy.e/tdz.e
      udk.e<-matrix(0,ncol=length(Estd),nrow=length(yk));for(i in 1:length(Estd)){udk.e[,i]<-(ydk[,i]-Estd[i]*zdk[,i])/tdz.e[i]}
      Vest<-((N^2)/m)*apply(udk.e,2,var)
    }
    e.e<-sqrt(Vest)
    Cve<-(sqrt(Vest)/Estd)*100
    if(parameter=="prop"){IC<-F.IC(n,tdy.e,tdz.e,Estd,e.e,Nc,parameter)}else{IC<-F.IC(n=n,Estd=Estd,e.e=e.e,Nc=Nc,parameter=parameter)}
    IC.inf<-IC$IC1;IC.sup<-IC$IC2
    if(parameter=="total"){
      Resul<-data.frame(Estd,Vest,e.e,Cve,IC.inf,IC.sup,deff)
      names(Resul)[1]<-parameter
    }else{
      if(parameter=="prop"){
        if(type=="estm" ){Uz=levels(yk);Resul<-data.frame(Uz,Estd,Vest,e.e,Cve,IC.inf,IC.sup);names(Resul)[2]<-parameter
        }else{
          Uzd=expand.grid(Uz=levels(yk),Ud=colnames(dk));Resul<-data.frame(Ud=Uzd$Ud,Uz=Uzd$Uz,Estd,Vest,e.e,Cve,IC.inf,IC.sup);names(Resul)[3]<-parameter}
      }else{
        Resul<-data.frame(Estd,Vest,e.e,Cve,IC.inf,IC.sup)
        names(Resul)[1]<-parameter}
    }
    return(list(Estimation=Resul))
  }
}
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

