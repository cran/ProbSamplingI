MAS<-function(N,n,yk=NULL,zk=NULL,dk=NULL,type="selec",method="fmuller",parameter="total",Nc=0.95,Ek=NULL){
  # Validation of Arguments...............................................................................................................
  if(!(parameter=="total" | parameter=="mean" | parameter=="prop" | parameter=="ratio")){stop("Error, argument in parameter is not valid \n")}
  if(!(method=="fmuller" | method=="cnegativo")){stop("Error, argument in method is not valid \n")}
  if(!(type=="selec" | type=="estm" | type=="estm.Ud")){stop("Error, argument in type is not valid \n")}
  if(missing(N)){stop("Error, the value of N is absent \n")}
  if(!is.wholenumber(N)){stop("Error, N must be a whole number \n")}
  if(missing(n)){stop("Error, the value of n is absent \n")}
  if(!is.wholenumber(n)){stop("Error, n must be a whole number \n")}
  if(type=="estm" | type=="estm.Ud"){
    if(missing(yk)){stop("Error, the vector yk is absent  \n")}else{
      if(length(yk)!=n){stop("Error, the vectors yk have no length n \n")}}
    if(parameter=="prop"){if(!is.factor(yk)){stop("Error, yk should be a factor \n")}}
    if(parameter=="ratio"){
      if(missing(zk)){stop("Error, the vector zk is absent \n")}
      if(length(yk)!=length(zk)){stop("Error, vectors yk and zk are not equal in length \n")} }
  }
  if(type=="estm.Ud"){
    if(missing(dk)){stop("Error, the vector dk is absent \n")}
    if(!is.factor(dk)){stop("Error, dk should be a factor \n")}
    if(length(yk)!=length(dk)){stop("Error, vectors yk and dk are not equal in length \n")}
    if(parameter=="ratio"){if(length(zk)!=length(dk)){stop("Error, vectors zk and dk are not equal in length \n")}}
  }
  if(N<=1){stop("Error, population size is not valid \n")}
  if(!is.wholenumber(N)){stop("Error, N must be a whole number \n")}
  if(!is.wholenumber(n)){stop("Error, n must be a whole number \n")}
  if(n<0){stop("Error, sample size is not valid \n")}
  if(n>=N){stop("Error, arguments n and N are not valid, (n<N) \n" )}
  if(!(Nc>0 & Nc<1)){stop("Error, the confidence level is not valid \n")}
  #.....................................................................................................................

  if(type=="selec"){
    if(missing(Ek)){Ek<-runif(N)}
    if(method=="fmuller"){
      Ik<-numeric()
      nk<-0
      k<-1
      while(nk<n){
        Ck<-(n-nk)/(N-(k-1))
        if(Ek[k]<Ck){Ik[k]<-1}else{Ik[k]<-0}
        nk<-nk+Ik[k]
        k<-k+1
      }
      K<-which(Ik==1)
      return(list(Ksel=K))
    }
    if(method=="cnegativo"){
      Ord<-sort(Ek,index.return=TRUE,decreasing=TRUE)
      K<-Ord$ix[1:n]
      Ik<-rep(0,N)
      Ik[K]<-1
      return(list(Ksel=K))
    }
  }
  if(type=="estm"|type=="estm.Ud"){
    if(type=="estm"){dk<-as.matrix(rep(1,length(yk)))}
    if(parameter!="ratio"){zk<-rep(1,length(yk))}
    dk<-Dk(dk)
    if(parameter=="prop"){yk<-as.factor(yk);ydk<-PRODM(dk,Dk(yk))}else{ydk<-yk*dk}
    if(parameter=="prop"){zk<-matrix(rep(zk,ncol(Dk(yk))),ncol=ncol(Dk(yk)));zdk<-PRODM(dk,zk)}else{zdk<-zk*dk}
    if(parameter=="total"){
      Estd<-(N/n)*apply(ydk,2,sum)
      Vest<-((N^2)/n)*(1-(n/N))*apply(ydk,2,var)
    }else{
      tdy.e<-(N/n)*apply(ydk,2,sum)
      tdz.e<-(N/n)*apply(zdk,2,sum)
      Estd<-tdy.e/tdz.e
      udk.e<-matrix(0,ncol=length(Estd),nrow=length(yk));for(i in 1:length(Estd)){udk.e[,i]<-(ydk[,i]-Estd[i]*zdk[,i])/tdz.e[i]}
      Vest<-((N^2)/n)*(1-(n/N))*apply(udk.e,2,var)
    }
    e.e<-sqrt(Vest)
    Cve<-(sqrt(Vest)/Estd)*100
    if(parameter=="prop"){IC<-F.IC(n,tdy.e,tdz.e,Estd,e.e,Nc,parameter)}else{IC<-F.IC(n=n,Estd=Estd,e.e=e.e,Nc=Nc,parameter=parameter)}
    IC.inf<-IC$IC1;IC.sup<-IC$IC2
    if(parameter=="total"){
      Resul<-data.frame(Estd,Vest,e.e,Cve,IC.inf,IC.sup)
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
#............................................................................................................

