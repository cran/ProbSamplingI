PPT<-function(xk,m,yk=NULL,zk=NULL,pk=NULL,dk=NULL,type="selec",parameter="total",method ="acum.total",Nc=0.95,Ek=NULL){
  # Validation of Arguments.......................................... ..................................................................
  if(!(type=="selec" | type=="estm" | type=="estm.Ud")){stop("Error, argument in type is not valid \n")}
  if(!(parameter=="total" | parameter=="mean" | parameter=="prop" | parameter=="ratio")){stop("Error, argument in parameter is not valid \n")}
  if(type=="selec"){
    if(missing(xk)){stop("Error, the vector xk is absent \n")}
    if(missing(m)){stop("Error, the value of m is absent \n")}
    if(!is.wholenumber(m)){stop("Error, m must be a whole number \n")}
    if(length(xk)==1){stop("Error, population size is not valid \n")}
    if(m<1){stop("Error, sample size is not valid \n")}
  }
  if(type=="estm" | type=="estm.Ud"){
    if(missing(yk)){stop("Error, the vector yk is absent \n")}
    if(parameter=="prop"){if(!is.factor(yk)){stop("Error, yk should be a factor")}}
    if(missing(pk)){stop("Error, the vector pk is absent \n")}
    if(length(pk)!=length(yk)){stop("Error, vectors pk and yk are not equal in length \n")}
    if(parameter=="ratio"){if(missing(zk)){stop("Error, the vector zk is absent \n")}}
  }
  if(type=="estm.Ud"){
    if(missing(dk)){stop("Error, the vector dk is absent \n")}
    if(!is.factor(dk)){stop("Error, dk should be a factor")}
    if(length(dk)!=length(yk)){stop("Error, vectors dk and yk are not equal in length \n")}
    if(length(dk)!=length(pk)){stop("Error, vectors pk and dk are not equal in length \n")}
    if(parameter=="ratio"){if(length(dk)!=length(zk)){stop("Error, vectors dk and zk are not equal in length \n")}}
  }
  if(!(Nc>0 & Nc<1)){stop("Error, the confidence level is not valid \n")}
  #................................................................................
  if(type=="selec"){
    if(missing(Ek)){Ek<-runif(m)}
    N<-length(xk);pk<-xk/sum(xk)
    if(method =="acum.total"){
      Tk<-c(0,cumsum(xk))
      Li<-Tk[-(N+1)]
      Ls<-Tk[-1]
      Ksel<-numeric()
      for(j in 1:m){
        Ksel[j]<-which((Li<(Ek[j]*Tk[N+1])) & ((Ek[j]*Tk[N+1])<=Ls))
      }
      Ksel
      pksel<-pk[Ksel]
      return(list(Ksel=Ksel,pksel=pksel))
    }

    if(method =="lahiri"){
      muestra<-numeric(0)
      k<-1
      while(m>length(muestra)) {
        l<-round(runif(1,1,N));l
        n<-round(runif(1,1,max(xk)));n
        if(xk[l]>n){
          muestra[k]<-l
          k<-k+1
        }
      }
      pksel<-pk[muestra]
      return(list(Ksel=muestra,pksel=pksel))
    }
  }

  #..................................................................................................
  ty<-function(yk,pk){m<-length(yk);t<-(1/m)*sum(yk/pk);return(t)}
  Vty<-function(yk,pk){
    m<-length(yk)
    t<-(1/m)*sum(yk/pk)
    v<-(1/(m*(m-1)))*sum(((yk/pk)-t)^2)
    return(v)
  }
  #............................................................................................

  if(type=="estm" | type=="estm.Ud"){
    m<-length(yk)
    if(type=="estm"){dk<-as.matrix(rep(1,length(yk)))}
    if(parameter!="ratio"){zk<-rep(1,length(yk))}
    dk<-Dk(dk)
    if(parameter=="prop"){yk<-as.factor(yk);ydk<-PRODM(dk,Dk(yk))}else{ydk<-yk*dk}
    if(parameter=="prop"){zk<-matrix(rep(zk,ncol(Dk(yk))),ncol=ncol(Dk(yk)));zdk<-PRODM(dk,zk)}else{zdk<-zk*dk}
    if(parameter=="total"){
      Estd<-apply(ydk,2,ty,pk)
      Vest<-apply(ydk,2,Vty,pk)
    }else{
      tdy.e<-apply(ydk,2,ty,pk)
      tdz.e<-apply(zdk,2,ty,pk)
      Estd<-tdy.e/tdz.e
      udk.e<-matrix(0,ncol=length(Estd),nrow=length(yk));for(i in 1:length(Estd)){udk.e[,i]<-(ydk[,i]-Estd[i]*zdk[,i])/tdz.e[i]}
      Vest<-apply(udk.e,2,Vty,pk)
    }
    e.e<-sqrt(Vest)
    Cve<-(sqrt(Vest)/Estd)*100
    if(parameter=="prop"){IC<-F.IC(m,tdy.e,tdz.e,Estd,e.e,Nc,parameter)}else{IC<-F.IC(n=m,Estd=Estd,e.e=e.e,Nc=Nc,parameter=parameter)}
    IC.inf<-IC$IC1;IC.sup<-IC$IC2
    if(parameter=="total"){
      Resul<-data.frame(Estd,Vest,e.e,Cve,IC.inf,IC.sup)
      names(Resul)[1]<-parameter
    }else{
      if(parameter=="prop"){
        if(type=="estm" ){Uz=levels(yk);Resul<-data.frame(Uz,Estd,Vest,e.e,Cve,IC.inf,IC.sup);names(Resul)[2]<-parameter
        }else{
          Uzd=expand.grid(Uz=levels(yk),Ud=colnames(dk));Resul<-data.frame(Uz=Uzd$Uz,Ud=Uzd$Ud,Estd,Vest,e.e,Cve,IC.inf,IC.sup);names(Resul)[3]<-parameter}
      }else{
        Resul<-data.frame(Estd,Vest,e.e,Cve,IC.inf,IC.sup)
        names(Resul)[1]<-parameter}
    }
    return(list(Estimation=Resul))
  }
}
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
