
BER<-function(N,Pi,yk=NULL,zk=NULL,dk=NULL,type="selec",parameter="total",Nc=0.95,Ek=NULL){
  # Validation of Arguments.....................................................................................................................
  if(!( type=="selec" | type=="estm" | type=="estm.Ud")){stop("Error, argument in type is invalid \n")}
  if(!(parameter=="total" | parameter=="mean" | parameter=="prop" | parameter=="ratio")){stop("Error, argument in parameter is invalid \n")}
  if(type=="selec"){
    if(missing(N)){stop("Error, the value of N is absent \n")}
    if(!is.wholenumber(N)){stop("Error, N must be a whole number \n")}
  }

  if(missing(Pi)){stop("Error, the value of Pi is absent \n")}
  if(type=="estm" | type=="estm.Ud"){
    if(missing(yk)){stop("Error, the vector yk is absent \n")}
    if(parameter=="prop"){if(!is.factor(yk)){stop("Error, yk should be a factor \n")}}
  }
  if(type=="estm.Ud"){
    if(missing(dk)){stop("the vector dk is absent \n")}
    if(length(dk)!=length(yk)){stop("Error, vectors yk and dk are not equal in length \n")}
    if(!is.factor(dk)){stop("Error, dk should be a factor \n")}
  }
  if((type=="estm" | type=="estm.Ud") & parameter=="ratio"){
    if(missing(zk)){stop("Error, the vector zk is absent \n" )}
    if(length(zk)!=length(yk)){stop("Error, vectors yk and zk are not equal in length \n")}
  }
  if(type=="selec"){if(N<=1){stop("Error, population size is not valid \n")}}
  if(!(Pi>0 & Pi<1)){stop("Error, Pi value is not valid (0<Pi<1) \n")}
  if(!(Nc>0 & Nc<1)){stop("Error, the confidence level is not valid \n")}
  #...........................................................................................................................

  if(type=="selec"){
    if(missing(Ek)){Ek<-runif(N)}
    Ik<-numeric()
    for(i in 1:N){
      if(Ek[i]<Pi){Ik[i]<-1}else{Ik[i]<-0}
    }
    ns<-sum(Ik)
    K<-which(Ik==1)
    return(list(Ksel=K,ns=ns))
  }

  if(type=="estm" | type=="estm.Ud"){
    n<-length(yk)
    if(type=="estm"){dk<-as.matrix(rep(1,length(yk)))}
    if(parameter!="ratio"){zk<-rep(1,length(yk))}
    dk<-Dk(dk)
    if(parameter=="prop"){yk<-as.factor(yk);ydk<-PRODM(dk,Dk(yk))}else{ydk<-yk*dk}
    if(parameter=="prop"){zk<-matrix(rep(zk,ncol(Dk(yk))),ncol=ncol(Dk(yk)));zdk<-PRODM(dk,zk)}else{zdk<-zk*dk}
    if(parameter=="total"){
      Estd<-(1/Pi)*apply(ydk,2,sum)
      Vest<-(1/Pi)*((1/Pi)-1)*apply(ydk^2,2,sum)
      Cve.y<-apply(ydk,2,sd)/apply(ydk,2,mean)
      if(missing(N)){deff<-1+(1/(Cve.y^2))}else{deff<-1+(1/(Cve.y^2))-(1/N)}
    }else{
      tdy.e<-(1/Pi)*apply(ydk,2,sum)
      tdz.e<-(1/Pi)*apply(zdk,2,sum)
      Estd<-tdy.e/tdz.e
      udk.e<-matrix(0,ncol=length(Estd),nrow=length(yk));for(i in 1:length(Estd)){udk.e[,i]<-(ydk[,i]-Estd[i]*zdk[,i])/tdz.e[i]}
      Vest<-(1/Pi)*((1/Pi)-1)*apply(udk.e^2,2,sum)
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
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


