
CONGL<-function(Argt,cong,design="MAS",type="selec",parameter="total",yk=NULL,zk=NULL,dk=NULL,Ek=NULL,Nc=0.95){
  if(!(parameter=="total" | parameter=="mean" | parameter=="prop" | parameter=="ratio")){stop("Error, argument in parameter is not valid \n")}
  if(!(type=="selec" | type=="estm" | type=="estm.Ud")){stop("Error, argument in type is not valid \n")}
  if(missing(design)){stop("Error, the vector design is absent \n")}
  if(!(design=="MAS" | design=="MCR" | design=="BER" | design=="PPT" | design=="PiPT")){stop("Error, design is not valid \n")}
  if(missing(Argt)){stop("Error, the data.frame Argt is absent \n")}
  if(type=="estm" | type=="estm.Ud"){
    if(missing(yk)){stop("Error, the vector yk is absent  \n")}
    if(parameter=="prop"){if(!is.factor(yk)){stop("Error, yk should be a factor \n")}}
    if(missing(cong)){stop("Error, the vector cong is absent  \n")}
    if(length(yk)!=length(cong)){stop("Error, vectors yk and cong are not equal in length \n")}
    if(parameter=="ratio"){if(missing(zk)){stop("Error, the vector zk is absent  \n")}
      if(length(yk)!=length(zk)){stop("Error, vectors yk and zk are not equal in length \n")}}
    if(!(Nc>0 & Nc<1)){stop("Error, the confidence level is not valid \n")}
  }
  if(type=="estm.Ud"){
    if(missing(dk)){stop("Error, the vector dk is absent \n")}
    if(!is.factor(dk)){stop("Error, dk should be a factor \n")}
    if(length(yk)!=length(dk)){stop("Error, vectors yk and dk are not equal in length \n")}
    if(parameter=="ratio"){if(length(zk)!=length(dk)){stop("Error, vectors yk and dk are not equal in length \n")}}
  }
  #.....................................................................................................................

  if(type=="selec"){
    if(missing(Ek)){Ek<-numeric()}
    if(design=="MAS"){if(length(Ek)==0){Ek<-runif(Argt$NI)};Result<-MAS(N=Argt$NI,n=Argt$nI,Ek=Ek)}
    if(design=="MCR"){if(length(Ek)==0){Ek<-runif(Argt$mI)};Result<-MCR(N=Argt$NI,m=Argt$mI,Ek=Ek)}
    if(design=="BER"){if(length(Ek)==0){Ek<-runif(Argt$NI)};Result<-BER(N=Argt$NI,Pi=Argt$PiI,Ek=Ek)}
    if(design=="PPT"){if(length(Ek)==0){Ek<-runif(Argt$mI)};Result<-PPT(xk=Argt$txkI,m=Argt$mI,Ek=Ek)}
    if(design=="PiPT"){if(length(Ek)==0){Ek<-runif(length(Argt$txkI))};Result<-PiPT(xk=Argt$txkI,n=Argt$nI,Ek=Ek)}
    return(Result)
  }

  F.TV<-function(yk,cong,design,Argt,opc=1){
    tyi<-as.vector(tapply(yk,cong,sum))
    R1<-data.frame(ty.e=numeric(),Vest=numeric())
    if(design=="BER"){R1[1,]<-BER(yk=tyi,Pi=Argt$PiI,type="estm")$Estimation[,1:2]}
    if(design=="MCR"){R1[1,]<-MCR(yk=tyi,N=Argt$NI,m=Argt$mI,type="estm")$Estimation[,1:2]}
    if(design=="MAS"){R1[1,]<-MAS(yk=tyi,N=Argt$NI,n=Argt$nI,type="estm")$Estimation[,1:2]}
    if(design=="PPT"){R1[1,]<-PPT(yk=tyi,pk=Argt$pkI,type="estm")$Estimation[,1:2]}
    if(design=="PiPT"){R1[1,]<-PiPT(yk=tyi,pik=Argt$pikI,mpikl=Argt$mpiklI,type="estm")$Estimation[,1:2]}
    if(opc==1){return(R1[,1])}else{return(R1[,2])}
  }
  if(type=="estm" | type=="estm.Ud"){
    if(type=="estm"){dk<-as.matrix(rep(1,length(yk)))}
    if(parameter!="ratio"){zk<-rep(1,length(yk))}
    dk<-Dk(dk)
    if(parameter=="prop"){yk<-as.factor(yk);ydk<-PRODM(dk,Dk(yk))}else{ydk<-yk*dk}
    if(parameter=="prop"){zk<-matrix(rep(zk,ncol(Dk(yk))),ncol=ncol(Dk(yk)));zdk<-PRODM(dk,zk)}else{zdk<-zk*dk}
    if(parameter=="total"){
      Estd<-apply(ydk,2,F.TV,cong,design,Argt,1)
      Vest<-apply(ydk,2,F.TV,cong,design,Argt,2)
      NI<-length(tapply(yk,cong,length))
      N.e<-ceiling(F.TV(yk=rep(1,length(yk)),cong=cong,design=design,Argt=Argt))
      S2yw<-(1/(N.e-NI))*sum((tapply(yk,cong,length)-1)*tapply(yk,cong,var))
      S2y<-var(yk)
      Ivi<-1-(S2yw/S2y)
    }else{
      tdy.e<-apply(ydk,2,F.TV,cong,design,Argt,1)
      tdz.e<-apply(zdk,2,F.TV,cong,design,Argt,1)
      Estd<-tdy.e/tdz.e
      udk.e<-matrix(0,ncol=length(Estd),nrow=length(yk));for(i in 1:length(Estd)){udk.e[,i]<-(ydk[,i]-Estd[i]*zdk[,i])/tdz.e[i]}
      Vest<-apply(udk.e,2,F.TV,cong,design,Argt,2)
    }
    e.e<-sqrt(Vest)
    Cve<-(sqrt(Vest)/Estd)*100
    n<-length(yk)
    if(parameter=="prop"){IC<-F.IC(n,tdy.e,tdz.e,Estd,e.e,Nc,parameter)}else{IC<-F.IC(n=n,Estd=Estd,e.e=e.e,Nc=Nc,parameter=parameter)}
    IC.inf<-IC$IC1;IC.sup<-IC$IC2
    if(parameter=="total"){
      Resul<-data.frame(Estd,Vest,e.e,Cve,IC.inf,IC.sup,Ivi)
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
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
