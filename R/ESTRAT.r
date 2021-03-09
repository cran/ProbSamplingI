
ESTRAT<-function(strata,designs,p,xk=NULL,yk=NULL,zk=NULL,dk=NULL,type="selec",Argt,parameter="total",rh=NULL,Ek=NULL,Nc=0.95){
  H<-length(table(strata))
  # Validation of Arguments............................................................................................................
  V.designs<-function(designs){
    r<-length(designs)
    V<-numeric()
    for(i in 1:r){
      if(designs[i]=="BER" | designs[i]=="MAS" | designs[i]=="MCR"  | designs[i]=="R.SIS"  | designs[i]=="PPT" | designs[i]=="PiPT"){
        V[i]<-1
      }else{V[i]<-0}
    }
    if(sum(V)==r){K<-TRUE}else{K<-FALSE}
    return(K)
  }

  if(!(type=="selec" | type=="estm"| type=="estm.Ud")){stop("Error, argument in type is not valid \n")}
  if(!(parameter=="total" | parameter=="mean" | parameter=="prop" | parameter=="ratio")){stop("Error, argument in parameter is not valid \n")}
  if(missing(strata)){stop("Error, the vector estrato is absent \n")}
  if(missing(designs)){stop("Error, the vector designs is absent \n")}
  if(V.designs(designs)==FALSE){stop("Error, one of the designs is not valid\n")}
  if(type=="selec"){
    if(missing(p)){stop("Error, the vector p is absent \n")}
    if(length(which(designs=="PiPT" | designs=="PPT"))!=0){if(missing(xk)){stop("Error, the vector xk is absent \n")}
      if(length(xk)!=length(strata)){stop("Error, vectors xk and strata are not equal in length \n")}}
  }
  if(type=="estm" | type=="estm.Ud"){
    if(missing(yk)){stop("Error, the vector yk is absent \n")}
    if(length(strata)!=length(yk)){stop("Error, vectors yk and strata are not equal in length \n")}
    if(parameter=="prop"){if(!is.factor(yk)){stop("Error, yk should be a factor")}}
    if(parameter=="ratio"){if(missing(zk)){stop("Error, the vector xk is absent \n")}
      if(length(zk)!=length(yk)){stop("Error, vectors yk and zk are not equal in length \n")}}
  }
  if(type=="estm.Ud"){
    if(missing(dk)){stop("Error, the vector dk is absent \n")}
    if(!is.factor(dk)){stop("Error, dk should be a factor")}
    if(length(yk)!=length(dk)){stop("Error, vectors yk and dk are not equal in length \n")}
    if(parameter=="ratio"){if(length(zk)!=length(dk)){stop("Error, vectors zk and dk are not equal in length \n")}}
  }
  if(!(Nc>0 & Nc<1)){stop("Error, the confidence level is not valid \n")}

  #........................................................................................................
  if(type=="selec"){
    if(missing(Ek)){Ek<-runif(length(strata))}
    POShk<-function(vec1,vec2,h,vec3){
      pos<-numeric()
      j<-1
      for(i in vec3){
        pos[j]<-which(vec1==h & vec2==i)
        j<-j+1
      }
      return(pos)
    }

    Nh<-as.vector(tapply(rep(1,length(strata)),strata,sum))
    INDh<-sequence(as.vector(Nh))
    datos<-data.frame(IND=1:length(strata),strata,INDh)
    nh<-ceiling(p*Nh)
    Rt.h<-list()
    j<-1
    for(i in 1:H){
      if(designs[i]=="MAS"){selh<-MAS(N=Nh[i],n=nh[i],Ek=Ek[which(strata==i)]);Rt.h[[i]]<-data.frame(Nh=Nh[i],nh=nh[i])}
      if(designs[i]=="MCR"){selh<-MCR(N=Nh[i],m=nh[i],Ek=Ek[which(strata==i)]);Rt.h[[i]]<-data.frame(Nh=Nh[i],mh=nh[i])}
      if(designs[i]=="BER"){selh<-BER(N=Nh[i],Pi=p[i],Ek=Ek[which(strata==i)]);Rt.h[[i]]<-data.frame(Pi=p[i])}
      if(designs[i]=="R.SIS"){selh<-R.SIS(N=Nh[i],n=nh[i],r=rh[i],Ek=Ek[which(strata==i)]);Rt.h[[i]]<-list(Nh=Nh[i],nh=nh[i],rh=rh[i],fact=selh$fact)}      #,fact=selh$fact)
      if(designs[i]=="PPT"){xkh<-xk[strata==i];selh<-PPT(xk=xkh,m=nh[i],Ek=Ek[which(strata==i)]);Rt.h[[i]]<-selh[2]}
      if(designs[i]=="PiPT"){xkh<-xk[strata==i];selh<-PiPT(xk=xkh,n=nh[i],Ek=Ek[which(strata==i)]);Rt.h[[i]]<-selh[2:3]}
      if(j==1){M<-datos[POShk(strata,INDh,i,selh$Ksel),]}
      if(j>1){M1<-datos[POShk(strata,INDh,i,selh$Ksel),];M<-rbind(M,M1)}
      j=j+1
    }
    return(list(Sample=M,Rtdos.h=Rt.h))
  }

  #................................................................................................................
  P.ESTRAT<-function(yk,zk,dk,Argt,designs,parameter,type,Nc){
    if(type=="estm"){if(parameter=="prop"){c<-7}else{c<-6}}
    if(type=="estm.Ud"){if(parameter=="prop"){c<-8}else{c<-6}}
    Resul.est<-list()
    for(i in 1:H){
      if(designs[i]=="BER"){
        if(parameter!="ratio" & type=="estm"){Resul.est[[i]]<-BER(yk=yk[strata==i],Pi=Argt[[i]]$Pi,type=type,parameter=parameter,Nc=Nc)$Estimation[,1:c]}
        if(parameter!="ratio" & type=="estm.Ud"){Resul.est[[i]]<-BER(yk=yk[strata==i],dk=dk[strata==i],Pi=Argt[[i]]$Pi,type=type,parameter=parameter,Nc=Nc)$Estimation[,1:c]}
        if(parameter=="ratio" & type=="estm"){Resul.est[[i]]<-BER(yk=yk[strata==i],zk=zk[strata==i],Pi=Argt[[i]]$Pi,type=type,parameter=parameter,Nc=Nc)$Estimation[,1:c]}
        if(parameter=="ratio" & type=="estm.Ud"){Resul.est[[i]]<-BER(yk=yk[strata==i],zk=zk[strata==i],dk=dk[strata==i],Pi=Argt[[i]]$Pi,type=type,parameter=parameter,Nc=Nc)$Estimation[,1:c]}
      }
      if(designs[i]=="MCR"){
        if(parameter!="ratio" & type=="estm"){Resul.est[[i]]<-MCR(yk=yk[strata==i],N=Argt[[i]]$N,m=Argt[[i]]$m,type=type,parameter=parameter,Nc=Nc)$Estimation[,1:c]}
        if(parameter!="ratio" & type=="estm.Ud"){Resul.est[[i]]<-MCR(yk=yk[strata==i],dk=dk[strata==i],N=Argt[[i]]$N,m=Argt[[i]]$m,type=type,parameter=parameter,Nc=Nc)$Estimation[,1:c]}
        if(parameter=="ratio" & type=="estm"){Resul.est[[i]]<-MCR(yk=yk[strata==i],zk=zk[strata==i],N=Argt[[i]]$N,m=Argt[[i]]$m,type=type,parameter=parameter,Nc=Nc)$Estimation[,1:c]}
        if(parameter=="ratio" & type=="estm.Ud"){Resul.est[[i]]<-MCR(yk=yk[strata==i],zk=zk[strata==i],dk=dk[strata==i],N=Argt[[i]]$N,m=Argt[[i]]$m,type=type,parameter=parameter,Nc=Nc)$Estimation[,1:c]}
      }
      if(designs[i]=="MAS"){
        if(parameter!="ratio" & type=="estm"){Resul.est[[i]]<-MAS(yk=yk[strata==i],N=Argt[[i]]$N,n=Argt[[i]]$n,type=type,parameter=parameter,Nc=Nc)$Estimation[,1:c]}
        if(parameter!="ratio" & type=="estm.Ud"){Resul.est[[i]]<-MAS(yk=yk[strata==i],dk=dk[strata==i],N=Argt[[i]]$N,n=Argt[[i]]$n,type=type,parameter=parameter,Nc=Nc)$Estimation[,1:c]}
        if(parameter=="ratio" & type=="estm"){Resul.est[[i]]<-MAS(yk=yk[strata==i],zk=zk[strata==i],N=Argt[[i]]$N,n=Argt[[i]]$n,type=type,parameter=parameter,Nc=Nc)$Estimation[,1:c]}
        if(parameter=="ratio" & type=="estm.Ud"){Resul.est[[i]]<-MAS(yk=yk[strata==i],zk=zk[strata==i],dk=dk[strata==i],N=Argt[[i]]$N,n=Argt[[i]]$n,type=type,parameter=parameter,Nc=Nc)$Estimation[,1:c]}
      }
      if(designs[i]=="R.SIS"){
        if(parameter!="ratio" & type=="estm"){Resul.est[[i]]<-R.SIS(yk=yk[strata==i],n=Argt[[i]]$n,N=Argt[[i]]$N,r=Argt[[i]]$r,fact=Argt[[i]]$fact,type=type,parameter=parameter,Nc=Nc)$Estimation[,1:c]}
        if(parameter!="ratio" & type=="estm.Ud"){Resul.est[[i]]<-R.SIS(yk=yk[strata==i],dk=dk[strata==i],n=Argt[[i]]$n,N=Argt[[i]]$N,r=Argt[[i]]$r,fact=Argt[[i]]$fact,type=type,parameter=parameter,Nc=Nc)$Estimation[,1:c]}
        if(parameter=="ratio" & type=="estm"){Resul.est[[i]]<-R.SIS(yk=yk[strata==i],zk=zk[strata==i],n=Argt[[i]]$n,N=Argt[[i]]$N,r=Argt[[i]]$r,fact=Argt[[i]]$fact,type=type,parameter=parameter,Nc=Nc)$Estimation[,1:c]}
        if(parameter=="ratio" & type=="estm.Ud"){Resul.est[[i]]<-R.SIS(yk=yk[strata==i],zk=zk[strata==i],dk=dk[strata==i],n=Argt[[i]]$n,N=Argt[[i]]$N,r=Argt[[i]]$r,fact=Argt[[i]]$fact,type=type,parameter=parameter,Nc=Nc)$Estimation[,1:c]}
      }
      if(designs[i]=="PPT"){
        if(parameter!="ratio" & type=="estm"){Resul.est[[i]]<-PPT(yk=yk[strata==i],pk=Argt[[i]]$pk,type=type,parameter=parameter,Nc=Nc)$Estimation[,1:c]}
        if(parameter!="ratio" & type=="estm.Ud"){Resul.est[[i]]<-PPT(yk=yk[strata==i],dk=dk[strata==i],pk=Argt[[i]]$pk,type=type,parameter=parameter,Nc=Nc)$Estimation[,1:c]}
        if(parameter=="ratio" & type=="estm"){Resul.est[[i]]<-PPT(yk=yk[strata==i],zk=zk[strata==i],pk=Argt[[i]]$pk,type=type,parameter=parameter,Nc=Nc)$Estimation[,1:c]}
        if(parameter=="ratio" & type=="estm.Ud"){Resul.est[[i]]<-PPT(yk=yk[strata==i],zk=zk[strata==i],dk=dk[strata==i],pk=Argt[[i]]$pk,type=type,parameter=parameter,Nc=Nc)$Estimation[,1:c]}
      }
      if(designs[i]=="PiPT"){
        if(parameter!="ratio" & type=="estm"){Resul.est[[i]]<-PiPT(yk=yk[strata==i],pik=Argt[[i]]$pik,mpikl=Argt[[i]]$mpikl,type=type,parameter=parameter,Nc=Nc)$Estimation[,1:c]}
        if(parameter!="ratio" & type=="estm.Ud"){Resul.est[[i]]<-PiPT(yk=yk[strata==i],dk=dk[strata==i],pik=Argt[[i]]$pik,mpikl=Argt[[i]]$mpikl,type=type,parameter=parameter,Nc=Nc)$Estimation[,1:c]}
        if(parameter=="ratio" & type=="estm"){Resul.est[[i]]<-PiPT(yk=yk[strata==i],zk=zk[strata==i],pik=Argt[[i]]$pik,mpikl=Argt[[i]]$mpikl,type=type,parameter=parameter,Nc=Nc)$Estimation[,1:c]}
        if(parameter=="ratio" & type=="estm.Ud"){Resul.est[[i]]<-PiPT(yk=yk[strata==i],zk=zk[strata==i],dk=dk[strata==i],pik=Argt[[i]]$pik,mpikl=Argt[[i]]$mpikl,type=type,parameter=parameter,Nc=Nc)$Estimation[,1:c]}
      }
    }
    k<-numeric()
    Resul1.h<-Resul.est[[1]]
    k[1]<-dim(Resul.est[[1]])[1]
    for(i in 2:H){
      k[i]<-dim(Resul.est[[i]])[1]
      Resul1.h<-rbind(Resul1.h,Resul.est[[i]])
    }
    Estrato<-rep(1:H,k)
    Resul1.h<-cbind(Estrato,Resul1.h)
    return(Resul.h=Resul1.h)
  }

  #.................................
  if(type=="estm" | type=="estm.Ud"){
    n<-length(yk)
    if(parameter=="total"){
      if(type=="estm"){dk<-rep(1,length(yk))}
      dk<-as.factor(dk)
      Ud<-rep(levels(dk),H)
      Resultados.h<-P.ESTRAT(yk=yk,dk=dk,Argt=Argt,designs=designs,parameter="total",type=type,Nc=Nc)
      rownames(Resultados.h)<-NULL
      if(type=="estm.Ud"){Resultados.h<-cbind(Ud,Resultados.h); Resultados.h<-Resultados.h[,c(2,1,3:8)]}
      ty.e<-tapply(Resultados.h$total,Ud,sum)
      Vest<-tapply(Resultados.h$Vest,Ud,sum)
      e.e<-sqrt(Vest)
      Cve<-(sqrt(Vest)/ty.e)*100
      IC<-F.IC(n=n,Estd=ty.e,e.e=e.e,Nc=Nc,parameter=parameter)
      IC.inf<-IC$IC1;IC.sup<-IC$IC2
      Resultados<-data.frame(total=ty.e,Vest,e.e,Cve,IC.inf,IC.sup)
      return(list(Resultados.h=Resultados.h,Resultados=Resultados))
    }
    if(parameter=="mean"){
      yk1<-rep(1,length(yk))
      if(type=="estm"){dk<-rep(1,length(yk))}
      dk<-as.factor(dk)
      Ud<-rep(levels(dk),H)
      Resultados.h<-P.ESTRAT(yk=yk,dk=dk,Argt=Argt,designs=designs,parameter="mean",type=type,Nc=Nc)
      rownames(Resultados.h)<-NULL
      if(type=="estm.Ud"){Resultados.h<-cbind(Ud,Resultados.h); Resultados.h<-Resultados.h[,c(2,1,3:8)]}
      ResultadosT.h<-P.ESTRAT(yk=yk,dk=dk,Argt=Argt,designs=designs,parameter="total",type=type,Nc=Nc)
      Resultados1.h<-P.ESTRAT(yk=yk1,dk=dk,Argt=Argt,designs=designs,parameter="total",type=type,Nc=Nc)
      ty.e<-tapply(ResultadosT.h$total,Ud,sum)
      N.e<-tapply(Resultados1.h$total,Ud,sum)
      yb.e<-ty.e/N.e
      Vest<-numeric()
      for(i in 1:length(yb.e)){
        uk.e<-((Dk(dk)*yk)[,i]-(yb.e[i])*(Dk(dk)[,i]))/N.e[i]
        Vest[i]<-sum(P.ESTRAT(yk=uk.e,Argt=Argt,designs=designs,parameter="total",type="estm",Nc=Nc)$Vest)
      }
      e.e<-sqrt(Vest)
      Cve<-(sqrt(Vest)/yb.e)*100
      IC<-F.IC(n=n,Estd=yb.e,e.e=e.e,Nc=Nc,parameter=parameter)
      IC.inf<-IC$IC1;IC.sup<-IC$IC2
      Resultados<-data.frame(mean=yb.e,Vest,e.e,Cve,IC.inf,IC.sup)
      return(list(Resultados.h=Resultados.h,Resultados=Resultados))
    }
    if(parameter=="prop"){
      yk<-as.factor(yk)
      if(type=="estm"){dk<-rep(1,length(yk))}
      dk<-as.factor(dk)
      ly<-levels(yk);ld<-levels(dk)
      yk1<-rep(1,length(yk))
      Resultados.h<-P.ESTRAT(yk=yk,dk=dk,Argt=Argt,designs=designs,parameter="prop",type=type,Nc=Nc)
      Resultados1.h<-P.ESTRAT(yk=yk1,dk=dk,Argt=Argt,designs=designs,parameter="total",type=type,Nc=Nc)
      Ud<-rep(ld,H)
      t<-matrix(0,ncol=length(ly),nrow=length(ld),dimnames=list(ld,ly))
      for(i in 1:dim(Dk(yk))[2]){
        t[,i]<-tapply(P.ESTRAT(yk=Dk(yk)[,i],dk=dk,Argt=Argt,designs=designs,parameter="total",type=type,Nc=Nc)$total,Ud,sum)
      }
      p<-t(prop.table(t,1))
      P.e<-as.vector(p)
      N.e<-tapply(Resultados1.h$total,Ud,sum)
      Vest<-numeric()
      k<-1
      for(j in 1:length(ld)){
        for(i in 1:length(ly)){
          uk.e<-(Dk(yk)[i,]*Dk(dk)[,j]-p[i,j]*Dk(dk)[,j])/N.e[j]
          Vest[k]<-sum(P.ESTRAT(yk=uk.e,Argt=Argt,designs=designs,parameter="total",type="estm",Nc=Nc)$Vest)
          k<-k+1
        }
      }
      e.e<-sqrt(Vest)
      Cve<-(sqrt(Vest)/P.e)*100
      IC<-F.IC(n,P.e*N.e,N.e,P.e,e.e,Nc,parameter)
      IC.inf<-IC$IC1;IC.sup<-IC$IC2
      Ud<-rep(ld,each=length(ly));Uz=rep(ly,length(ld))
      if(type=="estm.Ud"){Resultados<-data.frame(Ud=Ud,Uz=Uz,prop=P.e,Vest,e.e,Cve,IC.inf,IC.sup)
      }else{Resultados<-data.frame(Uz=Uz,prop=P.e,Vest,e.e,Cve,IC.inf,IC.sup)}
      return(list(Resultados.h=Resultados.h,Resultados=Resultados))
    }
    if(parameter=="ratio"){
      if(type=="estm"){dk<-rep(1,length(yk))}
      dk<-as.factor(dk)
      Ud<-rep(levels(dk),H)
      Resultados.h<-P.ESTRAT(yk=yk,zk=zk,dk=dk,Argt=Argt,designs=designs,parameter="ratio",type=type,Nc=Nc)
      rownames(Resultados.h)<-NULL
      if(type=="estm.Ud"){Resultados.h<-cbind(Ud,Resultados.h); Resultados.h<-Resultados.h[,c(2,1,3:8)]}
      ResultadosY.h<-P.ESTRAT(yk=yk,dk=dk,Argt=Argt,designs=designs,parameter="total",type=type,Nc=Nc)
      ResultadosZ.h<-P.ESTRAT(yk=zk,dk=dk,Argt=Argt,designs=designs,parameter="total",type=type,Nc=Nc)
      ty.e<-tapply(ResultadosY.h$total,Ud,sum)
      tz.e<-tapply(ResultadosZ.h$total,Ud,sum)
      R.e<-ty.e/tz.e
      Vest<-numeric()
      for(i in 1:length(R.e)){
        uk.e<-((Dk(dk)*yk)[,i]-(R.e[i])*(Dk(dk)*zk)[,i])/tz.e[i]
        Vest[i]<-sum(P.ESTRAT(yk=uk.e,Argt=Argt,designs=designs,parameter="total",type="estm",Nc=Nc)$Vest)
      }
      e.e<-sqrt(Vest)
      Cve<-(sqrt(Vest)/R.e)*100
      IC<-F.IC(n=n,Estd=R.e,e.e=e.e,Nc=Nc,parameter=parameter)
      IC.inf<-IC$IC1;IC.sup<-IC$IC2
      Resultados<-data.frame(ratio=R.e,Vest,e.e,Cve,IC.inf,IC.sup)
      return(list(Results.h=Resultados.h,Results=Resultados))
    }
  }
}
