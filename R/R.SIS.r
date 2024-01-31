R.SIS<-function(N,n,r,yk=NULL,zk=NULL,fact=NULL,dk=NULL,type="selec",parameter="total",Nc=0.95,Ek=NULL){
  # Validation of Arguments...............................................................................................................
  if(!(parameter=="total" | parameter=="mean" | parameter=="prop" | parameter=="ratio")){stop("Error, argument in parameter is not valid  \n")}
  if(!(type=="selec" | type=="estm" | type=="estm.Ud")){stop("Error, argument in type is not valid \n")}
  if(missing(N)){stop("Error, the value of N is absent \n")}
  if(!is.wholenumber(N)){stop("Error, N must be a whole number \n")}
  if(missing(n)){stop("Error, the value of n is absent \n")}
  if(!is.wholenumber(n)){stop("Error, n must be a whole number \n")}
  if(missing(r)){stop("Error, the value of r is absent \n")}
  if(!is.wholenumber(r)){stop("Error, r must be a whole number \n")}
  if(type=="estm" | type=="estm.Ud"){
    if(missing(fact)){stop("Error, the vector of fact is absent \n")}
    if(missing(yk)){stop("Error, the vector yk is absent \n")}
    if(length(yk)!=length(fact)){stop("Error, vectors yk and fact are not equal in length \n")}
    if(parameter=="prop"){if(!is.factor(yk)){stop("Error, yk should be a factor \n")}}
    if(parameter=="ratio"){if(missing(zk)){stop("Error, the vector zk is absent \n")}
      if(length(yk)!=length(zk)){stop("Error, vectors yk and zk are not equal in length \n")}}
  }
  if(type=="estm.Ud"){
    if(missing(dk)){stop("Error, the vector of dk is absent \n")}
    if(!is.factor(dk)){stop("Error, yk should be a factor")}
    if(length(yk)!=length(dk)){stop("Error, vectors yk and dk are not equal in length \n")}
    if(length(dk)!=length(fact)){stop("Error, vectors dk and fact are not equal in length \n")}
    if(parameter=="ratio"){if(length(zk)!=length(dk)){stop("Error, vectors zk and dk are not equal in length \n")}}
  }
  if(N<=1){stop("Error, population size is not valid \n")}
  if(n<0){stop("Error, sample size is not valid \n")}
  if(r<0 | r>n){stop("Error, Error, the number of starts is invalid \n")}
  if(n>=N){stop("Error, arguments n and N are not valid, (n<N) \n" )}
  if(!(Nc>0 & Nc<1)){stop("Error, the confidence level is not valid \n")}
  #.....................................................................................................................
  n..r=n%/%r
  a=N%/%n..r
  c<-N-(a*n..r)
  if(type=="selec"){
    if(missing(Ek)){Ek<-runif(n)}
    Ek<-Ek[1:a]
    Sel.MAS<-MAS(N=a,n=r,Ek=Ek,method="cnegativo")
    #Ik<-numeric()
    #nk<-0
    #k<-1
    #while(nk<r){
    #  Ck<-(r-nk)/(a-(k-1))
    #  if(Ek[k]<Ck){Ik[k]<-1}else{Ik[k]<-0}
    # nk<-nk+Ik[k]
    # k<-k+1
    #}
    #rs<-which(Ik==1)
    rs<-Sel.MAS$Ksel
    A<-numeric()
    for(i in 1:n..r){A[i]<-i*a}
    M<-matrix(rep(NA,r*(length(A)+1)),ncol=r,dimnames=list(paste("k",1:(length(A)+1),sep="_"),paste("U",rs,sep="_")))
    for(i in 1:r){
      if(rs[i]<=c){M[,i]<-c(rs[i],rs[i]+A)}else{M[,i]<-c(rs[i],rs[i]+A[-n..r],NA)}
    }
    ht<-numeric()
    for(i in 1:r){ht[i]<-length(na.omit(M[,i]))}
    fact<-rep(1:r,ht)
    sel<-as.vector(na.omit(as.vector(M)))
    n.s<-length(sel)
    return(list(Sel=M,Ksel=sel,fact=fact,n.s=n.s))
  }
  if(type=="estm" & parameter=="total"){
    if(r==1){fact<-rep(1,length(yk))}
    ty.e<-(a/r)*sum(yk)
    Ti<-tapply(yk,fact,sum)
    if(r>1){
      Vest<-((a^2)/r)*(1-(r/a))*var(Ti)
      SCT<-(length(yk)-1)*var(yk)
      SCE<-sum(((tapply(yk,fact,mean)-mean(yk))^2)*tapply(yk,fact,length))
      SCD<-SCT-SCE
      rho<-1-((length(yk)*SCD)/((length(yk)-1)*SCT))
      Ivi<- 1-(((N-1)*SCD)/((N-a)*SCT))
      deef<-1+((N-a)/(a-1))*Ivi
    }else{
      n.s<-length(yk)
      Vest<-((N^2)/n)*(1-(n/N))*var(yk)
    }
    e.e<-sqrt(Vest)
    Cve<-(sqrt(Vest)/ty.e)*100
    IC<-F.IC(n=n,Estd=ty.e,e.e=e.e,Nc=Nc,parameter=parameter)
    IC.inf<-IC$IC1;IC.sup<-IC$IC2
    if(r>1){Resul<-data.frame(ty.e,Vest,e.e,Cve,IC.inf,IC.sup,rho,Ivi,deef)
    names(Resul)<-c(parameter,"Vest","e.e","Cve","IC.inf","IC.sup","rho","Ivi","deef")
    }else{Resul<-data.frame(ty.e,Vest,e.e,Cve,IC.inf,IC.sup)
    names(Resul)<-c(parameter,"Vest","e.e","Cve","IC.inf","IC.sup")}
    return(list(Estimation=Resul))
  }
  if((type=="estm" & parameter!="total")| type=="estm.Ud"){
    if(type=="estm"){dk<-as.matrix(rep(1,length(yk)))}
    if(parameter!="ratio"){zk<-rep(1,length(yk))}
    dk<-Dk(dk)
    if(parameter=="prop"){yk<-as.factor(yk);ydk<-PRODM(dk,Dk(yk))}else{ydk<-yk*dk}
    if(parameter=="prop"){zk<-matrix(rep(zk,ncol(Dk(yk))),ncol=ncol(Dk(yk)));zdk<-PRODM(dk,zk)}else{zdk<-zk*dk}
    if(r==1){fact<-rep(1,length(yk))}
    if(parameter=="total"){
      Estd<-(a/r)*apply(ydk,2,sum)
      Ti<-apply(ydk,2,tapply,fact,sum)
      if(r>1){Vest<-((a^2)/r)*(1-(r/a))*apply(Ti,2,var)
      }else{
        Vest<-((N^2)/n)*(1-(n/N))*apply(ydk,2,var)
      }
    }else{
      tdy.e<-(a/r)*apply(ydk,2,sum)
      tdz.e<-(a/r)*apply(zdk,2,sum)
      Estd<-tdy.e/tdz.e
      udk.e<-matrix(0,ncol=length(Estd),nrow=length(yk));for(i in 1:length(Estd)){udk.e[,i]<-(ydk[,i]-Estd[i]*zdk[,i])/tdz.e[i]}
      Tduki<-apply(udk.e,2,tapply,fact,sum)
      if(r>1){Vest<-((a^2)/r)*(1-(r/a))*apply(Tduki,2,var)
      }else{
        Vest<-((N^2)/n)*(1-(n/N))*apply(udk.e,2,var)
      }
    }
    e.e<-sqrt(Vest)
    Cve<-(sqrt(Vest)/Estd)*100
    if(parameter=="prop"){IC<-F.IC(n,tdy.e,tdz.e,Estd,e.e,Nc,parameter)}else{IC<-F.IC(n=n,Estd=Estd,e.e=e.e,Nc=Nc,parameter=parameter)}       #,tdy.e=tdy.e
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


