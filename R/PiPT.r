
PiPT<-function(xk,n,yk=NULL,zk=NULL,pik=NULL,mpikl=NULL,dk=NULL,type="selec",parameter="total",Nc=0.95,Ek=NULL){
  # Validation of Arguments....................................................................................................................
  if(!(type=="selec" | type=="estm" | type=="estm.Ud")){stop("Error, argument in type is not valid \n")}
  if(!(parameter=="total" | parameter=="mean" | parameter=="prop" | parameter=="ratio")){stop("Error, argument in parameter is not valid \n")}
  if(type=="selec"){
    if(missing(xk)){stop("Error, the vector xk is absent \n")}
    if(missing(n)){stop("Error, the value of n is absent \n")}
    if(!is.wholenumber(n)){stop("Error, N must be a whole number \n")}
    if(length(xk)<2){stop("Error, population size is not valid \n")}
    if(n<2 | n>length(xk)){stop("Error, sample size is not valid \n")}
  }
  if(type=="estm" | type=="estm.Ud"){
    if(missing(yk)){stop("Error, the vector yk is absent \n")}
    if(parameter=="prop"){if(!is.factor(yk)){stop("Error, yk should be a factor")}}
    if(missing(pik)){stop("Error, the first-order inclusion probability vector is absent (pik) \n" )}
    if(missing(mpikl)){stop("Error, the inclusion probability matrix second order is absent \n")}
    if(dim(mpikl)[1]!=dim(mpikl)[2] | dim(mpikl)[1]!=length(pik)){stop("Error, the second-order inclusion probability matrix is not valid \n")}
    if(length(pik)!=length(yk)){stop("Error, vectors yk and pik are not equal in length \n")}
    if(parameter=="ratio"){if(missing(zk)){stop("Error, the vector zk is absent \n")}
      if(length(zk)!=length(yk)){stop("Error, vectors yk and zk are not equal in length \n")}}
    if(!(Nc>0 & Nc<1)){stop("Error, the confidence level is not valid \n")}
  }
  if(type=="estm.Ud"){
    if(missing(dk)){stop("Error, the vector dk is absent \n")}
    if(!is.factor(dk)){stop("Error, dk should be a factor")}
    if(length(yk)!=length(dk)){stop("Error, vectors yk and dk are not equal in length \n")}
    if(length(pik)!=length(dk)){stop("Error, vectors pik and dk are not equal in length \n")}
    if(parameter=="ratio"){if(length(zk)!=length(dk)){stop("Error, vectors zk and dk are not equal in length \n")}}
  }

  #.....................................................................................................................

  if(type=="selec"){
    if(missing(Ek)){Ek<-runif(length(xk))}
    if(n==2){
      N<-length(xk)
      xk1<-xk
      tx<-sum(xk)
      Ksel<-numeric()
      k<-1:N
      tx1<-sum(xk1)
      Ck<-(xk1*(tx1-xk1))/(tx1*(tx1-2*xk1))
      pk<-Ck/sum(Ck)
      Pk.acum<-cumsum(pk)
      Li<-c(0.0001,Pk.acum[-N]+0.0001)
      Ls<-c(Pk.acum[-N],0.9999)
      Ksel[1]<-which(Ek[1]<Ls & Ek[1]>Li)[1]
      xk2<-xk[-Ksel[1]]
      k<-k[-Ksel[1]]
      N<-N-1
      tx2<-sum(xk2)
      pk<-xk2/tx2
      Pk.acum<-cumsum(pk)
      Li<-c(0.0001,Pk.acum[-N]+0.0001)
      Ls<-c(Pk.acum[-N],0.9999)
      Ksel[2]<-k[which(Ek[2]<Ls & Ek[2]>Li)[1]]
      Ksel<-sort(Ksel)
      Ik<-rep(0,N);Ik[Ksel]<-1
      piksel<-(2*xk[Ksel])/tx
      pikl<-(2*xk[Ksel[1]]*xk[Ksel[2]]*(tx-xk[Ksel[1]]-xk[Ksel[2]]))/(tx*sum(Ck)*(tx-2*xk[Ksel[1]])*(tx-2*xk[Ksel[2]]))
      mpikl.s<-matrix(c(piksel[1],pikl,pikl,piksel[2]),ncol=2,dimnames=list(paste("K",Ksel,sep="="),paste("L",Ksel,sep="=")))
      return(list(Ksel=Ksel,piksel=piksel,mpikl.s=mpikl.s))
    }

    if(n>2 & n<length(xk)){
      N<-length(xk)
      xkord<-sort.int(xk,decreasing=T,index.return=TRUE) ### Ordenamiento Decreciente
      Ind<-xkord$ix
      tk<-numeric(0)
      tk[1]<-sum(xkord$x)
      for (i in 2:N ){
        tk[i]<-sum(xkord$x[-c(1:i-1)])
      }
      Ck<-(n*xkord$x)/tk
      Ek<-Ek[Ind]
      k1<-which(Ck<1)
      k2<-which(Ck>=1)
      Ek1<-Ek[k1]
      Ek2<-Ek[k2]
      nk<-0
      muestra1<-numeric()
      for(i in k1){
        if((((n-nk)*xkord$x[i])/tk[i])>Ek1[i] & nk<n ){
          muestra1[i]<-1
          nk<-nk+1
        }else{
          muestra1[i]<-0
          nk<-nk+0
        }
      }
      ##Selection for ck>=Ko (Fann Muller)
      nj<-length(which(muestra1!=0))
      j<-k2[1]
      muestra2<-numeric()
      for(i in 1:length(k2)){
        if(((n-nj)/(N-j+1))>Ek2[i] & nj<n){
          muestra2[i]<-1
          nj<-nj+1
          j=j+1
        }else{
          muestra2[i]<-0
          nj<-nj+0
          j=j+1
        }
      }
      Ikord<-c(muestra1,muestra2)
      ksel<-which(Ikord==1)
      Ksel<-sort(Ind[ksel])
      Ik<-rep(0,N);Ik[Ksel]<-1
      Ik
      #####...................... ...los pik.................................................................................... ........
      pik1<-(n*xkord$x[k1])/sum(xk)
      pik2<-rep((n*mean(xkord$x[k2]))/sum(xk),length(k2))
      pik<-c(pik1,pik2)
      piksel<-pik[ksel]
      ###.........................pikl (k<l).............................................................................................
      ko<-k2[1]
      pikl<-function(k,l){
        Tx<-sum(xk)
        gk<-numeric(0)
        gk[1]<-1/tk[2]
        for(i in 2:(N-1)){gk[i]<-gk[i-1]*((tk[i]-xkord$x[i-1])/tk[i+1])}
        if(1<=k & k<l & l<ko){pikl<-((n*(n-1))/Tx)*gk[k]*xkord$x[k]*xkord$x[l]}
        if(1<=k & k<ko & ko<=l & l<=N){pikl<-((n*(n-1))/Tx)*gk[k]*xkord$x[k]*mean(xkord$x[ko:N])}
        if(ko<=k & k<l & l<=N){
          pikl<-((n*(n-1))/Tx)*gk[ko-1]*((tk[ko]-xkord$x[ko-1])/(tk[ko]-mean(xkord$x[ko:N])))*(mean(xkord$x[ko:N])^2)
        }
        if(k>l | k==l){pikl<-NA}
        return(pikl)
      }
      ##.................................pikl...............................................................................................
      Pikl<-function(k,l){
        if(k==l){Pikl<-pik[k]}
        if(k<l){Pikl<-pikl(k,l)}
        if(k>l){Pikl<-pikl(l,k)}
        return(Pikl)
      }
      ### Matrix of second-order inclusion probabilities.
      mpikl.s<-matrix(0,ncol=n,nrow=n,dimnames=list(paste("K",Ksel,sep="="),paste("L",Ksel,sep="=")))
      for(i in 1:n){
        for(j in 1:n){
          mpikl.s[i,j]<-Pikl(ksel[i],ksel[j])
        }
      }
      mpikl.s
      return(list(Ksel=Ksel,piksel=piksel,mpikl.s=mpikl.s))
    }
  }
  #.....................................Estimation..........................................................
  Vty<-function(yk,pik,mpikl){
    z<-1
    v<- numeric()
    for(i in 1:n){
      for(j in 1:n){
        v[z]<-(1-(pik[i]*pik[j]/mpikl[i,j]))*(((yk[i]/pik[i])-(yk[j]/pik[j]))^2)
        z<-z+1
      }
    }
    Vest<-(-1/2)*sum(v)
    return(Vest)
  }

  ty<-function(yk,pik){t<-sum(yk/pik);return(t)}
  #...................................................................
  if(type=="estm" | type=="estm.Ud"){
    n<-length(yk)
    if(type=="estm"){dk<-as.matrix(rep(1,length(yk)))}
    if(parameter!="ratio"){zk<-rep(1,length(yk))}
    dk<-Dk(dk)
    if(parameter=="prop"){yk<-as.factor(yk);ydk<-PRODM(dk,Dk(yk))}else{ydk<-yk*dk}
    if(parameter=="prop"){zk<-matrix(rep(zk,ncol(Dk(yk))),ncol=ncol(Dk(yk)));zdk<-PRODM(dk,zk)}else{zdk<-zk*dk}
    if(parameter=="total"){
      Estd<-apply(ydk,2,ty,pik)
      Vest<-apply(ydk,2,Vty,pik,mpikl)
    }else{
      tdy.e<-apply(ydk,2,ty,pik)
      tdz.e<-apply(zdk,2,ty,pik)
      Estd<-tdy.e/tdz.e
      udk.e<-matrix(0,ncol=length(Estd),nrow=length(yk));for(i in 1:length(Estd)){udk.e[,i]<-(ydk[,i]-Estd[i]*zdk[,i])/tdz.e[i]}
      Vest<-apply(udk.e,2,Vty,pik,mpikl)
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

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
