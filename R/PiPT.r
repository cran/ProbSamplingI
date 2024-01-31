
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
    N<-length(xk)
    if(n==2){
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
      Tx<-sum(xk)
      k<-1:N
      xkord<-sort.int(xk,decreasing=T,index.return=TRUE)
      Ind<-xkord$ix
      Dat_ord<-data.frame(k,xk)[Ind,]
      xkord<-Dat_ord$xk
      tk<-numeric()
      tk[1]<-sum(xkord)
      for (i in 2:N ){tk[i]<-sum(xkord[-c(1:i-1)])}
      Ck<-(n*xkord)/tk
      Dat_ord<-cbind(Dat_ord,tk,Ck)
      U1<-Dat_ord[which(Ck<1),]
      U2<-Dat_ord[which(Ck>=1),]
      #-------para ck<1--------------------
      nk<-0
      Ek1<-Ek[1:nrow(U1)]
      xk<-U1$xk
      tk<-U1$tk
      Ik<-numeric()
      Fk<-numeric()
      for(i in 1:nrow(U1)){
        Fk[i]<-((n-nk)*xk[i])/tk[i]
        if(Ek1[i]<Fk[i]){Ik[i]<-1}else{Ik[i]<-0}
        nk<-nk+Ik[i]
      }
      k_sel1<-U1$k[Ik==1]
      ksel<-sort(k_sel1)
      n1<-nk
      ##Selection for ck>=Ko (Fann Muller)
      if(n1<n){
        N2<-nrow(U2)
        n2<-n-nk
        Ek2<-Ek[(nrow(U1)+1):nrow(Dat_ord)]
        sel2<-MAS(N=N2,n=n2,Ek=Ek2,method="fmuller")
        k_sel2<-U2$k[sel2$Ksel]
        ksel<-sort(c(k_sel1,k_sel2))
      }
      #####...................... ...los pik.................................................................................... ........
      pik<-numeric()
      for(i in 1:n){
        if(sum(ksel[i]==U1$k)==1){pik[i]<-n*(U1$xk[which(ksel[i]==U1$k)]/Tx)}else{
          pik[i]<-n*(mean(U2$xk)/Tx)
        }
      }
      PIK<-data.frame(ksel,pik)
      ###.........................pikl (k<l).............................................................................................
      tk<-Dat_ord$tk
      gk<-numeric()
      gk[1]<-1/tk[2]
      for(i in 2:(N-1)){gk[i]<-gk[i-1]*(tk[i]-xkord[i-1])/tk[i+1]}
      F.in<-function(k,U){ifelse(sum(k==U)==1,TRUE,FALSE)}
      m.pikl<-matrix(0,ncol=n,nrow=n,dimnames=list(paste("k",ksel,sep="="),paste("l",ksel,sep="=")))
      for(i in 1:n){
        for(j in i:n){
          if(i==j){m.pikl[i,j]<-pik[i]}else{
            if(F.in(ksel[i],U1$k) & F.in(ksel[j],U1$k)){
              kp<-which(Dat_ord$k==ksel[i])
              lp<-which(Dat_ord$k==ksel[j])
              m.pikl[i,j]<-n*(n-1)*gk[kp]*xkord[kp]*xkord[lp]/Tx
              m.pikl[j,i]<-m.pikl[i,j]
            }
            if(F.in(ksel[i],U1$k) & F.in(ksel[j],U2$k) ){
              kp<-which(Dat_ord$k==ksel[i])
              lp<-which(Dat_ord$k==ksel[j])
              m.pikl[i,j]<-n*(n-1)*gk[kp]*xkord[kp]*mean(U2$xk)/Tx
              m.pikl[j,i]<-m.pikl[i,j]
            }
            if(F.in(ksel[i],U2$k) & F.in(ksel[j],U2$k) ){
              kp<-which(Dat_ord$k==ksel[i])
              lp<-which(Dat_ord$k==ksel[j])
              ka<-nrow(U1)+1
              m.pikl[i,j]<-(n*(n-1)*gk[kp-1]/Tx)*((tk[ka]-xkord[ka-1])/(tk[ka]-mean(U2$xk)))*(mean(U2$xk)^2)
              m.pikl[j,i]<-m.pikl[i,j]
            }

          }
        }
      }
      m.pikl
      return(list(Ksel=ksel,piksel=pik,mpikl.s=m.pikl))
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
