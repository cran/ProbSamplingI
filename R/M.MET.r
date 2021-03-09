M.MET<-function(F.UM,designs,list.arg,p,type="selec",parameter="total",yk=NULL,zk=NULL,xk=NULL,dk=NULL,r=NULL,Nc=0.95){
  # Required Functions......................................................................................................................
  V.designs<-function(designs){
    r<-length(designs)
    V<-numeric()
    for(i in 1:r){
      if(designs[i]=="MAS" | designs[i]=="MCR"  | designs[i]=="R.SIS"  | designs[i]=="PPT" | designs[i]=="PiPT"){
        V[i]<-1
      }else{V[i]<-0}
    }
    if(sum(V)==r){K<-TRUE}else{K<-FALSE}
    return(K)
  }
  #................................................................
  CONT.F<-function(Vect1,Vect2){
    cont<-numeric()
    T1<-tapply(Vect2,Vect1,table)
    for(i in 1:length(T1)){cont[i]<-length(T1[[i]])}
    return(cont)
  }
  POS<-function(vec1,vec2){
    if(length(vec2)==1){vec3<-which(vec1==vec2)
    }else{
      vec3<-which(vec1==vec2[1])
      for(i in 2:length(vec2)){
        vec3<-c(vec3,which(vec1==vec2[i]))}
    }
    return(vec3)
  }
  #.................................................
  NF<-function(x){
    xpos<-which(x[-length(x)]!=x[-1])
    k<-c(xpos[1],xpos[-1]-xpos[-length(xpos)],length(x)-xpos[length(xpos)])
    nf<-rep(1:length(k),k)
    return(nf)
  }
  # Validation of Arguments...............................................................................................................
  if(!(parameter=="total" | parameter=="mean" | parameter=="prop" | parameter=="ratio")){stop("Error, argument in parameter is not valid  \n" )}
  if(!(type=="selec" | type=="estm" | type=="estm.Ud")){stop("Error, argument in type is not valid  \n")}
  if(missing(designs)){stop("Error, the vector designs is absent \n")}
  if(V.designs(designs)==FALSE){stop("Error, one of the designs is invalid \n")}
  if(missing(F.UM)){stop("Error, the data.frame F.UM  is absent \n")}
  if(type=="selec"){
    if(length(which(designs=="R.SIS"))!=0){if(which(designs=="R.SIS")[1]!=dim(F.UM)[2]){stop("Error, the R.SIS design is only allowed in the last step \n")}}
    if(missing(p)){stop("Error, the vector p is absent \n")}
    if(length(p)!=length(designs)){stop("Error, vectors p and designs are not equal in length \n")}
    if(length(p)!=dim(F.UM)[2]){stop("Error, the length of p does not correspond to the number of columns of F.UM \n")}
    if(length(which(designs=="PiPT" | designs=="PPT"))!=0){if(missing(xk)){stop("Error, the vector xk is absent \n")}
      if(length(xk)!=dim(F.UM)[1]){stop("Error, The length of xk does not match the number of rows of F.UM \n")}}
  }
  if(type=="estm" | type=="estm.Ud"){
    if(length(which(designs=="R.SIS"))!=0){if(which(designs=="R.SIS")[1]!=(dim(F.UM)[2]+1)){stop("Error, the R.SIS design is only allowed in the last step \n")}}
    if(missing(yk)){stop("Error, the vector yk is absent \n")}
    if(parameter=="prop"){if(!is.factor(yk)){stop("Error, yk should be a factor")}}
    if(missing(list.arg)){stop("Error, argument list for layouts is missing \n")}
    if(length(yk)!=dim(F.UM)[1]){stop("Error, vector yk has no valid length \n")}
    if(parameter=="ratio"){if(missing(zk)){stop("Error, the vector zk is absent \n")}
      if(length(yk)!=length(zk)){stop("Error, vectors yk and zk are not equal in length \n")}}
    if(!(Nc>0 & Nc<1)){stop("Error, the confidence level is not valid \n")}
  }
  if(type=="estm.Ud"){
    if(missing(dk)){stop("Error, the vector dk is absent \n")}
    if(!is.factor(dk)){stop("Error, dk should be a factor")}
    if(length(yk)!=length(dk)){stop("Error, vectors yk and dk are not equal in length \n")}
    if(parameter=="ratio"){if(length(zk)!=length(dk)){stop("Error, vectors zk and dk are not equal in length \n")}}
  }
  #.....................................................................................................................
  contd<-dim(F.UM)[2]
  if(contd==4){dimnames(F.UM)[[2]]<-c("F.UPM","F.USM","F.UTM","F.UCM")}
  if(contd==3){dimnames(F.UM)[[2]]<-c("F.UPM","F.USM","F.UTM")}
  if(contd==2){dimnames(F.UM)[[2]]<-c("F.UPM","F.USM")}
  if(contd==1){dimnames(F.UM)[[2]]<-c("F.UPM")}
  #........................................................................................................
  if(type=="selec"){
    UM<-cbind(IND=1:dim(F.UM)[1],F.UM)
    if(length(which(designs=="PPT" | designs=="PiPT"))!=0){UM<-cbind(UM,xk)}
    NI<-length(table(UM$F.UPM))
    nI<-ceiling(p[1]*NI)
    # Selection in the first stage
    if(contd<=4){
      if(designs[1]=="MAS"){sel1<-MAS(N=NI,n=nI);Rt.1<-data.frame(NI,nI)}
      if(designs[1]=="MCR"){sel1<-MCR(N=NI,m=nI);Rt.1<-data.frame(NI,mI=nI)}
      if(designs[1]=="PPT"){xkI<-tapply(xk,F.UM$F.UPM,sum);sel1<-PPT(xk=xkI,m=nI);Rt.1<-sel1[2:3]}
      if(designs[1]=="PiPT"){xkI<-tapply(xk,F.UM$F.UPM,sum);sel1<-PiPT(xk=xkI,n=nI);Rt.1<-sel1}
      m<-UM[POS(UM$F.UPM,sel1$Ksel),]
      # Construction of the new UPM factor
      k<-1
      th<-numeric()
      for(i in sel1$Ksel){
        th[k]<-dim(UM[UM$F.UPM==i,])[1]
        k<-k+1
      }
      NF.UPM<-rep(1:length(sel1$Ksel),th);m<-cbind(m,NF.UPM);NF<-data.frame(m$NF.UPM)
    }
    # Selection in the second stage
    if(2<=contd & contd<=4){
      Ni<-CONT.F(m$NF.UPM,m$F.USM)
      ni<-ceiling(p[2]*Ni)
      B.ni<-data.frame(i=rep(1:nI),ni)
      j=1
      Rt.2<-list();Rt.2r<-list()
      for(i in 1:nI){
        M2<-m[i==m$NF.UPM,]
        if(designs[2]=="MAS"){sel2<-MAS(N=Ni[i],n=ni[i]);Rt.2[[j]]<-sel2}
        if(designs[2]=="MCR"){sel2<-MCR(N=Ni[i],m=ni[i]);Rt.2[[j]]<-sel2}
        if(designs[2]=="R.SIS" & contd==2){sel2<-R.SIS(N=Ni[i],n=ni[i],r=r);Rt.2[[j]]<-sel2;Rt.2r[[j]]<-sel2$fact}
        if(designs[2]=="PPT"){xk2<-tapply(M2$xk,M2$F.USM,sum );sel2<-PPT(xk=xk2,m=ni[i]);Rt.2[[j]]<-sel2[2:3]}
        if(designs[2]=="PiPT"){xk2<-tapply(M2$xk,M2$F.USM,sum );sel2<-PiPT(xk=xk2,n=ni[i]);Rt.2[[j]]<-sel2}
        if(j==1){
          w<-1
          for(t in sel2$Ksel){
            if(w==1){M<-M2[POS(M2$F.USM,t),];if(contd>2){Niq<-CONT.F(M$F.USM,M$F.UTM); NF.USM<-rep(w,length(POS(M2$F.USM,t)))}} ### AQui
            if(w>1){M.<-M2[POS(M2$F.USM,t),];if(contd>2){H.<-CONT.F(M.$F.USM,M.$F.UTM);NF.USM.<-rep(w,length(POS(M2$F.USM,t)))}
            M<-rbind(M,M.);if(contd>2){Niq<-c(Niq,H.);NF.USM<-c(NF.USM,NF.USM.)}}
            w<-w+1
          }
        }
        if(j>1){
          w<-1
          for(t in sel2$Ksel){
            if(w==1){m2<-M2[POS(M2$F.USM,t),];if(contd>2){H<-CONT.F(m2$F.USM,m2$F.UTM); nf.usm<-rep(w,length(POS(M2$F.USM,t)))}}
            if(w>1){m2.<-M2[POS(M2$F.USM,t),];if(contd>2){H.<-CONT.F(m2.$F.USM,m2.$F.UTM);nf.usm.<-rep(w,length(POS(M2$F.USM,t)))}
            m2<-rbind(m2,m2.);if(contd>2){H<-c(H,H.);nf.usm<-c(nf.usm,nf.usm.)}}
            w<-w+1
          }
          M<-rbind(M,m2);if(contd>2){Niq<-c(Niq,H);NF.USM<-c(NF.USM,nf.usm)}
        }
        j=j+1
      }
      m<-M
      if(contd>2){m<-cbind(m,NF.USM);NF<-cbind(m$NF.UPM,m$NF.USM)}
      for(i in 1:nI){if(i==1){q<-Rt.2[[i]]$Ksel};if(i>1){q1<-Rt.2[[i]]$Ksel;q<-c(q,q1)}}
      if(designs[2]=="MAS"){Rt.2<-data.frame(i=sel1$Ksel,Ni=Ni,ni=ni)}
      if(designs[2]=="MCR"){Rt.2<-data.frame(Ni=Ni,mi=ni)}
      if(designs[2]=="R.SIS" & contd==2){Rt.2<-list(Ni=Ni,ni=ni,fact=Rt.2r,r=r)}
    }
    # Selection in the third stage
    if(3<=contd & contd<=4){
      niq<-ceiling(p[3]*Niq)
      j=1
      Rt.3<-list(); Rt.3r<-list()
      for(I in 1:length(sel1$Ksel)){
        for(Q in 1:ni[I]){
          M3<-m[I==m$NF.UPM & Q==m$NF.USM,]
          if(designs[3]=="MAS"){sel3<-MAS(N=Niq[j],n=niq[j]);Rt.3[[j]]<-sel3}
          if(designs[3]=="MCR"){sel3<-MCR(N=Niq[j],m=niq[j]);Rt.3[[j]]<-sel3}
          if(designs[3]=="R.SIS" & contd==3){sel3<-R.SIS(N=Niq[j],n=niq[j],r=r);Rt.3[[j]]<-sel3;Rt.3r[[j]]<-sel3$fact}
          if(designs[3]=="PPT"){xk3<-tapply(M3$xk,M3$F.UTM,sum);sel3<-PPT(xk=xk3,m=niq[j]);Rt.3[[j]]<-sel3[2:3]}
          if(designs[3]=="PiPT"){xk3<-tapply(M3$xk,M3$F.UTM,sum);sel3<-PiPT(xk=xk3,n=niq[j]);Rt.3[[j]]<-sel3}
          if(j==1){
            w<-1
            for(t in sel3$Ksel){
              if(w==1){M<-M3[POS(M3$F.UTM,t),];if(contd>3){Niqv<-CONT.F(M$F.UTM,M$F.UCM); NF.UTM<-rep(w,length(POS(M3$F.UTM,t)))}}
              if(w>1){M.<-M3[POS(M3$F.UTM,t),];if(contd>3){H.<-CONT.F(M.$F.UTM,M.$F.UCM); NF.UTM.<-rep(w,length(POS(M3$F.UTM,t)))}
              M<-rbind(M,M.);if(contd>3){Niqv<-c(Niqv,H.); NF.UTM<-c(NF.UTM,NF.UTM.)}}
              w<-w+1
            }
          }
          if(j>1){
            w<-1
            for(t in sel3$Ksel){
              if(w==1){m3<-M3[POS(M3$F.UTM,t),];if(contd>3){H<-CONT.F(m3$F.UTM,m3$F.UCM); nf.utm<-rep(w,length(POS(M3$F.UTM,t)))}}
              if(w>1){m3.<-M3[POS(M3$F.UTM,t),];if(contd>3){H.<-CONT.F(m3.$F.UTM,m3.$F.UCM); nf.utm.<-rep(w,length(POS(M3$F.UTM,t)))}
              m3<-rbind(m3,m3.);if(contd>3){H<-c(H,H.); nf.utm<-c(nf.utm,nf.utm.)}}
              w<-w+1
            }
            M=rbind(M,m3);if(contd>3){Niqv<-c(Niqv,H);NF.UTM<-c(NF.UTM,nf.utm)}}
          j=j+1
        }
      }
      m<-M
      if(contd>3){m<-cbind(m,NF.UTM);NF<-cbind(m$NF.UPM,m$NF.USM,m$NF.UTM)}
      if(designs[3]=="MAS"){Rt.3<-data.frame(i=rep(sel1$Ksel,ni),q,Niq,niq)}
      if(designs[3]=="MCR"){Rt.3<-data.frame(Niq,miq=niq)}
      if(designs[3]=="R.SIS"){Rt.3<-list(Niq=Niq,niq=niq,fact=Rt.3r,r=r)}
    }
    #Selection in the fourth stage
    if(contd==4){
      niqv<-ceiling(p[4]*Niqv)
      j=1
      f<-1
      Rt.4<-list()
      for(I in 1:length(sel1$Ksel)){
        for(Q in 1:ni[I]){
          for(V in 1:niq[f]){
            M4<-m[I==m$NF.UPM & Q==m$NF.USM & V==m$NF.UTM,]
            if(designs[4]=="MAS"){sel4<-MAS(N=Niqv[j],n=niqv[j]);Rt.4[[j]]<-sel4}
            if(designs[4]=="MCR"){sel4<-MCR(N=Niqv[j],m=niqv[j]);Rt.4[[j]]<-sel4}
            if(designs[4]=="R.SIS" & contd==4){sel4<-R.SIS(N=Niqv[j],n=niqv[j],r=r);Rt.4[[j]]<-sel4$fact}
            if(designs[4]=="PPT"){sel4<-PPT(xk=M4$xk,m=niqv[j]);Rt.4[[j]]<-sel4[2:3]}
            if(designs[4]=="PiPT"){sel4<-PiPT(xk=M4$xk,n=niqv[j]);Rt.4[[j]]<-sel4}
            if(j==1){M<-M4[sel4$Ksel,]}
            if(j>1){M4<-M4[sel4$Ksel,]
            M=rbind(M,M4)}
            j=j+1
          }
          f<-f+1
        }
      }
      m<-M ;NF<-cbind(NF.UPM=m$NF.UPM,NF.USM=m$NF.USM,NF.UTM=m$NF.UTM)
      if(designs[4]=="MAS"){Rt.4<-data.frame(Niqv,niqv)}
      if(designs[4]=="MCR"){Rt.4<-data.frame(Niqv,miqv=niqv)}
      if(designs[4]=="R.SIS" & contd==4){Rt.4<-list(Niqv=Niqv,niqv=niqv,fact=Rt.4,r=r)}
    }
    x<-which(designs=="PPT" | designs=="PiPT")
    if(length(x)!=0){m<-m[,-(contd+2)]}
    if(contd==1){return(list(Sample=m,Results=list(Rtd.UPM=Rt.1)))}
    if(contd==2){return(list(Sample=m,Results=list(Rtd.UPM=Rt.1,Rtd.USM=Rt.2)))}
    if(contd==3){return(list(Sample=m,Results=list(Rtd.UPM=Rt.1,Rtd.USM=Rt.2,Rtd.UTM=Rt.3)))}
    if(contd==4){return(list(Sample=m,Results=list(Rtd.UPM=Rt.1,Rtd.USM=Rt.2,Rtd.UTM=Rt.3,Rtd.UCM=Rt.4)))}
  }
  #..................................................................................................................
  alfa<-1-Nc
  F.TV<-function(yk,F.UM,designs,list.arg,opc=1){
    contd<-dim(F.UM)[2]
    if(contd==3){
      NF3<-NF(F.UM$F.UTM)
      C3<-length(table(NF3))
      R4<-data.frame(tyiqv=numeric(),S2y=numeric())
      for(i in 1:C3){
        yk4<-yk[which(NF3==i)]
        if(designs[4]=="MCR"){R4[i,]<-MCR(yk=yk4,N=list.arg[[4]]$Niqv[i],m=list.arg[[4]]$miqv[i],type="estm")$Estimation[,1:2]}
        if(designs[4]=="MAS"){R4[i,]<-MAS(yk=yk4,N=list.arg[[4]]$Niqv[i],n=list.arg[[4]]$niqv[i],type="estm")$Estimation[,1:2]}
        if(designs[4]=="R.SIS"){R4[i,]<-R.SIS(yk=yk4,n=list.arg[[4]]$niqv[i],N=list.arg[[4]]$Niqv[i],r=list.arg[[4]]$r,fact=list.arg[[4]]$fact[[i]],type="estm")$Estimation[,1:2]}
        if(designs[4]=="PPT"){R4[i,]<-PPT(yk=yk4,pk=list.arg[[4]][[i]]$pksel,type="estm")$Estimation[,1:2]}
        if(designs[4]=="PiPT"){R4[i,]<-PiPT(yk=yk4,pik=list.arg[[4]][[i]]$piksel,mpikl=list.arg[[4]][[i]]$mpikl.s,type="estm")$Estimation[,1:2]}
      }
      tabla<-tapply(F.UM$F.UTM,NF(F.UM$F.USM),table)
      k<-numeric()
      for(i in 1:length(table(NF(F.UM$F.USM)))){
        k[i]<-length(tabla[[i]])
      }
      NF2<-rep(1:length(k),k)
    }
    if(contd>=2){
      if(contd==2){yk3<-yk;NF2<-NF(F.UM$F.USM)}else{yk3<-R4$tyiqv}
      C2<-length(table(NF2))
      R3<-data.frame(tyiq=numeric(),S2t.iq=numeric())
      for(i in 1:C2){
        yk31<-yk3[which(NF2==i)]
        if(designs[3]=="MCR"){R3[i,]<-MCR(yk=yk31,N=list.arg[[3]]$Niq[i],m=list.arg[[3]]$miq[i],type="estm")$Estimation[,1:2]}
        if(designs[3]=="MAS"){R3[i,]<-MAS(yk=yk31,N=list.arg[[3]]$Niq[i],n=list.arg[[3]]$niq[i],type="estm")$Estimation[,1:2]}
        if(designs[3]=="R.SIS"){R3[i,]<-R.SIS(yk=yk31,n=list.arg[[3]]$niq[i],N=list.arg[[3]]$Niq[i],r=list.arg[[3]]$r,fact=list.arg[[3]]$fact[[i]],type="estm")$Estimation[,1:2]}
        if(designs[3]=="PPT"){R3[i,]<-PPT(yk=yk31,pk=list.arg[[3]][[i]]$pksel,type="estm")$Estimation[,1:2]}
        if(designs[3]=="PiPT"){R3[i,]<-PiPT(yk=yk31,pik=list.arg[[3]][[i]]$piksel,mpikl=list.arg[[3]][[i]]$mpikl.s,type="estm")$Estimation[,1:2]}
      }
      if(contd==3){
        TS2iq<-numeric()
        yk3<-R4$S2y
        for(i in 1:C2){
          yk32<-yk3[which(NF2==i)]
          if(designs[3]=="MCR"){TS2iq[i]<-MCR(yk=yk32,N=list.arg[[3]]$Niq[i],m=list.arg[[3]]$miq[i],type="estm")$Estimation[,1]}
          if(designs[3]=="MAS"){TS2iq[i]<-MAS(yk=yk32,N=list.arg[[3]]$Niq[i],n=list.arg[[3]]$niq[i],type="estm")$Estimation[,1]}
          if(designs[3]=="R.SIS"){TS2iq[i]<-R.SIS(yk=yk32,n=list.arg[[3]]$niq[i],N=list.arg[[3]]$Niq[i],r=list.arg[[3]]$r,fact=list.arg[[3]]$fact[[i]],type="estm")$Estimation[,1]}
          if(designs[3]=="PPT"){TS2iq[i]<-PPT(yk=yk32,pk=list.arg[[3]][[i]]$pksel,type="estm")$Estimation[,1]}
          if(designs[3]=="PiPT"){TS2iq[i]<-PiPT(yk=yk32,pik=list.arg[[3]][[i]]$piksel,mpikl=list.arg[[3]][[i]]$mpikl.s,type="estm")$Estimation[,1]}
        }
      }
      tabla<-tapply(F.UM$F.USM,NF(F.UM$F.UPM),table)
      k<-numeric()
      for(i in 1:length(table(NF(F.UM$F.UPM)))){
        k[i]<-length(tabla[[i]])
      }
      NF1<-rep(1:length(k),k)
    }
    if(contd>=1){
      if(contd==1){yk1<-yk;NF1<-NF(F.UM$F.UPM)}else{yk1<-R3$tyiq}
      C1<-length(table(NF1))
      R2<-data.frame(tyi=numeric(),S2t.i=numeric())
      for(i in 1:C1){
        yk21<-yk1[which(NF1==i)]
        if(designs[2]=="MCR"){R2[i,]<-MCR(yk=yk21,N=list.arg[[2]]$Ni[i],m=list.arg[[2]]$mi[i],type="estm")$Estimation[,1:2]}
        if(designs[2]=="MAS"){R2[i,]<-MAS(yk=yk21,N=list.arg[[2]]$Ni[i],n=list.arg[[2]]$ni[i],type="estm")$Estimation[,1:2]}
        if(designs[2]=="R.SIS"){R2[i,]<-R.SIS(yk=yk21,n=list.arg[[2]]$ni[i],N=list.arg[[2]]$Ni[i],r=list.arg[[2]]$r,fact=list.arg[[2]]$fact[[i]],type="estm")$Estimation[,1:2]}
        if(designs[2]=="PPT"){R2[i,]<-PPT(yk=yk21,pk=list.arg[[2]][[i]]$pksel,type="estm")$Estimation[,1:2]}
        if(designs[2]=="PiPT"){R2[i,]<-PiPT(yk=yk21,pik=list.arg[[2]][[i]]$piksel,mpikl=list.arg[[2]][[i]]$mpikl.s,type="estm")$Estimation[,1:2]}
      }
      if(contd>1){
        if(contd==2){yk2<-R3$S2t.iq}else{yk2<-R3$S2t.iq+TS2iq}
        TS2i<-numeric()
        for(i in 1:C1){
          yk22<-yk2[which(NF1==i)]
          if(designs[2]=="MCR"){TS2i[i]<-MCR(yk=yk22,N=list.arg[[2]]$Ni[i],m=list.arg[[2]]$mi[i],type="estm")$Estimation[,1]}
          if(designs[2]=="MAS"){TS2i[i]<-MAS(yk=yk22,N=list.arg[[2]]$Ni[i],n=list.arg[[2]]$ni[i],type="estm")$Estimation[,1]}
          if(designs[2]=="R.SIS"){TS2i[i]<-R.SIS(yk=yk22,n=list.arg[[2]]$ni[i],N=list.arg[[2]]$Ni[i],r=list.arg[[2]]$r,fact=list.arg[[2]]$fact[[i]],type="estm")$Estimation[,1]}
          if(designs[2]=="PPT"){TS2i[i]<-PPT(yk=yk22,pk=list.arg[[2]][[i]]$pksel,type="estm")$Estimation[,1]}
          if(designs[2]=="PiPT"){TS2i[i]<-PiPT(yk=yk22,pik=list.arg[[2]][[i]]$piksel,mpikl=list.arg[[2]][[i]]$mpikl.s,type="estm")$Estimation[,1]}
        }
      }
    }
    R1<-data.frame(ty=numeric(),S2t=numeric()); TS2t<-numeric()
    yk11<-R2$tyi
    if(contd==1){yk12<-R2$S2t.i}else{yk12<-R2$S2t.i+TS2i}
    if(designs[1]=="BER"){R1[1,]<-BER(yk=yk11,Pi=list.arg[[1]]$Pi,type="estm")$Estimation[,1:2]}
    if(designs[1]=="BER"){TS2t<-BER(yk=yk12,Pi=list.arg[[1]]$Pi,type="estm")$Estimation[,1]}
    if(designs[1]=="MCR"){R1[1,]<-MCR(yk=yk11,N=list.arg[[1]]$NI,m=list.arg[[1]]$mI,type="estm")$Estimation[,1:2]}
    if(designs[1]=="MCR"){TS2t<-MCR(yk=yk12,N=list.arg[[1]]$NI,m=list.arg[[1]]$mI,type="estm")$Estimation[,1]}
    if(designs[1]=="MAS"){R1[1,]<-MAS(yk=yk11,N=list.arg[[1]]$NI,n=list.arg[[1]]$nI,type="estm")$Estimation[,1:2]}
    if(designs[1]=="MAS"){TS2t<-MAS(yk=yk12,N=list.arg[[1]]$NI,n=list.arg[[1]]$nI,type="estm")$Estimation[,1]}
    if(designs[1]=="PPT"){R1[1,]<-PPT(yk=yk11,pk=list.arg[[1]]$pksel,type="estm")$Estimation[,1:2]}
    if(designs[1]=="PPT"){TS2t<-PPT(yk=yk12,pk=list.arg[[1]]$pksel,type="estm")$Estimation[,1]}
    if(designs[1]=="PiPT"){R1[1,]<-PiPT(yk=yk11,pik=list.arg[[1]]$piksel,mpikl=list.arg[[1]]$mpikl.s,type="estm")$Estimation[,1:2]}
    if(designs[1]=="PiPT"){TS2t<-PiPT(yk=yk12,pik=list.arg[[1]]$piksel,mpikl=list.arg[[1]]$mpikl.s,type="estm")$Estimation[,1]}
    ty.e<-R1$ty
    V.est<-R1$S2t+TS2t
    if(opc==1){return(ty.e)}else{return(V.est)}
  }

  #.............................................................................................................
  if(type=="estm" | type=="estm.Ud"){
    n<-length(yk)
    if(type=="estm"){dk<-as.matrix(rep(1,length(yk)))}
    if(parameter!="ratio"){zk<-rep(1,length(yk))}
    dk<-Dk(dk)
    if(parameter=="prop"){yk<-as.factor(yk);ydk<-PRODM(dk,Dk(yk))}else{ydk<-yk*dk}
    if(parameter=="prop"){zk<-matrix(rep(zk,ncol(Dk(yk))),ncol=ncol(Dk(yk)));zdk<-PRODM(dk,zk)}else{zdk<-zk*dk}
    if(parameter=="total"){
      Estd<-apply(ydk,2,F.TV,F.UM,designs,list.arg,1)
      Vest<-apply(ydk,2,F.TV,F.UM,designs,list.arg,2)
    }else{
      tdy.e<-apply(ydk,2,F.TV,F.UM,designs,list.arg,1)
      tdz.e<-apply(zdk,2,F.TV,F.UM,designs,list.arg,1)
      Estd<-tdy.e/tdz.e
      udk.e<-matrix(0,ncol=length(Estd),nrow=length(yk));for(i in 1:length(Estd)){udk.e[,i]<-(ydk[,i]-Estd[i]*zdk[,i])/tdz.e[i]}
      Vest<-apply(udk.e,2,F.TV,F.UM,designs,list.arg,2)
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

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
