n.MAS<-function(N,Argt,Nc=0.95,opc=2){
  #Validation of Arguments
  if(length(which((1:4)==opc))==0){stop("Error, the chosen option is not valid")}
  if(missing(N)){stop("Error, the value of N is absent \n")}else{
    if(!is.wholenumber(N)){stop("Error, N must be a whole number \n")}
    if(N<=1){stop("Error, population size is not valid \n")}}
  if(!(Nc>0 & Nc<1)){stop("Error, the confidence level is not valid \n")}
  if(missing(Argt)){stop("Error, the Argt vector is absent \n")}
  if(length(Argt)!=2){stop("Error, la longitud de Argt no es valida \n")}
  #...........................................................................
  alfa<-1-Nc
  z<-qnorm(1-alfa/2)
  n.1<-function(S,Emax.a){
    if(S>0 & Emax.a>0){
      no<-((z*S)/Emax.a)^2
      n<-ceiling(no/(1+(1/N)*no))
      return(list(n=n,no=ceiling(no)))
    }else{stop("Error, one of the Arguments is invalid \n")}
  }
  n.2<-function(Cve,Emax.r){
    if(Cve>0 & Emax.r>0){
      no<-((z*Cve)/Emax.r)^2
      n<-ceiling(no/(1+(1/N)*no))
      return(list(n=n,no=ceiling(no)))
    }else{stop("Error, one of the Arguments is invalid \n")}
  }
  n.3<-function(p,Emax.a){
    if((p>0 & p<1) & Emax.a>0){
      q<-1-p
      no<-(((z^2)*p*q)/Emax.a^2)
      n<-ceiling(no/(((N-1)/N)+(1/N)*no))
      return(list(n=n,no=ceiling(no)))
    }else{stop("Error, one of the Arguments is invalid \n")}
  }
  n.4<-function(p,Emax.r){
    if((p>0 & p<1) & Emax.r>0){
      q<-1-p
      no<-((z^2)*q)/((Emax.r^2)*p)
      n<-ceiling(no/(1+((1/N)*no)))
      return(list(n=n,no=ceiling(no)))
    }else{stop("Error, one of the Arguments is invalid \n")}
  }
  if(opc==1){n<-n.1(S=Argt[1],Emax.a=Argt[2])}
  if(opc==2){n<-n.2(Cve=Argt[1],Emax.r=Argt[2])}
  if(opc==3){n<-n.3(p=Argt[1],Emax.a=Argt[2])}
  if(opc==4){n<-n.4(p=Argt[1],Emax.r=Argt[2])}
  return(n)
}
