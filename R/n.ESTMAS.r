n.ESTMAS<-function(Nh,Sh,Ch,Ph,Emax.a,Nc=0.95,parameter="mean",Asig="Optima"){
  # Validation of Arguments.....................................................................................................................
  if(!( parameter=="mean" | parameter=="prop" )){stop("Error, argument in parameter is not valid  \n")}
  if(!( Asig=="Optima" | Asig=="Neyman" | Asig=="Proportional" )){stop("Error, argument in Asig is not valid \n")}
  if(missing(Nh)){stop("Error, the vector Nh is absent \n")}
  if(length(which(is.wholenumber(Nh)==FALSE))!=0){stop("Error, one of the strata sizes is not a whole number \n")}
  if(missing(Emax.a)){stop("Error, the value of Emax.a is absent \n")}
  if(parameter=="mean" ){if(missing(Sh)){stop("Error, the vector Sh is absent \n")}
    if(length(Sh)!=length(Nh)){stop("Error, the vectors Sh and Nh do not have the same length")}}
  if(parameter=="prop"){if(missing(Ph)){stop("Error, the vector Ph is absent \n")}
    if(length(Ph)!=length(Nh)){stop("Error, the vectors Ph and Nh do not have the same length")}}
  if(Asig=="Optima"){if(missing(Ch)){stop("Error, the vector Ch is absent \n")}
    if(length(Ch)!=length(Nh)){stop("Error, the vectors Ch and Nh do not have the same length")}}
  if(!(Nc>0 & Nc<1)){stop("Error, the confidence level is not valid \n")}
  #........................................................................................................................
  if(parameter=="prop"){Sh<-sqrt(Ph*(1-Ph))}
  N<-sum(Nh)
  alfa<-1-Nc
  V<-(Emax.a/qnorm(1-(alfa/2)))^2

  if(Asig=="Optima"){
    n<-((sum(Nh*Sh*sqrt(Ch))^2))/(((N^2)*V)+sum(Nh*(Sh^2)))
    nh<-ceiling(n*(((Nh*Sh)/sqrt(Ch))/sum(Nh*Sh*sqrt(Ch))))
    return(list(n=sum(nh),nh=nh))
  }
  if(Asig=="Neyman"){
    n<-(sum(Nh*Sh)^2)/(((N^2)*V)+sum(Nh*(Sh^2)))
    nh<-ceiling(n*((Nh*Sh/sum(Nh*Sh))))
    return(list(n=sum(nh),nh=nh))
  }
  if(Asig=="Proportional"){
    n<-sum(Nh*(Sh^2))/((N*V)+((1/N)*sum(Nh*(Sh^2))))
    nh<-ceiling(n*(Nh/N))
    return(list(n=sum(nh),nh=nh))
  }
}
