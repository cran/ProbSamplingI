n.MASC<-function(N,NI,Ni,St,Emax.a,Nc=0.95,n.equal=TRUE){
  #..................................Validation of Arguments...............................................
  if(!(n.equal==TRUE | n.equal==FALSE)){stop("Error, the chosen option is not valid \n")}
  if(n.equal==FALSE){
  if(missing(N)){stop("Error, the value of N is absent \n")}else{
  if(!is.wholenumber(N)){stop("Error, N must be a whole number \n")}
  if(N<=1){stop("Error, population size is not valid  \n")}}
  }

  if(n.equal==TRUE){
    if(missing(Ni)){stop("Error, the size of conglomerates (Ni) is absent \n")}else{
      if(!is.wholenumber(Ni)){stop("Error, Ni must be a whole number \n")}
      if(Ni<=1){stop("Error, the size of the clusters (Ni) in the population is not valid \n")}}
  }
  if(missing(NI)){stop("Error, the value of NI is absent \n")}else{
    if(!is.wholenumber(NI)){stop("Error, NI must be a whole number \n")}
    if(NI<=1){stop("Error, the number of clusters (NI) in the population is not valid \n")}}
  if(missing(St)){stop("Error, the standard deviation of the totals (St) is absent \n")}else{
    if(St<=0){stop("Error, the value of the standard deviation of the totals (St) is not valid \n")}}
  if(missing(Emax.a)){stop("Error, the absolute maximum error (Emax.a) is absent \n")}else{
    if(Emax.a<=0){stop("Error, the absolute maximum error value (Emax.a) is not valid \n")}}
  if(!(Nc>0 & Nc<1)){stop("Error, Confidence level is not valid \n")}
  #.............................................................................................

  alfa<-1-Nc
  z<-qnorm(1-(alfa/2))
  if(n.equal==TRUE){
    nIo<-((z^2)*(St^2))/((Emax.a^2)*(Ni^2))
    nI<-nIo/(1+(nIo/NI))
  }
  if(n.equal==FALSE){
    nIo<-((z^2)*(NI^2)*(St^2))/((Emax.a^2)*(N^2))
    nI<-nIo/(1+(nIo/NI))
  }
  return(nI=ceiling(nI))
}
