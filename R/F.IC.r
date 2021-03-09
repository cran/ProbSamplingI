F.IC<-function(n,tdy.e,tdz.e,Estd,e.e,Nc,parameter){
  alfa<-1-Nc
  if(n>=100){Qtz<-qnorm(1-(alfa/2))}else{Qtz<-qt(1-(alfa/2),n-1)}
  IC1<-numeric();IC2<-numeric()
  for(i in 1:length(Estd)){
    if(parameter!="prop" | (parameter=="prop" & n>=30 & n*Estd[i]>=5 & n*(1-Estd[i])>=5)){
      IC1[i]<-Estd[i]-e.e[i]*Qtz
      IC2[i]<-Estd[i]+e.e[i]*Qtz
    }else{
      IC1[i]<-IC.prop(tdz.e[i],tdy.e[i],Nc=Nc,opc=1)
      IC2[i]<-IC.prop(tdz.e[i],tdy.e[i],Nc=Nc,opc=2)
    }
  }
  return(data.frame(IC1,IC2))
}
