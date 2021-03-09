IC.prop<-function(n,x,Nc=0.95,opc=1){
  x<-ceiling(x)
  n<-ceiling(n)
  alfa<-1-Nc
  if(opc==1){
    p<-0.0001;e<-0.0001
    while(e>0 & abs(e)>=0.0001){
      k<-sum(dbinom(x:n,n,p))
      e<-(alfa/2)-k
      if(k<(alfa/2)){p<-p+0.0001}else{p=p}
    }
  }else{
    p<-0.9999
    e<-0.0001
    while(e>0 & abs(e)>=0.0001){
      k<-sum(dbinom(0:x,n,p))
      e<-(alfa/2)-k
      if(k<(alfa/2)){p<-p-0.0001}else{p=p}
    }
  }
  return(p)
}
