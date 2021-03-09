Dk<-function(x){
  x<-as.factor(x)
  lv<-levels(x)
  dk<-matrix(NA,ncol=length(lv),nrow=length(x))
  colnames(dk)<-lv
  for(i in 1:length(lv)){
    dk[,i]<-ifelse(x==lv[i],1,0)
  }
  return(dk)
}
