WHICH1<-function(V1,V2){
  n<-length(V1)
  indice<-which(V2==V1[1])
  if(n>=2){
    for(i in 2:n){
      indice<-c(indice,which(V2==V1[i]))
    }
  }
  return(indice)
}
