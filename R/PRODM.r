PRODM<-function(A,B){
  C<-matrix(NA,nrow=nrow(A),ncol=ncol(A)*ncol(B))
  k<-1
  for(i in 1:ncol(A)){
    for(j in 1:ncol(B)){
      C[,k]<-A[,i]*B[,j]
      k<-k+1
    }
  }
  colnames(C)<-rep(colnames(A),each=ncol(B))
  return(C)
}
