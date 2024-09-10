trtassignment<- function(n,n1) {
  M<-choose(n,n1)
  treat.index<-combn(n,n1)
  A<-matrix(0,n,M)
  for (i in 1:M) {
    treat<-treat.index[,i]
    A[treat,i]<-1
  }
  A
}
