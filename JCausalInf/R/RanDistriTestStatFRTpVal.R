RanDistriTestStatFRTpVal<- function(n,n1) {
  paste("Derive the randomization distribution of the test statistic, compute the exact p-value and
state your conclusions.")

  A<-data.frame(t(trtassignment(6,3)))
  for (i in 1:M) {
    t<-A[i,1:6]
    A[i,"est"]<-(sum(t*Y)-sum((1-t)*Y))/3
  }
  p_FRT<-sum(A$est>=diffmean)/20
}
