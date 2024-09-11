Lab31<- function() {
  paste(" Table 1 presents a subsample of observations from a completely randomized experiment to evaluate
the effect of an educational television program on reading skills. The unit is a class of students.
The outcome of interest is the average reading score in the class. Half of the classes were shown
the program of interest, and the other half did not have access to the program. [Source: “Causal
Inference and Program Evaluation” course by Professors Guido W. Imbens and V. Joseph Hotz]")

  T<-c(0,1,0,1,0,1)
  Y<-c(55,70,72,66,72.7,78.9)

  paste("d)  Consider the difference-in-means as the test statistic and compute it for the observed data.")

  diffmean<-(sum(T*Y)-sum((1-T)*Y))/3 #5.067

  paste("e)  How many different treatment assignments are possible?")

  M<-choose(6,3) #20

  paste("f) Derive the randomization distribution of the test statistic, compute the exact p-value and
state your conclusions.")

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

  A<-data.frame(t(trtassignment(6,3)))
  for (i in 1:M) {
    t<-A[i,1:6]
    A[i,"est"]<-(sum(t*Y)-sum((1-t)*Y))/3
  }
  p_FRT<-sum(A$est>=diffmean)/20 #0.3
  #Since 0.3>0.05 we do not reject H0 and conclude that
  #there is no evidence to claim that the television program improves reading skills.

  #Your conclusion should change depending on whether you choose a two-sided p-value or not
  p_FRT_2<-sum(abs(A$est)>=diffmean)/20
  #Since 0.6>0.05 we do not reject H0 and conclude that
  #there is no evidence to claim that the television program has any effect on reading skills.
}
