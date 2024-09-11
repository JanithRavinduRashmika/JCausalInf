Lab32<- function() {
  paste(" LaLonde (1986) was interested in the causal effect of a job training program on earnings. Their
experimental data are available in the Matching package in R. “treat” is the treatment indicating
whether a unit was randomly assigned to the job training program or not and “re78” is the outcome
representing a unit’s real earnings in 1978.
")

  library (Matching)
  data(lalonde)
  a = lalonde$treat
  y = lalonde$re78
  n1<-sum(a==1)
  n0<-sum(a==0)

  paste("a) Estimate the average treatment effect assuming a completely randomized design.
")

  est.diff<-mean(y[a==1])-mean(y[a==0])

  paste("b) Consider the estimator you selected in part (a) as the test statistic. By randomly permuting
the treatment vector, obtain the Monte Carlo approximation of the randomization distribution
of this test statistic. (Consider 104 permutations)
")

  set.seed(1234)
  MC<- 10^4
  Diffhat = rep (0, MC)
  for(mc in 1: MC){
    aperm = sample (a)
    Diffhat[mc]<-mean(y[aperm==1])-mean(y[aperm==0])
  }

  paste("c)  Compute the exact p-value using Monte Carlo approximation.")

  exact.pv.diff<-mean(Diffhat>=est.diff) #0.0022

  paste("d) Repeat parts (b) and (c) using t-statistic with equal variance assumption as the test statistic.
")

  t.stat<-t.test(y[a==1],y[a==0],var.equal = TRUE)$statistic
  TStathat = rep (0, MC)
  for(mc in 1: MC){
    aperm = sample (a)
    TStathat[mc]<-t.test(y[aperm==1],y[aperm==0],var.equal = TRUE)$statistic
  }

  exact.pv.tstat<-mean(TStathat>=t.stat) #0.0021

  paste("e) Without using Monte Carlo method, compute the asymptotic p-value for part (d), assuming
normal distributions for the two potential outcome distributions and equal variances.
")

  asymp.pv<-t.test(y[a==1],y[a==0],var.equal = TRUE)$p.value #0.0048

  paste("f) What are the possible reasons for any differences that you have observed between the p-values
in parts (c), (d) and (e)?
")

  paste("#due to asymptotic approximations.
#the default choice for t.test is a two-sided test.
#for a fair comparison we should multiple p-values in (c) and (d) by 2.")

  paste("g)  Assess the covariate balance with respect to all pre-treatment variables.")

  attach(lalonde)
  #age
  var.test(age[a==1],age[a==0])
  t.test(age[a==1],age[a==0],var.equal = TRUE)
  #educ
  var.test(educ[a==1],educ[a==0])
  t.test(educ[a==1],educ[a==0],var.equal = TRUE)
  #black
  prop.test(table(a,black))
  #hisp
  prop.test(table(a,hisp))
  #married
  prop.test(table(a,married))
  #nodegr
  prop.test(table(a,nodegr))
  #re74
  var.test(re74[a==1],re74[a==0])
  t.test(re74[a==1],re74[a==0],var.equal = FALSE)
  #re75
  var.test(re75[a==1],re75[a==0])
  t.test(re75[a==1],re75[a==0],var.equal = TRUE)
  #u74
  prop.test(table(a,u74))
  #u75
  prop.test(table(a,u75))

  paste("h) Estimate a linear regression model with all pre—treatment variables as controls (no interactions), and report the estimate of the average treatment effect and its standard error.")

  fit<-lm(re78~.,data=lalonde)
  summary(fit)
  fit$coefficients["treat"]
}
