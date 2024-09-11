Lab1<- function() {
  paste("Q1) The cars dataset in R gives the speed of cars and the distances taken to stop, which were recorded
in 1920s")

  data(cars)
  names(cars)

  paste("1a) Visualize the relationship between the two variables and find the correlation coefficient.")

  #x-axis->speed; y-axis->dist
  plot(cars$speed,cars$dist,xlab = "Speed",ylab = "Distance")
  cor(cars$speed,cars$dist)
  #The scatter plot indicates a positive linear relationship
  #Variability of y seems to be stable across different values of x

  paste("1b)  Fit a meaningful linear regression model and assess the adequacy of the fitted model.")

  model<-lm(dist~speed,data=cars)
  summary(model)
  #Checking the adequacy of the fitted model
  #Residual analysis
  SR<-rstandard(model)
  #Standardized residuals vs fitted values
  plot(model$fitted.values,SR,xlab = "Fitted values",ylab = "Standardized residuals")
  abline(h=0, col = "red")
  #Check normality of residuals
  qqnorm(SR,ylab = "Standardized Residuals of Model")
  qqline(SR,col = "red")
  shapiro.test(SR)
  hist(SR, col = 2,probability = T)
  #Check outliers based on standardized residuals
  which(SR>3 | SR<(-3)) # no outlier
  #Goodness of fit
  R2<-summary(model)$r.squared

  paste("1c) Write down the fitted model")

  paste("#Fitted model
#hat(distance)=-17.5791+3.9324*speed")

  paste("1d) What is the relationship between the R-squared value and the correlation coefficient between
X and Y ? Does this relationship apply to all regression models?")

  paste("#R^2=(corr)^2
#No, this relationship only applies to simple linear regression models")

  paste("Q2) The Western Collaborative Group Study (WCGS), a prospective cohort study, recruited middleaged men (ages 39 to 59) who were employees of 10 California companies and collected data on
3154 individuals during the years 1960-1961. These subjects were primarily selected to study the
relationship between behavior pattern and the risk of coronary hearth disease (CHD). A number
of other risk factors were also measured. The dataset is available in the ‘epitools’ package in R")

  library(epitools)
  data(wcgs)
  attach(wcgs)

  paste("2a) Construct a contingency table for behavior pattern type and CHD occurrence.")

  tab<-table(dibpat0,chd69)
  riskratio(tab)
  riskratio(tab)$measure #2.2191
  oddsratio(tab)
  oddsratio(tab)$measure #2.3697

  paste("2b)  Estimate the ‘risk ratio’ and the ‘odds ratio’ considering behavior pattern type B as the
reference level.")

  #Chi-sqaured test of independence
  expected(tab)
  chisq.test(tab)

  paste("Q3) The file “covid.csv” contains a dataset on Delta Variant Covid-19 cases in the UK (source: https:
//www.openintro.org/data/index.php?data=simpsons_paradox_covid#:~:text=A%20dataset%
20on%20Delta%20Variant,a%20much%20higher%20risk%20population.). It has 286,166 rows
and 3 variables:
• age group - Age of the person. Levels: under 50, 50 +.
• vaccine status - Vaccination status of the person. Levels: vaccinated, unvaccinated
• outcome - Did the person die from the Delta variant? Levels: death and survived.
")

  paste("3a)  Load the dataset into R.")

  covid<-read.csv("covid.csv")
  covid$outcome<-as.factor(covid$outcome)
  covid$outcome<-relevel(covid$outcome,ref = "survived")

  paste("3b) Construct a 2 × 2 table to summarize the variables vaccine status and outcome.")

  tab2<-table(covid$vaccine_status,covid$outcome)
  tab2

  paste("3c) Calculate the ‘risk ratio’ to quantify the association between vaccine status and outcome.")

  riskratio(tab2)$measure #2.4521

  paste("3d) Re-calculate the risk ratios in subgroups defined by the age group and comment on your
findings.")

  u50<-covid[covid$age_group=="under 50",]
  o50<-covid[covid$age_group=="50 +",]
  #Risk ratio for under 50 group
  tab_u50<-table(u50$vaccine_status,u50$outcome)
  tab_u50
  riskratio(tab_u50)$measure #0.7191
  #Risk ratio for 50+ group
  tab_o50<-table(o50$vaccine_status,o50$outcome)
  tab_o50
  riskratio(tab_o50)$measure #0.2827
  #Although the overall risk ratio, which is larger than 1, indicates that vaccination increases
  #the risk of death, once we consider the two subgroups separately it shows that vaccination
  #reduces the risk of death. This phenomenon is called the "Simpson's Paradox" or "Yule–Simpson effect".
}
