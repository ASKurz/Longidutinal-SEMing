#title:  Newsom Longitudinal SEM Chapter 7, Example 7.2b

library(lavaan)
setwd("<your directory path")
health1 <- read.table ("health.dat", header=FALSE)

names(health1) = c("age", "srh1", "srh2", "srh3", "srh4", "srh5", "srh6", "bmi1",
"bmi2", "bmi3", "bmi4", "bmi5", "bmi6", "cesdna1", "cesdpa1", "cesdso1",
"cesdna2", "cesdpa2", "cesdso2", "cesdna3", "cesdpa3", "cesdso3",
"cesdna4", "cesdpa4", "cesdso4", "cesdna5", "cesdpa5", "cesdso5",
"cesdna6", "cesdpa6", "cesdso6", "diab1", "diab2", "diab3 ", "diab4", "diab5", "diab6")

#+++++++++++++++++++++++++++++++
# Chapter 7, Example 7.2b	
#+++++++++++++++++++++++++++++++
#Note: slight differences from Mplus output fit and parameter estimates, 
#likely related to differences in WLSMV theta parameterization algorithms
#or handling sparse data, [to see warnings, use warnings(lavaan)]

attach(health1)

#define Variables

health1$wtcat1 [bmi1 < 18.5] <- 1
health1$wtcat1 [bmi1 >= 18.5 & bmi1 < 25] <- 2
health1$wtcat1 [bmi1 >= 25 & bmi1 < 30] <- 3
health1$wtcat1 [bmi1 >= 30] <- 4

health1$wtcat2 [bmi2 < 18.5] <- 1
health1$wtcat2 [bmi2 >= 18.5 & bmi2 < 25] <- 2
health1$wtcat2 [bmi2 >= 25 & bmi2 < 30] <- 3
health1$wtcat2 [bmi2 >= 30] <- 4

health1$wtcat3 [bmi3 < 18.5] <- 1
health1$wtcat3 [bmi3 >= 18.5 & bmi3 < 25] <- 2
health1$wtcat3 [bmi3 >= 25 & bmi3 < 30] <- 3
health1$wtcat3 [bmi3 >= 30] <- 4

health1$wtcat4 [bmi4 < 18.5] <- 1
health1$wtcat4 [bmi4 >= 18.5 & bmi4 < 25] <- 2
health1$wtcat4 [bmi4 >= 25 & bmi4 < 30] <- 3
health1$wtcat4 [bmi4 >= 30] <- 4

health1$wtcat5 [bmi5 < 18.5] <- 1
health1$wtcat5 [bmi5 >= 18.5 & bmi5 < 25] <- 2
health1$wtcat5 [bmi5 >= 25 & bmi5 < 30] <- 3
health1$wtcat5 [bmi5 >= 30] <- 4

health1$wtcat6 [bmi6 < 18.5] <- 1
health1$wtcat6 [bmi6 >= 18.5 & bmi6 < 25] <- 2
health1$wtcat6 [bmi6 >= 25 & bmi6 < 30] <- 3
health1$wtcat6 [bmi6 >= 30] <- 4

model7.2b <- '  i =~ 1*wtcat1 + 1*wtcat2 + 1*wtcat3 + 1*wtcat4 + 1*wtcat5 + 1*wtcat6
		s =~ 0*wtcat1 + 1*wtcat2 + 2*wtcat3 + 3*wtcat4 + 4*wtcat5 + 5*wtcat6

i ~~ i
s ~~ s

i ~~ s

i ~ 1
s ~ 1

wtcat1 | 0*t1
wtcat1 | a*t2
wtcat1 | b*t3

wtcat2 | 0*t1
wtcat2 | a*t2
wtcat2 | b*t3

wtcat3 | 0*t1
wtcat3 | a*t2
wtcat3 | b*t3

wtcat4 | 0*t1
wtcat4 | a*t2
wtcat4 | b*t3

wtcat5 | 0*t1
wtcat5 | a*t2
wtcat5 | b*t3

wtcat6 | 0*t1
wtcat6 | a*t2
wtcat6 | b*t3  

'

fitmodel7.2b <- growth(model7.2b, data=health1, parameterization="theta", estimator="wlsmv", 
		ordered=c("wtcat1", "wtcat2", "wtcat3", "wtcat4", "wtcat5", "wtcat6"))
summary(fitmodel7.2b, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

