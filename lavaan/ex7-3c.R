#title:  Newsom Longitudinal SEM Chapter 7, Example 7.3c;

library(lavaan)
setwd("<your directory path")
health1 <- read.table ("health.dat", header=FALSE)

names(health1) = c("age", "srh1", "srh2", "srh3", "srh4", "srh5", "srh6", "bmi1",
"bmi2", "bmi3", "bmi4", "bmi5", "bmi6", "cesdna1", "cesdpa1", "cesdso1",
"cesdna2", "cesdpa2", "cesdso2", "cesdna3", "cesdpa3", "cesdso3",
"cesdna4", "cesdpa4", "cesdso4", "cesdna5", "cesdpa5", "cesdso5",
"cesdna6", "cesdpa6", "cesdso6", "diab1", "diab2", "diab3 ", "diab4", "diab5", "diab6")

#+++++++++++++++++++++++++++++++
# Chapter 7, Example 7.3c
#+++++++++++++++++++++++++++++++

#define variables

attach(health1)
health1$agecat [age >= 65] <- 1
health1$agecat [age < 65 ] <- 0


model7.3c <-  ' i =~ 1*bmi1 + 1*bmi2 + 1*bmi3 + 1*bmi4 + 1*bmi5 + 1*bmi6
		s =~ 0*bmi1 + 1*bmi2 + 2*bmi3 + 3*bmi4 + 4*bmi5 + 5*bmi6

i ~~ i 
s ~~ s
i ~~ s
i ~ 1
s ~ 1 #do s ~ a*1 to test for group difference (adding equality constraint);

bmi1 ~ 0
bmi2 ~ 0
bmi3 ~ 0
bmi4 ~ 0
bmi5 ~ 0
bmi6 ~ 0 '

fitmodel7.3c <- growth(model7.3c, data=health1, group="agecat", group.equal="lv.variances") 
summary(fitmodel7.3c, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)


