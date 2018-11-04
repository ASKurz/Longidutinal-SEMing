#title:  Newsom Longitudinal SEM Chapter 7, Example 7.6c

library(lavaan)
setwd("<your directory path")
health1 <- read.table ("health.dat", header=FALSE)

names(health1) = c("age", "srh1", "srh2", "srh3", "srh4", "srh5", "srh6", "bmi1",
"bmi2", "bmi3", "bmi4", "bmi5", "bmi6", "cesdna1", "cesdpa1", "cesdso1",
"cesdna2", "cesdpa2", "cesdso2", "cesdna3", "cesdpa3", "cesdso3",
"cesdna4", "cesdpa4", "cesdso4", "cesdna5", "cesdpa5", "cesdso5",
"cesdna6", "cesdpa6", "cesdso6", "diab1", "diab2", "diab3 ", "diab4", "diab5", "diab6")

#+++++++++++++++++++++++++++++++
# Chapter 7, Example 7.6c
#+++++++++++++++++++++++++++++++

attach(health1)

#create equally weighted composites

health1$cesd1 = (cesdna1 + cesdpa1 + cesdso1)/3
health1$cesd2 = (cesdna2 + cesdpa2 + cesdso2)/3
health1$cesd3 = (cesdna3 + cesdpa3 + cesdso3)/3
health1$cesd4 = (cesdna4 + cesdpa4 + cesdso4)/3
health1$cesd5 = (cesdna5 + cesdpa5 + cesdso5)/3
health1$cesd6 = (cesdna6 + cesdpa6 + cesdso6)/3

model7.6c <- ' 	#model for only the first variable used as an indicator in the second-order model;

		i =~ 1*cesd1 + 1*cesd2 + 1*cesd3 + 1*cesd4 + 1*cesd5 + 1*cesd6
		s =~ 0*cesd1 + 1*cesd2 + 2*cesd3 + 3*cesd4 + 4*cesd5 + 5*cesd6

i ~~ i
s ~~ s
i ~~ s

i ~ 1
s ~ 1

cesd1 ~ 0*1
cesd2 ~ 0*1
cesd3 ~ 0*1
cesd4 ~ 0*1
cesd5 ~ 0*1
cesd6 ~ 0*1 '


fitmodel7.6c <- growth(model7.6c, data=health1)

summary(fitmodel7.6c, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)


		