#title:  Newsom Longitudinal SEM Chapter 7, Example 7.6b

library(lavaan)
setwd("<your directory path")
health1 <- read.table ("health.dat", header=FALSE)

names(health1) = c("age", "srh1", "srh2", "srh3", "srh4", "srh5", "srh6", "bmi1",
"bmi2", "bmi3", "bmi4", "bmi5", "bmi6", "cesdna1", "cesdpa1", "cesdso1",
"cesdna2", "cesdpa2", "cesdso2", "cesdna3", "cesdpa3", "cesdso3",
"cesdna4", "cesdpa4", "cesdso4", "cesdna5", "cesdpa5", "cesdso5",
"cesdna6", "cesdpa6", "cesdso6", "diab1", "diab2", "diab3 ", "diab4", "diab5", "diab6")

#+++++++++++++++++++++++++++++++
# Chapter 7, Example 7.6b
#+++++++++++++++++++++++++++++++

model7.6b <- ' 	#model for only the first variable used as an indicator in the second-order model;

		i =~ 1*cesdna1 + 1*cesdna2 + 1*cesdna3 + 1*cesdna4 + 1*cesdna5 + 1*cesdna6
		s =~ 0*cesdna1 + 1*cesdna2 + 2*cesdna3 + 3*cesdna4 + 4*cesdna5 + 5*cesdna6

i ~~ i
s ~~ s
i ~~ s

i ~ 1
s ~ 1

cesdna1 ~ 0*1
cesdna2 ~ 0*1
cesdna3 ~ 0*1
cesdna4 ~ 0*1
cesdna5 ~ 0*1
cesdna6 ~ 0*1 '


fitmodel7.6b <- growth(model7.6b, data=health1)

summary(fitmodel7.6b, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)


		