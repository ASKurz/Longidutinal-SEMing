#title:  Newsom Longitudinal SEM Chapter 7, Example 7.4b

library(lavaan)
setwd("<your directory path")
health1 <- read.table ("health.dat", header=FALSE)

names(health1) = c("age", "srh1", "srh2", "srh3", "srh4", "srh5", "srh6", "bmi1",
"bmi2", "bmi3", "bmi4", "bmi5", "bmi6", "cesdna1", "cesdpa1", "cesdso1",
"cesdna2", "cesdpa2", "cesdso2", "cesdna3", "cesdpa3", "cesdso3",
"cesdna4", "cesdpa4", "cesdso4", "cesdna5", "cesdpa5", "cesdso5",
"cesdna6", "cesdpa6", "cesdso6", "diab1", "diab2", "diab3 ", "diab4", "diab5", "diab6")

#+++++++++++++++++++++++++++++++
# Chapter 7, Example 7.4b	
#+++++++++++++++++++++++++++++++
#Note: differences from Mplus in the log likelihood values, AIC, and BIC, but
#other results are the identical or very close

attach(health1)

#grand mean centering

health1$srh1 = srh1 - 3.521
health1$srh2 = srh2 - 3.521
health1$srh3 = srh3 - 3.521
health1$srh4 = srh4 - 3.521
health1$srh5 = srh5 - 3.521
health1$srh6 = srh6 - 3.521

model7.4b <- '  i =~ 1*bmi1 + 1*bmi2 + 1*bmi3 + 1*bmi4 + 1*bmi5 + 1*bmi6
		s =~ 0*bmi1 + 1*bmi2 + 2*bmi3 + 3*bmi4 + 4*bmi5 + 5*bmi6

i ~~ i
s ~~ s
i ~~ s
	
i ~ 1
s ~ 1 
bmi1 ~ 0
bmi2 ~ 0
bmi3 ~ 0
bmi4 ~ 0
bmi5 ~ 0
bmi6 ~ 0 

bmi1 ~ a*srh1  #remove equality constraints for initial model;
bmi2 ~ a*srh2
bmi3 ~ a*srh3
bmi4 ~ a*srh4
bmi5 ~ a*srh5
bmi6 ~ a*srh6  '

fitmodel7.4b <- growth(model7.4b, data=health1, information = "observed")
summary(fitmodel7.4b, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
