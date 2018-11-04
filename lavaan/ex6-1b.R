#title:  Newsom Longitudinal SEM Chapter 6, Example 6.1b

library(lavaan)
setwd("<your directory path")
health1 <- read.table ("health.dat", header=FALSE)

names(health1) = c("age", "srh1", "srh2", "srh3", "srh4", "srh5", "srh6", "bmi1",
"bmi2", "bmi3", "bmi4", "bmi5", "bmi6", "cesdna1", "cesdpa1", "cesdso1",
"cesdna2", "cesdpa2", "cesdso2", "cesdna3", "cesdpa3", "cesdso3",
"cesdna4", "cesdpa4", "cesdso4", "cesdna5", "cesdpa5", "cesdso5",
"cesdna6", "cesdpa6", "cesdso6", "diab1", "diab2", "diab3 ", "diab4", "diab5", "diab6")

#+++++++++++++++++++++++++++++++
# Chapter 6, Example 6.1b   
#+++++++++++++++++++++++++++++++

model6.1b <- '	#set loadings all equal to 1 for simple computation of coefficients
		#occasion-specific factors;

eta1 =~ 1*cesdna1 + 1*cesdpa1 + 1*cesdso1
		eta2 =~ 1*cesdna2 + 1*cesdpa2 + 1*cesdso2 
		eta3 =~ 1*cesdna3 + 1*cesdpa3 + 1*cesdso3
		eta4 =~ 1*cesdna4 + 1*cesdpa4 + 1*cesdso4
		eta5 =~ 1*cesdna5 + 1*cesdpa5 + 1*cesdso5
		eta6 =~ 1*cesdna6 + 1*cesdpa6 + 1*cesdso6 
		
	#trait factor

		eta =~ 1*eta1 + 1*eta2 + 1*eta3 + 1*eta4 + 1*eta5 + 1*eta6

#setting occasion specific disturbances equal for simple comoputation of coefficients;

eta1 ~~ a*eta1
eta2 ~~ a*eta2
eta3 ~~ a*eta3
eta4 ~~ a*eta4
eta5 ~~ a*eta5
eta6 ~~ a*eta6 '

fitmodel6.1b <- sem(model6.1b, data=health1, information = "expected")

#lavCor(fitmodel6.1b, output = "cor") #for correlations.

summary(fitmodel6.1b, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
