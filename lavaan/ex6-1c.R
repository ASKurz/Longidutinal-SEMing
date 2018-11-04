#title:  Newsom Longitudinal SEM Chapter 6, Example 6.1c

library(lavaan)
setwd("<your directory path")
health1 <- read.table ("health.dat", header=FALSE)

names(health1) = c("age", "srh1", "srh2", "srh3", "srh4", "srh5", "srh6", "bmi1",
"bmi2", "bmi3", "bmi4", "bmi5", "bmi6", "cesdna1", "cesdpa1", "cesdso1",
"cesdna2", "cesdpa2", "cesdso2", "cesdna3", "cesdpa3", "cesdso3",
"cesdna4", "cesdpa4", "cesdso4", "cesdna5", "cesdpa5", "cesdso5",
"cesdna6", "cesdpa6", "cesdso6", "diab1", "diab2", "diab3 ", "diab4", "diab5", "diab6")

#+++++++++++++++++++++++++++++++
# Chapter 6, Example 6.1c
#+++++++++++++++++++++++++++++++

model6.1c <- '	#occasion-specific factors;

		eta1 =~ NA*cesdna1 + (lambda1)*cesdna1 + (labmda2)*cesdpa1 + (lambda3)*cesdso1
		eta2 =~ NA*cesdna2 + (lambda1)*cesdna2 + (lambda2)*cesdpa2 + (lambda3)*cesdso2 
		eta3 =~ NA*cesdna3 + (lambda1)*cesdna3 + (lambda2)*cesdpa3 + (lambda3)*cesdso3
		eta4 =~ NA*cesdna4 + (lambda1)*cesdna4 + (lambda2)*cesdpa4 + (lambda3)*cesdso4
		eta5 =~ NA*cesdna5 + (lambda1)*cesdna5 + (lambda2)*cesdpa5 + (lambda3)*cesdso5
		eta6 =~ NA*cesdna6 + (lambda1)*cesdna6 + (lambda2)*cesdpa6 + (lambda3)*cesdso6 
		
		#trait factor

		eta =~ 1*eta1 + 1*eta2 + 1*eta3 + 1*eta4 + 1*eta5 + 1*eta6

# correlated errors

cesdna1 ~~ cesdna2
cesdna1 ~~ cesdna3
cesdna1 ~~ cesdna4
cesdna1 ~~ cesdna5
cesdna1 ~~ cesdna6
cesdna2 ~~ cesdna3
cesdna2 ~~ cesdna4
cesdna2 ~~ cesdna5
cesdna2 ~~ cesdna6
cesdna3 ~~ cesdna4
cesdna3 ~~ cesdna5
cesdna3 ~~ cesdna6
cesdna4 ~~ cesdna5
cesdna4 ~~ cesdna6
cesdna5 ~~ cesdna6

cesdpa1 ~~ cesdpa2
cesdpa1 ~~ cesdpa3
cesdpa1 ~~ cesdpa4 
cesdpa1 ~~ cesdpa5 
cesdpa1 ~~ cesdpa6
cesdpa2 ~~ cesdpa3
cesdpa2 ~~ cesdpa4 
cesdpa2 ~~ cesdpa5 
cesdpa2 ~~ cesdpa6
cesdpa3 ~~ cesdpa4
cesdpa3 ~~ cesdpa5 
cesdpa3 ~~ cesdpa6
cesdpa4 ~~ cesdpa5
cesdpa4 ~~ cesdpa6
cesdpa5 ~~ cesdpa6

cesdso1 ~~ cesdso2
cesdso1 ~~ cesdso3 
cesdso1 ~~ cesdso4 
cesdso1 ~~ cesdso5 
cesdso1 ~~ cesdso6
cesdso2 ~~ cesdso3
cesdso2 ~~ cesdso4 
cesdso2 ~~ cesdso5 
cesdso2 ~~ cesdso6
cesdso3 ~~ cesdso4 
cesdso3 ~~ cesdso5 
cesdso3 ~~ cesdso6
cesdso4 ~~ cesdso5
cesdso4 ~~ cesdso6
cesdso5 ~~ cesdso6

#complex constraints for effects coding identification;
#Model constraint:

    lambda1 == 3 - lambda2 - lambda3 '

fitmodel6.1c <- sem(model6.1c, data=health1, information = "expected")

summary(fitmodel6.1c, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

