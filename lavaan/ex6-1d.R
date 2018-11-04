#title:  Newsom Longitudinal SEM Chapter 6, Example 6.1d

library(lavaan)
setwd("<your directory path")
health1 <- read.table ("health.dat", header=FALSE)

names(health1) = c("age", "srh1", "srh2", "srh3", "srh4", "srh5", "srh6", "bmi1",
"bmi2", "bmi3", "bmi4", "bmi5", "bmi6", "cesdna1", "cesdpa1", "cesdso1",
"cesdna2", "cesdpa2", "cesdso2", "cesdna3", "cesdpa3", "cesdso3",
"cesdna4", "cesdpa4", "cesdso4", "cesdna5", "cesdpa5", "cesdso5",
"cesdna6", "cesdpa6", "cesdso6", "diab1", "diab2", "diab3 ", "diab4", "diab5", "diab6")

#+++++++++++++++++++++++++++++++
# Chapter 6, Example 6.1d
#+++++++++++++++++++++++++++++++

model6.1d <- '	#occasion-specific factors

		eta1 =~ NA*cesdna1 + (lambda1)*cesdna1 + (labmda2)*cesdpa1 + (lambda3)*cesdso1
		eta2 =~ NA*cesdna2 + (lambda1)*cesdna2 + (lambda2)*cesdpa2 + (lambda3)*cesdso2 
		eta3 =~ NA*cesdna3 + (lambda1)*cesdna3 + (lambda2)*cesdpa3 + (lambda3)*cesdso3
		eta4 =~ NA*cesdna4 + (lambda1)*cesdna4 + (lambda2)*cesdpa4 + (lambda3)*cesdso4
		eta5 =~ NA*cesdna5 + (lambda1)*cesdna5 + (lambda2)*cesdpa5 + (lambda3)*cesdso5
		eta6 =~ NA*cesdna6 + (lambda1)*cesdna6 + (lambda2)*cesdpa6 + (lambda3)*cesdso6 
		
		#trait factor
	
		eta =~ 1*eta1 + 1*eta2 + 1*eta3 + 1*eta4 + 1*eta5 + 1*eta6

#method factors;
#two negative affect method factor loadings eliminated due to nonconvergence;

etana =~ 0*cesdna1 + 0*cesdna2 + NA*cesdna3 + (lambda6)*cesdna3 + (lambda7)*cesdna4 + (lambda8)*cesdna5 + (lambda9)*cesdna6  
etapa =~ NA*cesdpa1 + (lambda10)*cesdpa1 + (lambda11)*cesdpa2 + (lambda12)*cesdpa3 + (lambda13)*cesdpa4 + (lambda14)*cesdpa5 + (lambda15)*cesdpa6       
etaso =~ NA*cesdso1 + (lambda16)*cesdso1 + (lambda17)*cesdso2 + (lambda18)*cesdso3 + (lambda19)*cesdso4 + (lambda20)*cesdso5 + (lambda21)*cesdso6         
         
#correlated errors
    eta ~~ 0*etana + 0*etapa + 0*etaso
    etana ~~ 0*etapa + 0*etaso
    etapa ~~ 0*etaso

#complex constraints for effects coding identification;
#Model constraint:
    lambda1 == 3 - lambda2 - lambda3
    lambda6 == 4 - lambda7 - lambda8 - lambda9
    lambda10 == 6 - lambda11 - lambda12 - lambda13 - lambda14 - lambda15
    lambda16 == 6 - lambda17 - lambda18 - lambda19 - lambda20 - lambda21 '

fitmodel6.1d <- sem(model6.1d, data=health1, information = "expected")
summary(fitmodel6.1d, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
