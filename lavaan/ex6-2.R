#title:  Newsom Longitudinal SEM Chapter 6, Example 6.2

library(lavaan)
setwd("<your directory path")
health1 <- read.table ("health.dat", header=FALSE)

names(health1) = c("age", "srh1", "srh2", "srh3", "srh4", "srh5", "srh6", "bmi1",
"bmi2", "bmi3", "bmi4", "bmi5", "bmi6", "cesdna1", "cesdpa1", "cesdso1",
"cesdna2", "cesdpa2", "cesdso2", "cesdna3", "cesdpa3", "cesdso3",
"cesdna4", "cesdpa4", "cesdso4", "cesdna5", "cesdpa5", "cesdso5",
"cesdna6", "cesdpa6", "cesdso6", "diab1", "diab2", "diab3 ", "diab4", "diab5", "diab6")

#+++++++++++++++++++++++++++++++
# Chapter 6, Example 6.2	
#+++++++++++++++++++++++++++++++

attach(health1)

#define:
    health1$cesd1 = (cesdna1 + cesdpa1 + cesdso1)/3
    health1$cesd2 = (cesdna2 + cesdpa2 + cesdso2)/3
    health1$cesd3 = (cesdna3 + cesdpa3 + cesdso3)/3
    health1$cesd4 = (cesdna4 + cesdpa4 + cesdso4)/3
    health1$cesd5 = (cesdna5 + cesdpa5 + cesdso5)/3
    health1$cesd6 = (cesdna6 + cesdpa6 + cesdso6)/3

model6.2 <- '	#trait factor
	 	
	eta =~ 1*cesd1 + 1*cesd2 + 1*cesd3 + 1*cesd4 + 1*cesd5 + 1*cesd6
		
	#state factors
	    eta1 =~ 1*cesd1
	    eta2 =~ 1*cesd2
	    eta3 =~ 1*cesd3
	    eta4 =~ 1*cesd4
	    eta5 =~ 1*cesd5
	    eta6 =~ 1*cesd6

	#autoregressive paths
	    eta2 ~ (beta1)*eta1
	    eta3 ~ (beta1)*eta2
	    eta4 ~ (beta1)*eta3 
	    eta5 ~ (beta1)*eta4 
	    eta6 ~ (beta1)*eta5 

eta1 ~~ (psi2)*eta1
eta2 ~~ (psi3)*eta2
eta3 ~~ (psi3)*eta3
eta4 ~~ (psi3)*eta4
eta5 ~~ (psi3)*eta5
eta6 ~~ (psi3)*eta6

	#errors
cesd1 ~~ a*cesd1
cesd2 ~~ a*cesd2
cesd3 ~~ a*cesd3
cesd4 ~~ a*cesd4
cesd5 ~~ a*cesd5
cesd6 ~~ a*cesd6

	#exogenous covariances set to 0;
    eta ~~ 0*eta1 '

#complex constraints for the estimate of the state variance 
#(see Kenny & Zatura, 2001) were not necessary for these data;

#Model constraint:
#    psi3 = psi2 - psi2*beta1*beta1;

fitmodel6.2 <- sem(model6.2, data=health1, information = 'expected')

summary(fitmodel6.2, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
