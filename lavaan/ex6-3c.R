#title:  Newsom Longitudinal SEM Chapter 6, Example 6.3c

library(lavaan)
setwd("<your directory path")
health1 <- read.table ("health.dat", header=FALSE)

names(health1) = c("age", "srh1", "srh2", "srh3", "srh4", "srh5", "srh6", "bmi1",
"bmi2", "bmi3", "bmi4", "bmi5", "bmi6", "cesdna1", "cesdpa1", "cesdso1",
"cesdna2", "cesdpa2", "cesdso2", "cesdna3", "cesdpa3", "cesdso3",
"cesdna4", "cesdpa4", "cesdso4", "cesdna5", "cesdpa5", "cesdso5",
"cesdna6", "cesdpa6", "cesdso6", "diab1", "diab2", "diab3 ", "diab4", "diab5", "diab6")

#+++++++++++++++++++++++++++++++
# Chapter 6, Example 6.3c
#+++++++++++++++++++++++++++++++

#state factors;

model6.3c <- '	#state factors;

		eta1 =~ NA*cesdna1 + (lambda1)*cesdna1 + (labmda2)*cesdpa1 + (lambda3)*cesdso1
		eta2 =~ NA*cesdna2 + (lambda1)*cesdna2 + (lambda2)*cesdpa2 + (lambda3)*cesdso2 
		eta3 =~ NA*cesdna3 + (lambda1)*cesdna3 + (lambda2)*cesdpa3 + (lambda3)*cesdso3
		eta4 =~ NA*cesdna4 + (lambda1)*cesdna4 + (lambda2)*cesdpa4 + (lambda3)*cesdso4
		eta5 =~ NA*cesdna5 + (lambda1)*cesdna5 + (lambda2)*cesdpa5 + (lambda3)*cesdso5
		eta6 =~ NA*cesdna6 + (lambda1)*cesdna6 + (lambda2)*cesdpa6 + (lambda3)*cesdso6 
		
#occasion factors;
    etao1 =~ 1*eta1
    etao2 =~ 1*eta2
    etao3 =~ 1*eta3
    etao4 =~ 1*eta4
    etao5 =~ 1*eta5
    etao6 =~ 1*eta6


#occasion autoregression
#constrain all paths equal for stationarity test
    etao2 ~ etao1
    etao3 ~ etao2
    etao4 ~ etao3
    etao5 ~ etao4
    etao6 ~ etao5

# constrain for stationarity test;

etao2 ~~ etao2
etao3 ~~ etao3
etao4 ~~ etao4
etao5 ~~ etao5
etao6 ~~ etao6


#trait factor
		eta =~ 1*eta1 + 1*eta2 + 1*eta3 + 1*eta4 + 1*eta5 + 1*eta6

eta1 ~~0*eta1
eta2 ~~0*eta2
eta3 ~~0*eta3
eta4 ~~0*eta4
eta5 ~~0*eta5
eta6 ~~0*eta6

#covariances
    eta ~~ 0*etao1

#method factors using Eid m-1 approach with effects coding;

etam1 =~ NA*cesdpa1 + (lambda4)*cesdpa1 + (lambda5)*cesdpa2 + (lambda6)*cesdpa3 + (lambda7)*cesdpa4 + (lambda8)*cesdpa5 + (lambda9)*cesdpa6  
etam2 =~ NA*cesdso1 + (lambda10)*cesdso1 + (lambda11)*cesdso2 + (lambda12)*cesdso3 + (lambda13)*cesdso4 + (lambda14)*cesdso5 + (lambda15)*cesdso6         

      eta ~~ 0*etam1 + 0*etam2 + eta
    etam1 ~~ 0*etao2 + 0*etao3 + 0*etao4 + 0*etao5 + 0*etao6
    etam2 ~~ 0*etao2 + 0*etao3 + 0*etao4 + 0*etao5 + 0*etao6
   
#complex constraints for effects coding identification;
#Model constraint:
	lambda1 == 3 - lambda2 - lambda3 
	lambda4 == 6 - lambda5 - lambda6 - lambda7 - lambda8 - lambda9
	lambda10 == 6 - lambda11 - lambda12 - lambda13 - lambda14 - lambda15  '

fitmodel6.3c <- sem(model6.3c, data=health1, information = "expected")
summary(fitmodel6.3c, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

