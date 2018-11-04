#title:  Newsom Longitudinal SEM Chapter 9, Example 9.2

library(lavaan)
setwd("<your directory path")
health1 <- read.table ("health.dat", header=FALSE)

names(health1) = c("age", "srh1", "srh2", "srh3", "srh4", "srh5", "srh6", "bmi1",
"bmi2", "bmi3", "bmi4", "bmi5", "bmi6", "cesdna1", "cesdpa1", "cesdso1",
"cesdna2", "cesdpa2", "cesdso2", "cesdna3", "cesdpa3", "cesdso3",
"cesdna4", "cesdpa4", "cesdso4", "cesdna5", "cesdpa5", "cesdso5",
"cesdna6", "cesdpa6", "cesdso6", "diab1", "diab2", "diab3", "diab4", "diab5", "diab6")

#+++++++++++++++++++++++++++++++
# Chapter 9, Example 9.2 

#+++++++++++++++++++++++++++++++

model9.2 <- '#define latent factors for each measured variable

  eta1 =~ 1*diab1
  eta2 =~ 1*diab2
  eta3 =~ 1*diab3
  eta4 =~ 1*diab4
  eta5 =~ 1*diab5
  eta6 =~ 1*diab6

#define latent difference factors

    Deta2 =~ 1*eta2
    Deta3 =~ 1*eta3
    Deta4 =~ 1*eta4
    Deta5 =~ 1*eta5
    Deta6 =~ 1*eta6

#autoregressive paths set to 1
    
    eta2 ~ 1*eta1
    eta3 ~ 1*eta2
    eta4 ~ 1*eta3
    eta5 ~ 1*eta4
    eta6 ~ 1*eta5
    
#means, intercepts, thresholds

Deta2 ~ 1 # do Deta2 ~ a*1 etc to test for equal mean differences
Deta3 ~ 1
Deta4 ~ 1
Deta5 ~ 1
Deta6 ~ 1

eta2 ~ 0
eta3 ~ 0
eta4 ~ 0
eta5 ~ 0
eta6 ~ 0

diab1 | 0*t1
diab2 | 0*t1
diab3 | 0*t1
diab4 | 0*t1
diab5 | 0*t1
diab6 | 0*t1

eta1 ~ 1 #verify this.


#set covariances among difference factors to 0 for identification;
    
    eta1 ~~ Deta2 + Deta3 + Deta4 + Deta5 + Deta6
    Deta2 ~~ 0*Deta3 + 0*Deta4 + 0*Deta5 + 0*Deta6
    Deta3 ~~ 0*Deta4 + 0*Deta5 + 0*Deta6
    Deta4 ~~ 0*Deta5 + 0*Deta6
    Deta5 ~~ 0*Deta6

#estimate exogenous variances;
    
	eta1 ~~ eta1 #this may be problemtatic! verify
    
	Deta2 ~~ a*Deta2 #set variances equal for identification;
	Deta3 ~~ a*Deta3   
	Deta4 ~~ a*Deta4
	Deta5 ~~ a*Deta5
	Deta6 ~~ a*Deta6

# autoregressive disturbances;

	eta2 ~~ 0*eta2
	eta3 ~~ 0*eta3
	eta4 ~~ 0*eta4
	eta5 ~~ 0*eta5
	eta6 ~~ 0*eta6
	
	  
diab1 ~*~ diab1 
diab2 ~*~ diab2
diab3 ~*~ diab3
diab4 ~*~ diab4
diab5 ~*~ diab5
diab6 ~*~ diab6 '

fitmodel9.2 <- sem(model9.2, data=health1, estimator="wlsmv", ordered=c("diab1", "diab2", "diab3", "diab4", "diab5", "diab6"))
summary(fitmodel9.2, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)


