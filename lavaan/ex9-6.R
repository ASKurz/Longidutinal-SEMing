#title:  Newsom Longitudinal SEM Chapter 9, Example 9.6

library(lavaan)
setwd("<your directory path")
health1 <- read.table ("health.dat", header=FALSE)

names(health1) = c("age", "srh1", "srh2", "srh3", "srh4", "srh5", "srh6", "bmi1",
"bmi2", "bmi3", "bmi4", "bmi5", "bmi6", "cesdna1", "cesdpa1", "cesdso1",
"cesdna2", "cesdpa2", "cesdso2", "cesdna3", "cesdpa3", "cesdso3",
"cesdna4", "cesdpa4", "cesdso4", "cesdna5", "cesdpa5", "cesdso5",
"cesdna6", "cesdpa6", "cesdso6", "diab1", "diab2", "diab3", "diab4", "diab5", "diab6")
 
#+++++++++++++++++++++++++++++++
# Chapter 9, Example 9.6 
#+++++++++++++++++++++++++++++++

model9.6 <- '#define latent difference factors

	eta1 =~ NA*cesdna1 + (lambda1)*cesdna1 + (lambda2)*cesdpa1 + (lambda3)*cesdso1
	eta2 =~ NA*cesdna2 + (lambda1)*cesdna2 + (lambda2)*cesdpa2 + (lambda3)*cesdso2
	eta3 =~ NA*cesdna3 + (lambda1)*cesdna3 + (lambda2)*cesdpa3 + (lambda3)*cesdso3
	eta4 =~ NA*cesdna4 + (lambda1)*cesdna4 + (lambda2)*cesdpa4 + (lambda3)*cesdso4
	eta5 =~ NA*cesdna5 + (lambda1)*cesdna5 + (lambda2)*cesdpa5 + (lambda3)*cesdso5
	eta6 =~ NA*cesdna6 + (lambda1)*cesdna6 + (lambda2)*cesdpa6 + (lambda3)*cesdso6

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

#means and intercepts

cesdna1 ~ (nu1)*1	
cesdpa1 ~ (nu2)*1
cesdso1 ~ (nu3)*1

cesdna2 ~ (nu1)*1	
cesdpa2 ~ (nu2)*1
cesdso2 ~ (nu3)*1

cesdna3 ~ (nu1)*1	
cesdpa3 ~ (nu2)*1
cesdso3 ~ (nu3)*1

cesdna4 ~ (nu1)*1	
cesdpa4 ~ (nu2)*1
cesdso4 ~ (nu3)*1

cesdna5 ~ (nu1)*1	
cesdpa5 ~ (nu2)*1
cesdso5 ~ (nu3)*1

cesdna6 ~ (nu1)*1	
cesdpa6 ~ (nu2)*1
cesdso6 ~ (nu3)*1

Deta2 ~ 1
Deta3 ~ 1
Deta4 ~ 1
Deta5 ~ 1
Deta6 ~ 1

eta1 ~ 1
eta2 ~ 0
eta3 ~ 0
eta4 ~ 0
eta5 ~ 0
eta6 ~ 0


#set all exogenous covariances to 0 for initial model (but possible for 6 waves)
    eta1 ~~ Deta2 + Deta3 + Deta4 + Deta5 + Deta6;
    Deta2 ~~ Deta3 + Deta4 + Deta5 + Deta6;
    Deta3 ~~ Deta4 + Deta5 + Deta6;
    Deta4 ~~ Deta5 + Deta6;
    Deta5 ~~ Deta6;

#correlated measurement residualts
    cesdna1 ~~ cesdna2 + cesdna3 + cesdna4 + cesdna5 + cesdna6;
    cesdna2 ~~ cesdna3 + cesdna4 + cesdna5 + cesdna6;
    cesdna3 ~~ cesdna4 + cesdna5 + cesdna6;
    cesdna4 ~~ cesdna5 + cesdna6;
    cesdna5 ~~ cesdna6;
    cesdpa1 ~~ cesdpa2 + cesdpa3 + cesdpa4 + cesdpa5 + cesdpa6;
    cesdpa2 ~~ cesdpa3 + cesdpa4 + cesdpa5 + cesdpa6;
    cesdpa3 ~~ cesdpa4 + cesdpa5 + cesdpa6;
    cesdpa4 ~~ cesdpa5 + cesdpa6;
    cesdpa5 ~~ cesdpa6;
    cesdso1 ~~ cesdso2 + cesdso3 + cesdso4 + cesdso5 + cesdso6;
    cesdso2 ~~ cesdso3 + cesdso4 + cesdso5 + cesdso6;
    cesdso3 ~~ cesdso4 + cesdso5 + cesdso6;
    cesdso4 ~~ cesdso5 + cesdso6;
    cesdso5 ~~ cesdso6;

#estimate exogenous variances;
eta1 ~~ eta1

Deta2 ~~ Deta2
Deta3 ~~ Deta3
Deta4 ~~ Deta4
Deta5 ~~ Deta5
Deta6 ~~ Deta6

eta2 ~~ 0*eta2
eta3 ~~ 0*eta3
eta4 ~~ 0*eta4
eta5 ~~ 0*eta5
eta6 ~~ 0*eta6

#model constraint
#effects coding factor identification

lambda1 == 3 - lambda2 - lambda3
    nu1 == 0 - nu2 - nu3  '

fitmodel9.6 <- sem(model9.6, data=health1)
summary(fitmodel9.6, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)



