#title:  Newsom Longitudinal SEM Chapter 9, Example 9.3

library(lavaan)
setwd("<your directory path")
health1 <- read.table ("health.dat", header=FALSE)

names(health1) = c("age", "srh1", "srh2", "srh3", "srh4", "srh5", "srh6", "bmi1",
"bmi2", "bmi3", "bmi4", "bmi5", "bmi6", "cesdna1", "cesdpa1", "cesdso1",
"cesdna2", "cesdpa2", "cesdso2", "cesdna3", "cesdpa3", "cesdso3",
"cesdna4", "cesdpa4", "cesdso4", "cesdna5", "cesdpa5", "cesdso5",
"cesdna6", "cesdpa6", "cesdso6", "diab1", "diab2", "diab3", "diab4", "diab5", "diab6")

#+++++++++++++++++++++++++++++++
# Chapter 9, Example 9.3 	
#+++++++++++++++++++++++++++++++

model9.3 <-  '  #define latent difference factors

    Deta2 =~ 1*bmi2
    Deta3 =~ 1*bmi3
    Deta4 =~ 1*bmi4
    Deta5 =~ 1*bmi5
    Deta6 =~ 1*bmi6

#autoregressive paths set to 1
    
    bmi2 ~ 1*bmi1
    bmi3 ~ 1*bmi2
    bmi4 ~ 1*bmi3
    bmi5 ~ 1*bmi4
    bmi6 ~ 1*bmi5
    
#means, intercepts, thresholds

Deta2 ~ 0 

Deta3 ~ 0
Deta4 ~ 0
Deta5 ~ 0
Deta6 ~ 0

bmi1 ~ 0
bmi2 ~ 0
bmi3 ~ 0
bmi4 ~ 0
bmi5 ~ 0
bmi6 ~ 0

#add intercept and slope factors;

i =~ 1*bmi1
s =~ 0*bmi1 + 1*Deta2 + 2*Deta3 + 3*Deta4 + 4*Deta5 + 5*Deta6
i ~~ i
s ~~ 1*s #slope variance set for convergence
i ~ 1
s ~ 1
i ~~ s

#estimate exogenous variances
Deta2 ~~ Deta2
Deta3 ~~ Deta3
Deta4 ~~ Deta4
Deta5 ~~ Deta5
Deta6 ~~ Deta6

#set all residual covariances to 0
#    bmi1 ~~ 0*Deta2  + 0*Deta3 + 0*Deta4 + 0*Deta5 + 0*Deta6;
#    Deta2 ~~ 0*Deta3 + 0*Deta4 + 0*Deta5 + 0*Deta6;
#    Deta3 ~~ 0*Deta4 + 0*Deta5 + 0*Deta6;
#    Deta4 ~~ 0*Deta5 + 0*Deta6;
#    Deta5 ~~ 0*Deta6;

#disturbances set to 0
bmi1 ~~ 0*bmi1
bmi2 ~~ 0*bmi2
bmi3 ~~ 0*bmi3
bmi4 ~~ 0*bmi4
bmi5 ~~ 0*bmi5
bmi6 ~~ 0*bmi6  '

fitmodel9.3 <- sem(model9.3, data=health1 )
summary(fitmodel9.3, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)




