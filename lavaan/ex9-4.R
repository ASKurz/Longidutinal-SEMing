#title:  Newsom Longitudinal SEM Chapter 9, Example 9.4

library(lavaan)
setwd("<your directory path")
health1 <- read.table ("health.dat", header=FALSE)

names(health1) = c("age", "srh1", "srh2", "srh3", "srh4", "srh5", "srh6", "bmi1",
"bmi2", "bmi3", "bmi4", "bmi5", "bmi6", "cesdna1", "cesdpa1", "cesdso1",
"cesdna2", "cesdpa2", "cesdso2", "cesdna3", "cesdpa3", "cesdso3",
"cesdna4", "cesdpa4", "cesdso4", "cesdna5", "cesdpa5", "cesdso5",
"cesdna6", "cesdpa6", "cesdso6", "diab1", "diab2", "diab3", "diab4", "diab5", "diab6")

#+++++++++++++++++++++++++++++++
# Chapter 9, Example 9.4 
#+++++++++++++++++++++++++++++++

model9.4 <- ' #define latent difference factors

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

#self-feedback effects for dual change score model
    
    Deta2 ~ bmi1
    Deta3 ~ bmi2
    Deta4 ~ bmi3
    Deta5 ~ bmi4
    Deta6 ~ bmi5
    
#means, intercepts

Deta2 ~ 1 # Add equality constraint (e.g. Deta2 ~ a*1 etc) to test for constant differences
Deta3 ~ 1
Deta4 ~ 1
Deta5 ~ 1
Deta6 ~ 1

bmi1 ~ 1
bmi2 ~ 0
bmi3 ~ 0
bmi4 ~ 0
bmi5 ~ 0
bmi6 ~ 0

#Set all exogenous covariances to 0 for initial mode (but possible for 6 waves)
    bmi1 ~~ 0*Deta2  + 0*Deta3 + 0*Deta4 + 0*Deta5 + 0*Deta6;
    Deta2 ~~ 0*Deta3 + 0*Deta4 + 0*Deta5 + 0*Deta6;
    Deta3 ~~ 0*Deta4 + 0*Deta5 + 0*Deta6;
    Deta4 ~~ 0*Deta5 + 0*Deta6;
    Deta5 ~~ 0*Deta6;

#estimate exogenous variances
bmi1 ~~ bmi1

Deta2 ~~ Deta2
Deta3 ~~ Deta3
Deta4 ~~ Deta4
Deta5 ~~ Deta5
Deta6 ~~ Deta6

#disturbances set to 0

bmi2 ~~ 0*bmi2
bmi3 ~~ 0*bmi3
bmi4 ~~ 0*bmi4
bmi5 ~~ 0*bmi5
bmi6 ~~ 0*bmi6  '

fitmodel9.4 <- sem(model9.4, data=health1 )
summary(fitmodel9.4, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

