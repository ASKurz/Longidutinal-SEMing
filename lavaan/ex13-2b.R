#title:  Newsom Longitudinal SEM Chapter 13, Example 13.2b

#semTools package needed for missing data analysis with auxiliary variables
#install.packages("semTools", dependencies=TRUE)
library(semTools)

library(lavaan)
setwd("<your directory path")
healthmissing <- read.table ("health missing.dat", na="-99", header=FALSE) # and replace all missing values with "NA"

names(healthmissing) = c("age","srh1", "srh2", "srh3", "srh4", "srh5",
"srh6", "bmi1", "bmi2", "bmi3", "bmi4", "bmi5",
"bmi6", "cesdna1", "cesdpa1", "cesdso1", "cesdna2",
"cesdpa2", "cesdso2", "cesdna3", "cesdpa3", "cesdso3",
"cesdna4", "cesdpa4", "cesdso4", "cesdna5", "cesdpa5", "cesdso5",
"cesdna6", "cesdpa6", "cesdso6", "diab1",
"diab2", "diab3", "diab4", "diab5", "diab6", "srhm2", "srhm3",
"srhm4", "srhm5", "srhm6", "bmim2", "bmim3", "bmim4", "bmim5",
"bmim6", "cesdnam2", "cesdnam3", "cesdnam4", "cesdnam5", "cesdnam6",
"cesdpam2", "cesdpam3", "cesdpam4", "cesdpam5", "cesdpam6",
"cesdsom2", "cesdsom3", "cesdsom4", "cesdsom5", "cesdsom6",
"diabm2", "diabm3", "diabm4", "diabm5", "diabm6", "srha",
"bmia", "cesdnaa", "cesdpaa", "cesdsoa", "diaba")

#srhm2 to diabm6 are missingness indicators;
#srha to diaba are indicators of attrition;

#+++++++++++++++++++++++++++++++
# Chapter 13 Example 13.2b 
#+++++++++++++++++++++++++++++++
#Note: results comparable to Mplus and text results but not identical

model13.2b <- ' dep1 =~ NA*cesdna1 + (lambda1)*cesdna1 + (lambda2)*cesdpa1 + (lambda3)*cesdso1
		dep6 =~ NA*cesdna6 + (lambda1)*cesdna6 + (lambda2)*cesdpa6 + (lambda3)*cesdso6
				
dep1 ~ 1
dep6 ~ 1
srh1 ~ 1
srh6 ~ 1

cesdna1 ~ (nu1)*1
cesdpa1 ~ (nu2)*1
cesdso1 ~ (nu3)*1
cesdna6 ~ (nu1)*1
cesdpa6 ~ (nu2)*1
cesdso6 ~ (nu3)*1

srh6 ~ srh1 + dep1
dep6 ~ srh1 + dep1

srh1 ~~ dep1
srh6 ~~ dep6

#model constraint

lambda1 == 3 - lambda2 - lambda3
    nu1 == 0- nu2 - nu3   '

fitsem <- sem(model13.2b, data=healthmissing, meanstructure=TRUE, fixed.x=FALSE, missing = "fiml")
fitsemaux <- sem.auxiliary(fitsem, data=healthmissing, aux=c("age", "bmi1"), meanstructure=TRUE)
summary(fitsemaux, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

#listwise comparison
#fitmodel13.2b <- sem(model13.2b, data=healthmissing, fixed.x=FALSE, missing="listwise")
#summary(fitmodel13.2b, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

#full information maximum likelihood for normally distributed missing data
#fit = sem(model13.2b, data = healthmissing, fixed.x=FALSE, missing = "fiml")
#summary(fit,fit.measures=TRUE, rsquare=TRUE, standardized=TRUE)

#Yuan-Bentler robust estimates for nonnormal missing data 
#fit = sem(model13.2b, data = healthmissing, fixed.x=FALSE, missing = "fiml", estimator="mlr")
#summary(fit,fit.measures=TRUE, rsquare=TRUE, standardized=TRUE)


