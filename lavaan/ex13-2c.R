#title:  Newsom Longitudinal SEM Chapter 13, Example 13.2c

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
# Chapter 13 Example 13.2c 
#+++++++++++++++++++++++++++++++


model13.2c <- ' 	i =~ 1*bmi1 + 1*bmi2 + 1*bmi3 + 1*bmi4 + 1*bmi5 + 1*bmi6
			s =~ 0*bmi1 + 1*bmi2 + 2*bmi3 + 3*bmi4 + 4*bmi5 + 5*bmi6

	#variances/covariances
i ~~ i
s ~~ s
i ~~ s

	#intercepts
i ~ 1
s ~ 1 
bmi1 ~ 0
bmi2 ~ 0
bmi3 ~ 0
bmi4 ~ 0
bmi5 ~ 0

#lag1 autocorrelations if desired
#	bmi1 ~~ bmi2 
#	bmi2 ~~ bmi3 
#	bmi3 ~~ bmi4 
#	bmi4 ~~ bmi5 
#	bmi5 ~~ bmi6

'

fitsem <- sem(model13.2c, data=healthmissing, meanstructure=TRUE, fixed.x=FALSE, missing = "fiml")
fitsemaux <- sem.auxiliary(fitsem, data=healthmissing, aux=c("age", "diab1"), meanstructure=TRUE)
summary(fitsemaux, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

#listwise comparison
#fitmodel13.2c <- sem(model13.2c, data=healthmissing, fixed.x=FALSE, missing="listwise")
#summary(fitmodel13.2c, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

#full information maximum likelihood for normally distributed missing data
#fit = sem(model13.2c, data = healthmissing, fixed.x=FALSE, missing = "fiml")
#summary(fit,fit.measures=TRUE, rsquare=TRUE, standardized=TRUE)

#Yuan-Bentler robust estimates for nonnormal missing data 
#fit = sem(model13.2c, data = healthmissing, fixed.x=FALSE, missing = "fiml", estimator="mlr")
#summary(fit,fit.measures=TRUE, rsquare=TRUE, standardized=TRUE)


