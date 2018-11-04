#title:  Newsom Longitudinal SEM Chapter 13, Example 13.1

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
# Chapter 13 Example 13.1
#+++++++++++++++++++++++++++++++
#Note: obtain missing data descriptive information

model13.1 <- 	'   
bmi1 ~~ bmi1
bmi2 ~~ bmi2
bmi3 ~~ bmi3
bmi4 ~~ bmi4
bmi5 ~~ bmi5
bmi6 ~~ bmi6
'
fit <- sem(model13.1,data=healthmissing, missing="fiml",meanstructure=TRUE)
inspect(fit, 'patterns') 
inspect(fit, 'coverage')

