#title:  Newsom Longitudinal SEM Chapter 5, Example 5.5b

library(lavaan)
setwd("<your directory path")
socex1.1 <- read.table ("socex1.dat", header=FALSE)

names(health1) = c("age", "srh1", "srh2", "srh3", "srh4", "srh5", "srh6", "bmi1",
		"bmi2", "bmi3", "bmi4", "bmi5", "bmi6", "cesdna1", "cesdpa1", "cesdso1",
		"cesdna2", "cesdpa2", "cesdso2", "cesdna3", "cesdpa3", "cesdso3",
		"cesdna4", "cesdpa4", "cesdso4", "cesdna5", "cesdpa5", "cesdso5",
		"cesdna6", "cesdpa6", "cesdso6", "diab1", "diab2", "diab3 ", "diab4", "diab5", "diab6")

#+++++++++++++++++++++++++++++++
# Chapter 5, Example 5.5b 
#+++++++++++++++++++++++++++++++

model5.5b <- '	#quasi simplex model;

		eta1 =~ 1*srh1
		eta2 =~ 1*srh2
		eta3 =~ 1*srh3
		eta4 =~ 1*srh4
		eta5 =~ 1*srh5
		eta6 =~ 1*srh6

	#first and last residuals must be set to 0 for identification;
		
		srh1 ~~ 0*srh1
		srh6 ~~ 0*srh6
		srh2 ~~ srh2
		srh3 ~~ srh3
		srh4 ~~ srh4
		srh5 ~~ srh5

		eta2 ~ eta1
		eta3 ~ eta2
		eta4 ~ eta3
		eta5 ~ eta4
		eta6 ~ eta5  '

fitmodel5.5b <- sem(model5.5b, data=health1, information = "expected")
summary(fitmodel5.5b, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
