#title:  Newsom Longitudinal SEM Chapter 7, Example 7.6a

library(lavaan)
setwd("<your directory path")
health1 <- read.table ("health.dat", header=FALSE)

names(health1) = c("age", "srh1", "srh2", "srh3", "srh4", "srh5", "srh6", "bmi1",
"bmi2", "bmi3", "bmi4", "bmi5", "bmi6", "cesdna1", "cesdpa1", "cesdso1",
"cesdna2", "cesdpa2", "cesdso2", "cesdna3", "cesdpa3", "cesdso3",
"cesdna4", "cesdpa4", "cesdso4", "cesdna5", "cesdpa5", "cesdso5",
"cesdna6", "cesdpa6", "cesdso6", "diab1", "diab2", "diab3 ", "diab4", "diab5", "diab6")

#+++++++++++++++++++++++++++++++
# Chapter 7, Example 7.6a
#+++++++++++++++++++++++++++++++

model7.6a <- ' 	#first loading is referent by default;
		#equality constraints on loading and intercepts omit for this example;

		eta1 =~ cesdna1 + cesdpa1 + cesdso1
		eta2 =~ cesdna2 + cesdpa2 + cesdso2
		eta3 =~ cesdna3 + cesdpa3 + cesdso3
		eta4 =~ cesdna4 + cesdpa4 + cesdso4
		eta5 =~ cesdna5 + cesdpa5 + cesdso5
		eta6 =~ cesdna6 + cesdpa6 + cesdso6

	
cesdna1 ~ 0*1
cesdpa1 ~ 1
cesdso1 ~ 1
cesdna2 ~ 0*1
cesdpa2 ~ 1
cesdso2 ~ 1
cesdna3 ~ 0*1
cesdpa3 ~ 1
cesdso3 ~ 1
cesdna4 ~ 0*1
cesdpa4 ~ 1
cesdso4 ~ 1
cesdna5 ~ 0*1
cesdpa5 ~ 1
cesdso5 ~ 1
cesdna6 ~ 0*1
cesdpa6 ~ 1
cesdso6 ~ 1

i =~ 1*eta1 + 1*eta2 + 1*eta3 + 1*eta4 + 1*eta5 + 1*eta6
s =~ 0*eta1 + 1*eta2 + 2*eta3 + 3*eta4 + 4*eta5 + 5*eta6

i ~~ i
s ~~ s
i ~~ s

i ~ 1
s ~ 1

eta1 ~ 0
eta2 ~ 0
eta3 ~ 0
eta4 ~ 0
eta5 ~ 0
eta6 ~ 0 '


fitmodel7.6a <- growth(model7.6a, data=health1)

summary(fitmodel7.6a, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)


		