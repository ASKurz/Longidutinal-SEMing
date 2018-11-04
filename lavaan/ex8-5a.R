#title:  Newsom Longitudinal SEM Chapter 8, Example 8.5a

library(lavaan)
setwd("<your directory path")
health1 <- read.table ("health.dat", header=FALSE)

names(health1) = c("age", "srh1", "srh2", "srh3", "srh4", "srh5", "srh6", "bmi1",
"bmi2", "bmi3", "bmi4", "bmi5", "bmi6", "cesdna1", "cesdpa1", "cesdso1",
"cesdna2", "cesdpa2", "cesdso2", "cesdna3", "cesdpa3", "cesdso3",
"cesdna4", "cesdpa4", "cesdso4", "cesdna5", "cesdpa5", "cesdso5",
"cesdna6", "cesdpa6", "cesdso6", "diab1", "diab2", "diab3 ", "diab4", "diab5", "diab6")

#+++++++++++++++++++++++++++++++
# Chapter 8, Example 8.5a
#+++++++++++++++++++++++++++++++
#Note: some difference in SRMR value from Mplus

model8.5a <-   '   i =~ 1*bmi1 + 1*bmi2 + 1*bmi3 + 1*bmi4 + 1*bmi5 + 1*bmi6
		   s =~ 0*bmi1 + 1*bmi2 + NA*bmi3 + NA*bmi4 + NA*bmi5 + NA*bmi6
		

i ~~ i
s ~~ s
i ~~ s
i ~ 1
s ~ 1

bmi1 ~ 0
bmi2 ~ 0
bmi3 ~ 0
bmi4 ~ 0
bmi5 ~ 0
bmi6 ~ 0 

bmi1 ~~ a*bmi1
bmi2 ~~ a*bmi2
bmi3 ~~ a*bmi3
bmi4 ~~ a*bmi4
bmi5 ~~ a*bmi5
bmi6 ~~ a*bmi6  
'

fitmodel8.5a <- growth(model8.5a, data=health1)

summary(fitmodel8.5a, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
