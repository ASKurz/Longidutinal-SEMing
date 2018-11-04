#title:  Newsom Longitudinal SEM Chapter 8, Example 8.1b

library(lavaan)
setwd("<your directory path")
health1 <- read.table ("health.dat", header=FALSE)

names(health1) = c("age", "srh1", "srh2", "srh3", "srh4", "srh5", "srh6", "bmi1",
"bmi2", "bmi3", "bmi4", "bmi5", "bmi6", "cesdna1", "cesdpa1", "cesdso1",
"cesdna2", "cesdpa2", "cesdso2", "cesdna3", "cesdpa3", "cesdso3",
"cesdna4", "cesdpa4", "cesdso4", "cesdna5", "cesdpa5", "cesdso5",
"cesdna6", "cesdpa6", "cesdso6", "diab1", "diab2", "diab3 ", "diab4", "diab5", "diab6")

#+++++++++++++++++++++++++++++++
# Chapter 8, Example 8.1b
#+++++++++++++++++++++++++++++++

model8.1b <-   'i1 =~ 1*bmi1 + 1*bmi2 + 1*bmi3 + 0*bmi4 + 0*bmi5 + 0*bmi6
		i2 =~ 0*bmi1 + 0*bmi2 + 0*bmi3 + 1*bmi4 + 1*bmi5 + 1*bmi6
		s1 =~ 0*bmi1 + 1*bmi2 + 2*bmi3 + 3*bmi4 + 3*bmi5 + 3*bmi6
		s2 =~ 0*bmi1 + 0*bmi2 + 0*bmi3 + 0*bmi4 + 1*bmi5 + 2*bmi6

i1 ~~ i1
i2 ~~ i2
s1 ~~ s1
s2 ~~ s2
i1 ~~ i2 + s1 + s2
i2 ~~ s1 + s2 
s1 ~~ s2
i1 ~ 1
i2 ~ 1
s1 ~ 1 #do s1 ~ a*1 and s2 ~ a*1 to constrain intercepts to be equal for difference test;
s2 ~ 1

bmi1 ~ 0
bmi2 ~ 0
bmi3 ~ 0
bmi4 ~ 0
bmi5 ~ 0
bmi6 ~ 0 '

fitmodel8.1b <- growth(model8.1b, data=health1)

summary(fitmodel8.1b, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

