#title:  Newsom Longitudinal SEM Chapter 8, Example 8.4

library(lavaan)
setwd("<your directory path")
health1 <- read.table ("health.dat", header=FALSE)

names(health1) = c("age", "srh1", "srh2", "srh3", "srh4", "srh5", "srh6", "bmi1",
"bmi2", "bmi3", "bmi4", "bmi5", "bmi6", "cesdna1", "cesdpa1", "cesdso1",
"cesdna2", "cesdpa2", "cesdso2", "cesdna3", "cesdpa3", "cesdso3",
"cesdna4", "cesdpa4", "cesdso4", "cesdna5", "cesdpa5", "cesdso5",
"cesdna6", "cesdpa6", "cesdso6", "diab1", "diab2", "diab3 ", "diab4", "diab5", "diab6")

#+++++++++++++++++++++++++++++++
# Chapter 8, Example 8.4
#+++++++++++++++++++++++++++++++

model8.4 <-   '   i =~ 1*bmi1 + 1*bmi2 + 1*bmi3 + 1*bmi4 + 1*bmi5 + 1*bmi6
		  first =~ -2.5*bmi1 + -1.5*bmi2 + -.5*bmi3 + .5*bmi4 + 1.5*bmi5 + 2.5*bmi6
		  second =~ 3.125*bmi1 + 1.125*bmi2 + .125*bmi3 + .125*bmi4 + 1.125*bmi5 + 3.125*bmi6


i ~~ i
first ~~ first
second ~~ second

second ~ i + first

i ~~ first

     i ~ 1
 first ~ 1
second ~ 1 

bmi1 ~ 0
bmi2 ~ 0
bmi3 ~ 0
bmi4 ~ 0
bmi5 ~ 0
bmi6 ~ 0 '

fitmodel8.4 <- growth(model8.4, data=health1)

summary(fitmodel8.4, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

