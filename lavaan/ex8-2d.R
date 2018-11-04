#title:  Newsom Longitudinal SEM Chapter 8, Example 8.2d

library(lavaan)
setwd("<your directory path")
health1 <- read.table ("health.dat", header=FALSE)

names(health1) = c("age", "srh1", "srh2", "srh3", "srh4", "srh5", "srh6", "bmi1",
"bmi2", "bmi3", "bmi4", "bmi5", "bmi6", "cesdna1", "cesdpa1", "cesdso1",
"cesdna2", "cesdpa2", "cesdso2", "cesdna3", "cesdpa3", "cesdso3",
"cesdna4", "cesdpa4", "cesdso4", "cesdna5", "cesdpa5", "cesdso5",
"cesdna6", "cesdpa6", "cesdso6", "diab1", "diab2", "diab3 ", "diab4", "diab5", "diab6")

#+++++++++++++++++++++++++++++++
# Chapter 8, Example 8.2d
#+++++++++++++++++++++++++++++++

model8.2d <-   '   #rescaled orthogonal codes;

		   i =~ 1*bmi1 + 1*bmi2 + 1*bmi3 + 1*bmi4 + 1*bmi5 + 1*bmi6
		 lin =~ -.598*bmi1 + -.359*bmi2 + -.120*bmi3 + .120*bmi4 + .359*bmi5 + .598*bmi6
		quad =~ .546*bmi1 + -.109*bmi2 + -.436*bmi3 + -.436*bmi4 + -.109*bmi5 + .546*bmi6

i ~~ i
lin ~~ lin
quad ~~ quad
i ~~ lin + quad
lin ~~ quad
i ~ 1
lin ~ 1
quad ~ 1

bmi1 ~ 0
bmi2 ~ 0
bmi3 ~ 0
bmi4 ~ 0
bmi5 ~ 0
bmi6 ~ 0 '

fitmodel8.2d <- growth(model8.2d, data=health1)

summary(fitmodel8.2d, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

