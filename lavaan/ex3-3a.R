#title:  Newsom Longitudinal SEM Chapter 3, Example 3.3a

library(lavaan)
setwd("<your directory path")
socex1.1 <- read.table ("socex1.dat", header=FALSE)

names(socex1.1) = c("w1vst1", "w1vst2", "w1vst3", "w2vst1", "w2vst2",
"w2vst3", "w3vst1", "w3vst2", "w3vst3", "w1unw1", "w1unw2", "w1unw3",
"w2unw1", "w2unw2", "w2unw3", "w3unw1", "w3unw2", "w3unw3", "w1dboth",
"w1dsad", "w1dblues", "w1ddep", "w2dboth", "w2dsad","w2dblues", "w2ddep",
"w3dboth", "w3dsad", "w3dblues", "w3ddep", "w1marr2", "w1happy", "w1enjoy",
"w1satis", "w1joyful", "w1please", "w2happy", "w2enjoy", "w2satis", "w2joyful",
"w2please", "w3happy", "w3enjoy", "w3satis", "w3joyful", "w3please", "w1lea",
"w2lea", "w3lea") 

#+++++++++++++++++++++++++++++++
# Chapter 3, Example 3.3a 
#+++++++++++++++++++++++++++++++

model3.3a <- ' 	#specification with single indicator latent variable;

		eta1 =~ 1*w1dsad
		eta2 =~ 1*w2dsad

#set the factor means and thresholds equal for full marginal homogeity test;

w1dsad | 0*t1
w1dsad | a*t2
w1dsad | b*t3

w2dsad | 0*t1
w2dsad | a*t2
w2dsad | b*t3


		eta1 ~~ eta2

	#intercepts

		eta1 ~ c*1
		eta2 ~ c*1


		eta1 ~~ 1*eta1
		eta2 ~~ 1*eta2
		w1dsad ~~ 0*w1dsad
		w2dsad ~~ 0*w2dsad  '
  

fitmodel3.3a <- sem(model3.3a, data=socex1.1, estimator="wlsmv", parameterization ="theta", 
		ordered=c("w1dsad", "w2dsad"))

summary(fitmodel3.3a, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE) 
