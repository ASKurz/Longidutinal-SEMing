#title:  Newsom Longitudinal SEM Chapter 2, Example 2.3

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
# Chapter 2, Example 2.3 
#+++++++++++++++++++++++++++++++

model2.3 <- 'w1comp =~ w1vst1 + (ly21)*w1vst2 + (ly31)*w1vst3
	     
	     w2comp =~ w2vst1 + (ly52)*w2vst2 + (ly62)*w2vst3

#variances & covariance		

	w2comp ~~ w1comp 
	w2comp ~~ w2comp
	w1comp ~~ w1comp
	w1vst1 ~~ w2vst1
	w1vst2 ~~ w2vst2
	w1vst3 ~~ w2vst3

#complex constraints
      ly2diff := ly52 - ly21
      ly3diff := ly62 - ly31
      ly4diff := ly62/ly52 - ly31/ly21'

fitmodel2.3 <- sem(model2.3, data= socex1.1, bootstrap=1000L) #bootstrap required ofr ration tests;

summary (fitmodel2.3, fit.measures= TRUE, rsquare= TRUE, standardized= TRUE)

parameterEstimates(fitmodel2.3, ci= TRUE, level=0.95, boot.ci.type= 'bca', standardized= TRUE, fmi= "default")
