#title:  Newsom Longitudinal SEM Chapter 5, Example 5.4

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
# Chapter 5, Example 5.4 
#+++++++++++++++++++++++++++++++
#Note: different baseline model from that used in Mplus, because of how Mplus 
#counts exogenous covariances in the baseline model. This affects some fit indices.

attach(socex1.1)

#define variables 
   
    socex1.1$w1unw = (w1unw1 + w1unw2 + w1unw3)/3;
    socex1.1$w2unw = (w2unw1 + w2unw2 + w2unw3)/3;
    socex1.1$w1posaff = (w1happy + w1enjoy + w1satis + w1joyful + w1please)/5;
    socex1.1$w2posaff = (w2happy + w2enjoy + w2satis + w2joyful + w2please)/5;

model5.4 <- ' #synchronous effects;

		alpha =~ 1*w1posaff + 1*w2posaff

		
		alpha ~~ w1unw
		alpha ~~ w2unw
		w2unw ~~ w1unw

		w1posaff ~ (a)*w1unw
		w2posaff ~ (a)*w2unw

#alpha ~ 0  #add if means desired;
#w1unw ~ 1
#w2unw ~ 1
#w1posaff ~ 1
#w2posaff ~ 1

		w1posaff ~~ b*w1posaff
		w2posaff ~~ b*w2posaff  '

fitmodel5.4 <- sem(model5.4, data=socex1.1, information="expected", fixed.x = FALSE)

summary(fitmodel5.4, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)


