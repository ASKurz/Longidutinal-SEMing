#title:  Newsom Longitudinal SEM Chapter 3, Example 3.5f

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
# Chapter 3, Example 3.5f 	
#+++++++++++++++++++++++++++++++

model3.5f <- '	w1posaff =~ NA*w1happy + v*w1happy + a*w1enjoy + b*w1satis + c*w1joyful + d*w1please
		w2posaff =~ NA*w2happy + v*w2happy + a*w2enjoy + b*w2satis + c*w2joyful + d*w2please

		w1happy ~ q*1
		w1enjoy ~ e*1
		w1satis ~ f*1
		w1joyful ~ g*1
		w1please ~ h*1

		w2happy ~ q*1
		w2enjoy ~ e*1
		w2satis ~ f*1
		w2joyful ~ g*1
		w2please ~ h*1

		w1posaff ~ 0*1 #set to 0 for single occasion identification;
		w2posaff ~ 0*1 #!can constrain to 0 for LR test;

		w1posaff ~~ 1*w1posaff #set to 1 for single occasion identifcaiton;
		w2posaff ~~ w2posaff

		w1posaff ~~ w2posaff '

fitmodel3.5f <- sem(model3.5f, data=socex1.1, information="observed")
summary(fitmodel3.5f, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
