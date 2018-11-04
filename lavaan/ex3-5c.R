#title:  Newsom Longitudinal SEM Chapter 3, Example 3.5c

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
# Chapter 3, Example 3.5c 
#+++++++++++++++++++++++++++++++

model3.5c <- '	w1posaff =~ w1happy + w1enjoy + w1satis + w1joyful + w1please
		w2posaff =~ w2happy + w2enjoy + w2satis + w2joyful + w2please


	
		w1happy ~ 0
		w1enjoy ~ 1
		w1satis ~ 1
		w1joyful ~ 1
		w1please ~ 1

		w2happy ~ 0
		w2enjoy ~ 1
		w2satis ~ 1
		w2joyful ~ 1
		w2please ~ 1

		w1posaff ~ 1
		w2posaff ~ 1 #compare equal to unequal means for ANOVA test;


		w1posaff ~~ w1posaff
		w2posaff ~~ w2posaff

		w1posaff ~~ w2posaff

		w1happy ~~ w1happy
		w1enjoy ~~ w1enjoy
		w1satis ~~ w1satis
		w1joyful ~~ w1joyful
		w1please ~~ w1please

		w2happy ~~ w2happy
		w2enjoy ~~ w2enjoy
		w2satis ~~ w2satis
		w2joyful ~~ w2joyful
		w2please ~~ w2please  '

fitmodel3.5c <- sem(model3.5c, data=socex1.1, information="observed")
summary(fitmodel3.5c, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
