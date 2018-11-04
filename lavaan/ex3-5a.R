#title:  Newsom Longitudinal SEM Chapter 3, Example 3.5a

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
# Chapter 3, Example 3.5a 	different SRMR value
#+++++++++++++++++++++++++++++++

model3.5a <- ' 	w1posaff =~ w1happy + 1*w1enjoy + 1*w1satis + 1*w1joyful + 1*w1please
		w2posaff =~ w2happy + 1*w2enjoy + 1*w2satis + 1*w2joyful + 1*w2please

	
		w1happy ~ 0
		w1enjoy ~ 0
		w1satis ~ 0
		w1joyful ~ 0
		w1please ~ 0
		w2happy ~ 0
		w2enjoy ~ 0
		w2satis ~ 0
		w2joyful ~ 0
		w2please ~ 0

		w1posaff ~ a*1
		w2posaff ~ a*1  #compare equal to unequal means for ANOVA test;
		

		w1posaff ~~ b*w1posaff
		w2posaff ~~ b*w2posaff
		w1posaff ~~ w2posaff
		w1happy ~~ c*w1happy
		w1enjoy ~~ c*w1enjoy
		w1satis ~~ c*w1satis
		w1joyful ~~ c*w1joyful
		w1please ~~ c*w1please

		w2happy ~~ d*w2happy
		w2enjoy ~~ d*w2enjoy
		w2satis ~~ d*w2satis
		w2joyful ~~ d*w2joyful
		w2please ~~ d*w2please  '

fitmodel3.5a <- sem(model3.5a, data=socex1.1, information="observed")
summary(fitmodel3.5a, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)