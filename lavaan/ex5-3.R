#title:  Newsom Longitudinal SEM Chapter 5, Example 5.3

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
# Chapter 5, Example 5.3 
#+++++++++++++++++++++++++++++++

attach(socex1.1)

#define variables

	socex1.1$w1unw = (w1unw1 + w1unw2 + w1unw3)/3
    	socex1.1$w2unw = (w2unw1 + w2unw2 + w2unw3)/3
    	socex1.1$w1posaff = (w1happy + w1enjoy + w1satis + w1joyful + w1please)/5
	socex1.1$w2posaff = (w2happy + w2enjoy + w2satis + w2joyful + w2please)/5

model5.3 <- '	#common labels for loadings imposes longitudinal equality constraints;

		w1posaff =~ NA*w1happy + (lambda1)*w1happy + (lambda2)*w1enjoy + (lambda3)*w1satis + (lambda4)*w1joyful + (lambda5)*w1please
				
		w1dep =~ w1dboth + w1dsad + w1dblues + w1ddep
		w2dep =~ w2dboth + w2dsad + w2dblues + w2ddep

		w1vst =~ w1vst1 + w1vst2 + w1vst3
		w2vst =~ w2vst1 + w2vst2 + w2vst3

	#cross-lagged and autoregressive effects;
		w2dep ~ w1dep + w1vst + w1posaff
		w2vst ~ w1vst + w1dep + w1posaff

	#correlated measurement residuals;
		w1dboth ~~ w2dboth
		w1dsad ~~ w2dsad
		w1dblues ~~ w2dblues
		w1ddep ~~ w2ddep

		w1vst1 ~~ w2vst1
		w1vst2 ~~ w2vst2
		w1vst3 ~~ w2vst3

#complex constraints for effects coding identification;
	lambda1 == 5 - lambda2 - lambda3 - lambda4 - lambda5 ' 

fitmodel5.3 <- sem(model5.3, data=socex1.1, information="expected")
summary(fitmodel5.3, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)


