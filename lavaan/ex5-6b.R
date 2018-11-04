#title:  Newsom Longitudinal SEM Chapter 5, Example 5.6b

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
# Chapter 5, Example 5.6b 
#+++++++++++++++++++++++++++++++

model5.6b <- ' #common labels for loadings imposes longitudinal equality constraints;

	      w1posaff =~ NA*w1happy + (lambda1)*w1happy + (lambda2)*w1enjoy + (lambda3)*w1satis + (lambda4)*w1joyful + (lambda5)*w1please
	      w2posaff =~ NA*w2happy + (lambda1)*w2happy + (lambda2)*w2enjoy + (lambda3)*w2satis + (lambda4)*w2joyful + (lambda5)*w2please
	      w3posaff =~ NA*w3happy + (lambda1)*w3happy + (lambda2)*w3enjoy + (lambda3)*w3satis + (lambda4)*w3joyful + (lambda5)*w3please
	      w1unw =~ NA*w1unw1 + (lambda6)*w1unw1 + (lambda7)*w1unw2 + (lambda8)*w1unw3
	      w2unw =~ NA*w2unw1 + (lambda6)*w2unw1 + (lambda7)*w2unw2 + (lambda8)*w2unw3
	      w3unw =~ NA*w3unw1 + (lambda6)*w3unw1 + (lambda7)*w3unw2 + (lambda8)*w3unw3

	#cross-lagged and autoregressive effects;
   
		w2posaff ~ w1posaff + w1unw
		w2unw ~ w1unw + w1posaff
    		w3posaff ~ w2posaff + w2unw + w1posaff + w1unw
  		w3unw ~ w2unw + w2posaff + w1unw + w1posaff

	#correlated measurement residuals;	
		w1happy ~~ w2happy
		w1enjoy ~~ w2enjoy
		w1satis ~~ w2satis
		w1joyful ~~ w2joyful
		w1please ~~ w2please

		w2happy ~~ w3happy
		w2enjoy ~~ w3enjoy
		w2satis ~~ w3satis
		w2joyful ~~ w3joyful
		w2please ~~ w3please
	
		w1happy ~~ w3happy
		w1enjoy ~~ w3enjoy
		w1satis ~~ w3satis
		w1joyful ~~ w3joyful
		w1please ~~ w3please

    		w1unw1 ~~ w2unw1
		w1unw2 ~~ w2unw2
		w1unw3 ~~ w2unw3

    		w2unw1 ~~ w3unw1
		w2unw2 ~~ w3unw2
		w2unw3 ~~ w3unw3

    		w1unw1 ~~ w3unw1
		w1unw2 ~~ w3unw2
		w1unw3 ~~ w3unw3

#complex constraints for effects coding identification;

	lambda1 == 5 - lambda2 - lambda3 - lambda4 - lambda5
	lambda6 == 3 - lambda7 - lambda8 '

fitmodel5.6b <- sem(model5.6b, data=socex1.1, information = "expected")
summary(fitmodel5.6b, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

