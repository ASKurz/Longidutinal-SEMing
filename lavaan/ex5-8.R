#title:  Newsom Longitudinal SEM Chapter 5, Example 5.8

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
# Chapter 5, Example 5.8
#+++++++++++++++++++++++++++++++

model5.8 <- '	#same model as Example 4.2 adding means for continuous change estiamtes;
		#common labels for loadings imposes longitudinal equality constraints;

		w1posaff =~ NA*w1happy + (lambda1)*w1happy + (lambda2)*w1enjoy + (lambda3)*w1satis + (lambda4)*w1joyful + (lambda5)*w1please
	      	w2posaff =~ NA*w2happy + (lambda1)*w2happy + (lambda2)*w2enjoy + (lambda3)*w2satis + (lambda4)*w2joyful + (lambda5)*w2please
  		w1unw =~ NA*w1unw1 + (lambda6)*w1unw1 + (lambda7)*w1unw2 + (lambda8)*w1unw3
		w2unw =~ NA*w2unw1 + (lambda6)*w2unw1 + (lambda7)*w2unw2 + (lambda8)*w2unw3
		
	#cross-lagged and autoregressive effects;
		w1happy ~ (nu1)*1
		w1enjoy ~ (nu2)*1
		w1satis ~ (nu3)*1
		w1joyful ~ (nu4)*1
		w1please ~ (nu5)*1

		w2happy ~ (nu1)*1
		w2enjoy ~ (nu2)*1
		w2satis ~ (nu3)*1
		w2joyful ~ (nu4)*1
		w2please ~ (nu5)*1

		w1unw1 ~ (nu6)*1
    		w1unw2 ~ (nu7)*1
    		w1unw3 ~ (nu8)*1

    		w2unw1 ~ (nu6)*1
    		w2unw2 ~ (nu7)*1
    		w2unw3 ~ (nu8)*1

    		w1unw ~ 1 
		w2unw ~ 1
    		w1posaff ~ 1
		w2posaff ~ 1

	#cross-lagged and autoregressive effects;
		
		w2posaff ~ w1posaff + w1unw
		w2unw ~ w1unw + w1posaff

	#correlated measurement residuals;
		w1happy ~~ w2happy
		w1enjoy ~~ w2enjoy
		w1satis ~~ w2satis
		w1joyful ~~ w2joyful
		w1please ~~ w2please

	    	w1unw1 ~~ w2unw1
		w1unw2 ~~ w2unw2
		w1unw3 ~~ w2unw3

#complex constraints for effects coding identification;

    	lambda1 == 5 - lambda2 - lambda3 - lambda4 - lambda5
    	lambda6 == 3 - lambda7 - lambda8
    	    nu1 == 0 - nu2 - nu3 - nu4 - nu5
    	    nu6 == 0 - nu7 - nu8 '

fitmodel5.8 <- sem(model5.8, data=socex1.1)
summary(fitmodel5.8, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

