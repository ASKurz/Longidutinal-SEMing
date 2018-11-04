#title:  Newsom Longitudinal SEM Chapter 5, Example 5.3a

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
# Chapter 5, Example 5.3a 
#+++++++++++++++++++++++++++++++
#Note: some small descrepancies in parameter estimates compared with Mplus

model5.3a <-   ' #common labels for loadings imposes longitudinal equality constraints;

		w1dep =~ NA*w1dboth + (lambda1)*w1dboth + (lambda2)*w1dsad + (lambda3)*w1dblues + (lambda4)*w1ddep
		w2dep =~ NA*w2dboth + (lambda1)*w2dboth + (lambda2)*w2dsad + (lambda3)*w2dblues + (lambda4)*w2ddep

		w1vst =~ NA*w1vst1 + (lambda5)*w1vst1 + (lambda6)*w1vst2 + (lambda7)*w1vst3
		w2vst =~ NA*w2vst1 + (lambda5)*w2vst1 + (lambda6)*w2vst2 + (lambda7)*w2vst3

	#cross-lagged and autoregressive effects;
		w2dep ~ w1dep + w1vst
		w2vst ~ w1vst + w1dep

	#correlated measurement residuals;
		w1dboth ~~ w2dboth
		w1dsad ~~ w2dsad
		w1dblues ~~ w2dblues
		w1ddep ~~ w2ddep

		w1vst1 ~~ w2vst1
		w1vst2 ~~ w2vst2
		w1vst3 ~~ w2vst3

#complex constraints for effects coding identification;
	lambda1 == 4 - lambda2 - lambda3 - lambda4
	lambda5 == 3 - lambda6 - lambda7  '

fitmodel5.3a <- sem(model5.3a, data=socex1.1, parameterization="theta", estimator="wlsmv", 
		ordered=c("w1dboth", "w1dsad", "w1dblues", "w1ddep", "w2dboth", "w2dsad", "w2dblues", "w2ddep"))
summary(fitmodel5.3a, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

