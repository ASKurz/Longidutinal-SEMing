#title:  Newsom Longitudinal SEM Chapter 2, Example 2.2a

library(lavaan)
setwd("<your directory path>")
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
# Chapter 2, Example 2.2a 
#+++++++++++++++++++++++++++++++

model2.2a <- 'w1comp =~ w1vst1 + w1vst2 + w1vst3
	     
	     w2comp =~ w2vst1 + w2vst2 + w2vst3

w2comp ~~ w1comp

w1comp ~~ a*w1comp #factor variance equality constraint;
w2comp ~~ a*w2comp

w1vst1 ~~ w1vst1
w1vst2 ~~ w1vst2
w1vst3 ~~ w1vst3
w2vst1 ~~ w2vst1
w2vst2 ~~ w2vst2
w2vst3 ~~ w2vst3 
    	     	
w1vst1 ~~ w2vst1 
w1vst2 ~~ w2vst2 
w1vst3 ~~ w2vst3'

	
fitmodel2.2a <- sem(model2.2a, socex1.1, missing = "listwise")

summary (fitmodel2.2a, fit.measures= TRUE, rsquare= TRUE, standardized= TRUE)