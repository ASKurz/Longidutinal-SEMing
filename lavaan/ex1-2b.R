#title:  Newsom Longitudinal SEM Chapter 1, Example 1.2b

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
# Chapter 1, Example 1.2b 
#+++++++++++++++++++++++++++++++

model1.2b <- #Factor identification;

	'w1comp =~ NA*w1vst1 + w1vst2 + w1vst3

	
w1comp ~~ 1*w1comp

w1vst1 ~~ w1vst1
w1vst2 ~~ w1vst2
w1vst3 ~~ w1vst3


#Factor mean identification

w1vst1 ~ 1
w1vst2 ~ 1
w1vst3 ~ 1

w1comp ~ 0'

fitmodel1.2b <- sem (model1.2b, data = socex1.1, meanstructure = TRUE)
summary (fitmodel1.2b, fit.measures= TRUE, rsquare= TRUE, standardized=TRUE)