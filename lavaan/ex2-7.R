#title:  Newsom Longitudinal SEM Chapter 2, Example 2.7

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
# Chapter 2, Example 2.7
#+++++++++++++++++++++++++++++++

model2.7 <- 'w1comp =~ (ly11)*w1vst1 + (ly21)*w1vst2 + (ly31)*w1vst3
	     w2comp =~ (ly42)*w2vst1 + (ly52)*w2vst2 + (ly62)*w2vst3

w2comp ~~ w1comp

w1comp ~~ 1*w1comp
w2comp ~~ w2comp

w1vst1 ~~ w1vst1
w1vst2 ~~ w1vst2
w1vst3 ~~ w1vst3
w2vst1 ~~ w2vst1
w2vst2 ~~ w2vst2
w2vst3 ~~ w2vst3

w1vst1 ~~ w2vst1
w1vst2 ~~ w2vst2
w1vst3 ~~ w2vst3

w1comp ~ 1
w2comp ~ 1


#thresholds
# the corresponding Mplus model specification differs slightly here, but is equivalent
w1vst1 ~ 0 
w2vst1 ~ 0 

w1vst2 ~ (tau2)*1
w1vst3 ~ (tau3)*1
w2vst2 ~ (tau5)*1
w2vst3 ~ (tau6)*1

#complex constraints for intercept ratio test;

tau52dif := tau5 - tau2
tau63dif := tau6 - tau3
tauratio := tau63dif - ly62/ly52*tau52dif '

fitmodel2.7 <- sem(model2.7, socex1.1, bootstrap= 1000L, meanstructure = TRUE)

summary (fitmodel2.7, fit.measures= TRUE, standardized= TRUE)

parameterEstimates(fitmodel2.7, ci= TRUE, level=0.95, boot.ci.type= 'bca', standardized= TRUE, fmi= "default")

