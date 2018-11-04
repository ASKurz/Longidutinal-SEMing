#title:  Newsom Longitudinal SEM Chapter 3, Example 3.4d

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
# Chapter 3, Example 3.4d
#+++++++++++++++++++++++++++++++

attach(socex1.1)

    	socex1.1$w1posaff <- (w1happy + w1enjoy + w1satis + w1joyful + w1please)/5
    	socex1.1$w2posaff <- (w2happy + w2enjoy + w2satis + w2joyful + w2please)/5
    	socex1.1$w3posaff <- (w3happy + w3enjoy + w3satis + w3joyful + w3please)/5
    	socex1.1$w2w1diff <- socex1.1$w2posaff - socex1.1$w1posaff
    	socex1.1$w3w1diff <- socex1.1$w3posaff - socex1.1$w1posaff

model3.4d <-   'eta2 =~ 1*w2posaff
		eta3 =~ 1*w3posaff

		w2posaff ~ 1*w1posaff
    		w3posaff ~ 1*w2posaff

eta2 ~~ eta3
w1posaff ~~ eta2
w1posaff ~~ eta3

eta2 ~ 0
eta3 ~ 0 #constrain eta2 and eta3 to 0 for omnibus test

w1posaff ~ 1
w2posaff ~ 0*1
w3posaff ~ 0*1

eta2 ~~ eta2
eta3 ~~ eta3

w1posaff ~~ start(.4)*w1posaff
w2posaff ~~ 0*w2posaff
w3posaff ~~ 0*w3posaff  '

fitmodel3.4d <- sem(model3.4d, data=socex1.1, fixed.x=FALSE)
summary(fitmodel3.4d, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)		
