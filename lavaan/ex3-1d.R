#title:  Newsom Longitudinal SEM Chapter 3, Example 3.1d

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
# Chapter 3, Example 3.1d
#+++++++++++++++++++++++++++++++

attach(socex1.1)

socex1.1$w1posaff=(w1happy+w1enjoy+w1satis+w1joyful+w1please)/5
socex1.1$w2posaff=(w2happy+w2enjoy+w2satis+w2joyful+w2please)/5
socex1.1$posdiff=socex1.1$w2posaff-socex1.1$w1posaff

#this alternative works, but gives masking message
#attach(socex1.1)
#    	socex1.1$w1posaff <- (w1happy + w1enjoy + w1satis + w1joyful + w1please)/5
#    	socex1.1$w2posaff <- (w2happy + w2enjoy + w2satis + w2joyful + w2please)/5
#    	socex1.1$w3posaff <- (w3happy + w3enjoy + w3satis + w3joyful + w3please)/5
##attach(socex1.1)
#    	socex1.1$w2w1diff <- w2posaff - w1posaff
#    	socex1.1$w3w1diff <- w3posaff - w1posaff
#detach(socex1.1)

#this is what I did to fix it
#    	socex1.1$w1posaff <- (socex1.1$w1happy + socex1.1$w1enjoy + socex1.1$w1satis + socex1.1$w1joyful + socex1.1$w1please)/5
#    	socex1.1$w2posaff <- (socex1.1$w2happy + socex1.1$w2enjoy + socex1.1$w2satis + socex1.1$w2joyful + socex1.1$w2please)/5
#    	socex1.1$w3posaff <- (socex1.1$w3happy + socex1.1$w3enjoy + socex1.1$w3satis + socex1.1$w3joyful + socex1.1$w3please)/5
#    	socex1.1$w2w1diff <- socex1.1$w2posaff - socex1.1$w1posaff
#    	socex1.1$w3w1diff <- socex1.1$w3posaff - socex1.1$w1posaff


model3.1d <- '	#latent difference model;

		eta3 =~ 1*w2posaff

		w2posaff ~ 1*w1posaff

w1posaff ~~ eta3

eta3 ~ 0   #replace with eta3 ~ 0 for LR test;

w1posaff ~ 1
w2posaff ~ 0*1

eta3 ~~ eta3

w1posaff ~~ w1posaff
w2posaff ~~ 0*w2posaff '


fitmodel3.1d <- sem(model3.1d, data=socex1.1, fixed.x=FALSE)
summary(fitmodel3.1d, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
