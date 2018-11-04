#title:  Newsom Longitudinal SEM Chapter 3, Example 3.1c

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
# Chapter 3, Example 3.1c 
#+++++++++++++++++++++++++++++++

attach(socex1.1)

#define new variables

    	socex1.1$w1posaff=(w1happy+w1enjoy+w1satis+w1joyful+w1please)/5
    	socex1.1$w2posaff=(w2happy+w2enjoy+w2satis+w2joyful+w2please)/5
	socex1.1$posdiff=socex1.1$w2posaff-socex1.1$w1posaff

model3.1c <- ' #contrast coding model

	  i =~ 1*w1posaff + 1*w2posaff
	  d1 =~ 0*w1posaff + 1*w2posaff

d1 ~~ 0*i

w1posaff ~ 0*1
w2posaff ~ 0*1

i ~ 1 
d1 ~ 1 #do d1 ~ 0*1 to constrain d1 to 0 for test of equality

i ~~ i

d1 ~~ 0*d1

w1posaff ~~ w1posaff
w2posaff ~~ w2posaff  '  #do w1posaff ~~ a*w1posaff, w2posaff ~~ a*w2posaff for equal variance assumption


fitmodel3.1c <- sem(model3.1c, data=socex1.1, information = "observed")
summary(fitmodel3.1c, fit.measures= TRUE, standardized= TRUE, rsquare=TRUE)

