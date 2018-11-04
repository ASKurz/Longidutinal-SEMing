#title:  Newsom Longitudinal SEM Chapter 3, Example 3.7g

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
# Chapter 3, Example 3.7g 
#+++++++++++++++++++++++++++++++

attach(socex1.1)

#define variables:

    socex1.1$w1posaff = (w1happy+w1enjoy+w1satis+w1joyful+w1please)/5;
    socex1.1$w2posaff = (w2happy+w2enjoy+w2satis+w2joyful+w2please)/5;
    socex1.1$w3posaff = (w3happy+w3enjoy+w3satis+w3joyful+w3please)/5;
    
    socex1.1$w2w1diff = socex1.1$w2posaff-socex1.1$w1posaff;
    socex1.1$w3w1diff = socex1.1$w3posaff-socex1.1$w1posaff;

model3.7g <- '  w1posaff =~ NA*w1happy + a*w1happy + b*w1enjoy + c*w1satis + d*w1joyful + e*w1please
		w2posaff =~ NA*w2happy + a*w2happy + b*w2enjoy + c*w2satis + d*w2joyful + e*w2please

w1happy ~ f*1
w1enjoy ~ g*1
w1satis ~ h*1
w1joyful ~ j*1
w1please ~ k*1

w2happy ~ f*1
w2enjoy ~ g*1
w2satis ~ h*1
w2joyful ~ j*1
w2please ~ k*1


w1posaff ~ 0*1
w2posaff ~ 1

w1posaff ~~ 1*w1posaff 
w2posaff ~~ w2posaff
w1posaff ~~ w2posaff

w1posaff + w2posaff ~ w1marr2 '


fitmodel3.7g <- sem(model3.7g, data=socex1.1, estimator="ML", information="observed")
summary(fitmodel3.7g, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

