#title:  Newsom Longitudinal SEM Chapter 3, Example 3.7e

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
# Chapter 3, Example 3.7e 	
#+++++++++++++++++++++++++++++++

attach(socex1.1)

#define variables:

    socex1.1$w1posaff = (w1happy+w1enjoy+w1satis+w1joyful+w1please)/5
    socex1.1$w2posaff = (w2happy+w2enjoy+w2satis+w2joyful+w2please)/5
    socex1.1$w3posaff = (w3happy+w3enjoy+w3satis+w3joyful+w3please)/5
    
    socex1.1$w2w1diff = socex1.1$w2posaff-socex1.1$w1posaff
    socex1.1$w3w1diff = socex1.1$w3posaff-socex1.1$w1posaff


model3.7e <- '  w1posaff =~ NA*w1happy + (lambda1)*w1happy + (lambda2)*w1enjoy + (lambda3)*w1satis + (lambda4)*w1joyful + (lambda5)*w1please
		w2posaff =~ NA*w2happy + (lambda1)*w2happy + (lambda2)*w2enjoy + (lambda3)*w2satis + (lambda4)*w2joyful + (lambda5)*w2please


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


#w1posaff ~ 0*1
#w2posaff ~ 1

i =~ 1*w1posaff + 1*w2posaff
d1 =~ 0*w1posaff + 1*w2posaff

d1 ~~ 0*i



w1posaff ~ 0*1
w2posaff ~ 0*1

w1posaff ~~ e*w1posaff 
w2posaff ~~ e*w2posaff

i ~ 1
d1 ~ 1
i ~~ i
d1 ~~ d1

i ~ w1marr2
d1 ~ w1marr2

#complex constraints for effects coding identification  
#(use of common label across waves imposes longitudinal constraints)

#model constraint:

lambda1 == 5 - lambda2 - lambda3 - lambda4 - lambda5 
nu1 == 0 - nu2 - nu3 - nu4 - nu5 '

fitmodel3.7e <- lavaan::sem(model3.7e, data=socex1.1, estimator="ML", information="observed")
summary(fitmodel3.7e, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)




