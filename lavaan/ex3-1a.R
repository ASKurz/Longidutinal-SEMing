#title:  Newsom Longitudinal SEM Chapter 3, Example 3.1a

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
# Chapter 3, Example 3.1a 	
#+++++++++++++++++++++++++++++++

attach(socex1.1)

    	socex1.1$w1posaff = (w1happy+w1enjoy+w1satis+w1joyful+w1please)/5
    	socex1.1$w2posaff = (w2happy+w2enjoy+w2satis+w2joyful+w2please)/5
    	socex1.1$posdiff = socex1.1$w2posaff-socex1.1$w1posaff

#simple model to test difference score mean from zero

model3.1a <- '
posdiff ~ 1
posdiff ~~ posdiff
'

fitmodel3.1a <- sem(model3.1a, data=socex1.1)
summary(fitmodel3.1a, standardized=TRUE, rsquare=TRUE)


#single indicator specification, but not needed
#model3.1a <- ' 

#construct single-indicator latent variable
#eta1 =~ 1*posdiff
#posdiff ~ 0
#posdiff ~~ 0*posdiff

#estimate variance and mean of latent variable
#eta1 ~~ eta1
#eta1 ~ 1  '