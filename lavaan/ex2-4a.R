#title:  Newsom Longitudinal SEM Chapter 2, Example 2.4a

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
#Chapter 2, Example 2.4a 
#+++++++++++++++++++++++++++++++
#Note: some slight differences in the estimates from the Mplus results
#seems to occur with WLSMV with theta parameterization

#Create binary variables

socex1.1$w1unw1d <- ifelse(socex1.1$w1unw1 == 0,0,1)
socex1.1$w1unw2d <- ifelse(socex1.1$w1unw2 == 0,0,1)
socex1.1$w1unw3d <- ifelse(socex1.1$w1unw3 == 0,0,1)

socex1.1$w2unw1d <- ifelse(socex1.1$w2unw1 == 0,0,1)
socex1.1$w2unw2d <- ifelse(socex1.1$w2unw2 == 0,0,1)
socex1.1$w2unw3d <- ifelse(socex1.1$w2unw3 == 0,0,1)

attach(socex1.1)

model2.4a <- 'w1unw =~ 1*w1unw1d + w1unw2d + w1unw3d

	      w2unw =~ 1*w2unw1d + w2unw2d + w2unw3d

	
		w1unw ~~ w1unw
		w2unw ~~ w2unw
		w1unw ~~ w2unw	

		w1unw1d ~~ w2unw1d
		w1unw2d ~~ w2unw2d
		w1unw3d ~~ w2unw3d

	#intercepts
		w1unw ~ 1
		w2unw ~ 1

	#thresholds  
		w1unw1d | 0*t1
		w1unw2d | t1
		w1unw3d | t1

		w2unw1d | 0*t1
		w2unw2d | t1
		w2unw3d | t1  '

fitmodel2.4a <- sem(model2.4a, data = socex1.1, parameterization= "theta", estimator= "WLSMV", 
		ordered=c("w1unw1d", "w1unw2d", "w1unw3d", "w2unw1d", "w2unw2d", "w2unw3d"))

summary (fitmodel2.4a, fit.measures= TRUE, rsquare= TRUE, standardized= TRUE)




