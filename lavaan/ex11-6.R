#title:  Newsom Longitudinal SEM Chapter 11, Example 11.6

library(lavaan)
setwd("<your directory path")
diabetes1 <- read.table ("diabetes.dat", header=FALSE)

names(diabetes1) = c("posaff1", "posaff2", "posaff3", "posaff4", "posaff5", 
"posaff6", "posaff7", "posaff8", "posaff9", "posaff10", 
"posaff11", "posaff12", "posaff13", "posaff14", "posaff15", 
"posaff16", "posaff17", "posaff18", "posaff19", "posaff20", 
"posaff21", "posaff22", "posaff23", "posaff24",
"adhsup1", "adhsup2", "adhsup3", "adhsup4", "adhsup5", 
"adhsup6", "adhsup7", "adhsup8", "adhsup9", "adhsup10", 
"adhsup11", "adhsup12", "adhsup13", "adhsup14", "adhsup15", 
"adhsup16", "adhsup17", "adhsup18", "adhsup19" , "adhsup20", 
"adhsup21", "adhsup22", "adhsup23", "adhsup24")

#+++++++++++++++++++++++++++++++
# Chapter 11, Example 11.6 
#+++++++++++++++++++++++++++++++
#Note: SRMR value differs from Mplus output--likely differences in computation

model11.6 <- ' #lag 0
	 
    ksi1 =~ 1*posaff1
    ksi2 =~ 1*posaff2
    ksi3 =~ 1*posaff3
    ksi4 =~ 1*posaff4
    ksi5 =~ 1*posaff5
    ksi6 =~ 1*posaff6
    ksi7 =~ 1*posaff7
    ksi8 =~ 1*posaff8
    ksi9 =~ 1*posaff9
    ksi10 =~ 1*posaff10
    ksi11 =~ 1*posaff11
    ksi12 =~ 1*posaff12
    ksi13 =~ 1*posaff13
    ksi14 =~ 1*posaff14
    ksi15 =~ 1*posaff15
    ksi16 =~ 1*posaff16
    ksi17 =~ 1*posaff17
    ksi18 =~ 1*posaff18
    ksi19 =~ 1*posaff19
    ksi20 =~ 1*posaff20
    ksi21 =~ 1*posaff21
    ksi22 =~ 1*posaff22
    ksi23 =~ 1*posaff23
    ksi24 =~ 1*posaff24

#exogenous correlations

ksi1 ~~ 0*ksi2 + 0*ksi3 + 0*ksi4 + 0*ksi5
 + 0*ksi6 + 0*ksi7 + 0*ksi8 + 
	0*ksi9 + 0*ksi10 + 0*ksi11 + 0*ksi12
 + 0*ksi13 + 0*ksi14 + 0*ksi15 + 
	0*ksi16 + 0*ksi17 + 0*ksi18 + 0*ksi19
 + 0*ksi20 + 0*ksi21 + 0*ksi22 + 0*ksi23 + 0*ksi24 

ksi2 ~~ 0*ksi3 + 0*ksi4 + 0*ksi5
 + 0*ksi6 + 0*ksi7 + 0*ksi8 + 
	0*ksi9 + 0*ksi10 + 0*ksi11 + 0*ksi12
 + 0*ksi13 + 0*ksi14 + 0*ksi15 + 
	0*ksi16 + 0*ksi17 + 0*ksi18 + 0*ksi19
 + 0*ksi20 + 0*ksi21 + 0*ksi22 + 0*ksi23 + 0*ksi24 

ksi3 ~~ 0*ksi4 + 0*ksi5
 + 0*ksi6 + 0*ksi7 + 0*ksi8 + 
	0*ksi9 + 0*ksi10 + 0*ksi11 + 0*ksi12
 + 0*ksi13 + 0*ksi14 + 0*ksi15 + 
	0*ksi16 + 0*ksi17 + 0*ksi18 + 0*ksi19
 + 0*ksi20 + 0*ksi21 + 0*ksi22 + 0*ksi23 + 0*ksi24 

ksi4 ~~ 0*ksi5
 + 0*ksi6 + 0*ksi7 + 0*ksi8 + 
	0*ksi9 + 0*ksi10 + 0*ksi11 + 0*ksi12
 + 0*ksi13 + 0*ksi14 + 0*ksi15 + 
	0*ksi16 + 0*ksi17 + 0*ksi18 + 0*ksi19
 + 0*ksi20 + 0*ksi21 + 0*ksi22 + 0*ksi23 + 0*ksi24 

ksi5 ~~ 0*ksi6 + 0*ksi7 + 0*ksi8 + 
	0*ksi9 ++ 0*ksi10 + 0*ksi11 + 0*ksi12
 + 0*ksi13 + 0*ksi14 + 0*ksi15 + 
	0*ksi16 + 0*ksi17 + 0*ksi18 + 0*ksi19
 + 0*ksi20 + 0*ksi21 + 0*ksi22 + 0*ksi23 + 0*ksi24 

ksi6 ~~ 0*ksi7 + 0*ksi8 + 
	0*ksi9 + 0*ksi10 + 0*ksi11 + 0*ksi12
 + 0*ksi13 + 0*ksi14 + 0*ksi15 + 
	0*ksi16 + 0*ksi17 + 0*ksi18 + 0*ksi19
 + 0*ksi20 + 0*ksi21 + 0*ksi22 + 0*ksi23 + 0*ksi24 

ksi7 ~~ 0*ksi8 + 
	0*ksi9 + 0*ksi10 + 0*ksi11 + 0*ksi12
 + 0*ksi13 + 0*ksi14 + 0*ksi15 + 
	0*ksi16 + 0*ksi17 + 0*ksi18 + 0*ksi19
 + 0*ksi20 + 0*ksi21 + 0*ksi22 + 0*ksi23 + 0*ksi24 

ksi8 ~~	0*ksi9 + 0*ksi10 + 0*ksi11 + 0*ksi12
 + 0*ksi13 + 0*ksi14 + 0*ksi15 + 
	0*ksi16 + 0*ksi17 + 0*ksi18 + 0*ksi19
 + 0*ksi20 + 0*ksi21 + 0*ksi22 + 0*ksi23 + 0*ksi24 

ksi9 ~~	0*ksi10 + 0*ksi11 + 0*ksi12
 + 0*ksi13 + 0*ksi14 + 0*ksi15 + 
	0*ksi16 + 0*ksi17 + 0*ksi18 + 0*ksi19
 + 0*ksi20 + 0*ksi21 + 0*ksi22 + 0*ksi23 + 0*ksi24 

ksi10 ~~ 0*ksi11 + 0*ksi12
 + 0*ksi13 + 0*ksi14 + 0*ksi15 + 
	 0*ksi16 + 0*ksi17 + 0*ksi18 + 0*ksi19
 + 0*ksi20 + 0*ksi21 + 0*ksi22 + 0*ksi23 + 0*ksi24 

ksi11 ~~ 0*ksi12
 + 0*ksi13 + 0*ksi14 + 0*ksi15 + 
	 0*ksi16 + 0*ksi17 + 0*ksi18 + 0*ksi19
 + 0*ksi20 + 0*ksi21 + 0*ksi22 + 0*ksi23 + 0*ksi24 

ksi12 ~~ 0*ksi13 + 0*ksi14 + 0*ksi15 + 
	 0*ksi16 + 0*ksi17 + 0*ksi18 + 0*ksi19
 + 0*ksi20 + 0*ksi21 + 0*ksi22 + 0*ksi23 + 0*ksi24 

ksi13 ~~ 0*ksi14 + 0*ksi15 + 
	 0*ksi16 + 0*ksi17 + 0*ksi18 + 0*ksi19
 + 0*ksi20 + 0*ksi21 + 0*ksi22 + 0*ksi23 + 0*ksi24

ksi14 ~~ 0*ksi15 + 
	 0*ksi16 + 0*ksi17 + 0*ksi18 + 0*ksi19
 + 0*ksi20 + 0*ksi21 + 0*ksi22 + 0*ksi23 + 0*ksi24

ksi15 ~~ 0*ksi16 + 0*ksi17 + 0*ksi18 + 0*ksi19
 + 0*ksi20 + 0*ksi21 + 0*ksi22 + 0*ksi23 + 0*ksi24

ksi16 ~~ 0*ksi17 + 0*ksi18 + 0*ksi19
 + 0*ksi20 + 0*ksi21 + 0*ksi22 + 0*ksi23 + 0*ksi24

ksi17 ~~ 0*ksi18 + 0*ksi19
 + 0*ksi20 + 0*ksi21 + 0*ksi22 + 0*ksi23 + 0*ksi24

ksi18 ~~ 0*ksi19
 + 0*ksi20 + 0*ksi21 + 0*ksi22 + 0*ksi23 + 0*ksi24

ksi19 ~~ 0*ksi20 + 0*ksi21 + 0*ksi22 + 0*ksi23 + 0*ksi24

ksi20 ~~ 0*ksi21 + 0*ksi22 + 0*ksi23 + 0*ksi24

ksi21 ~~ 0*ksi22 + 0*ksi23 + 0*ksi24

ksi22 ~~ 0*ksi23 + 0*ksi24

ksi23 ~~ 0*ksi24

#autoregressive paths

 posaff2 ~ 1*posaff1
 posaff3 ~ 1*posaff2
 posaff4 ~ 1*posaff3
 posaff5 ~ 1*posaff4
 posaff6 ~ 1*posaff5
 posaff7 ~ 1*posaff6
 posaff8 ~ 1*posaff7
 posaff9 ~ 1*posaff8
 posaff10 ~ 1*posaff9
 posaff11 ~ 1*posaff10
 posaff12 ~ 1*posaff11
 posaff13 ~ 1*posaff12
 posaff14 ~ 1*posaff13
 posaff15 ~
 1*posaff14
 posaff16 ~ 1*posaff15
 posaff17 ~ 1*posaff16
 posaff18 ~ 1*posaff17
 posaff19 ~ 1*posaff18
 posaff20 ~ 1*posaff19
 posaff21 ~ 1*posaff20
 posaff22 ~ 1*posaff21
 posaff23 ~ 1*posaff22
 posaff24 ~ 1*posaff23


#exogenous variances

ksi1 ~~ a*ksi1
ksi2 ~~ a*ksi2
ksi3 ~~ a*ksi3
ksi4 ~~ a*ksi4
ksi5 ~~ a*ksi5
ksi6 ~~ a*ksi6
ksi7 ~~ a*ksi7
ksi8 ~~ a*ksi8
ksi9 ~~ a*ksi9
ksi10 ~~ a*ksi10
ksi11 ~~ a*ksi11
ksi12 ~~ a*ksi12
ksi13 ~~ a*ksi13
ksi14 ~~ a*ksi14
ksi15 ~~ a*ksi15
ksi16 ~~ a*ksi16
ksi17 ~~ a*ksi17
ksi18 ~~ a*ksi18
ksi19 ~~ a*ksi19
ksi20 ~~ a*ksi20
ksi21 ~~ a*ksi21
ksi22 ~~ a*ksi22
ksi23 ~~ a*ksi23
ksi24 ~~ a*ksi24

#endogenous variances

posaff1 ~~ 0*posaff1
posaff2 ~~ 0*posaff2
posaff3 ~~ 0*posaff3
posaff4 ~~ 0*posaff4
posaff5 ~~ 0*posaff5
posaff6 ~~ 0*posaff6
posaff7 ~~ 0*posaff7
posaff8 ~~ 0*posaff8
posaff9 ~~ 0*posaff9
posaff10 ~~ 0*posaff10
posaff11 ~~ 0*posaff11
posaff12 ~~ 0*posaff12
posaff13 ~~ 0*posaff13
posaff14 ~~ 0*posaff14
posaff15 ~~ 0*posaff15
posaff16 ~~ 0*posaff16
posaff17 ~~ 0*posaff17
posaff18 ~~ 0*posaff18
posaff19 ~~ 0*posaff19
posaff20 ~~ 0*posaff20
posaff21 ~~ 0*posaff21
posaff22 ~~ 0*posaff22
posaff23 ~~ 0*posaff23
posaff24 ~~ 0*posaff24  

#means
#should free posaff means and set ksi means to zero
#but can fix posaff means to zero and free ksi means to 
#to check for mean stationarity plotting or testing mean invariance of ksi

ksi1 ~ 1
ksi2 ~ 1 
ksi3 ~ 1
ksi4 ~ 1
ksi5 ~ 1
ksi6 ~ 1
ksi7 ~ 1
ksi8 ~ 1
ksi9 ~ 1
ksi10 ~ 1
ksi11 ~ 1
ksi12 ~ 1
ksi13 ~ 1
ksi14 ~ 1
ksi15 ~ 1
ksi16 ~ 1
ksi17 ~ 1
ksi18 ~ 1
ksi19 ~ 1
ksi20 ~ 1
ksi21 ~ 1
ksi22 ~ 1
ksi23 ~ 1
ksi24 ~ 1

posaff1 ~ 0
posaff2 ~ 0
posaff3 ~ 0
posaff4 ~ 0
posaff5 ~ 0
posaff6 ~ 0
posaff7 ~ 0
posaff8 ~ 0
posaff9 ~ 0
posaff10 ~ 0
posaff11 ~ 0
posaff12 ~ 0
posaff13 ~ 0
posaff14 ~ 0
posaff15 ~ 0
posaff16 ~ 0
posaff17 ~ 0
posaff18 ~ 0
posaff19 ~ 0
posaff20 ~ 0
posaff21 ~ 0
posaff22 ~ 0
posaff23 ~ 0
posaff24 ~ 0 '


fitmodel11.6 <- sem(model11.6, data=diabetes1)
summary(fitmodel11.6, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)


