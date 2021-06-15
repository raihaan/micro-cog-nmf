#Load necessary packages
library(nlme)
library(dplyr)

wh_long_cog <- read.csv("long_file_Rai_ph5_11.csv")


#wh_long_cog$phase[wh_long_cog$phase==3] <- "003"
wh_long_cog$phase[wh_long_cog$phase==5] <- "005"
wh_long_cog$phase[wh_long_cog$phase==7] <- "007"
wh_long_cog$phase[wh_long_cog$phase==9] <- "009"
wh_long_cog$phase[wh_long_cog$phase==11] <- "011"
wh_long_cog$phase[wh_long_cog$phase==12] <- "012"
wh_long_cog$phase <- as.factor(wh_long_cog$phase)
wh_long_cog$MRI_ID <- as.factor(wh_long_cog$MRI_ID)
wh_long_cog$oxf_id <- as.factor(wh_long_cog$oxf_id)
wh_long_cog$sex <- as.factor(wh_long_cog$sex)
levels(wh_long_cog$phase)

names(wh_long_cog)

#some demographics
#mean(subset(wh_long_cog, wh_long_cog$phase=="003")$age_c, na.rm=TRUE) # 47.42073
#sd(subset(wh_long_cog, wh_long_cog$phase=="003")$age_c, na.rm=TRUE) # 4.916547
mean(subset(wh_long_cog, wh_long_cog$phase=="005")$age_c, na.rm=TRUE) # 53.15145
sd(subset(wh_long_cog, wh_long_cog$phase=="005")$age_c, na.rm=TRUE) # 4.943024
mean(subset(wh_long_cog, wh_long_cog$phase=="007")$age_c, na.rm=TRUE) # 58.66636
sd(subset(wh_long_cog, wh_long_cog$phase=="007")$age_c, na.rm=TRUE) # 4.910422
mean(subset(wh_long_cog, wh_long_cog$phase=="009")$age_c, na.rm=TRUE) # 63.60719
sd(subset(wh_long_cog, wh_long_cog$phase=="009")$age_c, na.rm=TRUE) # 4.934678
mean(subset(wh_long_cog, wh_long_cog$phase=="011")$age_c, na.rm=TRUE) #67.68027
sd(subset(wh_long_cog, wh_long_cog$phase=="011")$age_c, na.rm=TRUE) # 4.931098
mean(subset(wh_long_cog, wh_long_cog$phase=="012")$age_c, na.rm=TRUE) # 70.87964
sd(subset(wh_long_cog, wh_long_cog$phase=="012")$age_c, na.rm=TRUE) # 4.954808

#subset wh_long_cog to only include phases 5-11, exclude phase 12
wh_long_cog_ph5_11 <- wh_long_cog[!(wh_long_cog$phase=="012"),]
summary(wh_long_cog_ph5_11$phase) #check

#### word recall ####
wordsmodel<- lme(swords~time_since_ph5*scale(age_baseline_ph5, scale=FALSE, center=45), data=wh_long_cog_ph5_11, random=~1+time_since_ph5|MRI_ID, method="ML",na.action=na.exclude, control=list(opt="optim"), correlation=corAR1(form=~time_since_ph5|MRI_ID))
summary(wordsmodel)
#ixed effects: swords ~ time_since_ph5 * scale(age_baseline_ph5, scale = FALSE,      center = 45) 
#Value Std.Error   DF  t-value p-value
#(Intercept)                                                        18.719788 0.3934834 1134 47.57453  0.0000
#time_since_ph5                                                     -0.086912 0.0232425 1134 -3.73938  0.0002
#scale(age_baseline_ph5, scale = FALSE, center = 45)                -0.104018 0.0412802  396 -2.51979  0.0121
#time_since_ph5:scale(age_baseline_ph5, scale = FALSE, center = 45) -0.003325 0.0024411 1134 -1.36195  0.1735

intervals(wordsmodel, which="fixed")
#Approximate 95% confidence intervals
#
#Fixed effects:
#  lower         est.        upper
#(Intercept)                                                        17.948757479 18.719787525 19.490817570
#time_since_ph5                                                     -0.132456099 -0.086912470 -0.041368840
#scale(age_baseline_ph5, scale = FALSE, center = 45)                -0.185067407 -0.104017531 -0.022967654
#time_since_ph5:scale(age_baseline_ph5, scale = FALSE, center = 45) -0.008108032 -0.003324678  0.001458676

words_coef <- coef(wordsmodel) #extract intercept and slope
words_coef <- tibble::rownames_to_column(words_coef, "MRI_ID") #make rownames first column
words_coef <- select(words_coef, MRI_ID, `(Intercept)`, time_since_ph5) #select variables of interest
words_coef <- rename(words_coef, "time_since_ph5_words"="time_since_ph5") #rename extracted slope
words_coef <- rename(words_coef, "intercept_words" = "(Intercept)") #rename extracted intercept
hist(words_coef$intercept_words)
hist(words_coef$time_since_ph5_words)

#### animal recall ####
animalsmodel<- lme(animals~time_since_ph5*scale(age_baseline_ph5, scale=FALSE, center=45), data=wh_long_cog_ph5_11, random=~1+time_since_ph5|MRI_ID, method="ML",na.action=na.exclude, control=list(opt="optim"), correlation=corAR1(form=~time_since_ph5|MRI_ID))
summary(animalsmodel)
#Fixed effects: animals ~ time_since_ph5 * scale(age_baseline_ph5, scale = FALSE,      center = 45) 
#Value Std.Error   DF  t-value p-value
#(Intercept)                                                        17.890598 0.3792598 1137 47.17240  0.0000
#time_since_ph5                                                     -0.059667 0.0222432 1137 -2.68249  0.0074
#scale(age_baseline_ph5, scale = FALSE, center = 45)                -0.088357 0.0397540  396 -2.22261  0.0268
#time_since_ph5:scale(age_baseline_ph5, scale = FALSE, center = 45) -0.005442 0.0023336 1137 -2.33207  0.0199

intervals(animalsmodel, which="fixed")
#Fixed effects:
#  lower         est.       upper
#(Intercept)                                                        17.14743869 17.890597522 18.63375636
#time_since_ph5                                                     -0.10325235 -0.059666938 -0.01608152
#scale(age_baseline_ph5, scale = FALSE, center = 45)                -0.16641079 -0.088357359 -0.01030393
#time_since_ph5:scale(age_baseline_ph5, scale = FALSE, center = 45) -0.01001468 -0.005442057 -0.00086943

animals_coef <- coef(animalsmodel) #extract intercept and slope
animals_coef <- tibble::rownames_to_column(animals_coef, "MRI_ID") #make rownames first column
animals_coef <- select(animals_coef, MRI_ID, `(Intercept)`, time_since_ph5) #select variables of interest
animals_coef <- rename(animals_coef, "time_since_ph5_animals"="time_since_ph5") #rename extracted slope
animals_coef <- rename(animals_coef, "intercept_animals" = "(Intercept)") #rename extracted intercept
hist(animals_coef$intercept_animals)
hist(animals_coef$time_since_ph5_animals)

#### memory #### 
memmodel<- lme(mem~time_since_ph5*scale(age_baseline_ph5, scale=FALSE, center=45), data=wh_long_cog_ph5_11, random=~1+time_since_ph5|MRI_ID, method="ML",na.action=na.exclude, control=list(opt="optim"), correlation=corAR1(form=~time_since_ph5|MRI_ID))
summary(memmodel)
#Fixed effects: mem ~ time_since_ph5 * scale(age_baseline_ph5, scale = FALSE,      center = 45) 
#Value  Std.Error   DF  t-value p-value
#(Intercept)                                                         7.983288 0.20703042 1136 38.56094  0.0000
#time_since_ph5                                                     -0.026480 0.01527611 1136 -1.73342  0.0833
#scale(age_baseline_ph5, scale = FALSE, center = 45)                -0.073789 0.02175628  396 -3.39164  0.0008
#time_since_ph5:scale(age_baseline_ph5, scale = FALSE, center = 45) -0.003143 0.00160820 1136 -1.95407  0.0509

intervals(memmodel, which="fixed")
#Fixed effects:
#  lower         est.         upper
#(Intercept)                                                         7.577612490  7.983288185  8.388964e+00
#time_since_ph5                                                     -0.056413348 -0.026479849  3.453649e-03
#scale(age_baseline_ph5, scale = FALSE, center = 45)                -0.116506027 -0.073789500 -3.107297e-02
#time_since_ph5:scale(age_baseline_ph5, scale = FALSE, center = 45) -0.006293785 -0.003142528  8.728957e-06
#attr(,"label")
#[1] "Fixed effects:"

mem_coef <- coef(memmodel) #extract intercept and slope
mem_coef <- tibble::rownames_to_column(mem_coef, "MRI_ID") #make rownames first column
mem_coef <- select(mem_coef, MRI_ID, `(Intercept)`, time_since_ph5) #select variables of interest
mem_coef <- rename(mem_coef, "time_since_ph5_mem"="time_since_ph5") #rename extracted slope
mem_coef <- rename(mem_coef, "intercept_mem" = "(Intercept)") #rename extracted intercept
hist(mem_coef$intercept_mem)
hist(mem_coef$time_since_ph5_mem)

#### ah4 ####
ah4model<- lme(ah4~time_since_ph5*scale(age_baseline_ph5, scale=FALSE, center=45), data=wh_long_cog_ph5_11, random=~1+time_since_ph5|MRI_ID, method="ML",na.action=na.exclude, control=list(opt="optim"), correlation=corAR1(form=~time_since_ph5|MRI_ID))
summary(ah4model)
#Fixed effects: ah4 ~ time_since_ph5 * scale(age_baseline_ph5, scale = FALSE,      center = 45) 
#Value Std.Error   DF  t-value p-value
#(Intercept)                                                        51.14760 0.8774548 1138 58.29087  0.0000
#time_since_ph5                                                     -0.06051 0.0332875 1138 -1.81782  0.0694
#scale(age_baseline_ph5, scale = FALSE, center = 45)                -0.23223 0.0919884  396 -2.52461  0.0120
#time_since_ph5:scale(age_baseline_ph5, scale = FALSE, center = 45) -0.01877 0.0034946 1138 -5.37150  0.0000

intervals(ah4model, which="fixed")
#Fixed effects:
#  lower        est.        upper
#(Intercept)                                                        49.42822993 51.14760049 52.866971048
#time_since_ph5                                                     -0.12573748 -0.06051074  0.004716011
#scale(age_baseline_ph5, scale = FALSE, center = 45)                -0.41284633 -0.23223490 -0.051623464
#time_since_ph5:scale(age_baseline_ph5, scale = FALSE, center = 45) -0.02561857 -0.01877099 -0.011923422

ah4_coef <- coef(ah4model) #extract intercept and slope
ah4_coef <- tibble::rownames_to_column(ah4_coef, "MRI_ID") #make rownames first column
ah4_coef <- select(ah4_coef, MRI_ID, `(Intercept)`, time_since_ph5) #select variables of interest
ah4_coef <- rename(ah4_coef, "time_since_ph5_ah4"="time_since_ph5") #rename extracted slope
ah4_coef <- rename(ah4_coef, "intercept_ah4" = "(Intercept)") #rename extracted intercept
hist(ah4_coef$intercept_ah4)
hist(ah4_coef$time_since_ph5_ah4)

#### ah4num ####
ahnummodel<- lme(ahnum~time_since_ph5*scale(age_baseline_ph5, scale=FALSE, center=45), data=wh_long_cog_ph5_11, random=~1+time_since_ph5|MRI_ID, method="ML",na.action=na.exclude, control=list(opt="optim"), correlation=corAR1(form=~time_since_ph5|MRI_ID))
summary(ahnummodel)
#Fixed effects: ahnum ~ time_since_ph5 * scale(age_baseline_ph5, scale = FALSE,      center = 45) 
#Value Std.Error   DF  t-value p-value
#(Intercept)                                                        25.145536 0.5058278 1138 49.71165  0.0000
#time_since_ph5                                                     -0.028040 0.0204608 1138 -1.37042  0.1708
#scale(age_baseline_ph5, scale = FALSE, center = 45)                -0.129765 0.0530297  396 -2.44703  0.0148
#time_since_ph5:scale(age_baseline_ph5, scale = FALSE, center = 45) -0.008982 0.0021481 1138 -4.18144  0.0000

intervals(ahnummodel, which="fixed")
#Fixed effects:
#  lower         est.        upper
#(Intercept)                                                        24.15436726 25.145535655 26.136704051
#time_since_ph5                                                     -0.06813283 -0.028039876  0.012053075
#scale(age_baseline_ph5, scale = FALSE, center = 45)                -0.23388418 -0.129764948 -0.025645716
#time_since_ph5:scale(age_baseline_ph5, scale = FALSE, center = 45) -0.01319116 -0.008982026 -0.004772892

ahnum_coef <- coef(ahnummodel) #extract intercept and slope
ahnum_coef <- tibble::rownames_to_column(ahnum_coef, "MRI_ID") #make rownames first column
ahnum_coef <- select(ahnum_coef, MRI_ID, `(Intercept)`, time_since_ph5) #select variables of interest
ahnum_coef <- rename(ahnum_coef, "time_since_ph5_ahnum"="time_since_ph5") #rename extracted slope
ahnum_coef <- rename(ahnum_coef, "intercept_ahnum" = "(Intercept)") #rename extracted intercept
hist(ahnum_coef$intercept_ahnum)
hist(ahnum_coef$time_since_ph5_ahnum)

#### ah4verb #### 
ahverbmodel<- lme(ahverb~time_since_ph5*scale(age_baseline_ph5, scale=FALSE, center=45), data=wh_long_cog_ph5_11, random=~1+time_since_ph5|MRI_ID, method="ML",na.action=na.exclude, control=list(opt="optim"), correlation=corAR1(form=~time_since_ph5|MRI_ID))
summary(ahverbmodel)
#Fixed effects: ahverb ~ time_since_ph5 * scale(age_baseline_ph5, scale = FALSE,      center = 45) 
#Value Std.Error   DF  t-value p-value
#(Intercept)                                                        25.997413 0.4085880 1138 63.62745  0.0000
#time_since_ph5                                                     -0.031916 0.0191391 1138 -1.66760  0.0957
#scale(age_baseline_ph5, scale = FALSE, center = 45)                -0.102213 0.0428377  396 -2.38605  0.0175
#time_since_ph5:scale(age_baseline_ph5, scale = FALSE, center = 45) -0.009814 0.0020093 1138 -4.88411  0.0000

intervals(ahverbmodel, which="fixed")
#Approximate 95% confidence intervals

#Fixed effects:
#  lower         est.        upper
#(Intercept)                                                        25.19678591 25.997413149 26.798040386
#time_since_ph5                                                     -0.06941954 -0.031916472  0.005586598
#scale(age_baseline_ph5, scale = FALSE, center = 45)                -0.18632111 -0.102212932 -0.018104751
#time_since_ph5:scale(age_baseline_ph5, scale = FALSE, center = 45) -0.01375069 -0.009813521 -0.005876349

ahverb_coef <- coef(ahverbmodel) #extract intercept and slope
ahverb_coef <- tibble::rownames_to_column(ahverb_coef, "MRI_ID") #make rownames first column
ahverb_coef <- select(ahverb_coef, MRI_ID, `(Intercept)`, time_since_ph5) #select variables of interest
ahverb_coef <- rename(ahverb_coef, "time_since_ph5_ahverb"="time_since_ph5") #rename extracted slope
ahverb_coef <- rename(ahverb_coef, "intercept_ahverb" = "(Intercept)") #rename extracted intercept
hist(ahverb_coef$intercept_ahverb)
hist(ahverb_coef$time_since_ph5_ahverb)

#### mill hill ####
mhmodel<- lme(mh~time_since_ph5*scale(age_baseline_ph5, scale=FALSE, center=45), data=wh_long_cog_ph5_11, random=~1+time_since_ph5|MRI_ID, method="ML",na.action=na.exclude, control=list(opt="optim"), correlation=corAR1(form=~time_since_ph5|MRI_ID))
summary(mhmodel)
#Fixed effects: mh ~ time_since_ph5 * scale(age_baseline_ph5, scale = FALSE,      center = 45) 
#Value Std.Error   DF  t-value p-value
#(Intercept)                                                        25.701569 0.3816823 1136 67.33759  0.0000
#time_since_ph5                                                      0.052918 0.0154599 1136  3.42293  0.0006
#scale(age_baseline_ph5, scale = FALSE, center = 45)                 0.042218 0.0400160  396  1.05503  0.2921
#time_since_ph5:scale(age_baseline_ph5, scale = FALSE, center = 45) -0.005421 0.0016232 1136 -3.33944  0.0009

intervals(mhmodel, which="fixed")
#Approximate 95% confidence intervals

#Fixed effects:
#  lower         est.        upper
#(Intercept)                                                        24.953663103 25.701568863 26.449474623
#time_since_ph5                                                      0.022624603  0.052918328  0.083212054
#scale(age_baseline_ph5, scale = FALSE, center = 45)                -0.036349914  0.042217933  0.120785779
#time_since_ph5:scale(age_baseline_ph5, scale = FALSE, center = 45) -0.008601406 -0.005420688 -0.002239971

mh_coef <- coef(mhmodel) #extract intercept and slope
mh_coef <- tibble::rownames_to_column(mh_coef, "MRI_ID") #make rownames first column
mh_coef <- select(mh_coef, MRI_ID, `(Intercept)`, time_since_ph5) #select variables of interest
mh_coef <- rename(mh_coef, "time_since_ph5_mh"="time_since_ph5") #rename extracted slope
mh_coef <- rename(mh_coef, "intercept_mh" = "(Intercept)") #rename extracted intercept
hist(mh_coef$intercept_mh)
hist(mh_coef$time_since_ph5_mh)

#merge together 
allcog_int_slope <- merge(words_coef, animals_coef, by="MRI_ID", sort=FALSE)
allcog_int_slope <- merge(allcog_int_slope, mem_coef, by="MRI_ID", sort=FALSE)
allcog_int_slope <- merge(allcog_int_slope, ah4_coef, by="MRI_ID", sort=FALSE)
allcog_int_slope <- merge(allcog_int_slope, ahverb_coef, by="MRI_ID", sort=FALSE)
allcog_int_slope <- merge(allcog_int_slope, ahnum_coef, by="MRI_ID", sort=FALSE)
allcog_int_slope <- merge(allcog_int_slope, mh_coef, by="MRI_ID", sort=FALSE)
#write out
write.csv(x=allcog_int_slope, file="intercepts_slopes_398subj_ph5to11.csv", row.names=FALSE)


#bonf correct pvals
time_psunadj<-c(summary(wordsmodel)$tTable[2,'p-value'])
time_psunadj<-append(time_psunadj,c(summary(animalsmodel)$tTable[2,'p-value']))
time_psunadj<-append(time_psunadj,c(summary(memmodel)$tTable[2,'p-value']))
time_psunadj<-append(time_psunadj,c(summary(ah4model)$tTable[2,'p-value']))
time_psunadj<-append(time_psunadj,c(summary(ahverbmodel)$tTable[2,'p-value']))
time_psunadj<-append(time_psunadj,c(summary(ahnummodel)$tTable[2,'p-value']))
time_psunadj<-append(time_psunadj,c(summary(mhmodel)$tTable[2,'p-value']))
time_psadj<-p.adjust(time_psunadj,method="bonferroni")


age_psunadj<-c(summary(wordsmodel)$tTable[3,'p-value'])
age_psunadj<-append(age_psunadj,c(summary(animalsmodel)$tTable[3,'p-value']))
age_psunadj<-append(age_psunadj,c(summary(memmodel)$tTable[3,'p-value']))
age_psunadj<-append(age_psunadj,c(summary(ah4model)$tTable[3,'p-value']))
age_psunadj<-append(age_psunadj,c(summary(ahverbmodel)$tTable[3,'p-value']))
age_psunadj<-append(age_psunadj,c(summary(ahnummodel)$tTable[3,'p-value']))
age_psunadj<-append(age_psunadj,c(summary(mhmodel)$tTable[3,'p-value']))
age_psadj<-p.adjust(age_psunadj,method="bonferroni")


time.age_psunadj<-c(summary(wordsmodel)$tTable[4,'p-value'])
time.age_psunadj<-append(time.age_psunadj,c(summary(animalsmodel)$tTable[4,'p-value']))
time.age_psunadj<-append(time.age_psunadj,c(summary(memmodel)$tTable[4,'p-value']))
time.age_psunadj<-append(time.age_psunadj,c(summary(ah4model)$tTable[4,'p-value']))
time.age_psunadj<-append(time.age_psunadj,c(summary(ahverbmodel)$tTable[4,'p-value']))
time.age_psunadj<-append(time.age_psunadj,c(summary(ahnummodel)$tTable[4,'p-value']))
time.age_psunadj<-append(time.age_psunadj,c(summary(mhmodel)$tTable[4,'p-value']))
time.age_psadj<-p.adjust(time.age_psunadj,method="bonferroni")

df_pvals<-as.data.frame(cbind(time_psunadj,time_psadj, age_psunadj, age_psadj, time.age_psunadj, time.age_psadj))
df_pvals
#time_psunadj   time_psadj age_psunadj age_psadj time.age_psunadj time.age_psadj
#1 7.451189e-01 1.0000000000  0.09605736 0.6724015     1.884273e-01   1.000000e+00
#2 1.549079e-01 1.0000000000  0.31201466 1.0000000     1.750383e-02   1.225268e-01
#3 1.861999e-01 1.0000000000  0.06199765 0.4339835     4.286583e-02   3.000608e-01
#4 3.745405e-05 0.0002621784  0.26249407 1.0000000     5.988808e-08   4.192166e-07
#5 1.357351e-04 0.0009501457  0.44484228 1.0000000     5.632337e-07   3.942636e-06
#6 1.543487e-03 0.0108044123  0.20380917 1.0000000     3.022692e-05   2.115885e-04
#7 7.879555e-04 0.0055156884  0.08828283 0.6179798     1.102432e-03   7.717024e-03

old<-read.csv('../ph5-11/intercepts_slopes_398subj_ph5to11.csv')

plot(old$intercept_ah4, allcog_int_slope$intercept_ah4)
plot(old$time_p1_ah4, allcog_int_slope$time_since_ph5_ah4)
