#Load necessary packages
library(nlme)
library(dplyr)

wh_long_cog <- read.csv("long_file_Rai_24-4-2.csv")


wh_long_cog$phase[wh_long_cog$phase==3] <- "003"
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
mean(subset(wh_long_cog, wh_long_cog$phase=="003")$age_c, na.rm=TRUE) # 47.42073
sd(subset(wh_long_cog, wh_long_cog$phase=="003")$age_c, na.rm=TRUE) # 4.916547
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
wh_long_cog_ph5_11 <- wh_long_cog[!(wh_long_cog$phase=="012" | wh_long_cog$phase=="003"),]
summary(wh_long_cog_ph5_11$phase) #check

#### word recall ####
wordsmodel<- lme(swords~time_p1*age_baseline, data=wh_long_cog_ph5_11, random=~1+time_p1|MRI_ID, method="ML",na.action=na.exclude, control=list(opt="optim"), correlation=corAR1(form=~time_p1|MRI_ID))
summary(wordsmodel)
#Fixed effects: swords ~ time_p1 * age_baseline 
#Value Std.Error   DF   t-value p-value
#(Intercept)          22.505997 2.3971983 1134  9.388459  0.0000
#time_p1               0.037749 0.1160941 1134  0.325161  0.7451
#age_baseline         -0.083894 0.0502885  396 -1.668246  0.0961
#time_p1:age_baseline -0.003205 0.0024356 1134 -1.316038  0.1884

intervals(wordsmodel, which="fixed")
#Approximate 95% confidence intervals
#
#Fixed effects:
#  lower         est.        upper
#(Intercept)          17.808691162 22.505997226 27.203303289
#time_p1              -0.189736815  0.037749314  0.265235443
#age_baseline         -0.182630450 -0.083893603  0.014843245
#time_p1:age_baseline -0.007978025 -0.003205394  0.001567238
#attr(,"label")
#[1] "Fixed effects:"

words_coef <- coef(wordsmodel) #extract intercept and slope
words_coef <- tibble::rownames_to_column(words_coef, "MRI_ID") #make rownames first column
words_coef <- select(words_coef, MRI_ID, `(Intercept)`, time_p1) #select variables of interest
words_coef <- rename(words_coef, "time_p1_words"="time_p1") #rename extracted slope
words_coef <- rename(words_coef, "intercept_words" = "(Intercept)") #rename extracted intercept
hist(words_coef$intercept_words)
hist(words_coef$time_p1_words)

#### animal recall ####
animalsmodel<- lme(animals~time_p1*age_baseline, data=wh_long_cog_ph5_11, random=~1+time_p1|MRI_ID, method="ML",na.action=na.exclude, control=list(opt="optim"), correlation=corAR1(form=~time_p1|MRI_ID))
summary(animalsmodel)
#Fixed effects: animals ~ time_p1 * age_baseline 
#Value Std.Error   DF   t-value p-value
#(Intercept)          20.133358 2.3447847 1137  8.586442  0.0000
#time_p1               0.157982 0.1109931 1137  1.423354  0.1549
#age_baseline         -0.049783 0.0491786  396 -1.012296  0.3120
#time_p1:age_baseline -0.005540 0.0023281 1137 -2.379426  0.0175
intervals(animalsmodel, which="fixed")
#Approximate 95% confidence intervals
#
#Fixed effects:
#  lower         est.         upper
#(Intercept)          15.53875764 20.133358125 24.7279586081
#time_p1              -0.05950826  0.157982410  0.3754730774
#age_baseline         -0.14634120 -0.049783303  0.0467745935
#time_p1:age_baseline -0.01010143 -0.005539532 -0.0009776339
#attr(,"label")
#[1] "Fixed effects:"
animals_coef <- coef(animalsmodel) #extract intercept and slope
animals_coef <- tibble::rownames_to_column(animals_coef, "MRI_ID") #make rownames first column
animals_coef <- select(animals_coef, MRI_ID, `(Intercept)`, time_p1) #select variables of interest
animals_coef <- rename(animals_coef, "time_p1_animals"="time_p1") #rename extracted slope
animals_coef <- rename(animals_coef, "intercept_animals" = "(Intercept)") #rename extracted intercept
hist(animals_coef$intercept_animals)
hist(animals_coef$time_p1_animals)

#### memory #### 
memmodel<- lme(mem~time_p1*age_baseline, data=wh_long_cog_ph5_11, random=~1+time_p1|MRI_ID, method="ML",na.action=na.exclude, control=list(opt="optim"), correlation=corAR1(form=~time_p1|MRI_ID))
summary(memmodel)
#Fixed effects: mem ~ time_p1 * age_baseline 
#Value Std.Error   DF   t-value p-value
#(Intercept)          10.199461 1.3513203 1136  7.547775  0.0000
#time_p1               0.099732 0.0753999 1136  1.322704  0.1862
#age_baseline         -0.053074 0.0283573  396 -1.871610  0.0620
#time_p1:age_baseline -0.003208 0.0015825 1136 -2.027291  0.0429
intervals(memmodel, which="fixed")
#Approximate 95% confidence intervals
#
#Fixed effects:
#  lower        est.         upper
#(Intercept)           7.551551964 10.19946139 12.8473708077
#time_p1              -0.048014157  0.09973171  0.2474775834
#age_baseline         -0.108751012 -0.05307388  0.0026032485
#time_p1:age_baseline -0.006309098 -0.00320819 -0.0001072818
#attr(,"label")
#[1] "Fixed effects:"
mem_coef <- coef(memmodel) #extract intercept and slope
mem_coef <- tibble::rownames_to_column(mem_coef, "MRI_ID") #make rownames first column
mem_coef <- select(mem_coef, MRI_ID, `(Intercept)`, time_p1) #select variables of interest
mem_coef <- rename(mem_coef, "time_p1_mem"="time_p1") #rename extracted slope
mem_coef <- rename(mem_coef, "intercept_mem" = "(Intercept)") #rename extracted intercept
hist(mem_coef$intercept_mem)
hist(mem_coef$time_p1_mem)

#### ah4 ####
ah4model<- lme(ah4~time_p1*age_baseline, data=wh_long_cog_ph5_11, random=~1+time_p1|MRI_ID, method="ML",na.action=na.exclude, control=list(opt="optim"), correlation=corAR1(form=~time_p1|MRI_ID))
summary(ah4model)
#Fixed effects: ah4 ~ time_p1 * age_baseline 
#Value Std.Error   DF   t-value p-value
#(Intercept)          55.68596  4.665314 1138 11.936166  0.0000
#time_p1               0.68768  0.166146 1138  4.139005  0.0000
#age_baseline         -0.10981  0.097857  396 -1.122114  0.2625
#time_p1:age_baseline -0.01901  0.003485 1138 -5.455496  0.0000

intervals(ah4model, which="fixed")
#Approximate 95% confidence intervals
#
#Fixed effects:
#  lower        est.       upper
#(Intercept)          46.54428703 55.68595811 64.82762919
#time_p1               0.36211622  0.68767878  1.01324133
#age_baseline         -0.30193994 -0.10980646  0.08232702
#time_p1:age_baseline -0.02584385 -0.01901431 -0.01218478
#attr(,"label")
#[1] "Fixed effects:"
ah4_coef <- coef(ah4model) #extract intercept and slope
ah4_coef <- tibble::rownames_to_column(ah4_coef, "MRI_ID") #make rownames first column
ah4_coef <- select(ah4_coef, MRI_ID, `(Intercept)`, time_p1) #select variables of interest
ah4_coef <- rename(ah4_coef, "time_p1_ah4"="time_p1") #rename extracted slope
ah4_coef <- rename(ah4_coef, "intercept_ah4" = "(Intercept)") #rename extracted intercept
hist(ah4_coef$intercept_ah4)
hist(ah4_coef$time_p1_ah4)

#### ah4num ####
ahnummodel<- lme(ahnum~time_p1*age_baseline, data=wh_long_cog_ph5_11, random=~1+time_p1|MRI_ID, method="ML",na.action=na.exclude, control=list(opt="optim"), correlation=corAR1(form=~time_p1|MRI_ID))
summary(ahnummodel)
#Fixed effects: ahnum ~ time_p1 * age_baseline 
#Value Std.Error   DF   t-value p-value
#(Intercept)          28.132272 2.7361635 1138 10.281649  0.0000
#time_p1               0.324454 0.1022204 1138  3.174067  0.0015
#age_baseline         -0.073053 0.0573925  396 -1.272875  0.2038
#time_p1:age_baseline -0.008982 0.0021444 1138 -4.188677  0.0000
intervals(ahnummodel, which="fixed")
#Approximate 95% confidence intervals
#
#Fixed effects:
#  lower         est.        upper
#(Intercept)          22.77076580 28.132271831 33.493777861
#time_p1               0.12415373  0.324454281  0.524754833
#age_baseline         -0.18573865 -0.073053444  0.039631761
#time_p1:age_baseline -0.01318398 -0.008982087 -0.004780192
#attr(,"label")
#[1] "Fixed effects:"
ahnum_coef <- coef(ahnummodel) #extract intercept and slope
ahnum_coef <- tibble::rownames_to_column(ahnum_coef, "MRI_ID") #make rownames first column
ahnum_coef <- select(ahnum_coef, MRI_ID, `(Intercept)`, time_p1) #select variables of interest
ahnum_coef <- rename(ahnum_coef, "time_p1_ahnum"="time_p1") #rename extracted slope
ahnum_coef <- rename(ahnum_coef, "intercept_ahnum" = "(Intercept)") #rename extracted intercept
hist(ahnum_coef$intercept_ahnum)
hist(ahnum_coef$time_p1_ahnum)

#### ah4verb #### 
ahverbmodel<- lme(ahverb~time_p1*age_baseline, data=wh_long_cog_ph5_11, random=~1+time_p1|MRI_ID, method="ML",na.action=na.exclude, control=list(opt="optim"), correlation=corAR1(form=~time_p1|MRI_ID))
summary(ahverbmodel)
#Fixed effects: ahverb ~ time_p1 * age_baseline 
#Value Std.Error   DF   t-value p-value
#(Intercept)          27.521615 2.2519614 1138 12.221175  0.0000
#time_p1               0.365391 0.0954297 1138  3.828897  0.0001
#age_baseline         -0.036126 0.0472353  396 -0.764807  0.4448
#time_p1:age_baseline -0.010074 0.0020019 1138 -5.032174  0.0000
intervals(ahverbmodel, which="fixed")
#Approximate 95% confidence intervals

#Fixed effects:
#  lower        est.        upper
#(Intercept)          23.10890181 27.52161497 31.934328127
#time_p1               0.17839631  0.36539068  0.552385049
#age_baseline         -0.12886843 -0.03612589  0.056616647
#time_p1:age_baseline -0.01399645 -0.01007378 -0.006151114
#attr(,"label")
#[1] "Fixed effects:"
ahverb_coef <- coef(ahverbmodel) #extract intercept and slope
ahverb_coef <- tibble::rownames_to_column(ahverb_coef, "MRI_ID") #make rownames first column
ahverb_coef <- select(ahverb_coef, MRI_ID, `(Intercept)`, time_p1) #select variables of interest
ahverb_coef <- rename(ahverb_coef, "time_p1_ahverb"="time_p1") #rename extracted slope
ahverb_coef <- rename(ahverb_coef, "intercept_ahverb" = "(Intercept)") #rename extracted intercept
hist(ahverb_coef$intercept_ahverb)
hist(ahverb_coef$time_p1_ahverb)

#### mill hill ####
mhmodel<- lme(mh~time_p1*age_baseline, data=wh_long_cog_ph5_11, random=~1+time_p1|MRI_ID, method="ML",na.action=na.exclude, control=list(opt="optim"), correlation=corAR1(form=~time_p1|MRI_ID))
summary(mhmodel)
#Fixed effects: mh ~ time_p1 * age_baseline 
#Value Std.Error   DF   t-value p-value
#(Intercept)          22.445690 2.0898641 1136 10.740263  0.0000
#time_p1               0.259579 0.0771162 1136  3.366078  0.0008
#age_baseline          0.074907 0.0438376  396  1.708739  0.0883
#time_p1:age_baseline -0.005293 0.0016178 1136 -3.271374  0.0011
intervals(mhmodel, which="fixed")
#Approximate 95% confidence intervals
#
#Fixed effects:
#  lower         est.        upper
#(Intercept)          18.350605268 22.445689734 26.540774200
#time_p1               0.108470144  0.259579245  0.410688345
#age_baseline         -0.011164217  0.074907091  0.160978399
#time_p1:age_baseline -0.008462764 -0.005292591 -0.002122418
#attr(,"label")
#[1] "Fixed effects:"
mh_coef <- coef(mhmodel) #extract intercept and slope
mh_coef <- tibble::rownames_to_column(mh_coef, "MRI_ID") #make rownames first column
mh_coef <- select(mh_coef, MRI_ID, `(Intercept)`, time_p1) #select variables of interest
mh_coef <- rename(mh_coef, "time_p1_mh"="time_p1") #rename extracted slope
mh_coef <- rename(mh_coef, "intercept_mh" = "(Intercept)") #rename extracted intercept
hist(mh_coef$intercept_mh)
hist(mh_coef$time_p1_mh)

#merge together 
allcog_int_slope <- merge(words_coef, animals_coef, by="MRI_ID", sort=FALSE)
allcog_int_slope <- merge(allcog_int_slope, mem_coef, by="MRI_ID", sort=FALSE)
allcog_int_slope <- merge(allcog_int_slope, ah4_coef, by="MRI_ID", sort=FALSE)
allcog_int_slope <- merge(allcog_int_slope, ahverb_coef, by="MRI_ID", sort=FALSE)
allcog_int_slope <- merge(allcog_int_slope, ahnum_coef, by="MRI_ID", sort=FALSE)
allcog_int_slope <- merge(allcog_int_slope, mh_coef, by="MRI_ID", sort=FALSE)
#write out
write.csv(x=allcog_int_slope, file="intercepts_slopes_398subj_ph5to11.csv", row.names=FALSE)

#fdr correct pvals
ps_unadj<-c(summary(wordsmodel)$tTable[2:4,'p-value'])
ps_unadj<-append(ps_unadj,c(summary(animalsmodel)$tTable[2:4,'p-value']))
ps_unadj<-append(ps_unadj,c(summary(memmodel)$tTable[2:4,'p-value']))
ps_unadj<-append(ps_unadj,c(summary(ah4model)$tTable[2:4,'p-value']))
ps_unadj<-append(ps_unadj,c(summary(ahverbmodel)$tTable[2:4,'p-value']))
ps_unadj<-append(ps_unadj,c(summary(ahnummodel)$tTable[2:4,'p-value']))
ps_unadj<-append(ps_unadj,c(summary(mhmodel)$tTable[2:4,'p-value']))
ps_adj<-p.adjust(ps_unadj,method="fdr")
df_pvals<-cbind(ps_unadj,ps_adj)
