library(ggplot2)
library(ggpubr)
library(cowplot)
library(gridExtra)
library(effects)
pls_demo<-read.csv('demo_nmfk10_pls_int-slope.csv')
link<-read.table('link_copy.txt')
names(link)<-c("WH_id","id")
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

#subset wh_long_cog to only include phase 12
wh_long_cog_ph12 <- wh_long_cog[(wh_long_cog$phase=="012"),]
pls_demo_link <- merge(pls_demo, link, by.x="oxmg_id", by.y="id", sort=FALSE, all=FALSE)
pls_demo_cog_ph12 <- merge(pls_demo_link, wh_long_cog_ph12, by.x="WH_id", by.y = "MRI_ID", sort=FALSE, all=FALSE)

partial_plot <- function(df,vs,lv1label,lv2label,behavlabel){
  pred<-data.frame(predict(vs))
  new<-merge(x=df, y=pred, by="row.names", all = FALSE)
  p1<-ggplot(new, aes(x=vsc_LV_1, y=predict.vs.)) + geom_smooth(method = "lm", se = FALSE, colour="maroon") +
    geom_point(colour="black") + stat_smooth(method="lm",fill=NA,colour="black",linetype=4,geom="ribbon") +
    xlab(lv1label)+ ylab(behavlabel)
  p2<-ggplot(new, aes(x=vsc_LV_2, y=predict.vs.)) + geom_smooth(method = "lm", se = FALSE, colour="maroon") +
    geom_point(colour="black") + stat_smooth(method="lm",fill=NA,colour="black",linetype=4,geom="ribbon") +
    xlab(lv2label)+ ylab(behavlabel)
  grid.arrange(p1,p2,nrow=1)
}


raw_plot <- function(df,behav,lv1label,lv2label,behavlabel){
  p1<-ggplot(aes(x=vsc_LV_1,y=behav), data=df) + geom_point()+ theme(text=element_text(size=20))+ 
    xlab(lv1label) + ylab(behavlabel)+ stat_smooth(method="lm")
  p2<-ggplot(aes(x=vsc_LV_2,y=behav), data=df) + geom_point()+ theme(text=element_text(size=20))+
    xlab(lv2label) + ylab("")+ stat_smooth(method="lm")
  grid.arrange(p1,p2,nrow=1)
}



  # Now running models for each cog test #
# I run an lm covarying for age, sex, edu #
# 6 total tests, so using p < 0.0083 (0.05/6, bonferroni) as threshold #




  #### animals ####
ps_vector<-vector()
vs<-lm(animals~vsc_LV_1 + vsc_LV_2 + age_c + OX.SEX + OX.EDUC_ftht, data=pls_demo_cog_ph12)
summary(vs)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  28.73737    1.94073  14.807  < 2e-16 ***
#  vsc_LV_1     -0.83856    0.05588 -15.007  < 2e-16 ***
#  vsc_LV_2     -0.48285    0.10051  -4.804 2.24e-06 ***
#  age_c        -0.18474    0.02616  -7.062 7.86e-12 ***
#  OX.SEXMale   -0.71910    0.32379  -2.221   0.0269 *  
#  OX.EDUC_ftht  0.05161    0.03842   1.343   0.1800     
confint(vs)
#2.5 %      97.5 %
#  (Intercept)  24.92148948 32.55325870
#vsc_LV_1     -0.94842592 -0.72869065
#vsc_LV_2     -0.68046919 -0.28523341
#age_c        -0.23618404 -0.13330387
#OX.SEXMale   -1.35574471 -0.08245135
#OX.EDUC_ftht -0.02393596  0.12716007

ps_vector<-summary(vs)$coefficients[2,4]
ps_vector<-append(ps_vector, summary(vs)$coefficients[3,4])


#sem_flu_plots<-effect_plot(pls_demo_cog_ph12,vs,pls_demo_cog_ph12$animals,"LV1","LV2","Semantic Fluency")

#dev.new(width = 3.4, height = 1.55, unit="in", noRStudioGD = T) 
#p<-grid.arrange(sem_flu_plots$p_lv1, sem_flu_plots$p_lv2, nrow=1,  widths = c(1.6, 1.4))
#p
#ggsave(p,file="figures/semfluency_lv1lv2.png", dpi=300)
#dev.off()

#### words ####
vs<-lm(swords~vsc_LV_1 + vsc_LV_2 + age_c + OX.SEX + OX.EDUC_ftht, data=pls_demo_cog_ph12)
summary(vs)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  34.93719    2.90271  12.036  < 2e-16 ***
#  vsc_LV_1     -0.84433    0.08358 -10.103  < 2e-16 ***
#  vsc_LV_2     -0.63135    0.15033  -4.200 3.33e-05 ***
#  age_c        -0.23870    0.03913  -6.100 2.60e-09 ***
#  OX.SEXMale   -0.96069    0.48429  -1.984    0.048 *  
#  OX.EDUC_ftht -0.09467    0.05747  -1.647    0.100   
confint(vs)
#2.5 %       97.5 %
#  (Intercept)  29.2298513 40.644528715
#vsc_LV_1     -1.0086526 -0.679999139
#vsc_LV_2     -0.9269231 -0.335777199
#age_c        -0.3156406 -0.161764855
#OX.SEXMale   -1.9129086 -0.008470433
#OX.EDUC_ftht -0.2076631  0.018328091

ps_vector<-append(ps_vector, summary(vs)$coefficients[2,4])
ps_vector<-append(ps_vector, summary(vs)$coefficients[3,4])


#lex_flu_plots<-effect_plot(pls_demo_cog_ph12,vs,pls_demo_cog_ph12$swords,"LV1","LV2","Lexical Fluency")

#dev.new(width = 3.4, height = 1.55, unit="in", noRStudioGD = T) 
#p<-grid.arrange(lex_flu_plots$p_lv1, lex_flu_plots$p_lv2, nrow=1,  widths = c(1.6, 1.4))
#p
#ggsave(p,file="figures/lexfluency_lv1lv2.png", dpi=300)
#dev.off()

#### mem ####
vs<-lm(mem~vsc_LV_1 + vsc_LV_2 + age_c + OX.SEX + OX.EDUC_ftht, data=pls_demo_cog_ph12)
summary(vs)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  14.97292    1.43139  10.460  < 2e-16 ***
#  vsc_LV_1     -0.36306    0.04129  -8.792  < 2e-16 ***
#  vsc_LV_2     -0.22663    0.07403  -3.061  0.00236 ** 
#  age_c        -0.13344    0.01928  -6.921 1.93e-11 ***
#  OX.SEXMale   -0.50157    0.23862  -2.102  0.03622 *  
#  OX.EDUC_ftht  0.04034    0.02835   1.423  0.15556  
confint(vs)
#2.5 %      97.5 %
#  (Intercept)  12.15846776 17.78738059
#vsc_LV_1     -0.44424973 -0.28186474
#vsc_LV_2     -0.37219481 -0.08107051
#age_c        -0.17135201 -0.09552967
#OX.SEXMale   -0.97075267 -0.03238629
#OX.EDUC_ftht -0.01540172  0.09608744

ps_vector<-append(ps_vector, summary(vs)$coefficients[2,4])
ps_vector<-append(ps_vector, summary(vs)$coefficients[3,4])


#mem_plots<-effect_plot(pls_demo_cog_ph12,vs,pls_demo_cog_ph12$mem,"LV1","LV2","Memory")

#dev.new(width = 3.4, height = 1.55, unit="in", noRStudioGD = T) 
#p<-grid.arrange(mem_plots$p_lv1, mem_plots$p_lv2, nrow=1,  widths = c(1.6, 1.4))
#p
#ggsave(p,file="figures/mem_lv1lv2.png", dpi=300)
#dev.off()

#### ah4 ####
vs<-lm(ah4~vsc_LV_1 + vsc_LV_2 + age_c + OX.SEX + OX.EDUC_ftht, data=pls_demo_cog_ph12)
summary(vs)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  84.78107    3.77319  22.469  < 2e-16 ***
#  vsc_LV_1     -3.34847    0.10840 -30.889  < 2e-16 ***
#  vsc_LV_2      1.12958    0.19544   5.780 1.56e-08 ***
#  age_c        -0.55026    0.05089 -10.813  < 2e-16 ***
#  OX.SEXMale    1.15848    0.62937   1.841   0.0664 .  
#OX.EDUC_ftht -0.05491    0.07444  -0.738   0.4612  
confint(vs)
#2.5 %      97.5 %
#  (Intercept)  77.36224849 92.19989470
#vsc_LV_1     -3.56161563 -3.13532811
#vsc_LV_2      0.74530241  1.51385123
#age_c        -0.65030918 -0.45020276
#OX.SEXMale   -0.07899584  2.39594888
#OX.EDUC_ftht -0.20128302  0.09146301

ps_vector<-append(ps_vector, summary(vs)$coefficients[2,4])
ps_vector<-append(ps_vector, summary(vs)$coefficients[3,4])

#ah4_plots<-effect_plot(pls_demo_cog_ph12,vs,pls_demo_cog_ph12$ah4,"LV1","LV2","Inductive Reasoning")

#dev.new(width = 3.4, height = 1.55, unit="in", noRStudioGD = T) 
#p<-grid.arrange(ah4_plots$p_lv1, ah4_plots$p_lv2, nrow=1,  widths = c(1.6, 1.4))
#p
#ggsave(p,file="figures/ah4_lv1lv2.png", dpi=300)
#dev.off()



#### ahnum ####
vs<-lm(ahnum~vsc_LV_1 + vsc_LV_2 + age_c + OX.SEX + OX.EDUC_ftht, data=pls_demo_cog_ph12)
summary(vs)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  41.36145    2.23229  18.529  < 2e-16 ***
#  vsc_LV_1     -1.84387    0.06413 -28.750  < 2e-16 ***
#  vsc_LV_2      0.61804    0.11563   5.345 1.56e-07 ***
#  age_c        -0.27554    0.03011  -9.153  < 2e-16 ***
#  OX.SEXMale    0.73600    0.37235   1.977   0.0488 *  
#  OX.EDUC_ftht -0.03964    0.04404  -0.900   0.3686  
confint(vs)
#2.5 %      97.5 %
#  (Intercept)  36.972336644 45.75056344
#vsc_LV_1     -1.969967792 -1.71776819
#vsc_LV_2      0.390699113  0.84538685
#age_c        -0.334735335 -0.21634866
#OX.SEXMale    0.003890407  1.46811362
#OX.EDUC_ftht -0.126238759  0.04695522

ps_vector<-append(ps_vector, summary(vs)$coefficients[2,4])
ps_vector<-append(ps_vector, summary(vs)$coefficients[3,4])


#ahnum_plots<-effect_plot(pls_demo_cog_ph12,vs,pls_demo_cog_ph12$ahnum,"LV1","LV2","Mathematical Reasoning")

#dev.new(width = 3.4, height = 1.55, unit="in", noRStudioGD = T) 
#p<-grid.arrange(ahnum_plots$p_lv1, ahnum_plots$p_lv2, nrow=1,  widths = c(1.6, 1.4))
#print(p)
#ggsave(plot = p,filename ="figures/ahnum_lv1lv2.png", dpi=300)
#dev.off()

#### ahverb ####
vs<-lm(ahverb~vsc_LV_1 + vsc_LV_2 + age_c + OX.SEX + OX.EDUC_ftht, data=pls_demo_cog_ph12)
summary(vs)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  43.41962    2.04323  21.250  < 2e-16 ***
#  vsc_LV_1     -1.50460    0.05870 -25.631  < 2e-16 ***
#  vsc_LV_2      0.51153    0.10583   4.833 1.95e-06 ***
#  age_c        -0.27471    0.02756  -9.969  < 2e-16 ***
#  OX.SEXMale    0.42247    0.34081   1.240    0.216    
#OX.EDUC_ftht -0.01527    0.04031  -0.379    0.705     
confint(vs)
#2.5 %      97.5 %
#  (Intercept)  39.40223716 47.43700594
#vsc_LV_1     -1.62002386 -1.38918390
#vsc_LV_2      0.30344450  0.71962318
#age_c        -0.32889402 -0.22053392
#OX.SEXMale   -0.24763207  1.09258108
#OX.EDUC_ftht -0.09453103  0.06399456

ps_vector<-append(ps_vector, summary(vs)$coefficients[2,4])
ps_vector<-append(ps_vector, summary(vs)$coefficients[3,4])


#ahverb_plots<-effect_plot(pls_demo_cog_ph12,vs,pls_demo_cog_ph12$ahverb,"LV1","LV2","Verbal Reasoning")

#dev.new(width = 3.4, height = 1.55, unit="in", noRStudioGD = T) 
#p<-grid.arrange(ahverb_plots$p_lv1, ahverb_plots$p_lv2, nrow=1,  widths = c(1.6, 1.4))
#p
#ggsave(p,file="figures/ahverb_lv1lv2.png", dpi=300)
#dev.off()

ps_vector
#[1]  2.517026e-40  2.238529e-06  2.033797e-21  3.328739e-05  5.202167e-17  2.360605e-03 6.625859e-106  1.556071e-08  1.420039e-97
# [10]  1.557159e-07  5.172665e-85  1.948123e-06
