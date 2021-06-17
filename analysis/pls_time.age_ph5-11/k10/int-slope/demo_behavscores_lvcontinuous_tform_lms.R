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

summary(pls_demo_cog_ph12$LV_Group)
#A   B   C   D 
#79  94 112 113 

df_lv_ph12<-pls_demo_cog_ph12[,c("swords","mem","animals","ah4","ahnum","ahverb","vsc_LV_1","vsc_LV_2")]
write.csv(df_lv_ph12,"df_lv_ph12.csv") #for plotting schematics

inormal <- function(x)
{
  qnorm((rank(x, na.last = "keep") - 0.5) / sum(!is.na(x)))
}




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



effect_plot <- function(df, vs, behav,lv1label, lv2label, behavlabel){
  fitdata_vs_1 = as.data.frame(Effect(c("vsc_LV_1"), vs))
  p1<-ggplot(data = df, aes(y=inormal(sqrt(behav)),x=vsc_LV_1)) +
    geom_point(size=0.2) + theme(text=element_text(size=8), plot.margin=unit(c(0.5,0.5,0.5,0.5),"mm"), axis.title.x=element_text(vjust = 2), axis.title.y=element_text(vjust = -1)) +
    geom_line(data = fitdata_vs_1, aes(y=fit)) +
    geom_ribbon(data = fitdata_vs_1, aes(y=fit, ymin=lower, ymax=upper), alpha=0.4) +
    xlab(lv1label) + ylab(behavlabel) + scale_y_continuous(breaks = seq(-2.5, 2.5, by = 2.5)) +
    scale_x_continuous(breaks = seq(-6, 9, by = 3))
  
  fitdata_vs_2 = as.data.frame(Effect(c("vsc_LV_2"), vs))
  p2<-ggplot(data = df, aes(y=inormal(sqrt(behav)),x=vsc_LV_2)) +
    geom_point(size=0.2) + theme(text=element_text(size=8), plot.margin=unit(c(0.5,0.5,0.5,0.5),"mm"),axis.title.y=element_blank(), axis.text.y = element_blank(), axis.title.x=element_text(vjust = 2)) +
    geom_line(data = fitdata_vs_2, aes(y=fit)) +
    geom_ribbon(data = fitdata_vs_2, aes(y=fit, ymin=lower, ymax=upper), alpha=0.4) +
    xlab(lv2label) + scale_y_continuous(breaks = seq(-2.5, 2.5, by = 2.5), labels=NULL) +
    scale_x_continuous(breaks = seq(-6, 4, by = 2))
  grid.arrange(p1,p2,nrow=1)
  return(list("p_lv1" = p1, "p_lv2" = p2))
}

#### MOCA  ####
#is moca related to LV?
vs<-lm(vsc_LV_1~MOCA_edu_group,data=pls_demo_cog_ph12)
summary(vs) 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)             -0.5089     0.1275  -3.990 7.87e-05 ***
#  MOCA_edu_groupimpaired   2.6648     0.2919   9.131  < 2e-16 ***
t_test<-t.test(subset(pls_demo_cog_ph12, pls_demo_cog_ph12$MOCA_edu_group=="impaired")$vsc_LV_1,
               subset(pls_demo_cog_ph12, pls_demo_cog_ph12$MOCA_edu_group=="healthy")$vsc_LV_1)
t_test #yes
#Welch Two Sample t-test
#
#data:  subset(pls_demo_cog_ph12, pls_demo_cog_ph12$MOCA_edu_group ==  and subset(pls_demo_cog_ph12, pls_demo_cog_ph12$MOCA_edu_group ==     "impaired")$vsc_LV_1 and     "healthy")$vsc_LV_1
#t = 7.9255, df = 98.461, p-value = 3.567e-12
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  1.997605 3.332020
#sample estimates:
#  mean of x  mean of y 
#2.1559540 -0.5088587 

#LV2
vs<-lm(vsc_LV_2~MOCA_edu_group,data=pls_demo_cog_ph12)
summary(vs) 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
#(Intercept)            -0.02781    0.07252  -0.384    0.702
#MOCA_edu_groupimpaired  0.14564    0.16595   0.878    0.381
t_test<-t.test(subset(pls_demo_cog_ph12, pls_demo_cog_ph12$MOCA_edu_group=="impaired")$vsc_LV_2,
               subset(pls_demo_cog_ph12, pls_demo_cog_ph12$MOCA_edu_group=="healthy")$vsc_LV_2)
t_test #no
#Welch Two Sample t-test
#
#data:  subset(pls_demo_cog_ph12, pls_demo_cog_ph12$MOCA_edu_group ==  and subset(pls_demo_cog_ph12, pls_demo_cog_ph12$MOCA_edu_group ==     "impaired")$vsc_LV_2 and     "healthy")$vsc_LV_2
#t = 0.79267, df = 101.91, p-value = 0.4298
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.2188046  0.5100920
#sample estimates:
#  mean of x   mean of y 
#0.11783233 -0.02781135

#plot
dat.lv1<-pls_demo_cog_ph12[,c("MOCA_edu_group","vsc_LV_1")]
dat.lv1$LV<-"LV1"
dat.lv2<-pls_demo_cog_ph12[,c("MOCA_edu_group","vsc_LV_2")]
dat.lv2$LV<-"LV2"

names(dat.lv1)<-c("MOCA_status","Score","LV")
names(dat.lv2)<-c("MOCA_status","Score","LV")

dat.lv<-rbind(dat.lv1, dat.lv2)
dodge <- position_dodge(width = 1)
p<-ggplot(dat.lv, aes(x=LV,y=Score, fill=MOCA_status))  + geom_violin(trim=FALSE,position = dodge)
p + geom_boxplot(width=0.1, position = dodge) +
  theme(text=element_text(size=8),plot.margin=unit(c(0.5,0.5,0.5,0.5),"mm"),axis.title.x=element_blank(),
        legend.position="bottom") + labs(fill="MOCA Status")

dev.new(width = 2.7, height = 1.6, unit="in", noRStudioGD = T) 
p<-ggplot(dat.lv, aes(x=LV,y=Score, fill=MOCA_status))  + geom_violin(trim=FALSE,position = dodge) + geom_boxplot(width=0.1, position = dodge) +
  theme(text=element_text(size=8),plot.margin=unit(c(0.5,0,0,0.5),"mm"),axis.title.x=element_blank(),
        legend.position="bottom",legend.margin=margin(0,0,0,-30),legend.box.margin = margin(-10, 0, 0, 0)) + labs(fill="MOCA Status")
p
ggsave(p,file="figures/moca_lv1lv2.png", dpi=200)
dev.off()


#### SEX ####
vs<-lm(vsc_LV_1~OX.SEX,data=pls_demo_cog_ph12)
summary(vs) 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   1.0556     0.2554   4.132 4.39e-05 ***
#  OX.SEXMale   -1.3729     0.2913  -4.713 3.39e-06 ***
t_test<-t.test(subset(pls_demo_cog_ph12, pls_demo_cog_ph12$OX.SEX=="Female")$vsc_LV_1,
               subset(pls_demo_cog_ph12, pls_demo_cog_ph12$OX.SEX=="Male")$vsc_LV_1)
t_test #yes
#Welch Two Sample t-test
#
#data:  subset(pls_demo_cog_ph12, pls_demo_cog_ph12$OX.SEX == "Female")$vsc_LV_1 and subset(pls_demo_cog_ph12, pls_demo_cog_ph12$OX.SEX == "Male")$vsc_LV_1
#t = 4.0649, df = 123.9, p-value = 8.477e-05
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  0.7044095 2.0414212
#sample estimates:
#  mean of x  mean of y 
#1.0555580 -0.3173573 

#plot
p<-ggplot(aes(x=OX.SEX,y=vsc_LV_1), data=pls_demo_cog_ph12) 
p + geom_violin(trim=FALSE) + geom_boxplot(width=0.1)+ theme(text=element_text(size=20))+ xlab("Sex") +
  ylab("LV1") 

#LV2
vs<-lm(vsc_LV_2~OX.SEX,data=pls_demo_cog_ph12)
summary(vs) 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  -0.4386     0.1335  -3.286 0.001106 ** 
#  OX.SEXMale    0.5704     0.1522   3.748 0.000205 ***
t_test<-t.test(subset(pls_demo_cog_ph12, pls_demo_cog_ph12$OX.SEX=="Female")$vsc_LV_2,
               subset(pls_demo_cog_ph12, pls_demo_cog_ph12$OX.SEX=="Male")$vsc_LV_2)
t_test #yes
#Welch Two Sample t-test
#
#data:  subset(pls_demo_cog_ph12, pls_demo_cog_ph12$OX.SEX == "Female")$vsc_LV_2 and subset(pls_demo_cog_ph12, pls_demo_cog_ph12$OX.SEX == "Male")$vsc_LV_2
#t = -3.7904, df = 152.53, p-value = 0.0002161
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.8677447 -0.2731067
#sample estimates:
#  mean of x  mean of y 
#-0.4385685  0.1318572 


#plot
p<-ggplot(aes(x=OX.SEX,y=vsc_LV_2), data=pls_demo_cog_ph12) 
p + geom_violin(trim=FALSE) + geom_boxplot(width=0.1)+ theme(text=element_text(size=20))+ xlab("Sex") +
  ylab("LV2") 

dat.lv1<-pls_demo_cog_ph12[,c("OX.SEX","vsc_LV_1")]
dat.lv1$LV<-"LV1"
dat.lv2<-pls_demo_cog_ph12[,c("OX.SEX","vsc_LV_2")]
dat.lv2$LV<-"LV2"

names(dat.lv1)<-c("Sex","Score","LV")
names(dat.lv2)<-c("Sex","Score","LV")

dat.lv<-rbind(dat.lv1, dat.lv2)
dodge <- position_dodge(width = 1)
p<-ggplot(dat.lv, aes(x=LV,y=Score, fill=Sex))  + geom_violin(trim=FALSE,position = dodge)
p + geom_boxplot(width=0.1, position = dodge) +
  theme(text=element_text(size=8),plot.margin=unit(c(0.5,0.5,0.5,0.5),"mm"),axis.title.x=element_blank(),
        legend.position="bottom") + labs(fill="Sex")

dev.new(width = 2.7, height = 1.6, unit="in", noRStudioGD = T) 
p<-ggplot(dat.lv, aes(x=LV,y=Score, fill=Sex))  + geom_violin(trim=FALSE,position = dodge) + geom_boxplot(width=0.1, position = dodge) +
  theme(text=element_text(size=8),plot.margin=unit(c(0.5,0,0,0.5),"mm"),axis.title.x=element_blank(),
        legend.position="bottom",legend.margin=margin(0,0,0,-30),legend.box.margin = margin(-10, 0, 0, 0)) + 
  labs(fill="Sex")
p
ggsave(p,file="figures/sex_lv1lv2.png", dpi=200)
dev.off()




#### Age ####
#is age related to LV scores?
vs<-lm(OX.AGE~vsc_LV_1,data=pls_demo_cog_ph12)
summary(vs) #no
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 69.52022    0.24608  282.50   <2e-16 ***
#  vsc_LV_1    -0.03817    0.09798   -0.39    0.697  
summary(aov(vs))
#Df Sum Sq Mean Sq F value Pr(>F)
#vsc_LV_1      1      4   3.659   0.152  0.697
#Residuals   396   9544  24.102   

fitdata_vs_1 = as.data.frame(Effect(c("vsc_LV_1"), vs))
p1<-ggplot(data = pls_demo_cog_ph12, aes(y=OX.AGE,x=vsc_LV_1)) +
  geom_point(size=0.2) + theme(text=element_text(size=8), plot.margin=unit(c(0.5,0.5,0.5,0.5),"mm"), axis.title.x=element_text(vjust = 2), axis.title.y=element_text(vjust = -1)) +
  geom_line(data = fitdata_vs_1, aes(y=fit)) +
  geom_ribbon(data = fitdata_vs_1, aes(y=fit, ymin=lower, ymax=upper), alpha=0.4) +
  xlab("LV1") + ylab("Age") 

vs<-lm(OX.AGE~vsc_LV_2,data=pls_demo_cog_ph12)
summary(vs) #no
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 69.52022    0.24609 282.496   <2e-16 ***
#  vsc_LV_2     0.06747    0.18941   0.356    0.722   
summary(aov(vs))
#Df Sum Sq Mean Sq F value Pr(>F)
#vsc_LV_2      1      3   3.059   0.127  0.722
#Residuals   396   9545  24.103   

fitdata_vs_2 = as.data.frame(Effect(c("vsc_LV_2"), vs))
p2<-ggplot(data = pls_demo, aes(y=OX.AGE,x=vsc_LV_2)) +
  geom_point(size=0.2) + theme(text=element_text(size=8), plot.margin=unit(c(0.5,0.5,0.5,0.5),"mm"),axis.title.y=element_blank(), axis.text.y = element_blank(), axis.title.x=element_text(vjust = 2)) +
  geom_line(data = fitdata_vs_2, aes(y=fit)) +
  geom_ribbon(data = fitdata_vs_2, aes(y=fit, ymin=lower, ymax=upper), alpha=0.4) +
  xlab("LV2") 
grid.arrange(p1,p2,nrow=1)

dev.new(width = 3.5, height = 2, unit="in", noRStudioGD = T)
p<-grid.arrange(p1,p2, nrow=1,  widths = c(1.6, 1.4))
p
ggsave(p,file="figures/age_lv1lv2.png", dpi=300)
dev.off()


  #### Educ ####
vs<-lm(OX.EDUC_ftht~vsc_LV_1,data=pls_demo_cog_ph12)
summary(vs) #yes
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 15.84235    0.16907  93.700  < 2e-16 ***
#  vsc_LV_1    -0.43483    0.06732  -6.459 3.09e-10 ***


fitdata_edu_lv1 = as.data.frame(Effect(c("vsc_LV_1"), vs))
edu_lv1<-ggplot(data = pls_demo_cog_ph12, aes(y=OX.EDUC_ftht,x=vsc_LV_1)) +
  geom_point(size=0.2) + theme(text=element_text(size=8), plot.margin=unit(c(0.5,0.5,0.5,0.5),"mm"), axis.title.x=element_text(vjust = 2), axis.title.y=element_text(vjust = -1)) +
  geom_line(data = fitdata_edu_lv1, aes(y=fit)) +
  geom_ribbon(data = fitdata_edu_lv1, aes(y=fit, ymin=lower, ymax=upper), alpha=0.4) +
  xlab("LV1") + ylab("Education Years")  +
  scale_x_continuous(breaks = seq(-6, 9, by = 3))



vs<-lm(OX.EDUC_ftht~vsc_LV_2,data=pls_demo_cog_ph12)
summary(vs) #no
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 15.84235    0.17774  89.133   <2e-16 ***
#  vsc_LV_2    -0.04119    0.13680  -0.301    0.763  

fitdata_edu_lv2 = as.data.frame(Effect(c("vsc_LV_2"), vs))
edu_lv2<-ggplot(data = pls_demo_cog_ph12, aes(y=OX.EDUC_ftht,x=vsc_LV_2)) +
  geom_point(size=0.2) + theme(text=element_text(size=8), plot.margin=unit(c(0.5,0.5,0.5,0.5),"mm"), axis.title.x=element_text(vjust = 2), axis.title.y=element_blank(), axis.text.y = element_blank()) +
  geom_line(data = fitdata_edu_lv2, aes(y=fit)) +
  geom_ribbon(data = fitdata_edu_lv2, aes(y=fit, ymin=lower, ymax=upper), alpha=0.4) +
  xlab("LV2") + 
  scale_x_continuous(breaks = seq(-6, 9, by = 3))

grid.arrange(edu_lv1,edu_lv2,nrow=1)

dev.new(width = 2.7, height = 1.6, unit="in", noRStudioGD = T)
p<-grid.arrange(edu_lv1,edu_lv2, nrow=1,  widths = c(1.6, 1.4))
p
ggsave(p,file="figures/educ_lv1lv2.png", dpi=300)
dev.off()

  # Now running models for each cog test #
# I run an lm covarying for age, sex, edu #
# 6 total tests, so using p < 0.0083 (0.05/6, bonferroni) as threshold #




  #### animals ####
ps_vector<-vector()
vs<-lm(inormal(sqrt(animals))~vsc_LV_1 + vsc_LV_2 + age_c + OX.SEX + OX.EDUC_ftht, data=pls_demo_cog_ph12)
summary(vs)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   3.757206   0.554300   6.778 4.64e-11 ***
#  vsc_LV_1     -0.241484   0.015959 -15.131  < 2e-16 ***
#  vsc_LV_2     -0.138991   0.028706  -4.842 1.87e-06 ***
#  age_c        -0.054052   0.007472  -7.234 2.60e-12 ***
#  OX.SEXMale   -0.192771   0.092480  -2.084   0.0378 *  
#  OX.EDUC_ftht  0.013123   0.010974   1.196   0.2325  
confint(vs)
#2.5 %      97.5 %
#  (Intercept)   2.667336152  4.84707532
#vsc_LV_1     -0.272863623 -0.21010418
#vsc_LV_2     -0.195433793 -0.08254897
#age_c        -0.068744423 -0.03936042
#OX.SEXMale   -0.374605928 -0.01093568
#OX.EDUC_ftht -0.008454281  0.03470084

ps_vector<-summary(vs)$coefficients[2,4]
ps_vector<-append(ps_vector, summary(vs)$coefficients[3,4])


sem_flu_plots<-effect_plot(pls_demo_cog_ph12,vs,pls_demo_cog_ph12$animals,"LV1","LV2","Semantic Fluency")

dev.new(width = 3.4, height = 1.55, unit="in", noRStudioGD = T) 
p<-grid.arrange(sem_flu_plots$p_lv1, sem_flu_plots$p_lv2, nrow=1,  widths = c(1.6, 1.4))
p
ggsave(p,file="figures/semfluency_lv1lv2.png", dpi=300)
dev.off()

#### words ####
vs<-lm(inormal(sqrt(swords))~vsc_LV_1 + vsc_LV_2 + age_c + OX.SEX + OX.EDUC_ftht, data=pls_demo_cog_ph12)
summary(vs)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   4.368988   0.612991   7.127 5.16e-12 ***
#  vsc_LV_1     -0.205862   0.017649 -11.664  < 2e-16 ***
#  vsc_LV_2     -0.152376   0.031746  -4.800 2.28e-06 ***
#  age_c        -0.055156   0.008263  -6.675 8.77e-11 ***
#  OX.SEXMale   -0.202652   0.102272  -1.981   0.0483 *  
#  OX.EDUC_ftht -0.019835   0.012136  -1.634   0.1030   
confint(vs)
#2.5 %       97.5 %
#  (Intercept)   3.16371760  5.574257431
#vsc_LV_1     -0.24056432 -0.171159627
#vsc_LV_2     -0.21479523 -0.089957651
#age_c        -0.07140364 -0.038908331
#OX.SEXMale   -0.40374037 -0.001563062
#OX.EDUC_ftht -0.04369710  0.004027483

ps_vector<-append(ps_vector, summary(vs)$coefficients[2,4])
ps_vector<-append(ps_vector, summary(vs)$coefficients[3,4])


lex_flu_plots<-effect_plot(pls_demo_cog_ph12,vs,pls_demo_cog_ph12$swords,"LV1","LV2","Lexical Fluency")

dev.new(width = 3.4, height = 1.55, unit="in", noRStudioGD = T) 
p<-grid.arrange(lex_flu_plots$p_lv1, lex_flu_plots$p_lv2, nrow=1,  widths = c(1.6, 1.4))
p
ggsave(p,file="figures/lexfluency_lv1lv2.png", dpi=300)
dev.off()

#### mem ####
vs<-lm(inormal(sqrt(mem))~vsc_LV_1 + vsc_LV_2 + age_c + OX.SEX + OX.EDUC_ftht, data=pls_demo_cog_ph12)
summary(vs)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   4.187804   0.638608   6.558 1.79e-10 ***
#  vsc_LV_1     -0.164088   0.018423  -8.907  < 2e-16 ***
#  vsc_LV_2     -0.097625   0.033028  -2.956  0.00331 ** 
#  age_c        -0.061014   0.008602  -7.093 6.49e-12 ***
#  OX.SEXMale   -0.224027   0.106459  -2.104  0.03601 *  
#  OX.EDUC_ftht  0.018879   0.012649   1.493  0.13639 
confint(vs)
#2.5 %      97.5 %
#  (Intercept)   2.932146516  5.44346227
#vsc_LV_1     -0.200311200 -0.12786382
#vsc_LV_2     -0.162566590 -0.03268271
#age_c        -0.077927625 -0.04409981
#OX.SEXMale   -0.433351052 -0.01470285
#OX.EDUC_ftht -0.005991519  0.04374890

ps_vector<-append(ps_vector, summary(vs)$coefficients[2,4])
ps_vector<-append(ps_vector, summary(vs)$coefficients[3,4])


mem_plots<-effect_plot(pls_demo_cog_ph12,vs,pls_demo_cog_ph12$mem,"LV1","LV2","Memory")

dev.new(width = 3.4, height = 1.55, unit="in", noRStudioGD = T) 
p<-grid.arrange(mem_plots$p_lv1, mem_plots$p_lv2, nrow=1,  widths = c(1.6, 1.4))
p
ggsave(p,file="figures/mem_lv1lv2.png", dpi=300)
dev.off()

#### ah4 ####
vs<-lm(inormal(sqrt(ah4))~vsc_LV_1 + vsc_LV_2 + age_c + OX.SEX + OX.EDUC_ftht, data=pls_demo_cog_ph12)
summary(vs)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   3.705984   0.394394   9.397  < 2e-16 ***
#  vsc_LV_1     -0.326870   0.011331 -28.848  < 2e-16 ***
#  vsc_LV_2      0.101824   0.020428   4.984 9.44e-07 ***
#  age_c        -0.052494   0.005319  -9.869  < 2e-16 ***
#  OX.SEXMale    0.081155   0.065786   1.234    0.218    
#OX.EDUC_ftht -0.004448   0.007781  -0.572    0.568 
confint(vs)
#2.5 %      97.5 %
#  (Intercept)   2.93052961  4.48143751
#vsc_LV_1     -0.34914937 -0.30459158
#vsc_LV_2      0.06165804  0.14199075
#age_c        -0.06295237 -0.04203620
#OX.SEXMale   -0.04819164  0.21050244
#OX.EDUC_ftht -0.01974754  0.01085179

ps_vector<-append(ps_vector, summary(vs)$coefficients[2,4])
ps_vector<-append(ps_vector, summary(vs)$coefficients[3,4])

ah4_plots<-effect_plot(pls_demo_cog_ph12,vs,pls_demo_cog_ph12$ah4,"LV1","LV2","Inductive Reasoning")

dev.new(width = 3.4, height = 1.55, unit="in", noRStudioGD = T) 
p<-grid.arrange(ah4_plots$p_lv1, ah4_plots$p_lv2, nrow=1,  widths = c(1.6, 1.4))
p
ggsave(p,file="figures/ah4_lv1lv2.png", dpi=300)
dev.off()



#### ahnum ####
vs<-lm(inormal(sqrt(ahnum))~vsc_LV_1 + vsc_LV_2 + age_c + OX.SEX + OX.EDUC_ftht, data=pls_demo_cog_ph12)
summary(vs)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   3.321374   0.410521   8.091 7.98e-15 ***
#  vsc_LV_1     -0.321037   0.011794 -27.220  < 2e-16 ***
#  vsc_LV_2      0.100097   0.021264   4.707 3.52e-06 ***
#  age_c        -0.047126   0.005536  -8.512 3.96e-16 ***
#  OX.SEXMale    0.098089   0.068476   1.432    0.153    
#OX.EDUC_ftht -0.005053   0.008100  -0.624    0.533  
confint(vs)
#2.5 %      97.5 %
#  (Intercept)   2.51421053  4.12853651
#vsc_LV_1     -0.34422711 -0.29784732
#vsc_LV_2      0.05828796  0.14190555
#age_c        -0.05801171 -0.03624027
#OX.SEXMale   -0.03654675  0.23272558
#OX.EDUC_ftht -0.02097792  0.01087265

ps_vector<-append(ps_vector, summary(vs)$coefficients[2,4])
ps_vector<-append(ps_vector, summary(vs)$coefficients[3,4])


ahnum_plots<-effect_plot(pls_demo_cog_ph12,vs,pls_demo_cog_ph12$ahnum,"LV1","LV2","Mathematical Reasoning")

dev.new(width = 3.4, height = 1.55, unit="in", noRStudioGD = T) 
p<-grid.arrange(ahnum_plots$p_lv1, ahnum_plots$p_lv2, nrow=1,  widths = c(1.6, 1.4))
print(p)
ggsave(plot = p,filename ="figures/ahnum_lv1lv2.png", dpi=300)
dev.off()

#### ahverb ####
vs<-lm(inormal(sqrt(ahverb))~vsc_LV_1 + vsc_LV_2 + age_c + OX.SEX + OX.EDUC_ftht, data=pls_demo_cog_ph12)
summary(vs)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   3.919535   0.434709   9.016  < 2e-16 ***
#  vsc_LV_1     -0.307339   0.012489 -24.608  < 2e-16 ***
#  vsc_LV_2      0.095778   0.022517   4.254 2.65e-05 ***
#  age_c        -0.055772   0.005863  -9.513  < 2e-16 ***
#  OX.SEXMale    0.049783   0.072510   0.687    0.493    
#OX.EDUC_ftht -0.001795   0.008577  -0.209    0.834     
confint(vs)
#2.5 %      97.5 %
#  (Intercept)   3.06481287  4.77425782
#vsc_LV_1     -0.33189535 -0.28278277
#vsc_LV_2      0.05150605  0.14005054
#age_c        -0.06729908 -0.04424482
#OX.SEXMale   -0.09278580  0.19235254
#OX.EDUC_ftht -0.01865892  0.01506834

ps_vector<-append(ps_vector, summary(vs)$coefficients[2,4])
ps_vector<-append(ps_vector, summary(vs)$coefficients[3,4])


ahverb_plots<-effect_plot(pls_demo_cog_ph12,vs,pls_demo_cog_ph12$ahverb,"LV1","LV2","Verbal Reasoning")

dev.new(width = 3.4, height = 1.55, unit="in", noRStudioGD = T) 
p<-grid.arrange(ahverb_plots$p_lv1, ahverb_plots$p_lv2, nrow=1,  widths = c(1.6, 1.4))
p
ggsave(p,file="figures/ahverb_lv1lv2.png", dpi=300)
dev.off()

ps_vector
#[1] 7.761563e-41 1.873156e-06 4.329740e-27 2.283819e-06 2.221665e-17 3.313864e-03 5.864803e-98 9.441762e-07 1.813568e-91 3.515450e-06 8.471469e-81 2.648081e-05

