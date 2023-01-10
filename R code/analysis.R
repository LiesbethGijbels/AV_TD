# calculate for result summary
d$cond_SNR <- paste(d$condition,d$SNR)
describeBy(d$correct, group=d$cond_SNR)

## read excel files
library(ppcor)
library(mirt)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(corrplot)
library(RColorBrewer)
library(stringr)
library(psych)
library(BayesFactor)
library(ggpubr)
library(ggh4x)


s=AV_V2_result_summary
d=AV_V2_alldata_typical

d$SNR = as.factor(d$SNR)
d$condition = as.factor(d$condition)
d$goal = as.factor(d$goal)
d$SNR_cond = as.factor(d$SNR_cond)
#describe data
describe(s)
table(s$Gender)
hist(s$Age)

# percentage correct per modality per SNR
boxplot(s$`A-5`,s$`AV-5`,s$`A-8`,s$`AV-8`,s$`A-11`,s$`AV-11`,s$V,ylab="percentage correct", xlab="modality and SNR"  ,names=c("A,SNR-5","AV,SNR-5","A,SNR-8","AV,SNR -8","A,SNR-11","AV,SNR -11","V-only"),main= "Percentage correct per modality per SNR",col=c("cadetblue1", "palegreen", "dodgerblue","#009966","#0033CC","darkgreen","orange"))points(6,mean(s$`AV-11`),pch=19, col="gray")
points(5,mean(s$`A-11`),pch=19, col="gray")
points(4,mean(s$`AV-8`),pch=19, col="gray")
points(3,mean(s$`A-8`),pch=19, col="gray")
points(2,mean(s$`AV-5`),pch=19, col="gray")
points(1,mean(s$`A-5`),pch=19, col="gray")
points(7,mean(s$V),pch=19, col ="gray")

###Performance per modality by age summary sheet A.V.AV
f=AV_V2_result_summary
f$score=f$score*100
ggplot(f, aes(y=score, x=Age,col =Modality)) + xlab("Age (years)")+ ylab("Performance (% correct)")+ geom_point() + geom_smooth(method=lm, fullrange=FALSE,se=TRUE,aes(fill=Modality)) +scale_color_manual(values=c("blue2", "springgreen4","orange"),labels = c("audio-only", "AV", "visual-only")) + scale_fill_manual(values=c("blue2", "springgreen4","orange"),labels = c("audio-only", "AV", "visual-only"))+scale_y_continuous(breaks=seq(0,100,by=20))+theme(text = element_text(size=15,color="black",face="bold"))
##more detail
ggplot(f, aes(y=score, x=Age,col =Modality)) + xlab("Age (years)")+ ylab("Performance (% correct)")+ geom_point(size=0.5) + geom_smooth(method=lm, fullrange=FALSE,se=TRUE,aes(fill=Modality)) +scale_color_manual(values=c("blue2", "springgreen4","orange"),labels = c("audio-only", "AV", "visual-only")) + scale_fill_manual(values=c("blue2", "springgreen4","orange"),labels = c("audio-only", "AV", "visual-only"))+scale_y_continuous(breaks=seq(0,100,by=20))+theme(text = element_text(size=13,color="black",face="bold"),legend.position="top")+ stat_cor(method = "pearson",digits=2,p.accuracy=0.001,size=5,  label.x = 11,label.y.npc = "bottom")
ggsave("performance.png",dpi=300)
m3=(lm(f$score[f$Modality=="AV"]~f$Age[f$Modality=="AV"]))
summary(m3)
m4=(lm(f$score[f$Modality=="A"]~f$Age[f$Modality=="A"]))
summary(m4)
m5=(lm(f$score[f$Modality=="V"]~f$Age[f$Modality=="V"]))
summary(m5)

cor.test(f$Age[f$Modality=="V"],f$score[f$Modality=="V"])
cor.test(f$Age[f$Modality=="A"],f$score[f$Modality=="A"])
cor.test(f$Age[f$Modality=="AV"],f$score[f$Modality=="AV"])


### plot A vs AV by SNR
##AV_V2_result_summary --> sheet A_Cat
z=AV_V2_result_summary
z$SNR = as.factor(z$SNR)
## sheet 1
f=AV_V2_result_summary
f$TypeA = as.factor(f$TypeA)

#a-cat pairwise test category and SNR vs AV Gain
pairwise.wilcox.test(z$gainpersnr, z$`A+SNR`, p.adjust.method = "bonferroni", paired = FALSE) 


## A vs AV by SNR (added jitter to not let dots completely overlap on grid)
set.seed(3)
ggplot(z, aes(y=AV, x=`audio-only`,col =SNR)) + xlab("% correct A-only")+ ylab("% correct audiovisual")+ geom_point(size=2, position=position_jitter(h=0.025,w=0.025)) + geom_smooth(method=loess, fullrange=FALSE,se=TRUE,aes(fill=SNR)) +scale_color_manual(values=c("#6A6599FF", "#F8766D","#00B81F")) +geom_abline(slope=1)+ scale_fill_manual(values=c("#6A6599FF", "#F8766D","#00B81F"))#Leave SE = false out to get CI
#more detail in plot
z$AV=z$AV*100
z$`audio-only`=z$`audio-only`*100
ggplot(z, aes(y=AV, x=`audio-only`,col =SNR)) + xlab("Auditory Performance (% correct)")+ ylab("AV Performance (% correct)")+ geom_point(size=0.75, position=position_jitter(h=2.5,w=2.5)) + geom_smooth(method=loess,size=1.15, fullrange=FALSE,se=TRUE,aes(fill=SNR)) +scale_color_manual(values=c("#6A6599FF", "#F8766D","#00B81F"),labels=c("-5 dB", "-8 dB", "-11 dB")) + scale_fill_manual(values=c("#6A6599FF", "#F8766D","#00B81F"),labels=c("-5 dB", "-8 dB", "-11 dB"))+theme(text = element_text(size=13,color="black",face="bold"),legend.position="top")+scale_x_continuous(breaks=seq(0,100,by=20))+geom_abline(slope=1,size=0.75)+geom_abline(slope=1,intercept=7.157348333,color="#3399CC",size=0.75)+geom_abline(slope=1,intercept=32.91306833,color="#0066CC",linetype="dotdash",size=0.75)+geom_abline(slope=1,intercept=-18.59837167,color="#0066CC",linetype="dotdash",size=0.75)+geom_abline(slope=1,intercept=-5.720511667,color="cornflowerblue",linetype="dashed",size=0.75)+geom_abline(slope=1,intercept=20.03520833,color="cornflowerblue",linetype="dashed",size=0.75)
ggsave("Fig2_left.png",dpi=300)
##dotted lines are calculated from the mean AV gain + 1 or 2 STD - across SNRs

## boxplots by AV gain A-only performance (summary sheet A_cat)
boxplot(z$`Avg AV Gain`[z$A_cat=="low"],z$`Avg AV Gain`[z$A_cat=="medium"],z$`Avg AV Gain`[z$A_cat=="high"],ylab = "average AV gain", xlab ="auditory only performance", names = c("low","medium","high"),col=c("orchid4","lightpink","salmon2"))
points(1,mean(z$`Avg AV Gain`[z$A_cat=="low"]),pch=19, col="gray")
points(2,mean(z$`Avg AV Gain`[z$A_cat=="medium"]),pch=19, col="gray")
points(3,mean(z$`Avg AV Gain`[z$A_cat=="high"]),pch=19, col="gray")
## by age
#linear
z$A_cat=as.factor(z$A_cat)
z$SNR=as.factor(z$SNR)
ggplot(z, aes(y=`Avg AV Gain`, x=Age,col =A_cat)) + xlab("Age (years)")+ ylab("Average AV Gain")+ geom_point(size=2, position=position_jitter(h=0.025,w=0.025)) + geom_smooth(method=lm, fullrange=FALSE,se=TRUE,aes(fill=A_cat))+ scale_fill_manual(values = c("orchid4","lightpink","salmon2")) +scale_color_manual(values = c("orchid4","lightpink","salmon2"))
#boxplots
boxplot(z$`Avg AV Gain`[z$`A+SNR`=="low5"],z$`Avg AV Gain`[z$`A+SNR`=="low8"],z$`Avg AV Gain`[z$`A+SNR`=="low11"],z$`Avg AV Gain`[z$`A+SNR`=="medium5"],z$`Avg AV Gain`[z$`A+SNR`=="medium8"],z$`Avg AV Gain`[z$`A+SNR`=="medium11"],z$`Avg AV Gain`[z$`A+SNR`=="high5"],z$`Avg AV Gain`[z$`A+SNR`=="high8"],z$`Avg AV Gain`[z$`A+SNR`=="high11"],ylab = "average AV gain", xlab ="auditory only performance per SNR ", names = c("low,SNR -5","low,SNR -8","low,SNR -11","medium, SNR -5","medium, SNR -8","medium, SNR -11","high, SNR-5","high, SNR-8","high, SNR-11"),col=c("#6A6599FF", "#F8766D","#00B81F"))

#boxplots AV Gain normalized and average
#import summary sheet normvsavg
r=AV_V2_result_summary
r$type_gain <- factor(r$type_gain ,levels=c("normalized", "Avg"))
ggplot(z, aes(y=AV, x=`audio-only`,col =SNR)) + xlab("Performance audio-only (% correct)")+ ylab("Performance AV (% correct)")+ geom_point(size=0.5, position=position_jitter(h=2.5,w=2.5)) + geom_smooth(method=loess, fullrange=FALSE,se=TRUE,aes(fill=SNR)) +scale_color_manual(values=c("#6A6599FF", "#F8766D","#00B81F"),labels=c("-5 dB", "-8 dB", "-11 dB")) +geom_abline(slope=1)+ scale_fill_manual(values=c("#6A6599FF", "#F8766D","#00B81F"),labels=c("-5 dB", "-8 dB", "-11 dB"))+theme(text = element_text(size=13,color="black",face="bold"),legend.position="top")+scale_x_continuous(breaks=seq(0,100,by=20))))

##GGPLOT BOXPLOTS PER SNR
z$A_cat <- factor(z$A_cat , levels=c("low", "medium", "high"))
z$SNR <- factor(z$SNR , levels=c("5", "8", "11"))
#AV -A
ggplot(z, aes(x=A_cat, y=gainpersnr)) + xlab("Performance audio-only (categorized per SNR)") +ylab("Normalized AV Gain [(AV-A)/(1-A)]")+  geom_boxplot()+facet_wrap(~SNR)+ geom_boxplot(fill=c("orchid4","lightpink","salmon2","orchid4","lightpink","salmon2","orchid4","lightpink","salmon2"),alpha=0.7)+theme(legend.position = "none",strip.background = element_rect(colour="black", fill=("white")))    + stat_summary(fun=mean, geom="point", shape=20, size=5, color="dark grey") 
# Normalized AV Gain
ggplot(z, aes(x=A_cat, y=normgainpersnr)) + xlab("Performance audio-only (categorized per SNR)") +ylab("Normalized AV Gain [(AV-A)/(1-A)]")+  geom_boxplot()+facet_wrap(~SNR)+ geom_boxplot(fill=c("orchid4","lightpink","salmon2","orchid4","lightpink","salmon2","orchid4","lightpink","salmon2"),alpha=0.7)+theme(legend.position = "none",strip.background = element_rect(colour="black", fill=("white")))    + stat_summary(fun=mean, geom="point", shape=20, size=5, color="dark grey") 

##Continuous plot
# AV-A
ggplot(z,aes(x=A_cat_z,y=gainpersnr.scaled, col=SNR))+geom_point(size=1, position=position_jitter(h=0.025,w=0.025))+geom_smooth(method=lm)+scale_color_manual(values=c("#6A6599FF", "#F8766D","#00B81F")) + scale_fill_manual(values=c("#6A6599FF", "#F8766D","#00B81F"))+theme(strip.background = element_rect(colour="black",fill="white"))
# normgain
ggplot(z,aes(x=A_cat_z,y=normgainpersnr, col=SNR))+geom_point(size=1, position=position_jitter(h=0.025,w=0.025))+geom_smooth(method=lm)+scale_color_manual(values=c("#6A6599FF", "#F8766D","#00B81F")) + scale_fill_manual(values=c("#6A6599FF", "#F8766D","#00B81F"))+theme(strip.background = element_rect(colour="black",fill="white"))
#moredetail
ggplot(z,aes(x=z$A_cat_z,y=normgainpersnr, col=SNR))+ylab("Normalized AV Gain, [(AV-A)/(1-A)]")+xlab("Auditory performance (Z-scores)")+geom_point(size=0.5, position=position_jitter(h=0.25,w=0.25))+ylim(-1,1)+geom_smooth(method="lm",fullrange=FALSE,se=TRUE,aes(fill=SNR))+scale_color_manual(values=c("#6A6599FF", "#F8766D","#00B81F"),labels = c("-5 dB", "-8 dB", "-11 dB")) + scale_fill_manual(values=c("#6A6599FF", "#F8766D","#00B81F"),labels = c("-5 dB", "-8 dB", "-11 dB"))+theme(text = element_text(size=13,color="black",face="bold"),legend.position = "top")+ stat_cor(method = "pearson",digits=2,p.accuracy=0.001,size=5,  label.x.npc = "left",label.y.npc = "bottom")

m=(lm(z$`Avg AV Gain`~z$SNR*z$A_cat*z$Age))
summary(m)
anova(m)

#density functions
#auditory
z$`audio-only`=z$`audio-only`*100
ggplot(z, aes(x = z$`audio-only`)) + scale_x_continuous(breaks = seq(0, 100, 25),limits=c(0,105)) +  scale_y_continuous(breaks = seq(0, 0.05, 0.01),limits=c(0,0.05)) + geom_histogram(aes(y=..density..),fill = "springgreen4", col = "white", binwidth =5) + facet_wrap("SNR")+stat_theodensity(colour="black")
#AV
z$AV=z$AV*100
ggplot(z, aes(x = z$AV)) + scale_x_continuous(breaks = seq(0, 100, 25),limits=c(0,105)) + scale_y_continuous(breaks = seq(0, 0.05, 0.01),limits=c(0,0.05))+ geom_histogram(aes(y=..density..),fill = "blue2", col = "white", binwidth =5) + facet_wrap("SNR")+stat_function(fun = dnorm, args = list(mean = mean(z$AV),sd = sd(z$AV)),col = "black",size = 0.5)
#visual
z$V=z$V*100
ggplot(z, aes(x = z$V)) + scale_x_continuous(breaks = seq(0, 100, 25),limits=c(0,105)) + scale_y_continuous(breaks = seq(0, 0.05, 0.01),limits=c(0,0.05))+ geom_histogram(aes(y=..density..),fill = "orange", col = "white", binwidth =5)+stat_theodensity(colour="black")
#gain
z$gainpersnr=z$gainpersnr*100
ggplot(z, aes(x = z$gainpersnr)) + scale_x_continuous(breaks = seq(-50, 100, 25),limits=c(-50,50)) +  scale_y_continuous(breaks = seq(0, 0.05, 0.01),limits=c(0,0.05))+ geom_histogram(aes(y=..density..),fill = "darkcyan", col = "white", binwidth =5) + facet_wrap("SNR")+stat_function(fun = dnorm, args = list(mean = mean(z$gainpersnr),sd = sd(z$gainpersnr)),col = "black",size = 0.5)

## in function of V
boxplot(z$V[z$A_cat=="low"],z$V[z$A_cat=="medium"],z$V[z$A_cat=="high"],ylab = "Performance visual-only ", xlab ="Performance audio-only (categorized per individual per SNR) ", names = c("low","medium","high"),col=c("orchid4","lightpink","salmon2"))
points(1,mean(z$V[z$A_cat=="low"]),pch=19, col="gray")
points(2,mean(z$V[z$A_cat=="medium"]),pch=19, col="gray")
points(3,mean(z$V[z$A_cat=="high"]),pch=19, col="gray")

z = z %>% mutate(A_cat = relevel(A_cat, "medium"))
x=lm(z$`Avg AV Gain`~z$V*z$A_cat)
summary(x)
anova(x)

#by A-only category
z$A_cat <- factor(z$A_cat , levels=c("low", "medium", "high"))
#plot AV gain vs V
ggplot(z, aes(y=gainpersnr, x=V,col=A_cat)) + xlab("visual-only")+ ylab("AV Gain, per SNR")+geom_point(size=1, position=position_jitter(h=0.025,w=0.025))+ geom_smooth(method=lm, fullrange=FALSE,se=TRUE,aes(fill=A_cat))+ scale_fill_manual(values = c("orchid4","lightpink","salmon2")) +scale_color_manual(values = c("orchid4","lightpink","salmon2"))#plot AV gain vs Age
#more detail
z$V=z$V*100
ggplot(z, aes(y=normgainpersnr, x=V,col=A_cat)) + xlab("Performance visual-only (% correct)")+ ylab("Normalized AV Gain, per SNR")+geom_point(size=0.5, position=position_jitter(h=2.5,w=2.5))+ geom_smooth(method=lm, fullrange=FALSE,se=TRUE,aes(fill=A_cat))+ scale_fill_manual(values = c("orchid4","lightpink","salmon2"),name="audio-only performance",labels=c("Low","Medium","High")) +scale_color_manual(values = c("orchid4","lightpink","salmon2"),name="audio-only performance",labels=c("Low","Medium","High"))+ylim(-3,3)+xlim(0,80)+theme(text = element_text(size=13,color="black",face="bold"),legend.position="top",legend.title=element_text(size=11))
ggsave("visual.png",dpi=300)
#plot AV gain vs Age
ggplot(z, aes(y=gainpersnr, x=Age,col=A_cat)) + xlab("Age(years)")+ ylab("AV Gain, per SNR")+geom_point(size=1, position=position_jitter(h=0.025,w=0.025))+ geom_smooth(method=lm, fullrange=FALSE,se=TRUE,aes(fill=A_cat))+ scale_fill_manual(values = c("orchid4","lightpink","salmon2")) +scale_color_manual(values = c("orchid4","lightpink","salmon2"))
#moredetail
ggplot(z, aes(y=normgainpersnr, x=Age,col=A_cat)) + xlab("Age (years))")+ ylab("Normalized AV Gain, per SNR")+geom_point(size=0.5, position=position_jitter(h=2.5,w=2.5))+ geom_smooth(method=lm, fullrange=FALSE,se=TRUE,aes(fill=A_cat))+ scale_fill_manual(values = c("orchid4","lightpink","salmon2"),name="audio-only performance",labels=c("Low","Medium","High")) +scale_color_manual(values = c("orchid4","lightpink","salmon2"),name="audio-only performance",labels=c("Low","Medium","High"))+ylim(-3,3)+xlim(4,15)+theme(text = element_text(size=13,color="black",face="bold"),legend.position="top",legend.title=element_text(size=11))
ggsave("age.png",dpi=300)
##model3_final
z=AV_V2_result_summary ##(sheet A_cat)
#Make SNR 8 reference
z$SNR=as.factor(z$SNR)
z = z %>% mutate(SNR = relevel(SNR, "8"))
#normalize variables --> z scores
#A-only individual performance is z$A_cat_z
z$gainpersnr.scaled=scale(z$gainpersnr,center=TRUE, scale = TRUE)
z$V.scaled = scale(z$V, center= TRUE, scale=TRUE)
z$Age.scaled = scale(z$Age, center= TRUE, scale=TRUE)
#linear mixed model with z scores and subject
m3=(lmer(z$normgainpersnr~z$A_cat_z+z$SNR+z$Age.scaled+z$V.scaled+(1|z$subject)))
summary(m3)
anova(m3)
#unstandardized version
m3b=(lmer(z$gainpersnr~z$A_cat_z*z$SNR*z$Age+z$V+(1|z$subject)))
summary(m3b)


#Check collinearity:
library(GGally)
library(mctest)

X <- subset(z,select=c(A_cat_z,SNR,Age,V))
ggpairs(X)
mc_test <-lm(gainpersnr.scaled~A_cat_z+SNR+Age.scaled+V.scaled, data = as.data.frame(z))
imcdiag(mc_test)
omcdiag(mc_test)

#partial cor test - m = AvsA2_cat
library(ppcor)
pcor.test(m$A_diff,m$Age,m$Cat_z)
#delete outliers
Q1 <- quantile(m$normgain8, .25)
Q3 <- quantile(m$normgain8, .75)
IQR <- IQR(m$normgain8)

#only keep rows in dataframe that have values within 1.5*IQR of Q1 and Q3
no_outliers <- subset(m, m$normgain8> (Q1 - 1.5*IQR) & m$normgain8< (Q3 + 1.5*IQR))

ggplot(no_outliers,aes(x=normgain8,y=A_diff_norm,col=Cat_z))+geom_point()+geom_smooth(method=lm)
cor.test(no_outliers$normgain8,no_outliers$A_diff_norm)

##also do outliers for A_diff_norm
##plot
ggplot(no_outliers2,aes(y=normgain8,x=A_diff_norm,col=Cat_z))+geom_point()+geom_smooth(method=lm)+ylab("Normalized AV Gain")+xlab("A stillface- A mismatch (normalized)")+scale_color_gradientn(colours = rainbow(4),name="Z scores A only performance")+ stat_cor(method = "pearson",digits=2,p.accuracy=0.001,size=5,  label.x.npc = "middle",label.y.npc = "bottom")+theme(text = element_text(size=13,color="black",face="bold"),legend.position="top",legend.title=element_text(size=11))
ggsave("adiff.png",dpi=300)
### AGE
## A-only scores by age + cor.test
ggplot(s, aes(y=Avg_A, x=Age)) + xlab("Age (years)")+ ylab("% correct A-only")+ geom_point() + geom_smooth(method=lm, fullrange=FALSE,se=TRUE) #Leave SE = false out to get CI
cor.test(s$Age,s$Avg_A)
## AV scores by age
ggplot(s, aes(y=Avg_AV, x=Age)) + xlab("Age (years)")+ ylab("% correct AV ")+ geom_point() + geom_smooth(method=lm, fullrange=FALSE,se=TRUE) #Leave SE = false out to get CI
cor.test(s$Age,s$Avg_AV)
## AV gain by age
ggplot(s, aes(y=`Avg AV Gain`, x=Age)) + xlab("Age (years)")+ ylab("Average AV gain ")+ geom_point() + geom_smooth(method=lm, fullrange=FALSE,se=TRUE) #Leave SE = false out to get CI
cor.test(s$Age,s$`Avg AV Gain`)


## difference in A only tasks by age
ggplot(s, aes(y=Acompare, x=Age)) + xlab("Age (years)")+ ylab( "Difference between 2 A-only conditions")+ geom_point() + geom_smooth(method=lm, fullrange=FALSE,se=TRUE) 
cor.test(s$Age,s$Acompare)
## AV gain vs V by age
ggplot(s, aes(y=`Avg AV Gain`, x=V, colour=Age)) + xlab("Visual only scores")+ ylab("Average AV gain ")+ geom_point() + geom_smooth(method=lm, fullrange=FALSE,se=TRUE)+scale_color_gradientn(colours = rainbow(4))
cor.test(s$V,s$`Avg AV Gain`)
cor.test(s$V,s$Age)

### GENDER
#AV gain by GENDER
boxplot(s$`Avg AV Gain`[s$Gender=="M"],s$`Avg AV Gain`[s$Gender=="F"],names=c("AV gain males","AV gain females"),main= "AV gain for male and female children",col=c( "red","yellow" ))
points(1,mean(s$`Avg AV Gain`[s$Gender=="M"]),pch=19, col="gray")
points(2,mean(s$`Avg AV Gain`[s$Gender=="F"]),pch=19, col="gray")
#t-test
t.test(s$`Avg AV Gain`[s$Gender=="M"],s$`Avg AV Gain`[s$Gender=="F"])

#plots A scores for still and video
boxplot(s$`A-8`,s$`A2-8`,names=c("A,SNR-8 still","A,SNR-8 video"),main= "Percentage correct for 2 A -8dB conditions",col=c( "#0033CC","turquoise2" ))
points(1,mean(s$`A-8`),pch=19, col="gray")
points(2,mean(s$`A2-8`),pch=19, col="gray")
# t-test
t.test(s$`A-8`,s$`A2-8`)

### slopes per age A vs A2 - load sheet AvsA2 save in p
ggplot(p, aes(y=score, x=Age,col =TypeA)) + xlab("Age")+ ylab("% correct")+ geom_point() + geom_smooth(method=lm, fullrange=FALSE,se=TRUE,aes(fill=TypeA)) +scale_color_manual(values=c("dodgerblue", "#006666")) +geom_abline(slope=1)+ scale_fill_manual(values=c("dodgerblue", "#006666"))
m1 = lm(s$`A-8`~s$Age)
m2 = lm(s$`A2-8`~s$Age)
anova(m1,m2)
describe(s)

#load mismatch_stillface in r
r$score_A8=r$score_A8*100
ggplot(r, aes(y=score_A8, x=Age,col =TypeA)) + xlab("Age (years)")+ ylab("Performance (% correct)")+ geom_point(size=0.5) + geom_smooth(method=lm, fullrange=FALSE,se=TRUE,aes(fill=TypeA)) +scale_color_manual(values=c("dodgerblue", "#006666"),name="Stimulus Type",labels=c("Still face","Mismatch video"))+ scale_fill_manual(values=c("dodgerblue", "#006666"),name="Stimulus Type",labels=c("Still face","Mismatch video"))+theme(text = element_text(size=13,color="black",face="bold"),legend.position="top",legend.title=element_text(size=11))+ stat_cor(method = "pearson",digits=2,p.accuracy=0.001,size=5,  label.x = 11,label.y.npc = "bottom")
ggsave("mismatch.png",dpi=300)
# with z-scores
s$Age.scaled = scale(s$Age, center= TRUE, scale=TRUE)
s$`A-8.scaled` = scale(s$`A-8`, center= TRUE, scale=TRUE)
s$`A2-8.scaled` = scale(s$`A2-8`, center= TRUE, scale=TRUE)
m3 = lm(s$`A-8.scaled`~s$Age.scaled)
m4 = lm(s$`A2-8.scaled`~s$Age.scaled)
summary(m3)
summary(m4)
anova(m3,m4)

#plots A scores for still and video by gender
boxplot(s$`A-8`[s$Gender=="M"],s$`A2-8`[s$Gender=="M"],s$`A-8`[s$Gender=="F"],s$`A2-8`[s$Gender=="F"],names=c("A-8 still M","A-8 video M ","A-8 still F","A-8 video F "),main= "Percentage correct for 2 A -8dB conditions by gender",col=c( "dodgerblue","#006666" ))
points(1,mean(s$`A-8`[s$Gender=="M"]),pch=19, col="gray")
points(2,mean(s$`A2-8`[s$Gender=="M"]),pch=19, col="gray")
points(3,mean(s$`A-8`[s$Gender=="F"]),pch=19, col="gray")
points(4,mean(s$`A2-8`[s$Gender=="F"]),pch=19, col="gray")

mt=lm(p$score~p$TypeA*p$Gender)
summary(mt)
anova(mt)


# percentage difference for still and video by gender
boxplot(s$Acompare[s$Gender=="M"],s$Acompare[s$Gender=="F"],names=c("Male","Female"),main= "Percentage difference for 2 A -8dB conditions by gender",col=c( "#0033CC","turquoise2" ))
points(1,mean(s$Acompare[s$Gender=="M"]),pch=19, col="gray")
points(2,mean(s$Acompare[s$Gender=="F"]),pch=19, col="gray")
#t-test
t.test(s$Acompare[s$Gender=="M"],s$Acompare[s$Gender=="F"])


### PHONOLOGICAL AWARENESS

#CTOPP_WJ import - sheet typical -->  s
s <- na.omit(s)
#norm gain
ggplot(s, aes(x=CTOPP_PA_raw, y=Norm_Gain, colour=Age)) + geom_point() +xlab("Phonological awareness scores")+ylab("Normed AV gain") + geom_smooth(method='lm') +scale_color_gradientn(colours = rainbow(4))
cor.test(s$CTOPP_PA_raw,s$Norm_Gain)

# average gain
ggplot(s, aes(x=CTOPP_PA_raw, y=`Avg AV Gain`, colour=Age)) + geom_point() +xlab("Phonological awareness scores")+ylab("Average AV gain") + geom_smooth(method='lm') +scale_color_gradientn(colours = rainbow(4))
cor.test(s$CTOPP_PA_raw,s$Norm_Gain)

#partial correlation of phonological awareness
df1 <-subset(AV.data,select=c("Avg AV Gain","Age","CTOPP_PA_raw","Avg_A","Avg_AV","V"))
df1 <- na.omit(df1)
pcor.test(df1$Avg_A,df1$CTOPP_PA_raw,df1$Age)
pcor.test(df1$Avg_AV,df1$CTOPP_PA_raw,df1$Age)
pcor.test(df1$V,df1$CTOPP_PA_raw,df1$Age)
pcor.test(df1$`Avg AV Gain`,df1$CTOPP_PA_raw,df1$Age)


##linear model av gain vs age, gender, pa
summary(lm(s$`Avg AV Gain`~s$Age+s$Gender+s$CTOPP_PA_raw))
Anova(lm(s$`Avg AV Gain`~s$Age+s$Gender+s$CTOPP_PA_raw))

model2 = lm(s$Norm_Gain~s$Age+s$CTOPP_PA_raw+s$Gender+s$foilcount)
library (car)
Anova(model2)
regressionBF(formula = Norm_Gain ~ Age, data = s)
##linear model av gain vs difference between 2 a-only conditions
summary(lm(AV.data$`Avg AV Gain`~AV.data$Acompare))
anova(lm(AV.data$`Avg AV Gain`~AV.data$Acompare))
##linear model
## sheet modal_SNR in summary, save as m
m=AV_V2_result_summary
m$modality=as.factor(m$modality)
m$Gender=as.factor(m$Gender)
m$SNR=as.factor(m$SNR)
m = m %>% mutate(SNR = relevel(SNR, "11"))
model1 = lmer(score~modality*Age*Gender+SNR+(1|subject),data=m)
summary(model1)
anova(model1)
##Logistic regression
d=AV_V2_alldata_typical

d$SNR = as.factor(d$SNR)
d$condition = as.factor(d$condition)
d$goal = as.factor(d$goal)
d$SNR_cond = as.factor(d$SNR_cond)

d2 = d %>% subset(condition != "V")
d3 = d2 %>% subset(condition != "A_2")
model1 = glmer(correct~SNR*condition+(1|goal)+(1|subject),data=d3,family=binomial)
summary(model1)
anova(model1)

d4 = d3 %>%
dplyr::group_by(subject, SNR,condition) %>%
dplyr::summarize(Nc = sum(correct), n = n(), PC = Nc/n)

model2 = lmer(PC~SNR*condition+ (1|subject), data=d4)
summary(model2)
anova(model2)

## Error analysis
d=AV_V2_alldata_typical

d$SNR = as.factor(d$SNR)
d$condition = as.factor(d$condition)
d$goal = as.factor(d$goal)
d$SNR_cond = as.factor(d$SNR_cond)

#d2 = d %>% subset(condition != "V")
d3 = d2 %>% subset(condition != "A_2")
d4 = d3  %>% subset(response != "Turtle_foil")
d5 = d4  %>% subset(response != "goal")

d5$score=1
d5pc = d5 %>%
  dplyr::group_by(condition,SNR, response,subject) %>%
  dplyr::summarize(Nc = sum(score))

library(tidyr)
d5pc$cond_resp=paste(d5pc$condition,d5pc$response,d5pc$SNR)
d5pc = subset(d5pc, select = -c(condition,response,SNR) )
d5pc=d5pc %>% pivot_wider(names_from = cond_resp, values_from = Nc)
d5pc[is.na(d5pc)] <- 0
write.csv(d5pc,'error_analysis_AVV2.csv')
# make column totals in csv file
# make new tab where every cell is divided by minpair+ vowel + random of same condition+SNR
## save as excelsheet - otherwise will delete tabs
#read error_analysis.xlsx = sheet 2
p=error_ana_final #sheet 1 for boxplots, 2 for pairwise comparison, sheet 3 for pairwise comparison per SNR
pairwise.wilcox.test(p$score, p$condition, p.adjust.method = "bonferroni", paired = FALSE) ##sheet 2
ggplot(p,aes(x=condition,y=score))+geom_boxplot()
#boxplots -11
boxplot(p$`A minimalpair 11`,p$`AV minimalpair 11`,p$`A vowel 11`,p$`AV vowel 11`,p$`A random 11`,p$`AV random 11`,  col=c("#0033CC","darkgreen"),names=c("","minimal pair","","vowel","","random"), main="Error analysis per category SNR -11",ylab="percentage errors compared to total errors per condition",xlab="type of error")
#boxplots -8
boxplot(p$`A minimalpair 8`,p$`AV minimalpair 8`,p$`A vowel 8`,p$`AV vowel 8`,p$`A random 8`,p$`AV random 8`,  col=c("dodgerblue","#009966"),names=c("","minimal pair","","vowel","","random"), main="Error analysis per category SNR -8",ylab="percentage errors compared to total errors per condition",xlab="type of error")
#boxplots -5
boxplot(p$`A minimalpair 5`,p$`AV minimalpair 5`,p$`A vowel 5`,p$`AV vowel 5`,p$`A random 5`,p$`AV random 5`,  col=c("cadetblue1", "palegreen" ),names=c("","minimal pair","","vowel","","random"), main="Error analysis per category SNR -5",ylab="percentage errors compared to total errors per condition",xlab="type of error")
## over SNRs
boxplot(p$A_minpair,p$AV_minpair,p$V_minpair,p$A_vowel,p$AV_vowel,p$V_vowel,p$A_random,p$AV_random,p$V_random,  col=c("blue2", "springgreen4","orange" ),names=c("","minimal pair","","","vowel","","","random",""), main="Error analysis per category all SNR's",ylab="percentage errors compared to total errors per condition",xlab="type of error")
points(1,mean(p$A_minpair),pch=19, col="gray")
points(2,mean(p$AV_minpair),pch=19, col="gray")
points(3,mean(p$V_minpair),pch=19, col="gray")
points(4,mean(p$A_vowel),pch=19, col="gray")
points(5,mean(p$AV_vowel),pch=19, col="gray")
points(6,mean(p$V_vowel),pch=19, col="gray")
points(7,mean(p$A_random),pch=19, col="gray")
points(8,mean(p$AV_random),pch=19, col="gray")
points(9,mean(p$V_random),pch=19, col="gray")



## analysis of viseme scores
d=AV_V2_alldata_typical
d2 = d %>% subset(condition != "V")
d3 = d2 %>% subset(condition != "A_2")
d4 = d3 %>%
  dplyr::group_by(subject,visemescore,condition) %>%
  dplyr::summarize(Nc = sum(correct), n = n(), PC = Nc/n)
d4$visemscore_cond <- paste(d4$visemescore,d4$condition)
#pairwise wilcox test
pairwise.wilcox.test(d4$PC, d4$visemscore_cond, p.adjust.method = "BH", paired = FALSE)

boxplot(d4$PC[d4$visemscore_cond=="1 A"],d4$PC[d4$visemscore_cond=="1 AV"],d4$PC[d4$visemscore_cond=="2 A"],d4$PC[d4$visemscore_cond=="2 AV"],d4$PC[d4$visemscore_cond=="3 A"],d4$PC[d4$visemscore_cond=="3 AV"],d4$PC[d4$visemscore_cond=="4 A"],d4$PC[d4$visemscore_cond=="4 AV"],col=c("blue2", "springgreen4"),names=c("A_1","AV_1","A_2","AV_2","A_3","AV_3","A_4","AV_4"), xlab = "viseme categories for A and AV", ylab="% correct", main="% correct per viseme category per condition")
points(1,mean(d4$PC[d4$visemscore_cond=="1 A"]),pch=19, col="gray")
points(2,mean(d4$PC[d4$visemscore_cond=="1 AV"]),pch=19, col="gray")
points(3,mean(d4$PC[d4$visemscore_cond=="2 A"]),pch=19, col="gray")
points(4,mean(d4$PC[d4$visemscore_cond=="2 AV"]),pch=19, col="gray")
points(5,mean(d4$PC[d4$visemscore_cond=="3 A"]),pch=19, col="gray")
points(6,mean(d4$PC[d4$visemscore_cond=="3 AV"]),pch=19, col="gray")
points(7,mean(d4$PC[d4$visemscore_cond=="4 A"]),pch=19, col="gray")
points(8,mean(d4$PC[d4$visemscore_cond=="4 AV"]),pch=19, col="gray")


#analysis visemescore  differences
d=visemes #(excel visemes)
d$visemescore=as.factor(d$visemescore)
summary(lm(d$pc_diff~d$visemescore))


#_________________________________________#
## TD VS DYS
l=dys_TD
describe(l)
summary(l$Age[l$Type=="TD"])
summary(l$Age[l$Type=="Dyslexic"])
t.test(l$Age[l$Type=="TD"],l$Age[l$Type=="Dyslexic"])
hist(l$Age[l$Type=="Dyslexic"], xlab="Age (years)", main= "Age Histogram Dyslexic children")
hist(l$Age[l$Type=="TD"], xlab="Age (years)", main= "Age Histogram TD children")
table(l$Gender[l$Type=="TD"])
table(l$Gender[l$Type=="Dyslexic"])

#boxplot
boxplot(l$`A-5`[l$Type=="TD"],l$`A-5`[l$Type=="Dyslexic"],l$`AV-5`[l$Type=="TD"],l$`AV-5`[l$Type=="Dyslexic"],l$`A-8`[l$Type=="TD"],l$`A-8`[l$Type=="Dyslexic"],l$`AV-8`[l$Type=="TD"],l$`AV-8`[l$Type=="Dyslexic"],l$`A-11`[l$Type=="TD"],l$`A-11`[l$Type=="Dyslexic"],l$`AV-11`[l$Type=="TD"],l$`AV-11`[l$Type=="Dyslexic"],ylab="percentage correct", xlab="modality and SNR"  ,names=c("A,SNR-5","A,SNR-5","AV,SNR-5","AV,SNR-5","A,SNR-8","A,SNR-8","AV,SNR -8","AV,SNR-8","A,SNR-11","A,SNR-11","AV,SNR -11","AV,SNR-11"),main= "Percentage correct per modality per SNR",col=c("cadetblue1","cadetblue1", "palegreen", "palegreen", "dodgerblue","dodgerblue",'#009966','#009966','#0033CC','#0033CC',"darkgreen","darkgreen"))
points(1,mean(l$`A-5`[l$Type=="TD"]),pch=19, col="gray")
points(2,mean(l$`A-5`[l$Type=="Dyslexic"]),pch=19, col="gray")
points(3,mean(l$`AV-5`[l$Type=="TD"]),pch=19, col="gray")
points(4,mean(l$`AV-5`[l$Type=="Dyslexic"]),pch=19, col="gray")
points(5,mean(l$`A-8`[l$Type=="TD"]),pch=19, col="gray")
points(6,mean(l$`A-8`[l$Type=="Dyslexic"]),pch=19, col="gray")
points(7,mean(l$`AV-8`[l$Type=="TD"]),pch=19, col="gray")
points(8,mean(l$`AV-8`[l$Type=="Dyslexic"]),pch=19, col="gray")
points(9,mean(l$`A-11`[l$Type=="TD"]),pch=19, col="gray")
points(10,mean(l$`A-11`[l$Type=="Dyslexic"]),pch=19, col="gray")
points(11,mean(l$`AV-11`[l$Type=="TD"]),pch=19, col="gray")
points(12,mean(l$`AV-11`[l$Type=="Dyslexic"]),pch=19, col="gray")

#Av gain by age
ggplot(l, aes(y=`Avg AV Gain`, x=Age,col =Type)) + xlab("Age")+ ylab("Average AV Gain")+ geom_point() + geom_smooth(method=lm, fullrange=FALSE,se=TRUE,aes(fill=Type)) +scale_color_manual(values=c("#6A6599FF", "#F8766D")) +geom_abline(slope=1)+ scale_fill_manual(values=c("#6A6599FF", "#F8766D"))
m1=lm(l$`Avg AV Gain`[l$Type=="TD"]~l$Age[l$Type=="TD"])
m2=lm(l$`Avg AV Gain`[l$Type=="Dyslexic"]~l$Age[l$Type=="Dyslexic"])
anova(m1,m2)


# A by AV
set.seed(3)
ggplot(l, aes(y=Avg_AV, x=Avg_A,col =Type)) + xlab("% correct A-only")+ ylab("% correct audiovisual")+ geom_point(size=2, position=position_jitter(h=0.025,w=0.025)) + geom_smooth(method=lm, fullrange=FALSE,se=TRUE,aes(fill=Type)) +scale_color_manual(values=c("#6A6599FF", "#F8766D")) +geom_abline(slope=1)+ scale_fill_manual(values=c("#6A6599FF", "#F8766D"))


#combine with Wj and ctopp data
df=data_zoom_sessions_LG
data <- left_join(l,df)
write.csv(data,'dys_td_wj.csv') # delete unnecessary columns +  save as excel
#import excel dys_td_wj.xlsx + save under g
g=dys_td_wj
summary(g$`WJ Basic Reading Skills`[g$Type=="TD"])
summary(g$`WJ Basic Reading Skills`[g$Type=="Dyslexic"])
boxplot(g$`WJ Basic Reading Skills`[g$Type=="TD"],g$`WJ Basic Reading Skills`[g$Type=="Dyslexic"],main="Reading skills",xlab= "Groups", names=c("TD","Dyslexic"),ylab= "WJ basic reading skills - normed")boxplot(g$`WJ Letter-Word ID raw`[g$Type=="TD"],g$`WJ Letter-Word ID raw`[g$Type=="Dyslexic"])

# normed reading scores and gain
ggplot(g, aes(y=`Avg AV Gain`, x=`WJ Basic Reading Skills`,col =Type)) + xlab("Normed reading scores")+ ylab("average AV Gain")+ geom_point(size=2, position=position_jitter(h=0.025,w=0.025)) + geom_smooth(method=lm, fullrange=FALSE,se=TRUE,aes(fill=Type)) +scale_color_manual(values=c("#6A6599FF", "#F8766D")) +geom_abline(slope=1)+ scale_fill_manual(values=c("#6A6599FF", "#F8766D"))
anova(lm(g$`Avg AV Gain`~g$`WJ Basic Reading Skills`*g$Type))

#raw reading scores and gain
ggplot(g, aes(y=`Avg AV Gain`, x=`WJ Letter-Word ID raw`,col =Type)) + xlab("Raw reading scores")+ ylab("average AV Gain")+ geom_point(size=2, position=position_jitter(h=0.025,w=0.025)) + geom_smooth(method=lm, fullrange=FALSE,se=TRUE,aes(fill=Type)) +scale_color_manual(values=c("#6A6599FF", "#F8766D")) +geom_abline(slope=1)+ scale_fill_manual(values=c("#6A6599FF", "#F8766D"))
anova(lm(g$`Avg AV Gain`~g$`WJ Letter-Word ID raw`*g$Type))

##linear model
## sheet modal_SNR in summary, save as m
m$modality=as.factor(m$modality)
m$Gender=as.factor(m$Gender)
m$SNR=as.factor(m$SNR)
m = m %>% mutate(SNR = relevel(SNR, "11"))
model1 = lmer(score~modality*Age*Type*Gender+SNR+(1|subject),data=m)
model1b = lmer(score~condition*Age*Type*Gender+(1|subject),data=m)

summary(model1)
anova(model1)


## density function
ggplot(data=x, aes(x=`Avg AV Gain`, group=Type, fill=Type)) + geom_density(adjust=1.5, alpha=.4) 
##bayes
regressionBF(formula = `Avg AV Gain` ~ Type, data = l)


## Error analysis
d=AV_V2_alldata_dyslexic

d$SNR = as.factor(d$SNR)
d$condition = as.factor(d$condition)
d$goal = as.factor(d$goal)
d$SNR_cond = as.factor(d$SNR_cond)
d$Type=as.factor(d$Type)

d2 = d %>% subset(condition != "V")
d3 = d2 %>% subset(condition != "A_2")
d4 = d3  %>% subset(response != "Turtle_foil")
d5 = d4  %>% subset(response != "goal")

d5$score=1
d5pc = d5 %>%
  dplyr::group_by(condition, response,subject,Type) %>%
  dplyr::summarize(Nc = sum(score))

library(tidyr)
d5pc$cond_resp=paste(d5pc$condition,d5pc$response,d5pc$Type)
d5pc = subset(d5pc, select = -c(condition,response,Type) )
d5pc=d5pc %>%
  pivot_wider(names_from = cond_resp, values_from = Nc)
d5pc[is.na(d5pc)] <- 0
write.csv(d5pc,'error_analysis_AVV2_Dys.csv')
# make column totals in csv file
# make new tab where every cell is divided by minpair+ vowel + random of same condition+SNR
## save as excelsheet - otherwise will delete tabs
#read error_analysis_AVV2_Dys.xlsx = sheet 2
p=error_analysis_AVV2_Dys
boxplot(p$`AV minimalpair TD`,p$`AV minimalpair Dyslexic`,p$`AV vowel TD`,p$`AV vowel Dyslexic`,p$`AV random TD`,p$`AV random Dyslexic`)
boxplot(p$`A minimalpair TD`,p$`A minimalpair Dyslexic`,p$`A vowel TD`,p$`A vowel Dyslexic`,p$`A random TD`,p$`A random Dyslexic`)

## analysis of viseme scores
d=AV_V2_alldata_dyslexic
d2 = d %>% subset(condition != "V")
d3 = d2 %>% subset(condition != "A_2")
d4 = d3 %>%
  dplyr::group_by(subject,visemescore,condition,Type) %>%
  dplyr::summarize(Nc = sum(correct), n = n(), PC = Nc/n)
d4$visemscore_cond <- paste(d4$visemescore,d4$condition,d4$Type)
boxplot(d4$PC[d4$visemscore_cond=="1 A TD"],d4$PC[d4$visemscore_cond=="1 A Dyslexic"],d4$PC[d4$visemscore_cond=="1 AV TD"],d4$PC[d4$visemscore_cond=="1 AV Dyslexic"],d4$PC[d4$visemscore_cond=="2 A TD"],d4$PC[d4$visemscore_cond=="2 A Dyslexic"],d4$PC[d4$visemscore_cond=="2 AV TD"],d4$PC[d4$visemscore_cond=="2 AV Dyslexic"],d4$PC[d4$visemscore_cond=="3 A TD"],d4$PC[d4$visemscore_cond=="3 A Dyslexic"],d4$PC[d4$visemscore_cond=="3 AV TD"],d4$PC[d4$visemscore_cond=="3 AV Dyslexic"],d4$PC[d4$visemscore_cond=="4 A TD"], d4$PC[d4$visemscore_cond=="4 A Dyslexic"],d4$PC[d4$visemscore_cond=="4 AV TD"], d4$PC[d4$visemscore_cond=="4 AV Dyslexic"],col=c("lightyellow","lightyellow","yellow","yellow","pink","pink","red","red","#BF80FF","#BF80FF","purple","purple","burlywood","burlywood","brown", "brown"),names=c("A_1 TD","A_1 Dys","AV_1 TD","AV_1 Dys","A_2 TD","A_2 Dys","AV_2 TD","AV_2 Dys","A_3 TD","A_3 Dys","AV_3 TD","AV_3 Dys","A_4 TD","A_4 Dys","AV_4 TD","AV_4 Dys"), xlab = "viseme categories for A and AV", ylab="% correct", main="% correct per viseme category per condition")

## reliability measure
# sheet 1 to make block A
library(dplyr)
d=Splitdata_dyslexic
d$SNR = as.factor(d$SNR)
d$condition = as.factor(d$condition)
d$goal = as.factor(d$goal)
d$SNR_cond = as.factor(d$SNR_cond)
d2 = d %>% subset(condition != "V")
d3 = d2 %>% subset(condition != "A_2")
d4 = d3 %>% dplyr::group_by(subject) %>% dplyr::summarize(Nc = sum(correct), n = n(), PC = Nc/n)
d4$block <-"A"
write.csv(d4,"/Users/liesbethgijbels/Desktop/Results_PA_AV/AV_V2/blockA.csv")
# repeat for sheet 2 and Block B
# combine 2 sheets in new excel sheet "reliab_dys_overall.xlsx"
s=reliab_dys_overall
cor.test(s$score_BlockA,s$score_BlockB)
#calculate spearman-brown using cor.coeff ==> 2*cor/(1+cor)   -> 2*(0.7228058)/(1+0.7228058)

##dys_td_wj - different for age matched page
anova(lm(r$Norm_Gain~r$Age+r$`WJ Basic Reading Skills`+r$`CTOPP Phonological Awareness`+r$Avg_A+r$V))

