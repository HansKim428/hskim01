library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)
library(lmtest)
library(car)
library(forecast)
library(MASS)
library(sandwich)


names(windowsFonts())


# 모형1
Remaster_08<-read.csv("C:\\Users\\NO160\\Desktop\\Remaster_08.csv")

##산점도
p <- ggplot(Remaster_08_re) +
    aes(x = 기준금리변경, y = CD오전변동) +
    coord_fixed(ratio=0.5)+
    scale_x_continuous(breaks=seq(-100,25,20))+
    scale_y_continuous(breaks=seq(-100,30,10))+
    geom_point(shape = "circle", size =1.75, colour = "black") +
    labs(x = "기준금리변경", y = "CD오전변동") +
    theme_minimal() +
    geom_smooth(
        method="lm",se=F, colour="blue") +
    theme(
        text = element_text(size = 17,family="mono", colour="black"),
        axis.title = element_text(size =22L,family="mono", face = "bold")
        )

plotly::ggplotly(p)


##회귀모형(K=29)
m1 <- lm(CD오전변동~기준금리변경, data=Remaster_08)

outlierTest(m1)
influence.measures(m1)
par(mfrow=c(2,2))
plot(m1)
summary(m1)
accuracy(m1)

dwtest(m1, alternative="two.sided")
coeftest(m1, vcov=vcovHC(m1))


###이상치 제거(k=26)
Remaster_08_re <- Remaster_08[-c(3,5,6),]
m1_re <- lm(CD오전변동~기준금리변경, data=Remaster_08_re)
outlierTest(m1_re)
par(mfrow=c(2,2))
plot(m1_re)
summary(m1_re)
accuracy(m1_re)
dwtest(m1_re, alternative="two.sided")
coeftest(m1_re, vcov=vcovHC(m1_re))





# 참고-전체
Remaster_En<-read.csv("C:\\Users\\NO160\\Desktop\\Remaster_Entire.csv")

##산점도
ggplot(Remaster_En) +
    aes(x = 기준금리변경, y = CD오전변동) +
    coord_fixed(ratio=0.5)+
    scale_x_continuous(breaks=seq(-100,25,25))+
    scale_y_continuous(breaks=seq(-100,25,25))+
    geom_point(shape = "circle", size = 1.75, colour = "black") +
    labs(x = "<기준금리변경>", y = "<CD오전변동>") +
    theme_minimal() +
    geom_smooth(
        method="lm",se=F, colour="blue") +
    theme(
        text = element_text(size = 16, colour="black"),
        axis.title = element_text(size = 17L, face = "bold")
    )

##회귀모형
m1_Entire <- lm(CD오전변동~기준금리변경, data=Remaster_En)
summary(m1_Entire)



# 모형Roley
##산점도
ggplot(Remaster_08_Roley) +
    aes(x = 기준금리변경, y = 총누적) +
    coord_fixed(ratio=0.5)+
    scale_x_continuous(breaks=seq(-100,25,25))+
    scale_y_continuous(breaks=seq(-100,25,25))+
    geom_point(shape = "circle", size = 1.75, colour = "black") +
    labs(x = "기준금리변경", y = "CD총변동") +
    theme_minimal() +
    geom_smooth(
        method="lm",se=F, colour="blue") +
    theme(
        text = element_text(size = 17,family="mono", colour="black"),
        axis.title = element_text(size =22L,family="mono", face = "bold")
    )

##회귀모형(총누적 k=29)
m_Roley <- lm(총누적~기준금리변경, data=Remaster_08)
outlierTest(m_Roley)
influence.measures(m_Roley)
par(mfrow=c(2,2))
plot(m_Roley)
summary(m_Roley)

###이상치 제거 (k=28)
Remaster_08_Roley <- Remaster_08[-c(6),]
m_Roley_re <- lm(총누적~기준금리변경, data=Remaster_08_Roley)
summary(m_Roley_re)
accuracy(m_Roley_re)
dwtest(m_Roley_re, alternative="two.sided")
coeftest(m_Roley_re, vcov=vcovHC(m_Roley_re))



##회귀모형(n일 누적)

Remaster_08<-read.csv("C:\\Users\\NO160\\Desktop\\Remaster_08.csv")


ggplot(Remaster_08) +
    aes(x = 기준금리변경, y = 누적6일) +
    coord_fixed(ratio=0.5)+
    scale_x_continuous(breaks=seq(-100,25,25))+
    scale_y_continuous(breaks=seq(-100,25,25))+
    geom_point(shape = "circle", size = 1.75, colour = "black") +
    labs(x = "기준금리변경", y = "CD총변동") +
    theme_minimal() +
    geom_smooth(
        method="lm",se=F, colour="blue") +
    theme(
        text = element_text(size = 17,family="mono", colour="black"),
        axis.title = element_text(size =22L,family="mono", face = "bold")
    )


Bef <- list()
Bef[1] <- lm(CD오전변동~기준금리변경분, data=Remaster_08)$coefficients[2]
Bef[2] <- lm(CD오전변동~기준금리변경분, data=Remaster_08)$coefficients[2]


Bef_1<-lm(누적1일~기준금리변경분, data=Remaster_08)
Bef_2<-lm(누적2일~기준금리변경분, data=Remaster_08)
Bef_3<-lm(누적3일~기준금리변경분, data=Remaster_08)
Bef_4<-lm(누적4일~기준금리변경분, data=Remaster_08)
Bef_5<-lm(누적5일~기준금리변경분, data=Remaster_08)
Bef_6<-lm(누적6일~기준금리변경분, data=Remaster_08)
Bef_7<-lm(누적7일~기준금리변경분, data=Remaster_08)
Bef_8<-lm(누적8일~기준금리변경분, data=Remaster_08)
Bef_9<-lm(누적9일~기준금리변경분, data=Remaster_08)
Bef_10<-lm(누적10일~기준금리변경분, data=Remaster_08)
Bef_11<-lm(누적11일~기준금리변경분, data=Remaster_08)
Bef_12<-lm(누적12일~기준금리변경분, data=Remaster_08)
Bef_13<-lm(누적13일~기준금리변경분, data=Remaster_08)
Bef_14<-lm(누적14일~기준금리변경분, data=Remaster_08)
Bef_15<-lm(누적15일~기준금리변경분, data=Remaster_08)
Bef_16<-lm(누적16일~기준금리변경분, data=Remaster_08)
Bef_17<-lm(누적17일~기준금리변경분, data=Remaster_08)
Bef_18<-lm(누적18일~기준금리변경분, data=Remaster_08)
Bef_19<-lm(누적19일~기준금리변경분, data=Remaster_08)
Bef_20<-lm(누적20일~기준금리변경분, data=Remaster_08)





outlierTest(Bef)
outlierTest(Bef_1)
outlierTest(Bef_2)
outlierTest(Bef_3)
outlierTest(Bef_4)
outlierTest(Bef_5)
outlierTest(Bef_6)
outlierTest(Bef_7)
outlierTest(Bef_8)
outlierTest(Bef_9)
outlierTest(Bef_10)
outlierTest(Bef_11)
outlierTest(Bef_12)
outlierTest(Bef_13)
outlierTest(Bef_14)
outlierTest(Bef_15)
outlierTest(Bef_15)
outlierTest(Bef_16)
outlierTest(Bef_17)
outlierTest(Bef_18)
outlierTest(Bef_19)
outlierTest(Bef_20)


influence.measures(Bef)
influence.measures(Bef_1)
influence.measures(Bef_2)
influence.measures(Bef_3)
influence.measures(Bef_4)
influence.measures(Bef_5)
influence.measures(Bef_6)
influence.measures(Bef_7)
influence.measures(Bef_8)
influence.measures(Bef_9)
influence.measures(Bef_10)
influence.measures(Bef_11)
influence.measures(Bef_12)
influence.measures(Bef_13)
influence.measures(Bef_14)
influence.measures(Bef_15)
influence.measures(Bef_16)
influence.measures(Bef_17)
influence.measures(Bef_18)
influence.measures(Bef_19)
influence.measures(Bef_20)

par(mfrow=c(2,2))
plot(Bef_12)



###이상치 제거 (k=25)
Remaster_08 <- Remaster_08[-c(2,3,4,6),]



summary(Bef)$adj.r.squared
summary(Bef_1)$adj.r.squared
summary(Bef_2)$adj.r.squared
summary(Bef_3)$adj.r.squared
summary(Bef_4)$adj.r.squared
summary(Bef_5)$adj.r.squared
summary(Bef_6)$adj.r.squared
summary(Bef_7)$adj.r.squared
summary(Bef_8)$adj.r.squared
summary(Bef_9)$adj.r.squared
summary(Bef_10)$adj.r.squared
summary(Bef_11)$adj.r.squared
summary(Bef_12)$adj.r.squared
summary(Bef_13)$adj.r.squared
summary(Bef_14)$adj.r.squared
summary(Bef_15)$adj.r.squared
summary(Bef_16)$adj.r.squared
summary(Bef_17)$adj.r.squared
summary(Bef_18)$adj.r.squared
summary(Bef_19)$adj.r.squared
summary(Bef_20)$adj.r.squared


accuracy(Bef)
accuracy(Bef_1)
accuracy(Bef_2)
accuracy(Bef_3)
accuracy(Bef_4)
accuracy(Bef_5)
accuracy(Bef_6)
accuracy(Bef_7)
accuracy(Bef_8)
accuracy(Bef_9)
accuracy(Bef_10)
accuracy(Bef_11)
accuracy(Bef_12)
accuracy(Bef_13)
accuracy(Bef_14)
accuracy(Bef_15)
accuracy(Bef_16)
accuracy(Bef_17)
accuracy(Bef_18)
accuracy(Bef_19)
accuracy(Bef_20)


summary(Bef)
summary(Bef_1)
summary(Bef_2)
summary(Bef_3)
summary(Bef_4)
summary(Bef_5)
summary(Bef_6)
summary(Bef_7)
summary(Bef_8)
summary(Bef_9)
summary(Bef_10)
summary(Bef_11)
summary(Bef_12)
summary(Bef_13)
summary(Bef_14)
summary(Bef_15)
summary(Bef_16)
summary(Bef_17)
summary(Bef_18)
summary(Bef_19)
summary(Bef_20)


dwtest(Bef, alternative="two.sided")
dwtest(Bef_1, alternative="two.sided")
dwtest(Bef_2, alternative="two.sided")
dwtest(Bef_3, alternative="two.sided")
dwtest(Bef_4, alternative="two.sided")
dwtest(Bef_5, alternative="two.sided")
dwtest(Bef_6, alternative="two.sided")
dwtest(Bef_7, alternative="two.sided")
dwtest(Bef_8, alternative="two.sided")
dwtest(Bef_9, alternative="two.sided")
dwtest(Bef_10, alternative="two.sided")
dwtest(Bef_11, alternative="two.sided")
dwtest(Bef_12, alternative="two.sided")
dwtest(Bef_13, alternative="two.sided")
dwtest(Bef_14, alternative="two.sided")
dwtest(Bef_15, alternative="two.sided")
dwtest(Bef_16, alternative="two.sided")
dwtest(Bef_17, alternative="two.sided")
dwtest(Bef_18, alternative="two.sided")
dwtest(Bef_19, alternative="two.sided")
dwtest(Bef_20, alternative="two.sided")


coeftest(Bef, vcov=vcovHC(Bef))
coeftest(Bef_1, vcov=vcovHC(Bef_1))
coeftest(Bef_2, vcov=vcovHC(Bef_2))
coeftest(Bef_3, vcov=vcovHC(Bef_3))
coeftest(Bef_4, vcov=vcovHC(Bef_4))
coeftest(Bef_5, vcov=vcovHC(Bef_5))
coeftest(Bef_6, vcov=vcovHC(Bef_6))
coeftest(Bef_7, vcov=vcovHC(Bef_7))
coeftest(Bef_8, vcov=vcovHC(Bef_8))
coeftest(Bef_9, vcov=vcovHC(Bef_9))
coeftest(Bef_10, vcov=vcovHC(Bef_10))
coeftest(Bef_11, vcov=vcovHC(Bef_11))
coeftest(Bef_12, vcov=vcovHC(Bef_12))
coeftest(Bef_13, vcov=vcovHC(Bef_13))
coeftest(Bef_14, vcov=vcovHC(Bef_14))
coeftest(Bef_15, vcov=vcovHC(Bef_15))
coeftest(Bef_16, vcov=vcovHC(Bef_16))
coeftest(Bef_17, vcov=vcovHC(Bef_17))
coeftest(Bef_18, vcov=vcovHC(Bef_18))
coeftest(Bef_19, vcov=vcovHC(Bef_19))
coeftest(Bef_20, vcov=vcovHC(Bef_20))



# 모형2
CHECK<-read.csv("C:\\Users\\NO160\\Desktop\\Regress_CHECKPOLL.csv")

##산점도
ggplot(CHECK_re) +
    aes(x = Unexpected_CHECK, y = CD오전변동) +
    coord_fixed(ratio=0.5)+
    scale_x_continuous(breaks=seq(-40,20,10))+
    scale_y_continuous(breaks=seq(-40,20,10))+
    geom_point(shape = "circle", size = 1.75, colour = "black") +
    labs(x = "기준금리변경-비예측", y = "CD오전변동") +
    theme_minimal() +
    geom_smooth(
        method="lm",se=F, colour="blue") +
    theme(
        text = element_text(size = 17,family="mono", colour="black"),
        axis.title = element_text(size =22L,family="mono", face = "bold")
    )


ggplot(CHECK) +
    aes(x = Expected, y = CD오전변동) +
    coord_fixed(ratio=0.5)+
    scale_x_continuous(breaks=seq(-100,25,25))+
    scale_y_continuous(breaks=seq(-100,25,25))+
    geom_point(shape = "circle", size = 1.75, colour = "black") +
    labs(x = "<기준금리변경_비예측>", y = "<CD오전변동>") +
    theme(
        text = element_text(size = 17,family="mono", colour="black"),
        axis.title = element_text(size =22L,family="mono", face = "bold")
    )

##회귀모형(k=9)
m2_1 <- lm(CD오전변동~Expected, data=CHECK)
m2_2 <- lm(CD오전변동~Unexpected_CHECK, data=CHECK)
m2_3 <- lm(CD오전변동~Expected+Unexpected_CHECK, data=CHECK)
summary(m2_1)
summary(m2_2)
summary(m2_3)

###임시회의제외시
CHECK_sub <- CHECK[-5,]
m2_sub1 <- lm(CD오전변동~Expected, data=CHECK_sub)
m2_sub2 <- lm(CD오전변동~Unexpected_CHECK, data=CHECK_sub)
m2_sub3 <- lm(CD오전변동~Expected+Unexpected_CHECK, data=CHECK_sub)
summary(m2_sub1)
summary(m2_sub2)
summary(m2_sub3)


outlierTest(m2_3)
influence.measures(m2_3)
par(mfrow=c(2,2))
plot(m2_3)
accuracy(m2_3)
vif(m2_3)
dwtest(m2_3, alternative="two.sided")
coeftest(m2_3, vcov=vcovHC(m2_3))

outlierTest(m2_sub3)
influence.measures(m2_sub3)
par(mfrow=c(2,2))
plot(m2_sub3)
accuracy(m2_sub3)
vif(m2_sub3)
dwtest(m2_sub3, alternative="two.sided")
coeftest(m2_sub3, vcov=vcovHC(m2_sub3))

outlierTest(m2_2)
influence.measures(m2_2)
par(mfrow=c(2,2))
plot(m2_2)
summary(m2_2)
accuracy(m2_2)
dwtest(m2_2, alternative="two.sided")
coeftest(m2_2, vcov=vcovHC(m2_2))                



###이상치 제거 모형(k=8)
CHECK_re <- CHECK[-c(2),]  #이상치는 없음 또는 2,4,9
m2_re <- lm(CD오전변동~Unexpected_CHECK, data=CHECK_re)

outlierTest(m2_re)
influence.measures(m2_re)
par(mfrow=c(2,2))
plot(m2_re)
summary(m2_re)
accuracy(m2_re)
dwtest(m2_re, alternative="two.sided")
coeftest(m2_re, vcov=vcovHC(m2_re)) 


# 모형3(실패)
BMSI<-read.csv("C:\\Users\\NO160\\Desktop\\Regress_BMSI.csv")

##산점도
ggplot(BMSI) +
    aes(x = Unexpected_BMSI, y = CD오전변동) +
    coord_fixed(ratio=0.5)+
    scale_x_continuous(breaks=seq(-100,25,25))+
    scale_y_continuous(breaks=seq(-100,25,25))+
    geom_point(shape = "circle", size = 1.75, colour = "black") +
    labs(x = "<기준금리변경_비예측>", y = "<CD오전변동>") +
    theme_minimal() +
    geom_smooth(
        method="lm",se=F, colour="blue") +
    theme(
        text = element_text(size = 17,family="mono", colour="black"),
        axis.title = element_text(size =22L,family="mono", face = "bold")
    )

ggplot(BMSI) +
    aes(x = Expected, y = CD오전변동) +
    coord_fixed(ratio=0.5)+
    scale_x_continuous(breaks=seq(-100,25,25))+
    scale_y_continuous(breaks=seq(-100,25,25))+
    geom_point(shape = "circle", size = 1.75, colour = "black") +
    labs(x = "<기준금리변경_예측>", y = "<CD오전변동>") +
    theme_minimal() +
    geom_smooth(
        method="lm",se=F, colour="blue") +
    theme(
        text = element_text(size = 17,family="mono", colour="black"),
        axis.title = element_text(size =22L,family="mono", face = "bold")
    )



##회귀모형
m3_1 <- lm(CD오전변동~Expected, data=BMSI)
m3_2 <- lm(CD오전변동~Unexpected_BMSI, data=BMSI)
m3_3 <- lm(CD오전변동~Expected+Unexpected_BMSI, data=BMSI)
summary(m3_1)
summary(m3_2)
summary(m3_3)


###큰 변동 제거 3,5,6,7,25
BMSI_25<- BMSI[-c(3,5,6,7,25),]
m3_25 <- lm(CD오전변동~Expected+Unexpected_BMSI, data=BMSI_25)
summary(m3_25)

outlierTest(m3)

influence.measures(m3)
par(mfrow=c(2,2))
plot(m3)
summary(m3)
accuracy(m3)

###이상치 제거 모형
BMSI_re <- BMSI[-c(2,5,6),]
m3_re <- lm(CD오전변동~Unexpected_BMSI, data=BMSI_re)

par(mfrow=c(2,2))
plot(m3_re)
summary(m3_re)
accuracy(m3_re)



# 모형4
IRS_AM<-read.csv("C:\\Users\\NO160\\Desktop\\Regress_IRS.csv")

##산점도
ggplot(IRS_AM) +
    aes(x = IRS6M오전변동, y = CD오전변동) +
    coord_fixed(ratio=0.5)+
    scale_x_continuous(breaks=seq(-25,15,5))+
    scale_y_continuous(breaks=seq(-40,20,10))+
    geom_point(shape = "circle", size = 1.75, colour = "black") +
    labs(x = "<기준금리변경_비예측IRS6M>", y = "<CD오전변동>") +
    theme_minimal() +
    geom_smooth(
        method="lm",se=F, colour="blue") +
    theme(
        text = element_text(size = 17,family="mono", colour="black"),
        axis.title = element_text(size =22L,family="mono", face = "bold")
    )

ggplot(IRS_AM) +
    aes(x = IRS1Y오전변동, y = CD오전변동) +
    coord_fixed(ratio=0.2)+
    scale_x_continuous(breaks=seq(-10,10,2))+
    scale_y_continuous(breaks=seq(-40,20,10))+
    geom_point(shape = "circle", size = 1.75, colour = "black") +
    labs(x = "<기준금리변경_비예측IRS1Y>", y = "<CD오전변동>") +
    theme_minimal() +
    geom_smooth(
        method="lm",se=F, colour="blue") +
    theme(
        text = element_text(size = 17,family="mono", colour="black"),
        axis.title = element_text(size =22L,family="mono", face = "bold")
    )

ggplot(IRS_AM) +
    aes(x = IRS2Y오전변동, y = CD오전변동) +
    coord_fixed(ratio=0.2)+
    scale_x_continuous(breaks=seq(-10,10,2))+
    scale_y_continuous(breaks=seq(-40,20,10))+
    geom_point(shape = "circle", size = 1.75, colour = "black") +
    labs(x = "<기준금리변경_비예측IRS2Y>", y = "<CD오전변동>") +
    theme_minimal() +
    geom_smooth(
        method="lm",se=F, colour="blue") +
    theme(
        text = element_text(size = 17,family="mono", colour="black"),
        axis.title = element_text(size =22L,family="mono", face = "bold")
    )

ggplot(IRS_AM) +
    aes(x = IRS3Y오전변동, y = CD오전변동) +
    coord_fixed(ratio=0.2)+
    scale_x_continuous(breaks=seq(-10,10,2))+
    scale_y_continuous(breaks=seq(-40,20,10))+
    geom_point(shape = "circle", size = 1.75, colour = "black") +
    labs(x = "<기준금리변경_비예측IRS3Y>", y = "<CD오전변동>") +
    theme_minimal() +
    geom_smooth(
        method="lm",se=F, colour="blue") +
    theme(
        text = element_text(size = 17,family="mono", colour="black"),
        axis.title = element_text(size =22L,family="mono", face = "bold")
    )

ggplot(IRS_AM) +
    aes(x = IRS5Y오전변동, y = CD오전변동) +
    coord_fixed(0.2)+
    scale_x_continuous(breaks=seq(-10,10,2))+
    scale_y_continuous(breaks=seq(-40,20,10))+
    geom_point(shape = "circle", size = 1.75, colour = "black") +
    labs(x = "<기준금리변경_비예측IRS5Y>", y = "<CD오전변동>") +
    theme_minimal() +
    geom_smooth(
        method="lm",se=F, colour="blue") +
    theme(
        text = element_text(size = 17,family="mono", colour="black"),
        axis.title = element_text(size =22L,family="mono", face = "bold")
    )


##회귀모형(n=14)
m4_6M <- lm(CD오전변동~IRS6M오전변동, data=IRS_AM)
m4_1Y <- lm(CD오전변동~IRS1Y오전변동, data=IRS_AM)
m4_2Y <- lm(CD오전변동~IRS2Y오전변동, data=IRS_AM)
m4_3Y <- lm(CD오전변동~IRS3Y오전변동, data=IRS_AM)
m4_5Y <- lm(CD오전변동~IRS5Y오전변동, data=IRS_AM)

confint(lm(CD오전변동~IRS6M오전변동, data=IRS_AM), level=0.99)



outlierTest(m4_6M)
influence.measures(m4_6M)
par(mfrow=c(2,2))
plot(m4_6M)

outlierTest(m4_1Y)
influence.measures(m4_1Y)
par(mfrow=c(2,2))
plot(m4_1Y)

outlierTest(m4_2Y)
influence.measures(m4_2Y)
par(mfrow=c(2,2))
plot(m4_2Y)

outlierTest(m4_3Y)
influence.measures(m4_3Y)
par(mfrow=c(2,2))
plot(m4_3Y)

#5y 보고안함
outlierTest(m4_5Y)
influence.measures(m4_5Y)
par(mfrow=c(2,2))
plot(m4_5Y)


summary(m4_6M)
max(abs(residuals(m4_6M))
accuracy(m4_6M)
dwtest(m4_6M, alternative="two.sided")
coeftest(m4_6M, vcov=vcovHC(m4_6M)) 

summary(m4_1Y)
accuracy(m4_1Y)
dwtest(m4_1Y, alternative="two.sided")
coeftest(m4_1Y, vcov=vcovHC(m4_1Y)) 


summary(m4_2Y)
accuracy(m4_2Y)
dwtest(m4_2Y, alternative="two.sided")
coeftest(m4_2Y, vcov=vcovHC(m4_2Y)) 


summary(m4_3Y)
accuracy(m4_3Y)
dwtest(m4_3Y, alternative="two.sided")
coeftest(m4_3Y, vcov=vcovHC(m4_3Y)) 

summary(m4_5Y)
accuracy(m4_5Y)
dwtest(m4_5Y, alternative="two.sided")
coeftest(m4_5Y, vcov=vcovHC(m4_5Y)) 


###계수 안정성 점검
IRS_AM<-read.csv("C:\\Users\\NO160\\Desktop\\Regress_IRS.csv")
IRS_AMT<-IRS_AM[-c(14:11),]
m4_6M <- lm(CD오전변동~IRS6M오전변동, data=IRS_AMT)
summary(m4_6M)


# 모형4 KTB

KTBF<-read.csv("C:\\Users\\NO160\\Desktop\\Regress_KTB.csv")

ggplot(KTBF) +
    aes(x = Win30, y = CD오전변동) +
    scale_y_continuous(breaks=seq(-40,20,10))+
    geom_point(shape = "circle", size = 1.75, colour = "black") +
    labs(x = "30분가격변동", y = "CD오전변동") +
    theme_minimal() +
    geom_smooth(
        method="lm",se=F, colour="blue") +
    theme(
        text = element_text(size = 17,family="mono", colour="black"),
        axis.title = element_text(size =22L,family="mono", face = "bold")
    )


ggplot(KTBF_rere) +
    aes(x = WinYest1130_수익률, y = CD오전변동) +
    scale_y_continuous(breaks=seq(-40,20,10))+
    geom_point(shape = "circle", size = 1.75, colour = "black") +
    labs(x = "<30분수익률변동>", y = "<CD오전변동>") +
    theme_minimal() +
    geom_smooth(
        method="lm",se=F, colour="blue") +
    theme(
        text = element_text(size = 17,family="mono", colour="black"),
        axis.title = element_text(size =22L,family="mono", face = "bold")
    )


m5 <- lm(CD오전변동~Win30, data=KTBF)
m6 <- lm(CD오전변동~Win30_수익률, data=KTBF)
m7 <- lm(CD오전변동~Win60, data=KTBF)
m8 <- lm(CD오전변동~Win60_수익률, data=KTBF)
m9 <- lm(CD오전변동~Win1130, data=KTBF)
m10 <- lm(CD오전변동~Win1130_수익률, data=KTBF)
m11 <- lm(CD오전변동~WinYest1130, data=KTBF)
m12 <- lm(CD오전변동~WinYest1130_수익률, data=KTBF)

summary(m5)
summary(m6)
summary(m7)
summary(m8)
summary(m9)
summary(m10)
summary(m11)
summary(m12)

# 9,10은 미보고
outlierTest(m6)
outlierTest(m8)
outlierTest(m10)

influence.measures(m6)
influence.measures(m8)
influence.measures(m10)


par(mfrow=c(2,2))
plot(m6)
plot(m8)
plot(m10)

outlierTest(m5)
outlierTest(m7)
outlierTest(m9)

influence.measures(m5)
influence.measures(m7)
influence.measures(m9)


par(mfrow=c(2,2))
plot(m5)
plot(m7)
plot(m9)



KTBF_re <- KTBF[-c(4,5,11),]

m5 <- lm(CD오전변동~Win30, data=KTBF_re)
m6 <- lm(CD오전변동~Win30_수익률, data=KTBF_re)
m7 <- lm(CD오전변동~Win60, data=KTBF_re)
m8 <- lm(CD오전변동~Win60_수익률, data=KTBF_re)
m9 <- lm(CD오전변동~Win1130, data=KTBF_re)
m10 <- lm(CD오전변동~Win1130_수익률, data=KTBF_re)

summary(m5)$adj.r.squared
summary(m6)$adj.r.squared
summary(m7)$adj.r.squared
summary(m8)$adj.r.squared
summary(m9)$adj.r.squared
summary(m10)$adj.r.squared


accuracy(m5)
accuracy(m6)
accuracy(m7)
accuracy(m8)
accuracy(m9)
accuracy(m10)

dwtest(m5, alternative="two.sided")
coeftest(m5, vcov=vcovHC(m5)) 
dwtest(m6, alternative="two.sided")
coeftest(m6, vcov=vcovHC(m6)) 
dwtest(m7, alternative="two.sided")
coeftest(m7, vcov=vcovHC(m7)) 
dwtest(m8, alternative="two.sided")
coeftest(m8, vcov=vcovHC(m8)) 
dwtest(m9, alternative="two.sided")
coeftest(m9, vcov=vcovHC(m9)) 
dwtest(m10, alternative="two.sided")
coeftest(m10, vcov=vcovHC(m10)) 

KTBF_re <- KTBF[-c(4,5),]
m5_re <- lm(CD오전변동~Window30, data=KTBF_re)
m6_re <- lm(CD오전변동~Window30_수익률, data=KTBF_re)

summary(m5_re)
outlierTest(m5_re)
influence.measures(m5_re)
par(mfrow=c(2,2))
plot(m5_re)

summary(m6_re)
outlierTest(m6_re)
influence.measures(m6_re)
par(mfrow=c(2,2))
plot(m6_re)

accuracy(m5_re)
accuracy(m6_re)


KTBF_rere <- KTBF[-c(2,3,5),]

m11 <- lm(CD오전변동~WinYest1130, data=KTBF_rere)
m12 <- lm(CD오전변동~WinYest1130_수익률, data=KTBF_rere)

summary(m11)$adj.r.squared
summary(m12)$adj.r.squared

accuracy(m11)
accuracy(m12)

dwtest(m11, alternative="two.sided")
coeftest(m11, vcov=vcovHC(m11)) 
dwtest(m12, alternative="two.sided")
coeftest(m12, vcov=vcovHC(m12)) 






###적용일자 확대시(단순화방안) 다중회귀

Regress_MPC<-read.csv("C:\\Users\\Admin\\Desktop\\Regress_MPC.csv")
m_mpc <- lm(CD변동~변수1+변수2, data=Regress_MPC)
Regress_All<-read.csv("C:\\Users\\Admin\\Desktop\\Regress_All.csv")
m_all <- lm(CD변동~변수1+변수2+변수3, data=Regress_All)

summary(m_mpc)
summary(m_all)

Regress_All[,13] <- abs(Regress_All[,10]*1.4-Regress_All$CD변동)
Regress_All[,14] <- abs(Regress_All[,10]-Regress_All$CD변동)

Regress_All[,15] <- abs(Regress_All[,11]*1.4-Regress_All$CD변동)
Regress_All[,16] <- abs(Regress_All[,11]-Regress_All$CD변동)

Regress_All[,17] <- abs(Regress_All[,12]*1.4-Regress_All$CD변동)
Regress_All[,18] <- abs(Regress_All[,12]-Regress_All$CD변동)


linearHypothesis(m_mpc, "변수1=변수2")
linearHypothesis(m_all, "변수1=변수3")
linearHypothesis(m_all, "변수1=변수2")
linearHypothesis(m_all, "변수2=변수3")

outlierTest(m12)
influence.measures(m12)
outlierTest(m11)
influence.measures(m11)
plot(m12)
plot(m11)

accuracy(m5)
accuracy(m5_100)

outlierTest(m5)
influence.measures(m5)


summary(m6)
outlierTest(m6)
influence.measures(m6)
par(mfrow=c(2,2))
plot(m6)




##call-IRS6M 다중회귀
Regress_call <- read.csv("C:\\Users\\Admin\\Desktop\\Regress_call.csv")
m_call <- lm(CD~Call+X6MIRS,data=Regress_call)
summary(m_call)
vif(m_call)

m_IRS6MM <- lm(CD~X6MIRS, data=Regress_call)
summary(m_IRS6MM)
m_calll <- lm(CD~Call, data=Regress_call)
summary(m_calll)

accuracy(m_call)
accuracy(m_IRS6MM)


















vars1<-c("오전CD", "오후CD", "기준금리")    #수준변수 다중산점도
pairs(Master[vars1],main="수준",cex=1)

vars2<-c("CD오전변동", "CD오후변동", "CD차분", "기준금리차분")  #차분변수 다중산점도
pairs(Master[vars2],main="차분",cex=1,)

Master[["전일차분"]] <- lag(Master[[7]],1)
Master$"전일차분"[1] <-  0

cor(Master$기준금리차분, Master$CD오전변동)
cor(Master$기준금리차분, Master$전일차분)


par(mfrow=c(1,2))

plot(오전변동~기준금리변경, data=Master, pch=18, col=3,cex=1)

abline(Bef_4,col="red")
r1<-str_c("r=",round(cor(Master$CD오전변동,Master$기준금리차분),3))
text(x=-40,y=-60,labels=r1)



plot(CD오후변동~기준금리차분, data=Master, pch=18, col=3,cex=1,ylim=c(-70,20))
m2<-lm(CD오후변동~기준금리차분,data=Master)
abline(m2,col="red")
r2<-str_c("r=",round(cor(Master$CD오후변동,Master$기준금리차분),3))
text(x=-40,y=-60,labels=r2)

#Master[["a"]] <- Master[[2]]-Master[[3]]
#lag(Master[[1]],1)

#상관분석--상관관계 유의성


#교차상관계수
ccf(Master$오전CD, Master$기준금리,type="correlation")
ccf(Master$오후CD, Master$기준금리,type="correlation")
cor(Master$오전CD, Master$기준금리)
cor(Master$오후CD, Master$기준금리)

ccf(Master$CD오전변동, Master$기준금리차분,type="correlation")
ccf(Master$CD오후변동, Master$기준금리차분,type="correlation")
print(ccf(Master$CD오전변동, Master$기준금리차분))



#시기에 따른 구조적 변화

#1. GQ Test
##수준
M1<-lm(오전CD~기준금리, data=Master)
M2<-lm(오후CD~기준금리, data=Master)

gqtest(M1,alternative="two.sided")
gqtest(M2,alternative="two.sided")

##차분
gqtest(m1,alternative="two.sided")
gqtest(m2,alternative="two.sided")

#2.Chow 및 Wald 검정



summary(m1)
par(mfrow=c(2,2))
plot(Bef_0)
influence.measures(Bef_0)

par(mfrow=c(2,2))
plot(Bef_4)
influence.measures(Bef_4)








summary(Bef_5)
plot(Bef_5)
dwtest(Bef_5)

summary(Bef)
plot(Bef_3)
dwtest(Bef_3)


summary(Bef_4)
summary.lm(Bef_4)
par(mfrow=c(2,2))
p<-plot(Bef_4)
plotly::ggplotly(p)


#dwtest,outliertest의미, accuracy 선택, non-announcement
#제거된 이상치에 대한 잔차도

#신뢰구간
confint(Bef_4)
#studentized residuals
stud_resids <- studres(Bef_4)
plot(Master$PreSh4, stud_resids)


#잔차의 자기상관 검정
dwtest(Bef_4, alternative="two.sided")
acf(Bef_4)

#tukey test????
residualPlots(Bef_4)
