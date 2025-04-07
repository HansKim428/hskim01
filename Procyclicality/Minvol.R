# <LIBRARY>--------------------------------------------------
library(tidyverse)
library(hablar)
library(lubridate)
library(foreach)
library(doParallel)
library(parallel)
library(data.table)
library(readxl)
library(reshape2)
library(DBI)       # DB관련 함수 사용을 위한 라이브러리
library(RSQLite)   # SQLite 사용을 위한 라이브러리
library(bbplot)


extrafont::loadfonts(device='win')
windowsFonts()



# <FUNCTION>--------------------------------------------------

## 반올림함수
round_off <- function(x, digits = 0) {
    posneg = sign(x)
    z = trunc(abs(x) * 10 ^ (digits + 1)) / 10
    z = floor(z * posneg + 0.5) / 10 ^ digits
    return(z)
}

## 선형보간함수
interpol <- function(TDay, D1, D2, R1, R2) {
    z = R1 + (R2 - R1) * as.numeric(TDay - D1) / as.numeric(D2 - D1)
    return(z)
}

## ZCRtoDF
ZCRtoDF <- function(zcr, Dt, De) {
    z = exp(-zcr * as.numeric(Dt - De) / 365)
    return(z)
}

## DFtoZCR
DFtoZCR <- function(DF, Dt, De) {
    z = log(1 / DF) * 365 / as.numeric(Dt - De)
    return(z)
}

## 계약별 CashFlow 생성
CF_3nM <- function(CAL,
                   Dur,
                   BDC,
                   EOM,
                   EffDate,
                   CLnum,
                   Pay_Acc,
                   Rec_Acc,
                   Notional,
                   Fixed_Rate,
                   Fixed_DCF,
                   Float_DCF) {
    ifelse(
        EOM == "N",
        CF_3nM_S <- EffDate %m+% months(seq(0, Dur - 3, 3)),
        CF_3nM_S <- ceiling_date(EffDate %m+%
                                     months(seq(0, Dur - 3, 3)),
                                 "month") - 1
    )
    
    ifelse(
        EOM == "N",
        CF_3nM_E <- EffDate %m+% months(seq(3, Dur, 3)),
        CF_3nM_E <- ceiling_date(EffDate %m+%
                                     months(seq(3, Dur, 3)),
                                 "month") - 1
    )
    
    
    return(
        cbind(
            CF_3nM_S,
            CF_3nM_E,
            BDC,
            CLnum,
            Pay_Acc,
            Rec_Acc,
            Notional,
            Fixed_Rate,
            Fixed_DCF,
            Float_DCF
        )
    )
} 






# <SETTINGS>--------------------------------------------------
## [기본세팅]
options(digits = 15)
options(scipen = 999)




##3days min vol----------------------------------------------

load('./MInvol 3days Result/IM_170807.Rdata')
IM_volmin <- IM
load('./MInvol 3days Result/IM_170807_nomin.Rdata')
IM_nomin <- IM

sum(IM_volmin$IM)
sum(IM_nomin$IM)



load('./MInvol 3days Result/IM_180808.Rdata')
IM_volmin <- IM
load('./MInvol 3days Result/IM_180808_nomin.Rdata')
IM_nomin <- IM

sum(IM_volmin$IM)
sum(IM_nomin$IM)



load('./MInvol 3days Result/IM_210111.Rdata')
IM_volmin <- IM
load('./MInvol 3days Result/IM_210111_nomin.Rdata')
IM_nomin <- IM

sum(IM_volmin$IM)
sum(IM_nomin$IM)




##Min vol 349days---------------------------------------

load('IM_Minvol.Rdata')
load('IM_Minvol_real.Rdata')

IM_Minvol <- na.omit(IM_Minvol)
IM_Minvol_real <- na.omit(IM_Minvol_real)

sum_Minvol <- apply(IM_Minvol[, -c(1,2)], 2, sum)
sum_Minvol_real <- apply(IM_Minvol_real[, -c(1,2)], 2, sum)

sum_Minvol_comp <- data.frame(cbind(sum_Minvol, sum_Minvol_real)) %>% 
    mutate(sum_Minvol_real = -sum_Minvol_real,
           sum_Minvol = -sum_Minvol,
        Rate = sum_Minvol_real/sum_Minvol)

sum_Minvol_comp <- cbind(DATE = as.Date(colnames(IM_Minvol)[-c(1,2)]), sum_Minvol_comp)


Min_vol_graph <- ggplot(sum_Minvol_comp, aes(x = DATE, group = 1)) +
    geom_line(aes(y = sum_Minvol / 10 ^ 8), color = '#FAAB18', size = 1.3) +
    geom_line(aes(y = sum_Minvol_real / 10 ^ 8), color = '#1380A1', size = 1.3) +
    geom_line(aes(y = Rate * 5000), color = '#ff3399', linetype = 'dashed', size = 1) +
    bbc_style() +
    geom_hline(yintercept = 5000,
               size = 1,
               colour = "#333333") +
    scale_y_continuous(
        name = 'Total IM',
        labels = scales::comma,
        breaks = seq(5000,13000, 2000),
        sec.axis = sec_axis(~ . / 5000, name = '증가효과', c(1.0, 1.4, 1.8, 2.2, 2.6))

    ) +
    labs(title = "최소변동성 적용여부에 따른 증거금(좌) 및 증가효과(우)",
         subtitle = "'20.6.9~'21.11.3(단위: 억원-좌, 배-우)") +
    scale_x_date(date_breaks = "3 month" , date_labels = "%y.%b.%d")


finalise_plot(plot_name = Min_vol_graph,
              source_name = '',
              save_filepath = 'Min_vol_graph.png',
              width_pixels = 900,
              height_pixels = 500)



##계좌별-------------------------------------------------------

Minvol_diff <- IM_Minvol[, -c(1,2)] - IM_Minvol_real[, -c(1,2)]

Minvol_diff <- rbind(Minvol_diff, t(data.frame(Minvol_diff %>% apply(2, sum))))


for (i in 1:ncol(Minvol_diff)) {
    Minvol_diff[62, i] <- length(which(Minvol_diff[-61, i] < 0))
}

Minvol_diff_F <- Minvol_diff[c(61,62), ]
rownames(Minvol_diff_F) <- c('Diff', 'Reduced_Num')

Minvol_diff_F <- cbind(data.frame(colnames(Minvol_diff_F)), data.frame(t(Minvol_diff_F)))
colnames(Minvol_diff_F)[1] <- 'DATE'
Minvol_diff_F <- Minvol_diff_F %>% mutate(DATE = as.Date(DATE))



Min_vol_diff_graph <- ggplot(Minvol_diff_F, aes(x = DATE, group = 1)) +
    geom_line(aes(y = Diff / 10 ^ 8), color = '#FAAB18', size = 1.3) +
    geom_line(aes(y = Reduced_Num * 100), color = '#1380A1', size = 1.3) +
    bbc_style() +
    geom_hline(yintercept = 0,
               size = 1,
               colour = "#333333") +
    scale_y_continuous(
        name = 'IM_diff',
        labels = scales::comma,
        sec.axis = sec_axis(~ . / 100, name = '감소계좌')
        
    ) +
    labs(title = "총증거금 증가분(좌) 및 증거금 감소 계좌수(우)",
         subtitle = "'20.6.9~'21.11.3(단위: 억원-좌, 개-우)") +
    scale_x_date(date_breaks = "3 month" , date_labels = "%y.%b.%d")


finalise_plot(plot_name = Min_vol_diff_graph,
              source_name = '',
              save_filepath = 'Min_vol_diff_graph.png',
              width_pixels = 900,
              height_pixels = 450)


