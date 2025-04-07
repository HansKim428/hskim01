# <LIBRARY>--------------------------------------------------
library(tidyverse)
library(hablar)
library(lubridate)
library(foreach)
library(doParallel)
library(parallel)
library(readxl)
library(reshape2)
library(DBI)       # DB관련 함수 사용을 위한 라이브러리
library(RSQLite)   # SQLite 사용을 위한 라이브러리
library(readr)

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


## CF별 DCF-1 : fixed leg
# FixLeg <-
#     function(Fixed_DCF,
#              CF_3nM_E,
#              CF_3nM_S
#              ) {
#         z = ifelse(Fixed_DCF == ' ACT/365.FIXED',
#                    as.integer(CF_3nM_E - CF_3nM_S) / 365,
#                    as.integer(CF_3nM_E - CF_3nM_S) / 360
#                    )
#     return(z)
#     }


## CF별 DCF-2 : floating leg
# FloLeg <-
#     function(Float_DCF,
#              CF_3nM_E,
#              CF_3nM_S
#              ) {
#         z = 
#             case_when(
#                 year(CF_3nM_E) %% 4 == 0 & year(CF_3nM_S) != year(CF_3nM_E) ~
#                     (
#                         as.integer(ymd(paste(
#                             year(CF_3nM_E), 01, 01
#                         )) - CF_3nM_S) / 365 +
#                             as.integer(CF_3nM_E - ymd(paste(
#                                 year(CF_3nM_E), 01, 01
#                             ))) / 366
#                     ),
#                 year(CF_3nM_E) %% 4 == 0 &
#                     year(CF_3nM_S) == year(CF_3nM_E) ~
#                     as.integer(CF_3nM_E - CF_3nM_S) / 366,
#                 year(CF_3nM_E) %% 4 == 1 &
#                     year(CF_3nM_S) != year(CF_3nM_E) ~
#                     (
#                         as.integer(ymd(paste(
#                             year(CF_3nM_E), 01, 01
#                         )) - CF_3nM_S) / 366 +
#                             as.integer(CF_3nM_E - ymd(paste(
#                                 year(CF_3nM_E), 01, 01
#                             ))) / 365
#                     ),
#                 TRUE ~ as.integer(CF_3nM_E - CF_3nM_S) / 365
#             )
#         return(z)
#     }
# 
# 
# 
# ifelse(Fixed_DCF == ' ACT/365.FIXED',
#        Fixed_DCF <- as.integer(CF_3nM_E - CF_3nM_S) / 365,
#        Fixed_DCF <- as.integer(CF_3nM_E - CF_3nM_S) / 360
# )
# 
# ifelse(
#     Float_DCF == 'ACT/365.FIXED',
#     Float_DCF <- as.integer(CF_3nM_E - CF_3nM_S) / 365,
#     case_when(
#         year(CF_3nM_E) %% 4 == 0 & year(CF_3nM_S) != year(CF_3nM_E) ~
#             Float_DCF <- (
#                 as.integer(ymd(paste(
#                     year(CF_3nM_E), 01, 01
#                 )) - CF_3nM_S) / 365 +
#                     as.integer(CF_3nM_E - ymd(paste(
#                         year(CF_3nM_E), 01, 01
#                     ))) / 366
#             ),
#         year(CF_3nM_E) %% 4 == 0 &
#             year(CF_3nM_S) == year(CF_3nM_E) ~
#             Float_DCF <- as.integer(CF_3nM_E - CF_3nM_S) / 366,
#         year(CF_3nM_E) %% 4 == 1 &
#             year(CF_3nM_S) != year(CF_3nM_E) ~
#             Float_DCF <- (
#                 as.integer(ymd(paste(
#                     year(CF_3nM_E), 01, 01
#                 )) - CF_3nM_S) / 366 +
#                     as.integer(CF_3nM_E - ymd(paste(
#                         year(CF_3nM_E), 01, 01
#                     ))) / 365
#             ),
#         TRUE ~ Float_DCF <- as.integer(CF_3nM_E - CF_3nM_S) / 365
#     )
#     
# )



## 알파 산출용 올림

ceiling_dec <-
    function(x, level = 1)
        round(x + 5 * 10 ^ (-level - 1), level)


# <SETTINGS>--------------------------------------------------
## [기본세팅]
options(digits = 15)
options(scipen = 999)

# 
# ## [산출일 입력]
# 
# 
# 
# ## 기간산출을 위한 부분(일별 산출시 #)
# Rate_raw_KRW <- read_csv("Rate.csv")
# BDate_init <- as.Date('2021-01-11')
# BDate_last <- as.Date('2021-11-03')
# 
# BDate_init_Index <- match(BDate_init, Rate_raw_KRW$DATE)
# BDate_last_Index <- match(BDate_last, Rate_raw_KRW$DATE)
# 
# for (n in seq(BDate_init_Index, BDate_last_Index, 3)) {
# 
#     BDate <- Rate_raw_KRW$DATE[n]
#     BDate_chr <- as.character(BDate)


##공동기금 관련 파라미터----------
ADate <- as.Date("2022-12-05")  #Stress Test시 사용할 EVT 꼬리지수(Alpha) 기준일-일반적으로 전월말


## [Stress Test 및 공동기금 산출을 위한 파라미터]
Cover_N <- 2          #최대위험 N개사
Conf_level <- 0.999   #EVT 신뢰수준
MPOR_CR <- 5          #credit ST SPOR
M <- 13               #EVT 경계선지수
q_int <- floor((1250 + 2 * M - 1) * Conf_level)                         #자연수부분
d_dec <- round_off((1250 + 2 * M - 1) * Conf_level - q_int, digit = 3)  #소수부분



##########################################
#          전체적인 사용순서             #
#  Mtm 반드시 실행하여 당일 NPV 검증 후  #
# PV01, IM, ST는 각각 실행가능(순서무관) #
##########################################

BDate <- as.Date('2024-09-13')
    
# <Mtm>----------------------------------

# Arrange Calendar
##1. 엑셀 활용시 
system.time(source("Module1_Calendar.R"))

##2. DB 활용시
# system.time(source("Module1_Calendar_DB.R"))

# elapsed 
# 19.6700000000001 

# Make Cashflow
##1. 엑셀 활용시 
system.time(source("Module2_Cashflow.R"))

##2. DB 활용시
# system.time(source("Module2_Cashflow_DB.R"))

# elapsed 
# 72.2199999999993


# Construct Mtm Curve
system.time(source("Module3_Curve_Mtm.R"))

# elapsed 
# 3.47999999999956


# Calculate PV
system.time(source("Module4_PV_Mtm.R"))

# #### Mtm Validation <R - KRX System>
NPV_Val <- data.frame(abs(Pos_KRW_FN$NPV - Pos_KRW_FN$NPV.x))
sum(NPV_Val)  #전체 거래에 대한 <청산시스템 - R>간 오차의 총합
which(NPV_Val > 100)  # 10원 이상 차이나는 거래


clipr::write_clip(Pos_KRW_FN)
clipr::write_clip(Curve_KRW)
# Pos_KRW_FN <- Pos_KRW_FN %>% mutate(Diff = NPV.x-NPV)
# 
# Diff_pay <- Pos_KRW_FN %>% group_by(Pay_Accnum) %>% summarise(sum = sum(Diff))
# Diff_rec <- Pos_KRW_FN %>% group_by(Rec_Accnum) %>% summarise(sum = sum(Diff))
# 
# 
# Diff <- full_join(Diff_pay,Diff_rec, by =c("Pay_Accnum" = "Rec_Accnum"))
# 
# Pos_KRW_FN %>% filter((abs(Diff) < 10) & (Maturity > as.Date("2025-02-28")))
# 
# 
# Diff <- Diff %>% mutate(
#     DIFF=sum.x-sum.y
# )
# 
# 
# aaa <- CF_3NM %>% filter(CLnum == "CL2023120600159")
# 
# clipr::write_clip(aaa)


# elapsed 
# 1.7399999999997817

system.time(source("Module3-2_Curve_IM.R"))

# 사용자  시스템 elapsed 
# 13.55    0.42   28.21 

# Calculate IM
system.time(source("Module4-2_PV_IM.R"))


#}

clipr::write_clip(IM)



# <PV01>---------------------------------

# Construct Delta Curve
system.time(source("Module3-1_Curve_PV01.R"))

# 사용자 시스템  elapsed
# 0.18   0.12    4.61 


# Calculate Delta
system.time(source("Module4-1_PV_PV01.R"))

# 사용자 시스템  elapsed
# 2.51   13.17   116.06 

#clipr::write_clip(Pos_KRW_FN_PV01)
#clipr::write_clip(PV01_Member)

# # for compression analysis, save Rdata
# save(Pos_KRW_FN_PV01, file = 'Pos_KRW_FN_PV01_pre.Rdata')
# save(PV01_Member, file = 'PV01_Member_pre.Rdata')
# save(Pos_KRW_FN_PV01, file = 'Pos_KRW_FN_PV01_aft.Rdata')
# save(PV01_Member, file = 'PV01_Member_aft.Rdata')


#system.time(source("Module3-2_Curve_IM_2500.R"))
#system.time(source("Module4-2_PV_IM_2500.R"))

# <Initial Margin>-----------------------

# Construct IMscn Curve
system.time(source("Module3-2_Curve_IM.R"))

# 사용자  시스템 elapsed 
# 13.55    0.42   28.21 

# Calculate IM
system.time(source("Module4-2_PV_IM.R"))
# elapsed(집)
# 889.3199999999988

# 사용자  시스템 elapsed 
# 18.88    9.59  510.49 


# # for compression analysis, save Rdata
# save(IM, file = 'IM_pre.Rdata')
# save(Pos_KRW_FN_IM, file = 'Pos_KRW_FN_IM_pre.Rdata')
# save(IM, file = 'IM_aft.Rdata')
# save(Pos_KRW_FN_IM, file = 'Pos_KRW_FN_IM_221118.Rdata')

clipr::write_clip(IM)


IM %>% 
    group_by(Member) %>% 
    sum(IM)


# <Stress Test>------------------------

# Construct ST Curve
system.time(source("Module3-3_Curve_ST.R"))

# 사용자 시스템  elapsed 
# 12.36  0.35    30.80 

# Calculate ST exposure
system.time(source("Module4-3_PV_ST.R"))


# # for compression analysis, save Rdata
# save(Pos_KRW_FN_ST, file = 'Pos_KRW_FN_ST_pre.Rdata')
# save(IM, file = 'IM_aft.Rdata')
# save(Pos_KRW_FN_ST, file = 'Pos_KRW_FN_ST_221118.Rdata')
