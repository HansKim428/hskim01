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


## [산출기간 입력]

BDate <- as.Date("2022-05-31")
BDate_last <- as.Date('2022-06-30')

##########################################
# 영업일 변화 무관
# 포지션 만기도래 무관
# 금리시나리오만 변경
##########################################


# <Mtm>----------------------------------

system.time(source("Module1_Calendar_DB.R"))

system.time(source("Module2_Cashflow_DB.R"))

system.time(source("Module3_Curve_Mtm.R"))

system.time(source("Module4_PV_Mtm.R"))

# #### Mtm Validation <R - KRX System>
# NPV_Val <- abs(Pos_KRW_FN$NPV - Pos_KRW_FN$NPV.x)
# sum(NPV_Val)  #전체 거래에 대한 <청산시스템 - R>간 오차의 총합
# which(NPV_Val > 10)  # 10원 이상 차이나는 거래
# rm(NPV_Val)



BDate_Index <- match(BDate, Rate_raw_KRW$DATE)
BDate_last_Index <- match(BDate_last, Rate_raw_KRW$DATE)

for (n in BDate_Index:BDate_last_Index) {
    
    BDate <- Rate_raw_KRW$DATE[n]
    BDate_chr <- as.character(BDate)
    
# <Initial Margin>-----------------------

# Construct IMscn Curve
system.time(source("Module3-2_Curve_IM.R"))

# Calculate IM
system.time(source("Module4-2_PV_IM.R"))

}

