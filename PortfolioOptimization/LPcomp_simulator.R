# <LIBRARY>--------------------------------------------------
library(tidyverse)
library(hablar)
library(lubridate)
library(foreach)
library(doParallel)
library(parallel)
library(readxl)
library(reshape2)
library(lpSolve)



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
getwd()


## 알파 산출용 올림

ceiling_dec <-
    function(x, level = 1)
        round(x + 5 * 10 ^ (-level - 1), level)


# <SETTINGS>--------------------------------------------------
## [기본세팅]
options(digits = 3)
options(scipen = 999)


BDate <- as.Date("2022-12-05")

system.time(source("Module1_Calendar.R"))

system.time(source("Module2_Cashflow.R"))

system.time(source("Module3_Curve_Mtm.R"))

system.time(source("Module4_PV_Mtm.R"))

# NPV_Val <- abs(Pos_KRW_FN$NPV - Pos_KRW_FN$NPV.x)
# sum(NPV_Val)  #전체 거래에 대한 <청산시스템 - R>간 오차의 총합
# which(NPV_Val > 10)  # 10원 이상 차이나는 거래

system.time(source("Module3-1_Curve_PV01.R"))

system.time(source("Module4-1_PV_PV01.R"))





#--------------------------------------------
##참가자 계좌정보(17사 - 2,3,4사)
accnum_LP <- c(
    '012010000001',
    '071010000000',
    '513010000001',
    '010010000001',
    '554010000001',
    '558010000001',
    '523011234567',
    '506010000001',
    '002010000001',
    '519011234567',
    '535010000001',
    '520010000000',
    '006010000006',
    '017010000000'
    )




##델타 레더 간략화
Pos_KRW_LPcomp <- Pos_KRW_FN_PV01 %>%
    mutate(
        L1_sum_1Y = Up_Call + Up_CD + Up_6M + Up_9M + Up_1Y,
        L1_sum_5Y = Up_18M + Up_2Y + Up_3Y + Up_4Y + Up_5Y,
        L1_sum_10Y = Up_6Y + Up_7Y + Up_8Y + Up_9Y + Up_10Y,
        L1_sum_20Y = Up_12Y + Up_15Y + Up_20Y,
        L2_sum_5Y = L1_sum_1Y + L1_sum_5Y,
        L2_sum_20Y = L1_sum_10Y + L1_sum_20Y
    ) %>%
    filter((Pay_Accnum %in% accnum_LP) &
               (Rec_Accnum %in% accnum_LP))


## 거래별 상대방 행렬
Mat_CP <- 
    (Pos_KRW_LPcomp$Pay_Accnum == matrix(rep(accnum_LP, nrow(Pos_KRW_LPcomp)),
                                         ncol = length(accnum_LP),
                                         byrow = TRUE)) -
    (Pos_KRW_LPcomp$Rec_Accnum == matrix(rep(accnum_LP, nrow(Pos_KRW_LPcomp)),
                                         ncol = length(accnum_LP),
                                         byrow = TRUE))



Vec_Notional <- Pos_KRW_LPcomp$Notional

#Constraints------------------------------------

#constraint#1(PV01, bucket delta)

Cons_PV01 <- Mat_CP * Pos_KRW_LPcomp$Up_sum
Cons_L1_1Y <- Mat_CP * Pos_KRW_LPcomp$L1_sum_1Y
Cons_L1_5Y <- Mat_CP * Pos_KRW_LPcomp$L1_sum_5Y
Cons_L1_10Y <- Mat_CP * Pos_KRW_LPcomp$L1_sum_10Y
Cons_L1_20Y <- Mat_CP * Pos_KRW_LPcomp$L1_sum_20Y
Cons_L2_5Y <- Mat_CP * Pos_KRW_LPcomp$L2_sum_5Y
Cons_L2_20Y <- Mat_CP * Pos_KRW_LPcomp$L2_sum_20Y

#constraint#2(CashAmount=NPV)

Cons_NPV <- Mat_CP * Pos_KRW_LPcomp$NPV



#Total Constraints(제약식 부호가 사실은 잘못되어있음 rhs도 같이 잘못되어 해는 같음)

Cons_Total <- cbind(Cons_PV01, -Cons_PV01,
                    Cons_L1_1Y, -Cons_L1_1Y,
                    Cons_L1_5Y, -Cons_L1_5Y,
                    Cons_L1_10Y, -Cons_L1_10Y,
                    Cons_L1_20Y, -Cons_L1_20Y,
                    Cons_L2_5Y, -Cons_L2_5Y,
                    Cons_L2_20Y, -Cons_L2_20Y,
                    Cons_NPV, -Cons_NPV, 
                    diag(nrow(Mat_CP)), 
                    diag(nrow(Mat_CP)))

#Unequality Signs
dir <- c(rep(rep("<=", length(accnum_LP)), 16),
         rep("<=", nrow(Mat_CP)),
         rep(">=", nrow(Mat_CP)))

#Tolerance(임의 : PV01은 15mil, CashAmount는 50억원)

rhs <- c(rep(rep(15000000, length(accnum_LP)), 14),
         c(
             c(
                 9999999999999,
                 rep(5000000000, 7),
                 1,
                 5000000000,
                 5000000000,
                 1,
                 5000000000,
                 5000000000
             ),
             c(9999999999999,
               rep(5000000000, 13))
         ),
         rep(1, nrow(Mat_CP)),
         rep(0, nrow(Mat_CP)))

#LP Solve
result <- lp("max", Vec_Notional, t(Cons_Total), dir, rhs)



# ###binary solution
# 
# Cons_Total <- cbind(Cons_PV01, -Cons_PV01,
#                     Cons_L1_1Y, -Cons_L1_1Y,
#                     Cons_L1_5Y, -Cons_L1_5Y,
#                     Cons_L1_10Y, -Cons_L1_10Y,
#                     Cons_L1_20Y, -Cons_L1_20Y,
#                     Cons_L2_5Y, -Cons_L2_5Y,
#                     Cons_L2_20Y, -Cons_L2_20Y,
#                     Cons_NPV, -Cons_NPV)
# 
# #Unequality Signs
# dir <- c(rep(rep("<=", length(accnum_LP)), 16))
# 
# #Tolerance(임의 : PV01은 10mil, CashAmount는 5억원)
# 
# rhs <- c(rep(rep(10000000,length(accnum_LP)), 14),
#          rep(rep(500000000,length(accnum_LP)), 2))
# 
# #LP Solve
# result <- lp("max", Vec_Notional, t(Cons_Total), dir, rhs, all.bin = T)



sum(Pos_KRW_LPcomp$Notional)

str(result)



nrow(Pos_KRW_LPcomp) - sum(result$solution %in% c(1))


SOL <- as.matrix(result$solution)
Pos_KRW <- Pos_KRW_LPcomp[,c(1:31)]
Pos_KRW <- cbind(Pos_KRW_LPcomp[,c(1:31)], Comprate = SOL)

clipr::write_clip(SOL)
clipr::write_clip(Pos_KRW)

Pos_KRW <- Pos_KRW %>% 
    mutate(Notional = Notional * (1 - Comprate),
           NPV = NPV * (1 - Comprate)) %>% 
    filter(Notional >= 1)

Pos_KRW <- Pos_KRW[,-32]


aaa <- Pos_KRW %>% group_by(Rec_Accnum) %>% summarise(sum(NPV))
bbb <- Pos_KRW %>% group_by(Pay_Accnum) %>% summarise(sum(NPV))

clipr::write_clip(aaa)
clipr::write_clip(bbb)


t(Cons_NPV) %*% SOL
t(Cons_PV01) %*% SOL
t(-Cons_PV01) %*% SOL

t(Cons_L1_1Y) %*% SOL

Vec_Notional %*% SOL
