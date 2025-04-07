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
library(Rsymphony)
library(CVXR)
library(kableExtra)

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
# rm(NPV_Val)

system.time(source("Module3-1_Curve_PV01.R"))

system.time(source("Module4-1_PV_PV01.R"))



aaa <- Cons_Total_unl[c(1:100), c(1:100)] %>% kbl()
aaa %>% kable_paper()


# 기존 축약 vs 2n으로 늘린 축약 vs 비연계 비교--------------------------------------------



# (1) 기존 축약 ------------------------

#--------------------------------------------
##참가자 계좌정보(6사)
accnum_LP <- c(
    '514010000001',
    '554010000001',
    '010010000001',
    '523011234567'
)
#_--------------------------------------------



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
         rep(rep(5000000000, length(accnum_LP)), 2),
         rep(1, nrow(Mat_CP)),
         rep(0, nrow(Mat_CP)))

#LP Solve
result <- lp("max", Vec_Notional, t(Cons_Total), dir, rhs)

result_symph <- Rsymphony_solve_LP(Vec_Notional, t(Cons_Total), dir, rhs, max = TRUE)

#결과분석
str(result)

SOL2<- as.matrix(result_symph$solution)

(Vec_Notional %*% SOL) / sum(Pos_KRW_LPcomp$Notional)







# (2) 2n 축약 ------------------------

#--------------------------------------------
##참가자 계좌정보(6사)
accnum_LP <- c(
    '514010000001',
    '554010000001',
    '010010000001',
    '523011234567'
)
#--------------------------------------------



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


## 거래별 당사자 행렬(2n * m)
Mat_CP_2n <- rbind((
    Pos_KRW_LPcomp$Pay_Accnum == matrix(
        rep(accnum_LP, nrow(Pos_KRW_LPcomp)),
        ncol = length(accnum_LP),
        byrow = TRUE
    )
),
- (
    Pos_KRW_LPcomp$Rec_Accnum == matrix(
        rep(accnum_LP, nrow(Pos_KRW_LPcomp)),
        ncol = length(accnum_LP),
        byrow = TRUE
    )
))



Vec_Notional_2n <- c(Pos_KRW_LPcomp$Notional, Pos_KRW_LPcomp$Notional) 




#Constraints------------------------------------

#constraint#1(PV01, bucket delta)

Cons_PV01_2n <- Mat_CP_2n * rep(Pos_KRW_LPcomp$Up_sum, 2)
Cons_L1_1Y_2n <- Mat_CP_2n * rep(Pos_KRW_LPcomp$L1_sum_1Y, 2)
Cons_L1_5Y_2n <- Mat_CP_2n * rep(Pos_KRW_LPcomp$L1_sum_5Y, 2)
Cons_L1_10Y_2n <- Mat_CP_2n * rep(Pos_KRW_LPcomp$L1_sum_10Y, 2)
Cons_L1_20Y_2n <- Mat_CP_2n * rep(Pos_KRW_LPcomp$L1_sum_20Y, 2)
Cons_L2_5Y_2n <- Mat_CP_2n * rep(Pos_KRW_LPcomp$L2_sum_5Y, 2)
Cons_L2_20Y_2n <- Mat_CP_2n * rep(Pos_KRW_LPcomp$L2_sum_20Y, 2)

#constraint#2(CashAmount=NPV)

Cons_NPV_2n <- Mat_CP_2n * rep(Pos_KRW_LPcomp$NPV, 2)



#Total Constraints(제약식 부호가 사실은 잘못되어있음 rhs도 같이 잘못되어 해는 같음) + [X 하한] 삭제

Cons_Total_2n <- cbind(Cons_PV01_2n, -Cons_PV01_2n,
                    Cons_L1_1Y_2n, -Cons_L1_1Y_2n,
                    Cons_L1_5Y_2n, -Cons_L1_5Y_2n,
                    Cons_L1_10Y_2n, -Cons_L1_10Y_2n,
                    Cons_L1_20Y_2n, -Cons_L1_20Y_2n,
                    Cons_L2_5Y_2n, -Cons_L2_5Y_2n,
                    Cons_L2_20Y_2n, -Cons_L2_20Y_2n,
                    Cons_NPV_2n, -Cons_NPV_2n, 
                    diag(nrow(Mat_CP_2n)), 
                    rbind(diag(nrow(Mat_CP_2n)/2), -(diag(nrow(Mat_CP_2n)/2)))
)




#Unequality Signs
dir_2n <- c(rep(rep("<=", length(accnum_LP)), 16),
         rep("<=", nrow(Mat_CP_2n)),
         rep("=", nrow(Mat_CP_2n) / 2))

#Tolerance(임의 : PV01은 15mil, CashAmount는 50억원)

rhs_2n <- c(rep(rep(15000000, length(accnum_LP)), 14),
         rep(rep(5000000000, length(accnum_LP)), 2),
         rep(1, nrow(Mat_CP_2n)),
         rep(0, nrow(Mat_CP_2n) / 2)
         )

#LP Solve
result_2n <- lp("max", Vec_Notional_2n, t(Cons_Total_2n), dir_2n, rhs_2n)



#결과분석
str(result_2n)

SOL_2n <- as.matrix(result_2n$solution)
sum((SOL_2n >= 0.99))
(Vec_Notional_2n %*% SOL_2n) / (sum(Pos_KRW_LPcomp$Notional) * 2)







# (3)) unlinked 축약 ------------------------

#--------------------------------------------
##참가자 계좌정보(6사)
accnum_LP <- c(
    '514010000001',
    '554010000001',
    '010010000001',
    '523011234567'
    )
#--------------------------------------------


##거래정보 양편화

Pos_KRW_FN_PV01_Pay <- Pos_KRW_FN_PV01[, -c(12:16)] %>% mutate(Direc = 1)
Pos_KRW_FN_PV01_Rec <- Pos_KRW_FN_PV01[, -c(7:11)] %>% mutate(Direc = -1)
Pos_KRW_FN_PV01_Rec[, c(22:23, 27:47)] <- - Pos_KRW_FN_PV01_Rec[, c(22:23, 27:47)]

colnames(Pos_KRW_FN_PV01_Pay)[7:11] <- c("CPnum", "CP", "Memnum", "Member", "Accnum")
colnames(Pos_KRW_FN_PV01_Rec)[7:11] <- c("CPnum", "CP", "Memnum", "Member", "Accnum")

Pos_KRW_FN_PV01_unl <- rbind(Pos_KRW_FN_PV01_Pay, Pos_KRW_FN_PV01_Rec)


rm(Pos_KRW_FN_PV01_Pay, Pos_KRW_FN_PV01_Rec)

## 델타 레더 간략화
Pos_KRW_LPcomp_unl <- Pos_KRW_FN_PV01_unl %>%
    mutate(
        L1_sum_1Y = Up_Call + Up_CD + Up_6M + Up_9M + Up_1Y,
        L1_sum_5Y = Up_18M + Up_2Y + Up_3Y + Up_4Y + Up_5Y,
        L1_sum_10Y = Up_6Y + Up_7Y + Up_8Y + Up_9Y + Up_10Y,
        L1_sum_20Y = Up_12Y + Up_15Y + Up_20Y,
        L2_sum_5Y = L1_sum_1Y + L1_sum_5Y,
        L2_sum_20Y = L1_sum_10Y + L1_sum_20Y
    ) %>%
    filter(Accnum %in% accnum_LP)


## 거래별 당사자 행렬(2n * m)
Mat_CP_unl <- Pos_KRW_LPcomp_unl$Accnum == matrix(rep(accnum_LP, nrow(Pos_KRW_LPcomp_unl)),
                                                  ncol = length(accnum_LP),
                                                  byrow = TRUE)

Vec_Notional_unl <- Pos_KRW_LPcomp_unl$Notional 

#Constraints------------------------------------

#constraint#1(PV01, bucket delta)

Cons_PV01_unl <- Mat_CP_unl * Pos_KRW_LPcomp_unl$Up_sum
Cons_L1_1Y_unl <- Mat_CP_unl * Pos_KRW_LPcomp_unl$L1_sum_1Y
Cons_L1_5Y_unl <- Mat_CP_unl * Pos_KRW_LPcomp_unl$L1_sum_5Y
Cons_L1_10Y_unl <- Mat_CP_unl * Pos_KRW_LPcomp_unl$L1_sum_10Y
Cons_L1_20Y_unl <- Mat_CP_unl * Pos_KRW_LPcomp_unl$L1_sum_20Y
Cons_L2_5Y_unl <- Mat_CP_unl * Pos_KRW_LPcomp_unl$L2_sum_5Y
Cons_L2_20Y_unl <- Mat_CP_unl * Pos_KRW_LPcomp_unl$L2_sum_20Y

#constraint#2(Cash Amount=NPV)

Cons_NPV_unl <- Mat_CP_unl * Pos_KRW_LPcomp_unl$NPV


#CCP constraint
#!제대로 하려면 고정/변동 나눠서 1. 변동만 맞추는 경우, 고정은 현금정산 / 2. 고정,변동 각각 맞춤)!!

CF_3NM_unl <- CF_3NM %>% filter(CLnum %in% (Pos_KRW_LPcomp_unl$CLnum)) %>% select(CF_3nM_E, CLnum, PV)

Cons_CCP_1 <- dcast(CF_3NM_unl, CLnum ~ CF_3nM_E)    #!영업일 변경시 영향 고려필요!!
Cons_CCP_1[is.na(Cons_CCP_1)] <- 0

Cons_CCP_number <- ncol(Cons_CCP_1) - 1

Cons_CCP_F <- left_join(Pos_KRW_LPcomp_unl, Cons_CCP_1, by = 'CLnum')

Cons_CCP_F <-
    Cons_CCP_F %>% mutate(across(c(55:(
        54 + Cons_CCP_number
    )), function(x)
        x * Cons_CCP_F$Direc))

Cons_CCP_F <- Cons_CCP_F[, c(55:(54 + Cons_CCP_number))]


#------------------------------------검증 완료--------------------------------



#Total Constraints(제약식 부호가 사실은 잘못되어있음 rhs도 같이 잘못되어 해는 같음) + [X 하한] 삭제

Cons_Total_unl <-
    cbind(
        diag(nrow(Mat_CP_unl)),
        Cons_CCP_F, -Cons_CCP_F,
        Cons_PV01_unl, -Cons_PV01_unl,
        Cons_L1_1Y_unl, -Cons_L1_1Y_unl,
        Cons_L1_5Y_unl, -Cons_L1_5Y_unl,
        Cons_L1_10Y_unl, -Cons_L1_10Y_unl,
        Cons_L1_20Y_unl, -Cons_L1_20Y_unl,
        Cons_L2_5Y_unl, -Cons_L2_5Y_unl,
        Cons_L2_20Y_unl, -Cons_L2_20Y_unl,
        Cons_NPV_unl, -Cons_NPV_unl
    )


#Unequality Signs
dir_unl <- c(
    rep("<=", nrow(Mat_CP_unl)),
    rep("<=", Cons_CCP_number), rep("<=", Cons_CCP_number),
    rep(rep("<=", length(accnum_LP)), 16)
)

#Tolerance(임의 : PV01은 15mil, CashAmount는 50억원)

rhs_unl <- c(
    rep(1, nrow(Mat_CP_unl)),
   rep(1000000, Cons_CCP_number), rep(1000000, Cons_CCP_number),
    rep(rep(15000000, length(accnum_LP)), 14),
    rep(rep(5000000000, length(accnum_LP)), 2)
    
)

#LP Solve
result_unl <- lp("max", Vec_Notional_unl, t(Cons_Total_unl), dir_unl, rhs_unl, timeout = 60)


result_unl_sympho <- Rsymphony_solve_LP(Vec_Notional_unl, t(Cons_Total_unl), dir_unl, rhs_unl, max = T,
                                        first_feasible = TRUE)

sum(result_unl_sympho$solution)

SOL_unl <- matrix(result_unl_sympho$solution)
sum(SOL_unl==1)


Vec_Notional_unl %*% SOL_unl / sum(Pos_KRW_LPcomp_unl$Notional)

t(Cons_CCP_F) %*% SOL_unl
t(Cons_NPV_unl) %*% SOL_unl

clipr::write_clip(Pos_KRW_LPcomp_unl)
clipr::write_clip(SOL_unl)
##CVXR
# 
# Comp_X <- Variable(length(Vec_Notional_unl), integer = F)
# 
# result_CVXR <-
#     CVXR::solve(Problem(Maximize(t(Vec_Notional_unl) %*% Comp_X),
#                         list(Comp_X >= 0, Comp_X <= 1,
#                              t(Cons_NPV_unl) %*% Comp_X <= 5000000000)),
#                 
#                 num_iter = 100000)
# 
# 
# verbose = T,
# 
# result_CVXR$status
# 
# SOL <- result_CVXR$getValue(Comp_X)
# 
# installed_solvers()



#결과분석
str(result_unl)

SOL_unl <- as.matrix(result_unl$solution)

(Vec_Notional_unl %*% SOL_unl) / (sum(Pos_KRW_LPcomp$Notional) * 2)
sum((SOL_unl == 1))
t(Cons_CCP_F) %*% KKK$Comprate

