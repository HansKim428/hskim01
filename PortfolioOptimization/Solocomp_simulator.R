# <LIBRARY>--------------------------------------------------
library(tidyverse)
library(hablar)
library(lubridate)
library(foreach)
library(doParallel)
library(parallel)
library(readxl)
library(reshape2)


# <FUNCTION>--------------------------------------------------

## 반올림함수
round_off <- function(x, digits = 0) {
    posneg = sign(x)
    z = trunc(abs(x) * 10 ^ (digits + 1)) / 10
    z = floor(z * posneg + 0.5) / 10 ^ digits
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



# <SETTINGS>--------------------------------------------------
## [기본세팅]
options(digits = 15)
options(scipen = 999)

BDate <- as.Date("2022-06-21")


#NPV 평가방법 차이 , 커브 차이에 상관없이 축약
#roll date 한바퀴 돌릴때까지 기회 많다가 없어질 수

#동일계좌여야 증거금 안흔들림


#시뮬레이션 케이스 분류 : 연계/비연계   +  비청산명세(forward, stub, VNS) 포함----------

#################Case1. 비청산명세를 포함하지 않는 경우, 즉 만기가 완벽하게 동일한 경우
#최초CF의 이자기산일을 포함한 모든 이자계산.지급일정이 동일
#이자계산방식, 휴일처리규칙 모두 영향 있다.
#단순Solo는 이자율이 같아야하는데, 만기가 같은데 이자율이 같다는 것은 시작일도 같을 가능성 높음
#따라서 회원에 따라서는 당일에 같은 방향으로만 거래하는 경우 많아 축약 기회가 없을 수

#과연 같은 방향 거래를 하나로 합쳐 거래건수만 줄이는게 돈을 내면서 할만큼 가치가 있는가?

#새로 등록되는 거래의 거래일, 발효일, 거래상대방은 어떻게 처리?
#잔존거래는 기존 거래의 일부라고 볼 수 있는 경우도 있고(잔존명목금액이 기존보다 감소),
#새로 청산등록된다고 봐야되는 경우도 있음(잔존명목금액이 기존보다 증가)
#물론 최종 1거래가 아니라 다수거래로 명목금액을 나누면 기존 거래의 일부로만 볼 수도 있음(건수측면 축약효율 감소)
#다만, 비연계에서 할 때는 거래상대방을 누구로 봐야되냐? 문제가 생기네
#명세 문제를 해결하지 못하는 경우, 내일을 발효일로 해서 명세를 맞출 수 밖에 없다.(축약기회축소)
#이 경우에도 거래소가 독자적인 거래상대방으로 인식될 수 있는지 문제가 남고,
#정보보고와 거래확인 측면 문제도 해결해야됨.

# <0. 축약가능거래 필터링 >---------------------------------------------------
###0-1. 최초로 도래하는 이자지급일(1st CashFlow)이 축약일 익익영업일 이후인 거래


###0-2. 만기가 거의 도래한 거래 - 실익여부





# <1. Make Cashflow>
system.time(source("Module1_Calendar.R"))

system.time(source("Module2_Cashflow.R"))


###Cashflow Date 정보 생성(시작일을 MMDD 형식으로 구분)
CF_3NM <- CF_3NM %>%
    mutate(MMDD_S = format(CF_3nM_S, "%m%d")) 

###거래별로 시작일들로 이뤄진 문자열 생성
CF_futuredate <- aggregate(MMDD_S ~ CLnum, data = CF_3NM, paste, collapse = "")

###포지션 정보에 합산
Pos_KRW_comp <- left_join(Pos_KRW, CF_futuredate, by = 'CLnum')


rm(Pos_KRW, CF_3NM)


# <2. 거래명세 필터링>----------------------#추후 필요없는 컬럼 애초 삭제

##2-0. 특이명세 제외 : BDC(MF만 포함), Fixed DCF(ACT/365F만 포함), Float_DCF(ACT/365F만 포함)
Pos_KRW_comp <-
    Pos_KRW_comp %>% filter(BDC == 7 &
                            Fixed_DCF == "ACT/365.FIXED" &
                            Float_DCF == "ACT/365.FIXED")


##2-1. roll date, month종류(1,4,7,10 / 2,5,8,11 / 3,6,9,12)
Pos_KRW_comp <-
    Pos_KRW_comp %>% mutate(Roll = ifelse(EOM == "Y", "EOM", day(Edate)),
                            Month = (month(Edate) %% 3))


##2-2. Grouping ID 부여("만기-roll-고정금리")
Pos_KRW_comp <- Pos_KRW_comp %>% mutate(Group = paste0(Maturity, "-", Roll, "-", Fixed_Rate))






### 계좌별 단순 단독축약 시행-------------------------------------------
All_Acc <- data.frame(unique(c(Pos_KRW_comp$Pay_Accnum,
                    Pos_KRW_comp$Rec_Accnum)))



core <- detectCores()
cluster <- makeCluster(spec = core) 
registerDoParallel(cl = cluster)



Comp_result <- foreach(Acc_i = 1:nrow(All_Acc), .packages = c('tidyverse', 'lubridate')) %dopar% {



#계좌정보
Comp_Acc <- All_Acc[Acc_i, 1]

#해당 계좌 포지션 필터링
Pos_KRW_comp_CP <-
    Pos_KRW_comp %>% filter(Pay_Accnum == Comp_Acc |
                                Rec_Accnum == Comp_Acc)
Pos_KRW_comp_CP <-
    Pos_KRW_comp_CP %>% mutate(
        Notional_Sign = ifelse(Pay_Accnum == Comp_Acc, Notional, -Notional),
        CP = ifelse(Pay_Accnum == Comp_Acc, Rec_Accnum, Pay_Accnum)
    )


# <3. 매칭>----------------------

## <2.9 명목금액 같은 상계. filter: 발효일+만기일>---------
Simul_2.9 <-
    Pos_KRW_comp_CP %>% 
    group_by(Edate, Maturity, Fixed_Rate, EOM, Notional) %>%
    summarise('Count' = n(),
              'Notional_sum_abs' = sum(Notional),
              'Notional_sum_sign' = sum(Notional_Sign))


Simul_2.9 <- Simul_2.9 %>% mutate(
    Compeff_count = 1 - 1 / Count,
    Compeff_notional = 1 - abs(Notional_sum_sign) / Notional_sum_abs
)

#전체 축약기회
##Count
C_2.9 <- 1 - nrow(Simul_2.9) / nrow(Pos_KRW_comp_CP)
##Notional
N_2.9 <- 1 - sum(abs(Simul_2.9$Notional_sum_sign)) / sum(Pos_KRW_comp_CP$Notional)
#검증 : 1 - sum(abs(Simul_2.9$Notional_sum_sign)) / sum(Simul_2.9$Notional_sum_abs)


#$#$#$#$linked model-------------

##filter: 발효일+만기일

Simul_2.9_linked <-
    Pos_KRW_comp_CP %>% 
    group_by(Edate, Maturity, Fixed_Rate, CP, EOM, Notional) %>%
    summarise('Count' = n(),
              'Notional_sum_abs' = sum(Notional),
              'Notional_sum_sign' = sum(Notional_Sign))

Simul_2.9_linked <- Simul_2.9_linked %>% mutate(
    Compeff_count = 1 - 1 / Count,
    Compeff_notional = 1 - abs(Notional_sum_sign) / Notional_sum_abs
)

#전체 축약기회
##Count
C_2.9L <- 1 - nrow(Simul_2.9_linked) / nrow(Pos_KRW_comp_CP)
##Notional
N_2.9L <- 1 - sum(abs(Simul_2.9_linked$Notional_sum_sign)) / sum(Pos_KRW_comp_CP$Notional)
#검증 : 1 - sum(abs(Simul_2.9_linked$Notional_sum_sign)) / sum(Simul_2.9_linked$Notional_sum_abs)



## <3.0 명목금액 다른 상계. filter: 발효일+만기일>---------

Simul_3.0 <-
    Pos_KRW_comp_CP %>% 
    group_by(Edate, Maturity, Fixed_Rate, EOM) %>%
    summarise('Count' = n(),
              'Notional_sum_abs' = sum(Notional),
              'Notional_sum_sign' = sum(Notional_Sign))

Simul_3.0 <- Simul_3.0 %>% mutate(
    Compeff_count = 1 - 1 / Count,
    Compeff_notional = 1 - abs(Notional_sum_sign) / Notional_sum_abs
)

#전체 축약기회
##Count
C_3.0 <- 1 - nrow(Simul_3.0) / nrow(Pos_KRW_comp_CP)
##Notional
N_3.0 <- 1 - sum(abs(Simul_3.0$Notional_sum_sign)) / sum(Pos_KRW_comp_CP$Notional)
#검증 : 1 - sum(abs(Simul_3.0$Notional_sum_sign)) / sum(Simul_3.0$Notional_sum_abs)



#$#$#$#$linked model-------------

##filter: 발효일+만기일

Simul_3.0_linked <-
    Pos_KRW_comp_CP %>% 
    group_by(Edate, Maturity, Fixed_Rate, CP, EOM) %>%
    summarise('Count' = n(),
              'Notional_sum_abs' = sum(Notional),
              'Notional_sum_sign' = sum(Notional_Sign))

Simul_3.0_linked <- Simul_3.0_linked %>% mutate(
    Compeff_count = 1 - 1 / Count,
    Compeff_notional = 1 - abs(Notional_sum_sign) / Notional_sum_abs
)

#전체 축약기회
##Count
C_3.0L <- 1 - nrow(Simul_3.0_linked) / nrow(Pos_KRW_comp_CP)
##Notional
N_3.0L <- 1 - sum(abs(Simul_3.0_linked$Notional_sum_sign)) / sum(Pos_KRW_comp_CP$Notional)
#검증 : 1 - sum(abs(Simul_3.0_linked$Notional_sum_sign)) / sum(Simul_3.0_linked$Notional_sum_abs)




## <3.1 상계+거래일 상이>----------------
### <3.1.0 filter: Roll>---------

Simul_3.1.0 <-
    Pos_KRW_comp_CP %>% 
    group_by(Roll, Month, Maturity, Fixed_Rate) %>%
    summarise('Count' = n(),
              'Notional_sum_abs' = sum(Notional),
              'Notional_sum_sign' = sum(Notional_Sign))

Simul_3.1.0 <- Simul_3.1.0 %>% mutate(
    Compeff_count = 1 - 1 / Count,
    Compeff_notional = 1 - abs(Notional_sum_sign) / Notional_sum_abs
)

#전체 축약기회
##Count
C_3.1.0 <- 1 - nrow(Simul_3.1.0) / nrow(Pos_KRW_comp_CP)
##Notional
N_3.1.0 <- 1 - sum(abs(Simul_3.1.0$Notional_sum_sign)) / sum(Pos_KRW_comp_CP$Notional)
#검증 : 1 - sum(abs(Simul_3.1.0$Notional_sum_sign)) / sum(Simul_3.1.0$Notional_sum_abs)


#$#$#$#$linked model-------------

##filter: Roll

Simul_3.1.0_linked <-
    Pos_KRW_comp_CP %>% 
    group_by(Group, CP) %>%
    summarise('Count' = n(),
              'Notional_sum_abs' = sum(Notional),
              'Notional_sum_sign' = sum(Notional_Sign))

Simul_3.1.0_linked <- Simul_3.1.0_linked %>% mutate(
    Compeff_count = 1 - 1 / Count,
    Compeff_notional = 1 - abs(Notional_sum_sign) / Notional_sum_abs
)

#전체 축약기회
##Count
C_3.1.0L <- 1 - nrow(Simul_3.1.0_linked) / nrow(Pos_KRW_comp_CP)
##Notional
N_3.1.0L <- 1 - sum(abs(Simul_3.1.0_linked$Notional_sum_sign)) / sum(Pos_KRW_comp_CP$Notional)
#검증 : 1 - sum(abs(Simul_3.1.0_linked$Notional_sum_sign)) / sum(Simul_3.1.0_linked$Notional_sum_abs)



### <3.1.1 filter: real Cashflow>---------

Simul_3.1.1 <-
    Pos_KRW_comp_CP %>% 
    group_by(MMDD_S, Fixed_Rate) %>%
    summarise('Count' = n(),
              'Notional_sum_abs' = sum(Notional),
              'Notional_sum_sign' = sum(Notional_Sign))

Simul_3.1.1 <- Simul_3.1.1 %>% mutate(
    Compeff_count = 1 - 1 / Count,
    Compeff_notional = 1 - abs(Notional_sum_sign) / Notional_sum_abs
)

#전체 축약기회
##Count
C_3.1.1 <- 1 - nrow(Simul_3.1.1) / nrow(Pos_KRW_comp_CP)
##Notional
N_3.1.1 <- 1 - sum(abs(Simul_3.1.1$Notional_sum_sign)) / sum(Pos_KRW_comp_CP$Notional)
#검증 : 1 - sum(abs(Simul_3.1.1$Notional_sum_sign)) / sum(Simul_3.1.1$Notional_sum_abs)


#$#$#$#$linked model-------------

##filter: real CF

Simul_3.1.1_linked <-
    Pos_KRW_comp_CP %>% 
    group_by(MMDD_S, Fixed_Rate, CP) %>%
    summarise('Count' = n(),
              'Notional_sum_abs' = sum(Notional),
              'Notional_sum_sign' = sum(Notional_Sign))

Simul_3.1.1_linked <- Simul_3.1.1_linked %>% mutate(
    Compeff_count = 1 - 1 / Count,
    Compeff_notional = 1 - abs(Notional_sum_sign) / Notional_sum_abs
)

#전체 축약기회
##Count
C_3.1.1L <- 1 - nrow(Simul_3.1.1_linked) / nrow(Pos_KRW_comp_CP)
##Notional
N_3.1.1L <- 1 - sum(abs(Simul_3.1.1_linked$Notional_sum_sign)) / sum(Pos_KRW_comp_CP$Notional)
#검증 : 1 - sum(abs(Simul_3.1.0_linked$Notional_sum_sign)) / sum(Simul_3.1.0_linked$Notional_sum_abs)


c(C_2.9,
N_2.9,
C_2.9L,
N_2.9L,
C_3.0,
N_3.0,
C_3.0L,
N_3.0L,
C_3.1.0,
N_3.1.0,
C_3.1.0L,
N_3.1.0L,
C_3.1.1,
N_3.1.1,
C_3.1.1L,
N_3.1.1L)


}

stopCluster(cluster)


Comp_result_table <- data.frame(do.call(cbind, Comp_result))
colnames(Comp_result_table) <- t(All_Acc)

# clipr::write_clip(Comp_result_table)
# clipr::write_clip(All_Acc)






# < 이자율 조정 축약 > ---------------------------------------------------------


### 계좌별 이자율 조정 축약 시행-------------------------------------------
All_Acc <- data.frame(unique(c(Pos_KRW_comp$Pay_Accnum,
                               Pos_KRW_comp$Rec_Accnum)))


core <- detectCores()
cluster <- makeCluster(spec = core) 
registerDoParallel(cl = cluster)


Comp_result <- foreach(Acc_i = 1:nrow(All_Acc), .packages = c('tidyverse', 'lubridate')) %dopar% {
    
    
    
    #계좌정보
    Comp_Acc <- All_Acc[Acc_i, 1]
    # Comp_Acc <- '025010000001'
    #해당 계좌 포지션 필터링
    Pos_KRW_comp_CP <-
        Pos_KRW_comp %>% filter(Pay_Accnum == Comp_Acc |
                                    Rec_Accnum == Comp_Acc)
    Pos_KRW_comp_CP <-
        Pos_KRW_comp_CP %>% mutate(
            Notional_Sign = ifelse(Pay_Accnum == Comp_Acc, Notional, -Notional),
            CP = ifelse(Pay_Accnum == Comp_Acc, Rec_Accnum, Pay_Accnum)
        )
    





## <3.2 이자율조정, RR 1개>---------
### <3.2.0.0 filter: Roll>---------

Simul_3.2.0.0 <-
    Pos_KRW_comp_CP %>% 
    group_by(Roll, Month, Maturity) %>%
    summarise('Count' = n(),
              'Notional_sum_abs' = sum(Notional),
              'Notional_sum_sign' = sum(Notional_Sign),
              'CF_Annual' = sum(Notional_Sign * Fixed_Rate)
    )

## RR거래의 고정금리 결정(net notional의 유지)

Simul_3.2.0.0 <- Simul_3.2.0.0 %>%
    mutate(NewRate = CF_Annual / Notional_sum_sign)

##문제1 순명목금액이 0인 경우, New Rate은 무한대임
## -> 사실상 고정이자만 계속 나가는 상황임. 스왑으로 대체할 수는 없고,
## -> NPV로 정산하거나(결국 모두 축약)
## -> 그대로 두는 것인데, 한 거래만 제외하여 축약하는게 나을듯

##문제2 순명목금액이 매우 작아지는 경우를 허용한다면, 금리가 매우 커짐(예:52%)
## -> 불가능하진 않지만 부자연스러움

##----1과 2를 종합하면, 순명목금액이 일정수준 미만일 때 거래하나를 제외하고 자연스럽게 축약하는 방법 생각 가능


##문제3 고정금리 음수인 경우 발생
## -> ISDA 정의상 가능한가? 가능하니까 LCH랑 EUREX가 하겠지
## -> 다만, 시스템 제약, 부자연스러움 때문에 선호하지 않을 수도

##문제4 순 CF가 0인 경우도 발생
## -> net notional is 0 -> new rate == NaN : Full netting
## -> what if net notional is not 0? -> new rate == 0

Simul_3.2.0.0 <- Simul_3.2.0.0 %>% mutate(
    Compeff_count = 1 - 1 / Count,
    Compeff_notional = 1 - abs(Notional_sum_sign) / Notional_sum_abs
)

#전체 축약기회
##Count
C_3.2.0.0 <- 1 - nrow(Simul_3.2.0.0) / nrow(Pos_KRW_comp_CP)
##Notional
N_3.2.0.0 <- 1 - sum(abs(Simul_3.2.0.0$Notional_sum_sign)) / sum(Pos_KRW_comp_CP$Notional)
#검증 : 1 - sum(abs(Simul_3.2.0.0$Notional_sum_sign)) / sum(Simul_3.2.0.0$Notional_sum_abs)



#$#$#$#$linked model-------------

##filter: Roll

Simul_3.2.0.0_linked <-
    Pos_KRW_comp_CP %>% 
    group_by(Roll, Month, Maturity, CP) %>%
    summarise('Count' = n(),
              'Notional_sum_abs' = sum(Notional),
              'Notional_sum_sign' = sum(Notional_Sign),
              'CF_Annual' = sum(Notional_Sign * Fixed_Rate)
    )


Simul_3.2.0.0_linked <- Simul_3.2.0.0_linked %>%
    mutate(NewRate = CF_Annual / Notional_sum_sign)


Simul_3.2.0.0_linked <- Simul_3.2.0.0_linked %>% mutate(
    Compeff_count = 1 - 1 / Count,
    Compeff_notional = 1 - abs(Notional_sum_sign) / Notional_sum_abs
)

#전체 축약기회
##Count
C_3.2.0.0L <- 1 - nrow(Simul_3.2.0.0_linked) / nrow(Pos_KRW_comp_CP)
##Notional
N_3.2.0.0L <- 1 - sum(abs(Simul_3.2.0.0_linked$Notional_sum_sign)) / sum(Pos_KRW_comp_CP$Notional)
#검증 : 1 - sum(abs(Simul_3.2.0.0_linked$Notional_sum_sign)) / sum(Simul_3.2.0.0_linked$Notional_sum_abs)



# 
# 
# ### <3.2.0.1 순명목 0일때, 축약 미수행 filter: Roll>---------
# 
# #전체 축약기회
# ###살아있는 거래 : 
# Revival_3.2.0.0 <- Simul_3.2.0.0 %>% filter(Notional_sum_sign == 0) %>% select(Count, Notional_sum_abs)
# 
# ##Count
# C3.2.0.1 <- 1 - (nrow(Simul_3.2.0.0) + sum(Revival_3.2.0.0$Count)) / nrow(Pos_KRW_comp_CP)
# ##Notional
# N3.2.0.1 <- 1 - (sum(abs(Simul_3.2.0.0$Notional_sum_sign)) + sum(Revival_3.2.0.0$Notional_sum_abs)) / sum(Pos_KRW_comp_CP$Notional)
# 
# #$#$#$#$linked model-------------
# 
# Revival_3.2.0.0L <- Simul_3.2.0.0_linked %>% filter(Notional_sum_sign == 0) %>% select(Count, Notional_sum_abs)
# 
# ##Count
# C3.2.0.1L <- 1 - (nrow(Simul_3.2.0.0_linked) + sum(Revival_3.2.0.0L$Count)) / nrow(Pos_KRW_comp_CP)
# ##Notional
# N3.2.0.1L <- 1 - (sum(abs(Simul_3.2.0.0_linked$Notional_sum_sign)) + sum(Revival_3.2.0.0L$Notional_sum_abs)) / sum(Pos_KRW_comp_CP$Notional)
# 





### <3.2.1.0 filter: real Cashflow>---------

Simul_3.2.1.0 <-
    Pos_KRW_comp_CP %>%
    group_by(MMDD_S) %>%
    summarise(
        'Count' = n(),
        'Notional_sum_abs' = sum(Notional),
        'Notional_sum_sign' = sum(Notional_Sign),
        'CF_Annual' = sum(Notional_Sign * Fixed_Rate)
    )

Simul_3.2.1.0 <- Simul_3.2.1.0 %>%
    mutate(NewRate = CF_Annual / Notional_sum_sign)


Simul_3.2.1.0 <- Simul_3.2.1.0 %>% mutate(
    Compeff_count = 1 - 1 / Count,
    Compeff_notional = 1 - abs(Notional_sum_sign) / Notional_sum_abs
)

#전체 축약기회
##Count
C_3.2.1.0 <- 1 - nrow(Simul_3.2.1.0) / nrow(Pos_KRW_comp_CP)
##Notional
N_3.2.1.0 <- 1 - sum(abs(Simul_3.2.1.0$Notional_sum_sign)) / sum(Pos_KRW_comp_CP$Notional)
#검증 : 1 - sum(abs(Simul_3.2.1.0$Notional_sum_sign)) / sum(Simul_3.2.1.0$Notional_sum_abs)




#$#$#$#$linked model-------------

##filter: reah Cashflow

Simul_3.2.1.0_linked <-
    Pos_KRW_comp_CP %>% 
    group_by(MMDD_S, CP) %>%
    summarise('Count' = n(),
              'Notional_sum_abs' = sum(Notional),
              'Notional_sum_sign' = sum(Notional_Sign),
              'CF_Annual' = sum(Notional_Sign * Fixed_Rate)
    )


Simul_3.2.1.0_linked <- Simul_3.2.1.0_linked %>%
    mutate(NewRate = CF_Annual / Notional_sum_sign)


Simul_3.2.1.0_linked <- Simul_3.2.1.0_linked %>% mutate(
    Compeff_count = 1 - 1 / Count,
    Compeff_notional = 1 - abs(Notional_sum_sign) / Notional_sum_abs
)

#전체 축약기회
##Count
C_3.2.1.0L <- 1 - nrow(Simul_3.2.1.0_linked) / nrow(Pos_KRW_comp_CP)
##Notional
N_3.2.1.0L <- 1 - sum(abs(Simul_3.2.1.0_linked$Notional_sum_sign)) / sum(Pos_KRW_comp_CP$Notional)
#검증 : 1 - sum(abs(Simul_3.2.1.0_linked$Notional_sum_sign)) / sum(Simul_3.2.1.0_linked$Notional_sum_abs)




# 
# 
# ### <3.2.1.1 순명목 0일때, 축약 미수행 filter: real>---------
# 
# #전체 축약기회
# ###살아있는 거래 : 
# Revival_3.2.1.0 <- Simul_3.2.1.0 %>% filter(Notional_sum_sign == 0) %>% select(Count, Notional_sum_abs)
# 
# ##Count
# C3.2.1.1 <- 1 - (nrow(Simul_3.2.1.0) + sum(Revival_3.2.1.0$Count)) / nrow(Pos_KRW_comp_CP)
# ##Notional
# N3.2.1.1 <- 1 - (sum(abs(Simul_3.2.1.0$Notional_sum_sign)) + sum(Revival_3.2.1.0$Notional_sum_abs)) / sum(Pos_KRW_comp_CP$Notional)
# 
# #$#$#$#$linked model-------------
# 
# Revival_3.2.1.0L <- Simul_3.2.1.0_linked %>% filter(Notional_sum_sign == 0) %>% select(Count, Notional_sum_abs)
# 
# ##Count
# C3.2.1.1L <- 1 - (nrow(Simul_3.2.1.0_linked) + sum(Revival_3.2.1.0L$Count)) / nrow(Pos_KRW_comp_CP)
# ##Notional
# N3.2.1.1L <- 1 - (sum(abs(Simul_3.2.1.0_linked$Notional_sum_sign)) + sum(Revival_3.2.1.0L$Notional_sum_abs)) / sum(Pos_KRW_comp_CP$Notional)
# 





c(C_3.2.0.0,
  N_3.2.0.0,
  C_3.2.0.0L,
  N_3.2.0.0L,
  C_3.2.1.0,
  N_3.2.1.0,
  C_3.2.1.0L,
  N_3.2.1.0L
  )


}

stopCluster(cluster)


Comp_result_table <- data.frame(do.call(cbind, Comp_result))
colnames(Comp_result_table) <- t(All_Acc)

# clipr::write_clip(Comp_result_table)
# clipr::write_clip(All_Acc)






## <3.3 이자율조정, RR 2개>---------


### 계좌별 이자율 조정 축약 시행-------------------------------------------
All_Acc <- data.frame(unique(c(Pos_KRW_comp$Pay_Accnum,
                               Pos_KRW_comp$Rec_Accnum)))


core <- detectCores()
cluster <- makeCluster(spec = core) 
registerDoParallel(cl = cluster)


Comp_result <- foreach(Acc_i = 1:nrow(All_Acc), .packages = c('tidyverse', 'lubridate')) %dopar% {
    
    
    
    #계좌정보
    Comp_Acc <- All_Acc[Acc_i, 1]


#해당 계좌 포지션 필터링
Pos_KRW_comp_CP <-
    Pos_KRW_comp %>% filter(Pay_Accnum == Comp_Acc |
                                Rec_Accnum == Comp_Acc)
Pos_KRW_comp_CP <-
    Pos_KRW_comp_CP %>% mutate(
        Notional_Sign = ifelse(Pay_Accnum == Comp_Acc, Notional, -Notional),
        CP = ifelse(Pay_Accnum == Comp_Acc, Rec_Accnum, Pay_Accnum)
    )



### <3.3.0 filter: Roll>---------

#### 3.3.0.1  PF내 Rmax Rmin 방식----------

Simul_3.3.0.1 <-
    Pos_KRW_comp_CP %>% 
    group_by(Roll, Month, Maturity) %>%
    summarise('Count' = n(),
              'Notional_sum_abs' = sum(Notional),
              'Notional_sum_sign' = sum(Notional_Sign),
              'CF_Annual' = sum(Notional_Sign * Fixed_Rate),
              'Rate_min' = min(Fixed_Rate),
              'Rate_max' = max(Fixed_Rate),
              'Notional_weighted' = sum(Fixed_Rate * Notional_Sign)
    )



## RR거래의 명목금액 결정(net notional의 유지)
### R1<=Rmax, R2>=Rmin으로 명목금액 제약을 두고 시작할 수


Simul_3.3.0.1 <- Simul_3.3.0.1 %>%
    #filter(Rate_min != Rate_max) %>%   # 금리 동일한 포지션 필터?
    mutate(NewNA1 = (Notional_weighted - Notional_sum_sign * Rate_min) 
           / (Rate_max - Rate_min),
           NewNA2 = (Notional_sum_sign - NewNA1),
           Notional_sum_abs_new = round(abs(NewNA1) + abs(NewNA2)),
           NA_reduced = ifelse(Notional_sum_abs_new < Notional_sum_abs, 1, 
                               ifelse(Notional_sum_abs_new == Notional_sum_abs, 0, -1))
    )

Simul_3.3.0.1$NA_reduced[is.na(Simul_3.3.0.1$NA_reduced) == T] <- -1

#net notional을 유지하면서 gross notional을 최소화 하기 위해서는,
#NA1의 절댓값을 줄여야되고, 이는 R1을 최대한 늘리거나 R2를 줄여서 달성가능


##문제1. Rate_max와 Rate_min이 같은 경우 - 무한 또는 적용불가
##blended rate 적용할 필요가 없으므로 제외

##문제2. 총 명목NA가 줄어들지 않는 경우 있음(NN=GN)


##문제3. R1<=Rmax, R2>=Rmin으로 명목금액 제약이 적절한가
##-> R1은 Pf의 가중평균 금리, 어떠한 최댓값, 시가 등으로 설정될 수 있음
### R1의 민감도는 전체 참가자의 청산포트폴리오의 최댓값 정도로만 해도 최대치 달성하며
### 30, 50% 늘어나더라도 큰 영향 없음

##-> R2는 0 또는 가중평균 금리, 어떠한 최솟값 등으로 설정될 수 있음
## 0으로 하면 거의 최대치임

##아무리 R1을 늘리거나 R2를 줄여도 어느 정도 이상으로는 효율이 안나옴
##산식상 축약후 명목 합은 제일 줄었을 때가 순명목임. 축약전 명목 합(지급수취 네팅이 없다면 순명목과 같음)이 순명목과 같으면 안줄어듦.
##얘네는 결국 축약에서 배제해야. 잘해봤자 건수만 줄어듦



#전체 축약기회
notComp <- Simul_3.3.0.1 %>% filter(NA_reduced == -1) 
Comp <- Simul_3.3.0.1 %>% filter(NA_reduced != -1) 
##Count
C_3.3.0.1 <- 1 - (nrow(Comp) + sum(notComp$Count)) / nrow(Pos_KRW_comp_CP)
##Notional
N_3.3.0.1 <- 1 - (sum(Comp$Notional_sum_abs_new) + sum(notComp$Notional_sum_abs)) / sum(Pos_KRW_comp_CP$Notional)





#$#$#$#$linked model-------------

##filter: Roll

Simul_3.3.0.1_linked <-
    Pos_KRW_comp_CP %>% 
    group_by(Roll, Month, Maturity, CP) %>%
    summarise('Count' = n(),
              'Notional_sum_abs' = sum(Notional),
              'Notional_sum_sign' = sum(Notional_Sign),
              'CF_Annual' = sum(Notional_Sign * Fixed_Rate),
              'Rate_min' = min(Fixed_Rate),
              'Rate_max' = max(Fixed_Rate),
              'Notional_weighted' = sum(Fixed_Rate * Notional_Sign)
    )


Simul_3.3.0.1_linked <- Simul_3.3.0.1_linked %>%
    mutate(NewNA1 = (Notional_weighted - Notional_sum_sign * Rate_min) 
           / (Rate_max - Rate_min),
           NewNA2 = (Notional_sum_sign - NewNA1),
           Notional_sum_abs_new = round(abs(NewNA1) + abs(NewNA2)),
           NA_reduced = ifelse(Notional_sum_abs_new < Notional_sum_abs, 1, 
                               ifelse(Notional_sum_abs_new == Notional_sum_abs, 0, -1))
    )


Simul_3.3.0.1_linked$NA_reduced[is.na(Simul_3.3.0.1_linked$NA_reduced) == T] <- -1




#전체 축약기회
notComp_linked <- Simul_3.3.0.1_linked %>% filter(NA_reduced == -1) 
Comp_linked <- Simul_3.3.0.1_linked %>% filter(NA_reduced != -1) 
##Count
C_3.3.0.1L <- 1 - (nrow(Comp_linked) + sum(notComp_linked$Count)) / nrow(Pos_KRW_comp_CP)
##Notional
N_3.3.0.1L <-  1 - (sum(Comp_linked$Notional_sum_abs_new) + sum(notComp_linked$Notional_sum_abs)) / sum(Pos_KRW_comp_CP$Notional)





#### 3.3.0.2  10%, 1% 방식----------

Simul_3.3.0.2 <-
    Pos_KRW_comp_CP %>% 
    group_by(Roll, Month, Maturity) %>%
    summarise('Count' = n(),
              'Notional_sum_abs' = sum(Notional),
              'Notional_sum_sign' = sum(Notional_Sign),
              'CF_Annual' = sum(Notional_Sign * Fixed_Rate),
              'Rate_min' = 1,
              'Rate_max' = 10,
              'Notional_weighted' = sum(Fixed_Rate * Notional_Sign)
    )



## RR거래의 명목금액 결정(net notional의 유지)
### R1<=Rmax, R2>=Rmin으로 명목금액 제약을 두고 시작할 수


Simul_3.3.0.2 <- Simul_3.3.0.2 %>%
    #filter(Rate_min != Rate_max) %>%   # 금리 동일한 포지션 필터?
    mutate(NewNA1 = (Notional_weighted - Notional_sum_sign * Rate_min) 
           / (Rate_max - Rate_min),
           NewNA2 = (Notional_sum_sign - NewNA1),
           Notional_sum_abs_new = round(abs(NewNA1) + abs(NewNA2)),
           NA_reduced = ifelse(Notional_sum_abs_new < Notional_sum_abs, 1, 
                               ifelse(Notional_sum_abs_new == Notional_sum_abs, 0, -1))
    )

Simul_3.3.0.2$NA_reduced[is.na(Simul_3.3.0.2$NA_reduced) == T] <- -1

#net notional을 유지하면서 gross notional을 최소화 하기 위해서는,
#NA1의 절댓값을 줄여야되고, 이는 R1을 최대한 늘리거나 R2를 줄여서 달성가능


##문제1. Rate_max와 Rate_min이 같은 경우 - 무한 또는 적용불가
##blended rate 적용할 필요가 없으므로 제외

##문제2. 총 명목NA가 줄어들지 않는 경우 있음(NN=GN)


##문제3. R1<=Rmax, R2>=Rmin으로 명목금액 제약이 적절한가
##-> R1은 Pf의 가중평균 금리, 어떠한 최댓값, 시가 등으로 설정될 수 있음
### R1의 민감도는 전체 참가자의 청산포트폴리오의 최댓값 정도로만 해도 최대치 달성하며
### 30, 50% 늘어나더라도 큰 영향 없음

##-> R2는 0 또는 가중평균 금리, 어떠한 최솟값 등으로 설정될 수 있음
## 0으로 하면 거의 최대치임

##아무리 R1을 늘리거나 R2를 줄여도 어느 정도 이상으로는 효율이 안나옴
##산식상 축약후 명목 합은 제일 줄었을 때가 순명목임. 축약전 명목 합(지급수취 네팅이 없다면 순명목과 같음)이 순명목과 같으면 안줄어듦.
##얘네는 결국 축약에서 배제해야. 잘해봤자 건수만 줄어듦



#전체 축약기회
notComp <- Simul_3.3.0.2 %>% filter(NA_reduced == -1) 
Comp <- Simul_3.3.0.2 %>% filter(NA_reduced != -1) 
##Count
C_3.3.0.2 <- 1 - (nrow(Comp) + sum(notComp$Count)) / nrow(Pos_KRW_comp_CP)
##Notional
N_3.3.0.2 <- 1 - (sum(Comp$Notional_sum_abs_new) + sum(notComp$Notional_sum_abs)) / sum(Pos_KRW_comp_CP$Notional)





#$#$#$#$linked model-------------

##filter: Roll

Simul_3.3.0.2_linked <-
    Pos_KRW_comp_CP %>% 
    group_by(Roll, Month, Maturity, CP) %>%
    summarise('Count' = n(),
              'Notional_sum_abs' = sum(Notional),
              'Notional_sum_sign' = sum(Notional_Sign),
              'CF_Annual' = sum(Notional_Sign * Fixed_Rate),
              'Rate_min' = 1,
              'Rate_max' = 10,
              'Notional_weighted' = sum(Fixed_Rate * Notional_Sign)
    )


Simul_3.3.0.2_linked <- Simul_3.3.0.2_linked %>%
    mutate(NewNA1 = (Notional_weighted - Notional_sum_sign * Rate_min) 
           / (Rate_max - Rate_min),
           NewNA2 = (Notional_sum_sign - NewNA1),
           Notional_sum_abs_new = round(abs(NewNA1) + abs(NewNA2)),
           NA_reduced = ifelse(Notional_sum_abs_new < Notional_sum_abs, 1, 
                               ifelse(Notional_sum_abs_new == Notional_sum_abs, 0, -1))
    )


Simul_3.3.0.2_linked$NA_reduced[is.na(Simul_3.3.0.2_linked$NA_reduced) == T] <- -1




#전체 축약기회
notComp_linked <- Simul_3.3.0.2_linked %>% filter(NA_reduced == -1) 
Comp_linked <- Simul_3.3.0.2_linked %>% filter(NA_reduced != -1) 
##Count
C_3.3.0.2L <- 1 - (nrow(Comp_linked) + sum(notComp_linked$Count)) / nrow(Pos_KRW_comp_CP)
##Notional
N_3.3.0.2L <-  1 - (sum(Comp_linked$Notional_sum_abs_new) + sum(notComp_linked$Notional_sum_abs)) / sum(Pos_KRW_comp_CP$Notional)




c(C_3.3.0.1,
  N_3.3.0.1,
  C_3.3.0.1L,
  N_3.3.0.1L,
  C_3.3.0.2,
  N_3.3.0.2,
  C_3.3.0.2L,
  N_3.3.0.2L
)


}

stopCluster(cluster)


Comp_result_table <- data.frame(do.call(cbind, Comp_result))
colnames(Comp_result_table) <- t(All_Acc)

clipr::write_clip(Comp_result_table)
# clipr::write_clip(All_Acc)

