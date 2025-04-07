# 금리차분(5일) 산출-----------------------------------------------------------

Rate_lag <- lag(Rate_raw_KRW[, -1], 5)

Rate_lag[is.na(Rate_lag)] <- 0

Rate_diff <-
    rename(cbind(Rate_raw_KRW$DATE, round((
        Rate_raw_KRW[, -1] - Rate_lag
    ) / 100, digits = 14)), DATE = 1)

Rate_diff[1:5, -1] <- NA


# 1. Max Up & Down 시나리오 생성(22.11.18 변경, 19년 변경??)

which_BDate <- which(Rate_diff$DATE == BDate)
which_ADate <- which(Rate_diff$DATE == ADate)

#### Max_Up <- apply(Rate_diff[c(6:which_BDate), -1], 2, max)  #이론적 최대상승 차분

#### Max_Down <- apply(Rate_diff[c(6:which_BDate), -1], 2, min)  # 이론적 최대하락 차분



############## 22.11.18일전 구차분
# Max_Up <-
#     c(
#         0.0045, 0.0030, 0.002825, 0.00425, 0.0047, 0.005425, 0.0061,
#         0.0058, 0.0053, 0.0049, 0.004725, 0.0048, 0.0048, 0.00495, 0.005, 0.005, 0.00515, 0.00505)
# 
# Max_Down <-
#     c(
#         -0.0095,-0.011, -0.009575, -0.00815,-0.0071,-0.00635, -0.0064, -0.0051, -0.0046,-0.0043,
#         -0.0042,-0.0042,-0.004066, -0.003933, -0.00395, -0.00415, -0.00425, -0.00435)

############## 22.11.18일후 신차분
Max_Up <-
    c(
        0.005717, 0.0053, 0.004568, 0.004371, 0.005289, 0.006236, 0.006918, 0.007179, 0.007136,
        0.007014, 0.006696,0.00645, 0.006207, 0.006011, 0.005839, 0.0058,  0.005786, 0.006014) 

Max_Down <-
    c(
        -0.0095,-0.011, -0.009575, -0.00815,-0.0071,-0.00635, -0.0064, -0.0051, -0.0046,-0.004521,
        -0.004357,-0.004221,-0.004168, -0.004121, -0.00415,-0.004386, -0.0048, -0.004993) 




Max_Up <- Max_Up * 100 + Rate_raw_KRW[which(Rate_raw_KRW$DATE == BDate), -1]   #최대상승 시나리오

Max_Down <- Max_Down * 100 + Rate_raw_KRW[which(Rate_raw_KRW$DATE == BDate), -1]   #최대하락 시나리오



# 2. Lehman 시나리오 생성(2008-12-11)

Lehman <-
    c(
        -0.0075,
        -0.0070,
        -0.00645,
        -0.0059,
        -0.0057,
        -0.0055,
        -0.0053,
        -0.0051,
        -0.0046,
        -0.0043,
        -0.0042,
        -0.0041,
        -0.0039,
        -0.0037,
        -0.0035,
        -0.0027,
        -0.0027,
        -0.0027
    )  #실제 지침상 차분

Lehman <- Lehman * 100 + Rate_raw_KRW[which(Rate_raw_KRW$DATE == BDate), -1]  #리만 시나리오



# 3. Historical 1250 days 시나리오

Historical <- Rate_diff[c((which_BDate - 1249) : which_BDate), ]



#[OPTION1]  EVT(극단치이론) 시나리오 - 꼬리지수(알파)가 125일 최소(250일 평균)인 경우-----------------------------------

alpha_daily <- data.frame(t(c(1:18)))


##1 < 실제 > 알파 기준일(ADate)이전 영업일 간의 알파값 산출 

for (j in c((which_ADate - 126):(which_ADate - 2))) {     ###앞변수 변경(period + 1)

    Historical_EVT <- Rate_diff[c((j - 1249):(j)), ]

    Historical_ordered <- c(1:1250)

    for (i in 2:19) {
        Historical_ordered <- cbind(Historical_ordered, Historical_EVT[c(order(-Historical_EVT[, i])), i])
    }

    alpha_UP <- ceiling_dec(log(26/5) / log(Historical_ordered[1, -1] / Historical_ordered[14, -1]), level = 3)
    alpha_DOWN <- ceiling_dec(log(26/5) / log(Historical_ordered[1250, -1] / Historical_ordered[1237, -1]), level = 3)
    alpha_day <- apply(rbind(alpha_UP, alpha_DOWN), 2, min)
    alpha_daily[j - which_ADate + 127, ] <- alpha_day    ###변경(period + 2)
}


##2 < 가상 > 아래는 시뮬레이션 용임-산출기준일 -2 영업일부터가 아닌 당일부터로 룩백(원래 숫자 순서 : 251, 2, 1249...마지막 252)

# for (j in c((which_ADate - 124) : (which_ADate))) {
# 
#   Historical_EVT <- Rate_diff[c((j - 1249) : (j)), ]
# 
#   Historical_ordered <- c(1:1250)
# 
#   for (i in 2:19) {
# 
#     Historical_ordered <- cbind(Historical_ordered, Historical_EVT[c(order(-Historical_EVT[, i])), i])
#   }
# 
#   alpha_UP <- ceiling_dec(log(26/5) / log(Historical_ordered[1, -1] / Historical_ordered[14, -1]), level = 3)
#   alpha_DOWN <- ceiling_dec(log(26/5) / log(Historical_ordered[1250, -1] / Historical_ordered[1237, -1]), level = 3)
#   alpha_day <- apply(rbind(alpha_UP, alpha_DOWN), 2, min)
#   alpha_daily[j - which_ADate + 125, ] <- alpha_day
# }


## [OPTION2] 최종 적용알파 계산  mean/min---------------------
alpha <-
    round_off(as.numeric(format(sapply(alpha_daily, min), scientific = F)), 3)



## 역사적 시나리오 1250와 결합----------------------------

Historical_EVT <- Rate_diff[c((which_BDate - 1249) : (which_BDate)), ]

Historical_ordered <- c(1:1250)
for (i in 2:19) {
    Historical_ordered <- cbind(Historical_ordered, Historical_EVT[c(order(-Historical_EVT[, i])), i])
}

EVT_Up <- data.frame()

EVT_Down <- data.frame()


## up시나리오 13개, down시나리오 13개 생성

for (p in seq((1 / 2500), (25 / 2500), (2 / 2500))) {
    EVT_Up <- rbind(EVT_Up, Historical_ordered[M + 1, -1] * (M / 1250 / p) ^ (1 / alpha))
    EVT_Down <- rbind(EVT_Down, Historical_ordered[1250 - M, -1] * (M / 1250 / p) ^ (1 / alpha))
}

EVT_Up <- cbind(c('EVTup01', 'EVTup02', 'EVTup03', 'EVTup04', 'EVTup05', 'EVTup06', 'EVTup07', 'EVTup08', 'EVTup09', 'EVTup10', 'EVTup11', 'EVTup12', 'EVTup13'),
                EVT_Up)

EVT_Down <- cbind(c('EVTdn01', 'EVTdn02', 'EVTdn03', 'EVTdn04', 'EVTdn05', 'EVTdn06', 'EVTdn07', 'EVTdn08', 'EVTdn09', 'EVTdn10', 'EVTdn11', 'EVTdn12', 'EVTdn13'),
                  EVT_Down)


colnames(EVT_Up) <- colnames(Rate_raw_KRW)
colnames(EVT_Down) <- colnames(Rate_raw_KRW)
colnames(Historical_ordered) <- colnames(Rate_raw_KRW)


## 최종 시나리오 테이블 정리(1250 + 최대상승1 + 최대하락1 + 리먼 + EVTup13 + EVTdn13 + Today = 1280)

Rate_ST <- rbind(EVT_Up, Historical, EVT_Down)     #Historical Diff data real

for (i in 1:nrow(Rate_ST)) { 
    Rate_ST[i, -1] <- Rate_ST[i, -1] + Rate_raw_KRW[which(Rate_raw_KRW$DATE == BDate), -1] / 100
}

Rate_ST <- rbind(c(DATE = as.character("9999-12-31"), 
                   Rate_raw_KRW[which(Rate_raw_KRW$DATE == BDate), -1] / 100), 
                 Rate_ST)



## 고정 시나리오 합체-------------------------------
Rate_ST[1278, ] <- c('Max_Up', Max_Up / 100)
Rate_ST[1279, ] <- c('Max_Dn', Max_Down / 100)
Rate_ST[1280, ] <- c('Lehman', Lehman / 100)



Rate_ST_order <- rbind(EVT_Up, Historical_ordered, EVT_Down)     #Historical Diff data ordered

for (i in 1:nrow(Rate_ST_order)) {
    Rate_ST_order[i, -1] <-
        Rate_ST_order[i, -1] * 100 + Rate_raw_KRW[which(Rate_raw_KRW$DATE == BDate), -1]
}


Rate_ST_order <-
    rbind(c(DATE = as.character("9999-12-31"), 
            Rate_raw_KRW[which(Rate_raw_KRW$DATE == BDate), -1]),
          Rate_ST_order)




#### Remove Objects
rm(EVT_Up, EVT_Down, Historical, Historical_ordered, Historical_EVT, alpha, alpha_daily,
   alpha_day, alpha_DOWN, alpha_UP)



# ST용 시나리오별 커브생성------------------------------------------------------------------

core <- detectCores()
cluster <- makeCluster(spec = core) 
registerDoParallel(cl = cluster)

NPV_scn_ST <- foreach(scn_i = 1:nrow(Rate_ST), .packages = c('tidyverse', 'lubridate')) %dopar% {
    
    
    # 기준만기 금리자료 교체
    
    BMat <- data.frame(cbind(
        INDEX = c(0, 3, 6, 9, 12, 18, 24, 36, 48, 60,
                  72, 84, 96, 108, 120, 144, 180, 240) / 3 + 1,  #정산금리 만기>3개월 단위 지수
        RATE = as.vector(t(Rate_ST[scn_i, -1]))
    ))
    
    
    ## 선형보간을 위한 이전/이후 정산금리 교체
    Prm_3NM <- Prm_3NM %>%
        mutate(RATE_Ante = BMat[match(Prm_3NM$INDEX_Ante, BMat$INDEX), 2],
               RATE_Post = BMat[match(Prm_3NM$INDEX_Post, BMat$INDEX), 2])
    
    
    ## 선형보간(소수점 14자리)
    Prm_3NM <- Prm_3NM %>%
        mutate(RATE_Inter = ifelse(DATE_Ante == DATE_Post, 
                                   RATE_Ante, 
                                   round_off(interpol(DATE_MF, DATE_Ante, DATE_Post, 
                                                      RATE_Ante, RATE_Post),
                                             digits = 14)
        )
        ) 
    
    
    # 주요만기 무이표금리 및 할인율 찾기-------------------------------
    
    ## call 할인율-뉴머레이터-무이표
    Prm_3NM[1, c('DF', 'Numer')] <- c(1, 0)       
    
    ## CD 할인율
    Prm_3NM[2, 'DF'] <- round_off(1 / (1 + Prm_3NM[2, 'RATE_Inter'] * 
                                           as.integer(Prm_3NM[2, 'DATE_MF'] - EDate) / 365), 
                                  digits = 12)
    Prm_3NM[2, 'Numer'] <- Prm_3NM[2, 'DF'] * 
        as.integer(Prm_3NM[2, 'DATE_MF'] - EDate) / 365
    
    ## Bootstrapping
    for (i in 3:81) {
        Prm_3NM[i, 'DF'] <- round_off((1 - Prm_3NM[i, 'RATE_Inter'] * 
                                           cumsum(Prm_3NM$Numer)[i - 1]) / 
                                          (1 + Prm_3NM[i, 'RATE_Inter'] / 365 *
                                               (as.integer(Prm_3NM[i, 'DATE_MF'] - 
                                                               Prm_3NM[i - 1, 'DATE_MF']))),
                                      digits = 12)
        Prm_3NM[i, 'Numer'] <- Prm_3NM[i, 'DF'] * 
            as.integer(Prm_3NM[i, 'DATE_MF'] - 
                           Prm_3NM[i - 1, 'DATE_MF']) / 365
    }
    
    ## DF로부터 ZCR계산
    Prm_3NM <- Prm_3NM %>% mutate(ZCR = round_off(DFtoZCR(DF, DATE_MF, EDate), 14))
    Prm_3NM[1, 'ZCR'] <- Prm_3NM[1, 'RATE_Inter']  # call ZCR == 1
    
    
    
    # 모든날짜 무이표금리 및 할인율 찾기-------------------------------
    
    ## 직전/직후만기 금리 변경 및 선형보간 - ZCR, DF 생성
    
    Curve_KRW %>%
        mutate(
            RATE_Ante = Prm_3NM[match(DATE_Ante, Prm_3NM$DATE_MF), 'ZCR'],
            RATE_Post = Prm_3NM[match(DATE_Post, Prm_3NM$DATE_MF), 'ZCR'],
            ZCR = ifelse(
                DATE_Ante == DATE_Post,
                RATE_Ante,
                round_off(
                    interpol(DATE,
                             DATE_Ante,
                             DATE_Post,
                             RATE_Ante,
                             RATE_Post),
                    digits = 14
                )
            ),
            DF = round_off(ZCRtoDF(ZCR, DATE, EDate), 12)
        ) %>% 
        select(ZCR, DF)
    
}    

stopCluster(cluster) 







