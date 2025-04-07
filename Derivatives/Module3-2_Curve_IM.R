# 금리차분(5일) 산출-----------------------------------------------------------

Rate_lag <- lag(Rate_raw_KRW[, -1], 5)

Rate_lag[is.na(Rate_lag)] <- 0

Rate_diff <-
    rename(cbind(Rate_raw_KRW$DATE, round_off((
        Rate_raw_KRW[, -1] - Rate_lag
    ) / 100, digits = 14)), DATE = 1)

Rate_diff[1:5, -1] <- NA


# EWMA 변동성 추정-----------------------------------------------------------
 
# [람다값]
Lambda <- 0.97  #EWMA decay factor


## 산출일 Index
Rate_index <- match(BDate, Rate_diff$DATE)

## 과거 5년 EWMA Table
EWMA <-
    data.frame(
        DATE = Rate_diff[c((Rate_index - 1250):Rate_index), 1],
        Call = 0,
        CD = 0,
        IRS_6M = 0,
        IRS_9M = 0,
        IRS_1Y = 0,
        IRS_18M = 0,
        IRS_2Y = 0,
        IRS_3Y = 0,
        IRS_4Y = 0,
        IRS_5Y = 0,
        IRS_6Y = 0,
        IRS_7Y = 0,
        IRS_8Y = 0,
        IRS_9Y = 0,
        IRS_10Y = 0,
        IRS_12Y = 0,
        IRS_15Y = 0,
        IRS_20Y = 0
    )

## 만기별 초기변동성 설정(10년) - 차분 제곱의 평균의 제곱근
for (i in 2:19) {
    EWMA[1, i] <- ifelse(Rate_index >= 2505,
                         (sum(Rate_diff[c((Rate_index - 2499):(Rate_index - 1250)),
                                        i] ^ 2) / 1250) ^ (1 / 2),
                         (sum(Rate_diff[c(6:(Rate_index - 1250)), 
                                        i] ^ 2) / (Rate_index - 1255)) ^ (1 / 2))
}

## T일부터 T-1249일까지의 EWMA
for (j in 2:nrow(EWMA)) {
    EWMA[j, c(2:19)] <-
        (EWMA[j - 1, c(2:19)] ^ 2 * Lambda + 
             Rate_diff[match(EWMA[j, 1], Rate_diff$DATE), c(2:19)] ^ 2 *
             (1 - Lambda)
         ) ^ (1 / 2)
}

## 최소변동성 적용
vol_min <- c()

for (i in 2:19) {
    vol_min[i] <- ifelse(Rate_index >= 2505,
                         (sum(Rate_diff[c((Rate_index - 2499):(Rate_index)), i] ^ 2) / 2500) ^ (1 / 2),
                         (sum(Rate_diff[c(6:Rate_index), i] ^ 2) / (Rate_index - 5)) ^ (1 / 2))
}



## 최종변동성 = max(최소변동성, 당일변동성)
vol_fn <- apply(rbind(EWMA[nrow(EWMA), c(2:19)], vol_min[c(2:19)]), 2, max)


# [OPTION] 최소변동성 미적용----------------------------
# vol_fn <- EWMA[nrow(EWMA), c(2:19)]

# 변동성 스케일링----------------------------------------------------------- 

## 과거 5년 Rate_IMscn scenario table
Rate_IMscn <- data.frame(DATE = Rate_diff[c((Rate_index - 1249):Rate_index), 1],
                       Call = 0,
                       CD = 0,
                       IRS_6M = 0,
                       IRS_9M = 0,
                       IRS_1Y = 0,
                       IRS_18M = 0,
                       IRS_2Y = 0,
                       IRS_3Y = 0,
                       IRS_4Y = 0,
                       IRS_5Y = 0,
                       IRS_6Y = 0,
                       IRS_7Y = 0,
                       IRS_8Y = 0,
                       IRS_9Y = 0,
                       IRS_10Y = 0,
                       IRS_12Y = 0,
                       IRS_15Y = 0,
                       IRS_20Y = 0)



## 당일 정산금리 + 차분 시나리오 = 증거금 시나리오
Rate_BDate <- Rate_raw_KRW[match(BDate, Rate_raw_KRW$DATE), -1] / 100

for (j in 1:nrow(Rate_IMscn)) {
    Rate_IMscn[j, c(2:19)] <-
        vol_fn * Rate_diff[match(Rate_IMscn[j, 1], Rate_diff$DATE), c(2:19)] /
        EWMA[match(Rate_IMscn[j, 1], EWMA$DATE), c(2:19)] +
        Rate_BDate
}

## 필터링 미적용 당일 금리 추가
Rate_IMscn <- rbind(Rate_raw_KRW[match(BDate, Rate_raw_KRW$DATE), ], Rate_IMscn)
Rate_IMscn[1, -1] <- Rate_IMscn[1, -1] / 100
Rate_IMscn[1, 1] <- as.Date("9999-12-31")



#### Remove Objects
rm(EWMA, vol_min, vol_fn, i ,j, Lambda)



# IM용 시나리오별 커브생성------------------------------------------------------------------

core <- detectCores()
cluster <- makeCluster(spec = core) 
registerDoParallel(cl = cluster)

NPV_scn_IM <- foreach(scn_i = 1:nrow(Rate_IMscn), .packages = c('tidyverse', 'lubridate')) %dopar% {
    
    
    # 기준만기 금리자료 교체
    
    BMat <- data.frame(cbind(
        INDEX = c(0, 3, 6, 9, 12, 18, 24, 36, 48, 60,
                  72, 84, 96, 108, 120, 144, 180, 240) / 3 + 1,  #정산금리 만기>3개월 단위 지수
        RATE = as.vector(t(Rate_IMscn[scn_i, -1]))
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




