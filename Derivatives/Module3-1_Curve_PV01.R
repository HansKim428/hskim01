# PV01용 정산금리 생성------------------------------------------------

## 산출일 정산금리
Rate_PV01 <-   
    data.frame(Rate_raw_KRW[which(Rate_raw_KRW$DATE == BDate), -1] / 100)

## 만기별 1bp 충격                  
for (i in 1:18) {
    Rate_PV01[i + 1, -i] <- Rate_PV01[1, -i]
    Rate_PV01[i + 1, i] <- Rate_PV01[1, i] + 0.0001
    Rate_PV01[i + 20, -i] <- Rate_PV01[1, -i]
    Rate_PV01[i + 20, i] <- Rate_PV01[1, i] - 0.0001
}

## 금리 평행이동 1bp
Rate_PV01[20, ] <- Rate_PV01[1, ] + 0.0001
Rate_PV01[39, ] <- Rate_PV01[1, ] - 0.0001

## 시나리오 이름 입력
Rate_PV01 <-
    cbind(
        'scn_i' = c(
            'Origin',
            'Up_Call',
            'Up_CD',
            'Up_6M',
            'Up_9M',
            'Up_1Y',
            'Up_18M',
            'Up_2Y',
            'Up_3Y',
            'Up_4Y',
            'Up_5Y',
            'Up_6Y',
            'Up_7Y',
            'Up_8Y',
            'Up_9Y',
            'Up_10Y',
            'Up_12Y',
            'Up_15Y',
            'Up_20Y',
            'Up_Prl',
            'Down_Call',
            'Down_CD',
            'Down_6M',
            'Down_9M',
            'Down_1Y',
            'Down_18M',
            'Down_2Y',
            'Down_3Y',
            'Down_4Y',
            'Down_5Y',
            'Down_6Y',
            'Down_7Y',
            'Down_8Y',
            'Down_9Y',
            'Down_10Y',
            'Down_12Y',
            'Down_15Y',
            'Down_20Y',
            'Down_Prl'
        ),
        Rate_PV01
    )



# PV01용 시나리오별 커브생성------------------------------------------------------------------

core <- detectCores()
cluster <- makeCluster(spec = core) 
registerDoParallel(cl = cluster)

NPV_scn_delta <- foreach(scn_i = 1:nrow(Rate_PV01), .packages = c('tidyverse', 'lubridate')) %dopar% {

        
    # 기준만기 금리자료 교체
    
    BMat <- data.frame(cbind(
        INDEX = c(0, 3, 6, 9, 12, 18, 24, 36, 48, 60,
                  72, 84, 96, 108, 120, 144, 180, 240) / 3 + 1,  #정산금리 만기>3개월 단위 지수
        RATE = as.vector(t(Rate_PV01[scn_i, -1]))
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



