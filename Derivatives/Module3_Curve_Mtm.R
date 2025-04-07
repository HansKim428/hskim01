# 3개월 주요만기 날짜 생성(EOM 적용)-----------------------------------

EDate_Next <- as.Date(Cal_KRW[[match(EDate + 1, Cal_KRW$DATE), 6]])  #Next Day of EDate 

Curve_EOM <- ifelse(month(EDate) == month(EDate_Next), FALSE, TRUE)  #둘이 다른 경우 EOM 적용

Prm_3N <- seq(0, 240, 3)  #20년까지의 주요만기 개월수 표현

ifelse(
    Curve_EOM == FALSE,
    Prm_3NM <-
        data.frame(DATE = EDate %m+% months(Prm_3N)),  #주요만기 날짜 생성
    Prm_3NM <-
        data.frame(DATE = ceiling_date(EDate %m+% months(Prm_3N), "month") - 1)
)


# 기준만기 금리자료 대입 및 3개월 주기 선형보간-------------------------------

Rate_raw_KRW <- read_csv("Rate.csv")  #금리자료 불러오기

BMat <-data.frame(cbind(
            INDEX = c(0, 3, 6, 9, 12, 18, 24, 36, 48, 60,
                    72, 84, 96, 108, 120, 144, 180, 240) / 3 + 1,  #정산금리 만기>3개월 단위 지수
            RATE = as.vector(t(
                Rate_raw_KRW[which(Rate_raw_KRW$DATE == BDate), -1] / 100  #금리 소수점 표시
                ))
        ))


## 주요만기 modified following 영업일 찾기
for (i in 2:81) {
    Prm_3NM[i, 'DATE_MF'] <-
        Cal_KRW[match(Prm_3NM$DATE[i], Cal_KRW$DATE), 7]
}

Prm_3NM[1, 2] <- EDate  #call의 MF는 Effective Date


## 선형보간을 위한 이전/이후 정산금리만기 INDEX 추출 및 정산금리 입력
for (i in 1:81) {
    Prm_3NM[i, 'INDEX_Ante'] <-
        BMat[which.min(ifelse(i >= BMat[, 1], i - BMat[, 1], 100)), 1]
    
    Prm_3NM[i, 'DATE_Ante'] <- 
        Prm_3NM$DATE_MF[Prm_3NM$INDEX_Ante[i]]
    
    Prm_3NM[i, 'RATE_Ante'] <- 
        BMat[match(Prm_3NM$INDEX_Ante[i], BMat$INDEX), 2]
    
    Prm_3NM[i, 'INDEX_Post'] <-
        BMat[which.min(ifelse(i <= BMat[, 1], BMat[, 1] - i, 100)), 1]
    
    Prm_3NM[i, 'DATE_Post'] <- 
        Prm_3NM$DATE_MF[Prm_3NM$INDEX_Post[i]]
    
    Prm_3NM[i, 'RATE_Post'] <- 
        BMat[match(Prm_3NM$INDEX_Post[i], BMat$INDEX), 2]
}


## 선형보간(소수점 14자리)
Prm_3NM <- Prm_3NM %>%
    #select(DATE_MF, DATE_Ante, RATE_Ante, DATE_Post, RATE_Post) %>% 
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

## 커브 모든날짜 찾기
Curve_KRW <- as.data.frame(Cal_KRW %>%    
                               filter(SeB == '1' & 
                                      DATE >= EDate &
                                      DATE <= Prm_3NM[81, 2]) %>% 
                               select(DATE))

## 선형보간을 위한 직전/직후 주요만기 찾기
for (i in 1:nrow(Curve_KRW)) {
    Curve_KRW[i, c('DATE_Ante', 'RATE_Ante')] <- 
        Prm_3NM[which.min(ifelse(0 <= as.integer(Curve_KRW[i, 1] - Prm_3NM[, 1]),  #직전 주요만기
                                 as.integer(Curve_KRW[i, 1] - Prm_3NM[, 1]),
                                 100)
                          ),
                c('DATE_MF', 'ZCR')]
    
    Curve_KRW[i, c('DATE_Post', 'RATE_Post')] <- 
        Prm_3NM[which.min(ifelse(0 >= as.integer(Curve_KRW[i, 1] - Prm_3NM[, 1]),  #직후 주요만기
                                 as.integer(Prm_3NM[, 1] - Curve_KRW[i, 1]),
                                 100)
                          ),
                c('DATE_MF', 'ZCR')]
}


## 모든날짜 무위험금리 선형보간 및 DF 계산
Curve_KRW <- Curve_KRW %>%
    select(DATE, DATE_Ante, RATE_Ante, DATE_Post, RATE_Post) %>%
    mutate(ZCR = ifelse(DATE_Ante == DATE_Post,
                        RATE_Ante,
                        round_off(interpol(DATE, 
                                           DATE_Ante, 
                                           DATE_Post, 
                                           RATE_Ante, 
                                           RATE_Post),
                                  digits = 14)
                        ),
           DF = round_off(ZCRtoDF(ZCR, DATE, EDate), 12)
    )


#### Remove Objects
rm(i)