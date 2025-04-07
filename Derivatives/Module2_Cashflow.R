# 포지션 정보 불러오기------------------------------------------------

BDate_chr <- as.character(BDate)  #산출일 문자열
xl <- paste(getwd(),              #주소
            "/PR14/PR14_KRW_", 
            substr(BDate_chr, 3, 4),
            substr(BDate_chr, 6, 7),
            substr(BDate_chr, 9, 10),
            ".xlsx",
            sep="")

Pos_KRW <- data.frame(read_xlsx(xl))
Pos_KRW[, 5] <- as.Date(Pos_KRW[, 5])
Pos_KRW[, 6] <- as.Date(Pos_KRW[, 6])
Pos_KRW[, 17] <- as.numeric(Pos_KRW[, 17])
Pos_KRW[, 20] <- as.numeric(Pos_KRW[, 20])
Pos_KRW[, 27] <- as.numeric(Pos_KRW[, 27])
Pos_KRW[, 28] <- as.numeric(Pos_KRW[, 28])

## 만기 개월수 추가, Business Day Convention 숫자 변환
Pos_KRW <- Pos_KRW %>% 
    mutate(Dur = as.numeric((year(Maturity) - year(Edate)) * 12 +
                             month(Maturity) - month(Edate) + 
                            (sign((day(Maturity) - day(Edate))) *
                             abs(day(Maturity) - day(Edate)) %/% 17)
                           ),
           BDC = case_when(
               BDC == "MODFOLLOWING" ~ 7,
               BDC == "PRECEDING" ~ 5,
               BDC == "FOLLOWING" ~ 6)
           )
#### 만기월수 3의배수 점검 : which(Pos_KRW$Dur %% 3 != 0)

#Pos_KRW <- read_csv('virtual_PR14_210111.csv')


# Cashflow 생성--------------------------------------------------

core <- detectCores()
cluster <- makeCluster(spec = core) 
registerDoParallel(cl = cluster)

CF_3NM <-
    foreach(i = 1:nrow(Pos_KRW), .packages = 'lubridate') %dopar% {
        CF_3NMs <- CF_3nM(
            Cal_KRW,
            Pos_KRW[i, 'Dur'],
            Pos_KRW[i, 'BDC'],
            Pos_KRW[i, 'EOM'],
            Pos_KRW[i, 'Edate'],
            Pos_KRW[i, 'CLnum'],
            Pos_KRW[i, 'Pay_Accnum'],
            Pos_KRW[i, 'Rec_Accnum'],
            Pos_KRW[i, 'Notional'],
            Pos_KRW[i, 'Fixed_Rate'],
            Pos_KRW[i, 'Fixed_DCF'],
            Pos_KRW[i, 'Float_DCF']
        )
        return(CF_3NMs)
    }

stopCluster(cluster)



# CF 정리------------------------------------------------

EDate <- as.Date(Cal_KRW[[match(BDate + 1, Cal_KRW$DATE), 6]])  #Curve Effective Day

## list 변경
CF_3NM <- data.frame(do.call(rbind, CF_3NM))

## 이자기간 영업일 찾기
CF_3NM <- CF_3NM %>%
    convert(num(c(
        CF_3nM_S, CF_3nM_E, Notional, Fixed_Rate, BDC
    ))) %>%
    #convert(dte(c(CF_3nM_S, CF_3nM_E))) )
    mutate(
        CF_3nM_S = case_when(
            BDC == 7 ~
                Cal_KRW$SeB_modfol[match(CF_3nM_S, Cal_KRW$DATE)],
            BDC == 6 ~
                Cal_KRW$SeB_fol[match(CF_3nM_S, Cal_KRW$DATE)],
            BDC == 5 ~
                Cal_KRW$SeB_pre[match(CF_3nM_S, Cal_KRW$DATE)]
        ),
        CF_3nM_E = case_when(
            BDC == 7 ~
                Cal_KRW$SeB_modfol[match(CF_3nM_E, Cal_KRW$DATE)],
            BDC == 6 ~
                Cal_KRW$SeB_fol[match(CF_3nM_E, Cal_KRW$DATE)],
            BDC == 5 ~
                Cal_KRW$SeB_pre[match(CF_3nM_E, Cal_KRW$DATE)]
        )
    ) %>% 
    select(-'BDC')


CF_3NM <- CF_3NM %>% 
    filter(CF_3nM_E >= EDate)   #이자지급일이 산출일 익영업일 전인 경우 가치평가에 영향없어 제외




## Fixed DCF, Float DCF 계산
CF_3NM <- CF_3NM %>%
    mutate(Fixed_DCF = ifelse(
        Fixed_DCF == 'ACT/365.FIXED',
        as.numeric(CF_3nM_E - CF_3nM_S) / 365,
        as.numeric(CF_3nM_E - CF_3nM_S) / 360),
        Float_DCF = ifelse(
            Float_DCF == 'ACT/365.FIXED',
                as.numeric(CF_3nM_E - CF_3nM_S) / 365,
                case_when(
                    year(CF_3nM_E) %% 4 == 0 & year(CF_3nM_S) != year(CF_3nM_E) ~
                        (
                            as.numeric(ymd(paste(
                                year(CF_3nM_E), 01, 01
                            )) - CF_3nM_S) / 365 +
                                as.numeric(CF_3nM_E - ymd(paste(
                                    year(CF_3nM_E), 01, 01
                                ))) / 366
                        ),
                    year(CF_3nM_E) %% 4 == 0 &
                        year(CF_3nM_S) == year(CF_3nM_E) ~
                        as.numeric(CF_3nM_E - CF_3nM_S) / 366,
                    year(CF_3nM_E) %% 4 == 1 &
                        year(CF_3nM_S) != year(CF_3nM_E) ~
                        (
                            as.numeric(ymd(paste(
                                year(CF_3nM_E), 01, 01
                            )) - CF_3nM_S) / 366 +
                                as.numeric(CF_3nM_E - ymd(paste(
                                    year(CF_3nM_E), 01, 01
                                ))) / 365
                        ),
                    TRUE ~ as.numeric(CF_3nM_E - CF_3nM_S) / 365
                )
        ),
        General_DCF_rev = 365 / as.numeric(CF_3nM_E - CF_3nM_S)
    )



# CD fixing용 임시계산----------------------------------------------

FixCF <- which(CF_3NM$CF_3nM_E > EDate &    #fixing 대상
                   Cal_KRW[match(CF_3NM$CF_3nM_S - 1 , Cal_KRW$DATE), 5] <=
                   as.numeric(BDate))

tempCD <- data.frame(Cal_KRW[match(CF_3NM$CF_3nM_S[FixCF] - 1,
                                   Cal_KRW$DATE),
                             5])   #선도금리 할당시 join용 임시변수
colnames(tempCD) <- 'DATE'



#### Remove Objects
rm(xl)
