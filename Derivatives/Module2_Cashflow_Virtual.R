Rate_raw_KRW <- read_csv("Rate.csv")  #금리자료 불러오기

BMat <-data.frame(cbind(
    INDEX = c(0, 3, 6, 9, 12, 18, 24, 36, 48, 60,
              72, 84, 96, 108, 120, 144, 180, 240) / 3 + 1,  #정산금리 만기>3개월 단위 지수
    RATE = as.vector(t(
        Rate_raw_KRW[which(Rate_raw_KRW$DATE == BDate), -1]   #금리 소수점 미표시
    ))
))


EDate <- as.Date(Cal_KRW[[match(BDate + 1, Cal_KRW$DATE), 6]])  #Curve Effective Day

EDate_Next <- as.Date(Cal_KRW[[match(EDate + 1, Cal_KRW$DATE), 6]])  #Next Day of EDate 

Curve_EOM <- ifelse(month(EDate) == month(EDate_Next), FALSE, TRUE)

Virtual_3N <- c(0, 3, 6, 9, 12, 18, 24, 36, 48, 60,
            72, 84, 96, 108, 120, 144, 180, 240)  #20년까지의 주요만기 개월수 표현


ifelse(
    Curve_EOM == FALSE,
    Prm_virtual <-
        data.frame(DATE = EDate %m+% months(Virtual_3N)),  #주요만기 날짜 생성
    Prm_virtual <-
        data.frame(DATE = ceiling_date(EDate %m+% months(Virtual_3N), "month") - 1)
)



for (i in 2:18) {
    Prm_virtual[i, 'DATE'] <-
        Cal_KRW[match(Prm_virtual$DATE[i], Cal_KRW$DATE), 7]
}


Prm_virtual[1, 1] <- EDate  #call의 MF는 Effective Date

Prm_virtual <- Prm_virtual %>% mutate(RATE = BMat$RATE)

Pos_KRW <- data.frame(CalDate = BDate,
                      BaseDate = BDate,
                      TCnum = 'TC_virtual',
                      CLnum = BMat$INDEX,
                      Edate = EDate,
                      Maturity = Prm_virtual$DATE,
                      Pay_CPnum = '00000',
                      Pay_CP = 'na',
                      Pay_Memnum = '00000',
                      Pay_Member = 'na',
                      Pay_Accnum = '00000',
                      Rec_CPnum = '00000',
                      Rec_CP = 'na',
                      Rec_Memnum = '00000',
                      Rec_Member = 'na',
                      Rec_Accnum = '00000',    
                      Notional = 100,
                      Bizcenter = "KRSE",
                      BDC = 'MODFOLLOWING',
                      Fixed_Rate = Prm_virtual$RATE,
                      Fixed_DCF = 'ACT/365.FIXED',
                      Fixed_Payment = '3M KRW',
                      Float_Spread = 0,
                      Float_DCF = 'ACT/365.FIXED',
                      Float_Payment = '3M KRW',
                      EOM = 'N',
                      NPV_pm5 = 0,
                      NPV = 0,
                      SEF = "na",
                      ANONY = 'NA'
                        )

write_csv(Pos_KRW[-c(1,2),], 'virtual_PR14_210111.csv')




