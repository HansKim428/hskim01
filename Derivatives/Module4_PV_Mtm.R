# 할인계수, 선도금리 할당----------------------------------

CF_3NM[['DF']] <- with(CF_3NM, Curve_KRW[match(CF_3nM_E, Curve_KRW$DATE), 'DF'])


CF_3NM[['FR']] <-
    with(CF_3NM,
         (Curve_KRW[match(CF_3nM_S, Curve_KRW$DATE), 'DF'] /
             Curve_KRW[match(CF_3nM_E, Curve_KRW$DATE), 'DF'] - 1) *
             General_DCF_rev)


## fixing CD금리 join
CF_3NM[FixCF, 'FR'] <-
    left_join(tempCD, Rate_raw_KRW[, c('DATE', 'CD')], by = 'DATE')$CD / 100

## floating rate spread 가산
spd <- c(left_join(as.data.frame(CF_3NM %>% select(CLnum)),
            Pos_KRW %>% select(CLnum, Float_Spread),
            by = 'CLnum')[, 2] / 10000)

CF_3NM[,12] <- CF_3NM[['FR']] + spd


# 이자계산----------------------------------------------
CF_3NM[['PV']] <- with(CF_3NM,
                       (Float_DCF * FR - Fixed_DCF * Fixed_Rate / 100) * Notional * DF)

CF_3NM[which(is.na(CF_3NM$PV)), 'PV'] <- 0

# 결과 취합----------------------------------------
Result <- aggregate(CF_3NM$PV ~ CF_3NM$CLnum, FUN = sum)  #계약별 합계

Result[[2]] <- ifelse(Result[[2]] >= 0,        #원미만 무시
                      floor(Result[[2]]),
                      ceiling(Result[[2]]))   

Result <- rename(Result, CLnum = 1, NPV = 2)

## 포지션 정보에 결합
Pos_KRW_FN <- left_join(Pos_KRW, Result, by = 'CLnum')

rm(Result)

Pos_KRW_FN <- rename(Pos_KRW_FN, NPV = NPV.y)

## 회원 및 계좌별 취합
NPV_sum_pay <- Pos_KRW_FN %>%    #고정지급포지션
    group_by(Pay_Accnum, Pay_Member) %>%
    summarise(NPV_sum_pay = sum(NPV))

NPV_sum_rec <- Pos_KRW_FN %>%    #고정수취포지션
    group_by(Rec_Accnum, Rec_Member) %>%
    summarise(NPV_sum_rec = sum(NPV))

NPV_sum_PminusR <-
    merge(
        NPV_sum_pay,
        NPV_sum_rec,
        all = TRUE,
        by.x = 'Pay_Accnum',
        by.y = 'Rec_Accnum'
    )

NPV_sum_PminusR <-
    NPV_sum_PminusR %>%
    mutate(
        Member = ifelse(is.na(Pay_Member) != TRUE, Pay_Member, Rec_Member),
        NPV_sum = ifelse(is.na(NPV_sum_pay) == TRUE, 0, NPV_sum_pay) -
            ifelse(is.na(NPV_sum_rec) == TRUE, 0, NPV_sum_rec)
    ) %>%
    select(Pay_Accnum, Member, NPV_sum)


#### Remove Objects
rm(NPV_sum_rec, NPV_sum_pay)
