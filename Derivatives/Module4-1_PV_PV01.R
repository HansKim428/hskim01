# 할인계수, 선도금리 할당----------------------------------

##DF INDEX, 고정이자 미리 도출
DF_E <- match(CF_3NM$CF_3nM_E, Curve_KRW$DATE)
DF_S <- match(CF_3NM$CF_3nM_S, Curve_KRW$DATE)


core <- detectCores()
cluster <- makeCluster(spec = core) 
registerDoParallel(cl = cluster)

Result_scn_PV01 <-  foreach(scn_i = 1:nrow(Rate_PV01), .packages = c('tidyverse', 'lubridate')) %dopar% {

    # 할인계수, 선도금리 할당
    CF_3NM[['DF']] <- with(CF_3NM, NPV_scn_delta[[scn_i]][DF_E, 'DF'])
    CF_3NM[['FR']] <-
        with(CF_3NM,
             (NPV_scn_delta[[scn_i]][DF_S, 'DF'] /
                  NPV_scn_delta[[scn_i]][DF_E, 'DF'] - 1) *
                 General_DCF_rev)
    
    ## fixing CD금리 join
    CF_3NM[FixCF, 'FR'] <-
        left_join(tempCD, Rate_raw_KRW[, c('DATE', 'CD')], by = 'DATE')$CD / 100
    
    
    # 이자계산----------------------------------------------
    
    CF_3NM[['PV']] <- with(CF_3NM,
                           (Float_DCF * FR - (Fixed_DCF * Fixed_Rate / 100)) * Notional * DF)
    
    CF_3NM[which(is.na(CF_3NM$PV)), 'PV'] <- 0 #산출일 익영업일이 만기일인 경우 NPV = 0
    
    # 결과 취합----------------------------------------
    Result <- aggregate(CF_3NM$PV ~ CF_3NM$CLnum, FUN = sum)  #계약별 합계
    Result[[2]] <- ifelse(Result[[2]] >= 0,        #원미만 무시
                          floor(Result[[2]]),
                          ceiling(Result[[2]]))   
    
    Result <- rename(Result, CLnum = 1, NPV = 2)
    
    Result$NPV

}

stopCluster(cluster) 

##--CF별 민감도 분석################################################


##DF INDEX, 고정이자 미리 도출
DF_E <- match(CF_3NM$CF_3nM_E, Curve_KRW$DATE)
DF_S <- match(CF_3NM$CF_3nM_S, Curve_KRW$DATE)


core <- detectCores()
cluster <- makeCluster(spec = core) 
registerDoParallel(cl = cluster)

Result_scn_PV01 <-  foreach(scn_i = 1:nrow(Rate_PV01), .packages = c('tidyverse', 'lubridate')) %dopar% {
    
    # 할인계수, 선도금리 할당
    CF_3NM[['DF']] <- with(CF_3NM, NPV_scn_delta[[scn_i]][DF_E, 'DF'])
    CF_3NM[['FR']] <-
        with(CF_3NM,
             (NPV_scn_delta[[scn_i]][DF_S, 'DF'] /
                  NPV_scn_delta[[scn_i]][DF_E, 'DF'] - 1) *
                 General_DCF_rev)
    
    ## fixing CD금리 join
    CF_3NM[FixCF, 'FR'] <-
        left_join(tempCD, Rate_raw_KRW[, c('DATE', 'CD')], by = 'DATE')$CD / 100
    
    
    # 이자계산----------------------------------------------
    
    CF_3NM[['PV']] <- with(CF_3NM,
                           (Float_DCF * FR - (Fixed_DCF * Fixed_Rate / 100)) * Notional * DF)
    
    CF_3NM[which(is.na(CF_3NM$PV)), 'PV'] <- 0
    
    CF_3NM

}

stopCluster(cluster) 
origin <- Result_scn_PV01[[1]]

UP_2Y <- Result_scn_PV01[[8]]

UP_3Y <- Result_scn_PV01[[9]]
UP_4Y <- Result_scn_PV01[[10]]
UP_5Y <- Result_scn_PV01[[11]]
UP_6Y <- Result_scn_PV01[[12]]


DN_4Y <- Result_scn_PV01[[29]]
DN_5Y <- Result_scn_PV01[[30]]

UP_Pl <- Result_scn_PV01[[20]]
DN_Pl <- Result_scn_PV01[[39]]


#################################################################





Result_scn_PV01 <- data.frame(do.call(cbind, Result_scn_PV01))



# Calculate NPV Difference(Delta)--------------------------------------------------

colnames(Result_scn_PV01) <- Rate_PV01$scn_i

Pos_KRW_FN_PV01 <- Pos_KRW[c(order(Pos_KRW$CLnum)),]

## 시나리오별 결과를 포지션 정보에 결합
Pos_KRW_FN_PV01 <- cbind(Pos_KRW_FN_PV01, Result_scn_PV01)   

# #### Row Order & NPV validation
# which(Pos_KRW_FN_PV01$Origin - Pos_KRW_FN_PV01$NPV > 10000)    


## 상/하향 Delta 평균
for (i in 1:19) {
    Pos_KRW_FN_PV01[, c(i + 32)] <-
        ((Pos_KRW_FN_PV01[, c(i + 32)] - Pos_KRW_FN_PV01[, 32]) -
             (Pos_KRW_FN_PV01[, c(i + 51)] - Pos_KRW_FN_PV01[, 32])) / 2
}

## 하향 Delta 삭제
Pos_KRW_FN_PV01 <- Pos_KRW_FN_PV01[, -c(52:70)]

## bucket delta 합산
Pos_KRW_FN_PV01 <- Pos_KRW_FN_PV01 %>% 
    mutate(Up_sum = Up_Call + Up_CD + Up_6M + Up_9M + Up_1Y + Up_18M + Up_2Y + Up_3Y + Up_4Y +
           Up_5Y + Up_6Y + Up_7Y + Up_8Y + Up_9Y + Up_10Y + Up_12Y + Up_15Y + Up_20Y)

# #### bucket/parallel delta validation
# max(abs(Pos_KRW_FN_PV01$Up_sum - Pos_KRW_FN_PV01$Up_Prl))




# PV01(parallel) 회원 및 계좌별 취합 -------------------------------------

SUMFIX <- Pos_KRW_FN_PV01 %>% 
    group_by(Pay_Accnum, Pay_Member) %>%
    summarise(SUMFIX = sum(Up_Prl))

SUMFLO <- Pos_KRW_FN_PV01 %>% 
    group_by(Rec_Accnum, Rec_Member) %>%
    summarise(SUMFLO = sum(Up_Prl))    

PV01_Member <- full_join(SUMFIX, SUMFLO, by = c('Pay_Accnum' = 'Rec_Accnum')) 

rm(SUMFIX, SUMFLO)

PV01_Member[is.na(PV01_Member[, 2]) == TRUE, 2] <- 
    PV01_Member[is.na(PV01_Member[, 2]) == TRUE, 4]
PV01_Member[is.na(PV01_Member[, 4]) == TRUE, 4] <- 
    PV01_Member[is.na(PV01_Member[, 4]) == TRUE, 2]
PV01_Member[is.na(PV01_Member[, 3]) == TRUE, 3] <- 0
PV01_Member[is.na(PV01_Member[, 5]) == TRUE, 5] <- 0


## 최종 PVBP(회원 및 계좌별)
PV01_Member <- PV01_Member %>% 
    mutate(PVBP = SUMFIX - SUMFLO) %>% 
    select(Pay_Accnum, Pay_Member, PVBP)





###### --------------------이하 compression시 참고----------------------------



# 
# 
# 
# #4-2. For compression test, save Position Delta
# 
# save(Pos_KRW_FN_PV01, file = 'Pos_KRW_FN_PV01_220630.Rdata')
# write_excel_csv(PV01_Member, 'PV01_Member_after_22-2nd.csv')
# 
# 
# ##################################################
# 
# PV01_SUM_FIX <- Pos_KRW_FN_PV01 %>% 
#     group_by(Pay_CP) %>% 
#     summarise(PV_Call = sum(Up_Call),
#               PV_CD = sum(Up_CD),
#               PV_6M = sum(Up_6M),
#               PV_9M = sum(Up_9M),
#               PV_1Y = sum(Up_1Y),
#               PV_18M = sum(Up_18M),
#               PV_2Y = sum(Up_2Y),
#               PV_3Y = sum(Up_3Y),
#               PV_4Y = sum(Up_4Y),
#               PV_5Y = sum(Up_5Y),
#               PV_6Y = sum(Up_6Y),
#               PV_7Y = sum(Up_7Y),
#               PV_8Y = sum(Up_8Y),
#               PV_9Y = sum(Up_9Y),
#               PV_10Y = sum(Up_10Y),
#               PV_12Y = sum(Up_12Y),
#               PV_15Y = sum(Up_15Y),
#               PV_20Y = sum(Up_20Y),
#               PVBP = sum(Up_Prl))
# 
# 
# PV01_SUM_FLO <- Pos_KRW_FN_PV01 %>% 
#     group_by(Rec_CP) %>% 
#     summarise(PV_Call = sum(Up_Call),
#               PV_CD = sum(Up_CD),
#               PV_6M = sum(Up_6M),
#               PV_9M = sum(Up_9M),
#               PV_1Y = sum(Up_1Y),
#               PV_18M = sum(Up_18M),
#               PV_2Y = sum(Up_2Y),
#               PV_3Y = sum(Up_3Y),
#               PV_4Y = sum(Up_4Y),
#               PV_5Y = sum(Up_5Y),
#               PV_6Y = sum(Up_6Y),
#               PV_7Y = sum(Up_7Y),
#               PV_8Y = sum(Up_8Y),
#               PV_9Y = sum(Up_9Y),
#               PV_10Y = sum(Up_10Y),
#               PV_12Y = sum(Up_12Y),
#               PV_15Y = sum(Up_15Y),
#               PV_20Y = sum(Up_20Y),
#               PVBP = sum(Up_Prl))    
# 
# 
# PV01_Member <- full_join(PV01_SUM_FIX, PV01_SUM_FLO, by = c('Pay_CP' = 'Rec_CP'))
# 
# PV01_Member[is.na(PV01_Member)] <- 0
# 
# PV01_Member <- PV01_Member %>% 
#     mutate(PV_Call = PV_Call.x - PV_Call.y,
#            PV_CD = PV_CD.x - PV_CD.y,
#            PV_6M = PV_6M.x - PV_6M.y,
#            PV_9M = PV_9M.x - PV_9M.y,
#            PV_1Y = PV_1Y.x - PV_1Y.y,
#            PV_18M = PV_18M.x - PV_18M.y,
#            PV_2Y = PV_2Y.x - PV_2Y.y,
#            PV_3Y = PV_3Y.x - PV_3Y.y,
#            PV_4Y = PV_4Y.x - PV_4Y.y,
#            PV_5Y = PV_5Y.x - PV_5Y.y,
#            PV_6Y = PV_6Y.x - PV_6Y.y,
#            PV_7Y = PV_7Y.x - PV_7Y.y,
#            PV_8Y = PV_8Y.x - PV_8Y.y,
#            PV_9Y = PV_9Y.x - PV_9Y.y,
#            PV_10Y = PV_10Y.x - PV_10Y.y,
#            PV_12Y = PV_12Y.x - PV_12Y.y,
#            PV_15Y = PV_15Y.x - PV_15Y.y,
#            PV_20Y = PV_20Y.x - PV_20Y.y,
#            PVBP = PVBP.x - PVBP.y) %>% 
#     select(Pay_CP,
#            PV_Call,
#            PV_CD,
#            PV_6M,
#            PV_9M,
#            PV_1Y,
#            PV_18M,
#            PV_2Y,
#            PV_3Y,
#            PV_4Y,
#            PV_5Y,
#            PV_6Y,
#            PV_7Y,
#            PV_8Y,
#            PV_9Y,
#            PV_10Y,
#            PV_12Y,
#            PV_15Y,
#            PV_20Y,
#            PVBP)
# 
# 
# sapply(PV01_Member[, -1], sum)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ######################End Sub
# 
# 
# 
# 
# #원금리 NPV
# NPV_Origin <- NPV_scn_delta %>% 
#     filter(DATE == 'Origin')
# 
# NPV_scn_delta <- NPV_scn_delta %>% 
#     mutate(NPV_Diff = NPV_scn_delta$NPV_sum - NPV_Origin$NPV_sum)
# 
# ?paste
# aaaaa <- NPV_scn_delta %>% 
#     filter(Member == 'SC은행')
# 
# sum(aaaaa$NPV_Diff[2:19])
# sum(aaaaa$NPV_Diff[20:37])
# 
# #검증용 CSV 생성
# 
# #abcdefg <- NPV_scn_delta %>% 
# #    filter(DATE != '9999-12-31') %>% 
# #    select(Pay_Accnum, NPV_sum) %>% 
# #    filter(Pay_Accnum == 1010000001) %>% 
# #    arrange(desc(NPV_sum))
# 
# #write.csv(abcdefg, 'Kyobo.csv')
# #write.csv(NPV_4th, 'IM.csv')
# 
# write.csv(aaaaa, 'aaaaa.csv')
