# 할인계수, 선도금리 할당----------------------------------

##DF INDEX, 고정이자 미리 도출
DF_E <- match(CF_3NM$CF_3nM_E, Curve_KRW$DATE)
DF_S <- match(CF_3NM$CF_3nM_S, Curve_KRW$DATE)


core <- detectCores()
cluster <- makeCluster(spec = core) 
registerDoParallel(cl = cluster)

Result_scn_ST <-  foreach(scn_i = 1:nrow(Rate_ST), .packages = c('tidyverse', 'lubridate')) %dopar% {
    
    # 할인계수, 선도금리 할당
    CF_3NM[['DF']] <- with(CF_3NM, NPV_scn_ST[[scn_i]][DF_E, 'DF'])
    CF_3NM[['FR']] <-
        with(CF_3NM,
             (NPV_scn_ST[[scn_i]][DF_S, 'DF'] /
                  NPV_scn_ST[[scn_i]][DF_E, 'DF'] - 1) *
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

Result_scn_ST <- data.frame(do.call(cbind, Result_scn_ST))




# Calculate NPV Difference(ST)--------------------------------------------------

colnames(Result_scn_ST) <- c("Today", Rate_ST[-1, 1])

Pos_KRW_FN_ST <- Pos_KRW[c(order(Pos_KRW$CLnum)),]

## 시나리오별 결과를 포지션 정보에 결합
Pos_KRW_FN_ST <- cbind(Pos_KRW_FN_ST, Result_scn_ST)   



# NPV difference(ST)-------------------------------------------------- 

## 고정지급쪽 계좌별 NPV 합계
NPV_sum_pay <- Pos_KRW_FN_ST %>%
    group_by(Pay_Accnum, Pay_Member) %>%
    summarise(across(Today : Lehman, sum)) %>% 
    rename(Accnum = Pay_Accnum,
           Member = Pay_Member)

## 고정수취쪽 계좌별 NPV 합계(음수화)
NPV_sum_rec <- Pos_KRW_FN_ST %>%
    group_by(Rec_Accnum, Rec_Member) %>%
    summarise(across(Today : Lehman, sum)) %>% 
    rename(Accnum = Rec_Accnum, 
           Member = Rec_Member)
NPV_sum_rec[, -c(1, 2)] <- - NPV_sum_rec[, -c(1, 2)] 

## 고정지급 NPV - 고정수취 NPV
NPV_sum_PminusR <- 
    rbind(NPV_sum_pay, NPV_sum_rec) %>% 
    group_by(Accnum, Member) %>%
    summarise(across(Today : Lehman, sum))

rm(NPV_sum_pay, NPV_sum_rec)


ST_data <- paste0('ST_', 
                  substr(BDate_chr, 3, 4),
                  substr(BDate_chr, 6, 7),
                  substr(BDate_chr, 9, 10),
                  ".Rdata")

save(NPV_sum_PminusR, file = ST_data)








# 최종 취합 - Stress Exposure / IM excess Exposure-------------------

ST_table <-  NPV_sum_PminusR %>%      
    mutate(across(EVTup01 : Lehman, function(x) Today - x))   # 당일NPV 차감하여 위험액 산출(손실이 +)

## 증거금 load

load(IM_data)


##유안타, BNK증권 복수계좌 특수케이스 처리(한 계좌로 몰아주기)

### ST용 파일 처리
ST_table[which(ST_table$Accnum == '024010000003'), -c(1, 2)] <-
    ST_table[which(ST_table$Accnum == '024010000003'),  -c(1, 2)] +
    ST_table[which(ST_table$Accnum == '024010000004'),  -c(1, 2)]
ST_table[which(ST_table$Accnum == '024010000004'),  -c(1, 2)] <- 0

ST_table[which(ST_table$Accnum == '086010000001'),  -c(1, 2)] <-
    ST_table[which(ST_table$Accnum == '086010000001'),  -c(1, 2)] +
    ST_table[which(ST_table$Accnum == '086010000002'),  -c(1, 2)]
ST_table[which(ST_table$Accnum == '086010000002'),  -c(1, 2)] <- 0

### IM용 파일 처리
IM[which(IM$Accnum == '024010000003'), 3] <-
    IM[which(IM$Accnum == '024010000003'), 3] + IM[which(IM$Accnum == '024010000004'), 3]
IM[which(IM$Accnum == '024010000004'), 3] <- 0

IM[which(IM$Accnum == '086010000001'), 3] <-
    IM[which(IM$Accnum == '086010000001'), 3] + IM[which(IM$Accnum == '086010000002'), 3]
IM[which(IM$Accnum == '086010000002'), 3] <- 0


### 증거금 결합
# ####Accnum order Validation
# NPV_sum_PminusR$Accnum == IM$Accnum

ST_table <- left_join(ST_table, IM[, c(1, 3)], by = 'Accnum')
rm(IM)



# 1. EVT(계좌별 위험노출액 FIX -> 계좌별 증거금초과위험 산출 -> Cover2 계산)------------

## ST Exposure(Direct) / IM Excess Exposure

for (i in 1:nrow(ST_table)) {    # 1276개 결과중 신뢰수준 고려한 위험액
    ST_table[i, 1284] <-
        (sort(as.matrix(ST_table[i, 
                                 -c(1, 2, 3, 1280, 1281, 1282, 1283)]
                        )
              )[q_int + 1] *
             (1 - d_dec) +
         sort(as.matrix(ST_table[i,
                                 -c(1, 2, 3, 1280, 1281, 1282, 1283)]
                        )
              )[q_int + 2] *
             d_dec)
}

ST_table <- ST_table %>%                # EVT 포함 통합 table
    rename(EVT_Loss = ...1284) %>%      # 오늘 NPV 대비 손익(손실이 +)
    mutate(EVT_ExcIM = ifelse(EVT_Loss + IM > 0, EVT_Loss + IM, 0))   # 증거금 초과 손익(손실이 +)

EVT_result_group <- ST_table %>%       ### EVT 위험액 취합용 table
    select(Accnum, Member, EVT_Loss, EVT_ExcIM)  

EVT_result_group$Member <-                                      #그룹사 고려
    case_when(
        EVT_result_group$Member == '신한금융투자' |
            EVT_result_group$Member == '신한은행' ~ '신한G',
        EVT_result_group$Member == 'KB증권' |
            EVT_result_group$Member == '국민은행' ~ '국민G',
        EVT_result_group$Member == '하나은행' |
            EVT_result_group$Member == '하나증권' ~ '하나G',
        EVT_result_group$Member == 'BNK증권' |
            EVT_result_group$Member == '부산은행' ~ '부산G',
        EVT_result_group$Member == '아이비케이투자증권' |
            EVT_result_group$Member == '중소기업은행' ~ '기업G',
        EVT_result_group$Member == 'NH투자증권' |
            EVT_result_group$Member == '농협은행' ~ 'NHG',
        TRUE ~ EVT_result_group$Member
    )


## Direct Result

ST_EVT <- EVT_result_group %>%
    group_by(Member) %>%
    summarise(ST_group = sum(EVT_Loss)) %>%
    arrange(desc(ST_group))

## IM_Excess Result

ST_EVT_Excess <- EVT_result_group %>%
    group_by(Member) %>%
    summarise(ST_group = sum(EVT_ExcIM)) %>%
    arrange(desc(ST_group))

sum(ST_EVT_Excess$ST_group[c(1, 2)])



# 2. Historical_5Y == 100% VaR w/o FHS(계좌별 1250개 위험노출액 -> 계좌별 1250개 초과위험액 산출 -> Cover2 1250개 중 최고치)

## St Exposure(Direct) 기준 매일의 Cover2 산출하고, 최대일 뽑기 - 실제 공동기금 산출과 관계없음
His_result <- ST_table[17:1266]    #1250일 시나리오중

His_result <- cbind(ST_table[, 2], (abs(His_result) + His_result) / 2)    #모든 이득 0으로 변경

His_result <- melt(His_result)  #long form 변경

His_result <- His_result %>%    #exposure와 증거금초과액 계산
    rename(c('DATE' = variable,
             'His_Loss' = value)) %>%
    mutate(His_ExcIM = ifelse(His_Loss + ST_table$IM > 0, His_Loss + ST_table$IM, 0),
           DATE = as.Date(as.numeric(as.character(DATE)), origin = '1970-01-01'))


His_result$Member <-                                      #그룹사 고려
    case_when(
        His_result$Member == '신한금융투자' | 
            His_result$Member == '신한은행' ~ '신한G',
        His_result$Member == 'KB증권' |
            His_result$Member == '국민은행' ~ '국민G',
        His_result$Member == '하나은행' |
            His_result$Member == '하나금융투자' ~ '하나G',
        His_result$Member == 'BNK증권' |
            His_result$Member == '부산은행' ~ '부산G',
        His_result$Member == '아이비케이투자증권' |
            His_result$Member == '중소기업은행' ~ '기업G',
        His_result$Member == 'NH투자증권' |
            His_result$Member == '농협은행' ~ 'NHG',
        TRUE ~ His_result$Member
    )



## Direct Result  

ST_His <- His_result %>%
    group_by(DATE, Member) %>%
    summarise(Loss_group = sum(His_Loss)) %>% 
    group_by(DATE) %>% 
    arrange(desc(Loss_group)) %>% 
    summarise(Cover2 = nth(Loss_group, 1) + nth(Loss_group, 2)) %>% 
    arrange(desc(Cover2))


## IM_Excess Result  

ST_His_Excess <- His_result %>%       
    group_by(DATE, Member) %>%
    summarise(Loss_group = sum(His_ExcIM)) %>% 
    group_by(DATE) %>% 
    arrange(desc(Loss_group)) %>% 
    summarise(Cover2 = nth(Loss_group, 1) + nth(Loss_group, 2)) %>% 
    arrange(desc(Cover2))


ST_His_Excess$Cover2[1]   #증거금 초과위험 최대일



# 3-3. Fix Scn = Max Up & Max Dn & Lehman

## Direct Stress Test Result / IM_Excess Result

Fix_result <- ST_table[1280:1282] 

Fix_result <- cbind(ST_table[, 2], (abs(Fix_result) + Fix_result) / 2)    #모든 이득 0으로 변경

Fix_result <- melt(Fix_result)  #long form 변경

Fix_result <- Fix_result %>%    #exposure와 증거금초과액 계산
    rename(c('DATE' = variable,
             'Fix_Loss' = value)) %>%
    mutate(Fix_ExcIM = ifelse(Fix_Loss + ST_table$IM > 0, Fix_Loss + ST_table$IM, 0))


Fix_result$Member <-                                      #그룹사 고려
    case_when(
        Fix_result$Member == '신한금융투자' | 
            Fix_result$Member == '신한증권' | 
            Fix_result$Member == '신한은행' ~ '신한G',
        Fix_result$Member == 'KB증권' |
            Fix_result$Member == '국민은행' ~ '국민G',
        Fix_result$Member == '하나은행' |
            Fix_result$Member == '하나증권' |
            Fix_result$Member == '하나금융투자' ~ '하나G',
        Fix_result$Member == 'BNK증권' |
            Fix_result$Member == '부산은행' ~ '부산G',
        Fix_result$Member == '아이비케이투자증권' |
            Fix_result$Member == '중소기업은행' ~ '기업G',
        Fix_result$Member == 'NH투자증권' |
            Fix_result$Member == '농협은행' ~ 'NHG',
        TRUE ~ Fix_result$Member
    )

## Direct Result  

ST_Fix <- Fix_result %>%
    group_by(DATE, Member) %>%
    summarise(Loss_group = sum(Fix_Loss)) %>% 
    group_by(DATE) %>% 
    arrange(desc(Loss_group)) %>% 
    summarise(Cover2 = nth(Loss_group, 1) + nth(Loss_group, 2)) %>% 
    arrange(DATE)

## IM_Excess Result  

ST_Fix_Excess <- Fix_result %>%       
    group_by(DATE, Member) %>%
    summarise(Loss_group = sum(Fix_ExcIM)) %>% 
    group_by(DATE) %>% 
    arrange(desc(Loss_group)) %>% 
    summarise(Cover2 = nth(Loss_group, 1) + nth(Loss_group, 2)) %>% 
    arrange(DATE)


ST_Fix_Excess   







