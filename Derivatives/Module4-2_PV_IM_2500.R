# 할인계수, 선도금리 할당----------------------------------

##DF INDEX, 고정이자 미리 도출
DF_E <- match(CF_3NM$CF_3nM_E, Curve_KRW$DATE)
DF_S <- match(CF_3NM$CF_3nM_S, Curve_KRW$DATE)


core <- detectCores()
cluster <- makeCluster(spec = core) 
registerDoParallel(cl = cluster)

Result_scn_IM <-  foreach(scn_i = 1:nrow(Rate_IMscn), .packages = c('tidyverse', 'lubridate')) %dopar% {
    
    
    # 할인계수, 선도금리 할당
    CF_3NM[['DF']] <- with(CF_3NM, NPV_scn_IM[[scn_i]][DF_E, 'DF'])
    CF_3NM[['FR']] <-
        with(CF_3NM,
             (NPV_scn_IM[[scn_i]][DF_S, 'DF'] /
                  NPV_scn_IM[[scn_i]][DF_E, 'DF'] - 1) *
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

Result_scn_IM <- data.frame(do.call(cbind, Result_scn_IM))




# Calculate NPV Difference(IM)--------------------------------------------------

colnames(Result_scn_IM) <- c("Today", paste("scn_", c(1:2500), sep = ""))

Pos_KRW_FN_IM <- Pos_KRW[c(order(Pos_KRW$CLnum)),]

## 시나리오별 결과를 포지션 정보에 결합
Pos_KRW_FN_IM <- cbind(Pos_KRW_FN_IM, Result_scn_IM)   

# #### Row Order & NPV validation
# which(Pos_KRW_FN_IM$'9999-12-31' - Pos_KRW_FN_IM$NPV > 10000)    




#신뢰수준 99.9%일 때 증거금-----------------------------

## 고정지급쪽 계좌별 NPV 합계
NPV_sum_pay <- Pos_KRW_FN_IM %>%
    group_by(Pay_Accnum, Pay_Member) %>%
    summarise(across(Today : scn_2500, sum)) %>% 
    rename(Accnum = Pay_Accnum,
           Member = Pay_Member)

## 고정수취쪽 계좌별 NPV 합계(음수화)
NPV_sum_rec <- Pos_KRW_FN_IM %>%
    group_by(Rec_Accnum, Rec_Member) %>%
    summarise(across(Today : scn_2500, sum)) %>% 
    rename(Accnum = Rec_Accnum, 
           Member = Rec_Member)

NPV_sum_rec[, -c(1, 2)] <- - NPV_sum_rec[, -c(1, 2)] 

## 고정지급 NPV - 고정수취 NPV
NPV_sum_PminusR <- 
    rbind(NPV_sum_pay, NPV_sum_rec) %>% 
    group_by(Accnum, Member) %>%
    summarise(across(Today : scn_2500, sum))

rm(NPV_sum_pay, NPV_sum_rec)


##[OPTION] 신뢰도 조정 1 --------------------------------------------------
for (i in 1:nrow(NPV_sum_PminusR)) {
    NPV_sum_PminusR[i, 2504] <- sort(as.matrix(NPV_sum_PminusR[i, -c(1, 2)]))[8]
}


IM <- NPV_sum_PminusR %>%
    rename(Min = ...2504) %>%
    mutate(IM = ifelse(Min - Today <= 0, Min - Today, 0)) %>% 
    select(Accnum, Member, IM)

IM_data <- paste0('IM2500_', 
                  substr(BDate_chr, 3, 4),
                  substr(BDate_chr, 6, 7),
                  substr(BDate_chr, 9, 10),
                  ".Rdata")

Pos_IM_data <- paste0(
    'Pos_KRW_IM2500_',
    substr(BDate_chr, 3, 4),
    substr(BDate_chr, 6, 7),
    substr(BDate_chr, 9, 10),
    ".Rdata"
)

sum(IM$IM)

save(IM, file = IM_data)

save(Pos_KRW_FN_IM, file = Pos_IM_data)




##validation
#IM_mem <- IM %>% group_by(Member) %>% summarise(sum = sum(IM))


