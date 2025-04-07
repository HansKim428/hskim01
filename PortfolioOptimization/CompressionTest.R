library(readxl)
library(tidyverse)
library(hablar)
library(ggnetwork)
library(ggplot2)
library(network)
library(sna)
library(reshape2)
library(igraph)
library(tidygraph)
library(plotly)
library(RColorBrewer)
library(visNetwork)
library(scales)
options(digits = 22)


#MEMO
# 개별 회원의 Proposal 상에서 Close-out amount 합계는 0이 아님
# Partial Compression의 잔존명목금액은 1원 단위까지도 조절
# 회원이 제출한 close-out amount와 최종 Proposal 상의 amount 간에는 미세한 차이가 있음 -> 이걸 T-O가 조정하는데,
# 항상 Proposal 상의 COA는 회원 제출 Mtm 이상임(음수든 양수든)
# 통합 Proposal 상의 close-out amount 합계는 0임(CCP입장 = 즉 협의결제금액 합이 0, 다만 원거래 관련 양방의 협의결제금액은 다를 수 있음)
# 통합 Proposal에는 청산약정거래별 1건씩 적시되어 있으며(중복 청산번호 2개씩), Partial Compression 반영된 COA임

# COA >회원MtM ==> 어떠한 회원도 장부가보다 손해를 보지는 않음
# if( KRX MtM > COA ) then 협의결제'지급'
# 그동안의 CCP 일일정산금액을 되돌리고, 회원 자체 MtM을 최대한 존중하여 종료 정산



###### 1. Basic Info & Load Files ######

Test_N <- 3     #21.2nd = 2, 22.1st = 3, 22.2nd = 3

# Margin Rates

Margin_mat <- c(1:24)
Margin_rate_21.3Q <- c(0.01, 0.05, 0.09, 0.13, 0.22, 0.33, 0.55, 0.78, 1.06, 1.29, 1.54, 1.79, 2.05, 2.28, 2.61, 2.92, 3.02, 3.09, 3.18, 3.35, 3.52, 3.78, 4.02, 4.25)
Margin_rate_22.2Q <- c(0.01, 0.06, 0.10, 0.16, 0.28, 0.43, 0.78, 1.11, 1.41, 1.59, 1.82, 2.04, 2.25, 2.39, 2.64, 2.94, 3.02, 3.13, 3.28, 3.54, 3.76, 4.01, 4.22, 4.48)
Margin_rate <- rbind(Margin_mat, Margin_rate_21.3Q, Margin_rate_22.2Q)


# Trade Files


###21.2nd
# Tadd <- "Test_21-02/Info_mutate/"    # "mutate" == File Names corrected
# Padd <- "Test_21-02/Proposal_mutate/"
# Proposal <- data.frame(read_csv("Test_21-02/Proposal_mutate/Consolidated_Unwind_Proposal.csv"))

# ###22.1st
# Tadd <- "Test_22-01/Info_mutate/"    # "mutate" == File Names corrected
# Padd <- "Test_22-01/Proposal_mutate/"
# Proposal <- data.frame(read_csv("Test_22-01/Proposal_mutate/Consolidated_Unwind_Proposal.csv"))

###22.2nd
Tadd <- "Test_22-02/Info_mutate/"    # "mutate" == File Names corrected
Padd <- "Test_22-02/Proposal_mutate/"
Proposal <- data.frame(read_csv("Test_22-02/Proposal_mutate/Consolidated_Unwind_Proposal.csv"))



### 21.2nd Compression Test(16 Members)

# add_T_104 <- paste(Tadd, "KRX104_Trade.xlsx", sep="")
# add_T_199 <- paste(Tadd, "KRX199_Trade.xlsx", sep="")
# add_T_202 <- paste(Tadd, "KRX202_Trade.csv", sep="")
# add_T_219 <- paste(Tadd, "KRX219_Trade.xlsx", sep="")
# add_T_355 <- paste(Tadd, "KRX355_Trade.xlsx", sep="")
# add_T_378 <- paste(Tadd, "KRX378_Trade.csv", sep="")
# add_T_409 <- paste(Tadd, "KRX409_Trade.xlsx", sep="")
# add_T_676 <- paste(Tadd, "KRX676_Trade.xlsx", sep="")
# add_T_706 <- paste(Tadd, "KRX706_Trade.xlsx", sep="")
# add_T_728 <- paste(Tadd, "KRX728_Trade.csv", sep="")
# add_T_744 <- paste(Tadd, "KRX744_Trade.xlsx", sep="")
# add_T_825 <- paste(Tadd, "KRX825_Trade.xlsx", sep="")
# add_T_888 <- paste(Tadd, "KRX888_Trade.csv", sep="")
# add_T_909 <- paste(Tadd, "KRX909_Trade.csv", sep="")
# add_T_924 <- paste(Tadd, "KRX924_Trade.xlsx", sep="")
# add_T_952 <- paste(Tadd, "KRX952_Trade.xls", sep="")
# 
# T_104 <- read_xlsx(add_T_104)
# T_199 <- read_xlsx(add_T_199)
# T_202 <- read_csv(add_T_202)
# T_219 <- read_xlsx(add_T_219)
# T_355 <- read_xlsx(add_T_355)
# T_378 <- read_csv(add_T_378)
# T_409 <- read_xlsx(add_T_409)
# T_676 <- read_xlsx(add_T_676)
# T_706 <- read_xlsx(add_T_706)
# T_728 <- read_csv(add_T_728)
# T_744 <- read_xlsx(add_T_744)
# T_825 <- read_xlsx(add_T_825)
# T_888 <- read_csv(add_T_888)
# T_909 <- read_csv(add_T_909)
# T_924 <- read_xlsx(add_T_924)
# T_952 <- read_xls(add_T_952)
# 
# 
# T_list <- list(T_104,T_199,T_202,T_219,T_355,T_378,T_409,T_676,T_706,T_728,T_744,T_825,
#               T_888,T_909,T_924,T_952)

### c(104, 199, 202, 219, 355, 378, 409, 676, 706, 728, 744, 825, 888, 909, 924, 952)
### T_104,T_199,T_202,T_219,T_355,T_378,T_409,T_676,T_706,T_728,T_744,T_825,T_888,T_909,T_924,T_952



# 22.1st Compression Test(13 Members)

# add_T_093 <- paste(Tadd, "KRX093_Trade.csv", sep="")     ##Member Trade Files Address
# add_T_142 <- paste(Tadd, "KRX142_Trade.csv", sep="")
# add_T_182 <- paste(Tadd, "KRX182_Trade.csv", sep="")
# add_T_224 <- paste(Tadd, "KRX224_Trade.csv", sep="")
# add_T_234 <- paste(Tadd, "KRX234_Trade.csv", sep="")
# add_T_465 <- paste(Tadd, "KRX465_Trade.csv", sep="")
# add_T_560 <- paste(Tadd, "KRX560_Trade.csv", sep="")
# add_T_574 <- paste(Tadd, "KRX574_Trade.csv", sep="")
# add_T_589 <- paste(Tadd, "KRX589_Trade.csv", sep="")
# add_T_628 <- paste(Tadd, "KRX628_Trade.csv", sep="")
# add_T_640 <- paste(Tadd, "KRX640_Trade.csv", sep="")
# add_T_869 <- paste(Tadd, "KRX869_Trade.csv", sep="")
# add_T_902 <- paste(Tadd, "KRX902_Trade.csv", sep="")
# 
# 
# T_093 <- data.frame(read_csv(add_T_093))       ##Live Execution Stage_Trade Files
# T_142 <- data.frame(read_csv(add_T_142))
# T_182 <- data.frame(read_csv(add_T_182))
# T_224 <- data.frame(read_csv(add_T_224))
# T_234 <- data.frame(read_csv(add_T_234))
# T_465 <- data.frame(read_csv(add_T_465))
# T_560 <- data.frame(read_csv(add_T_560))
# T_574 <- data.frame(read_csv(add_T_574))
# T_589 <- data.frame(read_csv(add_T_589))
# T_628 <- data.frame(read_csv(add_T_628))
# T_640 <- data.frame(read_csv(add_T_640))
# T_869 <- data.frame(read_csv(add_T_869))
# T_902 <- data.frame(read_csv(add_T_902))
# 
# 
# T_list <- list(T_093,T_142,T_182,T_224,T_234,T_465,T_560,T_574,T_589,T_628,T_640,T_869,T_902)     ##Trade File List




# 22.1st Compression Test(13 Members)

add_T_093 <- paste(Tadd, "KRX093_Trade.csv", sep="")     ##Member Trade Files Address
add_T_142 <- paste(Tadd, "KRX142_Trade.csv", sep="")
add_T_182 <- paste(Tadd, "KRX182_Trade.csv", sep="")
add_T_224 <- paste(Tadd, "KRX224_Trade.csv", sep="")
add_T_234 <- paste(Tadd, "KRX234_Trade.csv", sep="")
add_T_304 <- paste(Tadd, "KRX304_Trade.csv", sep="")
add_T_354 <- paste(Tadd, "KRX354_Trade.csv", sep="")
add_T_436 <- paste(Tadd, "KRX436_Trade.csv", sep="")
add_T_465 <- paste(Tadd, "KRX465_Trade.csv", sep="")
add_T_496 <- paste(Tadd, "KRX496_Trade.csv", sep="")
add_T_533 <- paste(Tadd, "KRX533_Trade.csv", sep="")
add_T_546 <- paste(Tadd, "KRX546_Trade.csv", sep="")
add_T_560 <- paste(Tadd, "KRX560_Trade.csv", sep="")
add_T_574 <- paste(Tadd, "KRX574_Trade.csv", sep="")
add_T_589 <- paste(Tadd, "KRX589_Trade.csv", sep="")
add_T_628 <- paste(Tadd, "KRX628_Trade.csv", sep="")
add_T_640 <- paste(Tadd, "KRX640_Trade.csv", sep="")
add_T_753 <- paste(Tadd, "KRX753_Trade.csv", sep="")
add_T_766 <- paste(Tadd, "KRX766_Trade.csv", sep="")
add_T_848 <- paste(Tadd, "KRX848_Trade.csv", sep="")
add_T_869 <- paste(Tadd, "KRX869_Trade.csv", sep="")
add_T_902 <- paste(Tadd, "KRX902_Trade.csv", sep="")


T_093 <- data.frame(read_csv(add_T_093))       ##Live Execution Stage_Trade Files
T_142 <- data.frame(read_csv(add_T_142))
T_182 <- data.frame(read_csv(add_T_182))
T_224 <- data.frame(read_csv(add_T_224))
T_234 <- data.frame(read_csv(add_T_234))
T_304 <- data.frame(read_csv(add_T_304))
T_354 <- data.frame(read_csv(add_T_354))
T_436 <- data.frame(read_csv(add_T_436))
T_465 <- data.frame(read_csv(add_T_465))
T_496 <- data.frame(read_csv(add_T_496))
T_533 <- data.frame(read_csv(add_T_533))
T_546 <- data.frame(read_csv(add_T_546))
T_560 <- data.frame(read_csv(add_T_560))
T_574 <- data.frame(read_csv(add_T_574))
T_589 <- data.frame(read_csv(add_T_589))
T_628 <- data.frame(read_csv(add_T_628))
T_640 <- data.frame(read_csv(add_T_640))
T_753 <- data.frame(read_csv(add_T_753))
T_766 <- data.frame(read_csv(add_T_766))
T_848 <- data.frame(read_csv(add_T_848))
T_869 <- data.frame(read_csv(add_T_869))
T_902 <- data.frame(read_csv(add_T_902))


T_list <- list(T_093,T_142,T_182,T_224,T_234,T_304,T_354,T_436,T_465,T_496,T_533,T_546,T_560,T_574,T_589,T_628,T_640,T_753,T_766,T_848,T_869,T_902)




for (i in seq_along(T_list)) {
    T_list[[i]] <- T_list[[i]] %>%      
        mutate(TRADE_DATE = as.Date(TRADE_DATE, format = "%d.%m.%Y"),       ##Class Convert(DATE)
               START_DATE = as.Date(START_DATE, format = "%d.%m.%Y"),
               END_DATE = as.Date(END_DATE, format = "%d.%m.%Y"),
               NEXT_FIXING_DATE = as.Date(NEXT_FIXING_DATE, format = "%d.%m.%Y"),
               NEXT_FLOAT_PAY_DATE = as.Date(NEXT_FLOAT_PAY_DATE, format = "%d.%m.%Y"),
               NEXT_FIX_PAY_DATE = as.Date(NEXT_FIX_PAY_DATE, format = "%d.%m.%Y"),
               MTM_DATE = as.Date(MTM_DATE, format = "%d.%m.%Y"),
               Mat_rem = as.integer(END_DATE - MTM_DATE - 1)) %>%     ##Calculate Maturity remain - Assumption : D-Day is the next day of MTM_DATE
        select(
            "TRADE_ID",
            'RISK_UNIT',
            'MTM_VALUE',
            'PARTY_ID',
            'CP_ID',
            'NOTIONAL',
            'PAY_REC',
            'COUPON_RATE',
            'DAYCOUNT',
            'FLOAT_RATE',
            'TRADE_DATE',
            'START_DATE',
            'END_DATE',
            'ROLL_DATE',
            'NEXT_FIXING_DATE',
            'NEXT_FLOAT_PAY_DATE',
            'NEXT_FIX_PAY_DATE',
            'Mat_rem')
}



T_con <- data.table::rbindlist(T_list)      ##Consolidated Trade Files from Members




###### 2. Trade Analysis ######


nrow(T_con)                             #LE Possible Compressed Trades Count(축약대상거래건수_LE)
apply(T_con[, 6], 2, sum) / 10 ^ 8      #LE Possible Compressed Trades NA(축약대상거래금액_LE)




###### 3. Proposal Analysis ######

# 3-1. Overview

Proposal <- Proposal %>%
    mutate(
        Unwind_NA = ORIGINAL_NOTIONAL - PAY_NOTIONAL,    #Unwind Notional Amount(Full & Partial)
        Unwind_NA_partial = ifelse(                      #Unwind Notional Amount(Partial)
            TRADE_COMPRESSION_TYPE == 'Residual',
            ORIGINAL_NOTIONAL - PAY_NOTIONAL,
            0),
        Maturity = T_con[match(CCP_TRADE_ID, T_con$TRADE_ID), 18],     #Maturity remain
        Margin_mat = case_when(
            Maturity <= 91 ~ 1,
            Maturity <= 183 ~ 2,
            Maturity <= 274 ~ 3,
            Maturity <= 365 ~ 4,
            Maturity <= 547 ~ 5,
            Maturity <= 730 ~ 6,
            Maturity <= 1095 ~ 7,
            Maturity <= 1461 ~ 8,
            Maturity <= 1826 ~ 9,
            Maturity <= 2191 ~ 10,
            Maturity <= 2556 ~ 11,
            Maturity <= 2922 ~ 12,
            Maturity <= 3287 ~ 13,
            Maturity <= 3652 ~ 14,
            Maturity <= 4017 ~ 15,
            Maturity <= 4383 ~ 16,
            Maturity <= 4748 ~ 17,
            Maturity <= 5113 ~ 18,
            Maturity <= 5478 ~ 19,
            Maturity <= 5844 ~ 20,
            Maturity <= 6209 ~ 21,
            Maturity <= 6574 ~ 22,
            Maturity <= 6939 ~ 23,
            TRUE ~ 24)
    )

###c(91,183,274,365,547,730,1095,1461,1826,2191,2556,2922,3287,3652,4017,4383,4748,5113,5478,5844,6209,6574,6939,7330)

apply(Proposal[, c(21, 23, 61, 62)], 2, sum)          #Proposal_Original NA / NA Remain / NA Unwind / NA Unwind_Partial

count(Proposal, TRADE_COMPRESSION_TYPE == 'Residual') #Partial Unwind Trades Count


Compressedfixed <- Proposal %>%                       #Unwind FIXED Amount(per Member)
    filter(PAY_LEG_TYPE == 'Fixed') %>% 
    group_by(TRIOPTIMA_PARTY_NAME) %>% 
    summarise(sum_fixed = sum(Unwind_NA) / 10 ^ 8)

sum(Compressedfixed$sum_fixed)  #Validation


Compressedfloat <- Proposal %>%                       #Unwind FLOAT Amount(per Member)
    filter(PAY_LEG_TYPE == 'Floating') %>% 
    group_by(TRIOPTIMA_PARTY_NAME) %>% 
    summarise(sum_float = sum(Unwind_NA) / 10 ^ 8)

sum(Compressedfloat$sum_float)  #Validation





# 3-2. Big 'Compressed Fixed/Float NA rate' members -> compressed Maturity analysis

Compressedfixed_Mat_KRX182 <- Proposal %>% 
    filter(PAY_LEG_TYPE == 'Fixed' & TRIOPTIMA_PARTY_NAME == 'KRX224') %>%
    group_by(Margin_mat) %>% 
    summarise(FIXED = sum(Unwind_NA) / 10 ^ 8)

sum(Compressedfixed_Mat_KRX182$FIXED)   #Validation

Compressedfloat_Mat_KRX182 <- Proposal %>% 
    filter(PAY_LEG_TYPE == 'Floating' & TRIOPTIMA_PARTY_NAME == 'KRX224') %>%
    group_by(Margin_mat) %>% 
    summarise(FLOATING = sum(Unwind_NA) / 10 ^ 8)

sum(Compressedfloat_Mat_KRX182$FLOATING)   #Validation

Compressed_Mat_KRX182 <- full_join(Compressedfixed_Mat_KRX182, Compressedfloat_Mat_KRX182)

Compressed_Mat_KRX182 <- Compressed_Mat_KRX182 %>% 
    mutate(Margin_mat = case_when(Margin_mat == 1 ~ '0',
                                  Margin_mat == 2 ~ '3',
                                  Margin_mat == 3 ~ '6',
                                  Margin_mat == 4 ~ '9',
                                  Margin_mat == 5 ~ '12',
                                  Margin_mat == 6 ~ '18',
                                  Margin_mat == 7 ~ '24',
                                  Margin_mat == 8 ~ '36', 
                                  Margin_mat == 9 ~ '48',
                                  Margin_mat == 10 ~ '60', 
                                  Margin_mat == 11 ~ '72',
                                  Margin_mat == 12 ~ '84',
                                  Margin_mat == 13 ~ '96',
                                  Margin_mat == 14 ~ '108',
                                  Margin_mat == 15 ~ '120',
                                  Margin_mat == 16 ~ '144',
                                  Margin_mat == 17 ~ '180',
                                  TRUE ~ '240'))


Margin_mat <- data.frame(Margin_mat = c('0', '3', '6', '9', '12', '18', '24', '36', '48', '60', '72', '84', '96', '108', '120', '144', '180', '240'))

Compressed_Mat_KRX182 <- left_join(Margin_mat, Compressed_Mat_KRX182)
Compressed_Mat_KRX182$Margin_mat <- factor(Compressed_Mat_KRX182$Margin_mat, levels = unique(Compressed_Mat_KRX182$Margin_mat))   #Remain Current Order as Factor -> x axis order
Compressed_Mat_KRX182[is.na(Compressed_Mat_KRX182)] <- 0


Compressed_Mat_KRX182_Graph <- Compressed_Mat_KRX182 %>% 
    melt(id = 'Margin_mat') %>% 
    ggplot(aes(x = Margin_mat, y = value, fill = variable)) +
    geom_col(position = "dodge", width = 0.7) +
    scale_fill_hue(direction = 1) +
    scale_y_continuous(labels = comma) +
    labs(x = "Maturity", y = "Compressed Amount", fill = "") +
    theme_minimal() +
    theme(legend.position = c(0.85, 0.85),
          legend.text = element_text(size = 20),
          axis.title = element_text(size = 25),
          axis.text.x = element_text(size = 17),
          axis.text.y = element_text(size = 17))

    
Compressed_Mat_KRX182_Graph





# 3-3. Concentration _ Big N members 


##21.2nd test - 'NH투자', '노무라투자', 'JPM', 'SC은행'

# BigN_21.2nd <- Proposal %>%
#     filter(TRIOPTIMA_PARTY_NAME == 'KRX378' |
#            TRIOPTIMA_PARTY_NAME == 'KRX706' |
#            TRIOPTIMA_PARTY_NAME == 'KRX744' |
#            TRIOPTIMA_PARTY_NAME == 'KRX409') %>%
#     filter(TRIOPTIMA_CP_NAME == 'KRX409' |
#            TRIOPTIMA_CP_NAME == 'KRX744' |
#            TRIOPTIMA_CP_NAME == 'KRX706' |
#            TRIOPTIMA_CP_NAME == 'KRX378')
# 
# sum(BigN_21.2nd$Unwind_NA) / 10^8


# ##22.1st test - '메리츠증권', '노무라투자', 'JPM'
# 
# BigN_22.1st <- Proposal %>%
#     filter(TRIOPTIMA_PARTY_NAME == 'KRX093' |
#            TRIOPTIMA_PARTY_NAME == 'KRX234' |
#            TRIOPTIMA_PARTY_NAME == 'KRX628') %>%
#     filter(TRIOPTIMA_CP_NAME == 'KRX093' |
#            TRIOPTIMA_CP_NAME == 'KRX234' |
#            TRIOPTIMA_CP_NAME == 'KRX628' )
# 
# sum(BigN_22.1st$Unwind_NA) / 10^8


##22.2nd test - '메리츠증권', '노무라투자', 'JPM', 'BNP'

BigN_22.2nd <- Proposal %>%
    filter(TRIOPTIMA_PARTY_NAME == 'KRX093' |
           TRIOPTIMA_PARTY_NAME == 'KRX234' |
           TRIOPTIMA_PARTY_NAME == 'KRX902' |
           TRIOPTIMA_PARTY_NAME == 'KRX628' ) %>%
    filter(TRIOPTIMA_CP_NAME == 'KRX093' |
           TRIOPTIMA_CP_NAME == 'KRX234'|
           TRIOPTIMA_CP_NAME == 'KRX902'|
           TRIOPTIMA_CP_NAME == 'KRX628')

sum(BigN_22.2nd$Unwind_NA) / 10^8




# 3-4. Proposal Network Graph

###21.2nd

# Proposal_Network <- Proposal %>%
#     mutate(TRIOPTIMA_PARTY_NAME = case_when(TRIOPTIMA_PARTY_NAME == 'KRX202' ~ '교보',
#                                             TRIOPTIMA_PARTY_NAME == 'KRX909' ~ '한투',
#                                             TRIOPTIMA_PARTY_NAME == 'KRX219' ~ '신영',
#                                             TRIOPTIMA_PARTY_NAME == 'KRX378' ~ 'NH투자',
#                                             TRIOPTIMA_PARTY_NAME == 'KRX728' ~ 'KB증권',
#                                             TRIOPTIMA_PARTY_NAME == 'KRX706' ~ '노무라',
#                                             TRIOPTIMA_PARTY_NAME == 'KRX744' ~ 'SC',
#                                             TRIOPTIMA_PARTY_NAME == 'KRX409' ~ 'JPM',
#                                             TRIOPTIMA_PARTY_NAME == 'KRX355' ~ 'SG',
#                                             TRIOPTIMA_PARTY_NAME == 'KRX952' ~ '미즈호',
#                                             TRIOPTIMA_PARTY_NAME == 'KRX104' ~ '모간',
#                                             TRIOPTIMA_PARTY_NAME == 'KRX199' ~ '미래',
#                                             TRIOPTIMA_PARTY_NAME == 'KRX676' ~ '도이치',
#                                             TRIOPTIMA_PARTY_NAME == 'KRX825' ~ '산은',
#                                             TRIOPTIMA_PARTY_NAME == 'KRX888' ~ 'KTB',
#                                             TRIOPTIMA_PARTY_NAME == 'KRX924' ~ '씨티'),
#            TRIOPTIMA_CP_NAME = case_when(TRIOPTIMA_CP_NAME == 'KRX202' ~ '교보',
#                                          TRIOPTIMA_CP_NAME == 'KRX909' ~ '한투',
#                                          TRIOPTIMA_CP_NAME == 'KRX219' ~ '신영',
#                                          TRIOPTIMA_CP_NAME == 'KRX378' ~ 'NH투자',
#                                          TRIOPTIMA_CP_NAME == 'KRX728' ~ 'KB증권',
#                                          TRIOPTIMA_CP_NAME == 'KRX706' ~ '노무라',
#                                          TRIOPTIMA_CP_NAME == 'KRX744' ~ 'SC',
#                                          TRIOPTIMA_CP_NAME == 'KRX409' ~ 'JPM',
#                                          TRIOPTIMA_CP_NAME == 'KRX355' ~ 'SG',
#                                          TRIOPTIMA_CP_NAME == 'KRX952' ~ '미즈호',
#                                          TRIOPTIMA_CP_NAME == 'KRX104' ~ '모간',
#                                          TRIOPTIMA_CP_NAME == 'KRX199' ~ '미래',
#                                          TRIOPTIMA_CP_NAME == 'KRX676' ~ '도이치',
#                                          TRIOPTIMA_CP_NAME == 'KRX825' ~ '산은',
#                                          TRIOPTIMA_CP_NAME == 'KRX888' ~ 'KTB',
#                                          TRIOPTIMA_CP_NAME == 'KRX924' ~ '씨티')) %>%
#     filter(PAY_LEG_TYPE == 'Fixed') %>%
#     select(TRIOPTIMA_PARTY_NAME, TRIOPTIMA_CP_NAME, Unwind_NA) %>%
#     group_by(TRIOPTIMA_PARTY_NAME, TRIOPTIMA_CP_NAME) %>%
#     summarise(Sum = sum(Unwind_NA))



# ###22.1st
# 
# Proposal_Network <- Proposal %>%
#     mutate(TRIOPTIMA_PARTY_NAME = case_when(TRIOPTIMA_PARTY_NAME == 'KRX224' ~ '교보',
#                                             TRIOPTIMA_PARTY_NAME == 'KRX589' ~ '한투',
#                                             TRIOPTIMA_PARTY_NAME == 'KRX560' ~ '신영',
#                                             TRIOPTIMA_PARTY_NAME == 'KRX093' ~ '메리츠',
#                                             TRIOPTIMA_PARTY_NAME == 'KRX142' ~ 'NH투자',
#                                             TRIOPTIMA_PARTY_NAME == 'KRX640' ~ 'KB증권',
#                                             TRIOPTIMA_PARTY_NAME == 'KRX234' ~ '노무라',
#                                             TRIOPTIMA_PARTY_NAME == 'KRX902' ~ 'BNP',
#                                             TRIOPTIMA_PARTY_NAME == 'KRX182' ~ '우리은행',
#                                             TRIOPTIMA_PARTY_NAME == 'KRX869' ~ 'SC',
#                                             TRIOPTIMA_PARTY_NAME == 'KRX628' ~ 'JPM',
#                                             TRIOPTIMA_PARTY_NAME == 'KRX465' ~ 'SG',
#                                             TRIOPTIMA_PARTY_NAME == 'KRX574' ~ '미즈호'),
#            TRIOPTIMA_CP_NAME = case_when(TRIOPTIMA_CP_NAME == 'KRX224' ~ '교보',
#                                          TRIOPTIMA_CP_NAME == 'KRX589' ~ '한투',
#                                          TRIOPTIMA_CP_NAME == 'KRX560' ~ '신영',
#                                          TRIOPTIMA_CP_NAME == 'KRX093' ~ '메리츠',
#                                          TRIOPTIMA_CP_NAME == 'KRX142' ~ 'NH투자',
#                                          TRIOPTIMA_CP_NAME == 'KRX640' ~ 'KB증권',
#                                          TRIOPTIMA_CP_NAME == 'KRX234' ~ '노무라',
#                                          TRIOPTIMA_CP_NAME == 'KRX902' ~ 'BNP',
#                                          TRIOPTIMA_CP_NAME == 'KRX182' ~ '우리은행',
#                                          TRIOPTIMA_CP_NAME == 'KRX869' ~ 'SC',
#                                          TRIOPTIMA_CP_NAME == 'KRX628' ~ 'JPM',
#                                          TRIOPTIMA_CP_NAME == 'KRX465' ~ 'SG',
#                                          TRIOPTIMA_CP_NAME == 'KRX574' ~ '미즈호')) %>%
#     filter(PAY_LEG_TYPE == 'Fixed') %>%
#     select(TRIOPTIMA_PARTY_NAME, TRIOPTIMA_CP_NAME, Unwind_NA) %>%
#     group_by(TRIOPTIMA_PARTY_NAME, TRIOPTIMA_CP_NAME) %>%
#     summarise(Sum = sum(Unwind_NA))



###22.2nd


Proposal_Network <- Proposal %>%
    mutate(TRIOPTIMA_PARTY_NAME = case_when(TRIOPTIMA_PARTY_NAME == 'KRX224' ~ '교보',
                                            TRIOPTIMA_PARTY_NAME == 'KRX304' ~ '신한금투',
                                            TRIOPTIMA_PARTY_NAME == 'KRX354' ~ '미래',
                                            TRIOPTIMA_PARTY_NAME == 'KRX589' ~ '한투',
                                            TRIOPTIMA_PARTY_NAME == 'KRX560' ~ '신영',
                                            TRIOPTIMA_PARTY_NAME == 'KRX093' ~ '메리츠',
                                            TRIOPTIMA_PARTY_NAME == 'KRX142' ~ 'NH투자',
                                            TRIOPTIMA_PARTY_NAME == 'KRX640' ~ 'KB증권',
                                            TRIOPTIMA_PARTY_NAME == 'KRX234' ~ '노무라',
                                            TRIOPTIMA_PARTY_NAME == 'KRX533' ~ '다올',
                                            TRIOPTIMA_PARTY_NAME == 'KRX848' ~ '농협',
                                            TRIOPTIMA_PARTY_NAME == 'KRX902' ~ 'BNP',
                                            TRIOPTIMA_PARTY_NAME == 'KRX496' ~ '산은',
                                            TRIOPTIMA_PARTY_NAME == 'KRX182' ~ '우리은행',
                                            TRIOPTIMA_PARTY_NAME == 'KRX753' ~ '도이치',
                                            TRIOPTIMA_PARTY_NAME == 'KRX869' ~ 'SC',
                                            TRIOPTIMA_PARTY_NAME == 'KRX628' ~ 'JPM',
                                            TRIOPTIMA_PARTY_NAME == 'KRX546' ~ 'CS',
                                            TRIOPTIMA_PARTY_NAME == 'KRX766' ~ 'BOA',
                                            TRIOPTIMA_PARTY_NAME == 'KRX465' ~ 'SG',
                                            TRIOPTIMA_PARTY_NAME == 'KRX436' ~ '모간스탠리',
                                            TRIOPTIMA_PARTY_NAME == 'KRX574' ~ '미즈호'),
           TRIOPTIMA_CP_NAME = case_when(TRIOPTIMA_CP_NAME == 'KRX224' ~ '교보',
                                         TRIOPTIMA_CP_NAME == 'KRX304' ~ '신한금투',
                                         TRIOPTIMA_CP_NAME == 'KRX354' ~ '미래',
                                         TRIOPTIMA_CP_NAME == 'KRX589' ~ '한투',
                                         TRIOPTIMA_CP_NAME == 'KRX560' ~ '신영',
                                         TRIOPTIMA_CP_NAME == 'KRX093' ~ '메리츠',
                                         TRIOPTIMA_CP_NAME == 'KRX142' ~ 'NH투자',
                                         TRIOPTIMA_CP_NAME == 'KRX640' ~ 'KB증권',
                                         TRIOPTIMA_CP_NAME == 'KRX234' ~ '노무라',
                                         TRIOPTIMA_CP_NAME == 'KRX533' ~ '다올',
                                         TRIOPTIMA_CP_NAME == 'KRX848' ~ '농협',
                                         TRIOPTIMA_CP_NAME == 'KRX902' ~ 'BNP',
                                         TRIOPTIMA_CP_NAME == 'KRX496' ~ '산은',
                                         TRIOPTIMA_CP_NAME == 'KRX182' ~ '우리은행',
                                         TRIOPTIMA_CP_NAME == 'KRX753' ~ '도이치',
                                         TRIOPTIMA_CP_NAME == 'KRX869' ~ 'SC',
                                         TRIOPTIMA_CP_NAME == 'KRX628' ~ 'JPM',
                                         TRIOPTIMA_CP_NAME == 'KRX546' ~ 'CS',
                                         TRIOPTIMA_CP_NAME == 'KRX766' ~ 'BOA',
                                         TRIOPTIMA_CP_NAME == 'KRX465' ~ 'SG',
                                         TRIOPTIMA_CP_NAME == 'KRX436' ~ '모간스탠리',
                                         TRIOPTIMA_CP_NAME == 'KRX574' ~ '미즈호')) %>%
    filter(PAY_LEG_TYPE == 'Fixed') %>%
    select(TRIOPTIMA_PARTY_NAME, TRIOPTIMA_CP_NAME, Unwind_NA) %>%
    group_by(TRIOPTIMA_PARTY_NAME, TRIOPTIMA_CP_NAME) %>%
    summarise(Sum = sum(Unwind_NA))





Proposal_Network <- acast(Proposal_Network, TRIOPTIMA_PARTY_NAME ~ TRIOPTIMA_CP_NAME, fill = 0)

max(Proposal_Network)

NET <- graph_from_adjacency_matrix(Proposal_Network,
                                   mode = 'directed',
                                   weighted = 'Sum',
                                   diag = FALSE
)

E(NET)$width <- E(NET)$Sum ^ 1.5  / 1000000000000000000
E(NET)$arrow.mode = 2
E(NET)$color = 'darkturquoise'

deg <- (degree(NET, mode="all") - 8) ^2 / 15  #최소 degree - 1 로 변경 필요

V(NET)$size = deg
V(NET)$color = 'skyblue'
V(NET)$font.size = 30
V(NET)$font.color = 'black'


permute(NET, permutation = sample(vcount(NET))) %>% 
    visIgraph(layout = "layout_in_circle",
              smooth = TRUE,
              idToLabel = FALSE) %>% 
    visOptions(highlightNearest = TRUE) 


# set.seed(100)
# plot(NET,
#     layout = layout.circle(NET),
#     edge.curved = 0.2,
#     edge.arrow.size = 0.5,
#     edge.color = 'yellow',
#     vertex.color = 'white',
#     vertex.shape = 'circle',
#     vertex.frame.color = 'black',
#     vertex.label.color = 'black',
#     vertex.label.font = 2,
#     vertex.label.cex = 1.5)

# 
# cut.off <- mean(Proposal_Network)
# NET_cut.off <- delete.edges(NET, E(NET)[Sum < cut.off])




###### 4. Total Value Variation Margin Calculation ###### 


Proposal <- left_join(Proposal, t(Margin_rate[c(1, Test_N),]), by = 'Margin_mat', copy = TRUE)   #Margin Interval 
names(Proposal)[65] <- ("Margin_rate")


# Margin per Trades

Proposal <- Proposal %>%
    mutate(Margin = ifelse(PAY_LEG_TYPE == 'Fixed',
                           Unwind_NA * Margin_rate / 100,
                           - Unwind_NA * Margin_rate / 100))


TV_Margin <- Proposal %>%
    group_by(TRIOPTIMA_PARTY_NAME, Margin_mat) %>% 
    summarise(MAR = abs(sum(Margin)))    #Margin per 'Member & Margin Interval'


TV_Margin_Member <- TV_Margin %>% 
    group_by(TRIOPTIMA_PARTY_NAME) %>% 
    summarise(MAR_sum = sum(round(MAR, digits = 0)))    #Margin per Member


summarise(TV_Margin_Member, Margin = sum(MAR_sum))   #Entire Margin(Validation)

write_excel_csv(TV_Margin_Member, 'TV_Margin_22.2nd.csv')





###### 5. PV01 Analysis ###### 
######------------------------Position_KRW_FN.Rdata load

#유안타 고민해봐야

Pos_KRW_FN_sub <- Pos_KRW_FN[, c(4, 33:51)]

## Implement Partial PV01 and Correct PV01 Direction

Proposal_PV01 <-
    left_join(Proposal, Pos_KRW_FN_sub, by = c("CCP_TRADE_ID" = "CLnum")) %>%
    mutate(Unwind_rate = Unwind_NA / ORIGINAL_NOTIONAL)            

Proposal_PV01[, c(67:85)] <-
    Proposal_PV01[, c(67:85)] * 
    Proposal_PV01$Unwind_rate *
    ifelse(Proposal_PV01$PAY_LEG_TYPE == 'Fixed', 1, -1)            




##5 - 1. Compressed PV01


sum(Proposal_PV01$Up_Prl)  # Validation == 0


Compressed_PV01 <- Proposal_PV01 %>%                       #Unwind FIXED PV01(per Member)
    group_by(TRIOPTIMA_PARTY_NAME) %>% 
    summarise(PV_Call = sum(Up_Call),
              PV_CD = sum(Up_CD),
              PV_6M = sum(Up_6M),
              PV_9M = sum(Up_9M),
              PV_1Y = sum(Up_1Y),
              PV_18M = sum(Up_18M),
              PV_2Y = sum(Up_2Y),
              PV_3Y = sum(Up_3Y),
              PV_4Y = sum(Up_4Y),
              PV_5Y = sum(Up_5Y),
              PV_6Y = sum(Up_6Y),
              PV_7Y = sum(Up_7Y),
              PV_8Y = sum(Up_8Y),
              PV_9Y = sum(Up_9Y),
              PV_10Y = sum(Up_10Y),
              PV_12Y = sum(Up_12Y),
              PV_15Y = sum(Up_15Y),
              PV_20Y = sum(Up_20Y),
              PVBP = sum(Up_Prl))

###21.2nd

# Compressed_PV01 <- Compressed_PV01 %>%
#     mutate(TRIOPTIMA_PARTY_NAME = case_when(
#         TRIOPTIMA_PARTY_NAME == 'KRX202' ~ '교보증권',
#         TRIOPTIMA_PARTY_NAME == 'KRX909' ~ '한국투자증권',
#         TRIOPTIMA_PARTY_NAME == 'KRX219' ~ '신영증권',
#         TRIOPTIMA_PARTY_NAME == 'KRX378' ~ 'NH투자증권',
#         TRIOPTIMA_PARTY_NAME == 'KRX728' ~ 'KB증권',
#         TRIOPTIMA_PARTY_NAME == 'KRX706' ~ '노무라금융투자',
#         TRIOPTIMA_PARTY_NAME == 'KRX744' ~ 'SC은행',
#         TRIOPTIMA_PARTY_NAME == 'KRX409' ~ '제이피모간체이스은행',
#         TRIOPTIMA_PARTY_NAME == 'KRX355' ~ 'SG 은행',
#         TRIOPTIMA_PARTY_NAME == 'KRX952' ~ '미즈호은행',
#         TRIOPTIMA_PARTY_NAME == 'KRX104' ~ '모간스탠리은행',
#         TRIOPTIMA_PARTY_NAME == 'KRX199' ~ '미래에셋증권',
#         TRIOPTIMA_PARTY_NAME == 'KRX676' ~ '도이치은행서울지점',
#         TRIOPTIMA_PARTY_NAME == 'KRX825' ~ '산업은행',
#         TRIOPTIMA_PARTY_NAME == 'KRX888' ~ '다올투자증권',
#         TRIOPTIMA_PARTY_NAME == 'KRX924' ~ '한국씨티은행'
#         )
#     )


###22.1st

Compressed_PV01 <- Compressed_PV01 %>%
    mutate(TRIOPTIMA_PARTY_NAME = case_when(
        TRIOPTIMA_PARTY_NAME == 'KRX224' ~ '교보증권',
        TRIOPTIMA_PARTY_NAME == 'KRX589' ~ '한국투자증권',
        TRIOPTIMA_PARTY_NAME == 'KRX560' ~ '신영증권',
        TRIOPTIMA_PARTY_NAME == 'KRX093' ~ '메리츠증권',
        TRIOPTIMA_PARTY_NAME == 'KRX142' ~ 'NH투자증권',
        TRIOPTIMA_PARTY_NAME == 'KRX640' ~ 'KB증권',
        TRIOPTIMA_PARTY_NAME == 'KRX234' ~ '노무라금융투자',
        TRIOPTIMA_PARTY_NAME == 'KRX902' ~ '비엔피파리바은행',
        TRIOPTIMA_PARTY_NAME == 'KRX182' ~ '우리은행',
        TRIOPTIMA_PARTY_NAME == 'KRX869' ~ 'SC은행',
        TRIOPTIMA_PARTY_NAME == 'KRX628' ~ '제이피모간체이스은행',
        TRIOPTIMA_PARTY_NAME == 'KRX465' ~ 'SG 은행',
        TRIOPTIMA_PARTY_NAME == 'KRX574' ~ '미즈호은행'
        )
    )


# for (i in 1:nrow(Compressed_PV01)) {
# Compressed_PV01[i, 21] <-  sum(Compressed_PV01[i, c(2:19)])
# }


(apply(Compressed_PV01[, -1], 2, sum) > 10 | apply(Compressed_PV01[, -1], 2, sum) < -10)   #Validation_All FALSE


# PV01_Entire Position

PV01_SUM_FIX <- Pos_KRW_FN %>% 
    group_by(Pay_CP) %>% 
    summarise(PV_Call = sum(Up_Call),
              PV_CD = sum(Up_CD),
              PV_6M = sum(Up_6M),
              PV_9M = sum(Up_9M),
              PV_1Y = sum(Up_1Y),
              PV_18M = sum(Up_18M),
              PV_2Y = sum(Up_2Y),
              PV_3Y = sum(Up_3Y),
              PV_4Y = sum(Up_4Y),
              PV_5Y = sum(Up_5Y),
              PV_6Y = sum(Up_6Y),
              PV_7Y = sum(Up_7Y),
              PV_8Y = sum(Up_8Y),
              PV_9Y = sum(Up_9Y),
              PV_10Y = sum(Up_10Y),
              PV_12Y = sum(Up_12Y),
              PV_15Y = sum(Up_15Y),
              PV_20Y = sum(Up_20Y),
              PVBP = sum(Up_Prl))


PV01_SUM_FLO <- Pos_KRW_FN %>% 
    group_by(Rec_CP) %>% 
    summarise(PV_Call = sum(Up_Call),
              PV_CD = sum(Up_CD),
              PV_6M = sum(Up_6M),
              PV_9M = sum(Up_9M),
              PV_1Y = sum(Up_1Y),
              PV_18M = sum(Up_18M),
              PV_2Y = sum(Up_2Y),
              PV_3Y = sum(Up_3Y),
              PV_4Y = sum(Up_4Y),
              PV_5Y = sum(Up_5Y),
              PV_6Y = sum(Up_6Y),
              PV_7Y = sum(Up_7Y),
              PV_8Y = sum(Up_8Y),
              PV_9Y = sum(Up_9Y),
              PV_10Y = sum(Up_10Y),
              PV_12Y = sum(Up_12Y),
              PV_15Y = sum(Up_15Y),
              PV_20Y = sum(Up_20Y),
              PVBP = sum(Up_Prl))    


PV01_Member <- full_join(PV01_SUM_FIX, PV01_SUM_FLO, by = c('Pay_CP' = 'Rec_CP'))

PV01_Member[is.na(PV01_Member)] <- 0

PV01_Member <- PV01_Member %>% 
    mutate(PV_Call = PV_Call.x - PV_Call.y,
           PV_CD = PV_CD.x - PV_CD.y,
           PV_6M = PV_6M.x - PV_6M.y,
           PV_9M = PV_9M.x - PV_9M.y,
           PV_1Y = PV_1Y.x - PV_1Y.y,
           PV_18M = PV_18M.x - PV_18M.y,
           PV_2Y = PV_2Y.x - PV_2Y.y,
           PV_3Y = PV_3Y.x - PV_3Y.y,
           PV_4Y = PV_4Y.x - PV_4Y.y,
           PV_5Y = PV_5Y.x - PV_5Y.y,
           PV_6Y = PV_6Y.x - PV_6Y.y,
           PV_7Y = PV_7Y.x - PV_7Y.y,
           PV_8Y = PV_8Y.x - PV_8Y.y,
           PV_9Y = PV_9Y.x - PV_9Y.y,
           PV_10Y = PV_10Y.x - PV_10Y.y,
           PV_12Y = PV_12Y.x - PV_12Y.y,
           PV_15Y = PV_15Y.x - PV_15Y.y,
           PV_20Y = PV_20Y.x - PV_20Y.y,
           PVBP = PVBP.x - PVBP.y)



###Comparison - Original Delta - Compressed Delta  <- need validation from changed PR14 Delta

#-------------------#
#Change Participants#
#-------------------#

###21.2nd Test
# PV01_Member_COMP <- PV01_Member[c(10, 50, 29, 4, 3, 12, 6, 7, 39, 22, 20, 21, 17, 26, 49, 14), c(1, 40:58)]  

###22.1st Test
PV01_Member_COMP <- PV01_Member[c(4,5,7,8,11,13,20,23,26,30,36,40,50), c(1, 40:58)]





PV01_Member_COMP <- PV01_Member_COMP %>% 
    mutate(Var = 'Original_PV01') %>% 
    rename('TRIOPTIMA_PARTY_NAME' = 'Pay_CP')

Compressed_PV01[, -1] <- -1 * Compressed_PV01[, -1] 
Compressed_PV01 <- Compressed_PV01 %>%      ##Warning : sign(now: PV01 change, not compressed PV01)
    mutate(Var = 'Proposal_PV01')

grgr <- rbind(PV01_Member_COMP, Compressed_PV01)

PVBP_member <- melt(grgr[, c(1, 20, 21)])

PVBP_member_graph <- PVBP_member %>% 
    ggplot(aes(x = TRIOPTIMA_PARTY_NAME, y = value / 10 ^ 4, fill = Var)) +
    geom_col(position = "dodge", width = 0.7) +
    labs(x = "", y = "PV01", fill = "") +
    theme_minimal() +
    scale_fill_hue(direction = -1) +
    scale_y_continuous(labels = comma) +
    theme(legend.position = c(0.15, 0.85),
          legend.text = element_text(size = 15),
          axis.title = element_text(size = 15),
          axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 13),
          ) 
    #coord_cartesian(ylim=c(-100000000, 100000000))


PVBP_member_graph    # original PV01와 compressed PV01의 수준 차이 과도하여 사용하지 않음


melt(Compressed_PV01[, c(1, 20)])

Compressed_PV01_PVBP_member <- melt(Compressed_PV01[, c(1, 20)]) 

###21.2nd
# Compressed_PV01_PVBP_member[10, 1] <- 'KB'
# Compressed_PV01_PVBP_member[5, 1] <- 'SG'
# Compressed_PV01_PVBP_member[13, 1] <- '다올'
# Compressed_PV01_PVBP_member[9, 1] <- '노무라'
# Compressed_PV01_PVBP_member[6, 1] <- 'NH'
# Compressed_PV01_PVBP_member[1, 1] <- '모간'
# Compressed_PV01_PVBP_member[2, 1] <- '미래'
# Compressed_PV01_PVBP_member[8, 1] <- '도이치'
# Compressed_PV01_PVBP_member[7, 1] <- 'JPM'
# Compressed_PV01_PVBP_member[3, 1] <- '교보'
# Compressed_PV01_PVBP_member[4, 1] <- '신영'
# Compressed_PV01_PVBP_member[12, 1] <- '산업'
# Compressed_PV01_PVBP_member[11, 1] <- 'SC'
# Compressed_PV01_PVBP_member[14, 1] <- '한투'
# Compressed_PV01_PVBP_member[15, 1] <- '씨티'
# Compressed_PV01_PVBP_member[16, 1] <- '미즈호'


##22.1st
Compressed_PV01_PVBP_member[10, 1] <- 'JPM'
Compressed_PV01_PVBP_member[5, 1] <- '노무라'
Compressed_PV01_PVBP_member[13, 1] <- 'BNP'
Compressed_PV01_PVBP_member[9, 1] <- '한투'
Compressed_PV01_PVBP_member[6, 1] <- 'SG'
Compressed_PV01_PVBP_member[1, 1] <- '메리츠'
Compressed_PV01_PVBP_member[2, 1] <- 'NH'
Compressed_PV01_PVBP_member[8, 1] <- '미즈호'
Compressed_PV01_PVBP_member[7, 1] <- '신영'
Compressed_PV01_PVBP_member[3, 1] <- '우리'
Compressed_PV01_PVBP_member[4, 1] <- '교보'
Compressed_PV01_PVBP_member[12, 1] <- 'SC'
Compressed_PV01_PVBP_member[11, 1] <- 'KB'

Compressed_PV01_graph <- Compressed_PV01_PVBP_member %>% 
    ggplot(aes(x = TRIOPTIMA_PARTY_NAME, weight = value / 10 ^ 6)) +
    geom_bar(fill = "#EF562D") +
    labs(x = "", y = "Compressed PV01") +
    scale_y_continuous(labels = label_number(accuracy = 1)) +
    theme_minimal() +
    theme(
        axis.title = element_text(size = 30),
        axis.text = element_text(size = 23)
        )


Compressed_PV01_graph



###bucket PV01 difference per member

max(grgr[c(17:32),c(2:19)])

PV01_Member_name <- '모간스탠리은행'

if (grgr[grgr[, 1] == PV01_Member_name, ][1, 21] == 'Original_PV01' &
    grgr[grgr[, 1] == PV01_Member_name, ][2, 21] == 'Proposal_PV01') {
    PVBP_Bucket <- (rbind(grgr[grgr[, 1] == PV01_Member_name, ][1, -c(1, 21)],
                          grgr[grgr[, 1] == PV01_Member_name, ][1, -c(1, 21)] + grgr[grgr[, 1] == PV01_Member_name, ][2, -c(1, 21)]))
}

# PVBP_Bucket <- PVBP_Bucket %>% 
#     mutate(id = c('PV01(전)', 'PV01(후)')) %>% 
#     rename('TOTAL' = 'PVBP') %>% 
#     melt()

#22.1차 우리은행 그래프때문에 아래 PVBP(parallel) 빼버림

PVBP_Bucket <- PVBP_Bucket[, -19] %>% 
    mutate(id = c('PV01(전)', 'PV01(후)')) %>%
    melt()


PVBP_Bucket_graph <- PVBP_Bucket %>% 
    ggplot(aes(x = variable, y= value / 10 ^ 6, fill = id)) +
    geom_col(position = "dodge", width = 0.7) +
    labs(x = "Tenor Buckets", y = "PV01", fill = "") +
    theme_minimal() +
    scale_fill_hue(direction = -1) +
    scale_y_continuous(labels = comma,
                       breaks = seq(-500,1250, by=100)) +
    theme(legend.position = c(0.65, 0.85),
          legend.text = element_text(size = 17),
          axis.title = element_text(size = 17),
          axis.text = element_text(size = 14))
    

write.csv(PVBP_Bucket, '21.2ndPVbucket_MS.csv')
    
PVBP_Bucket_graph




###### 6. Settlement Amount Analysis ###### ------------------------
#증거금과 협의결제금액은 하나의 프레임에 들어가있음

###22.1st

PR14_yest <- read_xlsx('PR14_KRW_220427.xlsx')

PR14_yest <- PR14_yest[, c(4, 28)]

## Implement Partial PV01 and Correct PV01 Direction

Proposal_Settle <-
    left_join(Proposal, PR14_yest, by = c("CCP_TRADE_ID" = "CLnum")) %>%
    mutate(Unwind_rate = Unwind_NA / ORIGINAL_NOTIONAL,
           NPV = NPV * Unwind_rate * ifelse(PAY_LEG_TYPE == "Fixed", 1, -1),
           Set_Amount = CLOSE_OUT_AMOUNT - NPV)  

SA_Amount_5pm <- Proposal_Settle %>% 
    group_by(TRIOPTIMA_PARTY_NAME) %>% 
    summarise(Set_Amount_Total = sum(ifelse(Set_Amount < 0 , Set_Amount, 0)))

SA_Amount_12am <- Proposal_Settle %>% 
    group_by(TRIOPTIMA_PARTY_NAME) %>% 
    summarise(Set_Amount_Total = sum(Set_Amount))

sum(ifelse(SA_Amount_12am$Set_Amount_Total < 0, SA_Amount_12am$Set_Amount_Total, 0))

abs(sum(SA_Amount_12am$Set_Amount_Total)) < 1   #Validate == TRUE 


write.csv(SA_Amount_12am, 'SA_Amount_12am_22_1st.csv')
write.csv(SA_Amount_5pm, 'SA_Amount_5pm_22_1st.csv')


###### #. ETC ------------------------

# 3D Graph

Proposal_3D <- Proposal %>%
    filter(PAY_LEG_TYPE == 'Fixed') %>% 
    mutate(MTM = CLOSE_OUT_AMOUNT / 10^6,
           NOTIONAL = Unwind_NA / 10^8,
           Mat_Year = as.numeric(unlist(Maturity)) / 365) %>% 
    select(MTM, NOTIONAL, Mat_Year, TRIOPTIMA_PARTY_NAME)


Proposal_3DGraph <- plot_ly(Proposal_3D, x = ~ NOTIONAL,
                            y = ~ Mat_Year, 
                            z = ~ MTM,
                            color = ~ TRIOPTIMA_PARTY_NAME,
                            colors = brewer.pal(n = 13, name = 'Paired'),
                            type = 'scatter3d',
                            mode = 'markers',
                            marker.size = 0.5)


Proposal_3DGraph
















#5 - 3-2.????????????????????????

# Proposal_PV01[, c(67:85)] <- ifelse(Proposal_PV01$PAY_LEG_TYPE == 'Fixed',
#                                     Proposal_PV01[, c(67:85)],
#                                     - Proposal_PV01[, c(67:85)])
# 
# 
# Compressed_Mat_PV01_KRX182 <- Proposal_PV01 %>% 
#     filter(TRIOPTIMA_PARTY_NAME == 'KRX909') 
# 
#     apply(Compressed_Mat_PV01_KRX182[, c(67:85)], 2, sum)
# 
# 
#     abcdefg <- Compressed_Mat_PV01_KRX182[order(Compressed_Mat_PV01_KRX182$TRADE_ID), ]
# 
# sum(Compressed_Mat_PV01_KRX182$FLOATING)   #Validation
# 
# Compressed_Mat_PV01_KRX182 <- Compressed_Mat_PV01_KRX182 %>% 
#     mutate(Margin_mat = case_when(Margin_mat == 1 ~ '0',
#                                   Margin_mat == 2 ~ '3',
#                                   Margin_mat == 3 ~ '6',
#                                   Margin_mat == 4 ~ '9',
#                                   Margin_mat == 5 ~ '12',
#                                   Margin_mat == 6 ~ '18',
#                                   Margin_mat == 7 ~ '24',
#                                   Margin_mat == 8 ~ '36', 
#                                   Margin_mat == 9 ~ '48',
#                                   Margin_mat == 10 ~ '60', 
#                                   Margin_mat == 11 ~ '72',
#                                   Margin_mat == 12 ~ '84',
#                                   Margin_mat == 13 ~ '96',
#                                   Margin_mat == 14 ~ '108',
#                                   Margin_mat == 15 ~ '120',
#                                   Margin_mat == 16 ~ '144',
#                                   Margin_mat == 17 ~ '180',
#                                   TRUE ~ '240'))
# 
# Margin_mat <- data.frame(Margin_mat = c('0', '3', '6', '9', '12', '18', '24', '36', '48', '60', '72', '84', '96', '108', '120', '144', '180', '240'))
# 
# Compressed_Mat_PV01_KRX182[is.na(Compressed_Mat_PV01_KRX182)] <- 0
#     
# Compressed_Mat_PV01_KRX182$Margin_mat <- factor(Compressed_Mat_PV01_KRX182$Margin_mat, levels = unique(Compressed_Mat_PV01_KRX182$Margin_mat))   #Remain Current Order as Factor -> x axis order
# Compressed_Mat_PV01_KRX182_Graph <- Compressed_Mat_PV01_KRX182 %>% 
#     melt(id = 'Margin_mat') %>% 
#     ggplot(aes(x = Margin_mat, y = value, fill = variable)) +
#     geom_col(position = "dodge", width = 0.7) +
#     scale_fill_hue(direction = 1) +
#     labs(x = "Maturity", y = "Compressed PV01", fill = "") +
#     theme_minimal() +
#     theme(legend.position = c(0.85, 0.85),
#           legend.text = element_text(size = 20),
#           axis.title = element_text(size = 25),
#           axis.text.x = element_text(size = 17),
#           axis.text.y = element_text(size = 17))
# 
# 
# Compressed_Mat_PV01_KRX182_Graph
# 
# Compressed_Mat_PV01_KRX182_SUM <- Compressed_Mat_PV01_KRX182 %>% 
#     mutate(SUM = FIXED + FLOATING)
# 
# Compressed_Mat_PV01_KRX182_SUM_Graph <- Compressed_Mat_PV01_KRX182_SUM %>% 
#     ggplot(aes(x = Margin_mat, y = SUM)) +
#     geom_col(position = "dodge", width = 0.7) +
#     scale_fill_hue(direction = 1) +
#     labs(x = "Maturity", y = "Compressed PV01") +
#     theme_minimal() +
#     theme(legend.position = c(0.85, 0.85),
#           legend.text = element_text(size = 20),
#           axis.title = element_text(size = 25),
#           axis.text.x = element_text(size = 17),
#           axis.text.y = element_text(size = 17))
# 
# 
# Compressed_Mat_PV01_KRX182_SUM_Graph
# 
# #Total Delta Difference
# sum(Compressed_Mat_PV01_KRX182$FIXED) + sum(Compressed_Mat_PV01_KRX182$FLOATING)








