# <LIBRARY>--------------------------------------------------
library(tidyverse)
library(hablar)
library(lubridate)
library(foreach)
library(doParallel)
library(parallel)
library(data.table)
library(readxl)
library(reshape2)
library(DBI)       # DB관련 함수 사용을 위한 라이브러리
library(RSQLite)   # SQLite 사용을 위한 라이브러리
library(bbplot)
library(patchwork)


# install.packages("C:/Users/Win10/Documents/Rttf2pt1_1.3.8.tar.gz", repos = NULL, type = "source")
# 
# remotes::install_version("Rttf2pt1", version = "1.3.8")
# extrafont::font_import(pattern = "Helvetica")
extrafont::loadfonts(device='win')
windowsFonts()





# <FUNCTION>--------------------------------------------------

## 반올림함수
round_off <- function(x, digits = 0) {
    posneg = sign(x)
    z = trunc(abs(x) * 10 ^ (digits + 1)) / 10
    z = floor(z * posneg + 0.5) / 10 ^ digits
    return(z)
}

## 선형보간함수
interpol <- function(TDay, D1, D2, R1, R2) {
    z = R1 + (R2 - R1) * as.numeric(TDay - D1) / as.numeric(D2 - D1)
    return(z)
}

## ZCRtoDF
ZCRtoDF <- function(zcr, Dt, De) {
    z = exp(-zcr * as.numeric(Dt - De) / 365)
    return(z)
}

## DFtoZCR
DFtoZCR <- function(DF, Dt, De) {
    z = log(1 / DF) * 365 / as.numeric(Dt - De)
    return(z)
}

## 계약별 CashFlow 생성
CF_3nM <- function(CAL,
                   Dur,
                   BDC,
                   EOM,
                   EffDate,
                   CLnum,
                   Pay_Acc,
                   Rec_Acc,
                   Notional,
                   Fixed_Rate,
                   Fixed_DCF,
                   Float_DCF) {
    ifelse(
        EOM == "N",
        CF_3nM_S <- EffDate %m+% months(seq(0, Dur - 3, 3)),
        CF_3nM_S <- ceiling_date(EffDate %m+%
                                     months(seq(0, Dur - 3, 3)),
                                 "month") - 1
    )
    
    ifelse(
        EOM == "N",
        CF_3nM_E <- EffDate %m+% months(seq(3, Dur, 3)),
        CF_3nM_E <- ceiling_date(EffDate %m+%
                                     months(seq(3, Dur, 3)),
                                 "month") - 1
    )
    
    
    return(
        cbind(
            CF_3nM_S,
            CF_3nM_E,
            BDC,
            CLnum,
            Pay_Acc,
            Rec_Acc,
            Notional,
            Fixed_Rate,
            Fixed_DCF,
            Float_DCF
        )
    )
} 




# <New Function>-----------------------
Lambda1250 <- function(Lambda) {
    return(matrix(c(
        Lambda ^ 1250, Lambda ^ seq(1249, 0) * (1 - Lambda)
    ), nrow = 1))
}

# <SETTINGS>--------------------------------------------------
## [기본세팅]
options(digits = 15)
options(scipen = 999)


# 금리차분(5일) 산출 : 07.01.02일 이후----------------------------------

Rate_raw_KRW <- read_csv("Rate.csv")  

Rate_lag <- lag(Rate_raw_KRW[, -1], 5)

Rate_lag[is.na(Rate_lag)] <- 0

Rate_diff <-
    rename(cbind(Rate_raw_KRW$DATE, round_off((
        Rate_raw_KRW[, -1] - Rate_lag
    ) / 100, digits = 14)), DATE = 1)

Rate_diff[1:5, -1] <- NA


# EWMA 변동성 추정-----------------------------------------------------------

# [람다값] EWMA decay factor
Lambda_param <- c(0.94, 0.97, 0.99)

# [최소변동성 관측기간]
volfl_param <- c(1250, 2500, 3250)


## 산출일 Index : VaR 증거금 도입 이후 최근까지
Day_s <- which(Rate_raw_KRW$DATE == as.Date("2016-08-29"))
Day_e <- nrow(Rate_raw_KRW)


core <- detectCores()
cluster <- makeCluster(spec = core) 
registerDoParallel(cl = cluster)

Result_EWMA <-  foreach(Rate_index = Day_s:Day_e, .packages = c('tidyverse', 'lubridate')) %dopar% {

    vol_day <- c()
    vol_min <- c()
    
    # 과거 5년 diff Table
    EWMA <- data.frame(Rate_diff[c((Rate_index - 1250):Rate_index), ])
    
    EWMA[, -1] <- EWMA[, -1] ^ 2
    
    ## 만기별 초기변동성 제곱(not 제곱근) - 차분 제곱의 평균(관측치 없는 경우 최대관측기간)
    for (i in 2:19) {
        EWMA[1, i] <- ifelse(Rate_index >= 2505,
                             sum(Rate_diff[c((Rate_index - 2499):(Rate_index - 1250)),
                                            i] ^ 2) / 1250,
                             sum(Rate_diff[c(6:(Rate_index - 1250)),
                                            i] ^ 2) / (Rate_index - 1255))
    }
    
    ## EWMA 계산
    for (j in 1:3) {                          #람다 파라미터 4개별 당일 변동성
        Lambda <- Lambda_param[j]
        Lambda_mat <- Lambda1250(Lambda)
        vol_day <-
            rbind(vol_day, c(
                Rate_index,                                   #날짜
                paste0('λ=', Lambda),                          #파라미터
                (Lambda_mat %*% as.matrix(EWMA[, -1])) ^ (1/2)          #만기별 당일 변동성   
            ))
    } 
    
    
    ## 최소변동성 - 관측치 없는 경우 최대관측기간
    Rate_diff_square <- Rate_diff[, -1] ^ 2
    
    for (k in 1:3) {
        vol_window <- volfl_param[k]
        
        ifelse(Rate_index >= (vol_window + 5),
               vol_min_temp <- apply(Rate_diff_square[c((Rate_index - vol_window + 1):(Rate_index)), ], 2, mean) ^ (1/2),
               vol_min_temp <- apply(Rate_diff_square[c(6:Rate_index), ], 2, mean) ^ (1/2)
        )
        
        vol_min <- 
            rbind(vol_min, c(
                Rate_index,
                paste0(vol_window,'일 vol'),
                vol_min_temp
            ))
    }
    
    rbind(vol_day, vol_min)
    
}  

stopCluster(cluster) 


# EWMA Dataframe-----------------------------------------------------
Result_EWMA_df <- data.frame(do.call(rbind, Result_EWMA))

Result_EWMA_df[, -2] <- as.numeric(unlist(Result_EWMA_df[, -2]))


Result_EWMA_df$V1 <- Rate_raw_KRW$DATE[as.numeric(Result_EWMA_df$V1)]

Result_EWMA_df <- Result_EWMA_df %>%
    rename('DATE' = V1,
           'ID' = V2) 


## NA 검증
which(is.na(Result_EWMA_df) == T)
    



# EWMA graph-----------------------------------------------------


## GRAPH 1


EWMA_graph_lambda_1Y <-
    ggplot(
        Result_EWMA_df %>%
            filter(ID == '2500일 vol' |
                       ID == "λ=0.94" |
                       ID == "λ=0.97" |
                       ID == "λ=0.99") %>%
            mutate(
                ID = case_when(
                    ID == '2500일 vol' ~ '최소변동성',
                    ID == 'λ=0.94' ~ '당일변동성(λ=0.94)',
                    ID == 'λ=0.99' ~ '당일변동성(λ=0.99)',
                    TRUE ~ '당일변동성'
                )
            ),
        aes(x = DATE, y = IRS_1Y * 10000, color = ID)
    ) +
    geom_line(aes(
        color = ID,
        linetype = ID,
        size = factor(
            ifelse(ID == "당일변동성" | ID == "최소변동성", 'thick', 'thin'),
            levels = c('thick', 'thin')
        ),
        alpha = factor(
            ifelse(ID == "당일변동성" | ID == "최소변동성", 'light', 'dark'),
            levels = c('light', 'dark')
        )
    )) +
    geom_hline(yintercept = 0,
               size = 1,
               colour = "#333333") +
    scale_linetype_manual(values = c('solid', 'solid', 'solid', 'solid')) +
    scale_size_manual(values = c(1.3, 0.7), guide = 'none') +
    scale_alpha_manual(values = c(1, 0.6), guide = 'none') +
    scale_color_manual(values = c('#1380A1', '#D01C8B', '#0000FF', '#FAAB18')) +
    bbc_style() +
    theme(
        legend.key.width = unit(1, "cm"),
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(0, 0, -40, 0)
    ) +
    guides(linetype = guide_legend(nrow = 2, byrow = FALSE)) +
    labs(title = "일별 변동성 추이_1Y",
         subtitle = "최소변동성 vs 당일변동성(람다별)") +
    scale_x_date(date_breaks = "6 month" , date_labels = "'%y.%b")


finalise_plot(plot_name = EWMA_graph_lambda_1Y,
              source_name = '',
              save_filepath = 'EWMA_lambda_1Y.png',
              width_pixels = 800,
              height_pixels = 450)



## GRAPH 2


EWMA_graph_volmin_1Y <-
    ggplot(
        Result_EWMA_df %>%
            filter(ID == '2500일 vol' |
                       ID == "1250일 vol" |
                       ID == "3250일 vol" |
                       ID == "λ=0.97") %>%
            mutate(
                ID = case_when(
                    ID == '2500일 vol' ~ '최소변동성',
                    ID == '1250일 vol' ~ '최소변동성(1250일)',
                    ID == '3250일 vol' ~ '최소변동성(3250일)',
                    TRUE ~ '당일변동성'
                )
            ),
        aes(x = DATE, y = IRS_1Y * 10000, color = ID)
    ) +
    geom_line(aes(
        color = ID,
        linetype = ID,
        size = factor(
            ifelse(ID == "당일변동성" | ID == "최소변동성", 'thick', 'thin'),
            levels = c('thick', 'thin')
        ),
        alpha = factor(
            ifelse(ID == "당일변동성" | ID == "최소변동성", 'light', 'dark'),
            levels = c('light', 'dark')
        )
    )) +
    geom_hline(yintercept = 0,
               size = 1,
               colour = "#333333") +
    scale_linetype_manual(values = c('solid', 'solid', 'dashed', 'dashed')) +
    scale_size_manual(values = c(1.2, 0.7), guide = 'none') +
    scale_alpha_manual(values = c(1, 1), guide = 'none') +
    scale_color_manual(values = c('#1380A1', '#FAAB18', '#D01C8B', '#0000FF')) +
    bbc_style() +
    theme(
        legend.key.width = unit(1, "cm"),
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(0, 0, -40, 0)
    ) +
    guides(linetype = guide_legend(nrow = 2, byrow = TRUE)) +
    labs(title = "일별 변동성 추이_1Y",
         subtitle = "당일변동성 vs 최소변동성(관측기간별)") +
    scale_x_date(date_breaks = "6 month" , date_labels = "'%y.%b")


finalise_plot(plot_name = EWMA_graph_volmin_1Y,
              source_name = '',
              save_filepath = 'EWMA_vomin_1Y.png',
              width_pixels = 800,
              height_pixels = 450)



### 최소변동성 추기(만기별)

Result_EWMA_df_2500vol <- Result_EWMA_df %>% filter(ID == '2500일 vol') %>% 
    select(DATE, Call, CD, IRS_6M, IRS_1Y, IRS_3Y, IRS_5Y, IRS_10Y, IRS_20Y)




Minvol_tenor <-
    ggplot(melt(Result_EWMA_df_2500vol, id.vars = 'DATE')) +
    geom_line(aes(x = DATE, y = value * 10000, color = variable), size = 1.3) +
    bbc_style() +
    geom_hline(yintercept = 0,
               size = 1,
               colour = "#333333") +
    scale_y_continuous(
        name = 'Minvol',
        labels = scales::comma
    ) +
    labs(title = "만기별 최소변동성 추이",
         subtitle = "'16.8.29~'22.11.30") +
    theme(legend.key.width = unit(2,"cm")) +
    scale_x_date(date_breaks = "6 month" , date_labels = "%y.%b")


finalise_plot(plot_name = Minvol_tenor,
              source_name = '',
              save_filepath = 'Minvol_tenor.png',
              width_pixels = 900,
              height_pixels = 450)


## 최소변동성 적용갯수 세기(만기별 합)

Minvol_number1 <- Result_EWMA_df %>% filter(ID == '2500일 vol') 
Minvol_number2 <- Result_EWMA_df %>% filter(ID == "λ=0.97") 


Minvol_number <-
    cbind(DATE = Minvol_number1[, 1], Minvol_number1[, -c(1, 2)] - Minvol_number2[, -c(1, 2)]) %>% 
    mutate_at(2:19, funs(ifelse(. > 0, ., 0))) %>% 
    mutate(Sum = rowSums(across(c(Call:IRS_20Y))))



for (i in 1:nrow(Minvol_number)) {
    Minvol_number[i, 21] <- length(which(Minvol_number[i, -c(1, 20)] > 0))
}

Minvol_num <- Minvol_number %>% select(DATE, Sum, V21) %>% rename('Num' = V21)

Minvol_num[c(930:1278), ]


Min_vol_num_graph <- ggplot(Minvol_num[c(930:1278), ], aes(x = DATE, group = 1)) +
    geom_line(aes(y = Num * 50/18), color = '#ff3399', size = 1.3) +
    geom_line(aes(y = Sum * 10000), color = '#3333ff', size = 1.3) +
    bbc_style() +
    geom_hline(yintercept = 0,
               size = 1,
               colour = "#333333") +
    scale_y_continuous(
        name = 'IM_diff',
        labels = scales::comma,
        sec.axis = sec_axis(~ . / 50*18, name = '감소계좌', breaks = c(6, 12, 18))
    ) +
    labs(title = "만기별 {최소변동성-당일변동성} 합계(좌) 및 최소변동성 적용 만기수(우)",
         subtitle = "'20.6.9~'21.11.3") +
    scale_x_date(date_breaks = "3 month" , date_labels = "%y.%b.%d")


finalise_plot(plot_name = Min_vol_num_graph,
              source_name = '',
              save_filepath = 'Min_vol_num_graph.png',
              width_pixels = 900,
              height_pixels = 450)









 
# 
# EWMA_graph_1Y <-
#     ggplot(Result_EWMA_df, aes(x = DATE, y = IRS_1Y * 10000)) +
#     geom_line(aes(
#         color = ID,
#         linetype = ID,
#         size = factor(
#             ifelse(ID == "λ=0.97" | ID == "2500일 vol", 'thick', 'thin'),
#             levels = c('thick', 'thin')
#         ),
#         alpha = factor(
#             ifelse(ID == "λ=0.97" | ID == "2500일 vol", 'light', 'dark'),
#             levels = c('light', 'dark')
#         )
#     )) +
#     geom_hline(yintercept = 0, size = 1, colour = "#333333") +
#     scale_linetype_manual(values = c('dashed', 'dashed', 'dashed', 'solid', 'solid', 'solid')) +
#     scale_size_manual(values = c(1.2, 0.7), guide = 'none') +
#     scale_alpha_manual(values = c(1, 0.4), guide = 'none') +
#     scale_color_manual(values = c('#35B779FF', '#FAAB18', '#D01C8B', '#663399', '#1380A1', '#404040')) +
#     bbc_style() +
#     theme(legend.key.width = unit(2,"cm")) +
#     guides(linetype = guide_legend(nrow = 2, byrow = TRUE)) +
#     labs(title = "일별 변동성 추이",
#          subtitle = "최소변동성 vs 당일변동성(단위: 억원)")
# 
# 
# finalise_plot(plot_name = EWMA_graph_1Y,
#               source_name = '',
#               save_filepath = 'EWMA_1Y.png',
#               width_pixels = 800,
#               height_pixels = 450)
# 
# 
# 
# EWMA_graph_3Y <-
#     ggplot(Result_EWMA_df, aes(x = DATE, y = IRS_3Y * 10000)) +
#     geom_line(aes(
#         color = ID,
#         linetype = ID,
#         size = factor(
#             ifelse(ID == "λ=0.97" | ID == "2500일 vol", 'thick', 'thin'),
#             levels = c('thick', 'thin')
#         ),
#         alpha = factor(
#             ifelse(ID == "λ=0.97" | ID == "2500일 vol", 'light', 'dark'),
#             levels = c('light', 'dark')
#         )
#     )) +
#     geom_hline(yintercept = 0, size = 1, colour = "#333333") +
#     scale_linetype_manual(values = c('dashed', 'dashed', 'dashed', 'solid', 'solid', 'solid')) +
#     scale_size_manual(values = c(1.2, 0.7), guide = 'none') +
#     scale_alpha_manual(values = c(1, 0.4), guide = 'none') +
#     scale_color_manual(values = c('#35B779FF', '#FAAB18', '#D01C8B', '#663399', '#1380A1', '#404040')) +
#     bbc_style() +
#     theme(legend.key.width = unit(2,"cm")) +
#     guides(linetype = guide_legend(nrow = 2, byrow = TRUE)) +
#     labs(title = "일별 변동성 추이",
#          subtitle = "최소변동성 vs 당일변동성(단위: 억원)")
# 
# 
# 
# EWMA_graph_5Y <-
#     ggplot(Result_EWMA_df, aes(x = DATE, y = IRS_5Y * 10000)) +
#     geom_line(aes(
#         color = ID,
#         linetype = ID,
#         size = factor(
#             ifelse(ID == "λ=0.97" | ID == "2500일 vol", 'thick', 'thin'),
#             levels = c('thick', 'thin')
#         ),
#         alpha = factor(
#             ifelse(ID == "λ=0.97" | ID == "2500일 vol", 'light', 'dark'),
#             levels = c('light', 'dark')
#         )
#     )) +
#     geom_hline(yintercept = 0, size = 1, colour = "#333333") +
#     scale_linetype_manual(values = c('dashed', 'dashed', 'dashed', 'solid', 'solid', 'solid')) +
#     scale_size_manual(values = c(1.2, 0.7), guide = 'none') +
#     scale_alpha_manual(values = c(1, 0.4), guide = 'none') +
#     scale_color_manual(values = c('#35B779FF', '#FAAB18', '#D01C8B', '#663399', '#1380A1', '#404040')) +
#     bbc_style() +
#     theme(legend.key.width = unit(2,"cm")) +
#     guides(linetype = guide_legend(nrow = 2, byrow = TRUE)) +
#     labs(title = "일별 변동성 추이",
#          subtitle = "최소변동성 vs 당일변동성(단위: 억원)")
# 
# 
# 
# 
# EWMA_graph_10Y <-
#     ggplot(Result_EWMA_df, aes(x = DATE, y = IRS_10Y * 10000)) +
#     geom_line(aes(
#         color = ID,
#         linetype = ID,
#         size = factor(
#             ifelse(ID == "λ=0.97" | ID == "2500일 vol", 'thick', 'thin'),
#             levels = c('thick', 'thin')
#         ),
#         alpha = factor(
#             ifelse(ID == "λ=0.97" | ID == "2500일 vol", 'light', 'dark'),
#             levels = c('light', 'dark')
#         )
#     )) +
#     geom_hline(yintercept = 0, size = 1, colour = "#333333") +
#     scale_linetype_manual(values = c('dashed', 'dashed', 'dashed', 'solid', 'solid', 'solid')) +
#     scale_size_manual(values = c(1.2, 0.7), guide = 'none') +
#     scale_alpha_manual(values = c(1, 0.4), guide = 'none') +
#     scale_color_manual(values = c('#35B779FF', '#FAAB18', '#D01C8B', '#663399', '#1380A1', '#404040')) +
#     bbc_style() +
#     theme(legend.key.width = unit(2,"cm")) +
#     guides(linetype = guide_legend(nrow = 2, byrow = TRUE)) +
#     labs(title = "일별 변동성 추이",
#          subtitle = "최소변동성 vs 당일변동성(단위: 억원)")

