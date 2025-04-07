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






# <SETTINGS>--------------------------------------------------
## [기본세팅]
options(digits = 15)
options(scipen = 999)


DailyIM <- data.frame(read_csv('DailyIM.csv'))

Rate_lag1 <- lag(DailyIM[, -1], 1)
Rate_lag10 <- lag(DailyIM[, -1], 10)
Rate_lag20 <- lag(DailyIM[, -1], 20)



DailyIM <- 
    cbind(DailyIM,
          Rate_lag1,
          Rate_lag10,
          Rate_lag20) %>% 
    mutate(Rate_lag1 = ifelse(IM - Rate_lag1 > 0, IM - Rate_lag1, 0),
           Rate_lag10 = ifelse(IM - Rate_lag10 > 0, IM - Rate_lag10, 0),
           Rate_lag20 = ifelse(IM - Rate_lag20 > 0, IM - Rate_lag20, 0)) %>% 
    rename('총증거금' = IM,
           '증거금차분_1일' = Rate_lag1,
           '증거금차분_10일' = Rate_lag10,
           '증거금차분_20일' = Rate_lag20)


DailyIM[is.na(DailyIM)] <- 0

# DailyIM_graph <-  ## 2 y-axis 적용은 되나, 
#     ggplot(DailyIM, aes(x = DATE)) +
#     geom_line(aes(y = IM / 10 ^ 8), color = '#1380A1') +
#     geom_line(aes(y = Rate_lag1 / 10 ^ 8 * 2), color = '#FAAB18') +
#     geom_line(aes(y = Rate_lag10 / 10 ^ 8 * 2), color = '#663399') +
#     geom_line(aes(y = Rate_lag20 / 10 ^ 8 * 2), color = '#ff0000') +
#     geom_hline(yintercept = 0,
#                size = 1,
#                colour = "#333333") +
#     bbc_style() +
#     scale_y_continuous(
#         name = 'Total IM',
#         labels = scales::comma,
#         sec.axis = sec_axis( ~ . / 2, name = 'n-day call', labels = scales::comma)
#     ) 

# graph -------------------------------------------------------

DailyIM_graph <-
    ggplot(DailyIM %>% melt(id.vars = 'DATE'),
           aes(x = DATE, y = value / 10 ^ 8)) +
    geom_line(aes(
        color = variable,
        alpha = factor(
            ifelse(variable == "총증거금", 'light', 'dark'),
            levels = c('light', 'dark')
        )
    ),
    size = 1.3) +
    geom_hline(yintercept = 0,
               size = 1,
               colour = "#333333") +
    scale_alpha_manual(values = c(1, 0.6), guide = 'none') +
    scale_color_manual(values = c('#1380A1', '#00ff33', '#ff3399', '#3333ff')) +
    bbc_style() +
    scale_y_continuous(name = 'Total IM',
                       labels = scales::comma)  +
    theme(legend.key.width = unit(1.5,"cm"),
          legend.text=element_text(size = 18)) +
    labs(title = "증거금필요액 추이",
         subtitle = "총증거금 및 증거금차분(단위: 억원)") +
    scale_x_date(date_breaks = "6 month" , date_labels = "'%y.%b")



finalise_plot(plot_name = DailyIM_graph,
              source_name = '',
              save_filepath = 'DailyIM.png',
              width_pixels = 800,
              height_pixels = 450)

#plotly::ggplotly(DailyIM_graph)




# fixed_position Series

load('IM_table_period1.Rdata')
load('IM_table_period2.Rdata')
load('IM_table_period3.Rdata')
load('IM_table_period4.Rdata')
load('IM_table_period4_sub.Rdata')

DailyIM_period1 <-
    ggplot(
        IM_table_period1 %>%
            rename('실제포지션' = IM,
                   '고정포지션' = IM_fixed) %>%
            melt(id.vars = 'DATE'),
        aes(x = DATE, y = value / 10 ^ 8)
    ) +
    geom_line(aes(color = variable), size = 1.3) +
    geom_hline(yintercept = 0,
               size = 1,
               colour = "#333333") +
    scale_color_manual(values = c('#1380A1', '#FAAB18')) +
    bbc_style() +
    scale_y_continuous(labels = scales::comma)  +
    theme(legend.key.width = unit(1.5, "cm"),
          legend.text = element_text(size = 18)) +
    labs(title = "증거금필요액 추이 - '20.1.23~4.8",
         subtitle = "실제포지션 vs 고정포지션(단위: 억원)") +
    scale_x_date(date_breaks = "2 weeks" , date_labels = "%y.%b.%d")



finalise_plot(plot_name = DailyIM_period1,
              source_name = '',
              save_filepath = 'DailyIM_period1.png',
              width_pixels = 800,
              height_pixels = 450)



DailyIM_period2 <-
    ggplot(
        IM_table_period2 %>%
            rename('실제포지션' = IM,
                   '고정포지션' = IM_fixed) %>%
            melt(id.vars = 'DATE'),
        aes(x = DATE, y = value / 10 ^ 8)
    ) +
    geom_line(aes(color = variable), size = 1.3) +
    geom_hline(yintercept = 0,
               size = 1,
               colour = "#333333") +
    scale_color_manual(values = c('#1380A1', '#FAAB18')) +
    bbc_style() +
    scale_y_continuous(labels = scales::comma)  +
    theme(legend.key.width = unit(1.5, "cm"),
          legend.text = element_text(size = 18)) +
    labs(title = "증거금필요액 추이 - '21.9.24~12.6",
         subtitle = "실제포지션 vs 고정포지션(단위: 억원)") +
    scale_x_date(date_breaks = "2 weeks" , date_labels = "%y.%b.%d")



finalise_plot(plot_name = DailyIM_period2,
              source_name = '',
              save_filepath = 'DailyIM_period2.png',
              width_pixels = 800,
              height_pixels = 450)



DailyIM_period3 <-
    ggplot(
        IM_table_period3 %>%
            rename('실제포지션' = IM,
                   '고정포지션' = IM_fixed) %>%
            melt(id.vars = 'DATE'),
        aes(x = DATE, y = value / 10 ^ 8)
    ) +
    geom_line(aes(color = variable), size = 1.3) +
    geom_hline(yintercept = 0,
               size = 1,
               colour = "#333333") +
    scale_color_manual(values = c('#1380A1', '#FAAB18')) +
    bbc_style() +
    scale_y_continuous(labels = scales::comma)  +
    theme(legend.key.width = unit(1.5, "cm"),
          legend.text = element_text(size = 18)) +
    labs(title = "증거금필요액 추이 - '22.3.21~5.17",
         subtitle = "실제포지션 vs 고정포지션(단위: 억원)") +
    scale_x_date(date_breaks = "2 weeks" , date_labels = "%y.%b.%d")



finalise_plot(plot_name = DailyIM_period3,
              source_name = '',
              save_filepath = 'DailyIM_period3.png',
              width_pixels = 800,
              height_pixels = 450)




DailyIM_period4 <-
    ggplot(
        left_join(IM_table_period4,
                  IM_table_period4_sub[,c(1, 3)], 
                  by  = 'DATE') %>%
            mutate(IM_fixed.y = ifelse(is.na(IM_fixed.y) == TRUE, IM_fixed.x, IM_fixed.y)) %>% 
            rename('실제포지션' = IM,
                   '고정포지션(99.7%)' = IM_fixed.x,
                   '고정포지션(99.9%)' = IM_fixed.y) %>%
            melt(id.vars = 'DATE'),
        aes(x = DATE, y = value / 10 ^ 8)
    ) +
    geom_line(aes(color = variable, linetype = variable), size = 1.3) +
    geom_hline(yintercept = 0,
               size = 1,
               colour = "#333333") +
    scale_color_manual(values = c('#1380A1', '#FAAB18', '#FAAB18')) +
    scale_linetype_manual(values = c('solid', 'solid', 'dashed')) +
    bbc_style() +
    scale_y_continuous(labels = scales::comma)  +
    theme(legend.key.width = unit(1.5, "cm"),
          legend.text = element_text(size = 18)) +
    labs(title = "증거금필요액 추이 - '22.5.31~6.30",
         subtitle = "실제포지션 vs 고정포지션(단위: 억원)") +
    scale_x_date(date_breaks = "2 weeks" , date_labels = "%y.%b.%d")



finalise_plot(plot_name = DailyIM_period4,
              source_name = '',
              save_filepath = 'DailyIM_period4.png',
              width_pixels = 800,
              height_pixels = 450)


