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
library(clipr)


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

# DailyIM(TOTAL)---------------------------------------------------------
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

## graph

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





# fixed_position Series(TOTAL IM)--------------------------------------------

load('IM_table_period1.Rdata')
load('IM_table_period2.Rdata')
load('IM_table_period3.Rdata')
load('IM_table_period4.Rdata')
load('IM_table_period4_sub1.Rdata')
load('IM_table_period4_sub2.Rdata')

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
    labs(title = "[기간1]증거금필요액 추이",
         subtitle = "'20.1.23~'20.4.8(단위: 억원)") +
    scale_x_date(date_breaks = "2 weeks" , date_labels = "%y.%b.%d")



finalise_plot(plot_name = DailyIM_period1,
              source_name = '',
              save_filepath = 'DailyIM_period1.png',
              width_pixels = 500,
              height_pixels = 500)


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
    labs(title = "[기간2]증거금필요액 추이",
         subtitle = "'21.9.24~'21.12.6(단위: 억원)") +
    scale_x_date(date_breaks = "2 weeks" , date_labels = "%y.%b.%d")



finalise_plot(plot_name = DailyIM_period2,
              source_name = '',
              save_filepath = 'DailyIM_period2.png',
              width_pixels = 520,
              height_pixels = 520)


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
    labs(title = "[기간3]증거금필요액 추이",
         subtitle = "'22.3.21~'22.5.17(단위: 억원)") +
    scale_x_date(date_breaks = "2 weeks" , date_labels = "%y.%b.%d")



finalise_plot(plot_name = DailyIM_period3,
              source_name = '',
              save_filepath = 'DailyIM_period3.png',
              width_pixels = 520,
              height_pixels = 520)



DailyIM_period4 <-
    ggplot(
        left_join(IM_table_period4,
                  left_join(IM_table_period4_sub1[,c(1, 3)], IM_table_period4_sub2[,c(1, 3)], by = 'DATE'), 
                  by  = 'DATE') %>%
            mutate(IM_fixed.y = ifelse(is.na(IM_fixed.y) == TRUE, IM_fixed.x, IM_fixed.y),
                   IM.y = ifelse(is.na(IM.y) == TRUE, IM.x, IM.y)) %>% 
            rename('실제포지션(99.9%)' = IM.x,
                   '실제포지션(99.7%)' = IM.y,
                   '고정포지션(99.7%)' = IM_fixed.x,
                   '고정포지션(99.9%)' = IM_fixed.y) %>%
            select(DATE, '실제포지션(99.9%)', '실제포지션(99.7%)', '고정포지션(99.9%)', '고정포지션(99.7%)') %>% 
            melt(id.vars = 'DATE'),
        aes(x = DATE, y = value / 10 ^ 8)
    ) +
    geom_line(aes(color = variable, linetype = variable), size = 1.3) +
    geom_hline(yintercept = 0,
               size = 1,
               colour = "#333333") +
    scale_color_manual(values = c('#1380A1', '#1380A1', '#FAAB18', '#FAAB18')) +
    scale_linetype_manual(values = c('solid', 'dashed', 'solid', 'dashed')) +
    bbc_style() +
    scale_y_continuous(labels = scales::comma)  +
    theme(legend.key.width = unit(1.5, "cm"),
          legend.text = element_text(size = 18)) +
    guides(color = guide_legend(nrow = 2)) + 
    labs(title = "[기간4]증거금필요액 추이",
         subtitle = "'22.5.31~'22.6.30(단위: 억원)") +
    scale_x_date(date_breaks = "2 weeks" , date_labels = "%y.%b.%d")



finalise_plot(plot_name = DailyIM_period4,
              source_name = '',
              save_filepath = 'DailyIM_period4.png',
              width_pixels = 520,
              height_pixels = 520)


# fixed_position Series(ACC)--------------------------------------------

load('IM_fixed_period1.Rdata')
load('IM_fixed_period1_real.Rdata')

load('IM_fixed_period2.Rdata')
load('IM_fixed_period2_real.Rdata')


load('IM_fixed_period3.Rdata')
load('IM_fixed_period3_real.Rdata')

load('IM_fixed_period4.Rdata')
load('IM_fixed_period4_real.Rdata')




#### Period1

IM_fixed_period1[,56] <- IM_fixed_period1[,47] / IM_fixed_period1[,3]
IM_fixed_period1[,57] <- -IM_fixed_period1[,47] + IM_fixed_period1[,3]
colnames(IM_fixed_period1)[56] <- '고정포지션_증가율'
colnames(IM_fixed_period1)[57] <- '고정포지션_증가분'


IM_fixed_period1_real[,56] <- IM_fixed_period1_real[,47] / IM_fixed_period1_real[,3]
IM_fixed_period1_real[,57] <- -IM_fixed_period1_real[,47] + IM_fixed_period1_real[,3]
colnames(IM_fixed_period1_real)[56] <- '실제포지션_증가율'
colnames(IM_fixed_period1_real)[57] <- '실제포지션_증가분'

Asc_Rate_period1 <-
    left_join(IM_fixed_period1_real[, c(1, 2, 56, 57)],
              IM_fixed_period1[, c(1, 56, 57)],
              by = 'Accnum') %>%
    filter(is.na(실제포지션_증가율) == FALSE)

Asc_Rate_period1[, c(3, 5)] %>% apply(2, mean)


Asc_R_period1 <-
    ggplot(melt(Asc_Rate_period1[, c(1, 3, 5)], by = Accnum), 
           aes(variable, (value - 1) * 100)) +
    geom_boxplot(color = "gray20",
                 width = 0.15,
                 alpha = 0.25) +
    stat_summary(fun.y = "mean",
                 color = 'red',
                 shape = 8) +
    geom_violin(aes(col = variable, fill = variable), alpha = 0.25) +
    bbc_style() +
    scale_y_continuous(labels = scales::comma,
                       breaks = seq(-100, 400, 100)) +
    theme(legend.position = 'none') +
    labs(title = "[기간1]계좌별 증거금 증가율",
         subtitle = "'20.1.23~'20.3.27(단위: %)")


finalise_plot(plot_name = Asc_R_period1,
              source_name = '',
              save_filepath = 'Asc_R_period1.png',
              width_pixels = 520,
              height_pixels = 520)


clipr::write_clip(Asc_Rate_period1)


#### Period2

IM_fixed_period2[,53] <- IM_fixed_period2[,50] / IM_fixed_period2[,3]
IM_fixed_period2[,54] <- -IM_fixed_period2[,50] + IM_fixed_period2[,3]
colnames(IM_fixed_period2)[53] <- '고정포지션_증가율'
colnames(IM_fixed_period2)[54] <- '고정포지션_증가분'


IM_fixed_period2_real[,53] <- IM_fixed_period2_real[,50] / IM_fixed_period2_real[,3]
IM_fixed_period2_real[,54] <- -IM_fixed_period2_real[,50] + IM_fixed_period2_real[,3]
colnames(IM_fixed_period2_real)[53] <- '실제포지션_증가율'
colnames(IM_fixed_period2_real)[54] <- '실제포지션_증가분'

Asc_Rate_period2 <-
    left_join(IM_fixed_period2_real[, c(1, 2, 53, 54)],
              IM_fixed_period2[, c(1, 53, 54)],
              by = 'Accnum') %>%
    filter(is.na(실제포지션_증가율) == FALSE)

Asc_Rate_period2 %>% filter(Accnum != '555010000001') %>% ungroup()  %>% select(실제포지션_증가율, 고정포지션_증가율) %>% 
    apply(2, mean)


Asc_R_period2 <- ggplot(melt(Asc_Rate_period2[, c(1, 3, 5)] %>% filter(Accnum != '555010000001'), by = Accnum), aes(variable, (value - 1) * 100)) +
    geom_boxplot(color = "gray20",
                 width = 0.15,
                 alpha = 0.25) +
    stat_summary(fun.y = "mean", color = 'red', shape = 8) +
    geom_violin(aes(col = variable, fill = variable), alpha = 0.25) +
    bbc_style() +
    scale_y_continuous(labels = scales::comma,
                       breaks = seq(-100, 400, 100))  +
    theme(legend.position = 'none') +
    labs(title = "[기간2]계좌별 증거금 증가율",
         subtitle = "'21.9.24~'21.12.2(단위: %)") 


finalise_plot(plot_name = Asc_R_period2,
              source_name = '',
              save_filepath = 'Asc_R_period2.png',
              width_pixels = 520,
              height_pixels = 520)


clipr::write_clip(Asc_Rate_period2)


#### Period3


IM_fixed_period3[,44] <- IM_fixed_period3[,41] / IM_fixed_period3[,3]
IM_fixed_period3[,45] <- -IM_fixed_period3[,41] + IM_fixed_period3[,3]
colnames(IM_fixed_period3)[44] <- '고정포지션_증가율'
colnames(IM_fixed_period3)[45] <- '고정포지션_증가분'


IM_fixed_period3_real[,44] <- IM_fixed_period3_real[,41] / IM_fixed_period3_real[,3]
IM_fixed_period3_real[,45] <- -IM_fixed_period3_real[,41] + IM_fixed_period3_real[,3]
colnames(IM_fixed_period3_real)[44] <- '실제포지션_증가율'
colnames(IM_fixed_period3_real)[45] <- '실제포지션_증가분'
IM_fixed_period3_real <- IM_fixed_period3_real[-61, ]

Asc_Rate_period3 <-
    left_join(IM_fixed_period3_real[, c(1, 2, 44, 45)],
              IM_fixed_period3[, c(1, 44, 45)],
              by = 'Accnum') %>%
    filter(is.na(실제포지션_증가율) == FALSE)


Asc_Rate_period3[, c(3, 5)] %>% apply(2, mean)

Asc_Rate_period3 %>% filter(Accnum != '025010000001' & Accnum != '046010000001') %>% ungroup()  %>% select(실제포지션_증가율, 고정포지션_증가율) %>% 
    apply(2, mean)


Asc_R_period3 <-
    ggplot(melt(Asc_Rate_period3[, c(1, 3, 5)] %>% filter(Accnum != '025010000001'& Accnum != '046010000001'), by = Accnum), aes(variable, (value - 1) * 100)) +
    geom_boxplot(color = "gray20",
                 width = 0.15,
                 alpha = 0.25) +
    stat_summary(fun.y = "mean",
                 color = 'red',
                 shape = 8) +
    geom_violin(aes(col = variable, fill = variable), alpha = 0.25) +
    bbc_style() +
    scale_y_continuous(labels = scales::comma,
                       breaks = seq(-100, 400, 100)) +
    theme(legend.position = 'none') +
    labs(title = "[기간3]계좌별 증거금 증가율",
         subtitle = "'22.3.21~'22.5.13(단위: %)")


finalise_plot(plot_name = Asc_R_period3,
              source_name = '',
              save_filepath = 'Asc_R_period3.png',
              width_pixels = 520,
              height_pixels = 520)


clipr::write_clip(Asc_Rate_period3)



#### Period4

IM_fixed_period4[,24] <- IM_fixed_period4[,19] / IM_fixed_period4[,3]
IM_fixed_period4[,25] <- -IM_fixed_period4[,19] + IM_fixed_period4[,3]
colnames(IM_fixed_period4)[24] <- '고정포지션_증가율'
colnames(IM_fixed_period4)[25] <- '고정포지션_증가분'


IM_fixed_period4_real[,24] <- IM_fixed_period4_real[,19] / IM_fixed_period4_real[,3]
IM_fixed_period4_real[,25] <- -IM_fixed_period4_real[,19] + IM_fixed_period4_real[,3]
colnames(IM_fixed_period4_real)[24] <- '실제포지션_증가율'
colnames(IM_fixed_period4_real)[25] <- '실제포지션_증가분'

IM_fixed_period4_real <- IM_fixed_period4_real[-61, ]

Asc_Rate_period4 <-
    left_join(IM_fixed_period4_real[, c(1, 2, 24, 25)],
              IM_fixed_period4[, c(1, 24, 25)],
              by = 'Accnum') %>%
    filter(is.na(실제포지션_증가율) == FALSE)


Asc_Rate_period4[, c(3, 5)] %>% apply(2, mean)


Asc_R_period4 <-
    ggplot(melt(Asc_Rate_period4[, c(1, 3, 5)], by = Accnum), 
           aes(variable, (value - 1) * 100)) +
    geom_boxplot(color = "gray20",
                 width = 0.15,
                 alpha = 0.25) +
    stat_summary(fun.y = "mean",
                 color = 'red',
                 shape = 8) +
    geom_violin(aes(col = variable, fill = variable), alpha = 0.25) +
    bbc_style() +
    scale_y_continuous(labels = scales::comma,
                       breaks = seq(-50, 400, 50)) +
    theme(legend.position = 'none') +
    labs(title = "[기간4]계좌별 증거금 증가율",
         subtitle = "'22.5.31~'22.6.24(단위: %)")


finalise_plot(plot_name = Asc_R_period4,
              source_name = '',
              save_filepath = 'Asc_R_period4.png',
              width_pixels = 520,
              height_pixels = 520)


clipr::write_clip(Asc_Rate_period4)







# 10Y VaR -----------------------------------------------------

load('IM_10Y.Rdata')
load('IM_5Y.Rdata')

IM_10Y <- na.omit(IM_10Y)
IM_5Y <- na.omit(IM_5Y)


IM_5Y_sum <- data.frame(apply(IM_5Y[, -c(1,2)], 2, sum))
colnames(IM_5Y_sum) <- c('VaR_5Y')
IM_10Y_sum <- data.frame(apply(IM_10Y[, -c(1,2)], 2, sum))
colnames(IM_10Y_sum) <- c('VaR_10Y')

DailyIM_10Y <- cbind(rownames(IM_5Y_sum), IM_5Y_sum, IM_10Y_sum)
colnames(DailyIM_10Y)[1] <- 'DATE'
DailyIM_10Y <- DailyIM_10Y %>% mutate(DATE = as.Date(DATE))


DailyIM_10Y_graph <-
    ggplot(DailyIM_10Y %>% melt(id.vars = 'DATE'),
           aes(x = DATE, y = - value / 10 ^ 8, color = variable)) +
    geom_line(size = 1.3) +
    geom_hline(yintercept = 0,
               size = 1,
               colour = "#333333") +
    scale_color_manual(values = c('#1380A1', '#FAAB18')) +
    bbc_style() +
    scale_y_continuous(labels = scales::comma,
                       breaks = seq(0, 30000, 5000)) +
    theme(legend.key.width = unit(1.5,"cm"),
          legend.text=element_text(size = 18)) +
    labs(title = "증거금필요액 추이",
         subtitle = "5Y FHS-VaR 및 10Y HS-VaR(단위: 억원)") +
    scale_x_date(date_breaks = "6 month" , date_labels = "'%y.%b")



finalise_plot(plot_name = DailyIM_10Y_graph,
              source_name = '',
              save_filepath = 'DailyIM_10Y_graph.png',
              width_pixels = 900,
              height_pixels = 450)

