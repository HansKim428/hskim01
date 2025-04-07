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

# extrafont::font_import(pattern = 'Helvetica')
# extrafont::loadfonts(device='win')
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





# PT/ND Measure------------------------------
System_csv <- data.frame(read_csv('System_procycle.csv'))


PT_measure <- System_csv %>% filter(Measure == 'PT')
PT_measure$DATE <- as.Date(PT_measure$DATE)


# PT Graph--------------------------------------------
PT_graph <-
    ggplot(
        PT_measure %>%
            filter(Maturity %in% c('1Y', '5Y', '20Y')) %>%
            select(DATE, Maturity, lamb_97_final),
        aes(x = DATE, y = lamb_97_final, color = Maturity)
    ) +
    geom_line(size = 1.5, alpha = 0.5) +
    geom_hline(yintercept = 0,
               size = 1,
               colour = "#333333") +
    bbc_style() +
    scale_y_continuous(labels = scales::comma)  +
    theme(legend.key.width = unit(1.5, "cm"),
          legend.text = element_text(size = 18)) +
    scale_color_discrete(breaks=c('1Y', '5Y', '20Y')) +
    labs(title = '장기순응성 추이',
         subtitle = "1,250일 정점-저점 비율") +
    scale_x_date(date_breaks = "6 month" , date_labels = "'%y.%b")


finalise_plot(plot_name = PT_graph,
              source_name = '',
              save_filepath = 'PT_measure.png',
              width_pixels = 800,
              height_pixels = 450)



# ND Graph-----------------------------------------
ND_measure <- System_csv %>% filter(Measure != 'PT')
ND_measure$DATE <- as.Date(ND_measure$DATE)
# 
# 
# ND_graph_1D <-
#     ggplot(
#         ND_measure %>%
#             filter(Maturity %in% c('1Y', '5Y', '20Y') & Measure == '1일') %>%
#             select(DATE, Maturity, lamb_97_final),
#         aes(x = DATE, y = lamb_97_final, color = Maturity)
#     ) +
#     geom_line(size = 1.5, alpha = 0.5) +
#     geom_hline(yintercept = 0,
#                size = 1,
#                colour = "#333333") +
#     bbc_style() +
#     scale_y_continuous(labels = scales::comma)  +
#     theme(legend.key.width = unit(1.5, "cm"),
#           legend.text = element_text(size = 18)) +
#     scale_color_discrete(breaks=c('1Y', '5Y', '20Y')) +
#     labs(title = '단기순응성 추이',
#          subtitle = "1일 차분")
# 
# 
ND_graph_5D <-
    ggplot(
        ND_measure %>%
            filter(Maturity %in% c('1Y', '5Y', '20Y') & Measure == '5일') %>%
            select(DATE, Maturity, lamb_97_final),
        aes(x = DATE, y = lamb_97_final, color = Maturity)
    ) +
    geom_line(size = 1.5, alpha = 0.5) +
    geom_hline(yintercept = 0,
               size = 1,
               colour = "#333333") +
    bbc_style() +
    scale_y_continuous(labels = scales::comma)  +
    theme(legend.key.width = unit(1.5, "cm"),
          legend.text = element_text(size = 18)) +
    scale_color_discrete(breaks=c('1Y', '5Y', '20Y')) +
    labs(title = '단기순응성 추이',
         subtitle = "5일 차분")
# 
# 
# ND_graph_30D <-
#     ggplot(
#         ND_measure %>%
#             filter(Maturity %in% c('1Y', '5Y', '20Y') & Measure == '30일') %>%
#             select(DATE, Maturity, lamb_97_final),
#         aes(x = DATE, y = lamb_97_final, color = Maturity)
#     ) +
#     geom_line(size = 1.5, alpha = 0.5) +
#     geom_hline(yintercept = 0,
#                size = 1,
#                colour = "#333333") +
#     bbc_style() +
#     scale_y_continuous(labels = scales::comma)  +
#     theme(legend.key.width = unit(1.5, "cm"),
#           legend.text = element_text(size = 18)) +
#     scale_color_discrete(breaks=c('1Y', '5Y', '20Y')) +
#     labs(title = '단단기순응성 추이',
#          subtitle = "30일 차분")



ND_graph_facet <-
    ggplot(
        ND_measure %>%
            filter(Maturity %in% c('1Y', '5Y', '20Y')) %>%
            select(DATE, Measure, Maturity, lamb_97_final),
        aes(x = DATE, y = lamb_97_final, color = Maturity)
    ) +
    facet_wrap(~ factor(Measure, levels = c('1일','5일','30일')), ncol = 3) +
    geom_line(size = 1.3) +
    geom_hline(yintercept = 0,
               size = 1,
               colour = "#333333") +
    bbc_style() +
    scale_y_continuous(labels = scales::comma)  +
    theme(legend.key.width = unit(1.5, "cm"),
          legend.text = element_text(size = 18),
          legend.box.margin = margin(0, 0, 0, 0)) +
    scale_color_discrete(breaks=c('1Y', '5Y', '20Y')) +
    labs(title = '단기순응성 추이',
         subtitle = "n일 증거금 증가분(n=1,5,30)") +
    scale_x_date(date_breaks = "years" , date_labels = "'%y")


finalise_plot(plot_name = ND_graph_facet,
              source_name = '',
              save_filepath = 'ND_measure.png',
              width_pixels = 800,
              height_pixels = 450)

