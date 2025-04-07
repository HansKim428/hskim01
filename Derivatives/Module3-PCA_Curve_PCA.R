library(tidyverse)
library(readxl)
library(plotly)
library(lubridate)

Rate_raw_KRW <- read_csv('Rate.csv')

# 분석대상기간 : 현재 22년 상반기까지(3846; 다음분기 초일도 포함 필요)
Rate_raw_PCA <- Rate_raw_KRW[c(1:3846),]

#매분기


c(1, which((month(Rate_raw_PCA$DATE) %in% c(1, 7)) & (month(Rate_raw_PCA$DATE) != month(lag(Rate_raw_PCA$DATE, 1)))))


ec(which(year(Rate_raw_PCA$DATE) - year(lead(Rate_raw_PCA$DATE, 1)) == 1))


?lag
PCA <- prcomp(Rate_diff[, -c(1:3)], center = TRUE, scale = FALSE)
rotation <- PCA$rotation[, 1:3]
colmeans <- colMeans(Rate_diff[, -c(1:3)])
sds <- PCA$sdev[1:3]
PC_shock <- sqrt(5) * qnorm(0.999, 0, 1) * rotation %*% diag(sds)

PCA_scnDOO <- colmeans * 5 + PC_shock[ ,1]
PCA_scnUOO <- colmeans * 5 - PC_shock[ ,1]
PCA_scnDUO <- colmeans * 5 + PC_shock[ ,1] - PC_shock[ ,2]
PCA_scnDDO <- colmeans * 5 + PC_shock[ ,1] + PC_shock[ ,2]
PCA_scnUDO <- colmeans * 5 - PC_shock[ ,1] + PC_shock[ ,2]
PCA_scnUUO <- colmeans * 5 - PC_shock[ ,1] - PC_shock[ ,2]

PCA_scnDDD <- colmeans * 5 + PC_shock[ ,1] + PC_shock[ ,2] - PC_shock[ ,3]
PCA_scnDDU <- colmeans * 5 + PC_shock[ ,1] + PC_shock[ ,2] + PC_shock[ ,3]
PCA_scnDUD <- colmeans * 5 + PC_shock[ ,1] - PC_shock[ ,2] - PC_shock[ ,3]
PCA_scnDUU <- colmeans * 5 + PC_shock[ ,1] - PC_shock[ ,2] + PC_shock[ ,3]
PCA_scnUUU <- colmeans * 5 - PC_shock[ ,1] - PC_shock[ ,2] + PC_shock[ ,3]
PCA_scnUUD <- colmeans * 5 - PC_shock[ ,1] - PC_shock[ ,2] - PC_shock[ ,3]
PCA_scnUDU <- colmeans * 5 - PC_shock[ ,1] + PC_shock[ ,2] + PC_shock[ ,3]
PCA_scnUDD <- colmeans * 5 - PC_shock[ ,1] + PC_shock[ ,2] - PC_shock[ ,3]

PCA_scn <- data.frame(cbind(
    c(
        "PCA_scnDOO",
        "PCA_scnUOO",
        "PCA_scnDUO",
        "PCA_scnDDO",
        "PCA_scnUDO",
        "PCA_scnUUO",
        "PCA_scnDDD",
        "PCA_scnDDU",
        "PCA_scnDUD",
        "PCA_scnDUU",
        "PCA_scnUUU",
        "PCA_scnUUD",
        "PCA_scnUDU",
        "PCA_scnUDD"
    ),
    rbind(
        PCA_scnDOO,
        PCA_scnUOO,
        PCA_scnDUO,
        PCA_scnDDO,
        PCA_scnUDO,
        PCA_scnUUO,
        PCA_scnDDD,
        PCA_scnDDU,
        PCA_scnDUD,
        PCA_scnDUU,
        PCA_scnUUU,
        PCA_scnUUD,
        PCA_scnUDU,
        PCA_scnUDD
    )
))
