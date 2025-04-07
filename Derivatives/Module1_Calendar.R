# 캘린더 생성----------------------------------------

## Read monthly calendar
Cal_raw <- read_csv(paste0(
    getwd(),
    "/Calendar/KRXCH1(",
    year(BDate),
    "_",
    month(BDate),
    ").csv"
))    

## Read calendar before 2016
Cal_pre2016 <- read_csv(paste0(
    getwd(),
    "/Calendar/pre_2016.csv"
))


## Convert, Good Business Day <-> 1, Holiday <-> 0
Cal_raw <- Cal_raw %>%
    mutate(SeBre = ifelse(SeB == "g", 1, 0))  #monthly cal 변환
Cal_pre2016 <- Cal_pre2016 %>% 
    mutate(SeBre = ifelse(SeB == "g", 1, 0))  #2016년 이전 cal 변환


## KRW Calendar : bind two calendars(pre2016 & monthly)
Cal_KRW <- bind_rows(Cal_pre2016 %>% select(DATE, SeBre),
                     Cal_raw %>% select(DATE, SeBre))

## USD Calendar(미사용)
#  Cal_USD <- Cal_raw %>%
#     convert(num(2:6)) %>%
#     select(DATE, NYB, LnB, SeB) %>%
#     mutate(NYBLnB = NYB * LnB, NYBLnBSeB = NYBLnB * SeB)



# Apply Biz Day Convention------------------------------

## New columns for calculating Preceding, Following
Cal_KRW <- Cal_KRW %>%
    rename('SeB' = SeBre) %>% 
    mutate(SeB_new = SeB - 1,
           SeB_sum1 = 0,
           SeB_sum2 = 0)

## SeB_sum1 == continual Holidays
for (i in 1:nrow(Cal_KRW)) {
    ifelse(Cal_KRW[i, 3] == -1, 
           Cal_KRW[i, 4] <- Cal_KRW[i, 3] + Cal_KRW[i - 1, 4],
           Cal_KRW[i, 4] <- 0)
}

## SeB_sum2 == continual Holidays(inverse)
for (j in nrow(Cal_KRW):1) {
    ifelse(Cal_KRW[j, 3] == -1,
           Cal_KRW[j, 5] <- Cal_KRW[j, 3] + Cal_KRW[j + 1, 5],
           Cal_KRW[j, 5] <- 0)
}

## Business Day Result
Cal_KRW <- Cal_KRW %>%
    select(DATE, SeB, SeB_sum1, SeB_sum2) %>%
    mutate(
        SeB_pre = DATE + SeB_sum1,              #preceding
        SeB_fol = DATE - SeB_sum2,              #following
        SeB_modfol = as.Date(                   #modified following
            ifelse(month(DATE) == month(SeB_fol),
                   SeB_fol,
                   SeB_pre),
            origin = '1970-01-01')
    ) 


#### Remove Objects
rm(Cal_pre2016, Cal_raw, i , j)
