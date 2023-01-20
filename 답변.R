library(dplyr)
library(readxl)
library(xts)
library(tsibble)
students.all <- read_excel("./students.xlsx", skip = 16, na = '-', sheet = 1, col_types
                           = c('text', 'text', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric','numeric', 'numeric', 'numeric'))

students <- students.all %>%
  filter(지역규모 == '계') %>% select(-지역규모)

students$연도 <- as.Date(paste0(students$연도, '-01-01'))

students.ts <- ts(students, frequency = 1, start = 1999)

students.xts <- as.xts(students[,-1], order.by = students$연도)

students.tsibble <- students %>%
  mutate(연도 = yearmonth(paste0(students$연도, '-01-01')))

students.tsibble <- as_tsibble(students.tsibble, index = 연도)


###  2.3.2 CSV 파일

employees <- read.csv('./산업별_취업자_20210206234505.csv', header = TRUE, na = '-', strip.white = TRUE, stringsAsFactors = TRUE)

colnames(employees) <- c('time', 'total', 'employees.edu')

employees$time <- as.Date(paste0(employees$time, '. 01'), format = '%Y. %m. %d')

employees.ts <- ts(employees, start = c(2013, 01), frequency = 12)

employees.xts <- xts(employees[,2:3], order.by = employees[,1])

employees.tsibble <- as_tsibble(employees, index = time)


###  2.3.3 추가 실습 데이터 생성
covid19 <- read.csv('./covid19.csv', header = TRUE, na = '-', strip.white = TRUE, stringsAsFactors = TRUE)

colnames(covid19) <- c('category', 'status', 'date', 'value')

covid19 <- covid19[, c(3, 1, 2, 4)]

covid19$date <- as.Date(covid19$date, "%Y. %m. %d")

covid19 <- covid19 %>%
  filter(grepl('세', category)) %>%
  filter(category != '세종')

covid19$value <- ifelse(is.na(covid19$value), 0, covid19$value)

covid19 <- tidyr::spread(covid19, category, value)

covid19.ts <- ts(covid19[, 2:10], frequency = 365)

covid19.xts <- as.xts(covid19[, 3:10], order.by = covid19$date)

covid19.tsibble <- as_tsibble(covid19, index = date)


######################################
###  7장 시계열 forecasting Part III - 시계열 분석 프레임워크

###  7.2 fable 프레임워크
###  7.2.1 미래 학생수 예측

split <- floor(nrow(students.tsibble) * 0.8)

students.tsibble.tr <- students.tsibble[1:split, ]

students.tsibble.test <- students.tsibble[(split+1):nrow(students.tsibble), ]

library(fable)

library(forecast)

library(fable.prophet)

model.fable.students <- model(students.tsibble.tr,
                              ets = ETS(학생수계),
                              arima = ARIMA(학생수계),
                              naive = NAIVE(학생수계),
                              tslm = TSLM(학생수계 ~ trend()),
                              rw = RW(학생수계),
                              mean = MEAN(학생수계),
                              nnetar = NNETAR(학생수계),
                              prophet = fable.prophet::prophet(학생수계), 
                              var = VAR(학생수계)
)

forecast.fable.students <- fabletools::forecast(model.fable.students, h = 10)

autoplot(forecast.fable.students, students.tsibble, level = NULL)

accuracy(forecast.fable.students, students.tsibble.test) %>%
  arrange(RMSE)

best.model.fable.students <- model.fable.students %>%
  select(ets, prophet)

best.model.fable.students %>% forecast(h = 10) %>%
  autoplot(students.tsibble, alpha = 0.6, level = NULL) +
  autolayer(fitted(best.model.fable.students))

### 7.2.2 미래 취업자수 예측

employees$yearmonth <- yearmonth(employees$time)

employees.tsibble <- as_tsibble(employees, index = yearmonth)

split <- floor(nrow(employees.tsibble) * 0.9)

n <- nrow(employees.tsibble)

employees.tsibble.tr <- employees.tsibble[1:split, ]

employees.tsibble.test <- employees.tsibble[(split+1):n, ]

model.fable.employees <- employees.tsibble.tr %>%
  model(ets = ETS(total),
        arima = ARIMA(total),
        naive = NAIVE(total),
        tslm = TSLM(total~trend() + season(12)),
        rw = RW(total),
        mean = MEAN(total),
        nnetar = NNETAR(total),
        prophet = fable.prophet::prophet(total)
  )

forecast.fable.employees <- model.fable.employees %>% forecast(h = 24)

forecast.fable.employees %>%
  autoplot(employees.tsibble, level = NULL) +
  ggplot2::labs(title = 'fable로 생성한 8가지 모델 예측 플롯', x = '연월', y = '취업자수')

forecast.fable.employees %>% forecast::accuracy(employees.tsibble.test) %>% arrange(RMSE)

best.model.fable.employees <- model.fable.employees %>%
  select(naive, rw)

library(ggplot2)

best.model.fable.employees %>%
  forecast(h = 12) %>%
  autoplot(employees.tsibble, level = NULL, lwd = 1) +
  autolayer(fitted(best.model.fable.employees), lwd = 1) +
  geom_point(aes(x = yearmonth, y = total)) +
  labs(title = '전체 취업자수 예측 모델', x = '연월', y = '취업자수')

### 7.2.3 미래 코로나 확진자수 예측

fill.covid19.tsibble <- fill_gaps(covid19.tsibble, `0-9세` = 0)

split <- floor(nrow(fill.covid19.tsibble) * 0.9)

n <- nrow(fill.covid19.tsibble)

fill.covid19.tsibble.tr <- fill.covid19.tsibble[1:split, ]

fill.covid19.tsibble.test <- fill.covid19.tsibble[(split+1):n, ]

model.covid19.tsibble <- fill.covid19.tsibble.tr %>%
  model(ets = ETS(`0-9세`),
        arima = ARIMA(`0-9세`),
        naive = NAIVE(`0-9세`),
        tslm = TSLM(`0-9세`),
        rw = RW(`0-9세`),
        mean = MEAN(`0-9세`),
        nnetar = NNETAR(`0-9세`),
        prophet = prophet(`0-9세`)
  )

forecast.covid19.tsibble <- model.covid19.tsibble %>%
  forecast(h = 120)

forecast.covid19.tsibble %>% autoplot(fill.covid19.tsibble, level = NULL) + labs(title = '코로나 확진자(0-9세)에 대한 8가지 모델 예측 결과', x = '날짜', y = '확진자수')

forecast.covid19.tsibble %>% forecast::accuracy(fill.covid19.tsibble.test) %>% arrange(RMSE)

best.model.covid19.tsibble <- model.covid19.tsibble %>% select(prophet)

best.model.covid19.tsibble %>%
  forecast(h = 120) %>%
  autoplot(fill.covid19.tsibble, lwd = 1, alpha = 0.6) +
  autolayer(fitted(best.model.covid19.tsibble), lwd = 1) +
  geom_point(aes(x = date, y = `0-9세`)) +
  labs(title = '코로나 확진자수(0-9세) 예측', x = '연월일', y = '확진자수')

