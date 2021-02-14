student.ts.lm <- tslm(students.total.ts[,3] ~ trend, data = students.total.ts)
summary(student.ts.lm)
student.ts.lm %>% forecast()  # tslm 함수로 생성된 모델을 forecast()함수를 통해 예측값을 생성
student.ts.lm %>% forecast() %>% autoplot()
student.ts.lm <- tslm(students.total.ts[,3] ~ trend, data = students.total.ts, lambda = 1)  # 초등학생 학생수를 예측모델에 독립변수로 트랜드를 사용하는 선형 모델을 생성
student.ts.lm %>% forecast(h = 22) %>% autoplot()

student.ts.lm <- tslm(students.total.ts[,5] ~ students.total.ts[,4] + trend, data = students.total.ts)  # 초등학생 학생수를 예측모델에 독립변수로 유치원 학생수와 트랜드를 사용하는 선형 모델을 생성
student.ts.lm %>% forecast(h = 22) %>% autoplot()


t <- time(students.total.ts[,3])
checkresiduals(student.ts.lm)
t_break1 <- 2005
t_break2 <- 2013

tb1 <- ts(pmax(0, t - t_break1), start = 1999)
tb2 <- ts(pmax(0, t - t_break2), start = 1999)

tslm(students.total.ts[,3] ~ t + tb1 + tb2) %>% forecast(h = 22)

summary(wo(students.total.ts[,3]))

((0.99 * 2747215) + (0.01 *2711381))

summary(ses(students.total.ts[,3], h = 5))
i <- 1
for(i in 1:nrow(students.total.ts)) {
  print(i)
  print(round(mean(students.total.ts[1:i,3]), 3))
}
fitted(ses(students.total.ts[,3], h = 5))

mean(students.total.ts[1:10,3])


autoplot(students.total.ts[,1], color = 'black') +
  autolayer(fitted(ses(students.total.ts[,1], alpha = 0.1)), series = 'alpha = 0.1') +
  autolayer(ses(students.total.ts[,1], alpha = 0.1, PI = FALSE), series = 'alpha = 0.1') +
  autolayer(fitted(ses(students.total.ts[,1], alpha = 0.2)), series = 'alpha = 0.2') + 
  autolayer(ses(students.total.ts[,1], alpha = 0.2, PI = FALSE), series = 'alpha = 0.2') +
  autolayer(fitted(ses(students.total.ts[,1], alpha = 0.3)), series = 'alpha = 0.3') +
  autolayer(ses(students.total.ts[,1], alpha = 0.3, PI = FALSE), series = 'alpha = 0.3') +
  autolayer(fitted(ses(students.total.ts[,1], alpha = 0.4)), series = 'alpha = 0.4') +
  autolayer(ses(students.total.ts[,1], alpha = 0.4, PI = FALSE), series = 'alpha = 0.4') +
  autolayer(fitted(ses(students.total.ts[,1], alpha = 0.5)), series = 'alpha = 0.5') +
  autolayer(ses(students.total.ts[,1], alpha = 0.5, PI = FALSE), series = 'alpha = 0.5') +
  autolayer(fitted(ses(students.total.ts[,1], alpha = 0.6)), series = 'alpha = 0.6') +
  autolayer(ses(students.total.ts[,1], alpha = 0.6, PI = FALSE), series = 'alpha = 0.6') +
  autolayer(fitted(ses(students.total.ts[,1], alpha = 0.7)), series = 'alpha = 0.7') +
  autolayer(ses(students.total.ts[,1], alpha = 0.7, PI = FALSE), series = 'alpha = 0.7') +
  autolayer(fitted(ses(students.total.ts[,1], alpha = 0.8)), series = 'alpha = 0.8') +
  autolayer(ses(students.total.ts[,1], alpha = 0.8, PI = FALSE), series = 'alpha = 0.8') +
  autolayer(fitted(ses(students.total.ts[,1], alpha = 0.9)), series = 'alpha = 0.9') +
  autolayer(ses(students.total.ts[,1], alpha = 0.9, PI = FALSE), series = 'alpha = 0.9')
  

students.modeltime <- students %>% filter(지역규모 == '계') %>% select(1, 3)
students.modeltime[,1] <- as.Date(as.character(paste0(students.modeltime[,1], '-01-01')), format = '%Y-%M-%d')
skim(students.modeltime)
glimpse(students.modeltime)
### 트레이닝 셋과 테스트 셋을 나눈다
splits <- initial_time_split(students.modeltime, prop = 0.9)
training(splits)
testing(splits)
###  trend와 season을 반영하여 linear model을 생성
model_fit_lm <- linear_reg() %>%
  set_engine("lm") %>%
  fit(학생수계 ~ as.numeric(연도),
      data = students.modeltime)

model_fit_lm_split <- linear_reg() %>%
  set_engine("lm") %>%
  fit(학생수계 ~ as.numeric(연도),
          data = training(splits))

###  모델 테이블 생성
model_tbl <- modeltime_table(model_fit_lm)
model_tbl_split <- modeltime_table(model_fit_lm_split)

###  테스팅 셋으로 모델 교정
calibration_tbl <- model_tbl %>% modeltime_calibrate(new_data = students.modeltime, quiet = TRUE)
calibration_tbl_split <- model_tbl_split %>% modeltime_calibrate(new_data = testing(splits), quiet = TRUE)

calibration_tbl %>%
  modeltime_accuracy()


###  3년 예측치 생성후 plotting
model_tbl %>%
  modeltime_forecast(
    new_data    = students.modeltime,
#    h = '10 years',
    actual_data = students.modeltime
  )%>%
  plot_modeltime_forecast(
    .interactive      = TRUE
  )

model_tbl_split %>%
  modeltime_forecast(
    #   new_data    = students.modeltime,
    h = '3 years',
    actual_data = students.modeltime
  )%>%
  plot_modeltime_forecast(
    .interactive      = TRUE
  )


students.modeltime <- students %>% filter(지역규모 == '계') %>% select(1, 3)
students.modeltime[,1] <- as.Date(as.character(paste0(students.modeltime[,1], '-01-01')), format = '%Y-%M-%d')
splits <- initial_time_split(students.modeltime, prop = 0.9)
training(splits)
testing(splits)

model_fit_lm <- linear_reg() %>%
  set_engine("lm") %>%
  fit(학생수계 ~ 연도,
          data = training(splits))

model_fit_ets <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(학생수계 ~ 연도, data = training(splits))

models_tbl <- modeltime_table(
  model_fit_lm,
  model_fit_ets
)

calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(splits))

calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = students.modeltime
  ) %>%
  plot_modeltime_forecast(
    .interactive      = TRUE
  )

models_tbl %>%
  modeltime_forecast(
#    new_data    = testing(splits),
    h = '10 years',
    actual_data = students.modeltime
  ) %>%
  plot_modeltime_forecast(
    .interactive      = TRUE
  )

HoltWinters(students.total.ts[,1])
Holt


students <- read.csv('./students.csv', skip = 16, header = TRUE, na = '-', strip.white = TRUE, stringsAsFactors = TRUE)
students[, 3:18] <- apply(students[, 3:18], 2, function(y) as.numeric(gsub(",", "", y)))

as_tsibble(students, key = 지역규모, index = 연도)


students.tsibble %>%
  autoplot()

interval(students.tsibble)

data(ansett, packege = 'tsibble')
library(tsibble)
data(ansett)
update.packages('tsibble')
ansett %>% autoplot()

melsyd_economy <- ansett %>%
  filter(Airports == "MEL-SYD", Class == "Economy")

autoplot(melsyd_economy, Passengers)

data(a10)
library(fpp3)
library(feasts)
glimpse(students.tsibble)
students.tsibble %>%
  filter(지역규모 == '계') %>%
  gg_tsdisplay(학생수계)


students.tsibble %>%
  filter(지역규모 == '계') %>%
  gg_lag(학생수계)

students.tsibble %>%
  filter(지역규모 == '계') %>%
  gg_arma()


?Arima
auto.arima(students %>% filter(지역규모 == '계') %>% select(학생수계))
students %>% filter(지역규모 == '계') %>% select(3)


auto.arima(students.total.ts[,1])
par(mfrow = c(1, 2))
Acf(students.total.ts[,1])
Pacf(students.total.ts[,1])
par(mfrow = c(1, 1))
kpss.test(students.total.ts[,1])  ### kpss 테스트를 통해 생성된 데이터가 정상성인지 테스트 - 0.05보다 작으므로 정상성, 차분 필요
ndiffs(students.total.ts[,1], test = 'kpss')   ### 비정상성을 제거하기 위해 필요한 차분수


auto.arima(students.total.ts[,1]) %>% forecast() %>% autoplot()
auto.arima(students.total.ts[,1]) %>% forecast() %>% ggtsdisplay()
ggtsdisplay(students.total.ts[,1])

students.total.ts[,1] %>% tbats() %>% forecast() %>% autoplot()


nnetar(students.total.ts[,1]) %>% forecast() %>% autoplot()

interval <- interval(as.Date('1980-01-01'), as.Date('2021-12-31'))

students %>% 
  filter(지역규모 == '계') %>% 
  select(학생수계) %>% apply(2, diff)
?diff

lag <- students %>% filter(지역규모 == '계') %>% lag(1) %>% select(학생수계) %>% rename(lag = 학생수계)

cbind(students %>% filter(지역규모 == '계') %>% select(연도, 학생수계),  ### 연도와 학생수 컬럼을 선택
      students %>% filter(지역규모 == '계') %>% lag(1) %>% select(학생수계) %>% rename(전월 = 학생수계)) %>% ### lag(1) 함수를 사용하여 시차1 데이터 생성(하나씩 아래로 shift) 하고 컬럼명을 lag로 변경 
  mutate(증감 = 학생수계 - lag, 증감율 = round((학생수계/lag)-1, 3) * 100)  ### 시차1 데이터와 원 데이터의 차이를 증감 컬럼으로, 원데이터를 시차1 데이터로 나눈 수치를 1에서 빼준 결과(비중을 증감으로 변환)에 100을 곱한다(백분률로 변환)


index(pedestrian)
monthly_ped <- pedestrian %>%
  group_by_key() %>%
  index_by(Year_Month = ~ yearmonth(.)) %>%
  summarise(
    Max_Count = max(Count),
    Min_Count = min(Count)
  )


wide.covid19.by.age.tsibble[, c(1,3)]%>%
  index_by(yearmon = ~ yearmonth(.)) %>%
  summarise(sum.monthly = sum(`0-9세`))

students.tsibble%>% filter(지역규모 == '계') %>% select(1, 3) -> students.tsibble.계
students.tsibble.계 %>%
  mutate(증감 = difference(students.tsibble.계$학생수계, lag = 1)) %>%
  mutate(증감율 = round((증감/학생수계), 3) * 100) %>% head(10)

students.total.xts$증감 <- diff(students.total.xts[,2]) 
students.total.xts$증감율 <- round((students.total.xts$증감/students.total.xts$학생수계), 3) * 100
students.total.xts[, c('학생수계', '증감', '증감율')]


