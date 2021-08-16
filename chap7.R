######################################
###  7장 시계열 forecasting Part III - 시계열 분석 프레임워크

###  7.2 fable 프레임워크
###  7.2.1 미래 학생수 예측

split <- floor(nrow(students.tsibble) * 0.8)

students.tsibble.tr <- students.tsibble[1:split, ]

students.tsibble.test <- students.tsibble[(split+1):nrow(students.tsibble), ]

library(fable)

library(fable.prophet)

model.fable.students <- model(students.tsibble.tr,
                              ets = ETS(학생수계),
                              arima = ARIMA(학생수계),
                              naive = NAIVE(학생수계),
                              tslm = TSLM(학생수계 ~ trend()),
                              rw = RW(학생수계),
                              mean = MEAN(학생수계),
                              nnetar = NNETAR(학생수계),
                              prophet = fable.prophet::prophet(학생수계)
)

forecast.fable.students <- forecast(model.fable.students, h = 10)

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
  labs(title = 'fable로 생성한 8가지 모델 예측 플롯', x = '연월', y = '취업자수')

forecast.fable.employees %>% forecast::accuracy(employees.tsibble.test) %>% arrange(RMSE)

best.model.fable.employees <- model.fable.employees %>%
  select(naive, rw)

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


###  7.3 modeltime 프레임워크
###  7.3.1 미래 학생수 예측

library(modeltime)

library(tidymodels)

splits.students <- initial_time_split(students, prop = 0.8)

model_fit_arima <- arima_reg() %>%
  set_engine(engine = "auto_arima") %>%
  fit(학생수계 ~ 연도, data = training(splits.students))

model_fit_ets <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(학생수계 ~ 연도, data = training(splits.students))

model_fit_prophet <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(학생수계 ~ 연도, data = training(splits.students))

model_fit_lm <- linear_reg() %>%
  set_engine("lm") %>%
  fit(학생수계 ~ 연도,
          data = training(splits.students))

model_fit_nnetar <- nnetar_reg() %>%
  set_engine("nnetar") %>%
  fit(학생수계 ~ 연도, data = training(splits.students))

model_fit_tbats <- seasonal_reg() %>%
  set_engine("tbats") %>%
  fit(학생수계 ~ 연도, data = training(splits.students))

(models_tbl <- modeltime_table(
  model_fit_arima,
  model_fit_ets,
  model_fit_prophet,
  model_fit_lm,
  model_fit_nnetar,
  model_fit_tbats))

(calibration_tbl <- models_tbl %>%
    modeltime_calibrate(new_data = testing(splits.students)))

calibration_tbl %>%
  modeltime_forecast(
    new_data = testing(splits.students),
    actual_data = students
  ) %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_show = FALSE
  ) +
  labs(title = 'modeltime을 사용한 전체 학생수 6개 모델 예측 결과', x = '연도', y = '학생수')

calibration_tbl %>%
  modeltime_accuracy() %>%
  arrange(rmse)

model_fit_ets <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(학생수계 ~ 연도, data = students)

model_fit_tbats <- seasonal_reg() %>%
  set_engine("tbats") %>%
  fit(학생수계 ~ 연도, data = students)

(models_tbl <- modeltime_table(
  model_fit_ets,
  model_fit_tbats))

models_tbl %>%
  modeltime_forecast(
    h = '10 years',
    actual_data = students
  ) %>%
  plot_modeltime_forecast(
    .interactive = FALSE
  )

###  7.3.2 미래 취업자수 예측

splits.employees <- initial_time_split(employees, prop = 0.9)

model_fit_arima <- arima_reg() %>%
  set_engine(engine = "auto_arima") %>%
  fit(total ~ time, data = training(splits.employees))

model_fit_ets <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(total ~ time, data = training(splits.employees))

model_fit_prophet <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(total ~ time, data = training(splits.employees))

model_fit_lm <- linear_reg() %>%
  set_engine("lm") %>%
  fit(total ~ time + factor(lubridate::month(time, label = TRUE), ordered = FALSE),
      data = training(splits.employees))

model_fit_nnetar <- nnetar_reg() %>%
  set_engine("nnetar") %>%
  fit(total ~ time, data = training(splits.employees))

model_fit_tbats <- seasonal_reg() %>%
  set_engine("tbats") %>%
  fit(total ~ time, data = training(splits.employees))

(models_tbl <- modeltime_table(
  model_fit_arima,
  model_fit_ets,
  model_fit_prophet,
  model_fit_lm,
  model_fit_nnetar,
  model_fit_tbats))

(calibration_tbl <- models_tbl %>%
    modeltime_calibrate(new_data = testing(splits.employees)))

calibration_tbl %>%
  modeltime_forecast(
    new_data = testing(splits.employees),
    actual_data = employees
  ) %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_show = FALSE
  ) + labs(title = 'modeltime을 사용한 신규 취업자수 6개 모델 예측 결과', x = '연도', y = '취업자수')

calibration_tbl %>%
  modeltime_accuracy() %>%
  arrange(rmse)

model_fit_lm <- linear_reg() %>%
  set_engine("lm") %>%
  fit(total ~ time + factor(lubridate::month(time, label = TRUE), ordered = FALSE),
      data = employees)

model_fit_nnetar <- nnetar_reg() %>%
  set_engine("nnetar") %>%
  fit(total ~ time, data = employees)

(models_tbl <- modeltime_table(
  model_fit_lm,
  model_fit_nnetar))

models_tbl %>%
  modeltime_forecast(
    h = '3 years',
    actual_data = employees
  ) %>%
  plot_modeltime_forecast(
    .interactive = FALSE
  ) + labs(title = '신규 취업자수 모델 예측 결과', x = '연도', y = '취업자수')

###  7.3.3 미래 코로나 확진자수 예측

splits.covid19 <- initial_time_split(covid19, prop = 0.9)

model_fit_arima <- arima_reg(seasonal_period = 365) %>%
  set_engine(engine = "auto_arima") %>%
  fit(`0-9세` ~ date, data = training(splits.covid19))

model_fit_ets <- exp_smoothing(seasonal_period = 365) %>%
  set_engine(engine = "ets") %>%
  fit(`0-9세` ~ date, data = training(splits.covid19))

model_fit_prophet <- prophet_reg(seasonality_daily = TRUE, seasonality_weekly = TRUE, ) %>%
  set_engine(engine = "prophet") %>%
  fit(`0-9세` ~ date, data = training(splits.covid19))

model_fit_lm <- linear_reg() %>%
  set_engine("lm") %>%
  fit(`0-9세` ~ date,
      data = training(splits.covid19))

model_fit_nnetar <- nnetar_reg() %>%
  set_engine("nnetar") %>%
  fit(`0-9세` ~ date, data = training(splits.covid19))

model_fit_tbats <- seasonal_reg() %>%
  set_engine("tbats") %>%
  fit(`0-9세` ~ date, data = training(splits.covid19))


(models_tbl <- modeltime_table(
  model_fit_arima,
  model_fit_ets,
  model_fit_prophet,
  model_fit_lm,
  model_fit_nnetar,
  model_fit_tbats))

(calibration_tbl <- models_tbl %>%
    modeltime_calibrate(new_data = testing(splits.covid19)))

calibration_tbl %>%
  modeltime_forecast(
    new_data = testing(splits.covid19),
    actual_data = covid19
  ) %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_show = FALSE
  ) + labs(title = 'modeltime을 사용한 코로나 확진자수(0-9세)에 대한 6개 모델 예측 결과', x = '연도',
           y = '확진자수')

calibration_tbl %>%
  modeltime_accuracy() %>%
  arrange(rmse)

model_fit_lm <- linear_reg() %>%
  set_engine("lm") %>%
  fit(`0-9세` ~ date,
      data = covid19)

model_fit_ets <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(`0-9세` ~ date, data = covid19)

(models_tbl <- modeltime_table(
  model_fit_lm,
  model_fit_ets))

models_tbl %>%
  modeltime_forecast(
    h = '3 months',
    actual_data = covid19
  ) %>%
  plot_modeltime_forecast(
    .interactive = FALSE
  ) + labs(title = '코로나 확진자수(0-9세) 모델 예측 결과', x = '날짜', y = '확진자수')


library(bsts)


students.ss <- AddLocalLinearTrend(list(), students.xts[, 1])
##students.ss <- AddSeasonal(students.ss, students.xts[, 1], nseasons = 1)
students.bayesian.model <- bsts(students.xts[, 1]~students.xts[, 2]+students.xts[, 3], data = students.xts, 
                                state.specification = students.ss,
                                niter = 1000)

plot(students.bayesian.model)
plot(students.bayesian.model, "components")  # plot(model1, "comp") works too!

students.bayesian.pred <- predict(students.bayesian.model, horizon = 12, newdata = students.xts)
plot(students.bayesian.pred)
plot(students.bayesian.model, burn = 100)
plot(students.bayesian.model, "residuals", burn = 100)
plot(students.bayesian.model, "components", burn = 100)
plot(students.bayesian.model, "forecast.distribution", burn = 100)



employees.ss <- AddLocalLinearTrend(list(), employees.xts[, 1])
employees.ss <- AddSeasonal(employees.ss, employees.xts[, 1], nseasons = 12)
employees.ss <- AddSeasonal(employees.ss, employees.xts[, 1], nseasons = 4)

employees.bayesian.model <- bsts(employees.xts[, 1],
                                 state.specification = employees.ss,
                                 niter = 1000)

plot(employees.bayesian.model)
plot(employees.bayesian.model, "components")  # plot(model1, "comp") works too!

employees.bayesian.pred <- predict(employees.bayesian.model, horizon = 12)
plot(employees.bayesian.pred)
plot(employees.bayesian.model, burn = 100)
plot(employees.bayesian.model, "residuals", burn = 100)
plot(employees.bayesian.model, "components", burn = 100)
plot(employees.bayesian.model, "forecast.distribution", burn = 100)


covid19.ss <- AddLocalLinearTrend(list(), covid19.xts[, 1])
covid19.ss <- AddSeasonal(covid19.ss, covid19.xts[, 1], nseasons = 52, season.duration = 7)
covid19.bayesian.model <- bsts(covid19.xts[, 1],
                               state.specification = covid19.ss,
                               niter = 100)
plot(covid19.xts[, 1])

plot(covid19.bayesian.model)
plot(covid19.bayesian.model, "components")  # plot(model1, "comp") works too!

covid19.bayesian.pred <- predict(covid19.bayesian.model, horizon = 30)
plot(pred1)
