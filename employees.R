employees <- read.csv('./산업별_취업자_20210206234505.csv', header = TRUE, na = '-', strip.white = TRUE, stringsAsFactors = TRUE)
colnames(employees) <- c('time', 'total', 'employees.edu')
employees$time <- as.Date(paste0(employees$time, '. 01'), format = '%Y. %m. %d')
employees.ts <- ts(employees, start = c(2013, 01), frequency = 12)
employees.xts <- xts(employees[,2:3], order.by = employees[,1])


as.Date(paste0(employees$time, '. 01'), format = '%Y. %m. %d') 

tslm(employees.ts[,2] ~ trend, data = employees.ts, lambda = 1) %>%
  forecast() %>%
  autoplot()


employees.ts[,2] %>%
  splinef(lambda = 0) %>%
  autoplot()

summary(wo(employees.ts[,3]))

summary(ses(employees.ts[,2]))
nrow(employees.ts)
mean(employees.ts[1:48,2])
i <- 1
for(i in 1:nrow(employees.ts)) {
  print(i)
  print(25251.684 - round(mean(mean(employees.ts[1:i,2])), 3))
}

autoplot(employees.ts[,2]) + 
  autolayer(fitted(holt(employees.ts[,2], beta = 0.1)), PI = FALSE, series = 'beta = 0.1') +
  autolayer(fitted(holt(employees.ts[,2], beta = 0.2)), PI = FALSE, series = 'beta = 0.2') + 
  autolayer(fitted(holt(employees.ts[,2], beta = 0.3)), PI = FALSE, series = 'beta = 0.3') +
  autolayer(fitted(holt(employees.ts[,2], beta = 0.4)), PI = FALSE, series = 'beta = 0.4') +
  autolayer(fitted(holt(employees.ts[,2], beta = 0.5)), PI = FALSE, series = 'beta = 0.5') +
  autolayer(fitted(holt(employees.ts[,2], beta = 0.6)), PI = FALSE, series = 'beta = 0.6') +
  autolayer(fitted(holt(employees.ts[,2], beta = 0.7)), PI = FALSE, series = 'beta = 0.7') +
  autolayer(fitted(holt(employees.ts[,2], beta = 0.8)), PI = FALSE, series = 'beta = 0.8') +
  autolayer(fitted(holt(employees.ts[,2], beta = 0.9)), PI = FALSE, series = 'beta = 0.9')



glimpse(employees)
employees$time <- as.Date(paste0(employees[, 1], '. 01'), format = '%Y. %m. %d')

splits <- initial_time_split(employees, prop = 0.9)
training(splits)
testing(splits)

model_fit_lm <- linear_reg() %>%
  set_engine("lm") %>%
  fit(total ~ as.numeric(time) + factor(lubridate::month(time, label = TRUE), ordered = FALSE),
      data = training(splits))

model_fit_ets <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(total ~ time, data = training(splits))

models_tbl <- modeltime_table(
  model_fit_lm,
  model_fit_ets
)

calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(splits))

calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = employees
  ) %>%
  plot_modeltime_forecast(
    .interactive      = TRUE
  )

models_tbl %>%
  modeltime_forecast(
    #    new_data    = testing(splits),
    h = '5 years',
    actual_data = employees
  ) %>%
  plot_modeltime_forecast(
    .interactive      = TRUE
  )

calibration_tbl %>%
  modeltime_accuracy()


##############################################
splits <- initial_time_split(employees, prop = 0.9)
training(splits)
testing(splits)

model_fit_lm1 <- linear_reg() %>%
  set_engine("lm") %>%
  fit(employees.edu ~ as.numeric(time) + factor(lubridate::month(time, label = TRUE), ordered = FALSE),
      data = training(splits))

model_fit_ets1 <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(employees.edu ~ time, data = training(splits))

models_tbl1 <- modeltime_table(
  model_fit_lm1,
  model_fit_ets1
)
models_tbl1$.model
calibration_tbl1 <- models_tbl1 %>%
  modeltime_calibrate(new_data = testing(splits))

calibration_tbl1 %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = employees
  ) %>%
  plot_modeltime_forecast(
    .interactive      = TRUE
  )
model_tbl$.model
models_tbl1 %>%
  modeltime_forecast(
    #    new_data    = testing(splits),
    h = '5 years',
    actual_data = employees
  ) %>%
  plot_modeltime_forecast(
    .interactive      = TRUE
  )

calibration_tbl1 %>%
  modeltime_accuracy()

?ets
ets(employees.ts[,2]) %>% forecast(h = 100) %>% autoplot()
autoplot(employees.ts[,3]) + 
  autolayer(fitted(ets(employees.ts[,3]))) +
  autolayer(forecast(ets(employees.ts[,3])))

?ets
ets(employees.ts[,3], model = 'MAM', alpha = 0.8759, beta = 0.0001, gamma = 0.0001, phi = 0.98) %>%
  forecast(h = 30) %>% autoplot()


autoplot(employees.ts[,2]) + 
  autolayer(fitted(hw(employees.ts[,2])), series = 'hw 적합값') +
  autolayer(hw(employees.ts[,2], seasonal = 'additive'), PI = FALSE, series = 'additive') + 
  autolayer(hw(employees.ts[,2], seasonal = 'multiplicative'), PI = FALSE, series = 'multiplicative')

summary(hw(employees.ts[,2], seasonal = 'additive'))

summary(hw(employees.ts[,2]))
?dshw
hw <- HoltWinters(employees.ts[,2])
forecast <- predict(hw, n.ahead = 12, prediction.interval = T, level = 0.95) 
plot(hw, forecast)
autoplot(employees.ts[,2]) +
  autolayer(fitted(HoltWinters(employees.ts[,2]))[, 1], series = 'HoltWinter') + 
  autolayer(fitted(hw(employees.ts[,2])), series = 'hw') + 
  autolayer(predict(hw, n.ahead = 12, prediction.interval = F), series = 'HoltWinter Predict') +
  autolayer(hw(employees.ts[,2]), PI = FALSE, series = 'hw Predict') 
  

fitted <- fitted(HoltWinters(employees.ts[,2]))[, 1]

seasonplot(employees.ts[,2])
ggseasonplot(employees.ts[,2])

summary(hw(employees.ts[,2]))

ggplot(data = employees, aes(x = as.factor(time), y = total)) +
  geom_line(aes(group = 1)) + 
  geom_point(shape = 'circle') +
  labs(title = '월별 취업자수', x = '시간') +
  scale_y_continuous(labels = scales::number_format(big.mark = ',')) +
  scale_x_discrete(breaks = c('2013. 01', '2014. 01', '2015. 01', '2016. 01', '2017. 01', '2018. 01', '2019. 01', '2020. 01')) +
  theme(axis.text.x=element_text(angle=90,hjust=1))


plot.xts(employees.xts[,1], main = '연도별 학생수', sub = '전체 학생수의 연도별 변화', xlab = '연도',  ylab = '학생수')

autoplot(employees.ts[,2], main = '월별 취업자수', xlab = '연도', ylab = '취업자수', series = '전체 취업자', lty = 1, lwd = 1)


employees %>%
  plot_time_series(.date_var = time, .value = total, .smooth = F, .title = '월별 취업자수', .x_lab = '시간', .y_lab = '확진자수')


employees$yearmonth <- yearmonth(employees$time)
employees.tsibble <- as_tsibble(employees, index = yearmonth)

summary(employees.tsibble)

yearmonth(employees$time)

interval(employees.tsibble)
employees.tsibble %>%
  autoplot(total)

employees.tsibble %>%
  gg_season(total)


employees <- read.csv('./산업별_취업자_20210206234505.csv', header = TRUE, na = '-', strip.white = TRUE, stringsAsFactors = TRUE)
colnames(employees) <- c('time', 'total', 'employees.edu')
employees$time <- as.Date(paste0(employees$time, '. 01'), format = '%Y. %m. %d')
employees.ts <- ts(employees, start = c(2013, 01), frequency = 12)
employees.xts <- xts(employees[,2:3], order.by = employees[,1])
employees$yearmonth <- yearmonth(employees$time)
employees.tsibble <- as_tsibble(employees, index = yearmonth)

### 트레이닝 셋과 테스트 셋을 나눈다
splits <- initial_time_split(employees, prop = 0.9)
###  trend와 season을 반영하여 linear model을 생성
model_fit_lm <- linear_reg() %>%
  set_engine("lm") %>%
  fit(total ~ as.numeric(time) + factor(lubridate::month(time, label = TRUE), ordered = FALSE),
      data = training(splits))
###  모델 테이블 생성
model_tbl <- modeltime_table(model_fit_lm)
###  테스팅 셋으로 모델 교정
calibration_tbl <- model_tbl %>% modeltime_calibrate(new_data = testing(splits))
###  3년 예측치 생성후 plotting
calibration_tbl %>%
  modeltime_forecast(
    h = '3 years',
    actual_data = employees, 
    conf_interval = 0.95
  ) %>%
  plot_modeltime_forecast(
    .interactive      = TRUE
  )