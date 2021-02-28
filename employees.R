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

employees.ts[, 2]

summary(auto.arima(employees.ts[, 2]))  ### 학생수의 ARIMA모형은 ARIMA(1, 2, 0)으로 선정됨
employees.ts[, 2] %>% ggtsdisplay()


employees.ts.seasadj <- employees.ts[, 2] %>% stl(s.window='periodic') %>% seasadj()
employees.ts.seasadj %>% ggtsdisplay()
diff(employees.ts.seasadj) %>% ggtsdisplay()

employees.ts[, 2] %>% diff(lag =12) %>% ggtsdisplay()
employees.ts[, 2] %>% diff(lag =12) %>% kpss.test()  ###비정상성
employees.ts[, 2] %>% diff(lag =12) %>% ndiffs()
employees.ts[, 2] %>% diff(lag =12) %>% diff() %>% kpss.test()  ### 정상성
employees.ts[, 2] %>% diff(lag =12) %>% diff() %>% ggtsdisplay()
auto.arima(employees.ts.seasadj)  ### 학생수의 ARIMA모형은 ARIMA(1, 2, 0)으로 선정됨

employees.ts.seasadj %>% ggtsdisplay()
diff(employees.ts.seasadj) %>% ggtsdisplay()

autoplot(forecast(arima(employees.ts.seasadj, order = c(1, 1, 0))))
autoplot(forecast((arima(employees.ts.seasadj, order = c(0, 1, 2)))))

par(mfrow = c(1, 2))
Acf(employees.ts.seasadj)  ### ACF가 tail off이고
Pacf(employees.ts.seasadj)  ### PACF가 1에서 cut off 이므로 ARMA(1,0)모델
par(mfrow = c(1, 1))
kpss.test(students.total.ts[,1])  ### kpss 테스트를 통해 생성된 데이터가 정상성인지 테스트 - 0.05보다 작으므로 정상성, 차분 필요
ndiffs(students.total.ts[,1], test = 'kpss')   ### 비정상성을 제거하기 위해 필요한 차분수가 2이므로 최종 모델은 ARIMA(1, 2, 0)
auto.arima(students.total.ts[,1]) %>% forecast() %>% autoplot()

employees.ts[, 2] %>% tbats() %>% forecast() %>% autoplot()

employees.ts[, 2] %>% nnetar() %>% forecast(PI = T) %>% autoplot()

sim <- simulate(employees.ts[, 2] %>% nnetar(), nsim=30L)
autoplot(sim)

sim <- ts(matrix(0, nrow=30L, ncol=9L), 
          start=end(employees.ts[, 2])+0:35, frequency = 12) 
for(i in seq(9)) {
  sim[,i] <- simulate(employees.ts[, 2] %>% nnetar(), nsim=30L)
}
autoplot(employees.ts[, 2]) + autolayer(sim)


employees %>% 
  mutate(year = year(time)) %>%
  group_by(year) %>%
  mutate(total.year = sum(total), 
            employees.edu = sum(employees.edu)) %>%
  ungroup()


employees %>% 
  mutate(year = year(time)) %>%
  group_by(year) %>%
  summarise(total.year = sum(total), 
         employees.edu = sum(employees.edu))


do.call(rbind, lapply(split(employees.xts, f = 'year'), cumsum)) %>%
  head(10)


cbind(employees %>% select(time, total),
      employees %>% select(total) %>% lag(1) %>% rename(전월 = total)) %>% 
  mutate(증감 = total - 전월, 증감율 = round((total/전월)-1, 4) * 100)


employees$전월 <- difference(employees.tsibble$total, lag = 1)
employees <- employees[, -4]
t <- students.tsibble[students.tsibble$지역규모 == '계', ]
employees%>%
  mutate(증감 = difference(t)) %>%
  mutate(증감율 = round((증감/total), 3) * 100) %>% select(1, 2, 4, 5) %>% head(10)
  
  
employees.xts$증감 <- diff(employees.xts$total)
employees.xts$증감율 <- round((employees.xts$증감/employees.xts$total), 3) * 100
employees.xts[, c('total', '증감', '증감율')]
  


employees %>% 
  group_by(year(time)) %>%
  mutate(sum.by.year = sum(total)) %>%
  ungroup() %>%
  mutate(rate.by.year = round(total/sum.by.year, 3) * 100) %>%
  head(15)

employees.tsibble%>%
  index_by(yearqtr = ~ yearquarter(.)) %>%
  mutate(sum.qtrly = sum(total)) %>% 
  mutate(rate.qtrly = (total/sum.qtrly)*100) %>%
head(15)  

employees.tsibble%>%
  index_by(yearqtr = ~ year(.)) %>%
  mutate(sum.qtrly = sum(total)) %>% 
  mutate(rate.qtrly = (total/sum.qtrly)*100) %>%
  head(15)  


do.call(rbind, lapply(split(employees.xts, f = 'year'), sum))

to.period(employees.xts, method = 'year')


employees %>% 
  group_by(year(time)) %>%
  mutate(cumsum.total = cumsum(total), 
         cumsum.edu = cumsum(employees.edu)) %>%
  head(15)


employees.prophet <- data.frame(ds = employees[,1], y = employees[,2])
model.prophet.employees <- prophet(employees.prophet, weekly.seasonality=TRUE, daily.seasonality=TRUE)
future.employees <- make_future_dataframe(model.prophet.employees, periods = 10, freq = 'month')
forecast.employees <- predict(model.prophet.employees, future.employees)
plot(model.prophet.employees, forecast.employees) +
  labs(title = '월별 전체 취업자수 추세(prophet model)', x = '연월', y = '취업자수') + 
  scale_y_continuous(labels = scales::number_format(big.mark = ','))
prophet_plot_components(model.prophet.employees, forecast.employees)



employees.tsibble %>% model(ets = ETS(total)) %>% forecast(h = '2 year') %>% autoplot()
model


library(fpp2)
library(tsibble)
library(fable)
data("auscafe")
cafe <- as_tsibble(auscafe)

autoplot(cafe)
cafe %>% ETS(value) %>% summary


update.packages('fable')


employees.tsibble <- as_tsibble(employees, index = yearmonth(time))
employees$yearmonth <- yearmonth(employees$time)
employees.tsibble <- as_tsibble(employees, index = yearmonth)
employees.tsibble %>% model(ets = ETS(total)) %>% glance
employees.tsibble %>% model(ets = ETS(total)) %>% tidy
employees.tsibble %>% model(ets = ETS(total)) %>% generate(bootstrap = TRUE) %>% autoplot
employees.tsibble %>% model(ets = ETS(total)) %>% forecast %>% autoplot
employees.tsibble %>% model(ets = ETS(total)) %>% components()

employees.tsibble %>% model(ets_A = ETS(log(total) ~ season("M")), 
                            ets_M = ETS(log(total) ~ season("M")), 
                            ets = ETS(total)
                            ) %>% forecast %>% autoplot(level = NULL, data = employees.tsibble)


model.fable.employees <- employees.tsibble %>% model(ets = ETS(total), 
                                                    arima = ARIMA(total), 
                                                    naive = NAIVE(total), 
                                                    tslm = TSLM(total),
                                                    rw = RW(total),
                                                    mean = MEAN(total),
                                                    nnetar = NNETAR(total),
                                                    prophet = prophet(total)
                                                    )

forecast.fable.employees <- model.fable.employees %>% forecast(h = 12)

forecast.fable.employees %>% autoplot(employees.tsibble, level = NULL)

model.fable.employees %>% accuracy %>% arrange(RMSE)

best.model.fable.employees <- model.fable.employees %>% select(ets, prophet)

best.model.fable.employees %>% 
  forecast(h = 12) %>% 
  autoplot(employees.tsibble, level = NULL, lwd = 1) + 
  autolayer(fitted(best.model.fable.employees), lwd = 1) + 
  geom_point(aes(x = yearmonth, y = total)) + 
  labs(title = '전체 취업자수 예측', x = '년월', y = '취업자수')


employees %>%
  ggplot(aes(x = time, y = total)) + 
  geom_line(aes(group = 1)) +
  labs(x = '연도', y = '신규 취업자수') + 
  scale_y_continuous(labels = scales::comma_format()) +
  scale_x_date(breaks = '1 year') + 
  ggthemes::theme_economist()

?scale_x_date




hw_grid_shallow <- ts_grid(ts.obj = employees.ts[,2],
                           periods = 6,
                           model = "HoltWinters",
                           optim = "MAPE",
                           window_space = 6,
                           window_test = 12,
                           hyper_params = list(alpha = seq(0.01, 1,0.1),
                                               beta =  seq(0.01, 1,0.1),
                                               gamma = seq(0.01, 1,0.1)),
                           parallel = TRUE,
                           n.cores = 8)

a_min <- min(hw_grid_shallow$grid_df$alpha[1:20])
a_max <- max(hw_grid_shallow$grid_df$alpha[1:20])

b_min <- min(hw_grid_shallow$grid_df$beta[1:20])
b_max <- max(hw_grid_shallow$grid_df$beta[1:20])

g_min <- min(hw_grid_shallow$grid_df$gamma[1:20])
g_max <- max(hw_grid_shallow$grid_df$gamma[1:20])

hw_grid_second <- ts_grid(ts.obj = employees.ts[,2],
                          periods = 6,
                          model = "HoltWinters",
                          optim = "MAPE",
                          window_space = 6,
                          window_test = 12,
                          hyper_params = list(alpha = seq(a_min, a_max,0.05),
                                              beta =  seq(b_min, b_max,0.05),
                                              gamma = seq(g_min, g_max,0.05)),
                          parallel = TRUE,
                          n.cores = 8)

md <- HoltWinters(employees.ts[,2], 
                  alpha = hw_grid_second$alpha,
                  beta = hw_grid_second$beta,
                  gamma = hw_grid_second$gamma)



employees %>% mutate(year = lubridate::year(time), 
                     qtr = lubridate::quarter(time)) %>%
  group_by(year, qtr) %>%
  summarise(sum = sum(total)) %>% ts(frequency = 4, start = c(2013,1))
qtr.employees.ts <- ts(qtr.employees, frequency = 4, start = c(2013,1))
auto.arima(qtr.employees.ts[,3])
qtr.employees.ts[,3] %>% tsdisplay()
qtr.employees.ts[,3] %>% ndiffs()
qtr.employees.ts[,3] %>% diff() %>% tsdisplay()
