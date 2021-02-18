covid19 <- read.csv('./covid19.csv', header = TRUE, na = '-', strip.white = TRUE, stringsAsFactors = TRUE)
colnames(covid19) <- c('category', 'status', 'date', 'value')
covid19 <- covid19[, c(3, 1, 2, 4)]
covid19$date <- as.Date(covid19$date, "%Y. %m. %d")
covid19.by.age <- covid19 %>% 
  filter(grepl('세', category)) %>% 
  filter(category != '세종')
covid19.by.age$value <- ifelse(is.na(covid19.by.age$value), 0, covid19.by.age$value)
wide.covid19.by.age <- tidyr::spread(covid19.by.age, category, value)

wide.covid19.by.age.ts = ts(wide.covid19.by.age[, 2:10], frequency = 365)
wide.covid19.by.age.xts <- as.xts(wide.covid19.by.age[, 3:10], order.by = wide.covid19.by.age$date)




glimpse(wide.covid19.by.age)
dayOfYear = as.numeric(format(wide.covid19.by.age[1,1], "%j"))
wide.covid19.by.age.ts = ts(wide.covid19.by.age[, 2:10], start = c(2020, dayOfYear), frequency = 365)

plot(wide.covid19.by.age.ts[,2], ylab = "Price", xaxt = "n")
tsp = attributes(wide.covid19.by.age.ts)$tsp
axis(1, at = seq(tsp[1], tsp[2], along = wide.covid19.by.age.ts), 
     labels = format(wide.covid19.by.age$date, "%Y-%m-%d"))




xts.wide.covid19.by.age <- as.xts(wide.covid19.by.age[, -c(1)], order.by = wide.covid19.by.age[, 1])

covid19.by.age %>% filter(category %in% c('0-9세', '10-19세')) %>% 
  mutate(cumsum = cumsum(ifelse(is.na(value), 0, value)))

         

plot_time_series(.data = covid19.by.age %>%  
                   mutate(cumsum = cumsum(ifelse(is.na(value), 0, value))), 
                 .date_var = date,
                 .value = cumsum, 
                 .color_var = category, 
                 .smooth = FALSE, 
                 .interactive = TRUE)




covid19.by.age$value1 <- ifelse(is.na(covid19.by.age$value), 0, covid19.by.age$value)
### 트레이닝 셋과 테스트 셋을 나눈다
splits <- initial_time_split(covid19.by.age %>% filter(category == '0-9세'), prop = 0.9)
training(splits)
testing(splits)
###  trend와 season을 반영하여 linear model을 생성
model_fit_lm <- linear_reg() %>%
  set_engine("lm") %>%
  fit(value ~ as.numeric(date),
      data = training(splits))
###  모델 테이블 생성
model_tbl <- modeltime_table(model_fit_lm)
###  테스팅 셋으로 모델 교정
calibration_tbl <- model_tbl %>% modeltime_calibrate(new_data = testing(splits))
###  3년 예측치 생성후 plotting
calibration_tbl %>%
  modeltime_forecast(
#    .newdata = testing(splits),
    h = '100 days',
    actual_data = covid19.by.age %>% filter(category == '0-9세')
    ) %>%
  plot_modeltime_forecast(
    .interactive      = TRUE
  )

glimpse(under10.covid19.by.age)
under10.covid19.by.age <- covid19.by.age %>% filter(category == '0-9세')
###  trend와 season을 반영하여 linear model을 생성
model_fit_lm <- linear_reg() %>%
  set_engine("lm") %>%
  fit(value ~ as.numeric(date),
      data = under10.covid19.by.age)
###  모델 테이블 생성
model_tbl <- modeltime_table(model_fit_lm)
###  테스팅 셋으로 모델 교정
calibration_tbl <- model_tbl %>% modeltime_calibrate(new_data = under10.covid19.by.age)
###  3년 예측치 생성후 plotting
calibration_tbl %>%
  modeltime_forecast(
    #    .newdata = testing(splits),
    h = '100 days',
    actual_data = under10.covid19.by.age
  ) %>%
  plot_modeltime_forecast(
    .interactive      = TRUE
  )

###  trend와 season을 반영하여 linear model을 생성
model_fit_lm <- linear_reg() %>%
  set_engine("lm") %>%
  fit(value ~ as.numeric(date),
      data = under10.covid19.by.age)
###  모델 테이블 생성
model_tbl <- modeltime_table(model_fit_lm)
###  테스팅 셋으로 모델 교정
#calibration_tbl <- model_tbl %>% modeltime_calibrate(new_data = under10.covid19.by.age)
###  3년 예측치 생성후 plotting
model_tbl %>%
  modeltime_forecast(
    #    .newdata = testing(splits),
    h = '100 days',
    actual_data = under10.covid19.by.age
  ) %>%
  plot_modeltime_forecast(
    .interactive      = TRUE
  )




covid.ts <- covid19.by.age %>% filter(category == '0-9세') %>% select(5) %>%
  as.ts()

tslm(value1 ~ trend, data = covid.ts)

install.packages('seastests')
library(seastests)
summary(wo(covid.ts))



rate = structure(list(Date = c("2012-11-01", "2012-11-02", "2012-11-10", 
                               "2012-11-11", "2012-11-12", "2012-11-13"), 
                      Price = c(6.2411, 6.2415, 6.2454, 6.2456, 6.2437, 6.2429)), 
                 .Names = c("Date", "Price"), class = "data.frame", row.names = c(NA, -6L))
rate$Date = as.Date(rate$Date, format = "%Y-%m-%d")

dayOfYear = as.numeric(format(rate[1,1], "%j"))
rate_ts = ts(rate$Price, start = c(2012, dayOfYear), frequency = 365)
time(rate_ts)
plot(rate_ts, ylab = "Price", xaxt = "n")
tsp = attributes(rate_ts)$tsp
axis(1, at = seq(tsp[1], tsp[2], along = rate_ts), 
     labels = format(rate$Date, "%Y-%m-%d"))


plot.xts(wide.covid19.by.age.xts[, 1], main = '일별 확진자수(0-9세)', xlab = '날짜',  ylab = '확진자수')

autoplot(wide.covid19.by.age.ts[,2], main = '일별 확진자수(0-9세)', xlab = '날짜', ylab = '확진자수', series = '확진자', lty = 1, lwd = 1)

wide.covid19.by.age %>%
  plot_time_series(.date_var = date, .value = `0-9세`, .smooth = F, .title = '일별 확진자수(0-9세)', .x_lab = '시간', .y_lab = '확진자수')

wide.covid19.by.age.tsibble <- as_tsibble(wide.covid19.by.age, index = date)
class(wide.covid19.by.age.tsibble)


auto.arima(wide.covid19.by.age.ts[,2]) ### 교육서비스업 취업자수의 ARIMA모형은 ARIMA(0, 1, 0)으로 선정됨
kpss.test(wide.covid19.by.age.ts[,2])  ### kpss 테스트를 통해 생성된 데이터가 정상성인지 테스트 - 0.05보다 작으므로 비정상성, 차분 필요
ndiffs(wide.covid19.by.age.ts[,2], test = 'kpss')   ### 비정상성을 제거하기 위해 필요한 차분수가 1
diff(wide.covid19.by.age.ts[,2]) %>% ggtsdisplay()  ### 1차 차분을 해본 결과 ACF, PACF 모두 절단(Cut off)이므로 ARMA(0,0)
kpss.test(diff(wide.covid19.by.age.ts[,2]))  ### kpss 테스트를 통해 생성된 데이터가 정상성인지 테스트 - 0.05보다 작으므로 비정상성, 차분 필요
kpss.test(diff(diff(wide.covid19.by.age.ts[,2])))
diff(diff(wide.covid19.by.age.ts[,2])) %>% ggtsdisplay()  ### 1차 차분을 해본 결과 ACF, PACF 모두 절단(Cut off)이므로 ARMA(0,0)
arima(wide.covid19.by.age.ts[,2], )

wide.covid19.by.age.ts[,2] %>% tbats() %>% forecast(h = 100) %>% autoplot()

wide.covid19.by.age.ts[,2] %>% nnetar() %>% forecast(h = 100, PI = T) %>% autoplot()



wide.covid19.by.age %>% 
  group_by(year(date), month(date)) %>%
  mutate(sum.by.month = sum(`0-9세`)) %>%
  ungroup() %>%
  mutate(rate.by.month = round(`0-9세`/sum.by.month, 3) * 100) %>%
  select(date, `0-9세`, sum.by.month, rate.by.month) %>%
  head(30)




covid.prophet <- data.frame(ds = wide.covid19.by.age$date, y = wide.covid19.by.age$`0-9세`)
model.prophet.covid <- prophet(covid.prophet, yearly.seasonality=TRUE, daily.seasonality=TRUE, weekly.seasonality=TRUE)
future.covid <- make_future_dataframe(model.prophet.covid, periods = 100, freq = 'day')
tail(future.covid, 10)
forecast.covid <- predict(model.prophet.covid, future.covid)
plot(model.prophet.covid, forecast.covid) +
  labs(title = '일별 코로나 확진자수 추세(0-9세, prophet model)', x = '연월', y = '확진자수') + 
  scale_y_continuous(labels = scales::number_format(big.mark = ','))
prophet_plot_components(model.prophet.covid, forecast.covid)
?prophet


fill.covid19.by.age.tsibble <- fill_gaps(wide.covid19.by.age.tsibble, `0-9세` = 0)

model.covid19.by.age.tsibble <- fill.covid19.by.age.tsibble %>% model(ets = ETS(`0-9세`), 
                                                     arima = ARIMA(`0-9세`), 
                                                     naive = NAIVE(`0-9세`), 
                                                     tslm = TSLM(`0-9세`),
                                                     rw = RW(`0-9세`),
                                                     mean = MEAN(`0-9세`),
                                                     nnetar = NNETAR(`0-9세`),
                                                     prophet = prophet(`0-9세`)
)

forecast.covid19.by.age.tsibble <- model.covid19.by.age.tsibble %>% forecast(h = 12)

forecast.covid19.by.age.tsibble %>% autoplot(fill.covid19.by.age.tsibble, level = NULL)

model.covid19.by.age.tsibble %>% accuracy %>% arrange(RMSE)

best.model.covid19.by.age.tsibble <- model.covid19.by.age.tsibble %>% select(nnetar, prophet)

model.covid19.by.age.tsibble %>% 
  forecast(h = 12) %>% 
  autoplot(fill.covid19.by.age.tsibble, level = NULL, lwd = 1) + 
  autolayer(fitted(best.model.covid19.by.age.tsibble), lwd = 1) + 
  geom_point(aes(x = date, y = `0-9세`)) + 
  labs(title = '코로나 확진자수 예측', x = '년월일', y = '확진자수')
