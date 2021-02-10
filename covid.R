covid19 <- read.csv('./covid19.csv', header = TRUE, na = '-', strip.white = TRUE, stringsAsFactors = TRUE)
colnames(covid19) <- c('category', 'status', 'date', 'value')
covid19 <- covid19[, c(3, 1, 4)]
covid19$date <- as.Date(covid19$date, "%Y. %m. %d")

covid19.by.age <- covid19 %>% 
  filter(grepl('세', category)) %>% 
  filter(category != '세종')

covid19.by.age %>% distinct(category)
wide.covid19.by.age <- spread(covid19.by.age, category, value)

dayOfYear = as.numeric(format(wide.covid19.by.age[1,1], "%j"))
wide.covid19.by.age.ts = ts(wide.covid19.by.age[, 2:10], start = c(2020, dayOfYear), frequency = 365)

plot(wide.covid19.by.age.ts[,1], ylab = "Price", xaxt = "n")
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
