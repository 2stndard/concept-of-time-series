covid19 <- read.csv('./covid19.csv', header = TRUE, na = '-', strip.white = TRUE, stringsAsFactors = TRUE)
colnames(covid19) <- c('category', 'status', 'date', 'value')
covid19 <- covid19[, c(3, 1, 2, 4)]
covid19$date <- as.Date(covid19$date, "%Y. %m. %d")
covid19.by.age <- covid19 %>% 
  filter(grepl('세', category)) %>% 
  filter(category != '세종')

covid19.by.age %>% distinct(category)
wide.covid19.by.age <- spread(covid19.by.age, category, value)

xts.wide.covid19.by.age <- as.xts(wide.covid19.by.age[, -c(1, 2)], order.by = wide.covid19.by.age[, 1])

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
