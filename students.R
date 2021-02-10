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


autoplot(students.total.ts[,3], PI = FALSE, series = '원본', color = 'black') +
  autolayer(fitted(ses(students.total.ts[,3], beta = 0.1)), PI = FALSE, series = 'alpha = 0.1') +  
  autolayer(fitted(ses(students.total.ts[,3], beta = 0.2)), PI = FALSE, series = 'alpha = 0.2') + 
  autolayer(fitted(ses(students.total.ts[,3], beta = 0.3)), PI = FALSE, series = 'alpha = 0.3') +
  autolayer(fitted(ses(students.total.ts[,3], beta = 0.4)), PI = FALSE, series = 'alpha = 0.4') +
  autolayer(fitted(ses(students.total.ts[,3], beta = 0.5)), PI = FALSE, series = 'alpha = 0.5') +
  autolayer(fitted(ses(students.total.ts[,3], beta = 0.6)), PI = FALSE, series = 'alpha = 0.6') +
  autolayer(fitted(ses(students.total.ts[,3], beta = 0.7)), PI = FALSE, series = 'alpha = 0.7') +
  autolayer(fitted(ses(students.total.ts[,3], beta = 0.8)), PI = FALSE, series = 'alpha = 0.8') +
  autolayer(fitted(ses(students.total.ts[,3], beta = 0.9)), PI = FALSE, series = 'alpha = 0.9')



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
