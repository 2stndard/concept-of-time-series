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
### 트레이닝 셋과 테스트 셋을 나눈다
splits <- initial_time_split(students.modeltime, prop = 0.9)
training(splits)
testing(splits)
###  trend와 season을 반영하여 linear model을 생성
model_fit_lm <- linear_reg() %>%
  set_engine("lm") %>%
  fit(students.modeltime[,2] ~ students.modeltime[,1],
      data = students.modeltime)
###  모델 테이블 생성
model_tbl <- modeltime_table(model_fit_lm)
###  테스팅 셋으로 모델 교정
calibration_tbl <- model_tbl %>% modeltime_calibrate(new_data = students.modeltime[, 3])
###  3년 예측치 생성후 plotting
model_tbl %>%
  modeltime_forecast(
    #    .newdata = testing(splits),
    h = 10,
    actual_data = students.modeltime
  ) %>%
  plot_modeltime_forecast(
    .interactive      = TRUE
  )
