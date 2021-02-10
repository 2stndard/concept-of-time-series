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
