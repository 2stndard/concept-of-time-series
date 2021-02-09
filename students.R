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
