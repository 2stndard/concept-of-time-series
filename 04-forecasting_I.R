set.seed(345)
정상성시계열 <- arima.sim(model = list(order = c(0,0,0)), n = 200)
ts.plot(정상성시계열)

set.seed(345)
비정상성시계열 <- arima.sim(model = list(order = c(0, 1, 0)), n = 200)
ts.plot(비정상성시계열)


library(forecast)
plot.ts(lynx)


acf(students$학생수계)


students %>%
  select(학생수계) %>%
  forecast::Acf()
#   forecast 패키지의 Acf 수치
students %>%
  select(학생수계) %>%
  forecast::Acf(plot = FALSE)
#   forecast 패키지의 ggAcf plot
students %>%
  select(학생수계) %>%
  forecast::ggAcf()
#   forecast 패키지의 ggAcf 수치
students %>%
  select(학생수계) %>%
  forecast::ggAcf(plot = FALSE)


students %>%
  select(연도, 학생수계) %>%
  timetk::plot_acf_diagnostics(.date_var = 연도, .value = 학생수계, .lag = 14, .show_white_noise_bars = TRUE)


students %>%
  select(학생수계) %>%
  stats::pacf()
#   stats 패키지의 pacf 수치
students %>%
  select(학생수계) %>%
  stats::pacf(plot = FALSE)


students %>%
  select(학생수계) %>%
  forecast::Pacf()
#   forecast 패키지의 Pacf 수치
students %>%
  select(학생수계) %>%
  forecast::Pacf(plot = FALSE)
#   forecast 패키지의 ggPacf plot
students %>%
  select(학생수계) %>%
  forecast::ggPacf()
#   forecast 패키지의 ggPacf 수치
students %>%
  select(학생수계) %>%
  forecast::ggPacf(plot = FALSE)


library(ggplot2)
# 학생수계 열의 선형회귀모형을 생성
student.ts.lm <- tslm(students.ts[,2] ~ trend, data = students.ts)    # 선형회귀모형에 의한 잔차를 res에 저장
res <- as.vector(residuals(student.ts.lm))
# 선형회귀모형에 의한 적합값을 fit에 저장
fit <- as.vector(fitted(student.ts.lm))
ggplot(students, aes(x = 연도, y = 학생수계)) + 
  geom_point() + 
  geom_line(aes(y = fit, group = 1), color = 'blue', size = 1) +
  geom_segment(aes(xend = 연도, yend = fit), color = 'red') + 
  scale_color_continuous(low = "black", high = "red") +
  scale_y_continuous(labels = scales::number_format(big.mark = ','))


set.seed(345)
백색잡음 <- arima.sim(model = list(order = c(0,0,0)), n = 200)
ts.plot(백색잡음)
ggAcf(백색잡음)


library(forecast)
data(goog200, package = 'fpp2')
checkresiduals(naive(goog200))


library(fpp2)
par(mfrow = c(1, 2))
data(ausbeer)
plot.ts(head(ausbeer, 64), main = '시계열 분해 : 덧셈방법 예', xlab = '', ylab = '')
data("AirPassengers")
plot(AirPassengers, main = '시계열 분해 : 곱셈방법 예', xlab = '', ylab = '')
par(mfrow = c(1, 1))


employees.ts[,2] %>%
  stl(s.window = 'periodic') %>% autoplot()


par(mfrow = c(1, 2))
plot(AirPassengers, main = '시계열 데이터', xlab = '', ylab = 'data')
plot(log(AirPassengers), main = 'log(시계열 데이터)', xlab = '', ylab = 'log(data)')
par(mfrow = c(1, 1))


library(forecast)
employees.ts[,2] %>% decompose() %>% seasadj() %>% autoplot()


ggseasonplot(employees.ts[,2], main = '연도별 월간 plot', ylab = '취업자수', xlab = '월', year.labels = T)
ggsubseriesplot(employees.ts[,2], main = '월별 연간 plot', ylab = '취업자수', xlab = '월')


