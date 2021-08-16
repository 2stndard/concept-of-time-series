######################################
###  5장시계열 forecasting Part I - 기초 개념

###  5.2 지연과 차분

students %>%
  mutate(lag1 = lag(학생수계, 1),
         lag3 = lag(학생수계, 3)) %>%
  select(연도, 학생수계, lag1, lag3) %>%
  head(10)

library(timetk)

students %>%
  mutate(lag1 = lag_vec(학생수계, lag = 1),
         lag3 = lag_vec(학생수계, lag = 3)) %>%
  select(연도, 학생수계, lag1, lag3) %>%
  head(10)

stats::lag(students.xts$학생수계, 1) %>% head(10)

students %>%
  mutate(lag1 = lag(학생수계, 1),
         lag3 = lag(학생수계, 3),
         diff1 = c(NA, diff(학생수계, lag = 1)),
         diff3 = c(NA, NA, NA, diff(학생수계, lag = 3))) %>%
  select(연도, 학생수계, lag1, diff1, lag3, diff3) %>%
  head(10)

students %>%
  mutate(diff1 = diff_vec(학생수계, lag = 1),
         diff3 = diff_vec(학생수계, lag = 3)) %>%
  select(연도, 학생수계, diff1, diff3) %>%
  head(10)

diff(students.xts$학생수계, 1) %>% head(10)

###  5.3 ACF와 PACF

### stats 패키지의 acf plot 생성
acf(students$학생수계)

# stats 패키지의 acf 수치 산출
acf(students$학생수계, plot = FALSE)

# forecast 패키지의 Acf plot
students %>%
  select(학생수계) %>%
  forecast::Acf()

# forecast 패키지의 Acf 수치
students %>%
  select(학생수계) %>%
  forecast::Acf(plot = FALSE)

# forecast 패키지의 ggAcf 플롯
students %>%
  select(학생수계) %>%
  forecast::ggAcf()

# forecast 패키지의 ggAcf 수치
students %>%
  select(학생수계) %>%
  forecast::ggAcf(plot = FALSE)

### timetk 패키지의 plot_acf_diagnostics plot
students %>%
  select(연도, 학생수계) %>%
  timetk::plot_acf_diagnostics(.date_var = 연도, .value = 학생수계, .lag = 14, .show_white_noise_bars = TRUE)

# stats 패키지의 pacf plot
students %>%
  select(학생수계) %>%
  stats::pacf()

# stats 패키지의 pacf 수치
students %>%
  select(학생수계) %>%
  stats::pacf(plot = FALSE)

# forecast 패키지의 Pacf plot
students %>%
  select(학생수계) %>%
  forecast::Pacf()

# forecast 패키지의 Pacf 수치
students %>%
  select(학생수계) %>%
  forecast::Pacf(plot = FALSE)

# forecast 패키지의 ggPacf plot
students %>%
  select(학생수계) %>%
  forecast::ggPacf()

# forecast 패키지의 ggPacf 수치
students %>%
  select(학생수계) %>%
  forecast::ggPacf(plot = FALSE)


###  5.4 적합값과 잔차

# 전체 학생수계의 선형 회귀 모델 생성(다음 장에서 설명)
student.ts.lm <- forecast::tslm(students.ts[,2] ~ trend, data = students.ts)
# 전체 학생수 선형 회귀 모델의 적합값 산출
fitted(student.ts.lm)
# 전체 학생수 선형 회귀 모델의 잔차 산출
residuals(student.ts.lm)

###  5.5 백색잡음

library(forecast)
data(goog200, package = 'fpp2')
checkresiduals(naive(goog200))


###  5.6 시계열 분해

# 학생수계는 연별 데이터이기 때문에 계절성을 추출할 수 없음 -> 에러나는게 정상입니다.
students.ts[, 2] %>%
  decompose() %>% autoplot()

# 취업자수를 stl()을 사용하여 분해
employees.ts[,2] %>%
  stl(s.window = 'periodic') %>% autoplot()


###  5.7 정상성 테스트

library(urca)
employees.ts[,2] %>% ur.kpss() %>% summary()

# nsdiffs()로 몇 회의 차분이 필요한지 검사 - 1회의 차분이 필요함
forecast::nsdiffs(employees.ts[,2], alpha = 0.05)

# 1회 차분한 결과에 대한 KPSS 테스트 시행
diff(employees.ts[,2]) %>% ur.kpss() %>% summary()


###  5.8 계절성 검정
library(seastests)
# 총학생수계는 연별 데이터이므로 계절성이 존재할 수 없다.
summary(wo(students.ts[,2]))

# 총취업자수는 계절성이 존재하는지 검사
summary(wo(employees.ts[,2]))        

forecast::nsdiffs(employees.ts[,2]) ### seasonality를 제거하기 위해 필요한 차분수

# 교육서비스업 취업자수의 계절성 검사
summary(wo(employees.ts[,3]))

library(forecast)
employees.ts[,2] %>% decompose() %>% seasadj() %>% autoplot()

ggseasonplot(employees.ts[,2], main = '연도별 월간 플롯', ylab = '취업자수', xlab = '월',
             year.labels = T)

ggsubseriesplot(employees.ts[,2], main = '월별 연간 플롯', ylab = '취업자수', xlab = '월')
