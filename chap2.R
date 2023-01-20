######################################
###  2장 시계열 데이터 객체
###  2.1 날짜/시간 데이터 클래스
###  2.1.1 date 클래스
(date <- as.Date(c('2021-01-31', '2021-02-28', '2021-03-31')))

(date <- as.Date(c('21/01/31', '21/02/28', '21/03/31'), format = '%y/%m/%d'))

unclass(date)

###  2.1.2 POSIXct, POSIXlt 클래스

# character를 POSIXct class로 변환
as.POSIXct('2021-01-31 12:34:56')

# POSIXct를 해제하면 정수
unclass(as.POSIXct('2021-01-31 12:34:56'))

# character를 POSIXlt class로 변환
as.POSIXlt('2021-01-31 12:34:56')

# POSIXlt에서 1900년 이후 연도를 추출
unclass(as.POSIXlt('2021-12-31 12:34:56'))

# POSIXlt에서 1900년 이후 연도를 추출
as.POSIXlt('2021-12-31 12:34:56')$year

###  2.1.3 yearmon, yearqtr 클래스

library(zoo)

# character를 yearmon class로 변환
as.yearmon("2007-02")

# yearmon class를 해제하면 double
unclass(as.yearmon("2007-02"))

# 날짜가 있어도 yearmon은 연, 월까지만 인식
as.yearmon("2007-02-01")
?as.yearmon

# character를 yearqtr class로 변환(1분기)
as.yearqtr("2007-01")

# yearqtr class를 해제하면 double
unclass(as.yearqtr("2007-04"))

###  2.1.4 날짜, 시간 포맷

as.Date('01/12/2010', format = '%d/%m/%Y')

Sys.setlocale("LC_ALL", "English")

as.Date('01jan21', format = '%d%b%y')

Sys.setlocale("LC_ALL", "Korean")

as.Date('011월21', format = '%d%b%y')


###  2.2시계열 데이터 객체
###  2.2.1 ts

library(zoo)

ts(1:10, frequency = 4, start = c(1959, 2))

###  2.2.2 xts

library(xts)

set.seed(345)

xts(rnorm(5), as.Date("2008-08-01") + 0:4)

ts <- ts(1:10, frequency = 4, start = c(1959, 2))

xts(ts)  ## 오류가 나는게 정상입니다.

as.xts(ts)

# 시계열 데이터 형태로 보이지 않음
head(ts)

# 시계열 형태로 보임
head(as.xts(ts))

###  2.2.3 tsibble

library(tsibble)

library(dplyr)

set.seed(345)

x <- data.frame(date = as.Date('2008-01-01') + 0:9, id = 1:10, x1 = rnorm(10), x2= rep('a', 10))

as_tsibble(x, key = id, index = date)

as_tsibble(x, index = date)

###  2.3 시계열 데이터 import 
###  2.3.1 엑셀 파일

library(readxl)

students.all <- read_excel("D:/R/Github/concept-of-time-series/students.xlsx", skip = 16, na = '-', sheet = 1, col_types = c('text', 'text', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric','numeric', 'numeric', 'numeric'))

students <- students.all %>%
  filter(지역규모 == '계') %>% select(-지역규모)

head(students)

students$연도 <- as.Date(paste0(students$연도, '-01-01'))

students.ts <- ts(students, frequency = 1, start = 1999)

students.xts <- as.xts(students[,-1], order.by = students$연도)

students.tsibble <- students %>%
  mutate(연도 = yearmonth(paste0(students$연도, '-01-01')))

students.tsibble <- as_tsibble(students.tsibble, index = 연도)


###  2.3.2 CSV 파일

employees <- read.csv('./산업별_취업자_20210206234505.csv', header = TRUE, na = '-', strip.white = TRUE, stringsAsFactors = TRUE)

colnames(employees) <- c('time', 'total', 'employees.edu')

employees$time <- as.Date(paste0(employees$time, '. 01'), format = '%Y. %m. %d')

employees.ts <- ts(employees, start = c(2013, 01), frequency = 12)

employees.xts <- xts(employees[,2:3], order.by = employees[,1])

employees.tsibble <- as_tsibble(employees, index = time)


###  2.3.3 추가 실습 데이터 생성
covid19 <- read.csv('./covid19.csv', header = TRUE, na = '-', strip.white = TRUE, stringsAsFactors = TRUE)

colnames(covid19) <- c('category', 'status', 'date', 'value')

covid19 <- covid19[, c(3, 1, 2, 4)]

covid19$date <- as.Date(covid19$date, "%Y. %m. %d")

covid19 <- covid19 %>%
  filter(grepl('세', category)) %>%
  filter(category != '세종')

covid19$value <- ifelse(is.na(covid19$value), 0, covid19$value)

covid19 <- tidyr::spread(covid19, category, value)

covid19.ts <- ts(covid19[, 2:10], frequency = 365)

covid19.xts <- as.xts(covid19[, 3:10], order.by = covid19$date)

covid19.tsibble <- as_tsibble(covid19, index = date)
