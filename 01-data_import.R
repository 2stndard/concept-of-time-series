library(tidyverse)

(date <- as.Date(c('2021-01-31', '2021-02-28', '2021-03-31')))
(date <- as.Date(c('21/01/31', '21/02/28', '21/03/31'), format = '%y/%m/%d'))
unclass(date)


as.POSIXct('2021-01-31 12:34:56')
unclass(as.POSIXct('2021-01-31 12:34:55'))


as.POSIXlt('2021-01-31 12:34:56')
unclass(as.POSIXlt('2021-12-31 12:34:56'))
as.POSIXlt('2021-12-31 12:34:56')$year  


if(!require(zoo)) {
  install.packages('zoo')
  library(zoo)
}
as.yearmon("2007-02")  
unclass(as.yearmon("2007-02"))
as.yearmon("2007-02-01")
as.yearqtr("2007-01")  
unclass(as.yearqtr("2007-04"))  


as.Date('01/12/2010', format = '%d/%m/%Y') 
Sys.setlocale("LC_ALL", "English")   
as.Date('01jan21', format = '%d%b%y')
Sys.setlocale("LC_ALL", "Korean")   
as.Date('011월21', format = '%d%b%y')


ts(1:10, frequency = 4, start = c(1959, 2))



if(!require(xts)) {
  install.packages('xts')
  library(xts)
}


set.seed(345) 
xts(rnorm(5), as.Date("2008-08-01") + 0:4) 



ts <- ts(1:10, frequency = 4, start = c(1959, 2))
xts(ts)
as.xts(ts)



head(ts) 
# 시계열 형태로 보임
head(as.xts(ts))  


if(!require(tsibble)) {
  install.packages('tsibble')
  library(tsibble)
}



library(dplyr) 
set.seed(345)
x <- data.frame(date = as.Date('2008-01-01') + 0:9, id = 1:10, x1 = rnorm(10), x2= rep('a', 10))
as_tsibble(x, key = id, index = date)



as_tsibble(x, index = date)


library(readxl)
library(tsibble)
library(xts)
students.all <- read_excel("./students.xlsx", skip = 16, na = '-', sheet = 1, col_types = c('text', 'text', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric'))
students <- students.all %>% 
  filter(지역규모 == '계') %>% select(-지역규모)
head(students)  # 데이터 확인



students$연도 <- as.Date(paste0(students$연도, '-01-01'))



students.ts <- ts(students, frequency = 1, start = 1999)
students.xts <- as.xts(students[,-1], order.by = students$연도)
students.tsibble <- students %>% 
  mutate(연도 = yearmonth(paste0(students$연도, '-01-01')))
students.tsibble <- as_tsibble(students.tsibble, index = 연도)



employees <- read.csv('./산업별_취업자_20210206234505.csv', header = TRUE, na = '-', strip.white = TRUE, stringsAsFactors = TRUE)
colnames(employees) <- c('time', 'total', 'employees.edu')



employees$time <- as.Date(paste0(employees$time, '. 01'), format = '%Y. %m. %d')


employees.ts <- ts(employees, start = c(2013, 01), frequency = 12)
employees.xts <- xts(employees[,2:3], order.by = employees[,1])
employees.tsibble <- as_tsibble(employees, index = time)



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
