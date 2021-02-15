ts <- ts(1:10, frequency = 4, start = c(1959, 2))
head(ts)

xts
head(zoo(rnorm(5), as.Date("2008-08-01") + 0:4), 1)
as.Date("2008-08-01") + 0:2

as.xts(x = rnorm(5), as.Date("2008-08-01") + 0:4)
xts(x = rnorm(5), as.Date("2008-08-01") + 0:4)

as.data.frame(as.xts(rnorm(5), as.Date("2008-08-01") + 0:4))
as.data.frame(xts(rnorm(5), as.Date("2008-08-01") + 0:4))

df1 <- data.frame(date = as.Date("2008-08-01") + 0:4, value1 = rnorm(5), value2 = rnorm(5))
as.xts(df1, order.by = df1[, 1])
xts(df1, order.by = df1[, 1])
xts(ts)
as.xts(ts)
?xts

mydata <- read_excel("exercise1.xlsx") # 엑셀 파일로부터 데이터를 읽어옴
as.xts(mydata[, -1], order.by = as.Date(as.character(mydata[, 1]), format = '%b-%y'))

Sys.setlocale("LC_ALL", "English")
as.Date(as.character(mydata[, 1]), format = '%b-%y')
as.Date('Jun-83', format = '%B-%y')

# Look at the first few lines of mydata
head(mydata)  # 데이터 확인
glimpse(mydata)
# Create a ts object called myts
myts <- ts(mydata[2:4], start = c(1981, 1), frequency = 4)  # 읽어온 데이터를 ts 객체로 생성

head(myts)  # 데이터 확인
myxts <- as.xts(myts)
myxts <- as.xts(mydata[,2:4], order.by = as.Date(mydata[,1], format = '%b-%y'), frequency = 4)


as.Date(as.vector(mydata[,1]), '%b-%y')
?as.Date


install.packages('tseries')
data(sunspots)
st <- start(sunspots)
class(st)
str(st)
fr <- frequency(sunspots)
date <- as.character(as.Date('2010-01-01') + 1:30)


df <- data.frame(date = seq(as.Date('2010-01-01'), length.out = 30, by = 'months'), value1 = rnorm(30), value2 = rnorm(30))
write(df, "testfile.csv")


write.csv(df[,-1], "testfile_readts.csv", row.names = F)

x <- read.ts("testfile_readts.csv", header = TRUE, sep = ',', start = c(2010, 1), frequency = 12)
x
write.csv(df, "testfile_readts.csv", row.names = F)
t <- as.xts(read.zoo("testfile_readts.csv", sep = ',', index.column = 1, format = '%Y-%m-%d', header = TRUE))
class(t)
?read.zoo
glimpse(x1)
read.zoo("exercise1.csv")

t <- as.Date(10000)
t

class(t)
?read.zoo
seq(as.Date('2010-01-01'), length.out = 30, by = 'months')
cbind(Date = as.character(as.Date('2010-01-01') + 1:100), Value = rnorm(100))


x <- as.Date('2021-01-31') + 1:30
class(x)
as.Date('2021-01-31')
as.numeric(as.Date('2021-01-31'))
as.Date(18659)
Sys.Date() + 10

as.POSIXct('2021-01-31 12:34:56')
unclass(as.POSIXct('2021-01-31 12:34:55'))
unclass(as.POSIXct('2021-01-31 12:34:56'))
as.POSIXlt('2021-01-31 12:34:56')
unclass(as.POSIXlt('2021-12-31 12:34:56'))
as.POSIXlt('2021-12-31 12:34:56')$year


as.numeric(as.POSIXct('2021-01-31 12:34:56'))

as.numeric(as.Date('2021-01-31'))
as.Date(18659)
Sys.Date() + 10


as.yearmon("2007-02")
as.yearmon("2007-12")
unclass(as.yearmon("2007-02"))
unclass(as.yearmon("2007-02-01"))
unclass(as.yearmon("2007-12"))


as.yearqtr("2007-01")
as.yearqtr("2007-04")
unclass(as.yearqtr("2007-01"))
unclass(as.yearqtr("2007-04"))

as.Date('01/12/2010')
as.Date('01/12/2010', format = '%d/%m/%Y')
as.Date('01jan70', format = '%d%b%y')
Sys.setlocale("LC_ALL", "English")
as.Date('01jan70', format = '%d%b%y')
Sys.setlocale("LC_ALL", "Korean")
as.Date('011월70', format = '%d%b%y')


require("knitr")
opts_knit$set(root.dir = "./doc")
getwd()


rmarkdown::render('./index.Rmd', output_dir='.')
setwd('../')
getwd()


install.packages('bookdown')
library(bookdown)

?read_excel
students.from.excel <- read_excel("./students.xlsx", skip = 16, na = '-', sheet = 1, col_types = c('text', 'text', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric'))

glimpse(mydata)
warnings()


students <- read.csv('./students.csv', skip = 16, header = TRUE, na = '-', strip.white = TRUE, stringsAsFactors = TRUE)

glimpse(students)
?read.csv

students[, 3:18] <- apply(students[, 3:18], 2, function(y) as.numeric(gsub(",", "", y)))
students.total.ts <- students %>% filter(지역규모 == '계') %>% ts(start = 1999, frequency = 1)
class(students.total.ts)
date <- as.Date(as.character(paste0(students.total.xts[,1], '-01-01')), format = '%Y-%m-%d')

students.total.xts <- students %>% filter(지역규모 == '계') %>% select(-지역규모)

students.total.xts <- as.xts(students.total.xts, order.by = as.Date(as.character(students.total.xts[,1]), format = '%Y'))


students.total.xts <- as.xts(students.total.xts, order.by = as.Date(as.character(paste0(students.total.xts[,1], '-01-01')), format = '%Y-%m-%d'))

students.total.xts <- as.xts(students.total.xts, order.by = as.Date(paste0(students.total.xts[,1], '-01-01'), format = '%Y-%m-%d'))


students.total.xts <- as.xts(students.total.xts, order.by = as.Date(date))

class(paste0(students.total.xts[,1], '-01-01'))

glimpse(students.total.xts)

?xts

xts(order.by = as.Date(students[,1], format = '%Y') )

glimpse(as.xts(students.total.xts, order.by = as.Date(as.character(students.total.xts[,1]), format = '%Y')))

as.Date(as.character(students[,1]), format = '%Y')
?as.Date


zoo.class <- read.zoo("testfile_readts.csv", sep = ',', index.column = 1, format = '%Y-%m-%d', header = TRUE)
class(zoo.class)
xts.class <- as.xts(zoo.class)
class(xts.class)

students.ggplot <- students %>% filter(지역규모 == '계')
ggplot(data = students.ggplot, aes(x = as.factor(연도), y = 학생수계)) +
  geom_line(aes(group = 1)) + 
  geom_point(shape = 'circle') +
  labs(title = '연도별 학생수', x = '연도') +
  scale_y_continuous(labels = scales::number_format(big.mark = ',')) + 
  theme(axis.text.x=element_text(angle=90,hjust=1))

?addLegend
colnames(students.total.xts)
?plot.xts

plot.xts(students.total.xts[,2], main = '학교급별 학생수')
plot.xts(students.total.xts[,5], main = '연도별 학생수', sub = '전체 학생수의 연도별 변화', xlab = '연도',  ylab = '학생수', ylim = c(0, 2200000))
lines(students.total.xts[,3], col = 'red')

addLegend("topright", legend.names=c('전체', '유치원'), col=c('black', 'red'), lty=c(1,1), lwd=c(2,2), ncol=2, bg="white")


students.timetk <- students %>% filter(지역규모 == '계')
students.timetk <- students
plot_time_series(.data = students.timetk, .date_var = students.timetk[,1], .value = students.timetk[,4], .color_var = students.timetk[,2], .smooth = F)


students %>%
  plot_time_series(.date_var = 연도, .value = 학생수계, .color_var = 지역규모, .smooth = F)


students %>%
  plot_time_series(.date_var = 연도, .value = 학생수계, .color_var = 지역규모, .smooth = F, .facet_vars = 지역규모)


students.total.ts <- students %>% 
  filter(지역규모 == '계') %>% 
  select(3:18) %>%
  ts(start = c(1999), frequency = 1)


autoplot(students.total.ts[,7], lty = 2, lwd = 2)
?autoplot.ts



library(lubridate)

test.time.date <- Sys.time()
test.time.char <- as.character(Sys.time())
test.time.xts <- as.xts(Sys.time())
year(test.time.date)
month(test.time.char)
day(test.time.xts)
yday(test.time.date)
qday(test.time.char)
wday(test.time.xts, label = T, abbr = T)
hour(test.time.date)
minute(test.time.char)
second(test.time.xts)
week(test.time.date)
quarter(test.time.char, with_year = T)
semester(test.time.xts, with_year = T)
am(test.time.date)
pm(test.time.char)
leap_year(test.time.date)

days_in_month(test.time.date)

format(test.time,"%S")

library(stringr)
test.time <- as.character(Sys.time())
str_sub(test.time, start = 1, end = 4)
str_sub(test.time, start = 6, end = 7)
str_sub(test.time, start = 9, end = 10)
str_sub(test.time, start = 12, end = 13)
str_sub(test.time, start = 15, end = 16)
str_sub(test.time, start = 18, end = 19)
?str_sub


test.time <- as.POSIXct(Sys.time())
test.time <- as.xts(Sys.time())
test.time <- as.ts(Sys.time())

class(test.time)

as.POSIXct(ts)

today <- today()
today + 100
test.time - months(2)
test.time - years(1)
test.time - 50

today + 1
class(today)

int <- interval(as.Date('1980-01-01'), as.Date('2021-12-31'))
as.period(int)
as.duration(int)


int <- interval(as.Date('1973-09-08'), as.Date('2021-02-03'))
as.period(int)
as.duration(int)


as.Date('2011-01-01') + years(1)
as.Date('2011-01-01') + dyears(1)
leap_year(2012)
as.Date('2012-01-01') + years(1)
as.Date('2012-01-01') + dyears(1)
library(rmarkdown)
render('03-data_handling.Rmd')

as.Date('2012-01-01') + weeks(0:5)


x <- ymd_hms("2009-08-03 12:01:59.23")
round_date(x, ".5s")
round_date(x, "sec")
round_date(x, "second")
round_date(x, "minute")
round_date(x, "5 mins")
round_date(x, "hour")
round_date(x, "2 hours")
round_date(x, "day")
round_date(x, "week")
round_date(x, "month")
round_date(x, "bimonth")
round_date(x, "quarter") == round_date(x, "3 months")
round_date(x, "halfyear")
round_date(x, "year")


한국장학재단_일일대출실행통계_20191231


loan <- read.csv('./한국장학재단_일일대출실행통계_20191231.csv', header = TRUE, na = '-', strip.white = TRUE, stringsAsFactors = TRUE)
glimpse(loan)
loan$기준일자 <- as.Date(loan$기준일자)


?rollmean
loan %>% 
  filter(상품명 == '취업후상환학자금_등록금') %>% 
  select(기준일자, 당일신규실행건수) %>%
  mutate(roll = rollmean(당일신규실행건수, k = 30, fill = NA))


loan %>%
  mutate(month = month(기준일자), year = year(기준일자))
  group_by(month, year, 말일) %>%
  summarise(total.신규실행건수 = sum(당일신규실행건수), 
            total.신규실행액수 = sum(당일신규실행금액.억원.))

47507/31

library(timetk)
loan %>% 
  summarise_by_time(.date_var = 기준일자, .by = 'month', total.신규실행건수  = sum(당일신규실행건수), total.신규실행액수 = sum(당일신규실행금액.억원.), mean.건수 = mean(당일신규실행건수))
  

as.Date('2011-01-01') + years(1)
x <- as.xts(as.Date('2011-01-01'))
x + dyears(1)
leap_year(2012)
as.Date('2012-01-01') + years(1)
as.Date('2012-01-01') + dyears(1)
as.Date('2012-01-01') + weeks(0:10)
as.Date('2012-01-01') + dweeks(0:10)


x + 100
students.total.xts[1,1]

loan.xts <- as.xts(loan[, -c(1, 2)], order.by = loan$기준일자)
loan.xts[,3] <- as.numeric(loan.xts[,3])
x <- as.xts(as.Date("2009-08-03"))
x + dyears(1)
last(loan.xts[10,], '1 year')


x <- as.xts(x = rnorm(500), order.by = as.Date("2008-08-01") + 0:499)
last(x, 'month')
first(x, '2 weeks')
x[endpoints(x), ]
ndays(x)
?last
apply.monthly(loan.xts[,1], sum)

temps <- read.zoo('D:/R/git/Manipulating-Time-Series-Data-with-xts-and-zoo-in-R/temps.csv', sep = ',', header = T)
class(temps)

# Create lastweek using the last 1 week of temps
lastweek <- last(temps, "1 week")

# Print the last 2 observations in lastweek
last(lastweek, n = 2)

# Extract all but the first two days of lastweek
first(lastweek, "-2 day")
days_in_month(2)
days_in_month(as.Date('2012-2', format = '%Y-%d', tz = 'american'))


local_time(as.Date('2012-2', format = '%Y-%d'), 'Europe/Amsterdam')

?economics 

ma3 <- slidify(mean, .period = 3, .align = "right")
sum3 <- slidify(sum, .period = 3, .align = "right")
class(ma3)
loan %>%
  mutate(ma3 = ma3(당일신규실행건수), sum3 = sum3(당일신규실행건수)) %>%
  select(당일신규실행건수, ma3, sum3) %>%
  head(10)
install.packages('tibbletime')
library(tibbletime)

as_tbl_time(loan, index = 기준일자) %>%
  collapse_by('monthly') %>%
  group_by(기준일자) %>% 
  summarise(total.신규실행건수 = sum(당일신규실행건수), 
            total.신규실행액수 = sum(당일신규실행금액.억원.)) %>%
  mutate(cum.건수 = cumsum(total.신규실행건수), 
         cum.액수 = cumsum(total.신규실행액수))%>%
  select(1, 2, 4, 3, 5)

as_tbl_time(loan, index = 기준일자)
?as_tbl_time


as_tbl_time(loan, index = 기준일자) %>%
  collapse_by('monthly') %>%
  group_by(기준일자) %>% 
  mutate(cumsum.건수 = cumsum(당일신규실행건수), 
         cumsum.액수 = cumsum(당일신규실행금액.억원.)) %>%
  head(20)


split(loan.xts, f = 'months')
lapply(split(loan.xts, f = 'months'), cumsum)
dim(do.call(rbind, lapply(split(loan.xts, f = 'months'), cumsum)))


loan %>%
  filter(기준일자 >= as.Date('2019-03-01') & 기준일자 <= as.Date('2019-03-03'))


white_noise <- arima.sim(model = list(order = c(0,0,0)), n = 100)


students <- read.csv('./students.csv', skip = 16, header = TRUE, na = '-', strip.white = TRUE, stringsAsFactors = TRUE)
students[, 3:18] <- apply(students[, 3:18], 2, function(y) as.numeric(gsub(",", "", y)))
students.total.xts <- students %>% filter(지역규모 == '계') %>% select(-지역규모)
students.total.xts <- as.xts(students.total.xts, order.by = as.Date(paste0(students.total.xts[,1], '-01-01'), format = '%Y-%m-%d'))
library(timetk)
students.total.ts <- students %>% 
  filter(지역규모 == '계') %>% 
  select(3:18) %>%
  ts(start = c(1999), frequency = 1)

students %>%
  filter(지역규모 == '계') %>%
  mutate(lag1 = lag(학생수계, 1), 
         lag3 = lag(학생수계, 3)) %>%
  select(연도, 학생수계, lag1, lag3) %>%
  head(10)

students %>%
  filter(지역규모 == '계') %>%
  mutate(diff1 = c(0, diff(학생수계, lag = 1)), 
         diff3 = c(0, 0, 0, diff(학생수계, lag = 3))) %>%
  select(연도, 학생수계, diff1, diff3) %>%
  head(10)

         
students %>%
  filter(지역규모 == '계') %>%
  mutate(lag1 = lag(학생수계, 1), 
         lag3 = lag(학생수계, 3), 
         diff1 = c(0, diff(학생수계, lag = 1)), 
         diff3 = c(0, 0, 0, diff(학생수계, lag = 3)), 
         diff1.cal = ifelse(!is.na(lag1), 학생수계-lag1, 0), 
         diff3.cal = ifelse(!is.na(lag3), 학생수계-lag3, 0))%>%
  select(연도, 학생수계, lag1, diff1, diff1.cal, lag3, diff3, diff3.cal) %>%
  head(10)


plot <- students %>%
  filter(지역규모 == '계') %>%
  mutate(lag1 = lag(학생수계, 1), 
         lag3 = lag(학생수계, 3), 
         diff1 = c(NA, diff(학생수계, lag = 1)), 
         diff3 = c(NA, NA, NA, diff(학생수계, lag = 3)), 
         diff1.cal = ifelse(!is.na(lag1), 학생수계-lag1, NA), 
         diff3.cal = ifelse(!is.na(lag3), 학생수계-lag3, NA)) %>%
  select(연도, 학생수계, lag1, diff1, diff1.cal, lag3, diff3, diff3.cal)
xts::as.xts(plot[, c(2, 3, 4)], order.by = as.Date(as.character(plot[, 1]), format = '%Y'))


students.total.xts$학생수계.lag1 <- stats::lag(students.total.xts$학생수계, 1)

colnames(employees) <- c('time', 'total', 'employees.edu')
ts(employees[, 3], start = c(2013, 01), frequency = 12) %>%
  decompose() %>% autoplot()
xts(employees[, 3], order.by = as.Date(employees[, 1], format = '%Y. %M')) %>%
  decompose() %>% autoplot()
?decompose
library(seasonal)
?seas
ts(employees[, 3], start = c(2013, 01), frequency = 12) %>%
  seas() %>% autoplot()

as.Date(paste0(employees[, 1], '. 01'), format = '%Y. %m. %d')

employees %>%
  mutate(date = as.Date(employees[, 1], format = '%Y. %M')) %>%
  select(4, 2) %>%
  plot_stl_diagnostics(
    .date = date, .value = total,
    .feature_set = c("observed", "season", "trend", "remainder"),
    .frequency = 12,
    .trend = 12)

tk_get_frequency(as.Date(employees[, 1], format = '%Y. %M'), period = "auto")

employees %>%
  mutate(date = as.Date(employees[, 1], format = '%Y. %M')) %>%
  select(4, 2) %>% tk_index() %>% tk_get_frequency(period = "auto")

employees %>%
  mutate(date = as.Date(employees[, 1], format = '%Y. %M')) %>%
  select(4, 2) %>%
  tk_index() %>% tk_get_trend(period = "auto")

ggseasonplot(ts(employees[, 3], start = c(2013, 01), frequency = 12), polar = T)


students %>% filter(지역규모 == '계') %>%
  ts(start = c('1999'), frequency = 1) %>%
  meanf(h = 10)
glimpse(students)
meanf(students$학생수계, h = 10)

students.total.ts <- students %>% 
  filter(지역규모 == '계') %>% 
  select(3) %>%
  ts(start = c(1999), frequency = 1)

glimpse(students.total.ts)

autoplot(meanf(students.total.ts, h = 10))

mean(students.total.ts) + sqrt(sd(students.total.ts))*1.96
7598603 + sd(students.total.ts - 7598603)*(sqrt(1+(1/length(students.total.ts))))*1.96

sd(resid(meanf(students.total.ts[,1])))*1.96 + mean(students.total.ts[,1])

class(meanf(students.total.ts, h = 10))


class(students.total.ts[1])
  ts(start = c(1999), frequency = 1)
  
  students.total.ts <- students %>% 
    filter(지역규모 == '계') %>% 
    select(3:18) %>%
    ts(start = c(1999), frequency = 1)  
forecast::meanf(students.total.ts[, 1])

install.packages('growthrates')
library(growthrates)
growth <- students %>% filter(지역규모 == '계') %>% select(1, 3)
fit_easylinear(1:22, as.numeric(growth[, 2]))  
data(bactgrowth, package = 'growthrates')
class(bactgrowth)
splitted.data <- multisplit(bactgrowth, c("strain", "conc", "replicate"))
dat <- splitted.data[[1]]
plot(value ~ time, data=dat)
fit <- fit_easylinear(dat$time, dat$value)
summary(lm(dat$time~dat$value))
class(dat$time)
class(dat$value)
tis::growth.rate(students.total.ts[,1], lag = 1, simple = T)
install.packages('tis')
fit_growthmodel(FUN = grow_twostep, p = p, time = dat$time, y = dat$value,
                lower = lower, upper = upper)
summary(naive(employees.ts[,2]))

?snaive

glimpse(students.ts)
library(forecast)
summary(tslm(students.ts[, 3] ~ students.ts[, 2], data = students.ts))

ts.lm <- tslm(students.ts[,1] ~ trend, data = students.ts)
summary(ts.lm)
ts.lm %>% forecast() %>% autoplot()

class(ts.lm)
newdata <- as.ts(as.numeric(students.ts[17:22, 2]))

tslm(students.ts[, 3] ~ students.ts[, 2], data = students.ts) %>% forecast(newdata = newdata) %>% autoplot()

autoplot(forecast.ts.lm)
summary(lm(students.ts[, 3] ~ students.ts[, 2], data = students))


employees <- read.csv('./산업별_취업자_20210206234505.csv', header = TRUE, na = '-', strip.white = TRUE, stringsAsFactors = TRUE)
colnames(employees) <- c('time', 'total', 'employees.edu')
employees.ts <- ts(employees, start = c(2013, 01), frequency = 12)

employee.total.ts.lm <- tslm(employees.ts[,2] ~ employees.ts[,3] + trend + season, data = employees.ts)
summary(employee.total.ts.lm)
employee.total.ts.lm %>% forecast(h = 96) %>% autoplot(PI = FALSE)


student.ts.lm <- tslm(students.total.ts[,3] ~ students.total.ts[,4] + trend, data = students.total.ts)
student.ts.lm %>% forecast(h = 22) %>% 
  autoplot() + scale_x_date(limits = as.Date(c('1999-01-01','2030-01-01')))
  
  c(as.Date('1999', format = '%Y'), as.Date('2030', format = '%Y')))


skim(students.total.ts)


x <- data.frame(date = as.Date('2008-01-01') + 0:9, id = 1:10, x1 = rnorm(10), x2= rnorm(10))

as_tsibble(x, key = id, index = date)


seq('a', 10)
set.seed(345)
arima.sim(model = list(order = c(1, 0, 0), ar = 0.9), n = 200) %>% autoplot()
diff(arima.sim(model = list(order = c(1, 0, 0), ar = 0.9), n = 200), 1) %>% autoplot()
arima.sim(model = list(order = c(2, 0, 0), ar = c(1.5, -0.75)), n = 200) %>% autoplot()
diff(arima.sim(model = list(order = c(2, 0, 0), ar = c(1.5, -0.75)), n = 200), 1) %>% autoplot()
?arima.sim

arima.sim(n = 63, list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488)),
          rand.gen = function(n, ...) sqrt(0.1796) * rt(n, df = 5)) %>% autoplot()

?rand.gen

employees %>% 
  mutate(year = year(time)) %>%
  group_by(year) %>%
  summarise(total.year = sum(total), 
            employees.edu.year = sum(employees.edu)) %>%
  ggplot(aes(year, total.year)) + 
  geom_line() + 
  geom_point() + 
  geom_text(aes(label = scales::comma(total.year)), vjust = 1) +
  labs(title = '연도별 전체 취업자수 합계 추이', x = '연도', y = '취업자수')
