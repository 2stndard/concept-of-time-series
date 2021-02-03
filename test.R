library(tidyverse)
library(zoo)
library(xts)
library(readxl)
if(!require(modeltime)) {
  install.packages('modeltime')
  library(modeltime)
}
library(tseries)
library(ggthemes)
library(timetk)
library(forecast)
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
  
