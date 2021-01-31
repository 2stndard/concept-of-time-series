ts <- ts(1:10, frequency = 4, start = c(1959, 2))
head(ts)

xts
library(tidyverse)
library(zoo)
head(zoo(rnorm(5), as.Date("2008-08-01") + 0:4), 1)
as.Date("2008-08-01") + 0:2

library(xts)
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

library(readxl)
mydata <- read_excel("exercise1.xlsx") # 엑셀 파일로부터 데이터를 읽어옴

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

if(!require(modeltime)) {
  install.packages('modeltime')
  library(modeltime)
}

install.packages('tseries')
library(tseries)
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
