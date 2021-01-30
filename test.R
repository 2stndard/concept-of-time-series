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
