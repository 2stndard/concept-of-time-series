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
library(lubridate)
library(tidymodels)
if(!require(skimr)) {
  install.packages('skimr')
  library(skimr)
}
library(tsibble)
library(feasts)

library(numbers)
library(prophet)
library(fable.prophet)
library(fable)
library(tibbletime)


#install.packages('astsa')
library(astsa)
library(seastests)

library(showtext)
font_add_google("Gochi Hand", "gochi") 
font_add_google("Schoolbell", "bell")
font_add_google("Nanum Gothic", "nanumgothic")
font_add_google("Poor Story", "poorstory")
showtext_auto()

library(devtools)

install_github("mrchypark/tqk")

library(tqk)
code <- code_get()
sscode <- code[grep("^삼성전자$", code$name),3]
samsung <- tqk_get(sscode, from="2019-01-01", to = '2020-12-31')
tail(samsung)


samsung %>%
  ggplot(aes(x = date, y = close)) + 
  geom_line(aes(group = 1)) + 
  labs(title = '삼성전자 주가', subtitle = '2019-01-01부터 2020-12-31까지', x = '일자', y = '종가')
