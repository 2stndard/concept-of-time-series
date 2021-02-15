students <- read.csv('./students.csv', skip = 16, header = TRUE, na = '-', strip.white = TRUE, stringsAsFactors = TRUE)
students[, 3:18] <- apply(students[, 3:18], 2, function(y) as.numeric(gsub(",", "", y)))
students.total.xts <- students %>% filter(지역규모 == '계') %>% select(-지역규모)
students.total.xts <- as.xts(students.total.xts, order.by = as.Date(paste0(students.total.xts[,1], '-01-01'), format = '%Y-%m-%d'))
library(timetk)
students.total.ts <- students %>% 
  filter(지역규모 == '계') %>% 
  select(3:18) %>%
  ts(start = c(1999), frequency = 1)
students.tsibble <- as_tsibble(students, key = 지역규모, index = 연도)

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
covid19.by.age <- covid19 %>% 
  filter(grepl('세', category)) %>% 
  filter(category != '세종')
covid19.by.age$value <- ifelse(is.na(covid19.by.age$value), 0, covid19.by.age$value)
wide.covid19.by.age <- tidyr::spread(covid19.by.age, category, value)

wide.covid19.by.age.ts = ts(wide.covid19.by.age[, 2:10], frequency = 365)
wide.covid19.by.age.xts <- as.xts(wide.covid19.by.age[, 3:10], order.by = wide.covid19.by.age$date)
wide.covid19.by.age.tsibble <- as_tsibble(wide.covid19.by.age, index = date)
