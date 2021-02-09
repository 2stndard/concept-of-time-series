employees <- read.csv('./산업별_취업자_20210206234505.csv', header = TRUE, na = '-', strip.white = TRUE, stringsAsFactors = TRUE)
colnames(employees) <- c('time', 'total', 'employees.edu')
employees.ts <- ts(employees, start = c(2013, 01), frequency = 12)

tslm(employees.ts[,2] ~ trend, data = employees.ts, lambda = 1) %>%
  forecast() %>%
  autoplot()


employees.ts[,2] %>%
  splinef(lambda = 0) %>%
  autoplot()

summary(wo(employees.ts[,3]))
