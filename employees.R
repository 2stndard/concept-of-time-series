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

summary(ses(employees.ts[,2]))
nrow(employees.ts)
mean(employees.ts[1:48,2])
i <- 1
for(i in 1:nrow(employees.ts)) {
  print(i)
  print(25251.684 - round(mean(mean(employees.ts[1:i,2])), 3))
}

autoplot(employees.ts[,2]) + 
  autolayer(fitted(holt(employees.ts[,2], beta = 0.1)), PI = FALSE, series = 'beta = 0.1') +
  autolayer(fitted(holt(employees.ts[,2], beta = 0.2)), PI = FALSE, series = 'beta = 0.2') + 
  autolayer(fitted(holt(employees.ts[,2], beta = 0.3)), PI = FALSE, series = 'beta = 0.3') +
  autolayer(fitted(holt(employees.ts[,2], beta = 0.4)), PI = FALSE, series = 'beta = 0.4') +
  autolayer(fitted(holt(employees.ts[,2], beta = 0.5)), PI = FALSE, series = 'beta = 0.5') +
  autolayer(fitted(holt(employees.ts[,2], beta = 0.6)), PI = FALSE, series = 'beta = 0.6') +
  autolayer(fitted(holt(employees.ts[,2], beta = 0.7)), PI = FALSE, series = 'beta = 0.7') +
  autolayer(fitted(holt(employees.ts[,2], beta = 0.8)), PI = FALSE, series = 'beta = 0.8') +
  autolayer(fitted(holt(employees.ts[,2], beta = 0.9)), PI = FALSE, series = 'beta = 0.9')

