######################################
###  3장 시계열 시각화
###  3.1 data.frame: ggplot2 패키지

library(ggplot2)

students %>%
  ggplot(aes(x = 연도, y = 학생수계)) +
  geom_line(aes(group = 1)) +
  labs(title = '연도별 학생수 추이')

ggplot(data = students, aes(x = as.factor(lubridate::year(연도)), y = 학생수계)) +
  geom_line(aes(group = 1)) +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(title = '연도별 학생수 추이', x = '연도')

ggplot(data = students.all, aes(x = 연도, y = 학생수계)) +
  geom_line(aes(group = 지역규모, linetype = 지역규모)) +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(title = '연도별 학생수 추이', x = '연도')

ggplot(data = students, aes(x = as.factor(lubridate::year(연도)), y = 학생수계)) +
  geom_line(aes(group = 1)) +
  geom_point(shape = 'circle') +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(title = '연도별 학생수 추이', x = '연도')

ggplot(data = students.all, aes(x = 연도, y = 학생수계)) +
  geom_line(aes(group = 지역규모, linetype = 지역규모)) +
  geom_point(shape = 'circle', size = 0.5) +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(title = '연도별 학생수 추이', x = '연도')

ggplot(data = students, aes(x = as.factor(lubridate::year(연도)), y = 학생수계)) +
  geom_line(aes(group = 1)) +
  geom_point(shape = 'circle') +
  geom_text(aes(label = scales::number(학생수계, big.mark = ',')), size = 2, vjust = 1.5) +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(title = '연도별 학생수 추이', x = '연도')

library(ggrepel)

ggplot(data = students, aes(x = as.factor(lubridate::year(연도)), y = 학생수계)) +
  geom_line(aes(group = 1)) +
  geom_point(shape = 'circle') +
  geom_text_repel(aes(label = scales::number(학생수계, big.mark = ',')), size = 2, vjust = 1.5) +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(title = '연도별 학생수 추이', x = '연도')

ggplot(data = students, aes(x = as.factor(lubridate::year(연도)), y = 학생수계)) +
  geom_line(aes(group = 1)) +
  geom_point(shape = 'circle') +
  geom_text_repel(aes(label = scales::number(학생수계, big.mark = ',')), size = 2, vjust = 1.5) +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(title = '연도별 학생수 추이', x = '연도') +
  scale_y_continuous(labels = scales::number_format(big.mark = ','))

ggplot(data = employees, aes(x = time, y = total)) +
  geom_line(aes(group = 1)) +
  geom_point(shape = 'circle') +
  labs(title = '월별 신규 취업자수', x = '기간', y = '취업자수') +
  scale_y_continuous(labels = scales::number_format(big.mark = ',')) +
  scale_x_date(breaks = '6 month') +
  theme(axis.text.x=element_text(angle=90,hjust=1))

ggplot(data = covid19, aes(x = date, y = `0-9세`)) +
  geom_line(aes(group = 1)) +
  geom_point(shape = 'circle') +
  labs(title = '일별 코로나 확진자수(0-9세)', x = '시간', y = '확진자수') +
  scale_y_continuous(labels = scales::number_format(big.mark = ',')) +
  scale_x_date(breaks = '15 day') +
  theme(axis.text.x=element_text(angle=90,hjust=1))


###  3.2 xts: xts 패키지

library(xts)

plot.xts(employees.xts$total, main = '월별 취업자수 추이', xlab = '월, 연', ylab = '취업자수')

plot.xts(employees.xts, main = '연도별 학생수 추이', xlab = '연', ylab = '학생수', yaxis.right=FALSE)

addLegend('bottomleft', ncol = 1, bg = 'white', lty=c(rep(1, 12)), lwd=c(rep(2, 12)), bty="o")

plot.xts(students.xts$초등학교, main = '연도별 학생수 추세', xlab = '연', ylab = '학생수', yaxis.right=FALSE, ylim = c(0, max(students.xts$초등학교)), col = 'black')

lines(students.xts$유치원, lty = 2, col = 'red')

lines(students.xts$중학교, lty = 3, col = 'blue')

addLegend('topright', ncol = 1, legend.names = c('초등학교', '유치원', '중학교'), col = c('black', 'red', 'blue'), lty=c(1, 2, 3), bg = 'white', bty="o")

plot.xts(covid19.xts, main = '일별 확진자수', xlab = '날짜', ylab = '확진자수')

addLegend('topleft', ncol = 2, legend.names = c('0-9세', '10-19세', '20-29세', '30-39세', '40-49세', '50-59세', '60-69세', '70-79세', '80세 이상'), lty = 1, bg = 'white', bty="o")


###  3.3 ts: forecast 패키지

library(forecast)

autoplot(students.ts[,-1], main = '연도별 학생수', xlab = '연도', ylab = '학생수')

autoplot(students.ts[, 4], main = '연도별 학생수', xlab = '연도', ylab = '학생수', series = '초등학교', lty = 1) +
  autolayer(students.ts[, 3], series = '유치원', lty = 2) +
  autolayer(students.ts[, 5], series = '중학교', lty = 3) +
  labs(colour = "학교급")

autoplot(students.ts[, 3:5], main = '연도별 학생수', xlab = '연도', ylab = '학생수', facet = TRUE)

autoplot(students.ts[,2], main = '연도별 학생수', xlab = '연도', ylab = '학생수', series = '유치원', lty = 1, lwd = 1) +
  autolayer(students.ts[,3], series = '초등학교', lty = 2, lwd = 1.2) +
  autolayer(students.ts[,4], series = '중학교', lty = 3, lwd = 1.4) +
  autolayer(students.ts[,5], series = '고등학교', lty = 4, lwd = 1.6) +
  scale_y_continuous(labels=scales::number_format(big.mark = ','))

autoplot(employees.ts[,2], main = '월별 취업자수', xlab = '연도', ylab = '취업자수', series = '전체 취업자', lty = 1, lwd = 1)

autoplot(covid19.ts[,2], main = '일별 확진자수(0-9세)', xlab = '날짜', ylab = '확진자수', series = '확진자', lty = 1, lwd = 1)


###  3.4 tsibble: feasts 패키지

library(feasts)

library(dplyr)

students.tsibble %>% autoplot(학생수계) +
  labs(title = '연도별 학생수', x = '연도', y = '학생수')

students.tsibble %>% select(1, 3, 4, 5) %>%
  tidyr::gather(category, value, 2:4) %>% autoplot()

ggplot(students.tsibble, aes(x = 연도)) +
  geom_line(aes(y = 초등학교, group = 1, linetype = '초등학교')) +
  geom_line(aes(y = 유치원, group =1, linetype = '유치원')) +
  geom_line(aes(y = 중학교, group =1, linetype = '중학교')) +
  labs(title = '연도별 학생수', x = '연도', y = '학생수', color = '학교급') +
  scale_y_continuous(labels = scales::number_format(big.mark = ',')) +
  scale_linetype_manual(values = c('초등학교' = 1, '유치원' = 2, '중학교' = 3))

employees.tsibble %>% mutate(time = yearmonth(employees.tsibble$time)) %>%
  gg_season(total)

employees.tsibble %>% mutate(time = yearmonth(employees.tsibble$time)) %>%
  gg_subseries(total)

###  3.5 data.frame: timetk 패키지

library(timetk)

students %>%
  plot_time_series(.date_var = 연도, .value = 학생수계, .smooth = T, .line_type = 2, .smooth_size = 0.5, .title = 'timetk를 사용한 전체 학생수 플롯', .x_lab = '연도', .y_lab = '학생수')

students.all %>%
  plot_time_series(.date_var = 연도, .value = 학생수계, .color_var = 지역규모, .smooth = F, .title= 'timetk를 사용한 전체 학생수 다변량 플롯', .x_lab = '연도', .y_lab = '학생수', .interactive = FALSE) + theme(axis.text.x=element_text(angle=90,hjust=1))

students %>% select(1, 3, 4, 5) %>%
  tidyr::gather(category, value, 2:4) %>%
  plot_time_series(.date_var = 연도, .value = value, .color_var = category, .smooth = F, .title = 'timetk를 사용한 전체 학생수 플롯', .x_lab = '연도', .y_lab = '학생수')

employees %>%
  plot_time_series(.date_var = time, .value = total, .smooth = F, .title = '월별 신규 취업자수', .x_lab = '연도', .y_lab = '취업자수')

covid19 %>%
  plot_time_series(.date_var = date, .value = `0-9세`, .smooth = F, .title = '일별 코로나 확진자수(0-9세)', .x_lab = '연월', .y_lab = '확진자수')
