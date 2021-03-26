employees.by.year %>% 
  ggplot(aes(as.factor(year), total.year)) + 
  geom_line(aes(group = 1)) +
  geom_text(aes(label = scales::number(total.year, big.mark = ',')), size = 3, vjust = 1.5) + 
  labs(title = '연별 취업자수', x = '연도', y = '취업자수') + 
  scale_y_continuous(labels = scales::number_format(big.mark = ','))


mean.covid19.by.age %>%
  tidyr::gather(category, value, 2:10) %>%
  ggplot(aes(x = yearmon, y = value)) + 
  geom_line(aes(group = category, color = category)) + 
  labs(title = '월간 평균 확진자수', x = '시간', y = '평균확진자', color = '세대')

library(tibbletime)
as_tbl_time(covid19, index = date) %>%
  collapse_by('weekly') %>%
  group_by(date) %>% 
  summarise(`01대` = mean(`0-9세`), 
            `10대` = mean(`10-19세`), 
            `20대` = mean(`20-29세`), 
            `30대` = mean(`30-39세`), 
            `40대` = mean(`40-49세`), 
            `50대` = mean(`50-59세`), 
            `60대` = mean(`60-69세`), 
            `70대` = mean(`70-79세`), 
            `80대` = mean(`80세 이상`)) %>%
  tidyr::gather(category, value, 2:10) %>%
  ggplot(aes(x = date, y = value)) + 
  geom_line(aes(group = category, color = category)) + 
  labs(title = '주간 평균 확진자수', x = '월', y = '평균확진자', color = '세대')


library(timetk)
covid19 %>% 
  summarise_by_time(.date_var = date, .by = 'week', 
                    `01대` = mean(`0-9세`), 
                    `10대` = mean(`10-19세`),
                    `20대` = mean(`20-29세`),
                    `30대` = mean(`30-39세`),
                    `40대` = mean(`40-49세`),
                    `50대` = mean(`50-59세`),
                    `60대` = mean(`60-69세`),
                    `70대` = mean(`70-79세`),
                    `80대` = mean(`80세 이상`)) %>%
  tidyr::gather(category, value, 2:10) %>%
  ggplot(aes(x = date, y = value)) + 
  geom_line(aes(group = category, color = category)) + 
  labs(title = '주간 평균 확진자수', x = '월', y = '평균확진자', color = '세대')


library(xts)
apply.quarterly(employees.xts, sum) %>% head(10)
apply.yearly(employees.xts, sum) %>% plot.xts()
apply.monthly(covid19.xts[,1], sum) %>% plot.xts(main = '월별 0-9세 코로나 확진자수')
apply.quarterly(covid19.xts[,1], sum) %>% plot.xts(main = '분기별 0-9세 코로나 확진자수')


library(zoo)
employees %>%
  mutate(ma3 = rollmean(total, k = 3, fill = NA),
         sum3 = rollapply(total, 3, sum, fill = NA)) %>%
  select(time, total, ma3, sum3) %>%
  ggplot(aes(x = time)) + 
  geom_line(aes(y = total, group = 1, color = 'total')) +
  geom_line(aes(y = ma3, group = 1, color = 'ma3')) + 
  scale_color_manual(values = c('total' = 'red', 'ma3' = 'blue'))


ma3 <- slidify(mean, .period = 3, .align = "center")
sum3 <- slidify(sum, .period = 3, .align = "center")
class(ma3)
class(sum3)
employees %>%
  mutate(ma3 = ma3(total), sum3 = sum3(total)) %>%
  select(time, total, ma3, sum3) %>%
  ggplot(aes(x = time)) + 
  geom_line(aes(y = total, group = 1, color = 'total')) +
  geom_line(aes(y = ma3, group = 1, color = 'ma3')) + 
  scale_color_manual(values = c('total' = 'red', 'ma3' = 'blue'))


students_lag %>%
  ggplot(aes(as.factor(year(연도)), 증감)) + 
  geom_line(aes(group = 1)) + 
  geom_point() +
  ggrepel::geom_text_repel(aes(label = scales::comma(증감)), vjust = 1, size = 3) +  ### ggrepel::geom_text_repel() 함수로 숫자들이 겹치지 않게 plotting
  labs(title = '전년대비 전체 학생수 증감 추이', x = '연도', y = '학생수 증감량') +
  scale_y_continuous(labels = scales::number_format(big.mark = ',')) +
  theme(axis.text.x=element_text(angle=90,hjust=1))


students.xts$증감 <- diff(students.xts[,2]) 
students.xts$증감율 <- round((students.xts$증감/students.xts$학생수계), 3) * 100
students.xts[, c('학생수계', '증감', '증감율')] %>% head(10)
plot.xts(students.xts[, '증감율'], main = '전년대비 학생수 증감률')
employees.xts$증감 <- diff(employees.xts$total)
employees.xts$증감율 <- round((employees.xts$증감/employees.xts$total), 3) * 100
employees.xts[, c('total', '증감', '증감율')] %>% head(10)
plot.xts(employees.xts[, c('증감율')], main = '전월대비 전체 취업자 증감률')


###   누적 취업자수 산출
employees %>% 
  mutate(cumsum = cumsum(total)) %>%
  select(time, total, cumsum) %>%
  head(15)
### 0-9세 코로나 확진자의 누적 plot
covid19 %>%
  mutate(cumsum = cumsum(`0-9세`)) %>%
  select(date, `0-9세`, cumsum) %>%
  ggplot(aes(date, cumsum)) +
  geom_line(aes(group = 1)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%y.%m")  +
  theme(axis.text.x=element_text(angle=90,hjust=1))


employees %>%
  mutate(year = lubridate::year(employees$time)) %>%
  ggplot(aes(as.factor(year), total)) + 
  geom_boxplot() + 
  geom_jitter(alpha = 0.2) + 
  labs(title = '동년별 취업자 분포', x = '연도', y = '취업자수')
employees %>%
  mutate(month = lubridate::month(employees$time)) %>%
  ggplot(aes(as.factor(month), total)) + 
  geom_boxplot() + 
  geom_jitter(alpha = 0.2) + 
  labs(title = '동월별 취업자 분포', x = '월', y = '취업자수')

employees %>%
  mutate(quarter = lubridate::quarter(employees$time)) %>%
  ggplot(aes(as.factor(quarter), total)) + 
  geom_boxplot() + 
  geom_jitter(alpha = 0.2) + 
  labs(title = '동분기별 취업자 분포', x = '분기', y = '취업자수')
covid19 %>%
  mutate(month = lubridate::month(covid19$date)) %>%
  ggplot(aes(as.factor(month), `0-9세`)) + 
  geom_boxplot() + 
  geom_jitter(alpha = 0.2) + 
  labs(title = '동월별 확진자 분포', x = '연도', y = '확진자수')
covid19 %>%
  mutate(wday = lubridate::wday(covid19$date, label = TRUE)) %>%
  ggplot(aes(as.factor(wday), `50-59세`)) + 
  geom_boxplot() + 
  geom_jitter(alpha = 0.2) + 
  labs(title = '동요일별 확진자 분포', x = '연도', y = '확진자수')


employees %>%
  timetk::plot_seasonal_diagnostics(.date_var = time, .value = total, .title = '전체 취업자의 주기별 Plot')

%>%
  layout(
    xaxis = list(tickfont = list(size = 20)), 
    yaxis = list(tickfont = list(size = 20)), 
    title = list(font = list(size = 20)))


covid19 %>% 
  timetk::plot_seasonal_diagnostics(.date_var = date, .value = `0-9세`, .title = '코로나 확진자(0-9세)의 주기별 Plot')
