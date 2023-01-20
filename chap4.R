######################################
###  4장 시계열 데이터 처리

###  4.1 오늘 며칠일까?: 시간 정보 추출

# lubridate 패키지 로딩
library(lubridate)

# 현재 시간을 now.date에 저장(date 클래스)
(now.date <- Sys.time())

(now.char <- as.character(Sys.time()))

paste0('오늘은 ', year(now.date), '년 ', month(now.char), '월 ', day(now.date), '일입니다')

paste0('1월 1일부터 오늘까지 ', yday(now.date), '일 지났습니다')

paste0('이번 분기 시작일부터 오늘까지 ', qday(now.date), '일 지났습니다')

paste0('오늘은 ', wday(now.date, label = T, abbr = T), '요일입니다')

paste0('지금은 ', hour(now.date), '시 ', minute(now.char), '분 ', second(now.date), '초입니다')

paste0('이번 주는 올해의 ', week(now.date), '번째 주입니다')


###  4.2 며칠 지났을까?: 시간 기간 연산

# 1980년 1월 1일부터 2021년 1월 1일까지의 날짜 수
as.Date('2021-01-01') - as.Date('1980-01-01')

# 오늘 날짜를 today에 저장
today <- today()
# 오늘부터 100일 후
today + 100

# 오늘부터 2개월 전
today - months(2)

# 오늘부터 1년 전
today - years(1)

# 1980.1.1부터 2021.12.31까지의 interval을 int에 저장
# 결과값을 보면 우리가 생각하는 형태가 아님
(int <- lubridate::interval(as.Date('1980-01-01'), as.Date('2021-12-31')))

# 연월일 형태로 interval 출력
lubridate::as.period(int)

# 경과 초 형태로 interval 출력
lubridate::as.duration(int)

# 1980.1.1부터 2021.12.31까지의 interval 클래스를 int1에 저장
int1 <- '1980-01-01' %--% '2021-12-31'
# 연월일 형태로 interval 출력
lubridate::as.period(int1)

# 2020년은 윤년
leap_year(2020)

# 2020-01-01부터 기간상 1년 후(period)는 우리의 상식대로 2021-01-01
as.Date('2020-01-01') + years(1)

# 2020-01-01부터 시간상 1년 후(duration)는 2020년은 윤년이므로 2020년은 366일임.그래서 365일 후인 2020-12-31이 표기됨
as.Date('2020-01-01') + dyears(1)

# 2020-02-01부터 한 달 후(period)는 2020년 3월 1일
as.Date('2020-02-01') + months(1)

# 2020-02-01부터 한 달 후(duration)는 30일 후인 2020년 3월 2일
as.Date('2020-02-01') + dmonths(1)

# 2021-02-01부터 한 달 후(period)는 2021년 3월 1일
as.Date('2021-02-01') + months(1)

# 2020-01-01부터 한 달 후(duration)는 30일 후인 3월 2일
as.Date('2021-02-01') + dmonths(1)


###  4.3 이번 주 마지막 날은 며칠일까?: 시간 반올림

(x <- as.Date("2020-11-12 13:45:40"))

# 주 단위로 반올림
round_date(x, "week")

# 주 단위로 내림
floor_date(x, "week")

# 주 단위로 올림
ceiling_date(x, "week")

# 월 단위로 반올림
round_date(x, "month")

# 월 단위로 내림
floor_date(x, "month")

# 월 단위로 올림
ceiling_date(x, "month")

# 연 단위로 반올림
round_date(x, "year")

# 연 단위로 내림
floor_date(x, "year")

# 연 단위로 올림
ceiling_date(x, "year")

# 말일을 구하는 코드
days_in_month(as.Date('2012-03-01'))


###  4.4 주간, 월간 데이터 합계, 평균은?: 시간 그루핑

library(dplyr)

library(ggplot2)

# 월별 취업자수를 연별 취업자수로 그루핑
(employees.by.year <- employees %>%
    mutate(year = year(time)) %>%
    group_by(year) %>%
    summarise(total.year = sum(total),
              employees.edu = sum(employees.edu)))

employees.by.year %>%
  ggplot(aes(as.factor(year), total.year)) +
  geom_line(aes(group = 1)) +
  geom_text(aes(label = scales::number(total.year, big.mark = ',')), size = 3, vjust = 1.5) +
  labs(title = '연도별 취업자수', x = '연도', y = '취업자수') +
  scale_y_continuous(labels = scales::number_format(big.mark = ','))

# 일별 평균 확진자수를 산출
(mean.covid19.by.age <- covid19 %>%
    mutate(yearmon = yearmonth(date)) %>%
    group_by(yearmon) %>%
    summarise(`01대` = mean(`0-9세`),
              `10대` = mean(`10-19세`),
              `20대` = mean(`20-29세`),
              `30대` = mean(`30-39세`),
              `40대` = mean(`40-49세`),
              `50대` = mean(`50-59세`),
              `60대` = mean(`60-69세`),
              `70대` = mean(`70-79세`),
              `80대` = mean(`80세 이상`)))

mean.covid19.by.age %>%
  tidyr::gather(category, value, 2:10) %>%
  ggplot(aes(x = yearmon, y = value)) +
  geom_line(aes(group = category, color = category)) +
  labs(title = '월간 평균 코로나 확진자수', x = '시간', y = '평균 확진자', color = '세대')

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
  labs(title = '주간 평균 코로나 확진자수', x = '월', y = '평균 확진자', color = '세대')

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
  labs(title = '주간 평균 코로나 확진자수', x = '월', y = '평균 확진자', color = '세대')

employees %>%
  summarise_by_time(.date_var = time, .by = 'month',
                    total.year = sum(total),
                    employees.edu = sum(employees.edu)) %>%
  head(10)

employees.tsibble%>%
  index_by(yearqtr = ~ yearquarter(.)) %>%
  summarise(sum.qtrly = sum(total)) %>%
  head(10)

covid19.tsibble[, c(1,3)]%>%
  index_by(yearweek = ~ yearweek(.)) %>%
  summarise(sum.weekly = sum(`0-9세`)) %>%
  head(10)

covid19.tsibble[, c(1,3)]%>%
  index_by(twomonth = ~ lubridate::floor_date(., "2 month")) %>%
  summarise(sum.2month = sum(`0-9세`)) %>%
  head(10)

covid19.tsibble[, c(1,3)]%>%
  index_by(fourday = ~ lubridate::floor_date(., "4 day")) %>%
  summarise(sum.4days = sum(`0-9세`)) %>%
  head(10)

library(xts)

apply.quarterly(employees.xts, sum) %>% head(10)

apply.yearly(employees.xts, sum) %>% plot.xts()

apply.monthly(covid19.xts[,1], sum) %>% plot.xts(main = '월별 0-9세 코로나 확진자수')

apply.quarterly(covid19.xts[,1], sum) %>% plot.xts(main = '분기별 0-9세 코로나 확진자수')


###  4.5 주식 시가, 고가, 저가, 종가는 어떻게 구할까?: OHLC

as_tbl_time(covid19, index = date) %>%
  collapse_by('weekly') %>%
  group_by(date) %>%
  summarise(Open = first(`0-9세`),
            High = max(`0-9세`),
            Low = min(`0-9세`),
            Close = last(`0-9세`)) %>%
  head(10)

to.period(covid19.xts, method = 'months', OHLC = TRUE)


###  4.6 3일 평균, 5일 합계는?: 시간 롤링

library(zoo)

employees %>%
  mutate(ma3 = rollmean(total, k = 3, fill = NA),
         sum3 = rollapply(total, 3, sum, fill = NA)) %>%
  select(time, total, ma3, sum3) %>%
  ggplot(aes(x = time)) +
  geom_line(aes(y = total, group = 1, color = '3개월 합계')) +
  geom_line(aes(y = ma3, group = 1, color = '3개월 평균')) +
  labs(y = '취업자수', x = '연도') +
  scale_color_manual(values = c('3개월 합계' = 'red', '3개월 평균' = 'blue'))

ma3 <- slidify(mean, .period = 3, .align = "center")

sum3 <- slidify(sum, .period = 3, .align = "center")

class(ma3)

class(sum3)

employees %>%
  mutate(ma3 = ma3(total), sum3 = sum3(total)) %>%
  select(time, total, ma3, sum3) %>%
  ggplot(aes(x = time)) +
  geom_line(aes(y = total, group = 1, color = '3개월 합계')) +
  geom_line(aes(y = ma3, group = 1, color = '3개월 평균')) +
  labs(y = '취업자수', x = '연도') +
  scale_color_manual(' ', values = c('3개월 합계' = 'red', '3개월 평균' = 'blue'))

rollapply(employees.xts, width = 3, FUN = mean) %>%
  head(10)


###  4.7 지난 달 데이터는?: 필터링

covid19 %>%
  filter(date >= as.Date('2020-10-01') & date <= as.Date('2020-10-10'))

covid19 %>%
  filter(between(date, as.Date('2021-01-01'), as.Date('2021-01-15')))

employees %>%
  filter(year(time) == 2019 & month(time) == 5)

# 매월 3일부터 7일까지 필터링
covid19 %>%
  filter(between(day(date), 3, 7)) %>%
  head(15)

covid19 %>%
  filter_by_time(.date_var = date, .start = '2020-10-01', .end = '2020-10-05')

covid19 %>%
  filter(`0-9세` != 0) %>%
  filter_period(.date_var = date, .period = '1 month', `0-9세` == max(`0-9세`)) %>%
  head(10)

# 2020-10-02에 해당하는 데이터 필터링
covid19.xts['2020-10-02']

# 2020-10-01에서부터 2020-10-10까지 데이터 필터링
covid19.xts['2020-10-01/2020-10-10']

# 2021-02-05일부터 끝까지 데이터 필터링
covid19.xts['2021-02-05/']

# 처음부터 2020-04-11까지의 필터링
covid19.xts['/2020-04-11']


###  4.8 월별, 분기별, 연별 증감량

students_lag <- cbind(연도 = students$연도,
                        학생수계 = students$학생수계,
                        전년 = students %>%
                          lag(1)%>%
                          select(학생수계) %>%
                          rename(전년 = 학생수계)
) %>%
  mutate(증감 = 학생수계 - 전년, 증감률 = round((학생수계/전년)-1, 3) * 100)

students_lag %>% head()

students_lag %>%
  ggplot(aes(as.factor(year(연도)), 증감)) +
  geom_line(aes(group = 1)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = scales::comma(증감)), vjust = 1, size = 3) +
  # ggrepel::geom_text_repel() 함수로 숫자들이 겹치지 않게 시각화
  labs(title = '전년 대비 전체 학생수 증감 추이', x = '연도', y = '학생수 증감량') +
  scale_y_continuous(labels = scales::number_format(big.mark = ',')) +
  theme(axis.text.x=element_text(angle=90,hjust=1))

students.tsibble%>% select(1, 2) %>%
  mutate(증감 = difference(.$학생수계, lag = 1)) %>%
  mutate(증감률 = round((증감/학생수계), 3) * 100) %>% head(10)

employees%>%
  mutate(증감 = difference(employees.tsibble$total, lag = 1)) %>%
  mutate(증감률 = round((증감/total), 3) * 100) %>% select(1, 2, 4, 5) %>% head(10)

students.xts$증감 <- diff(students.xts[,2])

students.xts$증감률 <- round((students.xts$증감/students.xts$학생수계), 3) * 100

students.xts[, c('유치원', '증감', '증감률')] %>% head(10)

employees.xts$증감 <- diff(employees.xts$total)

employees.xts$증감률 <- round((employees.xts$증감/employees.xts$total), 3) * 100

employees.xts[, c('total', '증감', '증감률')] %>% head(10)

plot.xts(employees.xts[, c('증감률')], main = '전월 대비 전체 취업자 증감률')


###  4.9 월 비중 백분율, 연 비중 백분율

employees %>%
  group_by(year(time)) %>%
  mutate(sum.by.year = sum(total)) %>%
  ungroup() %>%
  mutate(rate.by.year = round(total/sum.by.year, 3) * 100) %>%
  head(15)

covid19 %>%
  group_by(yearmonth(date)) %>%
  mutate(sum.by.month = sum(`0-9세`)) %>%
  ungroup() %>%
  mutate(rate.by.month = round(`0-9세`/sum.by.month, 3) * 100) %>%
  select(date, `0-9세`, sum.by.month, rate.by.month)

covid19 %>%
  group_by(year(date), month(date), week(date)) %>%
  mutate(sum.by.week = sum(`0-9세`)) %>%
  ungroup() %>%
  mutate(rate.by.week = round(`0-9세`/sum.by.week, 3) * 100) %>%
  select(date, `0-9세`, sum.by.week, rate.by.week)

# 취업자수의 분기별 비율
employees.tsibble%>%
  index_by(yearqtr = ~ yearquarter(.)) %>%
  mutate(sum.qtrly = sum(total)) %>%
  mutate(rate.qtrly = total/sum.qtrly) %>%
  head(15)

employees.tsibble%>%
  index_by(yearqtr = ~ year(.)) %>%
  mutate(sum.qtrly = sum(total)) %>%
  mutate(rate.qtrly = (total/sum.qtrly)*100) %>%
  head(15)


###  4.10 월별, 분기별, 연별 누적 합계

employees %>%
  mutate(cumsum = cumsum(total)) %>%
  select(time, total, cumsum) %>%
  head(15)

# 0-9세 코로나 확진자의 누적 플롯
covid19 %>%
  mutate(cumsum = cumsum(`0-9세`)) %>%
  select(date, `0-9세`, cumsum) %>%
  ggplot(aes(date, cumsum)) +
  geom_line(aes(group = 1)) +
  labs(title = '코로나 확진자 누적 합계(0-9세)', x = '날짜', y = '누적합계') +
  scale_x_date(date_breaks = "1 month", date_labels = "%y.%m") +
  theme(axis.text.x=element_text(angle=90,hjust=1))

employees %>%
  group_by(year(time)) %>%
  mutate(cumsum.total = cumsum(total),
         cumsum.edu = cumsum(employees.edu)) %>%
  select(time, total, cumsum.total, employees.edu, cumsum.edu) %>%
  head(15)

employees.tsibble%>%
  index_by(yearqtr = ~ yearquarter(.)) %>%
  mutate(cumsum.qtrly = cumsum(total)) %>%
  select(yearqtr, cumsum.qtrly) %>%
  head(10)

covid19.tsibble[, c(1,3)]%>%
  index_by(yearweek = ~ yearweek(.)) %>%
  mutate(cumsum.weekly = cumsum(`0-9세`)) %>%
  head(10)

do.call(rbind, lapply(split(employees.xts, f = 'year'), cumsum)) %>%
  head(15)


###  4.11 동월별, 동분기별, 동년별 플롯

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
  labs(title = '동월별 확진자 분포', x = '월', y = '확진자수')

covid19 %>%
  mutate(wday = lubridate::wday(covid19$date, label = TRUE)) %>%
  ggplot(aes(as.factor(wday), `50-59세`)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2) +
  labs(title = '동요일별 확진자 분포', x = '요일', y = '확진자수')

employees %>%
  timetk::plot_seasonal_diagnostics(.date_var = time, .value = total, .title = '전체
취업자의 주기별 플롯')

covid19 %>%
  timetk::plot_seasonal_diagnostics(.date_var = date, .value = `0-9세`, .title = '코로나
확진자(0-9세)의 주기별 플롯')
