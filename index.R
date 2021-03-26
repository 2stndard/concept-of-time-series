library(fpp2)
goog200

autoplot(goog) +
  scale_y_continuous(labels = scales::dollar)+ 
  labs(title = '구글의 주가변동', subtitle = '2013.02.25부터 2017.02.13일까지', y = '종가', x = '일수', family = 'nanumgothic')

ggplot(students, aes(x = 연도, y = 학생수계)) + 
  geom_line(aes(group = 1)) + geom_point() +
  labs(x = '연도', y = '총학생수') + 
  scale_y_continuous(labels = scales::comma_format()) + 
  geom_smooth(method = 'lm') + 
  ggthemes::theme_economist()

employees %>%
  ggplot(aes(x = time, y = total)) + 
  geom_line(aes(group = 1)) +
  labs(x = '연도', y = '신규 취업자수') + 
  scale_y_continuous(labels = scales::comma_format()) +
  scale_x_date(breaks = '1 year') + 
  ggthemes::theme_economist()
