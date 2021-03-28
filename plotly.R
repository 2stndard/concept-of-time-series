library(plotly)
require("processx")

fig <- students %>%  
  plot_time_series(.date_var = 연도, .value = 학생수계, .smooth = FALSE, .title = 'timetk를 사용한 전체 학생수 플롯', .x_lab = '연도', .y_lab = '학생수')
plotly_IMAGE(fig, format = "svg", out_file = "output.svg")
orca(fig, 't.svg')
?toWebGL

font_add(family = "NGULIM", regular = "C:/Windows/Fonts/NGULIM.TTF")

fig <- students.all %>%
  plot_time_series(.date_var = 연도, .value = 학생수계, .color_var = 지역규모, .smooth = F, .title = 'timetk를 사용한 전체 학생수 다변량 플롯', .x_lab = '연도', .y_lab = '학생수', .interactive = FALSE) + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5 )) 

orca(fig, 't.svg')
fig <- students.all %>%  
plot_time_series(.date_var = 연도, .value = 학생수계, .color_var = 지역규모, .smooth = F, .title = 'timetk를 사용한 전체 학생수 다변량 플롯', .x_lab = '연도', .y_lab = '학생수', .interactive = FALSE) + theme(axis.text.x=element_text(angle=90,hjust=1))

fig <- students %>% select(1, 3, 4, 5) %>% 
  tidyr::gather(category, value, 2:4) %>%
  plot_time_series(.date_var = 연도, .value = value, .color_var = category, .smooth = F, .title = '연도별 유치원, 초등학교, 중학교 학생수', .x_lab = '연도', .y_lab = '학생수')

fig <- employees %>%
  plot_time_series(.date_var = time, .value = total, .smooth = F, .title = '월별 신규 취업자수', .x_lab = '시간', .y_lab = '취업자수')

fig <- covid19 %>%
  plot_time_series(.date_var = date, .value = `0-9세`, .smooth = F, .title = '일별 코로나 확진자수(0-9세)', .x_lab = '연월', .y_lab = '확진자수')
orca(fig, 't.svg')
