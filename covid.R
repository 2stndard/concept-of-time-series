covid19 <- read.csv('./covid19.csv', header = TRUE, na = '-', strip.white = TRUE, stringsAsFactors = TRUE)
head(covid19)
colnames(covid19) <- c('category', 'status', 'date', 'value')
nrow(covid19)

covid19 <- covid19[, c(3, 1, 2, 4)]
covid19$date <- as.Date(covid19$date, "%Y. %m. %d")
summary(covid19)
glimpse(covid19)
distinct(covid19, category)
covid19 %>% group_by(category) %>% summarise(n = n())

covid19.by.age <- covid19 %>% 
  filter(grepl('세', category)) %>% 
  filter(category != '세종')

covid19.by.age %>% distinct(category)
wide.covid19.by.age <- spread(covid19.by.age, category, value)

xts.wide.covid19.by.age <- as.xts(wide.covid19.by.age[, -c(1, 2)], order.by = wide.covid19.by.age[, 1])

autoplot(xts.wide.covid19.by.age[, 1]) +
  autolayer(xts.wide.covid19.by.age[, 2]) +
  autolayer(xts.wide.covid19.by.age[, 3]) +
  autolayer(xts.wide.covid19.by.age[, 4]) +
  autolayer(xts.wide.covid19.by.age[, 5]) +
  autolayer(xts.wide.covid19.by.age[, 6]) +
  autolayer(xts.wide.covid19.by.age[, 7]) +
  autolayer(xts.wide.covid19.by.age[, 8]) +
  autolayer(xts.wide.covid19.by.age[, 9])


plot_time_series(.data = covid19.by.age %>% filter(category %in% c('0-9세', '10-19세')), 
                 .date_var = date,
                 .value = value, 
                 .color_var = category, 
                 .smooth = FALSE, 
                 .interactive = TRUE)

