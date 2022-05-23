library(dplyr)
library(maps)
library(usdata)
library(ggplot2)
library(plotly)

#Load the CSV file
incarceration_trends <-
read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")


state_highest_jail_race_ratio <- incarceration_trends %>%
select(state, white_jail_pop, black_jail_pop) %>% 
group_by(state) %>%
summarise(white_jail_pop = sum(white_jail_pop, na.rm = TRUE),
black_jail_pop = sum(black_jail_pop, na.rm = TRUE)) %>%
mutate(rate = black_jail_pop/white_jail_pop) %>%
filter(rate == max(rate, na.rm = TRUE)) %>%
pull(state)


state_highest_prison_race_ratio <- incarceration_trends %>%
select(state, white_prison_pop, black_prison_pop) %>% 
group_by(state) %>% 
summarise(white_prison_pop = sum(white_prison_pop, na.rm = TRUE),
black_prison_pop = sum(black_prison_pop, na.rm = TRUE)) %>% 
mutate(rate = black_prison_pop/white_prison_pop) %>% 
filter(rate == max(rate, na.rm = TRUE)) %>%
pull(state)


state_highest_black_incarceration <- incarceration_trends %>% 
select(state, black_jail_pop, black_prison_pop) %>% 
replace(is.na(.), 0) %>% 
mutate(sum = black_jail_pop + black_prison_pop) %>% 
group_by(state) %>% 
summarise(total_incarceration = sum(sum, na.rm = TRUE)) %>% 
filter(total_incarceration == max(total_incarceration, na.rm = TRUE)) %>% 
pull(state)


ca_black_incarceration <- incarceration_trends %>% 
select(state, black_jail_pop, black_prison_pop) %>% 
replace(is.na(.), 0) %>%
mutate(sum = black_jail_pop + black_prison_pop) %>% 
group_by(state) %>% 
summarise(total_incarceration = sum(sum, na.rm = TRUE)) %>% 
filter(total_incarceration == max(total_incarceration, na.rm = TRUE)) %>%
mutate(round = round(total_incarceration)) %>% 
pull(round)


black_jail_change_year <- incarceration_trends %>% 
select(year, state, black_jail_pop) %>% 
group_by(year) %>%
summarise(black_jail_pop = sum(black_jail_pop, na.rm = TRUE)) %>% 
summarise(
  change = abs((black_jail_pop[year == 2018] - black_jail_pop[year == 2008])/black_jail_pop[year == 2008]))%>% 
mutate(change_decimal = round(change, 2)) %>%
pull(change_decimal)


black_prison_change_year <- incarceration_trends %>%
select(year, state, black_prison_pop) %>% 
  group_by(year) %>%
  summarise(black_prison_pop = sum(black_prison_pop, na.rm = TRUE)) %>% 
  summarise(
    change = abs((black_prison_pop[year == 2016] - black_prison_pop[year == 
       2006])/black_prison_pop[year == 2006])) %>%
  mutate(change_decimal = round(change, 2)) %>% 
  pull()





top3_state_black_incarceration <- incarceration_trends %>% 
select(year, state, black_jail_pop, black_prison_pop) %>%
replace(is.na(.), 0) %>% 
mutate(sum = black_jail_pop + black_prison_pop) %>%
group_by(year, state) %>% 
summarise(total_incarceration = sum(sum, na.rm = TRUE)) %>% 
filter(state %in% c("CA", "FL","TX"))

trends_over_time <- plot_ly(
  data = top3_state_black_incarceration,
  x = ~year,
  y = ~total_incarceration,
  color = ~state,
  type = "bar",
  width = "1000") %>% 
layout(title = "Total Incarcerated Black Population in Top 3 States over the Years",
    xaxis = list(title = "Year"),yaxis = list(title = "Total Incarceration Number"))








