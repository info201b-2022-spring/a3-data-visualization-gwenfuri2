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


black_white_incarceration <- incarceration_trends %>% 
select(year, black_jail_pop, black_prison_pop, white_jail_pop, white_prison_pop) %>%
replace(is.na(.), 0) %>% 
mutate(black_sum = black_jail_pop + black_prison_pop, white_sum = white_jail_pop + white_prison_pop) %>% 
group_by(year) %>%
summarise(black_sum = sum(black_sum), white_sum = sum(white_sum))

variable_comparison <- plot_ly(
  data = black_white_incarceration,
  x = ~year,
  y = ~black_sum,
  name = "Black",
  type = 'scatter',
  mode = 'lines',
  width = "800") %>%
  add_trace(
  y = ~white_sum,
  name = "White",
  mode = 'lines') %>% 
  layout(
    title = "Black vs. White Total Incarceration Count over the Years",
    xaxis = list(title = "Year"),
    yaxis = list(title = "Total Incarceration Number")
    )


state_black_white_ratio <- incarceration_trends %>% 
select(state, black_jail_pop, black_prison_pop, white_jail_pop, white_prison_pop) %>%
replace(is.na(.), 0) %>% 
group_by(state) %>% 
mutate(black_sum = black_jail_pop + black_prison_pop, 
       white_sum = white_jail_pop + white_prison_pop) %>% 
summarise(black_sum = sum(black_sum), white_sum = sum(white_sum)) %>% 
mutate(rate = black_sum/white_sum) %>% 
select(-black_sum, -white_sum) %>% 
arrange(-rate) %>% 
slice(-c(1))
      
      
      
      
state_shape <- map_data("state") %>%
rename(state = region)

state_shape <- state_shape %>%
mutate(state = state2abbr(state))

state_shape <- state_shape %>%
left_join(state_black_white_ratio, by = "state")

map <- ggplot(state_shape) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = rate),
      color = "white",
      size = .1 
    ) +
  coord_map() + 
  scale_fill_continuous(low = "#132B43", high = "Red") +
labs(fill = "Ratio") +
ggtitle("Black-to-White Incarceration Ratio") +
theme_bw() +
theme(
  axis.line = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank(), 
  axis.title = element_blank(),
  plot.background = element_blank(), 
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), 
  panel.border = element_blank() 
  
)

  
  
  
  
  
  
  
  
  
  







