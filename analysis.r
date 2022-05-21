library("tidyverse")

incarceration_data <- read.csv(url('https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv'))

#count of rows and columns
row_count <- nrow(incarceration_data)
col_count <- ncol(incarceration_data)

#narrow down dataset.
narrow_data <- group_by(filter(
  select(incarceration_data, state, year, black_prison_pop_rate,
                     white_prison_pop_rate), year >= 2010), state)
narrow_data <- narrow_data %>%
  group_by(state, year) %>%
  summarise(black_pop_rate_mean = mean(black_prison_pop_rate,na.rm=TRUE), 
            white_pop_rate_mean = mean(white_prison_pop_rate,na.rm=TRUE)) %>%
  ungroup()
  
#new count of rows and columns.
row_count_new <- nrow(narrow_data)
col_count_new <- ncol(narrow_data)

#table with total rates
state_total_rates_mean <- group_by(narrow_data, state) %>%
  summarise( black_pop_rate_mean = mean(black_pop_rate_mean,na.rm=TRUE), 
             white_pop_rate_mean = mean(white_pop_rate_mean,na.rm=TRUE))

#overall mean black and white prison rate.
mean_black_rate <- mean(narrow_data$black_pop_rate_mean,na.rm=TRUE)
mean_white_rate <- mean(narrow_data$white_pop_rate_mean,na.rm=TRUE)

#which state has the highest black prison population rate.
state_highest_black_rate <- select(filter(
  state_total_rates_mean, black_pop_rate_mean == max(
    state_total_rates_mean$black_pop_rate_mean, na.rm=TRUE)),
  state, black_pop_rate_mean)

#which state has the highest white prison population rate.
state_highest_white_rate <- select(filter(
  state_total_rates_mean, white_pop_rate_mean == max(
    state_total_rates_mean$white_pop_rate_mean, na.rm=TRUE)), 
  state, white_pop_rate_mean)

#what is the discrepancy between black population rate and white population 
#rate in each state.
discrepancy_black_white <- mutate(state_total_rates_mean, difference = 
            black_pop_rate_mean - white_pop_rate_mean)

#Which state had the highest discrepancy.
state_highest_discrepancy <- select(
  filter(
  discrepancy_black_white, difference == max(
    discrepancy_black_white$difference,na.rm=TRUE)), state, difference)

#table tracking prison rates by state for Black people in the West region of 
#continental US.
west_black_rate <- incarceration_data %>%
  filter( region == 'West') %>%
  filter( year >= 2010) %>%
  filter( year <= 2016) %>%
  filter( state != 'AK') %>%
  filter( state != 'HI') %>% 
  #Oregon is incomplete
  filter (state != 'OR') %>%
  select(state,year,black_prison_pop_rate) %>%
  group_by(state, year) %>%
  summarise(black_pop_rate_mean = mean(black_prison_pop_rate,na.rm=TRUE))
  
scatter <- west_black_rate %>%
  ggplot( aes(x=year,y=black_pop_rate_mean,color=state)) +
  xlab('Year') +
  ylab('Black prison rate per 100,000') +
  ggtitle("Black Prison Rate for West Coast") +
  geom_line() +
  geom_point()
print(scatter)
