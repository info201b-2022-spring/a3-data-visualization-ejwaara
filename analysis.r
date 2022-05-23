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

#line graph tracking black prison rate for western states.  
scatter <- west_black_rate %>%
  ggplot( aes(x=year,y=black_pop_rate_mean,color=state)) +
  xlab('Year') +
  ylab('Black prison rate per 100,000') +
  ggtitle("Black Prison Rate for Western States") +
  geom_line() +
  geom_point()

#compare white prison rates to black prison rates nationally since 2010
national_rates <- incarceration_data %>%
  filter(year >= 2010) %>%
  filter(year <= 2016) %>%
  group_by(year) %>%
  summarise( Black = mean(
    black_prison_pop_rate,na.rm=TRUE), White = mean(
      white_prison_pop_rate,na.rm=TRUE)) 

#make long dataset
long_national_rates <- gather(
  national_rates, race, pop_rate, Black, White)


#plot the trends
plot_nat_rate <- long_national_rates %>%
  ggplot( aes(x=year,y=pop_rate,color=race)) +
  xlab('Year') +
  ylab('Prison rate per 100,000') +
  ggtitle('Average National Prison Rate') +
  geom_line() +
  geom_point()


#Make map showing difference between black prison rate and white prison rate average.


df_map <- select(discrepancy_black_white, state, difference) %>%
  filter(state != 'DC') %>%
  mutate(region = setNames(state.name, state.abb)[df_map$state])
df_map$region <- tolower(df_map$region)

#borrowed heavily from the textbook
state_shape <- map_data("state") %>%
  left_join( df_map,by='region')
map_diff <- ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = difference),
    color = "white", # show state outlines
    size = .1        # thinly stroked
  ) +
  coord_map() + # use a map-based coordinate system
  scale_fill_continuous(low = "#132B43", high = "Red") +
  labs(fill = "Difference between black and white") +
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
print(map_diff)











