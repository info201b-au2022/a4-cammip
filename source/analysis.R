library(tidyverse)
library(dplyr)
library(ggplot2)

incarceration_trends <- read.csv(
  file = "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv",
  stringsAsFactors = F
)

source("../source/a4-helpers.R")

## Section 2  ---- 
#----------------------------------------------------------------------------#

#Which race has the highest jail proportion as of the most recent year?
#Which state has the highest jail proportion of the race found in the last question?
#What county in Washington state has the highest jail population?

highest_prop_df <- incarceration_trends %>%
  select(year, state, aapi_pop_15to64, black_pop_15to64, latinx_pop_15to64,
         native_pop_15to64, white_pop_15to64, aapi_jail_pop, black_jail_pop,
         latinx_jail_pop, native_jail_pop, white_jail_pop) %>%
  filter(year == 2018) %>%
  group_by(state) %>%
  summarize(
    aapi_pop = sum(aapi_pop_15to64),
    black_pop = sum(black_pop_15to64),
    latinx_pop = sum(latinx_pop_15to64),
    native_pop = sum(native_pop_15to64),
    white_pop = sum(white_pop_15to64),
    aapi_jail_pop = sum(aapi_jail_pop, na.rm = T),
    black_jail_pop = sum(black_jail_pop, na.rm = T),
    latinx_jail_pop = sum(latinx_jail_pop, na.rm = T),
    native_jail_pop = sum(native_jail_pop, na.rm = T),
    white_jail_pop = sum(white_jail_pop, na.rm = T),
  ) %>%
  mutate(aapi_prop = aapi_jail_pop / aapi_pop,
         black_prop = black_jail_pop / black_pop,
         latinx_prop = latinx_jail_pop / latinx_pop,
         native_prop = native_jail_pop / native_pop,
         white_prop = white_jail_pop / white_pop)
#This dataframe contains the jail proportion of each race in every state

highest_race <- function () {
  highest_race_prop <- highest_prop_df %>%
  summarize(aapi_prop = max(aapi_prop),
            black_prop = max(black_prop),
            latinx_prop = max(latinx_prop),
            native_prop = max(native_prop),
            white_prop = max(white_prop)) %>%
  gather(key = "race", value = "proportion", 1:5) %>%
  filter(proportion == max(proportion)) %>%
  pull(race)
  return(highest_race_prop)
} #This function finds the race with the highest jail proportion 

highest_state <- function() {
  state <- highest_prop_df %>%
  select(state, highest_race()) %>%
  arrange(desc(state)) %>%
  filter(row_number() == 1) %>%
  pull(state)
  return(state)
} #This function finds the state with the highest jail proportion 
#of the race found in the function above

wa_highest_county <- function() {
  wa_counties <- incarceration_trends %>%
    filter(year == 2018, state == "WA") %>%
    select(county_name, total_jail_pop) %>%
    filter(total_jail_pop == max(total_jail_pop)) %>%
    pull(county_name)
  return(wa_counties)
} #This function finds the county in WA with the highest jail population

#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
get_year_jail_pop <- function() {
  pop_df <- incarceration_trends %>%
    select(year, total_jail_pop) %>%
    group_by(year) %>%
    summarize(total_pop = sum(total_jail_pop, na.rm = T))
  return(pop_df) 
}

plot_jail_pop_for_us <- function()  {
  data <- get_year_jail_pop()
  chart <- ggplot(data = data) +
    geom_col(mapping = aes(x = year, y = total_pop))
  return(chart)
} 
plot_jail_pop_for_us()
#----------------------------------------------------------------------------#

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 

get_jail_pop_by_states <- function(states) {
  population_states <- incarceration_trends %>%
    select(year, state, total_jail_pop) %>%
    group_by(year, state) %>%
    summarize(jail_population = sum(total_jail_pop, na.rm = T))
  
  population <- filter(population_states, state == states)
  return(population)
  }

plot_jail_pop_by_states <- function(states) {
  ggplot(data = get_jail_pop_by_states(states)) +
    geom_line (
      mapping = aes(x = year, y = jail_population, group = states, color = states)
)
}
plot_jail_pop_by_states(c("WA"))
#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
get_data <- function(){
  new_df <- incarceration_trends %>% 
    select(year, black_pop_15to64, white_pop_15to64,
           region, black_jail_pop, white_jail_pop) %>%
    group_by(year, region) %>%
    summarize(black_prop = sum(black_pop_15to64, na.rm = T),
              white_prop = sum(white_pop_15to64, na.rm = T),
              white = sum(white_jail_pop, na.rm = T),
              black = sum(black_jail_pop, na.rm = T)) %>%
    filter(year == "2018") %>%
    mutate(black_prop = black / black_prop,
           white_prop = white / white_prop) %>%
    gather(key = "race", value = "proportion", 3:4)
  return(new_df)
}

plot_prop_data <- function(){
  ggplot(data = get_data()) +
    geom_col(aes(x = region, y = proportion, fill = race, color = race),
             position = position_dodge()) }
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
get_state_data <- function() {
  #get data
}

plot_state_data <- function() {
  #plot
}

wa_trends <- incarceration_trends %>%
  select(year, fips, state, county_name, black_pop_15to64, black_jail_pop) %>%
  filter(state == "WA", year == 2018)
View(wa_trends)

state_shape <- map_data("state")

ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group),
    color = "white", # show state outlines
    size = .1        # thinly stroked
) + coord_map()


#----------------------------------------------------------------------------#

