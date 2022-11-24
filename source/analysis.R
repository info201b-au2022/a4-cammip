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
#What county in the state found above has the highest jail population?

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

highest_county <- function() {
  wy <- incarceration_trends %>%
    filter(year == 2018, state == highest_state()) %>%
    select(state, county_name, black_pop_15to64, black_jail_pop) %>%
    mutate(prop = black_jail_pop / black_pop_15to64) %>%
    filter(prop == max(prop)) %>%
    pull(county_name)
  return(wy)
} #This function finds the county in WY with the highest jail population

#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
get_year_jail_pop <- function() {
  pop_df <- incarceration_trends %>%
    select(year, total_jail_pop) %>%
    group_by(year) %>%
    summarize(total_pop = sum(total_jail_pop, na.rm = T))
  return(pop_df) 
} #Data wrangling function

plot_jail_pop_for_us <- function()  {
  data <- get_year_jail_pop()
  chart <- ggplot(data = data) +
    geom_col(mapping = aes(x = year, y = total_pop)) +
    labs(caption = "Growth of the U.S. prison population from 1970 to 2018")
  return(chart)
} #Plotting function

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
  } #Data wrangling function for line chart

plot_jail_pop_by_states <- function(states) {
  ggplot(data = get_jail_pop_by_states(states)) +
    geom_line (
      mapping = aes(x = year, y = jail_population, group = states, color = states)) +
    labs(caption = "Growth of Prison Population by State")
} #Plotting function
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
} #Data wrangling function

plot_prop_data <- function(){
  ggplot(data = get_data()) +
    geom_col(aes(x = region, y = proportion, fill = race, color = race),
             position = position_dodge()) +
    labs(caption = "Comparison between black jail proportion and white jail proportion")
} #Plotting function

#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
wy_df <- read.csv(file = "https://raw.githubusercontent.com/info201b-au2022/a4-cammip/main/source/wy_trends.csv",
                  stringsAsFactors = F)
wy_df <- wy_df %>%
  rename(county_name = X_name)

get_state_data <- function() {
  counties_shape <- map_data("county") %>%
    rename(state = "region", county_name = "subregion") %>%
    filter(state == "wyoming") %>%
    select(-state) %>%
    left_join(wy_df, by = "county_name")
  return(counties_shape)
} #Data wrangling function for map

plot_wy_data <- function() {
  ggplot(get_state_data()) +
    geom_polygon(
      mapping = aes(x = long, y = lat, group = group, fill = black_prop),
      color = "white",
      linewidth = .1
    ) + coord_map() +
    scale_fill_continuous(low = "132B43", high = "Red") +
    labs(fill = "Proportion") +
    labs(caption = "Comparison of incarceration proportion of Black individuals in Wyoming")
} #Plotting function creates map of Wyoming
#----------------------------------------------------------------------------#

