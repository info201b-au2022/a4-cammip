library(tidyverse)
library(dplyr)
library(ggplot2)

incarceration_trends <- read.csv(
  file = "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv",
  stringsAsFactors = F
)

View(incarceration_trends)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#

get_year_jail_pop <- function() {
  pop_df <- incarceration_trends %>%
    select(year, total_jail_pop) %>%
    group_by(year) %>%
    summarize(total_pop = sum(total_jail_pop, na.rm = T))
  return(pop_df) 
}

# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function()  {
  data <- get_year_jail_pop()
  chart <- ggplot(data = data) +
    geom_col(mapping = aes(x = year, y = total_pop))
  return(chart)
} 
plot_jail_pop_for_us()

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas

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
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
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
             position = position_dodge())
}

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 


