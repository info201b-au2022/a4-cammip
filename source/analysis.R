library(tidyverse)

incarceration_trends <- read.csv(
  file = "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv",
  stringsAsFactors = F
)

View(incarceration_trends)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>

library(dplyr)
library(ggplot2)

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
  #cleans data frame to combine state data
  
  population <- filter(population_states, state == states)
  #filters data frame based on function parameter
  return(population)
}

plot_jail_pop_by_states <- function(states) {
  ggplot(data = get_jail_pop_by_states(states)) +
    #calls function to get the data used for the plot
    geom_line (
      mapping = aes(x = year, y = jail_population, color = states)
)
}
plot_jail_pop_by_states(c("AZ", "WA"))

#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 


