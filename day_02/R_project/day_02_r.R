# ----------
# Day 2
# ----------

# Problem 1 ---------------------

# import necessary libraries
library(readr)
library(tidyverse)
library(dplyr)

# make an object from an example file
example <- read_delim("~/OneDrive - Universitetet i Oslo/learnings/advent_of_code_2021/day_02/example.txt", " ", col_names = c("direction","units"))

forward <- example %>% 
  filter(direction == "forward") %>% 
  select("units") %>% 
  sum()

down <- example %>%
  filter(direction == "down") %>% 
  select("units") %>% 
  sum()

up <- example %>%
  filter(direction == "up") %>% 
  select("units") %>% 
  sum()

depth <- down - up

# try with the actual input
input <- read_delim("~/OneDrive - Universitetet i Oslo/learnings/advent_of_code_2021/day_02/input.txt", " ", col_names = c("direction","units"))

forward <- input %>% 
  filter(direction == "forward") %>% 
  select("units") %>% 
  sum()

down <- input %>%
  filter(direction == "down") %>% 
  select("units") %>% 
  sum()

up <- input %>%
  filter(direction == "up") %>% 
  select("units") %>% 
  sum()

depth <- down - up

answer <- forward * depth

# Problem 2 ---------------------

# Try first with example data
example$aim <- 0
example$horizontal <- 0
example$depth <- 0

obs <- nrow(example)
example$aim[1] <- ifelse(example$direction[1] == "forward", 0, 
                         ifelse(example$direction[1] == "down", example$units[1], -example$units[1]))
for(i in 2:obs){example$aim[i] <- ifelse(example$direction[i] == "forward", example$aim[i-1]+0,
                                         ifelse(example$direction[i] == "down", example$aim[i-1]+example$units[i], example$aim[i-1]-example$units[i]))}
example$horizontal[1] <- ifelse(example$direction[1] == "forward", example$units[1], 0)
for(i in 2:obs){example$horizontal[i] <- ifelse(example$direction[i] == "forward", example$horizontal[i-1]+example$units[i],example$horizontal[i-1])}
# no matter what the first observation is, either aim or units for forward is 0 at the first row, so the following line of code is unnecessary.
# example$depth[1] <- ifelse(example$direction[1] == "forward", example$units[1], 0)
for(i in 2:obs){example$depth[i] <- ifelse(example$direction[i] == "forward", example$depth[i-1]+example$units[i]*example$aim[i],example$depth[i-1])}
print(example$horizontal[obs]*example$depth[obs])

# try with the actual input

input$aim <- 0
input$horizontal <- 0
input$depth <- 0

obs_input <- nrow(input)
input$aim[1] <- ifelse(input$direction[1] == "forward", 0, 
                         ifelse(input$direction[1] == "down", input$units[1], -input$units[1]))
for(i in 2:obs_input){input$aim[i] <- ifelse(input$direction[i] == "forward", input$aim[i-1]+0,
                                         ifelse(input$direction[i] == "down", input$aim[i-1]+input$units[i], input$aim[i-1]-input$units[i]))}
input$horizontal[1] <- ifelse(input$direction[1] == "forward", input$units[1], 0)
for(i in 2:obs_input){input$horizontal[i] <- ifelse(input$direction[i] == "forward", input$horizontal[i-1]+input$units[i],input$horizontal[i-1])}
for(i in 2:obs_input){input$depth[i] <- ifelse(input$direction[i] == "forward", input$depth[i-1]+input$units[i]*input$aim[i],input$depth[i-1])}
print(input$horizontal[obs_input]*input$depth[obs_input])

