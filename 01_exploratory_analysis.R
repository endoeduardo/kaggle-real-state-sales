#Importing libraries
library(tidyverse)
library(readr)

#setting working directory
setwd("C:/Users/Eduardo Endo/Documents/r_scripts/git_projects/kaggle-real-state-sales")

real_state_sales <- read_csv("dataset/real-state-sales.csv") %>% 
  janitor::clean_names()

dummy <- real_state_sales %>% 
  count(list_year)
