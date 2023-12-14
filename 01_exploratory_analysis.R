#Importing libraries
library(tidyverse)
library(readr)

#setting working directory
setwd("C:/Users/Eduardo Endo/Documents/r_scripts/git_projects/kaggle-real-state-sales")

real_state_sales <- read_csv("dataset/real-state-sales.csv") %>% 
  janitor::clean_names()


real_state_sales %>% 
  count(town)


#This part shows that the property type categorization has changed along the years
#an maybe the property_type is level 1 and the residential_type is level 2
df_plot <- real_state_sales %>% 
  # filter(property_type == 'Commercial') %>% 
  mutate(list_year = as.factor(list_year)) %>% 
  group_by(list_year, property_type) %>% 
  summarise(total = n()) %>% 
  ungroup()
  
  
ggplot(df_plot, aes(x = total, y = list_year)) +
  geom_col() + 
  facet_wrap(.~property_type) +
  theme_bw()

ggsave('charts/eda/property_type_changes_through_the_years.png', 
       height = 15,
       width = 15,
       units = 'cm')
