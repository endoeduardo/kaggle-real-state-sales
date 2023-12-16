#Importing libraries
library(tidyverse)
library(readr)

#setting working directory
setwd("C:/Users/Eduardo Endo/Documents/r_scripts/git_projects/kaggle-real-state-sales")

real_state_sales <- read_csv("dataset/real-state-sales.csv") %>% 
  janitor::clean_names()


real_state_sales %>% 
  count(town)

#-------------------------------------------------------------------------------
# EXPLORING THE PROPERTY TYPE COLUMN ----
#-------------------------------------------------------------------------------

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
  theme_bw() +
  theme(axis.text.y = element_text(size=6))

ggsave('charts/eda/property_type_changes_through_the_years.png', 
       height = 10,
       width = 10,
       units = 'cm')

#Treating the residential type column
real_state_sales_treated <- real_state_sales %>%
        #Changing residential_type
  mutate(residential_type = ifelse(property_type %in% c('Condo',
                                                        'Single Family',
                                                        'Two Family',
                                                        'Three Family',
                                                        'Four Family'),
                                   property_type,
                                   residential_type),
        #Changing the property_type to residential
         property_type = ifelse(property_type %in% c('Condo',
                                                     'Single Family',
                                                     'Two Family',
                                                     'Three Family',
                                                     'Four Family'),
                                'Residential',
                                property_type)
         )

ggplot(real_state_sales_treated %>% 
         mutate(list_year = as.factor(list_year)) %>% 
         group_by(list_year, property_type) %>% 
         summarise(total = n()) %>% 
         ungroup(), aes(x = total, y = list_year)) +
  geom_col() + 
  facet_wrap(.~property_type) +
  theme_bw() + 
  theme(axis.text.y = element_text(size=6))

ggsave('charts/eda/property_type_changes_through_the_years_after_treatment.png', 
       height = 10,
       width = 10,
       units = 'cm')


#-------------------------------------------------------------------------------
# EXPLORING THE RESIDENTIAL TYPE COLUMN ----
#-------------------------------------------------------------------------------
df_plot <- real_state_sales_treated %>%
  group_by(residential_type) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  mutate(residential_type = fct_reorder(residential_type, count, max)) %>% 
  add_tally(count) %>%
  rename(total = n) %>% 
  mutate(percentage = count/total*100,
         percentage = round(percentage, 1),
         percentage = paste0(percentage, '%'))

ggplot(df_plot, aes(x = count, y = residential_type)) +
  geom_col() + 
  geom_text(aes(x = count, label = percentage), hjust = -0.5) +
  scale_x_continuous(expand = c(0.2, 1.5)) +
  theme_bw() +
  theme(axis.text.y = element_text(size=12))

ggsave('charts/eda/residential_types.png', 
       height = 10,
       width = 10,
       units = 'cm')

#Treating the residential type column
aux <- real_state_sales_treated %>% 
  count(list_year, residential_type) %>% 
  group_by(list_year) %>% 
  add_tally(n) %>% 
  ungroup() %>% 
  mutate(percentage = n/nn*100,
         percentage = round(percentage, 1))

ggplot(aux, aes(x = list_year, y = percentage, color = residential_type)) +
  geom_line() + 
  geom_point(aes(shape = residential_type), size = 2) +
  # geom_text(aes(x = list_year, label = paste0(percentage, '%'))) +
  theme_bw() 
  # theme(axis.text.y = element_text(size=4))

ggsave('charts/eda/residential_type_proportions_through_years.png', 
       height = 10,
       width = 10,
       units = 'cm')


















