#Importing libraries
library(tidyverse)
library(readr)

#Cleaning the environment
rm(list = ls())

#setting working directory
setwd("C:/Users/Eduardo Endo/Documents/r_scripts/git_projects/kaggle-real-state-sales")

real_estate_sales <- read_csv("dataset/real-estate-sales.csv") %>% 
  janitor::clean_names()

#-------------------------------------------------------------------------------
# EXPLORING THE PROPERTY TYPE COLUMN ---
#-------------------------------------------------------------------------------

#This part shows that the property type categorization has changed along the years
#an maybe the property_type is level 1 and the residential_type is level 2
df_plot <- real_estate_sales %>% 
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
real_estate_sales_treated <- real_estate_sales %>%
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

ggplot(real_estate_sales_treated %>% 
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
df_plot <- real_estate_sales_treated %>%
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
  geom_text(aes(x = count, label = percentage), hjust = -0.2) +
  scale_x_continuous(expand = c(0.2, 1.5)) +
  theme_bw() +
  theme(axis.text.y = element_text(size=12))

ggsave('charts/eda/residential_types.png', 
       height = 10,
       width = 10,
       units = 'cm')

#Treating the residential type column
aux <- real_estate_sales_treated %>% 
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

#-------------------------------------------------------------------------------
# EXPLORING THE TOWN COLUMN ----
#-------------------------------------------------------------------------------
town_count <- real_estate_sales_treated %>% 
  group_by(list_year) %>%
  count(town) %>% 
  arrange(desc(n)) %>% 
  add_tally(n) %>% 
  mutate(percentage = n/nn*100,
         percentage = round(percentage, 1)) %>% 
  ungroup()

#Top3 cities with most observations
ggplot(town_count %>% 
         filter(town %in% c('Bridgeport', 'Waterbury', 'Stamford')), 
       aes(x=list_year, y=n, color=town)) +
  geom_line() +
  geom_point(aes(shape=town)) +
  theme_bw()
ggsave('charts/eda/number_of_sales_top3_cities.png')

#Real estate sales over the) years
ggplot(real_estate_sales_treated %>% count(list_year), 
       aes(x=list_year, y=n)) +
  geom_line() +
  theme_bw()
ggsave('charts/eda/total_number_of_sales.png')

ggplot(real_estate_sales_treated %>% 
         count(town) %>% 
         add_tally(n) %>% 
         mutate(percentage = n/nn*100,
                percentage = round(percentage, 1)),
       aes(x = n, y = fct_reorder(town, n, max))) +
  geom_col() +
  geom_text(aes(x = n, label = paste0(percentage, '%')), hjust = -0.5, size = 3) +
  labs(y = 'town') +
  theme(axis.text.y = element_text(size=4)) 
ggsave('charts/eda/number_of_sales_per_town.png')

#-------------------------------------------------------------------------------
# TREATING THE OUTLIERS ----
#-------------------------------------------------------------------------------

#removing the outliers 
q <- quantile(real_estate_sales$assessed_value, c(0.25, 0.75))

threshold <- 1.5
iqr <- IQR(real_estate_sales$assessed_value)
lower_bound <- q[1] - threshold * iqr
upper_bound <- q[2] + threshold * iqr

filtered_real_estate_sales <- real_estate_sales_treated %>% 
  filter(assessed_value >= lower_bound, assessed_value <= upper_bound)

aux <- filtered_real_estate_sales %>% 
  count(town)

ggplot(filtered_real_estate_sales %>% count(list_year), 
       aes(x=list_year, y=n)) +
  geom_line() +
  theme_bw()

ggplot(filtered_real_estate_sales, aes(x=assessed_value, y=as.factor(list_year))) +
  geom_boxplot(outlier.shape = NA)


#### checking the resulted distributions ---------------------------------------
ggplot(filtered_real_estate_sales, aes(x = assessed_value)) +
  geom_histogram(bins = 300)


#Assessed values
ggplot(filtered_real_estate_sales %>% 
         filter(assessed_value > 0), aes(x = assessed_value)) +
  geom_histogram(bins = 300) +
  theme_bw()
ggsave('charts/eda/assessed_value_distribution.png')

#Sales ratio
ggplot(filtered_real_estate_sales, aes(x = sales_ratio)) + 
  geom_histogram(bins = 100) +
  xlim(0, 1)

#Sales value
ggplot(filtered_real_estate_sales, aes(x = sale_amount)) +
  # geom_histogram(bins = 100, alpha = 0.3) +
  geom_density() +
  xlim(0, 1e6)


#Calculating the z-score
mean_assessed_value <- mean(amostra, na.rm = TRUE)
sd_assessed_values <- sd(amostra, na.rm = TRUE)
z_score <- (amostra - mean_assessed_value)/sd_assessed_values

amostra <- sample(filtered_real_estate_sales$assessed_value, size=500)
qqnorm(z_score)
abline(a=0, b=1, col='grey')

#QQ chart for normal sample
norm_samp <- rnorm(500)
qqnorm(norm_samp)
abline(a=0, b=1, col='grey')

poisson <- data.frame(poisson=rpois(10000, 10), exponencial=rexp(10000, 10))

ggplot(poisson, aes(x = exponencial)) +
  geom_histogram(bins = 25)

# Testing the TLC theorem ------------------------------------------------------
iterations <- 1000
sample_size <- 200

histogram_of_means <- function(iterations, sample_size){
  mean_list <- list()
  
  for (i in 1:iterations){
    n_sample <- sample(filtered_real_estate_sales$assessed_value, sample_size)
    sample_mean <- mean(n_sample)
    mean_list <- c(mean_list, sample_mean)
  }
  
  df_mean <- tibble(means = mean_list) %>% 
    mutate(means = as.numeric(means))
  
  
  ggplot(df_mean, aes(x = means)) +
    geom_histogram(bins=100)
  
}

# 
# histogram_of_means(100, 1000)
# ggsave('charts/eda/means_histogram_100_iterations.png')
# histogram_of_means(1000, 1000)
# ggsave('charts/eda/means_histogram_1000_iterations.png')
# histogram_of_means(10000, 1000)
# ggsave('charts/eda/means_histogram_10000_iterations.png')
# histogram_of_means(50000, 1000)
# ggsave('charts/eda/means_histogram_50000_iterations.png')
# histogram_of_means(70000, 1000)
# ggsave('charts/eda/means_histogram_70000_iterations.png')



histogram_of_means(1000, 200)
ggsave('charts/eda/means_histogram_200_sample_size.png')
histogram_of_means(1000, 500)
ggsave('charts/eda/means_histogram_500_sample_size.png')
histogram_of_means(1000, 1000)
ggsave('charts/eda/means_histogram_1000_sample_size.png')
histogram_of_means(1000, 15000)
ggsave('charts/eda/means_histogram_15000_sample_size.png')
histogram_of_means(1000, 40000)
ggsave('charts/eda/means_histogram_40000_sample_size.png')










