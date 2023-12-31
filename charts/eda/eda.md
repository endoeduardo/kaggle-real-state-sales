The following chart illustrates the categorization of properties into commercial, residential, and other types, beginning in the year 2020.
![Types of properties over the years](property_type_changes_through_the_years.png)

Before 2020, only residential real estate received a proper category, while commercial and other types were likely marked as NaN (Not a Number).

It is important to note that NaN values should be analyzed separately, as depicted in the chart below, which illustrates the data after treatment.

![Types of properties over the years after treatment](property_type_changes_through_the_years_after_treatment.png)

Even after treatment, it is evident that before 2006, real estate was not properly categorized. The adopted strategy involves separating the analysis into two parts: before and after 2006.

To assess the impact of residential properties, the dataset is split into two periods: before and after 2006.

![proportion of residential real estate types](residential_types.png)

![proportion of residential real estate types over the years](residential_type_proportions_through_years.png)

The charts depicting the proportion of residential types highlight how NaN values skew the data. While NaN values appear as the second most prevalent category, the majority of them were recorded before the residential category was created. The line plot indicates that the proportion of residential types does not vary significantly over time.

![Total Number of real estate sales for each town](number_of_sales_per_town.png)
The "town" category has a disproportionate number of observations that skew the data. This could potentially impact a future predictive model trained with this dataset. Ways to address or mitigate this problem need to be explored.

![Total Number of real estate sales over the years](total_number_of_sales.png)
The chart above indicates that the total number of real estate sales peaked in 2004 and hit a low point in 2008, during the American financial crisis. An area worth investigating is whether the prices exhibited a similar pattern.
