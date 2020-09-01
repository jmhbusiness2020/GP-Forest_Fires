
# Analyzing Forest Fires Data (R Fundamentals)

# Introduction

Forest fires are among one the most economically and ecologically damaging forces of nature to occur. In an attempt to mitigate the damages, scientific researches have look for many ways to combat the flames. In this project, we will be using a dataset from Portugal’s meteorological sites to better understand when they occur and what causes them. 


## Dataset

The dataset is provide by Dataquest.io and can be downloaded [here](https://archive.ics.uci.edu/ml/machine-learning-databases/forest-fires/) with the attached citation request file named forestfires.names. For more information on forest fires, you can read the [scientific reserach paper](http://www3.dsi.uminho.pt/pcortez/fires.pdf) on predicting the occurences of forest fires in Portugal by using modeling techniques.

### Quick Description of the columns

- X: X-axis spatial coordinate within the Montesinho park map: 1 to 9
- Y: Y-axis spatial coordinate within the Montesinho park map: 2 to 9
- month: Month of the year: 'jan' to 'dec'
- day: Day of the week: 'mon' to 'sun'
- FFMC: Fine Fuel Moisture Code index from the FWI system: 18.7 to 96.20
- DMC: Duff Moisture Code index from the FWI system: 1.1 to 291.3
- DC: Drought Code index from the FWI system: 7.9 to 860.6
- ISI: Initial Spread Index from the FWI system: 0.0 to 56.10
- temp: Temperature in Celsius degrees: 2.2 to 33.30
- RH: Relative humidity in percentage: 15.0 to 100
- wind: Wind speed in km/h: 0.40 to 9.40
- rain: Outside rain in mm/m2 : 0.0 to 6.4
- area: The burned area of the forest (in ha): 0.00 to 1090.84


## Goal

1. To visualize the frequency of forest fires
2. Predict any variables that might explain why this frequency exists


### Project Goal

The goal of this project is to show R Fundamentals regarding data visualization via ggplot2 package. These visualizations will not be grandiose, given the point of the project is to show basic knowledge of ggplot 2 package.


##### note after the project check the citation file in documents!!!

```{r}
#import needed packages
library(tidyverse)
```

```{r}
#import dataset via readr
forest_fires <- read_csv('forestfires.csv')
#look at the dataset
dim(forest_fires)
head(forest_fires,3)
```

```{r}
# visualize a simple graph that shows the frequency of forest fires for each month
# group by month and count the rows to get frequency
fires_by_month <- forest_fires %>%
  group_by(month) %>%
  summarize(total_fires = n())
ggplot(data = fires_by_month, aes(x = month, y = total_fires)) + 
  geom_bar(stat = 'identity') +
  labs( title = 'Number of Fires For Each Month', x = 'Month', y = 'Number of Fires')
```


```{r}
# visualize a simple graph that shows the frequency of forest fires for each day of the week
# group by day and count the total of rows to get frequency
fires_by_day <- forest_fires %>%
  group_by(day) %>%
  summarize(total_fires = n())
#plot forest fires frequency for days of the week
ggplot(data = fires_by_day, aes(x = day, y = total_fires )) +
  geom_bar(stat = 'identity') +
  labs( title = 'Number of Fires For Each day of The Week', x = 'Month', y = 'Number of Fires')
  
```

### Fixing The Graphs

Although we have shown the frequency for each month and day of the week, the readability of the x-axis labels is not up to par. Since we are used to reading these months or days of a week in chronological order, we will look to change how the graph appears by using the factor function.

```{r}
# Adjust the order of months
forest_fires <- forest_fires %>%
  mutate(month = factor(x = month, levels = c('jan', 'feb', 'mar','apr', 'may', 'jun', 'jul', 'aug', 'sep','oct','nov', 'dec')),
         day = factor(x = day, levels = c('mon', 'tue', 'wed', 'thu', 'fri', 'sat', 'sun')))
```

```{r}
# group by month and count the rows to get frequency with updated factor
fires_by_month <- forest_fires %>%
  group_by(month) %>%
  summarize(total_fires = n())
#plot the new graph with the adjustments to months
ggplot(data = fires_by_month, aes(x = month, y = total_fires)) + 
  geom_bar(stat = 'identity') +
  labs( title = 'Number of Fires For Each Month', x = 'Month', y = 'Number of Fires')
```



```{r}
# group by day and count the total of rows to get frequency with updated factor
fires_by_day <- forest_fires %>%
  group_by(day) %>%
  summarize(total_fires = n())
#plot the new graph with the adjustments to day
ggplot(data = fires_by_day, aes(x = day, y = total_fires )) +
  geom_bar(stat = 'identity') +
  labs( title = 'Number of Fires For Each day of The Week', x = 'Month', y = 'Number of Fires')
```

## Forest Fire Frequency Findings

After looking at frequency graphs for forest fires during each month and day of the week, we can conclude that most fires happen around the months of August and September. Additionally, it looks like forest fires are more prominent on the days of Friday, Sat, and Sunday. Given our findings, let’s look more into what causes these months and days to be higher in frequency than others.


```{r}
#this function allows us to plot multiple graph with different x_variables
create_boxplot <- function(x,y){
  ggplot(data = forest_fires) +
    aes_string(x = x, y = y) +
    geom_boxplot() +
    theme(panel.background = element_rect(fill = "white"))
}
x_var_month <- names(forest_fires)[3]
x_var_day <- names(forest_fires)[4]
y_var <- names(forest_fires)[5:12]
month_box <- map2(x_var_month, y_var, create_boxplot)
day_box <- map2(x_var_day, y_var, create_boxplot)
```

```{r}
month_box
```

```{r}
day_box
```


### Boxplot Findings

Why do the Months of August and September have a higher frequency for forest fires?

While looking at the box plot we discovered that temperatures and levels of drought were higher in the month of August and September. Furthermore, the level of humidity was also lower than all the other months. When looking at the graph we can make an assumption that temperatures, levels of drought, and humidity play a factor in the increase of forest fires. However further analysis to confirm are prediction is required. 

Why do the Days Friday, Saturday, and Sunday have a higher frequency in forest fires?

After looking at the graph it’s apparent that most of the findings look to be uniform in distribution.  Thus, it is to difficult to predict any difference between these days and the rest of the week. Further investigation will be needed to predict why those days are more frequent in forest fires. 
 

```{r}
#create a scatter plot that creates multiple graphs many x_vars and 1 y_var
scatter_plot <- function(x,y) {
  ggplot(data = forest_fires) +
    aes_string(x = x, y = y) +
    geom_point() +
    theme(panel.background = element_rect(fill = 'white'))
}
x_var_scatter <- names(forest_fires)[5:12]
y_var_scatter <- names(forest_fires)[13]
scatter_graph <- map2(x_var_scatter, y_var_scatter, scatter_plot)
```

```{r}
scatter_graph
```

### Scatter Plot Findings

In this dataset we have the area column which tells us the burned area of the forest in hectares (ha): 0.00 to 1090.84. Maybe we can use this variable as an indicator of the severity of a potential fire . In the graphs above, we compared the area as the y variable to multiple x variables to get an idea of what influences the most forest hectares burned.

After looking at the graphs, we can predict that these multiple variables do not influence the intensity of the flame. If the graphs were to show the variables influencing the intensity of the forest fire we would see a grouping of higher figures y-axis. However, what we see instead are only outliers that show large areas burned. Yet, this could be an issue where we are scaling the graph correctly. If we were to decrease the scale we might get better accuracy for the amount of area burned in relation to the different variables.  
