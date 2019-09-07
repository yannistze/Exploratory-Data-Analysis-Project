---
title: "EDAV Project"
output:
  html_document: default
  pdf_document: default
group: Costas, Ioannis, Eric, Kai
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE,
                      message = FALSE,
                      warning = FALSE,
                      cache = TRUE)
```

# Introduction

In today’s day and age where the consumer has zero tolerance for inefficiencies, one can’t help but wonder how flight delays are still prevalent. How can multimillion dollar airlines fail to anticipate these at a consistent basis? It is certainly to the benefit of all parties involved that they don’t occur, so how come one in three flights is delayed? As frequent flyers and curious data scientists, we set out to explore the natural phenomenon that is flight delays and its causes. Can we find a common denominator amongst all delays?

The team consists of four members where each analyzed the corresponding variables:

  * Eric Boxer: time, date, day of the week, destination  
  * Kai Kang: destination states and departure less arrival delay  
  * Ioannis Tzemos: distance, day of the month, carrier  
  * Costas Vafeades: day of the week, makeup delay, carrier and destination popularity  

On top of the contributions listed above, every member took it upon himself to further add value. A visualization aficionado, Eric produced half of the interactive plots found in our main analysis and maintained our project repository. Kai, a teamwork veteran, kept notes and led the discussions in our weekly meetings. Our efficiency expert Ioannis produced the other half of the interactive plots and optimized our pipeline’s run-time. Finally, as meticulous one as can be, Costas provided structure by producing agendas and schedules for the group and set up the data pipeline with Eric.

We used Github to collaborate and our Github repository can be found [here](https://github.com/Ecboxer/BKTV_Project).

# Description of data

Having chosen our topic, we steered our focus towards finding a dataset that can answer the aforementioned questions for us. As our source for data, we used the website of the [United States Department of Transportation](https://transtats.bts.gov/databases.asp?Mode_ID=1&Mode_Desc=Aviation&Subject_ID2=0). Of course, based on our goal, we interacted only with the portion of their collected data library that deals with the topic of Aviation.

The Aviation Data Library, incorporates 26 different databases with data going back to one or even two decades. From the available databases, we used the one called “Airline On-Time Performance Data”. It stores, amongst other features, monthly data for scheduled and actual arrival or departure times of flights as reported by US certified air carriers. The “Reporting Carrier On-Time Performance (1987-present)” table, is the one that we drew our dataset from.

In a nutshell, our data can be found:

  * [United States Department of Transportation](https://transtats.bts.gov/databases.asp?Mode_ID=1&Mode_Desc=Aviation&Subject_ID2=0)
    * Airline On-Time Performance Data
      * Reporting Carrier On-Time Performance (1987-present)

Given the plethora of available data, we had to implement an initial filtering before the dataset could be extracted. Based on the available parameters for compressed downloading that the website provides, we agreed upon the following conditions:

* From a total of 111 different features, distributed across 12 groups each encapsulating features relevant to the topic of the group, we chose 19 of them. These features can be combined to produce an answer for our already introduced questions. 
* Additionally, based on the fact that we wanted to explore the time delays in crucial periods within a year, we focused our data extraction on the last trimester of 2017 (October to December). During that time of year, there exist at least two major holidays (Thanksgiving and Christmas) when traveling is frequent and the delays can be clearly evaluated. Of course, we also included October in order to keep our conclusions unbiased and have a more clear representation of the situation concerning the delays. 
* Finally, our dataset’s aim was to provide data for domestic flights out of New York, for any available airline company that fulfills such flights and to every possible destination in United States.

Having extracted and downloaded a dataset of approximately 1.4 million observations, we were able to continue with the exploratory data analysis of our dataset.

```{r libraries}
# Import libraries
library(tidyverse)
library(lubridate)
library(viridis)
library(choroplethr)
library(choroplethrMaps)
# Set default theme
theme_set(theme_bw())
options(scipen=1)
```

# Analysis of data quality
## Pipeline
[__Here__](https://github.com/Ecboxer/BKTV_Project/blob/master/pipeline_opt.R) is our script.  
Having downloaded data as three csv files (one for each of October, November and December 2017) we wrote an R script to construct features for departure and arrival delays and output two csvs, of the full dataset and of a representative random sample.  
The pipeline:

  * Binds csv files of October, November and December flights by row,  
  * Renames columns of the combined dataframe for easier access,  
  * Formats departure and scheduled departure times from 'HH:MM' to 'YYYY:mm:dd HH:MM:00' and takes their difference to find departure delay in minutes,  
  * Repeats the previous step for arrival and scheduled arrival times to find arrival delays.  
  
Since the full data set contains 1.4 million rows, in the interest of quick computations and visualization in the exploratory data analysis phase, we took a random sample of 150 000 rows from the processed data. 150 000 was arrived at as a compromise between the processing speed of a small sample and the representativeness of a large one.

```{r load data}
# Import sampled and full datasets
df_sample <- read_csv('cleanest_sample.csv')
df <- read_csv('clean.csv')
```

## Missing values
```{r missing values}
colSums(is.na(df)) / nrow(df)
```

Of the features included in our final dataset, departure time, arrival time, departure delay and arrival delay were missing values for 0.7% of observations. Cancellation code had missing values for 99% of observations.

```{r delay missing values}
extracat::visna(df)
```

The dominant missing data pattern is cancellation code as the sole missing feature. Infrequently, observations are missing departure and arrival times, but we felt that all observations missing either of those values could be removed without losing too much information about delay durations.

```{r cancellation missing values}
df %>% filter(is.na(canc_code) & cancelled == 1) %>% nrow()
```

There are no cases in which cancellation code is missing but the data tells us that the flight was actually cancelled. That would have been a clear signal that some observations were incorrectly entered. As it is we will assume that the data on cancellations are accurate.

```{r cancellation look}
df %>% filter(!is.na(canc_code)) %>%
  group_by(canc_code) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_bar(aes(reorder(canc_code, -n), n), fill='#774184', stat='identity') +
  xlab('canc_code') + ggtitle('Reason for cancellation') +
  labs(caption = "Source: Final Project Dataset")
```

Cancellation codes are categorised by cause of cancellation as follows: A Carrier, B Weather, C National Air System, D Security. Security cancellations accounted for a tiny proportion while weather cancellations were the most frequent. We opted to forgo further investigation of cancellation. Since these flights never departed, there is no delay information from any of these observations in this dataset.

## Explain metadata
The final features are as follows: 

  * date: flight date in the format YYYY-mm-dd  
  * carrier: a unique two-letter carrier code for each airline, ie 'AA' for American Airlines  
  * flight_num: a unique four-digit number for each flight  
  * origin: a unique five-digit number for each origin airport, with all of these located in New York State  
  * dest: similar to origin but for each destination airport and located in any US state  
  * dest_state: a two-digit code for the flight destination state or US territory  
  * sched_dep: scheduled departure time in the format YYYY-mm-dd HH:MM:00  
  * dep: actual departure time in the same format  
  * sched_arr: schedulaed arrival time in the same format  
  * arr: actual arrival time in the same format  
  * cancelled: indicator of flight cancellation, with 1 corresponding to a cancelled flight and 0 otherwise  
  * canc_code: specifies the reason for cancellation  
  * distance: distance between origin and destination airports, in miles  
  * weekday: a string corresponding to the day of the week of the flight date  
  * dep_delay: departure delay, in minutes. Negative values correspond to flights that departed before schedule, positive values with flights that departed after they were scheduled  
  * arr_delay: similar to dep_delay but for arrival delay  

## Filtering
For the remainder of the data exploration we removed observations with missing values in the departure and arrival delay features. Then, we removed those observations with departure or arrival delay less than 2 hours or greater than 6 hours. In processing the data there were a small percentage of flights which were scheduled to depart before midnight but had actual times after midnight (or were scheduled for after midnight but had actual times before). As a consequence of our method for calculating delay (taking the difference of scheduled and actual times), those observations came out with delays of magnitude plus-minus 1440 (the number of minutes in one day). Keeping those observations would introduce extreme outliers. Since this happened to about 1% of the data we decided to exclude these observations from the analysis.
```{r exclude outliers}
df %>% 
  filter(dep_delay < -1200 | dep_delay > 1200 | arr_delay < -1200 | arr_delay > 1200) %>% 
  nrow() / nrow(df)
```

```{r filtering illustration, eval = FALSE}
df_sample %>% 
  ggplot() +
  geom_point(aes(dep_delay, flight_num), alpha=.2, shape=1) +
  labs(caption = "Source: Final Project Dataset Sample")
```
![](dep_delay.png)  

The decision to keep data with delays in the range [-120, 720] was a consequence of a tendency for data to fall within that range. Filtering within this range will serve to remove outliers from the remainder of our analysis.  

```{r clean data}
# Delete NA rows, which indicate flight cancellations
df_sample <- df_sample %>%
  filter(!is.na(dep_delay) & !is.na(arr_delay))
df <- df %>%
  filter(!is.na(dep_delay) & !is.na(arr_delay))

# Subset data with relevent dep_delay and arr_delay values
df_sample <- df_sample %>%
  filter(arr_delay >= -120 & arr_delay <= 720 & dep_delay >= -120 & dep_delay <= 720)
df_backup <- df
df <- df %>%
  filter(arr_delay >= -120 & arr_delay <= 720 & dep_delay >= -120 & dep_delay <= 720)
```

# Main analysis (Exploratory Data Analysis)

Our data exploration starts by first looking at variables that might help explain the delay:

  * Date  
  * Day of the Week  
  * Distance  
  * Destination Airport  
  * Carrier  
  
Naturally, we then proceed by looking at the following variables in an effort to determine if delays can be amended for:

  * Arrival Delay  
  * Airline  
  * Destination Airport  
  * Destination State  

### Day of the Month

We initiate our Exploratory Data Analysis by examining the Delays in Departure time for every day of the month. The following scatterplot helps us determine whether a relationship can be inferred.

```{r dates}
monthly_data <- mutate(df, month = format(as.Date(df$date,format="%Y-%m-%d"), "%m"), 
                       day = format(as.Date(df$date,format="%Y-%m-%d"), "%d"))
monthly_data <- select(monthly_data, date, dep_delay, month, day)

monthly_data$month[monthly_data$month == 10] <- "October" 
monthly_data$month[monthly_data$month == 11] <- "November" 
monthly_data$month[monthly_data$month == 12] <- "December" 
monthly_data$month = factor(monthly_data$month, levels=c("October","November","December"))
monthly_delay <- filter(monthly_data, df$dep_delay > 0)
monthly_early <- filter(monthly_data, df$dep_delay <= 0)

ggplot() + 
  geom_point(data = monthly_delay, aes(y = dep_delay, x = day), alpha = .5, color = "#774184", stroke = 0) +
  geom_point(data = monthly_early, aes(y = dep_delay, x = day), alpha = .8, color = "#fce640", stroke = 0) +
  facet_grid(month~.) +
  ylab("Delay in Departure (Minutes)") +
  xlab("Days of the Month") +
  ggtitle("Departure Delay correlated with Days of the Month",
        subtitle = "Limited between 12 hours delay and 2 hours early departure") +
  labs(caption = "Source: Final Project Dataset")
```

Clearly we can observe that during the Christmas holidays, departure delay shows an increase in duration, thus suggesting that passengers spend a lot more time waiting in airports to depart for their final destination. The same conclusion can be derived for the weekend following Thanksgiving (Thursday the 23 of November), which again constitutes an important holiday for the US. Departure delays during the Thanksgiving period are at a high on Sunday, when most people are returning home. Finally, we can observe that certain days of the week, like Tuesdays or Wednesdays, have reduced Departure Delay times. This is expected since the main reasons for travelling during the week are business related.

### Day of the Week

How are flights distributed throughout a week? Which day is most popular for travelling?
```{r day of the week}
num_flights <- df %>% group_by(weekday) %>% summarize(count = n())
num_flights$perc <- num_flights$count / sum(num_flights$count)
num_flights$weekday <- factor(num_flights$weekday, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))

ggplot(num_flights, aes(weekday, perc)) +
  geom_col(fill = '#774184' ) +
  ggtitle('Percentage of Flights on Each Day') +
  xlab("Day of the Week") +
  ylab("Percentage of Flights") +
  labs(caption = "Source: Final Project Dataset")
```

Flights seem to be evenly distributed over all days except Saturdays. This makes sense, as travellers with flights on Saturdays are likely to be on vacation - and are consequently more likely to depart for their destination on Friday. How do delays relate to days of the week?

```{r day of the week boxplot}
df$weekday <- factor(df$weekday, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))
ggplot(df, aes(x=weekday, y=dep_delay)) + geom_boxplot(varwidth=TRUE, fill = '#774184') +
  ggtitle('Distribution of Delay on Each Day') +
  xlab("") +
  ylab("Delay in Minutes") +
  labs(caption = "Source: Final Project Dataset")
```

The range of our dataset makes it hard to visualize the distribution effectively and identify any patterns. One way of addressing this is taking the log of our y-axis. Of course, logging negative values would result in erroneous results so we'll focus on positive delays for now. 

```{r day of the week log}
ggplot(subset(df, df$dep_delay >0), aes(x=weekday, y=log(dep_delay))) + geom_boxplot(varwidth=TRUE, fill = '#774184') +
  ggtitle('Distribution of Positive Delay on Each Day') +
  xlab("") +
  ylab("log(Delay in Minutes)") +
  labs(caption = "Source: Final Project Dataset")
```

Judging by the plot, delays are generally lower during Tuesdays, Wednesdays and Thursdays and pick up during the weekend. We assume this is due to the fact that most flights during those days are business related and also earlier on in the day, which as shown later on also plays a factor.

```{r day of the week split}
df$pos_delay <- ifelse(df$dep_delay > 0, 'Positive', 'Negative')

ggplot(df, aes(x=weekday, y=log(abs(dep_delay)))) + geom_boxplot(aes(fill=factor(pos_delay)), varwidth=TRUE) +
  scale_y_continuous(name = 'log(Delay in Minutes)') + xlab('Day of the Week') + guides(fill=guide_legend(title="Delay")) +
  scale_fill_manual(values=c('#4E8441', '#774184')) +
  labs(caption = "Source: Final Project Dataset")
```

More flights depart early than late, but positive delays have greater range. Early departures also don't seem to vary by day. 

### Distance

How does distance relate to Departure Delays?

```{r distance}
ggplot(df_backup, aes(x = dep_delay, y = distance)) +
    geom_point(alpha = .3, color = "#441152", stroke = 0) +
    xlab("Delays in Departure (Minutes)") +
    ylab("Distance (Miles)") +
    ggtitle("Departure Delays in relation to the travelled Distance") +
    labs(caption = "Source: Final Project Dataset")
```

Plotting the scatterplot of the Distance in correlation with the Departure Delay, helps us determine the following:

* The different values of the delays recorded for a specific distance, seem to form a line.  
* Also, there is a specific interval of delay that should become our main focus. 

To further investigate the latter claim, we produce the following two graphs.

```{r distance under 12 hrs}
ggplot(df_backup, aes(x = dep_delay, y = distance)) +
    geom_point(alpha = .3, color = "#4E8441", stroke = 0) +
    xlim(-720,720) +
    xlab("Delays in Departure (Minutes)") +
    ylab("Distance (Miles)") +
    ggtitle("Departure Delays in relation to the travelled Distance",
          subtitle = "Limited in 12 hours delays - early departures") +
    labs(caption = "Source: Final Project Dataset")
```
```{r distance under 3 hrs}
ggplot(df_backup, aes(x = dep_delay, y = distance)) +
    geom_point(alpha = .3, color = "#441152", stroke = 0) +
    xlim(-180,180) +
    xlab("Delays in Departure (Minutes)") +
    ylab("Distance (Miles)") +
    ggtitle("Departure Delays in relation to the travelled Distance",
            subtitle = "Limited in 3 hours delays - early departures") +
    labs(caption = "Source: Final Project Dataset")
```

Now, we can clearly bound the times of the departure delays that we are going to examine between a maximum of 12 hours delay and a maximum of 2 hours early departure. This follows from the fact that a 3 hour delay or early departure interval (third graph) does not constitute a clear representation of the phenomenon that we are exploring. On the contrary, a 12 hours delay interval is more than enough to give us the full picture. When combined with the 2 hours early departure barrier we have a clear picture of the situation at hand. 

Upon further investigation of the distance variable, we produced a plot of the unique values of distances traveled to reach each state.

```{r distance states}
states_recode <- c("1" = "AK", "2" = "HI", "3" = "PR", "4" = "VI", "5" = "TT", "11" = "CT", "12" = "ME", "13" = "MA", "14" = "NH", "15" = "RI", "16" = "VT", "21" = "NJ", "22" = "NY", "23" = "PA", "33" = "FL", "34" = "GA", "35" = "MD", "36" = "NC", "37" = "SC", "38" = "VA", "39" = "WV", "41" = "IL", "42" = "IN", "43" = "MI", "44" = "OH", "45" = "WI", "51" = "AL", "52" = "KY", "53" = "MS", "54" = "TN", "61" = "IA", "62" = "KS", "63" = "MN", "64" = "MO", "65" = "NE", "66" = "ND", "67" = "SD", "71" = "AR", "72" = "LA", "73" = "OK", "74" = "TX", "81" = "AZ", "82" = "CO", "83" = "ID", "84" = "MT", "85" = "NV", "86" = "NM", "87" = "UT", "88" = "WY", "91" = "CA", "92" = "OR", "93" = "WA")

distance_state_data <- df %>% select( dest_state, distance ) %>% group_by( dest_state, distance ) %>% summarize( count = n() ) 
distance_state_data <- distance_state_data %>% mutate( state = states_recode[as.character(dest_state)])

ggplot(distance_state_data, aes(x = state, y = distance)) +
    geom_point(alpha = .8, color = "#441152", stroke = 0) +
    xlab("Destination (State)") +
    ylab("Distance (Miles)") +
    theme(axis.text.x = element_text(size = 5)) +
    ggtitle("Distance in relation to the State Destination",
            subtitle = "Unique Distances traveled to reach corresponding State destination") +
    labs(caption = "Source: Final Project Dataset")
```

Additionally, we create a graph of the distances that each Airline claims to have covered. 

```{r distance carrier}
distance_carrier_data <- df %>% select( carrier, distance ) %>% group_by( carrier, distance ) %>% summarize( count = n() ) 

ggplot(distance_carrier_data, aes(x = carrier, y = distance)) +
    geom_point(alpha = .8, color = "#4E8441", stroke = 0) +
    coord_flip() +
    xlab("US Airline") +
    ylab("Distance (Miles)") +
    ggtitle("Distance in relation to the Airline") +
    labs(caption = "Source: Final Project Dataset")
```

Based on the two graphs presented, we are able to observe that values of the distance variable are more discrete than they were expected to be. Logically speaking, we should have a greater variety of values since it is extremely rare for two planes to cover the exact same distance going back and forth to the same destination. By going back to the source of our data, we were able to discover that distances provided by our dataset are  approximations of the shortest distance between any two points on the surface of a sphere. Therefore, any two flights with the same start and end points have the same distances. This can be verified from the table below.

```{r distance table}
distance_table <- df %>% select(origin, dest, dest_state, distance) %>% group_by(origin, dest)
head(distance_table, 10)
```

### Destination Airport

```{r number of destinations}
df$dest %>% unique() %>% length() #How many airports are in the data?
```

To begin exploring the data on destination, with 300 unique values, we first construct a dataframe grouped by destination and with features for number of flights, total departure delay and mean departure delay.

```{r destination frequency}
df_n <- df %>% group_by(dest) %>% 
  summarise(n = n(),
            total_delay = sum(dep_delay),
            avg_delay = total_delay / n) %>% 
  arrange(desc(n))
```

With the dataframe in hand, we will look at destinations with the most flights, more than 20 000 over the period.
```{r destination cleveland}
df_n %>% filter(n > 20000) %>% ggplot() +
  geom_point(aes(reorder(dest, avg_delay), avg_delay, size=n, fill=avg_delay), shape=21, alpha=.7) +
  coord_flip() + scale_fill_viridis_c() +
  xlab('destination') +
  labs(caption = "Source: Final Project Dataset")
```

11618 Newark Liberty International stands out as having the highest average delay of the lot. 14771 San Francisco International has a smaller average delay than Newark, but it stands out among those airports with more than 40 000 flights. Apart from San Francisco these airports have average delays in the neightborhood of five minutes. It is also noticeable that none of the most frequent destinations have a negative average delay.

```{r destination negative delay}
df_n %>% filter(avg_delay < 0) %>% ggplot() +
  geom_point(aes(reorder(dest, avg_delay), avg_delay, size=n, fill=avg_delay), shape=21, alpha=.7) +
  coord_flip() + scale_fill_viridis_c() +
  xlab('destination') +
  labs(caption = "Source: Final Project Dataset")
```

Here we looked at those destinations with negative average delays. According to our size scale, the frequency of flights to these airports is more than an order of magnitude lower than in the previous plot. Two airports stand out in terms of average delay, 10165 Adak Island, Alaska and 10754 Barrow, Alaska. 
```{r destination zero delay}
df_n %>% filter(avg_delay < 1 & avg_delay > -1) %>% ggplot() +
  geom_point(aes(reorder(dest, avg_delay), avg_delay, size=n, fill=avg_delay), shape=21, alpha=.7) +
  coord_flip() + scale_fill_viridis_c() +
  xlab('destination') +
  labs(caption = "Source: Final Project Dataset")
```

Among those airports with average delays within one minute of zero, the number of flights also tends to be small, no more than 3 000.
```{r destination high frequency}
df_n %>% filter(avg_delay > 10) %>% ggplot() +
  geom_point(aes(reorder(dest, avg_delay), avg_delay, size=n, fill=avg_delay), shape=21, alpha=.7) +
  coord_flip() + scale_fill_viridis_c() +
  xlab('destination') +
  labs(caption = "Source: Final Project Dataset")
```

When we filter to those airports with average delays greater than ten minutes, airport 11618 Newark stand out as by far the most popular destination in this bracket. As with the negative delay visualization, there are a small number of outliers in terms of average delay:

  * 13388 Mammoth Lakes, California  
  * 15295 Toledo, OH  
  * 13964 North Bend/Coos Bay, Oregon  
  * 10372 Aspen, Colorado  
  
Mammoth Lakes and Aspen are both ski resort towns so it might be interesting to look at the average delays in the summer to see if there is a reversion to the mean during warmer months. 

### Carrier

Are certain airlines more likely to experience delays? 

```{r carrier data}
top_carriers <- df %>% group_by(carrier) %>% summarize(num_flights = n())
top_carriers <- top_carriers[order(-top_carriers$num_flights), ]
top_carriers <- head(top_carriers, 5)
last_carriers <- df %>% group_by(carrier) %>% summarize(num_flights = n())
last_carriers <- subset(last_carriers, last_carriers$num_flights > 100)
last_carriers <- last_carriers[order(last_carriers$num_flights), ]
last_carriers <- head(last_carriers, 5)
df$pop_carrier <- ifelse(df$carrier %in% top_carriers$carrier, "Busy", 0)
df$pop_carrier <- ifelse(df$carrier %in% last_carriers$carrier, "Not Busy", df$pop_carrier)
head(top_carriers)
```

The five most popular airlines are:

  * WN: Southwest Airlines  
  * DL: Delta Airlines  
  * AA: American Airlines  
  * OO: Skywest Airlines  
  * UA: United Airlines  

```{r airlines tail}
head(last_carriers)
```

The five least popular airlines are:

  * VX: Virgin America  
  * HA: Hawaiian Airlines  
  * F9: Frontier Airlines  
  * NK: Spirit Airlines  
  * AS: Alaska Airlines  

A reminder that this is only in regards to domestic flights departing New York.

```{r carrier boxplots}
ggplot(subset(df, df$pop_carrier != 0), aes(x=factor(carrier), y=log(dep_delay))) + geom_boxplot(varwidth=TRUE, fill = '#774184') +
  ggtitle('Distribution of Positive Delay in the most and least frequent Airlines') +
  facet_grid(. ~ pop_carrier) +
  xlab("Airline") +
  ylab("log(Delay in Minutes)") +
  labs(caption = "Source: Final Project Dataset")
```

The departure delay does not seem to vary as much, implying that the frequency of an airline does not play a factor on departure delay. Upon closer inspection, Skywest is the airline with the highest median delay whereas Hawaiaan has the lowest. Southwest is the most popular out of the ten, with more than 3500 flights out of New York State every day. It is also the best performing airline out of the "Busy" group. 

Judging from the plots presented so far, departure delays are more likely to be caused by factors like the time of departure and the day of the week rather than the popularity of the airline or the flight's destination. However in the event of a delay prior to departure, is it a sign that the arrival delay is going to be even larger or will the pilot make-up for the delay by getting to the destination faster? Are long flights more likely to have a high delay make-up (difference between departure and arrival delay) or do they exacerbate the situation further?

### Arrival Delay

How do departure delay and arrival delay relate? Is it possible that pilots make up for delays with a shorter flight, or is it a sign that the flight is going to take even longer? 

```{r diff hexbin}
options(scipen=1)
ggplot(df, aes(dep_delay, arr_delay)) +
  geom_hex(bins = 50) + 
  scale_fill_gradientn(colours = viridis(5, direction = 1)) +
  xlab('Departure Delay in Minutes') + 
  ylab('Arrival Delay in Minutes') +
  ggtitle('Departure Delay in relation to Arrival Delay') +
  guides(fill=guide_legend(title="Frequency")) +
  labs(caption = "Source: Final Project Dataset")
```

The two variables appear to have a roughly linear relationship, especially at higher values. There are exceptions to this, where a large number of flights experience high arrival delay even though they had zero departure delay. This can be explained by flights that had trouble landing because destination airport landing strips were busy. 

We construct a variable called diff_delay that measures the difference between departure delay and arrival delay. This will allow us to examine whether a departure delay is likely to be amended for. 

```{r construct diff_delay}
df$diff_delay <- df$dep_delay - df$arr_delay
```

### Airline

```{r airline boxplots}
df$pos_makeup <- ifelse(df$diff_delay >= 0, "Positive", "Negative")
df$pos_makeup <- factor(df$pos_makeup)

ggplot(subset(df, df$pop_carrier != 0 & df$pos_makeup == 'Positive'), aes(x=factor(carrier), y=log(dep_delay))) + geom_boxplot(varwidth=TRUE, fill = '#774184') +
  ggtitle('Distribution of Positive Makeup Delay in the busiest and least busy Airlines') +
  facet_grid(. ~ pop_carrier) +
  xlab("Airline") +
  ylab("log(Positive Makeup Delay in Minutes)") +
  labs(caption = "Source: Final Project Dataset")
```

Apparently, the size of an airline doesn't have an impact on its ability to make up for a delay.

### Destination State

We wish to explore any patterns relating departure delay and arrival delay. Does the distance between New York and the destination state influence a flight's performance? What about the destination state's population density? 

```{r destination state arrival time}
# data frame for the cleveland plot and map
flight <- df
flight$dest_state <- factor(flight$dest_state)
region_code <- c("1", "2", "3", "4", "5", "11", "12", "13", "14", "15", "16", "21", "22", "23", "31", "33", "34", "35", "36", "37", "38", "39", "41", "42", "43", "44", "45", "51", "52", "53", "54", "61", "62", "63", "64", "65", "66", "67", "71", "72", "73", "74", "81", "82", "83", "84", "85", "86", "87", "88", "91", "92", "93", "31")
region_name <- c("Alaska", "Hawaii", "Puerto Rico", "U.S. Virgin Islands", "U.S. Pacific Trust Territories and Possessions", "Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont", "New Jersey", "New York", "Pennsylvania", "Delware", "Florida", "Georgia", "Maryland", "North Carolina", "South Carolina", "Virginia", "West Virginia", "Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin", "Alabama", "Kentucky", "Mississippi", "Tennessee", "Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska", "North Dakota", "South Dakota", "Arkansas", "Louisiana", "Oklahoma", "Texas", "Arizona", "Colorado", "Idaho", "Montana", "Nevada", "New Mexico", "Utah", "Wyoming", "California", "Oregon", "Washington", "Delaware")
flight$dest_state <- factor(flight$dest_state, region_code, region_name)

# tidy data for cleveland plot and map
flight_state <- flight %>%
  group_by(dest_state) %>%
  dplyr::summarize(sum_n = n(), sum_arr = sum(arr_delay), sum_dep = sum(dep_delay), sum_diff = sum(diff_delay)) %>%
  mutate(mean_arr = sum_arr/sum_n, mean_dep = sum_dep/sum_n, mean_diff = sum_diff/sum_n)

# cleveland plot of arrival time and depature time
flight_state$dest_state <- reorder(flight_state$dest_state, flight_state$mean_arr)
ggplot(flight_state) +
  geom_point(aes(mean_arr, dest_state, color = 'mean_arr'), size = 3) +
  geom_point(aes(mean_dep, dest_state, color = 'mean_dep'), size = 3) +
  labs(title = "Arrival and Departure Delay over States", x = "Average Delay in Minutes", y = "", color = '') +
  theme(legend.position = 'bottom') + 
  scale_color_manual(name = '', values = c('mean_arr'= '#774184', 'mean_dep'='#4E8441'), labels=c("Arrival Delay", "Departure Delay"))

# generate arrival map format for choropleth
arr_map_form <- subset(flight_state, select = c('dest_state', 'mean_arr'))
names(arr_map_form)[1] <- paste('region')
names(arr_map_form)[2] <- paste('value')
arr_map_form$region <- as.character(arr_map_form$region)
arr_map_form$region <- tolower(arr_map_form$region)
# generate arrival map
arr_map <- StateChoropleth$new(arr_map_form)
arr_map$title = "Arrival Delay over States"
arr_map$show_labels = TRUE
arr_map$set_num_colors(1)
arr_map$ggplot_scale = scale_fill_viridis(name = 'Average Arrival Delay in Minutes')
arr_map$render()
```

Neither departure nor arrival delay are strongly associated with destination state. The five regions with highest arrival delays are New Jersey, Puerto Rico, the U.S Virgin Islands, Hawaii and New York. The five regions with lowest arrival delays are Virginia, Maryland, Utah, South Carolina and the U.S Pacific Trust Territories and Possessions.

We surprisingly found the average departure delay is higher than the average arrival delay in all states, no matter their location or population density.

```{r destination state delay time difference}
# cleveland plot of time difference
flight_state$dest_state <- reorder(flight_state$dest_state, flight_state$mean_diff)
ggplot(flight_state) +
  geom_point(aes(mean_diff, dest_state), color = '#774184', size = 3) +
  labs(title = "Difference between Departure and Arrival Delay over States", x = "Average Difference in Minutes", y = "", color = '')

# generate diff map format for choropleth
diff_map_form <- subset(flight_state, select = c('dest_state', 'mean_diff'))
names(diff_map_form)[1] <- paste('region')
names(diff_map_form)[2] <- paste('value')
diff_map_form$region <- as.character(diff_map_form$region)
diff_map_form$region <- tolower(diff_map_form$region)

# generate diff map
diff_map <- StateChoropleth$new(diff_map_form)
diff_map$title = "Difference in Delay over States"
diff_map$show_labels = TRUE
diff_map$set_num_colors(1)
diff_map$ggplot_scale = scale_fill_viridis(name = 'Average Difference in Minutes')
diff_map$render()
```

To our surprise, flights to the Northwestern states (Washington, Oregon, Idaho, Montana, Wyoming, North Dakota and South Dakota) had a lower delay difference than the others. This contradicts our hypothesis that pilots can make up for time lost during longer flights. There doesn't appear to be a relationship between destination and the difference between arrival and departure delay. In addition to the Northwest states, Alaska, Hawaii, U.S. Virgin Islands and Mississippi also have a low make-up delay.

# Executive summary

## Daily Departure Delays
[__Here__](https://bl.ocks.org/ecboxer/6be5cf6fc44449823e08ca5f1b443935) is a visualization of the average departure delay for each date in the dataset. Each month is shown as a calendar heatmap with each week in a row starting on Monday and ending on Sunday. We used a Value Suppressing Uncertainty Scale (VSUP) which decreases the number of distinct colors according to increasing uncertainty, measured in standard mean error of delay time. Hover over a tile to show the precise departure delay.  
Starting in October, we do not see a clear pattern to delays. Tuesdays and Wednesdays have low departure delays for all but one week, the 24th for Tuesdays and the 11th for Wednesdays. In November, there is a similarly varied distribution to delays. Note the scale changes across months, so the 3rd of November is encoded as high delay with a 10 minute average while the 12th of October has a similar average but lies closer to the middle of that month's distribution. The clearest pattern in December is the split between the beginning of the month and the final two weeks. After the 18th, delays are almost all on the higher-end, with the 30th having an average delay of 20 minutes (but also the highest uncertainty).  

## Weekly Departure Delays
[__Here__](https://bl.ocks.org/ecboxer/221d818a4b9fad4feb0ab14e7ca704a1) is a visualization of the average departure delay grouped by day of the week and hour of departure. It was constructed with the same VSUP scale as the daily departure delay visualization. Again, hovering over a tile shows the precise departure delay.  
There are two patterns here: the distinction between weekdays and weekends, and an increase in delay length as a given day progresses. Weekends have a longer period of dark-blue than weekdays (corresponding to delays in the range of 3-9 minutes), and even in the hour before midnight do not reach the same magnitude of delay as do weekdays.  
Across all days, until 9am average delays are small or negative. Afterwards there is a gradual increase in delays until we see an average delay length of 38 minutes in flights departing before midnight on Mondays.  
Note: the choice was made to start the x-axis at 4am since at all previous times the standard mean errors were at least 0.1875 and their values were thus suppressed by our scale.

## Departure Delays and Days of the Month
[__Here__](https://bl.ocks.org/yannistze/a0a46f2740c8916dc71fc4da111995d1) is a scatterplot showing the distribution of departure delay or early departure based on the day of the month. We used a sample of 20% from the whole dataset for better response times and a clear view of the results. Hovering over the name of the month and then clicking will display an enlarged view of the distribution for the corresponding month. The scatterplot allows users to acquire an accurate understanding of how delays and early departures are distributed within the different days of the month and maybe plan their departure date accordingly.

## Departure Delays and US Carriers
[__Here__](https://bl.ocks.org/yannistze/f6679d566e5c1d8ceabab043535dfcac) is a chart of boxplots that show the distribution of departure delay based on the US carrier fulfilling the flight. Taking into account the width of the boxplots, we can infer which carrier fulfills the most flights. Also, when clicking upon the boxplot of the carrier that interests the viewer, they can see the distribution of departure delay for that carrier, as well as the distribution of early to depart flights. As a result, the viewer can compare airline carriers and choose according to their needs.  

## Arrival and Departure Delays
```{r presentation hexbin}
options(scipen=1)
ggplot(df, aes(dep_delay, arr_delay)) +
  geom_hex(bins = 50) + 
  scale_fill_gradientn(colours = viridis(5, direction = 1)) +
  xlab('Departure Delay in Minutes') + 
  ylab('Arrival Delay in Minutes') +
  ggtitle('Departure Delay in relation to Arrival Delay') +
  guides(fill=guide_legend(title="Frequency")) +
  labs(caption = "Source: Final Project Dataset")
```

It appears that there's generally a positive linear relationship between the two variables, debunking the common theory that pilots always make up for time lost. The evident vertical line indicates that flights with no departure delay are often delayed post take-off. We hypothesize that this occurs when the destination landing strip is too busy, forcing the plane to fly around the airport until a strip vacates. However, a plane can't fly aimlessly for long implying that there must be other underlying causes.


## Make-up Delay and State
```{r main choropleth}
diff_map <- StateChoropleth$new(diff_map_form)
diff_map$title = "Average Difference between Departure and Arrival Delay"
diff_map$show_labels = TRUE
diff_map$set_num_colors(1)
diff_map$ggplot_scale = scale_fill_viridis(name = 'Difference in Minutes')
diff_map$render()
```

The Northwestern states; Washington, Oregon, Idaho, Montana, Wyoming, North Dakota and South Dakota have the lowest difference in departure and arrival delay. Again, this contradicts our hypothesis that longer flights allow a pilot to make-up for departure delays. The state with the highest mean difference in delays is New York, the complete opposite of what we expected. Further investigation may help uncover the causes behind large differences in delay for short flights.

# Conclusion

In our study, we used airline on-time performance data from the United States Department of Transportation to explore the performance of airlines departing New York in the fourth quarter of 2017. Our analysis first focused on the relationship between departure delay and the following: flight date, day of the week, destination and airline. The results showed that departure delays were higher during the holiday season, for both Thanksgiving and Christmas. Overall, flights were more likely to depart early than late. Upon closer examination of the date variable, we discovered that weekend travellers are more likely to encounter delays and morning flights have a higher chance of departing on time. For the most part, neither destination state nor airline were strongly associated with departure delay. We then proceeded to examine the linear relationship between departure and arrival delay, in hopes of identifying any causal factors. To do so we constructed 'make-up delay', a variable measuring the difference between the two features. Our maps indicated that the Northwestern states experienced unusually small differences in make-up delay, providing evidence to reject our hypothesis that pilots can make up for time lost on long flights. We also identified two airports -- namely, Newark Liberty International and San Fransisco International -- that have a higher propensity to experience arrival delays. 

Limited information and rough calculations from the dataset restricted us in our investigations. Distance was calculated as the distance between two airports, not true distance travelled. A more accurate figure, along with other flight metrics like number of passengers carried, would add new dimensions to our analysis. How does a fully-booked flight compare to an empty one? Does the number of passengers have an impact on delay? Further granularity in the form of additional variables would also allow us to more closely examine the relationship between arrival delay and departure delay. A variable stating the cause of the delay could prove to be the distinguishing factor between a positive and a negative make-up delay. 