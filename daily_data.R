library(tidyverse)
library(lubridate)
df <- read_csv('clean.csv')

#Remove missing values and outliers
df <- df %>%
  filter(dep_delay > -120 & dep_delay < 720 & !is.na(dep_delay))

#Get average dep_delay
avg_delay <- df$dep_delay %>% mean()

#Group data by date
df_daily <- df %>% 
  mutate(month = month(date)) %>% 
  group_by(date, weekday, month) %>% 
  summarise(n = n(),
            total_delay = sum(dep_delay),
            avg_dep_delay = total_delay / n,
            var = (avg_dep_delay - avg_delay) ^ 2 / n,
            std = sqrt(var),
            sme = std / sqrt(n))

#Convert weekday to integer
df_daily$weekday <- df_daily$weekday %>% 
  factor(levels = c('Monday', 'Tuesday',
                    'Wednesday', 'Thursday',
                    'Friday', 'Saturday', 'Sunday'),
         ordered = TRUE)
df_daily$weekday <- df_daily$weekday %>% as.integer()

#Get day of the month
df_daily$day <- df_daily$date %>% day()

#Get October
df_oct <- df_daily %>% filter(month == 10)
write_csv(df_oct, 'delays_daily_oct.csv')

#Get November
df_nov <- df_daily %>% filter(month == 11)
write_csv(df_nov, 'delays_daily_nov.csv')

#Get December
df_dec <- df_daily %>% filter(month == 12)
write_csv(df_dec, 'delays_daily_dec.csv')