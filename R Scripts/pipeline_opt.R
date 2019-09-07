library(tidyverse)
library(lubridate)
library(magrittr)
library(chron)

df_oct <- read_csv('oct2017.csv')
df_nov <- read_csv('nov2017.csv')
df_dec <- read_csv('dec2017.csv')

df <- bind_rows(df_oct, df_nov)
df <- bind_rows(df, df_dec)

colnames(df)

df <- select(df, colnames(df)[1:13])

colnames(df) <- c('date', 'carrier', 'flight_num', 'origin', 'dest', 'dest_state', 'sched_dep', 'dep', 'sched_arr', 'arr', 'cancelled', 'canc_code', 'distance')

df <- mutate(df, weekday = weekdays(df$date))

df$dep <- format(strptime(df$dep, format='%H%M'), format = '%H:%M:00')
df$sched_dep <- format(strptime(df$sched_dep, format='%H%M'), format='%H:%M:00')

df$dep <- paste(df$date, df$dep, sep=' ')
df$sched_dep <- paste(df$date, df$sched_dep, sep=' ')

df$dep <- df$dep %>% strptime('%Y-%m-%d %H:%M:%OS')
df$sched_dep <- df$sched_dep %>% strptime('%Y-%m-%d %H:%M:%OS')

df <- mutate(df, dep_delay = difftime(df$dep, df$sched_dep, units=c('mins'))) #Delay in mins st >0 => delay ^ <0 => early

df$arr <- format(strptime(df$arr, format='%H%M'), format = '%H:%M:00')
df$sched_arr <- format(strptime(df$sched_arr, format='%H%M'), format='%H:%M:00')

df$arr <- paste(df$date, df$arr, sep=' ')
df$sched_arr <- paste(df$date, df$sched_arr, sep=' ')

df$arr <- df$arr %>% strptime('%Y-%m-%d %H:%M:%OS')
df$sched_arr <- df$sched_arr %>% strptime('%Y-%m-%d %H:%M:%OS')

df <- mutate(df, arr_delay = difftime(df$arr, df$sched_arr, units=c('mins')))

write_csv(df, 'clean.csv')

set.seed(42)
df_sample <- df[sample(nrow(df), 1000), ]

write_csv(df_sample, 'clean_sample.csv')
