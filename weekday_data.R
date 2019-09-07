library(tidyverse)
library(lubridate)
df <- read_csv('clean.csv')

avg_delay <- df %>% filter(dep_delay >= -120 & dep_delay <= 720 & !is.na(dep_delay)) %>% 
  mutate(mean = mean(dep_delay)) %>% select(mean) %>% head(1)

N <- df %>% nrow()
df_weekly <- df %>% 
  filter(dep_delay >= -120 & dep_delay <= 720 & !is.na(dep_delay)) %>% 
  mutate(dep_time = hour(dep)) %>% 
  group_by(weekday, dep_time) %>% 
  filter(dep_time > 3) %>% 
  summarise(n = n(),
            total_dep_delay = sum(dep_delay),
            avg_dep_delay = total_dep_delay / n,
            var = (avg_dep_delay - avg_delay$mean) ^ 2 / n,
            std = sqrt(var),
            sme = std / sqrt(n))

df_weekly$weekday <- df_weekly$weekday %>% 
  factor(levels = c('Monday', 'Tuesday',
                                   'Wednesday', 'Thursday',
                                   'Friday', 'Saturday', 'Sunday'),
                        ordered = TRUE)
df_weekly$weekday <- df_weekly$weekday %>% as.integer()

df_weekly %>% head()
df_weekly %>% str()
write_csv(df_weekly, 'delays_weekly.csv')