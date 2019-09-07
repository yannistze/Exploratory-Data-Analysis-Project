png('dep_delay.png', height=400)
p <- df_sample %>% 
  ggplot() +
  geom_point(aes(dep_delay, flight_num), alpha=.2, shape=1)
p
dev.off()