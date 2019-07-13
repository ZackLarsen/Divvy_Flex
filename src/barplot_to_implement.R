
# Inspired by: https://stephenslab.github.io/wflow-divvy/time-of-day-trends.html

ggplot(subset(divvy$trips,from_station_name == "University Ave & 57th St"),
       aes(start.hour)) +
  geom_bar(fill = "dodgerblue",width = 0.75) +
  facet_wrap(~start.day,ncol = 2) +
  scale_x_discrete(breaks = seq(0,24,2)) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
