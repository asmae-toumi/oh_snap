library(tidyverse)

# load trained_plus_phat_time_of_throw and trained_plus_phat_time_of_throw

load("data/BART_time_of_throw/trained_plus_phat_time_of_throw.RData")
load("data/BART_time_of_arrival/trained_plus_phat_time_of_arrival.RData")

# Example of plot with trained_plus_phat_time_of_throw data 

trained_plus_phat_time_of_throw %>% 
  ggplot(aes(angle_diff, prob_means)) + 
  geom_smooth(method = "loess", se = FALSE) + 
  geom_smooth(aes(angle_diff, low_bound), method = "loess", lty = 3, se = FALSE) + 
  geom_smooth(aes(angle_diff, upp_bound), method = "loess", lty = 3, se = FALSE) + 
  geom_hline(aes(yintercept = 0)) + 
  theme_minimal() + 
  labs(title = "Time of throw model",
       x = "angle_diff",
       y = "Completion probability") +
  theme(legend.title = element_blank()) + 
  scale_y_continuous(breaks = scales::pretty_breaks())

trained_plus_phat_time_of_throw %>% 
  ggplot(aes(humidity, prob_means, color = roof)) + 
  geom_smooth(method = "loess", se = FALSE) + 
  geom_smooth(aes(humidity, low_bound), method = "loess", lty = 3, se = FALSE) + 
  geom_smooth(aes(humidity, upp_bound), method = "loess", lty = 3, se = FALSE) + 
  geom_hline(aes(yintercept = 0)) + 
  theme_minimal() + 
  labs(title = "Time of throw model",
       x = "humidity",
       y = "Completion probability") +
  theme(legend.title = element_blank()) + 
  scale_y_continuous(breaks = scales::pretty_breaks())

# Example of plot with trained_plus_phat_time_of_arrival data 

trained_plus_phat_time_of_arrival %>% 
  ggplot(aes(dist_traveled, prob_means)) + 
  geom_smooth(method = "loess", se = FALSE) + 
  geom_smooth(aes(dist_traveled, low_bound), method = "loess", lty = 3, se = FALSE) + 
  geom_smooth(aes(dist_traveled, upp_bound), method = "loess", lty = 3, se = FALSE) + 
  geom_hline(aes(yintercept = 0)) + 
  theme_minimal() + 
  labs(title = "Time of arrival model",
       x = "dist_traveled",
       y = "Completion probability") +
  theme(legend.title = element_blank()) + 
  scale_y_continuous(breaks = scales::pretty_breaks())