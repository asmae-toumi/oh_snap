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

trained_plus_phat_time_of_throw %>%
  filter(number_of_pass_rushers %in% 2:7) %>% 
  ggplot(aes(time_to_throw, prob_means, color = as.factor(number_of_pass_rushers))) + 
  geom_smooth(method = "loess", se = FALSE) + 
  geom_smooth(aes(time_to_throw, low_bound), method = "loess", lty = 3, se = FALSE) + 
  geom_smooth(aes(time_to_throw, upp_bound), method = "loess", lty = 3, se = FALSE) + 
  geom_hline(aes(yintercept = 0)) + 
  theme_minimal() + 
  labs(title = "Time of throw model",
       subtitle = "time_to_throw by # pass rushers",
       x = "time_to_throw",
       y = "Completion probability") +
  theme(legend.title = element_blank()) + 
  scale_y_continuous(breaks = scales::pretty_breaks())

trained_plus_phat_time_of_throw %>% 
  filter(number_of_pass_rushers %in% 2:7) %>% 
  group_by(number_of_pass_rushers) %>% 
  summarise(ave_prob = mean(prob_means), 
            se_prob = sd(prob_means)/sqrt(n())) %>% 
  ggplot(aes(number_of_pass_rushers, ave_prob)) + 
  geom_bar(stat = "identity", width=0.3, fill = "#132f3c") +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(
    y = "Completion probability", 
    x = "# pass rushers") +
  theme_minimal()

trained_plus_phat_time_of_throw %>% 
  group_by(qb_hit) %>% 
  summarise(ave_prob = mean(prob_means), 
            sd = sd(prob_means),
            se_prob = sd(prob_means)/sqrt(n())) %>% 
  ggplot(aes(qb_hit, ave_prob)) + 
  geom_bar(stat = "identity", width=0.3, fill = "#FFA3AF") +
  geom_errorbar(aes(ymin=ave_prob-sd, ymax=ave_prob+sd), 
                color = "#132f3c", width = 0.05) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(
    y = "Completion probability", 
    x = "qb_hit") +
  theme_minimal()

trained_plus_phat_time_of_throw %>% 
  group_by(is_leading) %>% 
  summarise(ave_prob = mean(prob_means), 
            sd = sd(prob_means),
            se_prob = sd(prob_means)/sqrt(n())) %>% 
  ggplot(aes(is_leading, ave_prob)) + 
  geom_bar(stat = "identity", width=0.3, fill = "#FFA3AF") +
  geom_errorbar(aes(ymin=ave_prob-sd, ymax=ave_prob+sd), 
                color = "#132f3c", width = 0.05) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(
    y = "Completion probability", 
    x = "is_leading") +
  theme_minimal()

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