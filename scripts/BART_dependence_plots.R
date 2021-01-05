library(tidyverse)
library(hrbrthemes)
library(patchwork)

# load trained_plus_phat_time_of_throw and trained_plus_phat_time_of_throw

load("data/BART_time_of_throw/trained_plus_phat_time_of_throw.RData")
load("data/BART_time_of_arrival/trained_plus_phat_time_of_arrival.RData")

# trained_plus_phat_time_of_throw data 

p1 <- trained_plus_phat_time_of_throw %>% 
  ggplot(aes(dist_def1, prob_means)) + 
  geom_smooth(method = "loess", se = FALSE, color = "#132f3c") + 
  geom_smooth(aes(dist_def1, low_bound), method = "loess", lty = 3, se = FALSE, color = "#132f3c") + 
  geom_smooth(aes(dist_def1, upp_bound), method = "loess", lty = 3, se = FALSE, color = "#132f3c") + 
  geom_hline(aes(yintercept = 0)) + 
  theme_minimal() + 
  labs(title = "Time of throw model",
       x = "Distance to nearest defender (yards)",
       y = "Completion probability") +
  theme(legend.title = element_blank()) + 
  theme_ipsum() + ylim(0,1)

p2 <- trained_plus_phat_time_of_throw %>% 
  ggplot(aes(dist_qb, prob_means)) + 
  geom_smooth(method = "loess", se = FALSE, color = "#132f3c") + 
  geom_smooth(aes(dist_qb, low_bound), method = "loess", lty = 3, se = FALSE, color = "#132f3c") + 
  geom_smooth(aes(dist_qb, upp_bound), method = "loess", lty = 3, se = FALSE, color = "#132f3c") + 
  geom_hline(aes(yintercept = 0)) + 
  labs(x = "Distance from QB (yards)",
       y = "Completion probability") +
  theme(legend.title = element_blank()) + 
  theme_ipsum() + ylim(0,1)

p3 <- trained_plus_phat_time_of_throw %>% 
  ggplot(aes(time_to_throw, prob_means)) + 
  geom_smooth(method = "loess", se = FALSE, color = "#132f3c") + 
  geom_smooth(aes(time_to_throw, low_bound), method = "loess", lty = 3, se = FALSE, color = "#132f3c") + 
  geom_smooth(aes(time_to_throw, upp_bound), method = "loess", lty = 3, se = FALSE, color = "#132f3c") + 
  geom_hline(aes(yintercept = 0)) + 
  theme_ipsum() +
  labs(x = "Time from snap to throw (seconds)",
       y = "Completion probability") +
  theme(legend.title = element_blank())  + ylim(0,1)

# trained_plus_phat_time_of_arrival data 

p4 <- trained_plus_phat_time_of_arrival %>% 
  ggplot(aes(dist_def1, prob_means)) + 
  geom_smooth(method = "loess", se = FALSE,  color = "#FFA3AF") + 
  geom_smooth(aes(dist_def1, low_bound), method = "loess", lty = 3, se = FALSE,  color = "#FFA3AF") +
  geom_smooth(aes(dist_def1, upp_bound), method = "loess", lty = 3, se = FALSE,  color = "#FFA3AF") +
  geom_hline(aes(yintercept = 0)) + 
  labs(title = "Time of arrival model",
       x = "Distance to nearest defender (yards)",
       y = "Completion probability") +
  theme_ipsum() + ylim(0,1)

p5 <- trained_plus_phat_time_of_arrival %>% 
  ggplot(aes(receiver_speed, prob_means)) + 
  geom_smooth(method = "loess", se = FALSE,  color = "#FFA3AF") +
  geom_smooth(aes(receiver_speed, low_bound), method = "loess", lty = 3, se = FALSE,  color = "#FFA3AF") +
  geom_smooth(aes(receiver_speed, upp_bound), method = "loess", lty = 3, se = FALSE,  color = "#FFA3AF") +
  geom_hline(aes(yintercept = 0)) + 
  theme_ipsum() +
  labs(
       x = "Receiver speed (yards/seconds)",
       y = "Completion probability") +
  theme(legend.title = element_blank()) + ylim(0,1)

p6 <- trained_plus_phat_time_of_arrival %>% 
  ggplot(aes(dist_traveled, prob_means)) + 
  geom_smooth(method = "loess", se = FALSE,  color = "#FFA3AF") +
  geom_smooth(aes(dist_traveled, low_bound), method = "loess", lty = 3, se = FALSE,  color = "#FFA3AF") +
  geom_smooth(aes(dist_traveled, upp_bound), method = "loess", lty = 3, se = FALSE,  color = "#FFA3AF") +
  geom_hline(aes(yintercept = 0)) + 
  theme_ipsum(caption_size = 12) + 
  labs(
       x = "Distance traveled by receiver (yards)",
       y = "Completion probability", 
       caption = "Dotted lines represent 95% credible intervals") +
  theme(legend.title = element_blank()) + ylim(0,1)

panel <- (p1 | p2 | p3) / (p4 | p5 | p6)

panel




