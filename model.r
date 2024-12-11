library(tidyverse)
library(dplyr)
library(survivoR)
library(olsrr)
library(VGAM)
library(dplyr)


top_5 <- castaways %>% group_by(version_season) %>%
  dplyr::summarise(top = max(order))

final_5_episode <- boot_mapping %>% filter(final_n == 5) %>%
  group_by(version_season) %>%
  mutate(total_episodes = max(episode)) %>%
  ungroup() %>%
  dplyr::select(version_season, total_episodes) %>%
  distinct()

# get confessional count
castaway_confessionals <- confessionals %>% group_by(castaway_id, version_season, episode, confessional_count) %>%
  merge(final_5_episode, by = "version_season") %>%
  filter(episode <= total_episodes) %>%
  group_by(version_season, castaway_id, total_episodes) %>%
  dplyr::summarise(total_confessional_count = sum(confessional_count),
            confessional_per_ep = total_confessional_count/total_episodes) %>%
  distinct()

#get immunity and reward wins
challenge_wins <- challenge_results %>% filter(result == 'Won') %>%
  merge(final_5_episode, by = "version_season") %>%
  filter(episode <= total_episodes) %>%
  mutate(immunity_win_ind = case_when(grepl("immunity", challenge_type, ignore.case = TRUE) ~ 1,
                                      TRUE ~ 0),
         reward_win_ind = case_when(grepl("reward", challenge_type, ignore.case = TRUE) ~ 1,
                                    TRUE ~ 0)) %>%
  group_by(version_season, castaway_id) %>%
  dplyr::summarise(reward_wins = sum(reward_win_ind),
            immunity_wins = sum(immunity_win_ind))

top_castaways_raw <-castaways %>% filter(version == 'US') %>% #us seasons only
  merge(top_5, by= "version_season") %>%
  filter(order >= top - 4) %>%
  mutate(rank = case_when(order == top ~ 1,
                          order == top - 1 ~ 2,
                          order == top - 2 ~ 3,
                          order == top - 3 ~ 4,
                          order == top - 4 ~ 5
  )) %>%
  dplyr::select(castaway_id, version_season, season, rank, age) %>%
  merge(castaway_details, by="castaway_id") %>%
  dplyr::select(castaway_id, full_name, version_season, season, rank, age, gender) %>%
  merge(castaway_confessionals, by = c('version_season', 'castaway_id'))%>%
  merge(challenge_wins, by = c('version_season', 'castaway_id')) 

top_castaways <- top_castaways_raw %>% filter(season <= 40)

model <- vglm(rank ~ I(gender) +immunity_wins + total_confessional_count, data = top_castaways, family=cumulative(parallel=F))
summary(model)

survivor44 <- read_csv("survivor44.csv")

predict(model, survivor44, type="response")  

