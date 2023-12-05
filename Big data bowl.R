setwd("C:/Users/roymy/OneDrive/바탕화~2-DESKTOP-TTPA583-6709/Big data bowl")
library(tidyverse)
library(ggplot2)
library(gganimate)

games <- read.csv("games.csv")
players <- read.csv("players.csv")
plays <- read.csv("plays.csv")
tackles <- read.csv("tackles.csv")
# week1 <- read.csv("tracking_week_1.csv")
# week2 <- read.csv("tracking_week_2.csv")
# week3 <- read.csv("tracking_week_3.csv")
# week4 <- read.csv("tracking_week_4.csv")
# week5 <- read.csv("tracking_week_5.csv")
# week6 <- read.csv("tracking_week_6.csv")
# week7 <- read.csv("tracking_week_7.csv")
# week8 <- read.csv("tracking_week_8.csv")
# week9 <- read.csv("tracking_week_9.csv")

load("week1.Rdata")
load("week2.Rdata")
load("week3.Rdata")
load("week4.Rdata")
load("week5.Rdata")
load("week6.Rdata")
load("week7.Rdata")
load("week8.Rdata")
load("week9.Rdata")

tackler <- left_join(tackles, players, by = "nflId")
tackleplay <- left_join(tackler, plays, by = c("gameId", "playId"))

tackleplay <- tackleplay %>% 
  select(-birthDate, -collegeName) %>% 
  rename("tacklePlayer" = "displayName") %>% 
  rename("ballCarrier" = "ballCarrierDisplayName")
 
tackle_passplay <- tackleplay %>% 
  filter(passResult == "C")

tackle_rushplay <- tackleplay %>% 
  filter(passResult == "")

tackle_scramble <- tackleplay %>% 
  filter(passResult == "R")

week1rushtackle <-tackle_rushplay %>%
  filter(gameId %in% week1$gameId & playId %in% week1$playId) %>% 
  select(-foulNFLId2) 

week1rushtackle <- left_join(week1rushtackle, players, by = c("foulNFLId1" = "nflId")) %>% 
  select(-height.y, -weight.y, -46, -47) %>% 
  rename("foulposition" = "position.y") %>% 
  rename("foulPlayer" = "displayName")

week1rushplay <- semi_join(week1, week1rushtackle, by = c("gameId", "playId"))
week1rushplay <- week1rushplay[week1rushplay$displayName != "football", ]
week1football <- week1rushplay[week1rushplay$displayName == "football", ]

week1dist <- merge(week1rushplay, week1football, by = c("gameId", "playId", "frameId", "time"), all.x = T)
week1dist <- week1dist %>% 
  select(-18, -20, -21, -22, -30)

week1dist <- week1dist %>% 
  rename(nflId = nflId.x,
         displayName = displayName.x,
         jerseyNumber = jerseyNumber.x,
         club = club.x,
         playDirection = playDirection.x,
         x.player = x.x,
         y.player = y.x,
         s.player = s.x,
         a.player = a.x,
         dis.player = dis.x,
         o.player = o.x,
         dir.player = dir.x,
         event = event.x,
         football = displayName.y,
         x.ball = x.y,
         y.ball = y.y,
         s.ball = s.y,
         a.ball = a.y,
         dis.ball = dis.y,
         o.ball = o.y,
         dir.ball = dir.y)

week1dist$distance <- sqrt((week1dist$x.player - week1dist$x.ball)^2 + (week1dist$y.player - week1dist$y.ball)^2)

football_pos <- week1rushplay %>% 
  filter(club=="football")
unique_football <- football_pos %>% 
  distinct(gameId, playId)


# dist_list <- list()
# 
# for (i in seq_along(unique_football)) {
#   
#   game_id <- unique_football$gameId[i]
#   play_id <- unique_football$playId[i]
#   
#   unique_play <- week1rushplay %>%
#     filter(gameId == game_id, playId == play_id)
#   
#   football_xy <- unique_play %>% 
#     filter(club == "football") %>% 
#     select(x,y)
#   
#   player_xy <- unique_play %>%
#     filter(club != "football") %>%
#     select(displayName, nflId, x, y)
#   
#   distances <- player_xy %>%
#     mutate(distance = sqrt((x - football_xy$x)^2 + (y - football_xy$y)^2))
#   
#   dist_list[[i]] <- distances
# }
#players distance from the ball


# week1rushplay$time <- as.POSIXct(week1rushplay$time, format = "%Y-%m-%d %H:%M:%OS")
# 
# 
# nfl <- week1rushplay[week1rushplay$gameId == 2022090800 & week1rushplay$playId == 101, ]
# nfl_animate <- ggplot(nfl, aes(x = x, y = y, color = club, frame = event)) +
#   geom_point(size = 5) +
#   geom_text(aes(label = jerseyNumber), vjust = 1, hjust = 1) +
#   labs(title = "NFL Player Movement Animation") +
#   transition_states(time, transition_length = 1, state_length = 1) +
#   enter_fade() +
#   exit_fade() +
#   theme_minimal()
# nfl_animate
# anim_save("play_56.gif", animation = nfl_animate, path = "C:/Users/roymy/OneDrive/바탕화~2-DESKTOP-TTPA583-6709/Big data bowl")
# 
# play_data <- week1 %>%
#   group_by(gameId, playId)
# 
# anima_list <- list()
# output <- "C:/Users/roymy/OneDrive/바탕화~2-DESKTOP-TTPA583-6709/Big data bowl/animation"
# 
# for (i in unique(play_data$playId)){
# 
#   play_id <- play_data[play_data$playId == i, ]
#   game_id <- unique(play_id$gameId)
# 
#   nfl_animate <- ggplot(play_id, aes(x = x, y = y, color = club, frame = event)) +
#     geom_point(size = 5) +
#     labs(title = paste("NFL Player Movement Animation - Play ID:", i, "Game ID:", game_id)) +
#     transition_states(time, transition_length = 1, state_length = 1) +
#     enter_fade() +
#     exit_fade() +
#     theme_minimal()
# 
#   anima_list[[i]] <- nfl_animate
#   anim_save(file.path(output, paste("nfl_player_movement_play_", i, ".gif")), animation = nfl_animate)
# }
# 
# for (i in 1:length(anima_list)) {
#   browseURL(file.path(output, paste("nfl_player_movement_play_", i, ".gif")))
# }
