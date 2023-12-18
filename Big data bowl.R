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
week1rushplayer <- week1rushplay[week1rushplay$displayName != "football", ]
week1rushfootball <- week1rushplay[week1rushplay$displayName == "football", ]

week1dist <- merge(week1rushplayer, week1rushfootball, by = c("gameId", "playId", "frameId", "time"), all.x = T)
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
week1dist <- week1dist %>% 
  select(-25)

week1tackledist <- left_join(week1dist, tackle_rushplay, by = c("gameId", "nflId", "playId"))
week1tackledist <- week1tackledist %>% 
  select(-30, -31, -43:-48)

week1tackledist <- week1tackledist %>% 
  group_by(gameId, playId, event, frameId) %>% 
  mutate(player_order = rank(distance)) %>% 
  ungroup

View(week1tackledist %>% filter(playId == 101 & gameId == 2022090800))


frame_diff <- week1tackledist %>% 
  select(gameId, playId, frameId, event) %>% 
  group_by(gameId) %>% 
  distinct(frameId, playId, event) %>% 
  ungroup() %>% 
  group_by(gameId, playId) %>% 
  filter(event == 'ball_snap' | event == 'first_contact' | event == 'tackle' | event == 'touchdown') %>% 
  pivot_wider(names_from = event, values_from = frameId) %>% 
  mutate(tackle_fc = tackle - first_contact) %>% 
  left_join(tackles, by = c("gameId", "playId")) %>% 
  select(1:7, forcedFumble, pff_missedTackle)

players <- players %>% 
  mutate(OD = as.factor(ifelse(position %in% c("QB", "T", "TE", "WR", "G", "RB", "C", "FB", "LS"), "Offense", "Defense")))

new_players <- data.frame(
  displayName = c("Robby Anderson", "Cameron Sample", "Zachary Carter", "Daxton Hill", "Jacob Martin"),
  OD = c("Offense", "Defense", "Defense", "Defense", "Defense"),
  position = c("WR", "DE", "DT", "SS", "DE"),
  nflId = c(43808, 53540, 54560, 54496, 46255)
)

players <- bind_rows(players, new_players)


##week1tackdist and players
week1tackledistpos <- left_join(week1tackledist, 
                                players %>% select(nflId, displayName, OD), 
                                by = c("nflId", "displayName"))


#join plays(from plays only select gameid, playid, ballcarrierId) and week1tackdist
week1ballcarrier <- left_join(week1tackledistpos,
                              plays %>% select(gameId, playId, ballCarrierId),
                              by = c("gameId", "playId"))
week1ballcarrier <- week1ballcarrier %>%
  select(-32) %>% 
  rename(ballCarrierId = ballCarrierId.y)

#after joining mutate new column for who the ballcarrier is, if ballcarrierId == playerID then set it equal to 1
week1ballcarrier <- week1ballcarrier %>% 
  mutate(BallCarrier = ifelse(ballCarrierId == nflId, 1, 0))

week1ballcarrier <- week1ballcarrier %>% 
  filter(OD == "Defense" | (OD =="Offense" & BallCarrier == 1))


###EDA
tackle_assist <- week1ballcarrier %>%
  group_by(quarter, down, yardsToGo, defendersInTheBox) %>%
  summarise(
    total_tackle = sum(tackle, na.rm = TRUE),
    total_assist = sum(assist, na.rm = TRUE)
  ) %>%
  ungroup()

ggplot(tackle_assist, aes(x = quarter, y = total_tackle, fill = as.factor(down))) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  labs(title = "Total Tackles by Quarter and Down",
       x = "Quarter",
       y = "Total Tackles",
       fill = "Down") +
  theme_minimal()

ggplot(tackle_assist, aes(x = quarter, y = total_assist, fill = as.factor(down))) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  labs(title = "Total Assists by Quarter and Down",
       x = "Quarter",
       y = "Total Assists",
       fill = "Down") +
  theme_minimal()

missing_values <- tackle_assist[is.na(tackle_assist$total_tackle) | is.na(tackle_assist$total_assist), ]

print(missing_values)

ggplot(tackle_assist, aes(x = yardsToGo, y = total_tackle, fill = as.factor(down))) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  labs(title = "Total Tackles by YardsToGo, Down, and Defenders in the Box",
       x = "YardsToGo",
       y = "Total Tackles",
       fill = "Down") +
  facet_grid(quarter ~ defendersInTheBox) +
  theme_minimal()

ggplot(tackle_assist, aes(x = yardsToGo, y = total_assist, fill = as.factor(down))) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  labs(title = "Total Assists by YardsToGo, Down, and Defenders in the Box",
       x = "YardsToGo",
       y = "Total Assists",
       fill = "Down") +
  facet_grid(quarter ~ defendersInTheBox) +
  theme_minimal()


tackle_assist_2 <- week1ballcarrier %>%
  group_by(quarter, down, playDirection, yardsToGo) %>%
  summarise(
    total_tackle = sum(tackle, na.rm = TRUE),
    total_assist = sum(assist, na.rm = TRUE)
  ) %>%
  ungroup()

ggplot(tackle_assist_2, aes(x = yardsToGo, y = total_tackle, fill = as.factor(down))) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  labs(title = "Total Tackles by PlayDirection, Distance, and Down",
       x = "Distance",
       y = "Total Tackles",
       fill = "Down") +
  facet_grid(quarter ~ playDirection) +
  theme_minimal()

ggplot(tackle_assist_2, aes(x = yardsToGo, y = total_assist, fill = as.factor(down))) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  labs(title = "Total Assists by PlayDirection, Distance, and Down",
       x = "Distance",
       y = "Total Assists",
       fill = "Down") +
  facet_grid(quarter ~ playDirection) +
  theme_minimal()
# football_pos <- week1rushplay %>% 
#   filter(club=="football")
# unique_football <- football_pos %>% 
#   distinct(gameId, playId)
# 


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
