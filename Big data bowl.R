library(tidyverse)
library(ggplot2)
library(gganimate)
library(nflverse)
library(tidymodels)
library(stacks)


#Export playResult_w_pred, left_join with tackle play, and then we can see how many yards 
#each player saves per tackle on avg.
pred_actual <- left_join(playResult_w_pred, tackleplay, by = c("gameId", "playId"))
pred_actual <- pred_actual %>% 
  mutate(save = predicted - playResult)

# Find distance between ball snap and tackle, to see which players run the most to make tackles.
# then we can group by positions and see which players run the most out of dline to make tackles,
# which players run the most out of linebackers and which players run the most out of the secondary 
# to make tackles




setwd("C:/Users/roymy/OneDrive/바탕화~2-DESKTOP-TTPA583-6709/Big data bowl")
setwd("C:/Users/dhaks/OneDrive/Desktop/Big Data Bowl")
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

pbp <- load_pbp(seasons = 2022)
team_box_def <- pbp %>% 
  group_by(game_id, defteam, play_type, week) %>% 
  summarise(total_yards = sum(yards_gained),
            total_plays = n()) %>% 
  ungroup() %>% 
  filter(play_type %in% c('pass', 'run')) %>% 
  mutate(yards_per_attempt_def = total_yards / total_plays) %>% 
  pivot_wider(names_from = play_type, names_glue = "{play_type}_{.value}",values_from = c(total_yards, total_plays, yards_per_attempt_def)) %>% 
  filter(week<=9)

team_box_off <- pbp %>% 
  group_by(game_id, posteam, play_type, week) %>% 
  summarise(total_yards = sum(yards_gained),
            total_plays = n()) %>% 
  ungroup() %>% 
  filter(play_type %in% c('pass', 'run')) %>% 
  mutate(yards_per_attempt_off = total_yards / total_plays) %>% 
  pivot_wider(names_from = play_type, names_glue = "{play_type}_{.value}",values_from = c(total_yards, total_plays, yards_per_attempt_off)) %>% 
  filter(week<=9)

off_box <- team_box_off %>% 
  group_by(posteam) %>%
  arrange(week) %>% 
  mutate(across(pass_total_yards:run_total_plays, cumsum, .names = "season_total_{col}")) %>% 
  mutate(pass_yards_per_attempt_off = lag(season_total_pass_total_yards/season_total_pass_total_plays),
         run_yards_per_attempt_off = lag(season_total_run_total_yards/season_total_run_total_plays))

def_box <- team_box_def %>% 
  group_by(defteam) %>%
  arrange(week) %>% 
  mutate(across(pass_total_yards:run_total_plays, cumsum, .names = "season_total_{col}")) %>% 
  mutate(pass_yards_per_attempt_def = lag(season_total_pass_total_yards/season_total_pass_total_plays),
         run_yards_per_attempt_def = lag(season_total_run_total_yards/season_total_run_total_plays))


tackler <- left_join(tackles, players, by = "nflId")
tackleplay <- left_join(tackler, plays, by = c("gameId", "playId")) %>% 
  select(gameId, playId, nflId, ballCarrierId, tackle, assist, displayName, passResult)

tackleplay <- tackleplay %>%  ########################### JOIN WITH THIS
  rename("tacklePlayer" = "displayName")

tackle_passplay <- tackleplay %>% 
  filter(passResult == "C")

tackle_rushplay <- tackleplay %>% 
  filter(passResult == "")

tackle_scramble <- tackleplay %>% 
  filter(passResult == "R")

players <- players %>% 
  mutate(OD = as.factor(ifelse(position %in% c("QB", "T", "TE", "WR", "G", "RB", "C", "FB", "LS"), "Offense", "Defense")))

new_players <- data.frame(
  displayName = c("Robby Anderson", "Cameron Sample", "Zachary Carter", "Daxton Hill", "Jacob Martin"),
  OD = c("Offense", "Defense", "Defense", "Defense", "Defense"),
  position = c("WR", "DE", "DT", "SS", "DE"),
  nflId = c(43808, 53540, 54560, 54496, 46255)
)

players <- bind_rows(players, new_players)


all_play <- function(week){
  
  weekrushtackle <-tackle_rushplay %>%
    filter(gameId %in% week$gameId & playId %in% week$playId)
  
  weekrushtackle <- left_join(weekrushtackle, players, by =  c("tacklePlayer" = "displayName")) %>% 
    select(-9,-10,-11,-12,-13) %>% 
    rename("nflId" = "nflId.x") 
  
  weekrushplay <- semi_join(week, weekrushtackle, by = c("gameId", "playId"))
  weekrushplayer <- weekrushplay[weekrushplay$displayName != "football", ]
  weekrushfootball <- weekrushplay[weekrushplay$displayName == "football", ]
  
  weekdist <- merge(weekrushplayer, weekrushfootball, by = c("gameId", "playId", "frameId", "time"), all.x = T)
  weekdist <- weekdist %>% 
    select(-18, -20, -21, -22, -30)
  
  weekdist <- weekdist %>% 
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
  
  weekdist$distance <- sqrt((weekdist$x.player - weekdist$x.ball)^2 + (weekdist$y.player - weekdist$y.ball)^2)
  
  
  weektackledist <- left_join(weekdist, tackle_rushplay, by = c("gameId", "nflId", "playId"))
  
  weektackledist <- weektackledist %>% 
    group_by(gameId, playId, event, frameId) %>% 
    mutate(player_order = rank(distance)) %>% 
    ungroup
  
  ##View(weektackledist %>% filter(playId == 101 & gameId == 2022090800))
  
  
  # frame_diff <- weektackledist %>% 
  #   select(gameId, playId, frameId, event) %>% 
  #   group_by(gameId) %>% 
  #   distinct(frameId, playId, event) %>% 
  #   ungroup() %>% 
  #   group_by(gameId, playId) %>% 
  #   filter(event == 'ball_snap' | event == 'first_contact' | event == 'tackle' | event == 'touchdown') %>% 
  #   pivot_wider(names_from = event, values_from = frameId) %>% 
  #   mutate(tackle_fc = tackle - first_contact) %>% 
  #   left_join(tackles, by = c("gameId", "playId")) %>% 
  #   select(1:7, forcedFumble, pff_missedTackle)
  
  
  
  ##weektackdist and players
  weektackledistpos <- left_join(weektackledist, 
                                 players %>% select(nflId, displayName, OD, position), 
                                 by = c("nflId", "displayName"))
  
  
  #join plays(from plays only select gameid, playid, ballcarrierId) and weektackdist
  weekballcarrier <- left_join(weektackledistpos,
                               plays %>% select(gameId, playId, preSnapHomeScore, preSnapVisitorScore, yardlineNumber, 
                                                preSnapHomeScore, preSnapVisitorScore, yardlineSide, 
                                                possessionTeam, defensiveTeam, offenseFormation,
                                                down, defendersInTheBox, yardsToGo),
                               by = c("gameId", "playId"))
  
  weekballcarrier <- weekballcarrier %>% 
    group_by(gameId, playId)%>%
    mutate(ballCarrierId = first(ballCarrierId, na_rm = T)) 
  
  #after joining mutate new column for who the ballcarrier is, if ballcarrierId == playerID then set it equal to 1
  weekballcarrier <- weekballcarrier %>% 
    mutate(BallCarrier = ifelse(ballCarrierId == nflId, 1, 0))
  
  weekballcarrier <- weekballcarrier %>% 
    filter(OD == "Defense" | (OD =="Offense" & BallCarrier == 1))
  
  weekballcarrier <- left_join(weekballcarrier,
                               games %>% select(gameId, week, gameDate, homeTeamAbbr, visitorTeamAbbr),
                               by = c("gameId"))
  
  
  ballcarrier <- weekballcarrier %>% 
    filter(event %in% c("ball_snap", "first_contact", "tackle"))
  
  newdistance_change <- ballcarrier %>% 
    filter(displayName == tacklePlayer) %>% 
    group_by(gameId, playId) %>% 
    filter(event %in% c("ball_snap", "tackle")) %>% 
    mutate(
      new_x = lag(x.player),
      new_y = lag(y.player),
      distance_covered = sqrt((new_x - x.player)^2 + (new_y - y.player)^2) 
    )
  
  newdistance_change <- newdistance_change %>% 
    filter(event == "tackle") %>% 
    select(gameId, playId, nflId, displayName, week, homeTeamAbbr, visitorTeamAbbr, preSnapHomeScore, preSnapVisitorScore, distance_covered)
  
  
  
  # distance_change <- distance_change %>% 
  #   mutate(yards_to_FD = yardsToGo - last(playResult))
  
  distance_change <- ballcarrier %>%
    arrange(gameId, playId, nflId, time) %>%
    group_by(gameId, playId, nflId, displayName) %>%
    filter(event %in% c("ball_snap", "first_contact", "tackle")) %>%
    mutate(
      distance_change_first_contact = distance - lag(distance), # bs - fc
      distance_change_tackle = distance - lag(distance, default = first(distance)), # fc - tack
      distance_change_snap_tackle = sum(distance_change_first_contact, distance_change_tackle, na.rm = TRUE)
    )
  
  distance_change <- distance_change %>% 
    rename(Home = homeTeamAbbr,
           Visitor = visitorTeamAbbr,
           Home_score = preSnapHomeScore,
           Visitor_score = preSnapVisitorScore)
  
  distance_change <- distance_change %>% 
    mutate(yardstoEnd = ifelse(
      possessionTeam == yardlineSide,
      100 - yardlineNumber,
      yardlineNumber
    ),
    Scorediff = ifelse(
      possessionTeam == Home,
      Home_score - Visitor_score,
      Visitor_score - Home_score
    )
    )  
  
  
  
  
  ##week passing play
  weekpasstackle <- tackle_passplay %>% 
    filter(gameId %in% week$gameId & playId %in% week$playId)
  
  weekpasstackle <- left_join(weekpasstackle, players, by =  c("tacklePlayer" = "displayName")) %>% 
    select(-9,-10,-11,-12,-13) %>% 
    rename("nflId" = "nflId.x") 
  
  weekpassplay <- semi_join(week, weekpasstackle, by = c("gameId", "playId"))
  weekpassplayer <- weekpassplay[weekpassplay$displayName != "football", ]
  weekpassfootball <- weekpassplay[weekpassplay$displayName == "football", ]
  
  weekpassdist <- merge(weekpassplayer, weekpassfootball, by = c("gameId", "playId", "frameId", "time"), all.x = T)
  weekpassdist <- weekpassdist %>% 
    select(-18, -20, -21, -22, -30)
  
  weekpassdist <- weekpassdist %>% 
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
  
  weekpassdist$distance <- sqrt((weekpassdist$x.player - weekpassdist$x.ball)^2 + (weekpassdist$y.player - weekpassdist$y.ball)^2)
  
  weektacklepassdist <- left_join(weekpassdist, tackle_passplay, by = c("gameId", "nflId", "playId"))
  
  
  weektacklepassdist <- weektacklepassdist %>% 
    group_by(gameId, playId, event, frameId) %>% 
    mutate(player_order = rank(distance)) %>% 
    ungroup
  
  ##weektackdist and players
  weektacklepassdistpos <- left_join(weektacklepassdist, 
                                     players %>% select(nflId, displayName, OD, position), 
                                     by = c("nflId", "displayName"))
  
  
  #join plays(from plays only select gameid, playid, ballcarrierId) and weektackdist
  weekpasscarrier <- left_join(weektacklepassdistpos,
                               plays %>% select(gameId, playId, preSnapHomeScore, preSnapVisitorScore, yardlineNumber, yardlineSide,
                                                preSnapHomeScore, preSnapVisitorScore, possessionTeam, defensiveTeam, offenseFormation,
                                                down, defendersInTheBox, yardsToGo),
                               by = c("gameId", "playId"))
  
  weekpasscarrier <- weekpasscarrier %>% 
    group_by(gameId, playId)%>%
    mutate(ballCarrierId = first(ballCarrierId, na_rm = T))
  
  #after joining mutate new column for who the ballcarrier is, if ballcarrierId == playerID then set it equal to 1
  weekpasscarrier <- weekpasscarrier %>% 
    mutate(BallCarrier = ifelse(ballCarrierId == nflId, 1, 0))
  
  weekpasscarrier <- weekpasscarrier %>% 
    filter(OD == "Defense" | (OD =="Offense" & BallCarrier == 1))
  
  weekpasscarrier <- left_join(weekpasscarrier,
                               games %>% select(gameId, week, gameDate, homeTeamAbbr, visitorTeamAbbr),
                               by = c("gameId"))
  
  passcarrier <- weekpasscarrier %>% 
    filter(event %in% c("pass_arrived", "pass_outcome_caught", "first_contact", "tackle"))
  
  
  newdistance_change_pass <- passcarrier %>% 
    filter(displayName == tacklePlayer) %>% 
    group_by(gameId, playId) %>% 
    filter(event %in% c("pass_outcome_caught", "tackle")) %>% 
    mutate(
      new_x = lag(x.player),
      new_y = lag(y.player),
      distance_covered = sqrt((new_x - x.player)^2 + (new_y - y.player)^2) 
    )
  
  newdistance_change_pass <- newdistance_change_pass %>% 
    filter(event == "pass_outcome_caught") %>% 
    select(gameId, playId, nflId, displayName, week, homeTeamAbbr, visitorTeamAbbr, preSnapHomeScore, preSnapVisitorScore, distance_covered)
  
  distance_change_pass <- passcarrier %>%
    arrange(gameId, playId, nflId, time) %>%
    group_by(gameId, playId, nflId, displayName) %>%
    filter(event %in% c("pass_arrived", "pass_outcome_caught", "first_contact", "tackle")) %>%
    mutate(
      distance_change_first_contact = distance - lag(distance), # caught - fc
      distance_change_tackle = distance - lag(distance, default = first(distance)), # fc - tackle
      distance_change_caught_tackle = sum(distance_change_first_contact, distance_change_tackle, na.rm = TRUE)
    )
  
  
  
  # distance_change <- distance_change %>% 
  #   mutate(yards_to_FD = yardsToGo - last(playResult))
  
  
  distance_change_pass <- distance_change_pass %>% 
    rename(Home = homeTeamAbbr,
           Visitor = visitorTeamAbbr,
           Home_score = preSnapHomeScore,
           Visitor_score = preSnapVisitorScore)
  
  distance_change_pass <- distance_change_pass %>% 
    mutate(yardstoEnd = ifelse(
      possessionTeam == yardlineSide,
      100 - yardlineNumber,
      yardlineNumber
    ),
    Scorediff = ifelse(
      possessionTeam == Home,
      Home_score - Visitor_score,
      Visitor_score - Home_score
    )
    )
  
  distance <- bind_rows(distance_change, distance_change_pass)
  newdistance <- bind_rows(newdistance_change, newdistance_change_pass)
  
  model_plays <- distance%>% 
    group_by(gameId, playId, offenseFormation, possessionTeam, defensiveTeam, week) %>% 
    summarize(down = mean(down, na.rm = T),
              defendersInTheBox = mean(defendersInTheBox, na.rm = T),
              yardsToGo = mean(yardsToGo, narm = T),
              yardstoEnd = mean(yardstoEnd, na.rm = T),
              Scorediff = mean(Scorediff, na.rm = T),
    ) %>% 
    na.omit()
  
  ##### 
  #Test
  #####
  
  model_data <- model_plays %>% 
    left_join(off_box %>% select(posteam, week, pass_yards_per_attempt_off, run_yards_per_attempt_off),
              by = join_by(possessionTeam == posteam, week == week)) %>% 
    left_join(def_box %>% select(defteam, week, pass_yards_per_attempt_def, run_yards_per_attempt_def),
              by = join_by(defensiveTeam == defteam, week == week))
  
  return (list(model_data, newdistance))
}

all_plays <- list()

for (n in 1:9) {
  week_data <- get(paste0("week", n))
  all_plays[[n]] <- all_play(week_data)
}

saveRDS(all_plays, 'all_plays.RDS')
final_data <- data.frame()
distance <- data.frame()
for (n in 1:9) {
  final_data <- rbind(final_data, all_plays[[n]][[1]])
  distance <- rbind(distance, all_plays[[n]][[2]])
}

final_data <- bind_rows(all_plays)
save(final_data, file = 'finaldata.Rdata')
load('finaldata.Rdata')
saveRDS(distance, 'distance.rds')
yards <- plays %>% 
  select(gameId, playId, playResult, passResult)

final_data <- final_data %>% 
  filter(week > 1) %>% 
  left_join(yards) %>% 
  mutate(offenseFormation = as.factor(offenseFormation),
         down = as.factor(down)) 
saveRDS(final_data, 'final_data.RDS')
final_data <- readRDS('final_data.RDS')
rushing_data <- final_data %>% filter(passResult == '')
passing_data <- final_data %>% filter(passResult != '')



####
#Modeling
####

nfl_split <- make_splits(
  passing_data %>% filter(week <= 7),
  passing_data %>% filter(week > 7))



nfl_training <- training(nfl_split)
nfl_test <- testing(nfl_split)

nfl_recipe <- recipe(playResult ~ offenseFormation + down + defendersInTheBox + 
                       yardsToGo + yardstoEnd + Scorediff + pass_yards_per_attempt_off + 
                       pass_yards_per_attempt_def, data = nfl_training) %>% 
  step_dummy(all_nominal_predictors())

nfl_prep <- prep(nfl_recipe)
nfl_juice <- juice(nfl_prep)

nfl_lm <- linear_reg() %>% 
  set_mode('regression') %>% 
  set_engine('lm')

nfl_workflow <- workflow() %>% 
  add_recipe(nfl_recipe) %>% 
  add_model(nfl_lm)

nfl_train_model <- nfl_workflow %>% fit(nfl_training)

test_pred <- nfl_workflow %>% 
  last_fit(nfl_split) %>% 
  collect_predictions()

rmse(test_pred, playResult, .pred)

final_model <- extract_fit_parsnip(nfl_train_model)

bake(nfl_prep, final_data)
#penalized regression
nfl_glm <- linear_reg(
  penalty = tune(),
  mixture = tune()
) %>% 
  set_mode('regression') %>% 
  set_engine('glmnet')

nfl_pen_workflow <- workflow() %>% 
  add_recipe(nfl_recipe) %>% 
  add_model(nfl_glm)

tidy_kfolds <- vfold_cv(nfl_training, v = 5, repeats = 5)


nfl_tune <- tune_grid(nfl_pen_workflow,
                      resamples = tidy_kfolds, 
                      grid = 10, 
                      metrics = metric_set(rmse))

nfl_tune %>% show_best('rmse')

nfl_pen_best_tune <- nfl_tune%>% select_best('rmse')

nfl_finalize_wf <- nfl_pen_workflow %>% 
  finalize_workflow(nfl_pen_best_tune)

nfl_pen_test_pred <- nfl_finalize_wf %>% 
  last_fit(nfl_split) %>% 
  collect_predictions() 

rmse(nfl_pen_test_pred, playResult, .pred)

### XGBOOST
nfl_xgb <- boost_tree(
  mode = 'regression',
  mtry = tune(),
  trees = tune(),
  min_n = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune(),
  sample_size = tune(),
  stop_iter = tune()) %>% 
  set_engine("xgboost")

nfl_xgb_param <- extract_parameter_set_dials(nfl_xgb) %>% 
  update(mtry = mtry(c(1, 14)))

xgboost_grid <- grid_max_entropy(
  nfl_xgb_param, size = 16)

nfl_xgb_wf <- workflow() %>% 
  add_recipe(nfl_recipe) %>% 
  add_model(nfl_xgb)

nfl_xgb_tune_results <- tune_grid(
  nfl_xgb_wf,
  resamples = tidy_kfolds,
  grid = xgboost_grid,
  metrics = metric_set(rmse)
)

saveRDS(nfl_xgb_tune_results, "nfl_xgb_tune_results.rds")

#distance_change_pass <- readRDS('distance_pass.rds') %>% 
#Join box score stats by week and posTeam and DefTeam

##https://www.kaggle.com/code/seanyman84/nfl-rush-prediction
##offense formation, defender in the box, down, yardsToGo, yards till the end zone, presnapHometeamscore - presnapvisitorteamscore
##plays data, games data
######

# 1.Analyze yards saved by tackles, build a model to predict yards gained per play
# 2.and analyze tackle success rate based on area of field(open field, behind the line of scrimmage, etc.)
# 3.potentially analyze largest distance covered to make a tackle ()


######

# football_pos <- weekrushplay %>% 
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
#   unique_play <- weekrushplay %>%
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


# weekrushplay$time <- as.POSIXct(weekrushplay$time, format = "%Y-%m-%d %H:%M:%OS")
# 
# 
# nfl <- weekrushplay[weekrushplay$gameId == 2022090800 & weekrushplay$playId == 101, ]
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
# play_data <- week %>%
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


###EDA
tackle_by_team <- weekballcarrier %>% 
  group_by(playId, club, down, yardsToGo) %>% 
  summarise(
    total_tackle = max(tackle, na.rm = TRUE),
    total_assist = max(assist, na.rm = TRUE),
    total_missed = max(pff_missedTackle, na.rm = T)
  ) %>% 
  ungroup() %>% 
  group_by(club, yardsToGo) %>% 
  summarise(
    total_tackle = sum(total_tackle, na.rm = TRUE),
    total_assist = sum(total_assist, na.rm = TRUE),
    total_missed = sum(total_missed, na.rm = T)
  ) %>% 
  mutate(total_attempts = total_tackle+total_missed,
         tackle_per = total_tackle/total_attempts)


ggplot(data = tackle_by_team, aes())

# Make a line graph to show team success rates of tackles based on down and yards to go
tackle_assist <- weekballcarrier %>%
  group_by(playId, quarter, down, yardsToGo, defendersInTheBox) %>%
  summarise(
    total_tackle = sum(tackle, na.rm = TRUE),
    total_assist = sum(assist, na.rm = TRUE),
    total_missed = sum(pff_missedTackle, na.rm = T)
  ) %>%
  ungroup()%>% 
  group_by(quarter, down, yardsToGo) %>% 
  summarise(
    total_tackle = sum(total_tackle, na.rm = TRUE),
    total_assist = sum(total_assist, na.rm = TRUE),
    total_missed = sum(total_missed, na.rm = T)
  )

ggplot(tackle_assist, aes(x = quarter, y = total_missed, fill = as.factor(down))) +
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


tackle_assist_2 <- weekballcarrier %>%
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

# ballcarrier <- weekballcarrier %>%
#   select(gameId, playId, frameId, time, nflId, displayName, club,
#          s.player, x.player, y.player, a.player, dis.player, o.player, dir.player, playDirection,
#          event, x.ball, y.ball, s.ball, a.ball, dis.ball, o.ball, distance,
#          tackle, assist, tacklePlayer, ballCarrier, quarter, down, yardsToGo, playResult)