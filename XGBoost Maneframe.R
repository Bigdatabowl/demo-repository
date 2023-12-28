library(tidyverse)
library(ggplot2)
library(gganimate)
library(nflverse)
library(tidymodels)
library(stacks)
library(ranger)


plays <- read.csv('plays.csv')
final_data <- readRDS('final_data.RDS')

yards <- plays %>% 
  select(gameId, playId, playResult, passResult)

final_data <- final_data %>% 
  filter(week > 1) %>% 
  left_join(yards) %>% 
  mutate(offenseFormation = as.factor(offenseFormation),
         down = as.factor(down)) 
rushing_data <- final_data %>% filter(passResult == '')
passing_data <- final_data %>% filter(passResult != '')



####
#Modeling
####

nfl_split <- make_splits(
  rushing_data %>% filter(week <= 7),
  rushing_data %>% filter(week > 7))



nfl_training <- training(nfl_split)
nfl_test <- testing(nfl_split)

nfl_recipe <- recipe(playResult ~ offenseFormation + down + defendersInTheBox + 
                       yardsToGo + yardstoEnd + Scorediff + run_yards_per_attempt_off + 
                       run_yards_per_attempt_def, data = nfl_training) %>% 
  step_dummy(all_nominal_predictors())

nfl_prep <- prep(nfl_recipe)
nfl_juice <- juice(nfl_prep)


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

nfl_xgb_best_tune_tidy <- nfl_xgb_tune_results %>%
  select_best("rmse") 

nfl_final_xgb_tidy <- finalize_model(nfl_xgb, nfl_xgb_best_tune_tidy)

#Note that we need to update our workflow
nfl_xgb_wf2 <- workflow() %>% 
  add_recipe(nfl_recipe) %>% 
  add_model(nfl_final_xgb_tidy)
nfl_xgb_wf2

final_xgb_tidy <- nfl_xgb_wf2 %>% 
  last_fit(nfl_split, metrics = metric_set(rmse))

xgboost.tidy.nfl.rmse <- final_xgb_tidy %>% collect_predictions() %>% rmse(estimate=.pred, truth=playResult) %>% pull(.estimate)


### Random Forest
##Fit model with tuning grid
nfl_ranger_tune <- rand_forest(trees = 150,
                               mtry = tune(),
                               min_n = tune()) %>%
  set_mode("regression") %>%
  set_engine("ranger", seed = 123, num.threads = 7)

nfl_ranger_wf <- workflow() %>% 
  add_recipe(nfl_recipe) %>% 
  add_model(nfl_ranger_tune)

tuneboth_param <- parameters(nfl_ranger_tune) %>% 
  update(mtry = mtry(c(1, 15)))

nfl_grid_tune <- grid_latin_hypercube(tuneboth_param, size=10)

nfl.ranger.final <- ranger(playResult ~ ., data = nfl_training,
                           num.trees       = 2000,
                           mtry            = 9,
                           min.node.size   = 10,
                           replace         = TRUE,
                           sample.fraction = 1,
                           seed            = 123,
                           respect.unordered.factors = 'order',
                           importance      = 'impurity'
)

nfl.ranger.final

nfl.ranger.testpred <- predict(nfl.ranger.final, data = nfl_test)
##Test data RMSE
rmse_vec(estimate=nfl.ranger.testpred$predictions, truth=pull(nfl_test[,c("playResult")]))