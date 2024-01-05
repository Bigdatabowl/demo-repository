library(tidyverse)
library(ggplot2)
library(gganimate)
library(nflverse)
library(tidymodels)
library(stacks)
library(ranger)

setwd("C:/Users/roymy/OneDrive/바탕화~2-DESKTOP-TTPA583-6709/Big data bowl")

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
nfl_bake <- bake(nfl_prep, passing_data)

tidy_kfolds <- vfold_cv(nfl_training, v = 5, repeats = 5)

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
  nfl_xgb_param, size = 50)

nfl_xgb_wf <- workflow() %>% 
  add_recipe(nfl_recipe) %>% 
  add_model(nfl_xgb)

# nfl_xgb_tune_results_pass <- tune_grid(
#   nfl_xgb_wf,
#   resamples = tidy_kfolds,
#   grid = xgboost_grid,
#   metrics = metric_set(rmse)
# )

saveRDS(nfl_xgb_tune_results_pass, "nfl_xgb_tune_results_pass.rds")
saveRDS(nfl_xgb_tune_results_rush, "nfl_xgb_tune_results_rush.rds")
nfl_xgb_tune_results_pass <- readRDS(file = "nfl_xgb_tune_results_pass.rds")
nfl_xgb_best_tune_tidy <- nfl_xgb_tune_results_pass %>%
  select_best("rmse") 

nfl_final_wf <- nfl_xgb_wf %>% #this line works the same as finalize_model + add_model
  finalize_workflow(nfl_xgb_best_tune_tidy)

# nfl_final_xgb_tidy <- finalize_model(nfl_xgb, nfl_xgb_best_tune_tidy)
# 
# #Note that we need to update our workflow
# nfl_xgb_wf2 <- workflow() %>% 
#   add_recipe(nfl_recipe) %>% 
#   add_model(nfl_final_xgb_tidy)
# nfl_xgb_wf2

nfl_xgb_test_pred <- nfl_final_wf %>% 
  last_fit(nfl_split) %>% 
  collect_predictions() 

rmse(nfl_xgb_test_pred, playResult, .pred) #9.73

# xgboost.tidy.nfl.rmse <- final_xgb_tidy %>% collect_predictions() %>% rmse(estimate=.pred, truth=playResult) %>% pull(.estimate)
# 
# 
# xgb_final_workflow <- nfl_xgb_wf2 %>% 
#   finalize_workflow(nfl_xgb_best_tune_tidy)
# xgb_final_workflow
# 
# xgb_fit <- xgb_final_workflow %>%
#   finalize_workflow(nfl_xgb_tune_results %>% select_best("rmse")) %>%
#   last_fit(nfl_split, 
#            metrics = metric_set(rmse)) %>% 
#   collect_metrics() %>% 
#   select(-c(".estimator", ".config")) %>%
#   rename(xgb_estimates = .estimate)
#
# predict_xgb <- xgb_final_workflow %>% 
#   last_fit(nfl_split) 

xgb_final_model <- extract_fit_parsnip(nfl_final_wf %>% fit(nfl_training))

all_plays_xgb_pred_pass <- predict(xgb_final_model, nfl_bake %>% select(-playResult), type = 'numeric') %>% 
  rename('pass_predicted' = '.pred')

all_plays_xgb_pred_rush <- predict(xgb_final_model, nfl_bake %>% select(-playResult), type = 'numeric') %>% 
  rename('rush_predicted' = '.pred')
#all_plays_pen_class <- cbind(predict(nba_pen_final_model, bake(nba_prep, model_data)), model_data$shot_made_flag)
playResult_w_pred_xgb_pass <- cbind(passing_data, all_plays_xgb_pred_pass)
save(playResult_w_pred_xgb_pass, file = "all_plays_xgb_pred_pass.rdata")
load("all_plays_xgb_pred_pass.rdata")
playResult_w_rush <- cbind(rushing_data, all_plays_xgb_pred_rush)
playResult_w_pred <- rbind(playResult_w_pred_xgb_pass, playResult_w_rush)
saveRDS(playResult_w_pred, file = "resultpred_pass_XGB.RDS")


pfun.xgb <- function(model, newdata){
  newData_x <- xgb.DMatrix(data.matrix(newdata), missing = NA)
  results <- predict(model, newData_x)
  return(results)
}

predictor.xgb <- Predictor$new(model = xgb_final_model, data = nfl_bake[,-7], y = nfl_bake[,7],
                               predict.fun = pfun.xgb)

##Feature importance - uses permutations so takes a long time to run, but allows for CI's
imp.xgb <- FeatureImp$new(predictor.xgb, loss = "rmse")
plot(imp.xgb)


#Export playResult_w_pred, left_join with tackle play, and then we can see how many yards 
#each player saves per tackle on avg.

#### XGBoost Model wins


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

# tuneboth_param <- parameters(nfl_ranger_tune) %>% 
#   update(mtry = mtry(c(1, 15)))
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

nfl_ranger_best_tune_tidy <- nfl.ranger.final %>% select_best("rmse")

nfl_final_ranger_tidy <- finalize_model(nfl_ranger_tune, nfl_ranger_best_tune_tidy)

#Note that we need to update our workflow
nfl_ranger_wf3 <- workflow() %>% 
  add_recipe(nfl_rec) %>% 
  add_model(nfl_final_ranger_tidy)
nfl_ranger_wf3


nfl_ranger_wf3 %>% 
  last_fit(nfl_split, metrics = metric_set(rmse)) %>% 
  collect_predictions() %>%
  rmse(estimate=.pred, truth=playResult)

all_plays_rf_pred <- data.frame('prediction' = predict(nfl.ranger.final, final_data, type = 'response')[[1]])

#all_plays_pen_class <- cbind(predict(nba_pen_final_model, bake(nba_prep, model_data)), model_data$shot_made_flag)

playResult_w_pred_RF <- cbind(final_data, all_plays_rf_pred)
saveRDS(playResult_w_pred_RF, file = "resultpred_RF.RDS")


ModelMetrics::rmse( playResult_w_pred$playResult, playResult_w_pred$prediction)
ModelMetrics::rmse(nfl_test$playResult,nfl.ranger.testpred[[1]]) #9.89
