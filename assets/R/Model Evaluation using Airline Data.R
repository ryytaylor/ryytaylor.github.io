install.packages("tidymodels")
install.packages("glmnet")

library("rlang")
library("tidymodels")
library("tidyverse")
library("glmnet")

#replace NA values in the data
flight_delays <- sub_airline %>% 
  replace_na(list(CarrierDelay = 0,
                  WeatherDelay = 0,
                  NASDelay = 0,
                  SecurityDelay = 0,
                  LateAircraftDelay = 0)) %>%
  select(c(ArrDelayMinutes, DepDelayMinutes, CarrierDelay, WeatherDelay, NASDelay, SecurityDelay, LateAircraftDelay, DayOfWeek, Month))

#split the data into training and testing sets
set.seed(1234)
flight_split <- initial_split(flight_delays)
train_data <- training(flight_split)
test_data <- testing(flight_split)

#split the data so 80% of samples will be used for training
flight_split2 <- initial_split(flight_delays, prop = 4/5)  # prop = 0.8 works as well
train_data2 <- training(flight_split2)
test_data2 <- testing(flight_split2)

#pick linear regression and set engine
lm_spec <- linear_reg() %>%
set_engine(engine = "lm")

lm_spec

#fit the trained model
train_fit <- lm_spec %>% 
  fit(ArrDelayMinutes ~ DepDelayMinutes, data = train_data)

train_fit 

#make predicted values in the training set
train_results <- train_fit %>%
predict(new_data = train_data) %>%

#create a new column to save the true values
mutate(truth = train_data$ArrDelayMinutes)
head(train_results)

#create a dataframe with predictions and true values
test_results <- train_fit %>%
predict(new_data = test_data) %>%

mutate(truth = test_data$ArrDelayMinutes)
head(test_results)

#calculate rmse values for the training and test sets
rmse(train_results, truth = truth,
     estimate = .pred)

rmse(test_results, truth = truth,
     estimate = .pred)

#calculate R^2 values for the training and testing sets
rsq(train_results, truth = truth,
    estimate = .pred)

rsq(test_results, truth = truth,
    estimate = .pred)

#create a plot to visualize the accuracy of both sets
test_results %>%
  mutate(train = "testing") %>%
  bind_rows(train_results %>% mutate(train = "training")) %>%
  ggplot(aes(truth, .pred)) +
  geom_abline(lty = 2, color = "orange", 
              size = 1.5) +
  geom_point(color = '#006EA1', 
             alpha = 0.5) +
  facet_wrap(~train) +
  labs(x = "Truth", 
       y = "Predicted Arrival Delays (min)")

#find the R^2 value of the test data using 80% of the training data for arrival delays and departure delays
train_fit2 <- lm_spec %>% 
  fit(ArrDelayMinutes ~ DepDelayMinutes, 
      data = train_data2)
test_results2 <- train_fit2 %>%

predict(new_data = test_data2) %>%

mutate(truth = test_data2$ArrDelayMinutes)
rsq(test_results2, truth = truth,
    estimate = .pred)

#cross-validate the data
set.seed(1234)
cv_folds <- vfold_cv(train_data, v = 10)
results <- fit_resamples(lm_spec, 
                         ArrDelayMinutes ~ DepDelayMinutes,
                         resamples = cv_folds)

#calculate the average RMSE and R^2 of the estimate
results %>% collect_metrics()

#calculate the average RMSE and R^2 using three folds
cv_folds_3 <- vfold_cv(train_data, v = 3)
results <- fit_resamples(
  lm_spec, 
  ArrDelayMinutes ~ DepDelayMinutes, 
  resamples = cv_folds_3)
results %>% collect_metrics()

#underfitting model example
ggplot(cars, aes(x = speed, y = dist)) + 
  geom_point() + 
  geom_hline(yintercept = mean(cars$dist), 
             col = "red") 

#overfitting model example
ggplot(cars, aes(x = speed, y = dist)) + 
  geom_point() + 
  geom_smooth(method = "lm", 
              formula = y ~ poly(x, 8), 
              col = "red", se = FALSE) 

#simplifying the overfit model
ggplot(cars, aes(x = speed, y = dist)) + 
  geom_point() + 
  geom_smooth(method = "lm", 
              formula = y ~ x, 
              col = "red", 
              se = FALSE) 

#create a recipe() that includes the model formula
flight_recipe <-
  recipe(ArrDelayMinutes ~ ., data = train_data)

#specify the model
ridge_spec <- linear_reg(penalty = 0.1, mixture = 0) %>%
  set_engine("glmnet")

#create a workflow object
ridge_wf <- workflow() %>%
  add_recipe(flight_recipe)

#add the ridge model and fit the model
ridge_fit <- ridge_wf %>%
  add_model(ridge_spec) %>%
  fit(data = train_data)

ridge_fit %>%
  extract_fit_parsnip() %>%
  tidy()

#lasso regression example
lasso_spec <- linear_reg(penalty = 0.1, mixture = 1) %>%
  set_engine("glmnet")

lasso_wf <- workflow() %>%
  add_recipe(flight_recipe)

lasso_fit <- lasso_wf %>%
  add_model(lasso_spec) %>%
  fit(data = train_data)

#elastic net regularization example
elasticnet_spec <- linear_reg(penalty = 0.1, mixture = 0.3) %>%
  set_engine("glmnet")

elasticnet_wf <- workflow() %>%
  add_recipe(flight_recipe)

elasticnet_fit <- elasticnet_wf %>%
  add_model(elasticnet_spec) %>%
  fit(data = train_data)

#elastic net regression with "mixture = 0.5" and "penalty = 0.2" using all variables from the training data
flight_recipe <-
  recipe(ArrDelayMinutes ~ ., data = train_data)

el_spec <- linear_reg(penalty = 0.5, mixture = 0.2) %>%
  set_engine("glmnet")

el_wf <- workflow() %>%
  add_recipe(flight_recipe)

el_fit <- el_wf %>%
  add_model(el_spec) %>%
  fit(data = train_data)

el_fit %>%
  pull_workflow_fit() %>%
  tidy()

#grid search with the lasso model
tune_spec <- linear_reg(penalty = tune(), mixture = 1) %>% 
  set_engine("glmnet")

lasso_wf <- workflow() %>%
  add_recipe(flight_recipe)

#define cross validation
flight_cvfolds <- vfold_cv(train_data)

#set up the grid using grid_regular()
lambda_grid <- grid_regular(levels = 50,
                            penalty(range = c(-3, 0.3)))

#tune the grid
lasso_grid <- tune_grid(
  lasso_wf %>% add_model(tune_spec), 
  resamples = flight_cvfolds, 
  grid = lambda_grid)

show_best(lasso_grid, metric = "rmse")

#visualize the RMSE results
lasso_grid %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  ggplot(aes(penalty, mean)) +
  geom_line(size=1, color="red") +
  scale_x_log10() +
  ggtitle("RMSE")

#grid search for the lambda parameter on ridge regression and find the best values
tune_spec <- linear_reg(
  penalty = tune(), 
  mixture = 0) %>% 
  set_engine("glmnet")

ridge_grid <- tune_grid(ridge_wf %>% 
                          add_model(tune_spec), 
                        resamples = flight_cvfolds, 
                        grid = lambda_grid)

show_best(ridge_grid, metric = "rmse")
