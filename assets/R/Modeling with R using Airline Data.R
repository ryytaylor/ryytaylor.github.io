library("tidyverse")

#define dataset with american airlines as the reporting airline
aa_delays <- sub_airline %>%
  filter(CarrierDelay != "NA", Reporting_Airline == "AA")

head(aa_delays)

#create a linear model to show arrival delays and departure delays
linear_model <- lm(ArrDelayMinutes ~ DepDelayMinutes, data = aa_delays)
summary(linear_model)

#input data
new_depdelay <- data.frame(
  DepDelayMinutes = c(12, 19, 24))

#predict data points
pred <- predict(linear_model, newdata = new_depdelay, interval = "confidence")
pred

#find coefficients
linear_model$coefficients

#create a linear model to show arrival delays and carrier delays
linear_model2 <- lm(ArrDelayMinutes ~ CarrierDelay, 
data = aa_delays)

#find coefficients
linear_model2$coefficients              

#create a multiple linear regression model for arrival delays, departure delays, and late aircraft delays
mlr <- lm(ArrDelayMinutes ~ DepDelayMinutes + LateAircraftDelay, data = aa_delays)

summary(mlr)

#find coefficients
mlr$coefficients

#create a multiple linear regression model for arrival delays, departure delays, late aircraft delays, and carrier delays
mlr2 <- lm(
  ArrDelayMinutes ~ DepDelayMinutes + 
    LateAircraftDelay + CarrierDelay, 
  data = aa_delays)

summary(mlr2)  

#find coefficients
mlr2$coefficients

#define new data points
DepDelayMinutes <- c(10, 20, 30)
LateAircraftDelay <- c(20, 60, 30)
new_multidelay <- data.frame(DepDelayMinutes, LateAircraftDelay)

#find predicted values
pred <- predict(mlr, 
                newdata = new_multidelay, 
                interval = "confidence")  
summary(pred)

#create a regression plot for departure delays and arrival delays
ggplot(aa_delays, aes(x = DepDelayMinutes, y = ArrDelayMinutes)) +
  geom_point() + 
  stat_smooth(method = "lm", col = "red")

#create a regression plot for carrier delays and arrival delays
ggplot(
  aa_delays, 
  aes(x = CarrierDelay, y = ArrDelayMinutes)) +
  geom_point() + 
  stat_smooth(method = "lm", col = "red")

#is arrival delays more strongly correlated with departure delays or carrier delays
cor(aa_delays$DepDelayMinutes, 
    aa_delays$ArrDelayMinutes)
cor(aa_delays$CarrierDelay, 
    aa_delays$ArrDelayMinutes)

#create a residual plot
aa_delays <- sub_airline %>%
  filter(CarrierDelay != "NA", Reporting_Airline == "AA")
score_model <- lm(ArrDelayMinutes ~ DepDelayMinutes, data = aa_delays)
aa_delays$predicted <- predict(score_model)

ggplot(aa_delays, aes(x = DepDelayMinutes, y = ArrDelayMinutes)) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Plot regression slope
  geom_segment(aes(xend = DepDelayMinutes, yend = predicted), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()

#create a non-linear plot
ggplot(lm(ArrDelayMinutes ~ DepDelayMinutes, data = aa_delays)) +
  geom_point(aes(x=DepDelayMinutes, y=.resid))

#create a polynomial regression plot
set.seed(20)
x <- seq(from=0, to=20, by=0.1)

y <- 500 + 0.4 * (x-10)^3

noise <- rnorm(length(x), mean=10, sd=80)
noisy.y <- y + noise

#polynomial 2nd order
time <- 6:19
temp <- c(4,6,7,9,10,11,11.5,12,12,11.5,11,10,9,8)

ggplot(data = NULL, aes(time, temp)) + 
  geom_point() 

#create a 4th order polynomial model for time and temp
polyfit4 <- lm(temp ~ poly(time, 4, raw = TRUE))
summary(polyfit4)

#calculate MSE, RMSE, and R^2 - simple linear regression
linear_model <- lm(ArrDelayMinutes ~ DepDelayMinutes, aa_delays)

mse <- mean(linear_model$residuals^2)
mse

rmse <- sqrt(mse)
rmse

summary(linear_model)$r.squared

#calculate MSE, RMSE, and R^2 - multiple linear regression
mlr <- lm(ArrDelayMinutes ~ DepDelayMinutes + LateAircraftDelay, data = aa_delays)

mse_mlr <- mean(mlr$residuals^2)
mse_mlr

rmse_mlr <- sqrt(mse_mlr)
rmse_mlr

summary(mlr)$r.squared

#calculate MSE, RMSE, and R^2 - polynomial regression
poly_reg <- lm(ArrDelayMinutes ~ poly(DepDelayMinutes, 3), data = aa_delays)

mse_poly <- mean(poly_reg$residuals^2)
mse_poly

rmse_poly <- sqrt(mse)
rmse_poly

summary(poly_reg)$r.squared

#predict the score model
head(predict(score_model))














