# load pre-built models

model_small <- readRDS("small_model")
summary(model_small)

model_full <- readRDS("full_diet_model")
summary(model_full)

model_continuous <- readRDS("lm_continuous")


# reclassify true /  false

data <-
  data %>%
  mutate(five_donuts = ifelse(five_donuts == 1, TRUE, FALSE),
         walk = ifelse(walk == 1, TRUE, FALSE),
         run = ifelse(run == 1, TRUE, FALSE),
         wine = ifelse(wine == 1, TRUE, FALSE),
         prot = ifelse(prot == 1, TRUE, FALSE),
         weight = ifelse(weight == 1, TRUE, FALSE))


# create additional features needed for full model

data <-
  data %>%
  mutate(binge = ifelse(data$calories > 5000, TRUE, FALSE))

data <-
  data %>%
  mutate(binge_yesterday = as.logical(lag(data$binge, 1)),
         binge_two_days_ago = as.logical(lag(data$binge, 2)),
         binge_three_days_ago = as.logical(lag(data$binge, 3)),
         yesterday_cals = lag(calories),
         day_before_yest_cals = lag(calories, 2))

glimpse(data)


# fill in NA with 1 as previous day was binge

data[is.na(data)] <- TRUE


# correct using pre-diet calorie intake

data[1, 19] <- 4900
data[2, 20] <- 4900
data[1, 20] <- 3200


# create new dataframe to store predictions

predicted_df <- data.frame(data$date, data$weight_oz, data$change)
predicted_df <-
  predicted_df %>%
  rename(date = data.date,
         weight = data.weight_oz,
         change = data.change) %>%
  mutate(date = as.Date(date, "%m/%d/%Y"))


# make predictions

predicted_df$small <- round(predict(model_small, data))
predicted_df$full <- round(predict(model_full, data))
predicted_df$continuous <- round(predict(model_continuous, data))


# create cumulative change columns for predicteds

predicted_df$small_sum <- cumsum(predicted_df$small)
predicted_df$full_sum <- cumsum(predicted_df$full)
predicted_df$cont_sum <- cumsum(predicted_df$continuous)


# create columns of predicted weight

predicted_df$sm_weight <- 2754 + lag(predicted_df$small_sum, 1)
predicted_df$full_weight <- 2754 + lag(predicted_df$full_sum, 1)
predicted_df$cont_weight <- 2754 + lag(predicted_df$cont_sum, 1)


# plot predicted changes against actual

library(tidyr)
library(ggplot2)
predicted_df[-nrow(predicted_df), ] %>%
  select(date,
         change,
         small,
         full) %>%
  gather(model, change, -date) %>%
  ggplot(aes(x = date,
             y = change,
             fill = model)) + 
  geom_col(position = "dodge")


# create dataframe for plotting predicted weights (not changes)

plot_predict <-
  predicted_df %>%
  select(date,
         weight,
         sm_weight,
         full_weight,
         cont_weight) %>%
  rename(actual = weight,
         small_model = sm_weight,
         full_model = full_weight,
         continuous_model = cont_weight) %>%
  gather(model, weight, -date) 



# plot predictions against actual

ggplot(plot_predict, aes(x = date,
                         y = weight,
                         colour = model)) +
  geom_line()

