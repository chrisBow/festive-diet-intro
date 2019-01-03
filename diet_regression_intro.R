# import data

library(readr)

diet_df <- read_csv("diet_data.csv")


# quick look at data

head(diet_df)

tail(diet_df)


# tidy up the dataframe

library(dplyr)

diet_df <- 
  diet_df %>%
  select(-Stone, -Pounds, -Ounces) %>%
  mutate(Date = as.Date(Date, "%m/%d/%Y"),
         cals_per_oz = as.numeric(cals_per_oz)) %>%
  rename(date = Date) %>%
  na.omit()

glimpse(diet_df)
tail(diet_df)


#change 1/0 factors to TRUE/FALSE

diet_df <-
  diet_df %>%
  mutate(five_donuts = ifelse(five_donuts == 1, TRUE, FALSE),
         walk = ifelse(walk == 1, TRUE, FALSE),
         run = ifelse(run == 1, TRUE, FALSE),
         wine = ifelse(wine == 1, TRUE, FALSE),
         prot = ifelse(prot == 1, TRUE, FALSE),
         weight = ifelse(weight == 1, TRUE, FALSE)
         )

glimpse(diet_df)


# quick exploratory data analysis

library(ggplot2)

ggplot(diet_df, aes(x = date,
                    y = weight_oz)) +
  geom_line() +
  geom_smooth(method = "auto", se = FALSE) +
  labs(title = "My Weight Over Time",
       x = "Date",
       y = "Weight (oz)") +
  theme_chris()

ggplot(diet_df, aes(x = calories,
                    y = change)) +
  geom_point() +
  labs(title = "Change in Weight Against Daily Calorie Intake",
       x = "Daily calories",
       y = "Weight change (oz)") +
  xlim(1000, 10000) +
  theme_chris()

ggplot(diet_df, aes(x = calories,
                    y = change)) +
  geom_point() +
  labs(title = "Change in Weight Against Daily Calorie Intake",
       x = "Daily calories",
       y = "Weight change (oz)") +
  xlim(1000, 5000) +
  geom_smooth(method = "auto", se = FALSE) +
  theme_chris()

ggplot(diet_df, aes(x = calories,
                    y = change)) +
  geom_point() +
  labs(title = "Change in Weight Against Daily Calorie Intake",
       subtitle = "Only showing days with under 3500 calories",
       x = "Daily calories",
       y = "Weight change (oz)") +
  xlim(1250, 3500) +
  ylim(-75, 50) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_chris()


# linear model for days with calories under 3500

under_3k <- 
  diet_df %>%
  filter(calories <= 3500)

under_3k_lm <- lm(change ~ calories, data = under_3k)

summary(under_3k_lm)


#----- what happens with doughnuts? -----

# quick boxplot

ggplot(diet_df, aes(x = five_donuts,
                    y = change)) +
  geom_boxplot() +
  labs(title = "Boxplot of Daily Weight Changes",
       subtitle = "Comparing days where I ate five jam doughnuts or not",
       x = "Did I eat five jam doughnuts for lunch?",
       y = "Daily weight change (oz)") +
  theme_chris()

# summary statistics

tapply(diet_df$change, diet_df$five_donuts, summary)

# t-test

t.test(change ~ five_donuts, data = diet_df)

# linear model

under_3k_dn_lm <- lm(change ~ calories + five_donuts, data = under_3k)

summary(under_3k_dn_lm)


#---- what happens with protein? ----

# quick boxplot to look at weight change

ggplot(diet_df, aes(x = prot,
                    y = change)) +
  geom_boxplot() +
  labs(title = "Boxplot of Daily Weight Changes",
       subtitle = "Comparing days where I was on a high protein diet or not",
       x = "Was I on a high protein diet?",
       y = "Daily weight change (oz)") +
  theme_chris()


# quick boxplot to look at daily calories

ggplot(diet_df, aes(x = prot,
                    y = calories)) +
  geom_boxplot() +
  labs(title = "Boxplot of Daily Calorie Intake",
       subtitle = "Comparing days where I was on a high protein diet or not",
       x = "Was I on a high protein diet?",
       y = "Daily calorie consumption") +
  theme_chris()


# summary statistics

tapply(diet_df$change, diet_df$prot, summary)

tapply(diet_df$calories, diet_df$prot, summary)


# t-test

t.test(change ~ prot, data = diet_df)

t.test(calories ~ prot, data = diet_df)


# linear model

under_3k_pr_lm <- lm(change ~ calories + prot, data = under_3k)

summary(under_3k_pr_lm)


#---- under 3.5k cals with walking and running -----

# build dataframe

under_3k_wr <-
  under_3k %>%
  select(change, calories, walk, run) %>%
  mutate(walk_run = ifelse(walk == FALSE & run == FALSE, "none",
                           ifelse(walk == TRUE & run == FALSE, "walk",
                                  ifelse(walk == FALSE & run == TRUE, "run", "both"))))

# set factor levels

levels(under_3k_wr$walk_run) <- c("none", "walk", "run", "both")

# plot

ggplot(under_3k_wr, aes(x = calories,
                        y = change,
                        colour = walk_run)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


# linear model

under_3k_wr_lm <- lm(change ~ calories + walk + run, data = under_3k_wr)

summary(under_3k_wr_lm)


# plot model to check assumptions

plot(under_3k_wr_lm)


# augment with broom package

library(broom)

under_3k_wr_lm_aug <- augment(under_3k_wr_lm)


# plot parallel slopes model

ggplot(under_3k_wr_lm_aug, aes(x = calories,
                               y = change,
                               colour = walk,
                               shape = run)) +
  geom_point() +
  geom_line(aes(y = .fitted, linetype = run)) +
  labs(title = "Parallel Slopes Model to Investigate Effect of Exercise on \nDaily Weight Change",
       x = "Calorie intake",
       y = "Weight change (oz.)") +
  theme_chris()


# calculate daily calorie allowance for no weight change
# point where regression line crosses x at y = 0

tidy_coefficients <- tidy(under_3k_wr_lm)
  
exercise <- c("None", "Walk", "Run", "Walk & Run")
no_change_cals <- 
  c(as.numeric(abs(tidy_coefficients[1, 2] / tidy_coefficients[2, 2])),
    as.numeric(abs(tidy_coefficients[1, 2] + tidy_coefficients[3, 2]) / tidy_coefficients[2, 2]),
    as.numeric(abs(tidy_coefficients[1, 2] + tidy_coefficients[4, 2]) / tidy_coefficients[2, 2]),
    as.numeric(abs(tidy_coefficients[1, 2] + tidy_coefficients[3, 2] + tidy_coefficients[4, 2]) / tidy_coefficients[2, 2]))

library(knitr)

data.frame(exercise, no_change_cals) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  kable()


#----- Return to unfiltered (< 3k) dataset -----

# linear model

full_lm <- lm(change ~ calories, data = diet_df)

summary(full_lm)


# does using calories per ounce explain more variability?

full_lm_per_oz <- lm(change ~ cals_per_oz, data = diet_df)

summary(full_lm_per_oz)


# model with all features

full_lm_all <- lm(change ~ calories +
                    five_donuts +
                    walk +
                    run +
                    wine +
                    prot +
                    weight,
                  data = diet_df)

summary(full_lm_all)


# remove weight

full_lm_all_a <- lm(change ~ calories +
                    five_donuts +
                    walk +
                    run +
                    wine +
                    prot,
                  data = diet_df)

summary(full_lm_all_a)


# remove wine

full_lm_all_b <- lm(change ~ calories +
                      five_donuts +
                      walk +
                      run +
                      prot,
                    data = diet_df)

summary(full_lm_all_b)


# remove prot

full_lm_all_c <- lm(change ~ calories +
                      five_donuts +
                      walk +
                      run,
                    data = diet_df)

summary(full_lm_all_c)


# remove five_donuts

full_lm_all_d <- lm(change ~ calories +
                      walk +
                      run,
                    data = diet_df)

summary(full_lm_all_d)


# remove walk

full_lm_all_e <- lm(change ~ calories +
                      run,
                    data = diet_df)

summary(full_lm_all_e)


#----- create additional features -----

# create 'binge' variable

diet_df_extra <-
  diet_df %>%
  mutate(binge = ifelse(diet_df$calories > 5000, TRUE, FALSE))


# create 1, 2 and 3 day lags of binge

diet_df_extra_lagged <-
  diet_df_extra %>%
  mutate(binge_yesterday = lag(diet_df_extra$binge, 1),
         binge_two_days_ago = lag(diet_df_extra$binge, 2),
         binge_three_days_ago = lag(diet_df_extra$binge, 3)) %>%
  na.omit()

glimpse(diet_df_extra_lagged)


#----- build new linear model -----

# use all features

lm_extra_all <- lm(change ~ calories +
                     five_donuts +
                     walk + 
                     run +
                     wine +
                     prot +
                     weight +
                     binge +
                     binge_yesterday +
                     binge_two_days_ago +
                     binge_three_days_ago,
                   data = diet_df_extra_lagged)

summary(lm_extra_all)

#  omit weight

lm_extra_all_b <- lm(change ~ calories +
                     five_donuts +
                     walk + 
                     run +
                     wine +
                     prot +
                     binge +
                     binge_yesterday +
                     binge_two_days_ago +
                     binge_three_days_ago,
                   data = diet_df_extra_lagged)

summary(lm_extra_all_b)


# omit walk

lm_extra_all_c <- lm(change ~ calories +
                       five_donuts +
                       run +
                       wine +
                       prot +
                       binge +
                       binge_yesterday +
                       binge_two_days_ago +
                       binge_three_days_ago,
                     data = diet_df_extra_lagged)

summary(lm_extra_all_c)


# omit prot

lm_extra_all_d <- lm(change ~ calories +
                       five_donuts +
                       run +
                       wine +
                       binge +
                       binge_yesterday +
                       binge_two_days_ago +
                       binge_three_days_ago,
                     data = diet_df_extra_lagged)

summary(lm_extra_all_d)


# omit five_donuts

lm_extra_all_e <- lm(change ~ calories +
                       run +
                       wine +
                       binge +
                       binge_yesterday +
                       binge_two_days_ago +
                       binge_three_days_ago,
                     data = diet_df_extra_lagged)

summary(lm_extra_all_e)


# omit wine

lm_extra_all_e <- lm(change ~ calories +
                       run +
                       binge +
                       binge_yesterday +
                       binge_two_days_ago +
                       binge_three_days_ago,
                     data = diet_df_extra_lagged)

summary(lm_extra_all_e)


# try model with calories per ounce

lm_extra_all_per_oz <- lm(change ~ cals_per_oz +
                       run +
                       binge +
                       binge_yesterday +
                       binge_two_days_ago +
                       binge_three_days_ago,
                     data = diet_df_extra_lagged)

summary(lm_extra_all_per_oz)


# try quadratic version of above model

diet_df_extra_lagged$cals_per_oz_sq <- diet_df_extra_lagged$cals_per_oz^2

lm_extra_all_per_oz_sq <- lm(change ~ cals_per_oz +
                            cals_per_oz_sq +   
                            run +
                            binge +
                            binge_yesterday +
                            binge_two_days_ago +
                            binge_three_days_ago,
                          data = diet_df_extra_lagged)

summary(lm_extra_all_per_oz_sq)





