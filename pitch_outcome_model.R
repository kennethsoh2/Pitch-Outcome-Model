library(tidyverse)
library(ggplot2)

setwd("~/R/SMGT 435")
pitch <- read_csv("pitch.csv")
player <- read_csv("player.csv")
event <- read_csv("event.csv")
cv <- read_csv("count_value.csv")

source("https://raw.githubusercontent.com/saberpowers/sabRmetrics/v1.1.0/R/get_quadratic_coef.R")
source("https://raw.githubusercontent.com/saberpowers/sabRmetrics/v1.1.0/R/get_trackman_metrics.R")

##Part 1##

#Getting only pitches where there was a swing
contact_labels <- c("Foul", "In play, out(s)", "In play, no out", "In play, run(s)")
swing_miss_labels <- c("Swinging Strike", "Swing Strike (Blocked)", "Foul Tip", "Missed Bunt")

pitch <- pitch |>
  mutate(
    swing_flag = as.integer(description %in% c(contact_labels, swing_miss_labels)),
    contact = as.integer(description %in% contact_labels)
  )

#filtering out extremely uncommon pitch types (outliers in the data that were skewing results later on)
pitch <- pitch |>
  filter(!(pitch_type %in% c("SC", "FO", "CS", "EP", "KN")))

swing <- pitch |> 
  filter(swing_flag == 1)

#Linear weights
lw <- read_csv("linear_weight_old.csv")

lw_walk <- lw |>
  filter(event == "Walk") |>
  pull(linear_weight) |>
  first()
lw_strikeout <- lw |>
  filter(event == "Strikeout") |>
  pull(linear_weight) |>
  first()

swing <- swing |>
  mutate(
    balls_next = pmin(balls + 1L, 4L),
    strikes_next = pmin(strikes + 1L, 3L)
  ) |>
  #current count value
  left_join(cv |> rename(count_value_current = count_value),
            by = c("balls","strikes")) |>
  #value of ball next pitch
  left_join(cv |> rename(balls_next = balls, count_value_ball = count_value),
            by = c("balls_next","strikes")) |>
  #value of strike next pitch
  left_join(cv |> rename(strikes_next = strikes, count_value_strike = count_value),
            by = c("balls","strikes_next")) |>
  #terminal states (3 balls -> walk, 2 strikes -> strikeout)
  mutate(
    count_value_ball = if_else(balls == 3L, lw_walk, count_value_ball),
    count_value_strike = if_else(strikes == 2L, lw_strikeout, count_value_strike),
    count_pressure = count_value_strike - count_value_ball #new variable for "pressure" of the upcoming pitch
  ) |>
  select(-balls_next, -strikes_next)

#Creating Features
swing_q <- get_quadratic_coef(swing, source = "statsapi")
swing_tm <- get_trackman_metrics(swing_q)

swing_tm <- swing_tm |>
  rename(
    plate_x_tm = plate_x,
    plate_z_tm = plate_z,
    ivb = induced_vert_break,
    hb = horz_break
  ) |>
  mutate(
    z_center = (strike_zone_top + strike_zone_bottom) / 2, #"middle-middle" of strike zone
    plate_x_use = coalesce(swing$plate_x, plate_x_tm),
    plate_z_use = coalesce(swing$plate_z, plate_z_tm),
    x_abs = abs(plate_x_use),
    z_distance = plate_z_use - z_center,
    
    #adding squared terms to sharply decrease contact probability as the pitch gets farther from 
    #middle-middle, instead of just decreasing at a linear rate
    x_abs_sq = x_abs ^ 2,
    z_distance_sq = z_distance ^ 2,
    
    release_pos_x = release_x,
    release_pos_z = release_z,
    
    release_speed_mph = release_speed / 1.4667 #convert ft/s to mph
  )


#Looking at the simple logistic regression we did in class
simple_df <- swing_tm |>
  transmute(
    contact,
    plate_x_use,
    plate_z_use,
    balls_f = factor(balls),
    strikes_f = factor(strikes),
    release_speed_mph
  ) |>
  drop_na()

set.seed(42)
idx_simple <- sample.int(nrow(simple_df), floor(0.75 * nrow(simple_df)))
train_simple <- simple_df[idx_simple, ]
test_simple <- simple_df[-idx_simple, ]

fit_simple <- glm(
  contact ~ balls_f + strikes_f + I(plate_x_use ^ 2) + I((plate_z_use - 2.5) ^ 2) + release_speed_mph,
  data = train_simple, family = binomial()
)
summary(fit_simple)

#New logistic regression with added predictors
model_2 <- swing_tm |>
  transmute(
    contact,
    plate_x_use, plate_z_use,
    x_abs, z_distance, x_abs_sq, z_distance_sq,
    release_speed_mph, release_pos_x, release_pos_z, extension,
    hb, ivb,
    balls, strikes, count_pressure,
    pitch_type = factor(pitch_type)
  ) |>
  drop_na()

set.seed(42)
idx <- sample.int(nrow(model_2), floor(0.75 * nrow(model_2)))
train_2 <- model_2[idx, ]
test_2  <- model_2[-idx, ]

fit_2 <- glm(
  contact ~ release_speed_mph +
    x_abs + z_distance + x_abs_sq + z_distance_sq +
    release_pos_x + release_pos_z + extension +
    hb + ivb +
    balls + strikes + count_pressure +
    pitch_type +
    release_speed_mph:pitch_type, #interaction: release speed effect differs for different pitch types
  data = train_2, family = binomial()
)
summary(fit_2)

#Bar Chart Visualizing Change in Contact Probability
comparison_data <- tibble(
  location = c("Middle-Middle (0.00, 2.50)",
               "Inside (0.83, 2.50)",
               "Outside (-0.83, 2.50)",
               "High-In (0.83, 3.20)",
               "Low-Out (-0.83, 1.80)"),
  plate_x_use = c(0.00, 0.83, -0.83, 0.83, -0.83),
  plate_z_use = c(2.50, 2.50, 2.50, 3.20, 1.80)
)

ff_speed <- median(swing_tm$release_speed_mph[swing_tm$pitch_type == "FF"], na.rm = TRUE)
ff_hb <- median(swing_tm$hb[swing_tm$pitch_type == "FF"], na.rm = TRUE)
ff_ivb <- median(swing_tm$ivb[swing_tm$pitch_type == "FF"], na.rm = TRUE)
ff_rx <- median(swing_tm$release_pos_x[swing_tm$pitch_type == "FF"], na.rm = TRUE)
ff_rz <- median(swing_tm$release_pos_z[swing_tm$pitch_type == "FF"], na.rm = TRUE)
ff_ext <- median(swing_tm$extension[swing_tm$pitch_type == "FF"], na.rm = TRUE)
z_center <- mean((swing_tm$strike_zone_top + swing_tm$strike_zone_bottom) / 2, na.rm = TRUE)

newdata <- comparison_data |>
  mutate(
    release_speed_mph = ff_speed,
    hb = ff_hb,
    ivb = ff_ivb,
    release_pos_x = ff_rx,
    release_pos_z = ff_rz,
    extension = ff_ext,
    x_abs = abs(plate_x_use),
    z_distance = plate_z_use - z_center,
    x_abs_sq = x_abs ^ 2,
    z_distance_sq = z_distance ^ 2,
    balls = 1L, strikes = 1L, count_pressure = 0,
    pitch_type = factor("FF", levels = levels(train_2$pitch_type))
  )

newdata$contact_prob <- predict(fit_2, newdata = newdata, type = "response")

ggplot(newdata, aes(x = reorder(location, -contact_prob), y = contact_prob, fill = contact_prob)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", contact_prob * 100)),
            vjust = -0.4, size = 4.2, fontface = "bold") +
  scale_fill_viridis_c(option = "magma", direction = -1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Predicted Contact Probability (using Model 2) Within the Strike Zone",
    subtitle = "Four-seam fastballs at Average Speed/Movement",
    x = "Plate Location (plate_x, plate_z (in feet))", y = "Contact Probability"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))

#Visualizing relationship between top predictors and contact outcome
library(broom)

#Standardize all predictors so coefficient magnitudes can be compared
fit_2_std <- glm(
  contact ~ scale(release_speed_mph) +
    scale(x_abs) + scale(z_distance) + scale(x_abs_sq) + scale(z_distance_sq) +
    scale(release_pos_x) + scale(release_pos_z) + scale(extension) +
    scale(hb) + scale(ivb) +
    scale(balls) + scale(strikes) + scale(count_pressure) +
    pitch_type + scale(release_speed_mph):pitch_type,
  data = train_2,
  family = binomial()
)

coef_imp <- tidy(fit_2_std) |>
  filter(!str_detect(term, "(Intercept)")) |>
  mutate(abs_est = abs(estimate)) |>
  arrange(desc(abs_est))
head(coef_imp, 10)

#Look at top 10 most important features according to effect magnitude
top_features <- coef_imp |> slice_head(n = 10)

ggplot(top_features, aes(x = reorder(term, abs_est), y = abs_est, fill = abs_est)) +
  geom_col() +
  coord_flip() +
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  labs(
    title = "Relative Importance of Pitch-Tracking Features",
    subtitle = "Absolute standardized logistic coefficients from Model 2",
    x = "Feature",
    y = "Effect Magnitude"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

#BONUS - xgBoost
library(xgboost)
library(Matrix)
library(parallel)

#Create a design matrix (one-hot; numeric only)
x_formula <- ~ release_speed_mph +
  x_abs + z_distance + x_abs_sq + z_distance_sq +
  release_pos_x + release_pos_z + extension +
  hb + ivb +
  balls + strikes + count_pressure +
  pitch_type + release_speed_mph:pitch_type

X_train <- sparse.model.matrix(x_formula, data = train_2)
y_train <- train_2$contact

X_valid <- sparse.model.matrix(x_formula, data = test_2)
y_valid <- test_2$contact

dtrain <- xgb.DMatrix(X_train, label = y_train)
dvalid <- xgb.DMatrix(X_valid, label = y_valid)

#5-fold cross-validation to minimize log loss
cv_params <- list(
  objective = "binary:logistic",
  eval_metric = "logloss",
  eta = 0.05,
  max_depth = 5,
  min_child_weight = 5,
  subsample = 0.9,
  colsample_bytree = 0.8
)

cv_results <- xgb.cv(
  params = modifyList(cv_params, list(nthread = parallel::detectCores())),
  data = dtrain,
  nrounds = 2000,
  nfold = 5,
  metrics = "logloss",
  stratified = TRUE,
  early_stopping_rounds = 50,
  verbose = 0,
  callbacks = list(xgboost::cb.print.evaluation(period = 25))
)

print(cv_params)

#saveRDS(cv_params, file = "cv_contact_prob_params.rds")
cv_params <- readRDS("cv_contact_prob_params.rds")

#Best iteration and mean log loss across folds
best_iter <- cv_results$best_iteration
cv_results$evaluation_log %>% tail(1)
cat(sprintf("[XGBoost CV] Best nrounds: %d | Mean Log Loss: %.4f\n",
            best_iter,
            cv_results$evaluation_log$test_logloss_mean[best_iter]))

xgb_fit <- xgb.train(
  params = cv_params,
  data = dtrain,
  nrounds = best_iter,
  watchlist = list(train = dtrain, valid = dvalid),
  verbose = 1
)

#saveRDS(xgb_fit, file = "xgb_contact_model.rds")
xgb_fit <- readRDS("xgb_contact_model.rds")

cv_df <- cv_results$evaluation_log
ggplot(cv_df, aes(iter)) +
  geom_line(aes(y = test_logloss_mean), linewidth = 0.8) +
  geom_line(aes(y = train_logloss_mean), linetype = "dashed", linewidth = 0.6) +
  labs(title = "XGBoost 5-fold CV: Log Loss by Boosting Round",
       x = "Boosting round", y = "Log loss",
       caption = sprintf("Best nrounds = %d", best_iter)) +
  theme_minimal(base_size = 12)

##Part 2##
#Negative Log Loss Function
log_loss_vec <- function(y_true, p_hat, eps = 1e-12) {
  p <- pmin(pmax(p_hat, eps), 1 - eps)
  -mean(y_true * log(p) + (1 - y_true) * log(1 - p))
}

prob_simple <- predict(fit_simple, newdata = test_simple, type = "response")
ll_simple <- log_loss_vec(test_simple$contact, prob_simple)
prob_2 <- predict(fit_2, newdata = test_2, type = "response")
ll_2 <- log_loss_vec(test_2$contact, prob_2)

cat(sprintf("\n[Baseline] Test Log Loss: %.4f\n", ll_simple))
cat(sprintf("\n[Model 2] Test Log Loss: %.4f\n", ll_2))

#Generating Contact Probability Predictions
pitch_test <- read_csv("pitch_test.csv")

#Preprocess the data the same way as earlier
pitch_test <- pitch_test |>
  mutate(
    balls_next = pmin(balls + 1L, 4L),
    strikes_next = pmin(strikes + 1L, 3L)
  ) |>
  left_join(cv |> rename(count_value_current = count_value), 
            by = c("balls","strikes")) |>
  left_join(cv |> rename(balls_next = balls, count_value_ball = count_value),
            by = c("balls_next","strikes")) |>
  left_join(cv |> rename(strikes_next = strikes, count_value_strike = count_value),
            by = c("balls","strikes_next")) |>
  mutate(
    count_value_ball = if_else(balls == 3L, lw_walk, count_value_ball),
    count_value_strike = if_else(strikes == 2L, lw_strikeout, count_value_strike),
    count_pressure = count_value_strike - count_value_ball
  ) |>
  select(-balls_next, -strikes_next)

#Same filtering of extremely uncommon pitches that were skewing data
pitch_test <- pitch_test |>
  filter(!(pitch_type %in% c("SC","FO","CS","EP","KN")))

pt_q <- get_quadratic_coef(pitch_test, source = "statsapi")
pt_tm <- get_trackman_metrics(pt_q) |>
  rename(
    plate_x_tm = plate_x,
    plate_z_tm = plate_z,
    hb = horz_break,
    ivb = induced_vert_break
  ) |>
  mutate(
    z_center = (strike_zone_top + strike_zone_bottom) / 2,
    plate_x_use = coalesce(pitch_test$plate_x, plate_x_tm),
    plate_z_use = coalesce(pitch_test$plate_z, plate_z_tm),
    x_abs = abs(plate_x_use),
    z_distance = plate_z_use - z_center,
    x_abs_sq = x_abs ^ 2,
    z_distance_sq = z_distance ^ 2,
    release_pos_x = release_x,
    release_pos_z = release_z,
    release_speed_mph = release_speed / 1.46667
  )

pitch_test_processed <- pt_tm |>
  transmute(
    pitch_id = index,
    release_speed_mph, plate_x_use, plate_z_use,
    x_abs, z_distance, x_abs_sq, z_distance_sq,
    release_pos_x, release_pos_z, extension,
    hb, ivb,
    balls, strikes, count_pressure,
    pitch_type = factor(pitch_type, levels = levels(train_2$pitch_type))
  ) |>
  drop_na()

#predict probabilities using model 2
pred_lm2 <- pitch_test_processed |>
  mutate(prob_contact = predict(fit_2, newdata = pitch_test_processed, type = "response")) |>
  select(pitch_id, prob_contact) |>
  mutate(prob_contact = round(prob_contact, 6))

write_csv(pred_lm2, "pitch_test_predictions_logistic.csv")

#BONUS - xgBoost

#Evaluate out-of-sample log loss on the validation split
pred_xgb <- predict(xgb_fit, dvalid)

ll_xgb <- log_loss_vec(y_valid, pred_xgb)
cat(sprintf("[XGBoost] Test Log Loss: %.4f\n", ll_xgb))

#Feature Importance
imp <- xgb.importance(model = xgb_fit)
xgb.plot.importance(imp, top_n = 15)

#Generating Predictions on pitch_test.csv
#Build the exact same design matrix as training/validation
X_test <- sparse.model.matrix(x_formula, data = pitch_test_processed)
dtest <- xgb.DMatrix(X_test)
pred_xgb <- tibble(
  pitch_id = pitch_test_processed$pitch_id,
  prob_contact = round(predict(xgb_fit, dtest), 6)
)

write_csv(pred_xgb, "pitch_test_predictions_xgb.csv")

##Part 3##
#Joining each swing with its actual outcome
event_pitcher <- event |>
  select(game_id, event_index, pitcher_id) |>
  distinct()

swing_2 <- swing_tm |>
  left_join(event_pitcher, by = c("game_id","event_index")) |>
  mutate(
    pitch_type = factor(pitch_type, levels = levels(train_2$pitch_type))
  )

swing_2 <- swing_2 |>
  mutate(prob_hat = predict(fit_2, newdata = swing_2, type = "response"))

#Calculating observed and expected contact rate, as well as residual contact rate, for each pitcher
by_pitcher <- swing_2 |>
  group_by(pitcher_id) |>
  summarise(
    n_pitches = n(),
    a_obs = mean(contact, na.rm = TRUE), #observed contact rate
    b_exp = mean(prob_hat, na.rm = TRUE), #expected contact rate
    .groups = "drop"
  ) |>
  mutate(c_resid = a_obs - b_exp) #residual contact rate

by_pitcher <- by_pitcher |>
  filter(!is.na(b_exp), !is.nan(b_exp),
         !is.na(c_resid), !is.nan(c_resid))

#Regression to the mean
library(lme4)
library(broom.mixed)

#Pitch-level residuals for each pitcher
swing_re <- swing_2 |>
  mutate(
    pitcher_id = factor(pitcher_id),
    resid = contact - prob_hat
  ) |>
  filter(!is.na(resid))

#Mixed model: residual = overall mean + pitcher effect + noise
fit_mixed <- lmer(resid ~ 1 + (1 | pitcher_id), data = swing_re, REML = TRUE)

#Extract mean regressed pitcher effects
re_pitcher <- tidy(fit_mixed, effects = "ran_vals") |>
  filter(group == "pitcher_id", term == "(Intercept)") |>
  transmute(
    pitcher_id = factor(level),
    c_reg = estimate,
    c_reg_se = std.error
  )

by_pitcher <- swing_re |>
  group_by(pitcher_id) |>
  summarise(
    n_pitches = n(),
    a_obs = mean(contact, na.rm = TRUE),
    b_exp = mean(prob_hat, na.rm = TRUE),
    c_resid = mean(resid, na.rm = TRUE),
    .groups = "drop"
  ) |>
  #Attach mean regressed residual effects from lmer
  left_join(re_pitcher, by = "pitcher_id") |>
  mutate(
    d_pred = b_exp + c_reg    #mean-regressed predicted contact rate
  )

pitchers_only <- player |>
  transmute(
    pitcher_id = factor(player_id),
    pitcher_name = name_full,
    primary_position
  )

by_pitcher <- by_pitcher |>
  left_join(pitchers_only, by = "pitcher_id") %>%
  filter(primary_position == 1)

#Leaderboard of best pitchers according to Mean-Regressed Predicted Contact Rate
library(gt)
library(scales)

leaderboard_gt <- by_pitcher |>
  arrange(d_pred) |>
  slice_head(n = 20) |>
  mutate(
    rank = row_number(),
    a_obs = percent(a_obs, accuracy = 0.1),
    b_exp = percent(b_exp, accuracy = 0.1),
    c_resid = percent(c_resid, accuracy = 0.1),
    c_reg = percent(c_reg, accuracy = 0.1),
    d_pred = percent(d_pred, accuracy = 0.1)
  ) |>
  select(
    rank, pitcher_name, pitcher_id, n_pitches,
    a_obs, b_exp, c_resid, c_reg, d_pred
  ) |>
  gt() |>
  tab_header(
    title = md("**Pitcher Leaderboard — Lowest Predicted Contact Probability (d_pred)**"),
    subtitle = md("Regressed-to-mean estimates using Model 2")
  ) |>
  cols_label(
    rank = "Rank",
    pitcher_name = "Pitcher",
    pitcher_id = "ID",
    n_pitches = "Swung Pitches",
    a_obs = "Observed (a)",
    b_exp = "Expected (b)",
    c_resid = "Residual (c)",
    c_reg = "Reg. (c)",
    d_pred = "True Contact (d)"
  ) |>
  fmt_number(columns = n_pitches, decimals = 0, use_seps = TRUE) |>
  tab_options(table.font.size = px(13)) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  )

leaderboard_gt

#Visualization of all pitchers, observed contact rate on x-axis, predicted contact rate on y-axis
ggplot(by_pitcher, aes(x = a_obs, y = d_pred, size = n_pitches)) +
  geom_point(alpha = 0.55) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  scale_size_continuous(name = "Swung Pitches") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Observed vs. Estimated True Contact Probability by Pitcher",
    subtitle = "Observed rate (a) vs. regressed estimate (d = b_exp + c_reg). Dashed line = parity.",
    x = "Observed Contact Rate (a)",
    y = "Estimated True Contact Rate (d)"
  ) +
  theme_minimal(base_size = 12)
