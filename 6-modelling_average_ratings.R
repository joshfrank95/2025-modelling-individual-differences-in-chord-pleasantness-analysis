#Linear models for averaged (chord-level) ratings

library(AICcmodavg)

#Create a dataframe with chord-level mean ratings and average features
df_for_lm <- merge(
  df_trials %>%
  dplyr::group_by(chord) %>%
  dplyr::summarise(answer = mean(answer)),
  df_features,
  by = "chord",
  all.x = TRUE
)

#Check correlations between features
cor_table_features <-
  round(
    cor(
      df_features %>%
        dplyr::select(- chord)
    ), 2
  )

#Model formula definitions ----

#Full model - all predictors. Scaled and unscaled
full <- as.formula(
  scale(answer) ~
    scale(roughness) +
    scale(harmonicity_milne) +
    scale(cardinality) +
    scale(count) +
    scale(representation_in_major_scale) +
    scale(from_major) +
    scale(interval_count_1) +
    scale(interval_count_2) +
    scale(interval_count_3) +
    scale(interval_count_4) +
    scale(interval_count_5) +
    scale(interval_count_6) +
    scale(scale_degree_1) +
    scale(scale_degree_2) +
    scale(scale_degree_3) +
    scale(scale_degree_4) +
    scale(scale_degree_5) +
    scale(scale_degree_6) +
    scale(scale_degree_7) +
    scale(scale_degree_8) +
    scale(scale_degree_9) +
    scale(scale_degree_10) # scale_degree_11 removed due to aliasing
)

full_unscaled <- as.formula(
  answer ~
    roughness +
    harmonicity_milne +
    cardinality +
    count +
    representation_in_major_scale +
    from_major +
    interval_count_1 +
    interval_count_2 +
    interval_count_3 +
    interval_count_4 +
    interval_count_5 +
    interval_count_6 +
    scale_degree_1 +
    scale_degree_2 +
    scale_degree_3 +
    scale_degree_4 +
    scale_degree_5 +
    scale_degree_6 +
    scale_degree_7 +
    scale_degree_8 +
    scale_degree_9 +
    scale_degree_10
)

#Psychoacoustic model - all features minus interval counts and scale degrees.
#Scaled and unscaled.
psych <- as.formula(
  scale(answer) ~
    scale(roughness) +
    scale(harmonicity_milne) +
    scale(cardinality) +
    scale(count) +
    scale(representation_in_major_scale) +
    scale(from_major)
)

psych_unscaled <- as.formula(
  answer ~
    roughness +
    harmonicity_milne +
    cardinality +
    count +
    representation_in_major_scale +
    from_major
)

#"Classic" model, key predictors from Harrison & Pearce (2020).
#Scaled and unscaled.
classic <- as.formula(
  scale(answer) ~
    scale(roughness) +
    scale(harmonicity_milne) +
    scale(cardinality) +
    scale(count)
)

classic_unscaled <- as.formula(
  answer ~
    roughness +
    harmonicity_milne +
    cardinality +
    count
)

#Notation-based model - interval counts and scale degrees.
#Scaled and unscaled.
notation <- as.formula(
  scale(answer) ~
    scale(interval_count_1) +
    scale(interval_count_2) +
    scale(interval_count_3) +
    scale(interval_count_4) +
    scale(interval_count_5) +
    scale(interval_count_6) +
    scale(scale_degree_1) +
    scale(scale_degree_2) +
    scale(scale_degree_3) +
    scale(scale_degree_4) +
    scale(scale_degree_5) +
    scale(scale_degree_6) +
    scale(scale_degree_7) +
    scale(scale_degree_8) +
    scale(scale_degree_9) +
    scale(scale_degree_10) +
    scale(scale_degree_11)
)

notation_unscaled <- as.formula(
  answer ~
    interval_count_1 +
    interval_count_2 +
    interval_count_3 +
    interval_count_4 +
    interval_count_5 +
    interval_count_6 +
    scale_degree_1 +
    scale_degree_2 +
    scale_degree_3 +
    scale_degree_4 +
    scale_degree_5 +
    scale_degree_6 +
    scale_degree_7 +
    scale_degree_8 +
    scale_degree_9 +
    scale_degree_10 +
    scale_degree_11
)

#Interval counts only. Scaled and unscaled.
interval_counts <- as.formula(
  scale(answer) ~
    scale(interval_count_1) +
    scale(interval_count_2) +
    scale(interval_count_3) +
    scale(interval_count_4) +
    scale(interval_count_5) +
    scale(interval_count_6)
)

interval_counts_unscaled <- as.formula(
  answer ~
    interval_count_1 +
    interval_count_2 +
    interval_count_3 +
    interval_count_4 +
    interval_count_5 +
    interval_count_6
)

#Scale degrees only. Scaled and unscaled.
scale_degrees <- as.formula(
  scale(answer) ~
    scale(scale_degree_1) +
    scale(scale_degree_2) +
    scale(scale_degree_3) +
    scale(scale_degree_4) +
    scale(scale_degree_5) +
    scale(scale_degree_6) +
    scale(scale_degree_7) +
    scale(scale_degree_8) +
    scale(scale_degree_9) +
    scale(scale_degree_10) +
    scale(scale_degree_11)
)

scale_degrees_unscaled <- as.formula(
  answer ~
    scale_degree_1 +
    scale_degree_2 +
    scale_degree_3 +
    scale_degree_4 +
    scale_degree_5 +
    scale_degree_6 +
    scale_degree_7 +
    scale_degree_8 +
    scale_degree_9 +
    scale_degree_10 +
    scale_degree_11
)


#Run, assumption-check, and summarise OLS models.
#Note: major triad is an outlier (high leverage) in predictor space.
fit_full <- lm(full, data = df_for_lm, na.action = "na.fail")
fit_full_unscaled <- lm(full_unscaled, data = df_for_lm, na.action = "na.fail")
performance::check_model(fit_full_unscaled)
summary(fit_full)
summary(fit_full_unscaled)

fit_psych <- lm(psych, data = df_for_lm, na.action = "na.fail")
fit_psych_unscaled <- lm(psych_unscaled, data = df_for_lm, na.action = "na.fail")
performance::check_model(fit_psych_unscaled)
summary(fit_psych)
summary(fit_psych_unscaled)

fit_classic <- lm(classic, data = df_for_lm, na.action = "na.fail")
fit_classic_unscaled <- lm(classic_unscaled, data = df_for_lm, na.action = "na.fail")
performance::check_model(fit_classic_unscaled)
summary(fit_classic)
summary(fit_classic_unscaled)

fit_notation <- lm(notation, data = df_for_lm, na.action = "na.fail")
fit_notation_unscaled <- lm(
  notation_unscaled, data = df_for_lm, na.action = "na.fail")
performance::check_model(fit_notation_unscaled)
summary(fit_notation)
summary(fit_notation_unscaled)

fit_interval_counts <- lm(interval_counts, data = df_for_lm, na.action = "na.fail")
fit_interval_counts_unscaled <- lm(
  interval_counts_unscaled, data = df_for_lm, na.action = "na.fail")
performance::check_model(fit_interval_counts_unscaled)
summary(fit_interval_counts)
summary(fit_interval_counts_unscaled)

fit_scale_degrees <- lm(scale_degrees, data = df_for_lm, na.action = "na.fail")
fit_scale_degrees_unscaled <- lm(
  scale_degrees_unscaled, data = df_for_lm, na.action = "na.fail")
performance::check_model(fit_scale_degrees)
summary(fit_scale_degrees)
summary(fit_scale_degrees_unscaled)

#Run and summarise robust regressions with MM estimation
set.seed(1)
fit_full_robust <- MASS::rlm(
  full,
  data = df_for_lm,
  seed = 1,
  method = "MM",
  init = "ls",
  acc = 1e-15,
  maxit = 100000,
  na.action = "na.fail")
summarise_robust(fit_full_robust)

set.seed(1)
fit_full_robust_unscaled <- MASS::rlm(
  full_unscaled,
  data = df_for_lm,
  seed = 1,
  method = "MM",
  init = "ls",
  acc = 1e-14, #Needed for convergence
  maxit = 100000,
  na.action = "na.fail")
summarise_robust(fit_full_robust_unscaled) 

set.seed(1)
fit_psych_robust <- MASS::rlm(
  psych, 
  data = df_for_lm,
  seed = 1,
  method = "MM",
  init = "ls",
  acc = 1e-15,
  maxit = 10000,
  na.action = "na.fail")
summarise_robust(fit_psych_robust) 

set.seed(1)
fit_psych_robust_unscaled <- MASS::rlm(
  psych_unscaled,
  data = df_for_lm,
  seed = 1,
  method = "MM",
  init = "ls",
  acc = 1e-15,
  maxit = 10000,
  na.action = "na.fail")
summarise_robust(fit_psych_robust_unscaled)

set.seed(1)
fit_classic_robust <- MASS::rlm(
  classic,
  data = df_for_lm,
  seed = 1, 
  method = "MM",
  init = "ls",
  acc = 1e-15,
  maxit = 10000,
  na.action = "na.fail")
summarise_robust(fit_classic_robust)

set.seed(1)
fit_classic_robust_unscaled <- MASS::rlm(
  classic_unscaled,
  data = df_for_lm,
  seed = 1,
  method = "MM",
  init = "ls",
  acc = 1e-15,
  maxit = 10000,
  na.action = "na.fail")
summarise_robust(fit_classic_robust_unscaled)

set.seed(1)
fit_notation_robust <- MASS::rlm(
  notation, 
  data = df_for_lm,
  seed = 1,
  method = "MM",
  init = "ls",
  acc = 1e-14, #Needed for convergence
  maxit = 100000,
  na.action = "na.fail")
summarise_robust(fit_notation_robust)

set.seed(1)
fit_notation_robust_unscaled <- MASS::rlm(
  notation_unscaled,
  data = df_for_lm,
  seed = 1,
  method = "MM",
  init = "ls",
  acc = 1e-14, #Needed for convergence
  maxit = 100000,
  na.action = "na.fail")
summarise_robust(fit_notation_robust_unscaled)

set.seed(1)
fit_interval_counts_robust <- MASS::rlm(
  interval_counts, 
  data = df_for_lm,
  seed = 1,
  method = "MM",
  init = "ls",
  acc = 1e-15,
  maxit = 10000,
  na.action = "na.fail")
summarise_robust(fit_interval_counts_robust)

set.seed(1)
fit_interval_counts_robust_unscaled <- MASS::rlm(
  interval_counts_unscaled,
  data = df_for_lm,
  seed = 1,
  method = "MM",
  init = "ls",
  acc = 1e-15,
  maxit = 10000,
  na.action = "na.fail")
summarise_robust(fit_interval_counts_robust_unscaled)

set.seed(1)
fit_scale_degrees_robust <- MASS::rlm(
  scale_degrees, 
  data = df_for_lm,
  seed = 1,
  method = "MM",
  init = "ls",
  acc = 1e-15,
  maxit = 10000,
  na.action = "na.fail")
summarise_robust(fit_scale_degrees_robust)

set.seed(1)
fit_scale_degrees_robust_unscaled <- MASS::rlm(
  scale_degrees_unscaled,
  data = df_for_lm,
  seed = 1,
  method = "MM",
  init = "ls",
  acc = 1e-15,
  maxit = 10000,
  na.action = "na.fail")
summarise_robust(fit_scale_degrees_robust_unscaled)

#Bootstrapping OLS models

bca_fit_full <- bca_model(
  full,
  data = df_for_lm,
  mode = "ols",
  na.action = "na.fail",
  .nboot = 10000)
print(bca_fit_full$par, n = 25)

bca_fit_full_unscaled <- bca_model(
  full_unscaled,
  data = df_for_lm,
  mode = "ols",
  na.action = "na.fail",
  .nboot = 10000)
print(bca_fit_full_unscaled$par, n = 25)

bca_fit_psych <- bca_model(
  psych,
  data = df_for_lm,
  mode = "ols",
  na.action = "na.fail",
  .nboot = 10000
)
print(bca_fit_psych$par)

bca_fit_psych_unscaled <- bca_model(
  psych_unscaled,
  data = df_for_lm,
  mode = "ols",
  na.action = "na.fail",
  .nboot = 10000)
print(bca_fit_psych_unscaled$par)

bca_fit_classic <- bca_model(
  classic,
  data = df_for_lm,
  mode = "ols",
  na.action = "na.fail",
  .nboot = 10000)
print(bca_fit_classic$par)

bca_fit_classic_unscaled <- bca_model(
  classic_unscaled,
  data = df_for_lm,
  mode = "ols",
  na.action = "na.fail",
  .nboot = 10000)
print(bca_fit_classic_unscaled$par)

bca_fit_notation <- bca_model(
  notation,
  data = df_for_lm,
  mode = "ols",
  na.action = "na.fail",
  .nboot = 10000)
print(bca_fit_notation$par)

bca_fit_notation_unscaled <- bca_model(
  notation_unscaled,
  data = df_for_lm,
  mode = "ols",
  na.action = "na.fail",
  .nboot = 10000)
print(bca_fit_notation_unscaled$par)

bca_fit_interval_counts <- bca_model(
  interval_counts,
  data = df_for_lm,
  mode = "ols",
  na.action = "na.fail",
  .nboot = 10000)
print(bca_fit_interval_counts$par)

bca_fit_interval_counts_unscaled <- bca_model(
  interval_counts_unscaled,
  data = df_for_lm,
  mode = "ols",
  na.action = "na.fail",
  .nboot = 10000)
print(bca_fit_interval_counts_unscaled$par)

bca_fit_scale_degrees <- bca_model(
  scale_degrees,
  data = df_for_lm,
  mode = "ols",
  na.action = "na.fail",
  .nboot = 10000)
print(bca_fit_scale_degrees$par)

bca_fit_scale_degrees_unscaled <- bca_model(
  scale_degrees_unscaled,
  data = df_for_lm,
  mode = "ols",
  na.action = "na.fail",
  .nboot = 10000)
print(bca_fit_scale_degrees_unscaled$par)

#Compare model AIC values
models <- list(
  fit_full,
  fit_psych,
  fit_classic,
  fit_notation,
  fit_interval_counts,
  fit_scale_degrees)

model_names = c(
  "Full",
  "Psychoacoustic",
  "Classic",
  "Notation",
  "Interval counts",
  "Scale degrees")

AICcmodavg::aictab(cand.set = models, modnames = model_names)
