#Timbre modelling of systematic differences between our results and
#the results of Bowling et al. (2018)

library(MASS)

#Bowling et al.'s data for dyads, triads, and tetrads
df_bowl <- inconData::bowl18 %>%
  dplyr::filter(
    chord_size <= 4 &
    !grepl("\\b12\\b", pi_chord_type_int) #Remove chords containing octaves
  ) %>%
  dplyr::mutate(
    chord = purrr::map_chr(
      pi_chord_type_int, ~ as.character(list(hrep::pc_set_type(.))))
  ) %>%
  dplyr::group_by(chord) %>%
  dplyr::summarise(
    bowl_rating = mean(rating),
    chord_size = chord_size
  ) %>% unique()

#Our data
df_comparison <-
  df_trials %>%
  dplyr::select(
    chord,
    answer,
    cardinality
  ) %>%
  dplyr::group_by(chord) %>%
  dplyr::summarise(
    our_rating = mean(answer),
    cardinality = cardinality
  ) %>% unique()

#Combined data
df_joint <- left_join(df_bowl, df_comparison, by = "chord") %>%
  dplyr::select(- chord_size) %>% ungroup()

df_dyads <- df_joint[df_joint$cardinality == 2, ]
df_triads <- df_joint[df_joint$cardinality == 3, ]
df_tetrads <- df_joint[df_joint$cardinality == 4, ]

#Correlations by cardinality to account for blocking in Bowling et al.
cor.test(df_dyads$bowl_rating, df_dyads$our_rating, method = "pearson")
cor.test(df_triads$bowl_rating, df_triads$our_rating, method = "pearson")
cor.test(df_tetrads$bowl_rating, df_tetrads$our_rating, method = "pearson")

#Regression model to extract residuals
lm_resid <- lm(our_rating ~ bowl_rating, data = df_tetrads)
residuals <- resid(lm_resid)

df_resid <-
  df_tetrads %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    our_relative_pleasantness = residuals
  ) %>%
  dplyr::select(chord, our_relative_pleasantness) %>%
  left_join(df_features, by = "chord")

#Plot this regression to visualise residuals
original_chord_labels <- df_tetrads$chord
plot_chord_labels <- mgsub(pattern = c("c\\(", " ", "\\)"),
                           replacement = c("[", "", "]"),
                           original_chord_labels)
tetrad_labels <- setNames(plot_chord_labels, original_chord_labels)

set.seed(1)

residuals_plot <- df_tetrads %>% 
  ggplot(
    aes(
      x = scale(bowl_rating),
      y = scale(our_rating),
      label = tetrad_labels[chord]
    )
  ) + 
  geom_point(size = 5) +
  theme(text = element_text(size = 30),
        axis.text = element_text(size = 30)) +
  geom_smooth(method = "lm") +
  ggrepel::geom_label_repel(force = 20, max.overlaps = 30, size = 10) +
  labs(
    x = expression(
      paste("Bowling ", paste(italic("et al. ")), paste("data (normalised)"))),
    y = "Our data (normalised)")

ggsave("output/residuals_plot.pdf", residuals_plot, width = 24, height = 18)

#Add piano spectra to df_trials
df_piano <- df_trials %>%
  dplyr::filter(cardinality == 4) %>%
  dplyr::mutate(
    piano = purrr::map(realized_chord, piano_chord),
    piano_sparse_pi = purrr::map(piano, ~ hrep::sparse_pi_spectrum(
      list(
        pitch = .$midi_harmonics,
        amplitude = .$weights
      )
    )),
    piano_sparse_fr = purrr::map(piano_sparse_pi, ~ hrep::sparse_fr_spectrum(.)),
    piano_roughness = purrr::map(piano_sparse_fr, dycon::roughness_hutch),
    piano_harmonicity_milne = purrr::map(
      piano_sparse_pi, ~ get_harmonicity(., mode = "milne"))
  )

#Full dataset with differences between Shepard and piano timbres
df_feature_comparisons <-
  df_piano %>%
  dplyr::group_by(chord) %>%
  dplyr::summarise(
    chord = chord,
    shep_rough = mean(as.numeric(roughness)),
    shep_harm = mean(as.numeric(harmonicity_milne)),
    piano_rough = mean(as.numeric(piano_roughness)),
    piano_harm = mean(as.numeric(piano_harmonicity_milne))
  ) %>%
  unique() %>%
  dplyr::mutate(
    our_relative_roughness = shep_rough - piano_rough,
    our_relative_harmonicity = shep_harm - piano_harm
  ) %>%
  left_join(
    df_resid,
    by = "chord"
  )

#Regression models
#Roughness and harmonicity as predictors
#Scaled
fit_resid_roughness_harmonicity <- lm(
  scale(our_relative_pleasantness) ~
    scale(our_relative_roughness) + scale(our_relative_harmonicity),
  data = df_feature_comparisons,
  na.action = "na.fail"
)

performance::check_model(fit_resid_roughness_harmonicity)
performance::check_outliers(fit_resid_roughness_harmonicity)
summary(fit_resid_roughness_harmonicity)

bca_fit_resid_roughness_harmonicity <-
  bca_model(
    as.formula(scale(our_relative_pleasantness) ~
      scale(our_relative_roughness) + scale(our_relative_harmonicity)),
    data = df_feature_comparisons,
    mode = "ols",
    na.action = "na.fail",
    .nboot = 10000
  )
print(bca_fit_resid_roughness_harmonicity$par)

set.seed(1)

fit_resid_roughness_harmonicity_robust <- MASS::rlm(
  scale(our_relative_pleasantness) ~
    scale(our_relative_roughness) + scale(our_relative_harmonicity),
  data = df_feature_comparisons,
  seed = 1,
  method = "MM",
  init = "ls",
  acc = 1e-15,
  maxit = 10000,
  na.action = "na.fail"
)

summarise_robust(fit_resid_roughness_harmonicity_robust)

#Interval counts as predictors
interval_model <- lm(our_relative_pleasantness ~ 0 +
     interval_count_1 +
     interval_count_2 +
     interval_count_3 +
     interval_count_4 +
     interval_count_5 +
     interval_count_6,
   data = df_feature_comparisons,
   na.action = "na.fail"
)

performance::check_model(interval_model)
summary(interval_model)

set.seed(1)

bca_fit_interval_counts <- bca_model(
  as.formula(our_relative_pleasantness ~
    0 +
    interval_count_1 +
    interval_count_2 +
    interval_count_3 +
    interval_count_4 +
    interval_count_5 +
    interval_count_6),
  data = df_feature_comparisons,
  mode = "ols",
  na.action = "na.fail",
  .nboot = 10000
)

print(bca_fit_interval_counts$par)

interval_rlm <- MASS::rlm(our_relative_pleasantness ~
    0 +
    interval_count_1 +
    interval_count_2 +
    interval_count_3 +
    interval_count_4 +
    interval_count_5 +
    interval_count_6,
  data = df_feature_comparisons,
  seed = 1,
  method = "MM",
  init = "ls",
  acc = 1e-15,
  maxit = 10000,
  na.action = "na.fail"
)

summarise_robust(interval_rlm)


