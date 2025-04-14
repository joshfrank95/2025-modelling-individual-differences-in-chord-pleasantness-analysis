#Conduct PCA and check relationships between PCs and participant background features

library(factoextra)
library(ggcorrplot)
library(ggrepel)
library(car)
library(egg)
library(Hmisc)
library(ggpubr)
library(patchwork)

#Create dataframe
df_pca <- df_trials %>%
  dplyr::group_by(participant_id, chord) %>%
  dplyr::summarise(
    answer = mean(answer)
  ) %>%
  dplyr::distinct() %>%
  pivot_wider(
    names_from = chord,
    values_from = answer
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(-participant_id)

#Conduct PCA
res_pca <- prcomp(df_pca)

#Significance testing

#Traditional scree plot
pca_scree_plot <- factoextra::fviz_eig(
  res_pca,
  choice = "variance",
  geom = "bar",
  ncp=10,
  ggtheme = theme_pubr() +
  theme(
  plot.title = element_blank(),
  axis.text = element_text(size = 14),
  axis.title = element_text(size = 14),
  ),
  addlabels = TRUE,
  hjust = 0.45
) +
  labs(
    x = "Component",
    y = "Explained Variance (%)"
  ) +
  scale_y_continuous(breaks = seq(0, 15, by = 2.5), limits = c(0, 15))

ggsave("output/pca_scree_plot.pdf", plot = pca_scree_plot, height = 8, width = 12)

#Plotting original explained variances against permutation results

pca_sig_perm_for_loop <- simple_pca_perm_for_loop(
  df_pca, scale = F, center = T, nperm = 10000, alpha = 0.05)

original_explained_variances <- res_pca$sdev^2 / sum(res_pca$sdev^2)

perm_results <- pca_sig_perm_for_loop[[2]]

# Calculate 95% confidence intervals for each component
ci_95 <- apply(perm_results, 1, function(row) {
  quantile(row, probs = c(0.025, 0.975))})

ci_95_df <- data.frame(
  component = seq_len(nrow(perm_results)),
  lower_perm = ci_95[1, ],
  upper_perm = ci_95[2, ],
  original_explained_variance = pca_sig_perm_for_loop[[3]])

ci_plot <- ggplot(ci_95_df[1:10, ], aes(x = component)) +
  geom_errorbar(aes(ymin = lower_perm * 100, ymax = upper_perm * 100),
                width = 0.5, color = "red") +
  geom_point(aes(y = original_explained_variance * 100), color = "blue") +
  geom_text(aes(y = original_explained_variance * 100, 
                label = round(original_explained_variance * 100, 1),
                vjust = ifelse(component >= 7, 1.5, -1)),
            color = "blue", size = 6) +
  labs(x = "Component", y = "Explained Variance (%)") +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(breaks = seq(0, 15, by = 2.5), limits = c(0, 15)) +
  theme_minimal() +
  theme(
    text = element_text(size = 16),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    plot.title = element_blank()
  )

ggsave("output/ci_plot.pdf", plot = ci_plot, height = 8, width = 12)

#Assess reliability of PCA using test-retest ratings
df_pca_test <- df_trials[!(df_trials$is_repeat_trial), ] %>%
  dplyr::group_by(participant_id) %>%
  # dplyr::mutate(
  #   answer = scale(answer)
  # ) %>%
  dplyr::group_by(participant_id, chord) %>%
  dplyr::summarise(
    answer = answer,
  ) %>%
  dplyr::distinct() %>%
  pivot_wider(
    names_from = chord,
    values_from = answer
  ) %>%
  ungroup() %>%
  dplyr::select(-participant_id)

df_pca_retest <- df_trials[df_trials$is_repeat_trial, ] %>%
  dplyr::group_by(participant_id) %>%
  # dplyr::mutate(
  #   answer = scale(answer)
  # ) %>%
  dplyr::group_by(participant_id, chord) %>%
  dplyr::summarise(
    answer = answer,
  ) %>%
  dplyr::distinct() %>%
  pivot_wider(
    names_from = chord,
    values_from = answer
  ) %>%
  ungroup() %>%
  dplyr::select(-participant_id)

coords_repeat <- predict(res_pca, newdata = df_pca_test)
coords_first <- predict(res_pca, newdata = df_pca_retest)

cor_results <- lapply(1:68, function(i) {
  cor_test_res <- cor.test(coords_first[, i], coords_repeat[, i])
  list(
    component = i,
    correlation = cor_test_res$estimate,
    p_value = cor_test_res$p.value
  )
})

cor_results_df <- do.call(rbind, lapply(cor_results, as.data.frame))
print(cor_results_df)

#Visualise chords and individuals in PC-space

row.names(res_pca$rotation) <- gsub(
  "c\\((.*?)\\)", "[\\1]", row.names(res_pca$rotation))


set.seed(1)
pca_variables_biplot <- factoextra::fviz_pca_var(
  res_pca,
  axes = c(1, 2),
  col.var = "contrib",
  legend.title = "Contribution",
  gradient.cols = viridis(n = 100, begin = 0.05,end = 0.9, direction = -1),
  repel = TRUE,
  ggtheme = theme_pubr() +
  theme(
  plot.title = element_text(size = 18),
  legend.position = "right"  
  )
) +
  theme(aspect.ratio = 1) +
  labs(x = paste0("PC1 (", round(factoextra::get_eigenvalue(res_pca)[1,2], 1), "%)"),
       y = paste0("PC2 (", round(factoextra::get_eigenvalue(res_pca)[2,2], 1), "%)"))

pca_variables_biplot <- pca_variables_biplot + ggtitle("A: Chord loadings")

set.seed(1)
pca_individuals_biplot <- factoextra::fviz_pca_ind(
  res_pca,
  axes = c(1, 2),
  col.ind = "cos2",
  legend.title = "Representation",
  gradient.cols = viridis(n = 100, begin = 0.05,end = 0.9, direction = -1),
  repel = TRUE,
  ggtheme = theme_pubr() +
  theme(
    plot.title = element_text(size = 18),
    legend.position = "right" 
  )
) + theme(aspect.ratio = 1) +
  labs(x = paste0("PC1 (", round(factoextra::get_eigenvalue(res_pca)[1,2], 1), "%)"),
       y = paste0("PC2 (", round(factoextra::get_eigenvalue(res_pca)[2,2], 1), "%)"))

pca_individuals_biplot <- pca_individuals_biplot + ggtitle("B: Participant loadings")

combined_biplots <- egg::ggarrange(
  pca_variables_biplot, pca_individuals_biplot, nrow = 2
)

ggsave("output/PCA_combined_biplots.pdf", combined_biplots, width = 8.27, height = 11.69)

#Check relationships between chords' coordinates and features

#Visualise correlations
res_var <- factoextra::get_pca_var(res_pca)
df_features_pca <- merge(
  df_features,
  as.data.frame(res_var$coord[, 1:2]) %>% dplyr::mutate(
    chord = rownames(.) %>% gsub("\\[(.*?)\\]", "c(\\1)", .)
  ),
  by = "chord",
  all.x = TRUE
)

pca_cor_table <- as.data.frame(
  round(
    cor(
      df_features_pca %>% dplyr::select(-chord)
    ), 2
  )
)

pca_corrplot <- ggcorrplot::ggcorrplot(
  pca_cor_table,
  hc.order = TRUE,
  type = "lower",
  lab = TRUE) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    legend.title = element_blank(),
    text = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.text.x = element_text(size = 20))

pca_cor_table_neat <- pca_cor_table %>%
  dplyr::filter(colnames(.) %in% c("Dim.1", "Dim.2"))

View(pca_cor_table_neat)

#Multiple regression of features onto each PC

#Model definitions
psych_d1 <- as.formula(scale(Dim.1) ~
                         scale(roughness) +
                         scale(harmonicity_milne) +
                         scale(count) +
                         scale(cardinality) +
                         scale(representation_in_major_scale) +
                         scale(from_major))

psych_d1_unscaled <- as.formula(Dim.1 ~
                         roughness +
                         harmonicity_milne +
                         count +
                         cardinality +
                         representation_in_major_scale +
                         from_major)

psych_d2 <- as.formula(scale(Dim.2) ~
                         scale(roughness) +
                         scale(harmonicity_milne) +
                         scale(count) +
                         scale(cardinality) +
                         scale(representation_in_major_scale) +
                         scale(from_major))

psych_d2_unscaled <- as.formula(Dim.2 ~
                         roughness +
                         harmonicity_milne +
                         count +
                         cardinality +
                         representation_in_major_scale +
                         from_major)

#Run OLS models

#Note: the major triad is an outlier (high leverage) in predictor space.
fit_pca_psych_d1 <- lm(psych_d1, data = df_features_pca, na.action = "na.fail")
performance::check_model(fit_pca_psych_d1)
performance::check_outliers(fit_pca_psych_d1)
summary(fit_pca_psych_d1)

fit_pca_psych_d1_unscaled <- lm(
  psych_d1_unscaled, data = df_features_pca, na.action = "na.fail")
performance::check_model(fit_pca_psych_d1_unscaled)
performance::check_outliers(fit_pca_psych_d1_unscaled)
summary(fit_pca_psych_d1_unscaled)

fit_pca_psych_d2 <- lm(psych_d2, data = df_features_pca, na.action = "na.fail")
performance::check_model(fit_pca_psych_d2)
performance::check_outliers(fit_pca_psych_d2)
summary(fit_pca_psych_d2)

fit_pca_psych_d2_unscaled <- lm(
  psych_d2_unscaled, data = df_features_pca, na.action = "na.fail")
performance::check_model(fit_pca_psych_d2_unscaled)
performance::check_outliers(fit_pca_psych_d2_unscaled)
summary(fit_pca_psych_d2_unscaled)

#Run robust regressions
set.seed(1)
fit_pca_psych_d1_robust <- MASS::rlm(
  psych_d1,
  data = df_features_pca,
  seed = 1,
  method = "MM",
  acc = 1e-15,
  maxit = 10000,
  na.action = "na.fail")
summarise_robust(fit_pca_psych_d1_robust)

set.seed(1)
fit_pca_psych_d1_robust_unscaled <- MASS::rlm(
  psych_d1_unscaled,
  data = df_features_pca,
  seed = 1,
  method = "MM",
  acc = 1e-15,
  maxit = 10000,
  na.action = "na.fail")
summarise_robust(fit_pca_psych_d1_robust_unscaled)

set.seed(1)
fit_pca_psych_d2_robust <- MASS::rlm(
  psych_d2,
  data = df_features_pca,
  seed = 1,
  method = "MM",
  acc = 1e-15,
  maxit = 10000,
  na.action = "na.fail")
summarise_robust(fit_pca_psych_d2_robust)

set.seed(1)
fit_pca_psych_d2_robust_unscaled <- MASS::rlm(
  psych_d2_unscaled,
  data = df_features_pca,
  seed = 1,
  method = "MM",
  acc = 1e-15,
  maxit = 10000,
  na.action = "na.fail")
summarise_robust(fit_pca_psych_d2_robust_unscaled)

#Bootstrap OLS models
fit_pca_psych_d1_bca <- bca_model(
  psych_d1,
  data = df_features_pca,
  mode = "ols")
print(fit_pca_psych_d1_bca$par)

fit_pca_psych_d1_bca_unscaled <- bca_model(
  psych_d1_unscaled,
  data = df_features_pca,
  mode = "ols")
print(fit_pca_psych_d1_bca_unscaled$par)

fit_pca_psych_d2_bca <- bca_model(
  psych_d2,
  data = df_features_pca,
  mode = "ols")
print(fit_pca_psych_d2_bca$par)

fit_pca_psych_d2_bca_unscaled <- bca_model(
  psych_d2_unscaled,
  data = df_features_pca,
  mode = "ols")
print(fit_pca_psych_d2_bca_unscaled$par)

#Checking relationships between participant coordinates and background features

#Combine questionnaire data with PCA coordinates
res_ind <- factoextra::get_pca_ind(res_pca)

df_participant_features <-
  df_questionnaire_recoded %>% bind_cols(res_ind$coord[, 1:2])

#Add taste-typicality and reliability scores

average_scores_per_participant <- df_trials %>% 
  dplyr::select(participant_id, chord, answer) %>% 
  dplyr::group_by(participant_id, chord) %>%
  dplyr::summarise(mean_rating = mean(answer, na.rm = TRUE))

unique_participants <- unique(average_scores_per_participant$participant_id)
taste_typicality_scores <- tibble(participant_id = character(), taste_typicality = numeric())

for (participant in unique_participants) {

  other_participants <- average_scores_per_participant %>% 
    dplyr::filter(participant_id != participant) %>% 
    dplyr::group_by(chord) %>%
    dplyr::summarise(group_mean_rating = mean(mean_rating, na.rm = TRUE))

  participant_scores <- average_scores_per_participant %>% 
    dplyr::filter(participant_id == participant)
  
  correlation <- cor(participant_scores$mean_rating, other_participants$group_mean_rating)
  
  taste_typicality_scores <- taste_typicality_scores %>% 
    dplyr::add_row(participant_id = as.character(participant), taste_typicality = correlation)
}

df_participant_features <- df_participant_features %>% 
  mutate(participant_id = id)

df_participant_features <- df_participant_features %>% 
  merge(taste_typicality_scores, by = "participant_id") %>%
  merge(df_stability, by = "participant_id")

participant_cor_table <- Hmisc::rcorr(as.matrix(df_participant_features))

formatted_cor_table <- matrix(paste0(round(participant_cor_table$r, 2),
                                     " (p = ",
                                     round(participant_cor_table$P, 3),
                                     ")"),
                              nrow = nrow(participant_cor_table$r))

rownames(formatted_cor_table) <- rownames(participant_cor_table$r)
colnames(formatted_cor_table) <- colnames(participant_cor_table$r)

View(as_tibble(formatted_cor_table) %>%
       dplyr::filter(
         colnames(.) %in% c("Dim.1", "Dim.2")))

View(as_tibble(formatted_cor_table) %>%
       dplyr::filter(
         colnames(.) == "gender"))

#Correlation plot
cor_labels <- c(
  "gender" = "Gender",
  "wam_training" = "Formal music training",
  "MT" = "Musical Training",
  "SA" = "Singing Abilities",
  "PA" = "Perceptual Abilities",
  "EM" = "Emotion",
  "taste_typicality" = "Taste-typicality",
  "pearson_r" = "Reliability",
  "Dim.1" = "PC1",
  "Dim.2" = "PC2"
)

# Calculate correlation matrix and p-values
cor_test <- Hmisc::rcorr(as.matrix(df_participant_features %>% 
  dplyr::select(
    gender,
    wam_training,
    MT,
    SA,
    PA,
    EM,
    taste_typicality,
    pearson_r,
    Dim.1,
    Dim.2
  )))

cor_matrix <- cor_test$r
p_matrix <- cor_test$P

colnames(cor_matrix) <- cor_labels[colnames(cor_matrix)]
rownames(cor_matrix) <- cor_labels[rownames(cor_matrix)]
colnames(p_matrix) <- cor_labels[colnames(p_matrix)]
rownames(p_matrix) <- cor_labels[rownames(p_matrix)]

pca_cor_plot <- ggcorrplot(
  cor_matrix,
  type = "lower",
  lab = TRUE,
  lab_col = "black",
  ggtheme = theme_minimal() +
    theme(panel.grid = element_blank()),
  legend.title = "Correlation"
) +
  scale_fill_viridis_c(
    begin = 0.05,  # Start of the color scale
    end = 0.9,     # End of the color scale
    direction = -1, # Reverse the color direction
    name = "Correlation"
  ) +
  theme(
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16)
  )
ggsave("output/pca_cor_plot.pdf", pca_cor_plot, width = 20, height = 20)

# Extract coefficients and confidence intervals for individual participants
regression_coefs_ci <- df_trials %>% 
  dplyr::group_by(participant_id) %>%
  dplyr::summarise(
    model = list(lm(answer ~ as.numeric(roughness) + cardinality)),
    coefs = list(broom::tidy(model[[1]], conf.int = TRUE))
  ) %>%
  tidyr::unnest(coefs) %>%
  dplyr::filter(term != "(Intercept)")

# Create coefficient plots
roughness_coef_plot <- regression_coefs_ci %>%
  dplyr::filter(term == "as.numeric(roughness)") %>%
  ggplot(aes(x = reorder(participant_id, estimate), y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "A: Roughness",
    x = "Participants",
    y = "Coefficient Value"
  ) +
  theme(
    text = element_text(size = 16)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5)

cardinality_coef_plot <- regression_coefs_ci %>%
  dplyr::filter(term == "cardinality") %>%
  ggplot(aes(x = reorder(participant_id, estimate), y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "B: Cardinality",
    x = "Participants",
    y = "Coefficient Value"
  ) +
  theme(
    text = element_text(size = 16)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5)

# Combine plots using patchwork
combined_coef_plot <- roughness_coef_plot / cardinality_coef_plot

ggsave("output/regression_coefficients.pdf", combined_coef_plot, width = 10, height = 12)

#Assess the taste-typicality of participants with
#reliably non-negative roughness coefs
roughness_outliers <-regression_coefs_ci %>% 
  dplyr::filter(term == "as.numeric(roughness)") %>% 
  dplyr::filter(conf.high >= 0) %>% 
  dplyr::pull(participant_id)

summary(df_participant_features$taste_typicality)
max(df_participant_features$taste_typicality[df_participant_features$participant_id %in% roughness_outliers])

#Correlation between cardinaliy and ratings at the population level
cor.test(df_for_lm$cardinality, df_for_lm$answer)












# #Alternative bootstrap

# # Function to perform PCA and return explained variance
# # Original function to calculate explained variance
# pca_explained_variance <- function(data, indices) {
#   boot_sample <- data[indices, ]  # Create a bootstrapped sample
#   projected_data <- predict(res_pca, newdata = boot_sample)  # Project onto original PCA
#   explained_variance <- apply(projected_data, 2, var) / sum(apply(projected_data, 2, var))
#   return(explained_variance)
# }

# # Simple example that bootstraps the data once and returns the projected data
# pca_projected_data <- function(data) {
#   set.seed(1)
#   boot_sample <- data[sample(nrow(data), replace = TRUE), ]  # Create a bootstrapped sample
#   projected_data <- predict(res_pca, newdata = boot_sample)  # Project onto original PCA
#   return(projected_data)
# }

# # Perform bootstrapping
# set.seed(1)
# boot_results <- boot(data = df_pca, statistic = pca_explained_variance, R = 1000)

# # Initialize a tibble to store the results
# ci_tibble <- tibble(
#   component = integer(),
#   ci_95_lower = numeric(),
#   ci_95_upper = numeric()
# )

# # Calculate 95% confidence intervals for each component
# for (i in 1:ncol(df_pca)) {
#   ci_95 <- boot.ci(boot_results, type = "perc", index = i)
#   ci_tibble <- ci_tibble %>% add_row(
#     component = i,
#     ci_95_lower = ci_95$percent[4],
#     ci_95_upper = ci_95$percent[5]
#   )
# }

# # Create a plot for explained variances with error bars from ci_tibble

# # Extract original explained variances
# original_explained_variance <- apply(res_pca$x, 2, var) / sum(apply(res_pca$x, 2, var))

# # Create a data frame for plotting
# plot_data <- tibble(
#   component = 1:length(original_explained_variance),
#   explained_variance = original_explained_variance,
#   ci_95_lower_boot = ci_tibble$ci_95_lower,
#   ci_95_upper_boot = ci_tibble$ci_95_upper
# ) %>% left_join(ci_95_df, by = "component")



# # Plot the confidence intervals
# ci_plot_all <- ggplot(plot_data[1:10, ], aes(x = component)) +
#   geom_errorbar(aes(ymin = lower_perm * 100, ymax = upper_perm * 100), width = 0.5, color = "red") +
#   geom_point(aes(y = explained_variance * 100), color = "blue") +
#   geom_text(aes(y = explained_variance * 100, 
#                 label = round(explained_variance * 100, 1), 
#                 vjust = ifelse(component >= 7, 1.5, -1)), 
#             color = "blue", size = 6) +  # Increase text size
#    geom_errorbar(aes(ymin = ci_95_lower_boot * 100, ymax = ci_95_upper_boot * 100), width = 0.5, color = "blue") +
#   labs(x = "Component", y = "Explained Variance (%)") +
#   scale_x_continuous(breaks = 1:10) +
#   scale_y_continuous(breaks = seq(0, 21, by = 2.5), limits = c(0, 21)) +
#   theme_bw () + theme(
#   text = element_text(size = 16),  # Set a common text size
#   axis.title = element_text(size = 18),
#   axis.text = element_text(size = 14),
#   legend.text = element_text(size = 12),
#   legend.title = element_text(size = 14),
#   plot.title = element_blank()
# )

# # Save the plot
# ggsave("output/ci_plot_all.png", plot = ci_plot_all, height = 8, width = 12, dpi = 600)
