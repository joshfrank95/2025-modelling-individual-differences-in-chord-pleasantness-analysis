#Variance modelling

library(lavaan)

#Create dataframe
df_sem <- df_trials %>%
  dplyr::select(
    participant_id,
    chord,
    is_repeat_trial,
    answer
  ) %>%
  pivot_wider(
    names_from = is_repeat_trial,
    values_from = answer
  ) %>%
  dplyr::rename(
    test = `FALSE`,
    retest = `TRUE`
  ) %>% dplyr::mutate(
  chord = gsub("[^0-9]", "", chord)
  ) %>% pivot_wider(
    names_from = chord,
    names_glue = "{.value}{chord}",
    values_from = c(test, retest)
  )

#SD-based approach
sd_boot <- bca_sd(df_sem)
sd_boot$par
mean(sd_boot$par$estimate)
max(sd_boot$par$estimate)
min(sd_boot$par$estimate)

sd_plot <- sd_boot$par %>%
  mutate(parameter = as.character(mgsub(
           pattern = c("c\\(", ")", ", "), replacement = rep("", 3), parameter)),
         parameter = factor(parameter, levels = parameter[order(estimate)])) %>%
  ggplot(
    aes(
      x = parameter,
      y = estimate,
      fill = parameter
    )
  ) + geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = ci_95_lower, ymax = ci_95_upper, width = 0.2)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90),
    text = element_text(size = 40),
    axis.text.y = element_text(size = 20)
  ) + labs(
    x = "Chord",
    y = "SD of normalised ratings"
  ) + guides(fill = "none") +
  theme(
    scale_fill_manual(scale_fill_viridis_c(option = "viridis"))
  ) +
  theme(
    text = element_text(size = 24),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  ) + coord_flip()

ggsave("output/sd_plot.pdf", plot = sd_plot, width = 12, height = 18)

df_sd <- merge(
  df_features %>% 
  dplyr::mutate(
    chord = as.character(mgsub(pattern = c("c\\(", ")", ", "), replacement = rep("", 3), chord))), 
  sd_boot$par %>% 
  dplyr::mutate(
    chord = as.character(mgsub(pattern = c("c\\(", ")"), replacement = rep("", 2), parameter)))  %>% 
  dplyr::select(chord, estimate),
  by = "chord")

#Check correlations between SD and features
sd_cor <- Hmisc::rcorr(df_sd %>% dplyr::select(
    -chord, -starts_with("interval"), -starts_with("scale_degree")) %>%
  as.matrix(), type = "pearson")

sd_cor_neat <- merge(
  sd_cor$r %>% as.data.frame() %>%
    dplyr::select(estimate) %>%
    tibble::rownames_to_column("feature"),
  sd_cor$P %>% as.data.frame() %>%
    dplyr::select(estimate) %>%
    dplyr::rename(p.value = estimate) %>%
    tibble::rownames_to_column("feature"),
  by = "feature"
) %>%
  arrange(desc(abs(estimate)))

sd_cor_neat

#SEM approach

#Bootstrap models over all chords
identifiers <- gsub("[a-z]", "", colnames(df_sem)[grep("retest", colnames(df_sem))])

estimates_bca <- tibble(chord = rep(0, 68),
                        pref_var_estimate = rep(0, 68),
                        pref_var_ci_95_lower = rep(0, 68),
                        pref_var_ci_95_upper = rep(0, 68),
                        test_var_estimate = rep(0, 68),
                        test_var_ci_95_lower = rep(0, 68),
                        test_var_ci_95_upper = rep(0, 68),
                        retest_var_estimate = rep(0, 68),
                        retest_var_ci_95_lower = rep(0, 68),
                        retest_var_ci_95_upper = rep(0, 68),
                        mean_test_retest_pref_var = rep(0, 68),
                        mean_test_retest_var_ci_95_lower = rep(0, 68),
                        mean_test_retest_var_ci_95_upper = rep(0, 68))

for (i in seq_along(identifiers)) {

  current_id <- identifiers[i]

  form <- paste0("preference", current_id, " =~ ", "1*test", current_id, " + ", "1*retest", current_id)

  out <- bca_model(formula = form, data = df_sem, mode = "sem", .nboot = 10000)

  estimates_bca$chord[i] <- current_id

  parameters <- out$par

  estimates_bca$pref_var_estimate[i] <- as.numeric(
    parameters[parameters$parameter ==
    paste0("coef_preference", current_id, "~~preference", current_id),
    colnames(parameters) == "estimate"])

  estimates_bca$pref_var_ci_95_lower[i] <- as.numeric(
    parameters[parameters$parameter ==
    paste0("coef_preference", current_id, "~~preference", current_id),
    colnames(parameters) == "ci_95_lower"])

  estimates_bca$pref_var_ci_95_upper[i] <- as.numeric(
    parameters[parameters$parameter ==
    paste0("coef_preference", current_id, "~~preference", current_id),
    colnames(parameters) == "ci_95_upper"])

  estimates_bca$test_var_estimate[i] <- as.numeric(
    parameters[parameters$parameter ==
    paste0("coef_test", current_id, "~~test", current_id),
    colnames(parameters) == "estimate"])

  estimates_bca$test_var_ci_95_lower[i] <- as.numeric(
    parameters[parameters$parameter ==
    paste0("coef_test", current_id, "~~test", current_id),
    colnames(parameters) == "ci_95_lower"])

  estimates_bca$test_var_ci_95_upper[i] <- as.numeric(
    parameters[parameters$parameter ==
    paste0("coef_test", current_id, "~~test", current_id),
    colnames(parameters) == "ci_95_upper"])

  estimates_bca$retest_var_estimate[i] <- as.numeric(
    parameters[parameters$parameter ==
    paste0("coef_retest", current_id, "~~retest", current_id),
    colnames(parameters) == "estimate"])

  estimates_bca$retest_var_ci_95_lower[i] <- as.numeric(
    parameters[parameters$parameter ==
    paste0("coef_retest", current_id, "~~retest", current_id),
    colnames(parameters) == "ci_95_lower"])

  estimates_bca$retest_var_ci_95_upper[i] <- as.numeric(
    parameters[parameters$parameter ==
    paste0("coef_retest", current_id, "~~retest", current_id),
    colnames(parameters) == "ci_95_upper"])

  estimates_bca$mean_test_retest_pref_var[i] <- as.numeric(
    parameters[parameters$parameter ==
    "mean_test_retest_var",
    colnames(parameters) == "estimate"])

  estimates_bca$mean_test_retest_var_ci_95_lower[i] <- as.numeric(
    parameters[parameters$parameter ==
    "mean_test_retest_var",
    colnames(parameters) == "ci_95_lower"])

  estimates_bca$mean_test_retest_var_ci_95_upper[i] <- as.numeric(
    parameters[parameters$parameter ==
    "mean_test_retest_var",
    colnames(parameters) == "ci_95_upper"])

}

saveRDS(estimates_bca, file = "output/estimates_bca.rds")

bca_sem_plot <- estimates_bca %>%
  arrange(pref_var_estimate) %>%
  mutate(chord = factor(chord, levels = chord)) %>%
  ggplot(
    aes(
      x = chord,
      y = pref_var_estimate,
      fill = "Preference",
    )
  ) + geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(
    aes(
      x = chord,
      ymin = pref_var_ci_95_lower,
      ymax = pref_var_ci_95_upper,
    ),
    position = position_dodge(width = 0.9),
    width = 0.2
  ) +
  geom_bar(
    aes(
      x = chord,
      y = -mean_test_retest_pref_var,
      fill = "Residual",
    ),
    stat = "identity",
    position = position_dodge(width = 0.9)
  ) +
  geom_errorbar(
    aes(
      x = chord,
      ymin = -mean_test_retest_var_ci_95_lower,
      ymax = -mean_test_retest_var_ci_95_upper,
    ),
    position = position_dodge(width = 0.9),
    width = 0.2
  ) +
  scale_fill_manual(values = c("Preference" = "blue", "Residual" = "red")) +
  guides(fill = guide_legend(title.position = "top")) +
  labs(fill = "Variance component") +
  scale_y_continuous(
    labels = function(y) abs(y)
  ) +
  theme_minimal() +
  labs(
    x = "Chord",
    y = "Normalised variance"
  ) +
  theme(
    text = element_text(size = 24)
  ) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
    legend.position = "top"
  )

ggsave("output/sem_variance_plot.pdf", plot = bca_sem_plot,
  width = 18, height = 12)

#Do psychoacoustic feature relate to degrees of individual difference and noise?
df_sem_features <- merge(
  estimates_bca,
  df_features %>% dplyr::mutate(
    chord = as.character(
      mgsub(
        pattern = c("c\\(", ")", ", "), replacement = rep("", 3), chord))
  ),
  by = "chord"
)

sem_cor <-Hmisc::rcorr(df_sem_features %>% 
dplyr::select(-chord, -starts_with("interval"), -starts_with("scale_degree")) %>% 
as.matrix(), type = "spearman")

sem_cor_preference <- merge(
  sem_cor$r %>% as.data.frame() %>% 
    dplyr::select(pref_var_estimate) %>% 
    tibble::rownames_to_column("feature"),
  sem_cor$P %>% as.data.frame() %>% 
    dplyr::select(pref_var_estimate) %>%
    dplyr::rename(p.value = pref_var_estimate) %>%
    tibble::rownames_to_column("feature"),
  by = "feature"
) %>%
  dplyr::filter(!grepl("(var|lower|upper|estimate)$", feature)) %>%
  arrange(desc(abs(pref_var_estimate)))

sem_cor_preference

sem_cor_noise <- merge(
  sem_cor$r %>% as.data.frame() %>% 
    dplyr::select(mean_test_retest_pref_var) %>% 
    tibble::rownames_to_column("feature"),
  sem_cor$P %>% as.data.frame() %>% 
    dplyr::select(mean_test_retest_pref_var) %>%
    dplyr::rename(p.value = mean_test_retest_pref_var) %>%
    tibble::rownames_to_column("feature"),
  by = "feature"
) %>%
  dplyr::filter(!grepl("(var|lower|upper|estimate)$", feature)) %>%
  arrange(desc(abs(mean_test_retest_pref_var)))

sem_cor_noise



