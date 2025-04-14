#Provide descriptive statistics on the rating and questionnaire data

library(ggpubr)
library(viridis)

theme_set(theme_pubr())

#Chord-wise summary statistics
chordwise_summary <-
  df_trials %>%
  dplyr::group_by(chord) %>%
  dplyr::summarise(
    mean = mean(answer),
    sd = sd(answer),
    n = n(),
    se = sd / sqrt(n)
  )

#Bar graph of chord means and SEs
chordwise_barplot <-
  chordwise_summary %>%
  arrange(mean) %>%
  mutate(chord = gsub("\\[|\\]", "", chord),  # Remove square brackets from chord format
         chord = gsub(", ", "", chord),  # Remove commas and spaces
         chord = factor(chord, levels = chord)) %>%
  ggplot(
    aes(
      x = chord,
      y = mean,
      fill = chord
    )
  ) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se, width = .2)) +
  theme_bw() +
  theme(
    text = element_text(size = 40),
    axis.text.y = element_text(size = 20)
  ) +
  coord_flip() +
  scale_fill_viridis_d(option = "viridis", guide = "none") +
  labs(x = "Chord", y = "Normalised mean rating")

ggsave("output/chordwise_barplot.pdf", plot = chordwise_barplot, height = 24, width = 18)

#Check reliability of averaged data - two ways
#First - test-retest correlation averaging over all participants
chordwise_test_retest <-
  df_trials %>%
  dplyr::group_by(
    chord,
    is_repeat_trial
  ) %>%
  dplyr::summarise(
    mean = mean(answer)
  ) %>%
  dplyr::mutate(
    is_repeat_trial = if_else(is_repeat_trial, "retest", "test")
  ) %>%
  pivot_wider(
    names_from = "is_repeat_trial",
    values_from = "mean"
  )

cor(chordwise_test_retest$test, chordwise_test_retest$retest) # .99

#Second - classical test theory reliability
error_variance <- mean(chordwise_summary$se ^ 2)
signal_variance <- sd(chordwise_summary$mean) ^ 2
reliability <- signal_variance / (signal_variance + error_variance)
reliability # .99

#Check stability of rating profiles within participants
df_stability <-
  df_trials %>%
  filter(!is_repeat_trial) %>%
  dplyr::select(participant_id, chord, answer) %>%
  left_join(
    df_trials %>% filter(is_repeat_trial) %>% dplyr::select(participant_id, chord, answer),
    by = c("participant_id", "chord"),
    suffix = c("_test", "_retest")
  ) %>%
  dplyr::group_by(participant_id) %>%
  summarise(pearson_r = cor(answer_test, answer_retest, use = "pairwise.complete.obs"))

mean_stability <- mean(df_stability$pearson_r)
mean_stability # .53


# Histogram of participant stability scores
participant_stability_histogram <-
  ggplot(df_stability, aes(x = pearson_r)) +
  geom_histogram(
    binwidth = 0.1,
    color = "black",
    fill = "grey",
    boundary = 0
  ) +
  geom_vline(
    aes(xintercept = mean_stability),
    color = "red",
    linetype = "solid",
    linewidth = 1
  ) +
  scale_y_continuous(limits = c(0, 25)) +
  labs(
    x = "Test-retest correlation",
    y = "Frequency"
  )
ggsave("output/participant_stability_histogram.pdf", plot = participant_stability_histogram, height = 20, width = 20)

#Participant summary statistics
summary(df_questionnaire$age)
mean(df_questionnaire$age)
sd(df_questionnaire$age)

summary(df_questionnaire$wam_training)
mean(df_questionnaire$wam_training)
sd(df_questionnaire$wam_training)

table(df_questionnaire$gender)

time_avg <- sum(df_trials$time_taken / nrow(df_trials)) * (68*2) + 20 #Includes 20-second break
time_avg/60 #Average time taken on the rating portion of the experiment = 8.89 minutes

