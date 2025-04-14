#Read rating and questionnaire data from PsyNet output

library(tidyverse)

#RATING DATA
df_trials <- read_csv("ChordTrial.csv", guess_max = Inf) %>%
  dplyr::select(
    "node_id",
    "participant_id",
    "is_repeat_trial",
    "answer",
    "time_taken",
    "chord",
    "base_pitch",
    "realized_chord",
    "complete"
  ) 

#Filter for incomplete IDs, with criteria:
# 1 - not all trials are complete (should catch most cases)
# 2 - number of trials is not 136 (for edge cases where the 136th trial was not completed)
incomplete_ids <- df_trials %>%
  dplyr::group_by(participant_id) %>%
  filter(!all(complete == TRUE)) %>%
  count() %>%
  filter(n != 136)

df_trials <- df_trials %>%
  filter(!participant_id %in% incomplete_ids$participant_id)

stopifnot(all(!is.na(df_trials$answer)))

#Normalise ratings within participants
df_trials <- df_trials %>%
  dplyr::group_by(participant_id) %>%
  dplyr::mutate(answer = as.numeric(scale(answer))) %>%
  dplyr::ungroup()

#QUESTIONNAIRE DATA
df_questionnaire <- read_csv("Participant.csv") %>%
  dplyr::select(
    "id",
    "complete",
    "answer",
    "questionnaire"
  )

#Split answers into separate columns
#First create a template tibble with N/A values to return in case of parsing errors
template_error_response <- df_questionnaire$questionnaire %>%
  map(~ tryCatch(
    jsonlite::fromJSON(.x) %>%
      list_flatten() %>%
      as_tibble(),
    error = function(e) NULL)) %>%
  keep(~ !is.null(.)) %>%
  first() %>%
  mutate(across(everything(), ~ NA))

#Try the JSON parsing, returning the error template if needed
df_questionnaire <-
  bind_cols(
    df_questionnaire %>% dplyr::select(- questionnaire),
    df_questionnaire$questionnaire %>%
      map_dfr(~ tryCatch(
        {
          jsonlite::fromJSON(.x) %>%
            list_flatten() %>%
            as_tibble()
        },
        error = function(e){
          template_error_response
        }
      )
      )
  )

#Extract problematic IDs for manual inspection
error_ids <- df_questionnaire %>%
  filter(rowSums(is.na(.)) > 0) %>%
  pull(id)

#Drop problematic IDs - in our case they are all appropriate to drop.
#Alternative approaches can be implemented as needed.
df_questionnaire <- df_questionnaire %>% filter(!id %in% error_ids)

#Manually fix a few columns and values
#The direct value setting is for cases where participants gave text answers, e.g., "none" becomes 0
df_questionnaire$age <- as.numeric(df_questionnaire$age)
df_questionnaire$gender <- as.factor(df_questionnaire$gender)
df_questionnaire$wam_training[23] <- 0
df_questionnaire$wam_training[26] <- 0
df_questionnaire$wam_training[41] <- 0
df_questionnaire$wam_training[90] <- 0
df_questionnaire$wam_training[62] <- 5
df_questionnaire$wam_training <- as.numeric(df_questionnaire$wam_training)

#Remove unneeded columns
df_questionnaire <- df_questionnaire %>%
  dplyr::select(-complete, -answer)

#Recode: scale reversal, numeric values, summary GMSI measures
recoding_scheme_genres <- c(
  "Never" = 0,
  "Seldom" = 1,
  "Sometimes" = 2,
  "Quite often" = 3,
  "Very often" = 4
)

recoding_scheme_GMSI <- c(
  "1" = 7,
  "2" = 6,
  "3" = 5,
  "4" = 4,
  "5" = 3,
  "6" = 2,
  "7" = 1
)

GMSI_cols_to_recode = c(
  "MT_03",
  "MT_07",
  "PA_08",
  "SA_04",
  "EM_02"
)

recoding_scheme_gender = c(
  "male" = 0,
  "Male" = 0,
  "man" = 0,
  "Man" = 0,
  "female" = 1,
  "Female" = 1,
  "woman" = 1,
  "Woman" = 1,
  "Non-Binary" = NA,
  "non binary" = NA,
  "Agender" = NA
)

df_questionnaire_recoded <- df_questionnaire %>%
  dplyr::select(- occupation) %>%
  dplyr::mutate(
    across(
      .cols = starts_with("genre") | starts_with("film"),
      .fns = ~ as.numeric(recoding_scheme_genres[.])
    )
  ) %>% dplyr::mutate(
    across(
      .cols = all_of(GMSI_cols_to_recode),
      .fns = ~ as.numeric(recoding_scheme_GMSI[.])
      )) %>% dplyr::mutate(
      MT = (MT_03 + as.numeric(MT_06) + MT_07) / 3,
      PA = PA_08,
      SA = (SA_03 + SA_04) /2,
      EM = (EM_01 + EM_02 + EM_03 + EM_04 + EM_05 + EM_06) / 6,
      total_music_listening = rowSums(dplyr::select(., starts_with("genre"))),
      total_film_viewing = rowSums(dplyr::select(., starts_with("film")))
    ) %>% dplyr::mutate(across(
      .cols = gender,
      .fns = ~ as.numeric(recoding_scheme_gender[.])
    )) %>%
  dplyr::select(
      - starts_with("MT_"),
      - starts_with("PA_"),
      - starts_with("SA_"),
      - starts_with("EM_")
    )
