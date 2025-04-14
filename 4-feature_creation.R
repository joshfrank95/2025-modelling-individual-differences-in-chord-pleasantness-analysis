#Compute stimulus features and add to rating data

library(jsonlite)
library(incon)

#Cardinality, Shepard spectrum, roughness, harmonicity
#Note: This may take several minutes to run
df_trials <- df_trials %>%
  dplyr::mutate(
    chord = purrr::map(chord, jsonlite::fromJSON),
    realized_chord = purrr::map(realized_chord, jsonlite::fromJSON),
    cardinality = purrr::map_int(chord, length),
    spect = purrr::map(realized_chord, shepard_chord),
    sparse_pi_spect = purrr::map(spect, ~ hrep::sparse_pi_spectrum(
      list(
        pitch = .$midi_harmonics,
        amplitude = .$weights
      ))),
    sparse_fr_spect = purrr::map(
      sparse_pi_spect, ~ hrep::sparse_fr_spectrum(.)),
    roughness = purrr::map(sparse_fr_spect, dycon::roughness_hutch),
    harmonicity_milne = purrr::map(
      sparse_pi_spect, ~ get_harmonicity(., mode = "milne")),
    harmonicity_harrison = purrr::map(
      sparse_pi_spect, ~ get_harmonicity(., mode = "harrison")),
    chord = as.character(chord)
  )

#Quick fix for colon notation
df_trials$chord <- sapply(df_trials$chord, colon_to_list)

#Familiarity
familiarity <-
  tibble(
    pc_set = hrep::pc_set_alphabet$by_id,
    count = corpdiss::count_chords(hcorp::popular_1, type = "pc_set")
  ) %>%
  dplyr::mutate(chord = map(pc_set, hrep::pc_set_type)) %>%
  dplyr::mutate(chord = as.character(chord)) %>%
  dplyr::group_by(chord) %>%
  dplyr::summarise(count = sum(count)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    count = count / sum(count)
  )

df_trials <- merge(df_trials, familiarity, by = "chord", all.x=TRUE)

#Major-scale compatibility
major_scale <- c(0, 2, 4, 5, 7, 9, 11)
major_chords <- get_combinations(major_scale)
chord_counts <- get_chord_counts(major_chords)
chord_counts$chord <- paste0(
  "c(", gsub(pattern = " ", replacement = ", ", chord_counts$chord), ")")

df_trials <-
  left_join(
    df_trials,
    chord_counts %>%
      dplyr::select(- count, representation_in_major_scale = proportion),
    by = "chord"
  ) %>%
  dplyr::mutate(
    representation_in_major_scale = if_else(
      is.na(representation_in_major_scale),
      0,
      representation_in_major_scale
    )
  ) %>%
  dplyr::mutate(from_major = case_when(
    representation_in_major_scale > 0 ~ TRUE,
    representation_in_major_scale <= 0 ~ FALSE
  ))

#Intervals and scale degrees
df_trials$interval_vector <- purrr::map(
  df_trials$chord %>% lapply(., function(x) eval(parse(text = x))),
  hrep::int_vec)

for (interval in 1:6) {
  col <- paste0("interval_count_", interval)
  df_trials[[col]] <- purrr::map_int(df_trials$interval_vector, interval)
}

# Scale degrees
df_trials$root <- purrr::map_int(df_trials$chord %>%
                            lapply(., function(x) eval(parse(text = x))),
                          parn88::root)

df_trials$scale_degrees <- purrr::map2(
  df_trials$chord %>% lapply(., function(x) eval(parse(text = x))),
  df_trials$root,
  ~ sort((.x - .y) %% 12)
)

for (scale_degree in 0:11) {
  col <- paste0("scale_degree_", scale_degree)
  df_trials[[col]] <- map_lgl(
    df_trials$scale_degrees,
    ~ scale_degree %in% .
  )
}

#Chordwise features dataframe
df_features <-
  df_trials %>%
  dplyr::group_by(chord) %>%
  dplyr::summarise(
    cardinality = first(cardinality),
    harmonicity_milne = mean(as.numeric(harmonicity_milne)),
    harmonicity_harrison = mean(as.numeric(harmonicity_harrison)),
    roughness = mean(as.numeric(roughness)),
    count = first(count),
    representation_in_major_scale = first(representation_in_major_scale),
    from_major = first(from_major)
  ) %>%
  dplyr::ungroup()

df_features_2 <-
  df_trials %>%
  dplyr::select(
    starts_with("interval_count_"),
    starts_with("scale_degree_"),
    chord,
    - scale_degree_0
  ) %>%
  unique()

df_features <-
  merge(
    df_features,
    df_features_2,
    by = "chord",
    all.x = TRUE
  )
