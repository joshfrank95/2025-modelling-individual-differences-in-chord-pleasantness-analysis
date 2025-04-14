#Definitions of functions for feature creation, permutation testing,
#and bootstrapping, as well as convenience functions

library(parallel)

#Calculate SD of test and retest ratings for each chord
bca_sd <- function(df) {
  R <- 10000
  alpha <- 0.05
  set.seed(1)
  
  # Get chord numbers from column names
  chord_nums <- unique(gsub("^(test|retest)", "", 
                          names(df)[grep("^(test|retest)", names(df))]))
  
  statistic <- function(d, indices) {
    d <- d[indices, ]
    sapply(chord_nums, function(chord) {
      test_col <- paste0("test", chord)
      retest_col <- paste0("retest", chord)
      
      test_sd <- sd(d[[test_col]], na.rm = TRUE)
      retest_sd <- sd(d[[retest_col]], na.rm = TRUE)
      
      mean(c(test_sd, retest_sd), na.rm = TRUE)
    })
  }

  original_statistic <- statistic(df, indices = seq_len(nrow(df)))
  
  boot_results <- boot::boot(df, statistic, R = R)

  results <- tibble(
    parameter = paste0("c(", chord_nums, ")"),
    estimate = original_statistic,
    ci_95_lower = NA_real_,
    ci_95_upper = NA_real_
  )

  for (i in seq_len(nrow(results))) {
    res_boot_bca <- boot::boot.ci(boot_results, type = "bca", index = i)
    if (!is.null(res_boot_bca)) {
      ci <- res_boot_bca$bca[4:5]
      results$ci_95_lower[i] <- ci[1]
      results$ci_95_upper[i] <- ci[2]
    }
  }
  
  return(list(par = results))
}

library(R6)
library(reticulate)
library(hrep)
library(mgsub)

#Convert between MIDI and frequency values
midi_to_freq <- function(note){
  freq <- 2 ** ((note - 69) / 12) * 440
  return(freq)
}

freq_to_midi <- function(note){
  midi <- log2(note / 440) * 12 + 69
  return(midi)
}

#Create a Shepard-timbre spectrum for a given chord
gaussian <- function(x, mu, sigma) {
  N <- sqrt(2 * pi * (sigma ** 2))
  return(1 / N * exp(-1 * ((x - mu) ** 2) / (2 * sigma ** 2)))
}

shepard <- function(num_octave_transpositions,
                    midi_note,
                    octave_definition){
  midi_harmonics <- numeric()
  weights <- numeric()
  norm <- 0
  gamma <- log2(octave_definition)
  freq <- midi_to_freq(midi_note)

  for (n in 0:(2 * num_octave_transpositions)){
    curr_freq <- freq_to_midi(
      freq * octave_definition ** (n - num_octave_transpositions)
    )
    weight <- gaussian(curr_freq, gamma * 65.5, gamma * 8.2)
    weights <- append(weights, weight)
    norm <- norm + weight ** 2
  }

  # weights <- purrr::map(weights, ~ . / sqrt(norm))

  weights <- weights / sqrt(norm)

  for (n in -4:4){
    midi_harmonics <- append(midi_harmonics, midi_note + (12 * n))
  }

  stopifnot(length(weights) == length(midi_harmonics))

  shep_def <- tibble(midi_harmonics, weights)
  shep_def <- tidyr::unnest(shep_def, cols = c(weights))

  return(shep_def)
}

.shepard_chord <- function(chord){
  result <- dplyr::bind_rows(lapply(chord, function(note){
    shepard(
      num_octave_transpositions = 4,
      midi_note = note,
      octave_definition = 2)
  }))
  return(result)
}

shepard_chord <- memoise::memoise(.shepard_chord)

#Create a piano-timbre spectrum for a given chord
piano_amps <- c(1, 0.508024615648808, 0.0933536943448577, 0.222457796845064,
                0.102246797048138, 0.0606420093604131, 0.104786291832625,
                0.0843553191337145, 0.0445420508366155, 0.0307915719139996)

get_piano_harmonics <- function(note) {
  base_freq <- midi_to_freq(note)
  harmonic_freqs <- base_freq * (1:10)
  midi_harmonics <- freq_to_midi(harmonic_freqs)
  weights <- piano_amps
  piano_def <- tibble(midi_harmonics = midi_harmonics, weights = weights)
  return(piano_def)
}

.piano_chord <- function(chord) {
  result <- dplyr::bind_rows(lapply(chord, get_piano_harmonics))
  return(result)
}

piano_chord <- memoise::memoise(.piano_chord)

#Calculate a chord's harmonicity using either the model of Milne (2013)
#or Harrison & Pearce (2018). The input x must be a sparse_pi_spectrum.
.get_harmonicity <- function(x, mode) {
  pc_spectrum <- hrep::smooth_pc_spectrum(x)
  pitch_profile <- get_pitch_profile(pc_spectrum)
  eval_pitch_profile(pitch_profile, mode)
}

get_harmonicity <- memoise::memoise(.get_harmonicity)

get_pitch_profile <- function(x) {
  stopifnot(is(x, "smooth_pc_spectrum"))
  x <- as.numeric(x)
  array_dim <- length(x)
  template <- hrep::smooth_pc_spectrum(
    hrep::pi_chord(60),
    array_dim = array_dim
  )
  res <- har18::sweep_template(x, template)
  hrep::.smooth_pc_spectrum(res)
}

eval_pitch_profile <- function(profile, mode) {
  if (mode == "harrison") {
    kl_div_from_uniform(as.numeric(profile))
  } else if (mode == "milne") {
    max(as.numeric(profile))
  } else {
    stop("unrecognised mode: ", mode)
  }
}

kl_div_from_uniform <- function(x) {
  probs <- x / sum(x)
  n <- length(probs)
  uniform_probs <- 1 / n
  non_zero_probs <- probs[probs > 0]
  sum(
    non_zero_probs * log(non_zero_probs / uniform_probs, base = 2)
  )
}

#Check a chord's X-scale compatibility, i.e.,
#number of unique appearances in any scale X.
#This version is for pc-set types (Forte classes).
get_combinations <- function(scale) {
  combinations <- unlist(lapply(seq_along(scale), function(i) {
    combn(scale, i, simplify = FALSE)
  }), recursive = FALSE)

  return(combinations)
}

#The output of get_combinations(scale_X) becomes the input to this function.
get_chord_counts <- function(chords) {
  chord_types <- lapply(chords, function(chord) chord - chord[1])
  chord_counts <- tibble(
    chord = purrr::map_chr(chord_types, ~ as.character(hrep::pc_set_type(.))),
    count = 1
  )

  chord_counts <- chord_counts %>%
    dplyr::group_by(chord) %>%
    dplyr::summarise(
      count = n(),
      .groups = 'drop'
    ) %>%
    dplyr::mutate(
      proportion = count / sum(count)
    )

  return(chord_counts)

}

#Bootstrap a given model.
#Currently supports OLS, robust regression with MM estimation (in theory),
#and SEM.
bca_model <- function(formula, data, mode, ..., .nboot = 1000) {

  set.seed(1)

  statistic <- function(data, indices) {
    sampled_data <- data[indices, ]
    warning_flag <- FALSE
    log_file <- NULL
    mean_test_retest_var <- NULL
    if (mode == "ols"){
      mod <- lm(formula = formula, data = sampled_data, ...)

      fit_stats <-
        broom::glance(mod) |>
        dplyr::select(
          "r_squared" = r.squared,
          "adj_r_squared" = adj.r.squared
        ) |>
        unlist()
    } else if (mode == "robust") {
      mod <- MASS::rlm(
        formula = formula,
        data = sampled_data,
        seed = 1,
        method = "MM",
        init = "ls",
        acc = 1e-15,
        maxit = 10000,
        na.action = "na.fail"
      )
      fit_stats <- NULL
    } else if (mode == "sem") {
      mod <- withCallingHandlers(
        expr = lavaan(
          model = formula, data = sampled_data, model.type = "sem", ...),
        warning = function(w) {
          warning_flag <<- TRUE
          invokeRestart("muffleWarning")
        }
      )
      fit_stats <- NULL
    }

    coef_stats <- coef(mod)

    names(coef_stats) <- gsub(
      "(Intercept)", "intercept", names(coef_stats), fixed = TRUE)
    names(coef_stats) <- paste("coef_", names(coef_stats), sep = "")

    if (mode == "sem") {
      test_coefs <- as.numeric(coef_stats[grep("coef_test", names(coef_stats))])
      retest_coefs <- as.numeric(coef_stats[grep("coef_retest", names(coef_stats))])
      if (length(test_coefs) > 0 && length(retest_coefs) > 0) {
        mean_test_retest_var <- mean(c(test_coefs, retest_coefs))
      } else {
        mean_test_retest_var <- 0
      }
      mean_test_retest_var <- rlang::set_names(
        mean_test_retest_var, "mean_test_retest_var")
    }


    coef_stats <- c(fit_stats, coef_stats, mean_test_retest_var)

    if (warning_flag) {
      dir.create("debug_logs", showWarnings = FALSE)
      log_file <- paste0("debug_logs/debug_log_", Sys.time(), ".rds")
      saveRDS(
        list(sampled_data = sampled_data, formula = formula, model = mod),
        log_file
      )
    }

    coef_stats

  }

  if (mode == "ols") {
    non_bootstrapped_model <- lm(formula = formula, data = data, ...)
  } else if (mode == "robust") {
    non_bootstrapped_model <- withCallingHandlers(
      expr = {
        MASS::rlm(
          formula = formula, 
          data = data, 
          seed = 1,
          method = "MM",
          init = "ls",
          acc = 1e-15,
          maxit = 10000,
          na.action = "na.fail"
        )
      },
      warning = function(w) {
        warning_flag <<- TRUE
        invokeRestart("muffleWarning")
      }
    )
  } else if (mode == "sem") {
    non_bootstrapped_model <- lavaan(
      model = formula, data = data, model.type = "sem")
  }

  non_bootstrapped_stats <- statistic(data, indices = seq_len(nrow(data)))

  res_boot <- boot::boot(
    data = data,
    statistic = statistic,
    R = .nboot,
    parallel = "multicore",
    ncpus = detectCores() - 1
  )

  df <- tibble::tibble(
    parameter = names(non_bootstrapped_stats),
    estimate = non_bootstrapped_stats,
    ci_95_lower = NA_real_,
    ci_95_upper = NA_real_
  )

  for (i in seq_len(nrow(df))) {
    res_boot_bca <- boot::boot.ci(res_boot, type = "bca", index = i)
    ci <- res_boot_bca$bca[4:5]
    df$ci_95_lower[i] <- ci[1]
    df$ci_95_upper[i] <- ci[2]
  }

  warning_count <- sum(apply(res_boot$t, 1, function(row) any(is.na(row))))

  list(
    par = df,
    mod = non_bootstrapped_model,
    nboot = .nboot,
    warnings = warning_count
  )
}

#Convert colon notation to standard list
colon_to_list <- function(chord) {
  if (grepl(":", as.character(chord))) {
    nums <- unlist(strsplit(chord, ":"))
    start_num <- as.numeric(nums[1])
    end_num <- as.numeric(nums[2])
    list <- paste0("c(", paste(seq(start_num, end_num), collapse = ", "), ")")
    return(list)
  } else {
    return(chord)
  }
}

#Summarise robust regression with p-values
summarise_robust <- function(robust_fit) {
  print(
    broom::tidy(robust_fit) %>%
      dplyr::mutate(
        p.value = repmod::rob.pvals(robust_fit)
      ),
    n = 100
  )
}

#Custom scale function that avoids issues with 0-sd variables
scale <- function(x) {
  stdev <- sd(x)
  if (is.na(stdev) || stdev == 0) {
    return(x)
  } else {
    return(base::scale(x))
  }
}

#Permutation test for PCA
library(boot)
library(tibble)
library(dplyr)

simple_pca_perm_for_loop <- function(
  df, scale = FALSE, center = FALSE, nperm = 1000,
  boot = TRUE, nboot = 1000, 
  alpha = 0.05) {

  pca_res <- prcomp(df, scale. = scale, center = center)
  original_explained_variances <- pca_res$sdev^2 / sum(pca_res$sdev^2)

  set.seed(1)

  # Preallocate storage for permuted data frames and results
  perm_dfs <- vector("list", nperm)
  perm_results <- matrix(NA, nrow = nperm, ncol = ncol(df))

  # Run permutation test
  for (i in seq_len(nperm)) {
    perm_dfs[[i]] <- as.data.frame(apply(df, 2, sample))
    pca_res_perm <- prcomp(perm_dfs[[i]], scale. = scale, center = center)
    perm_results[i, ] <- pca_res_perm$sdev^2 / sum(pca_res_perm$sdev^2)
  }

  # Convert permutation results to matrix format
  perm_results <- t(perm_results)
  
  # Compute permutation p-values
  p_values <- sapply(seq_along(original_explained_variances), function(i) {
    mean(perm_results[i, ] > original_explained_variances[i])
  })
 
  result_tibble <- tibble(
    component = seq_along(original_explained_variances),
    p_value = p_values
  )

  return(list(
    result_tibble, perm_results, original_explained_variances, perm_dfs))

}

simple_pca_bootstrap <- function(df, scale = FALSE, center = FALSE, nboot = 1000) {

  pca_res <- prcomp(df, scale. = scale, center = center)
  original_explained_variances <- pca_res$sdev^2 / sum(pca_res$sdev^2)

  boot_variances <- function(df, indices) {
    df <- df[indices, , drop = FALSE]
    pca_res_boot <- prcomp(df, scale. = scale, center = center)
    explained_variance_boot <- pca_res_boot$sdev^2 / sum(pca_res_boot$sdev^2)
    return(explained_variance_boot)
  }

  # Bootstrap Confidence Intervals
  set.seed(1)
  boot_results <- boot::boot(df, boot_variances, R = nboot)
  num_components <- length(original_explained_variances)

  bootstrap_variances <- matrix(NA, nrow = nboot, ncol = num_components)

  for (i in seq_len(nboot)) {
    bootstrap_variances[i, ] <- boot_results$t[i, ]
  }

  boot_tibble <- tibble(
    component = seq_len(num_components),
    original_explained_variance = original_explained_variances,
    ci_95_lower = rep(NA_real_, num_components),
    ci_95_upper = rep(NA_real_, num_components)
  )

  bca_results <- vector("list", num_components)
  use_bca <- TRUE

  for (i in seq_len(num_components)) {
    bca_results[[i]] <- tryCatch(
      boot.ci(boot_results, type = "bca", index = i), 
      error = function(e) {
        message("BCa error for component ", i, ": ", e$message)
        return(NULL)
      }
    )
    if (is.null(bca_results[[i]]$bca)) {
      use_bca <- FALSE
      break
    }
  }

  # Use the appropriate method based on the availability of BCa results
  if (use_bca) {
    message("Using BCa method for confidence intervals.")
    for (i in seq_len(num_components)) {
      ci <- bca_results[[i]]$bca[4:5]
      boot_tibble$ci_95_lower[i] <- ci[1]
      boot_tibble$ci_95_upper[i] <- ci[2]
    }
  } else {
    message("Using percentile method for confidence intervals.")
    for (i in seq_len(num_components)) {
      res_boot_perc <- boot.ci(boot_results, type = "perc", index = i)
      ci <- res_boot_perc$percent[4:5]
      boot_tibble$ci_95_lower[i] <- ci[1]
      boot_tibble$ci_95_upper[i] <- ci[2]
    }
  }

  list(boot_tibble = boot_tibble, bootstrap_variances = bootstrap_variances)

}

