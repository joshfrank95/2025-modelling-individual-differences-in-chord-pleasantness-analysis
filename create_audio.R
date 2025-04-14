#Create .wav files of the Shepard tones used in the experiment

library(tuneR)

#Create specific Shepard chords
create_shepard_audio <- function(midi_notes, sample_rate = 44100,
  duration = 2, amplitude_scale = 0.8) {

  audio <- numeric(duration * sample_rate)

  # Get Shepard spectrum for the chord
  spectrum <- shepard_chord(midi_notes)

  for (i in 1:nrow(spectrum)) {

    freq <- midi_to_freq(spectrum$midi_harmonics[i])

    t <- seq(0, duration - 1/sample_rate, 1/sample_rate)

    wave <- sin(2 * pi * freq * t)

    wave <- wave * spectrum$weights[i]

    audio <- audio + wave
  }

  # Normalize audio to prevent clipping
  audio <- audio / max(abs(audio))
  audio <- audio * amplitude_scale * 32767

  # Write to WAV file

  #Create directory if it doesn't exist
  dir.create("output/shepard_audio_specific", showWarnings = FALSE)

  output_file <- paste0("output/shepard_audio_specific/shepard_", 
    paste(midi_notes, collapse="_"), ".wav")

  tuneR::writeWave(tuneR::Wave(audio, samp.rate = sample_rate, bit = 16),
    output_file)

  return(output_file)
  plot(audio)
}

#A few testers
create_shepard_audio(c(60)) #Individual note
create_shepard_audio(c(60, 64, 67)) #Major triad
create_shepard_audio(c(52, 55, 60)) #Major triad, nominally inverted

#High PC1
create_shepard_audio(c(60, 61)) #Semitone/major seventh dyad
create_shepard_audio(c(60, 62)) #Whole-tone/minor seventh dyad
create_shepard_audio(c(60, 61, 62)) #Semitone cluster

#Low PC1
create_shepard_audio(c(60, 63, 66, 68)) #Dominant seventh
create_shepard_audio(c(60, 62, 64, 67)) #Major add-nine
create_shepard_audio(c(60, 64, 67)) #Major triad

#High PC2
create_shepard_audio(c(60, 64, 67)) #Major triad
create_shepard_audio(c(60, 64)) #Major third/minor sixth dyad
create_shepard_audio(c(60, 61)) #Semitone/major seventh dyad

#Low PC2
create_shepard_audio(c(60, 63, 64, 67))
create_shepard_audio(c(60, 62, 63, 66))
create_shepard_audio(c(60, 63, 66, 67)) #All cardinality 4

#Generate audio for all chords heard in the experiment
generate_audio_from_spect <- function(df_trials) {
  
  # Create directory for audio files
  audio_dir <- "output/shepard_audio_full"
  if (!dir.exists(audio_dir)) {
    dir.create(audio_dir, recursive = TRUE)
  }

  # Get unique realized chords
  unique_chords <- unique(df_trials$realized_chord)

  # Iterate over each unique chord
  for (chord in unique_chords) {
    # Get the Shepard spectrum for the chord
    spectrum <- shepard_chord(chord)

    # Initialize empty audio vector
    audio <- numeric(2 * 44100) # 2 seconds at 44100 Hz

    # For each harmonic component
    for (i in 1:nrow(spectrum)) {

      freq <- midi_to_freq(spectrum$midi_harmonics[i])

      t <- seq(0, 2 - 1/44100, 1/44100)

      wave <- sin(2 * pi * freq * t)

      wave <- wave * spectrum$weights[i]

      audio <- audio + wave
    }

    # Normalize audio to prevent clipping
    audio <- audio / max(abs(audio))
    audio <- audio * 0.8 * 32767

    # Create output file name
    output_file <- paste0(audio_dir, "/shepard_",
      paste(chord, collapse="_"), ".wav")

    # Write to WAV file

    #Create directory if it doesn't exist
    dir.create(audio_dir, showWarnings = FALSE)

    tuneR::writeWave(tuneR::Wave(audio, samp.rate = 44100, bit = 16),
      output_file)
  }
}

generate_audio_from_spect(df_trials)
