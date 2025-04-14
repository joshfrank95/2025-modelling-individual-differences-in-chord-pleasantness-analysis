# 2025-individual-difference-in-chord-pleasantness-analysis

# Description

This repository contains the code for the analyses from the paper "Modelling
individual differences in chord pleasantness". It consists of a series of R 
scripts, which should be run in the order indicated by their numbering.

# Installation

It should be sufficient to clone the repository and run the scripts in order.
The data files required for the analyses are included in the repository.

# Breakdown of scripts

## 1-read_data.R

This script reads the data, cleans it, and performs some basic manipulations.
These include removing participants who did not complete the task, scaling ratings within participants, and recoding negatively-framed questionnaire items.

## 2-descriptive_statistics.R

This script calculates various descriptive statistics of the data.

## 3-functions.R

This script defines various functions used in the other scripts. Functions include bootstrapping and permutation testing frameworks, as well as convenience functions.

## 4-feature_creation.R

This script performs various manipulations and analyses of the data to generate stimulus features for use in the other analyses. Depending on your system, this may take a few minutes to run.

## 5-timbre_modelling.R

This script pertains to the analyses reported in the Supplementary Materials. It assesses the relationship between ratings from our experiment and the prior data of Bowling *et al.* (2018).

## 6-modelling_average_ratings.R

This script contains stimulus-level regression analyses of the average rating data. As it contains a large number of models and bootstrapping, it may take a long time to run depending on your system.

## 7-pca.R

This script conducts a Q-type PCA of the rating data of all participants over all stimuli, and assesses the relationships between the resulting components and stimulus-side and participant-side features. As this script contains a large number of models, bootstrapping, and permutation testing, it may take a long time to run depending on your system.

## 8-sem.R

This script contains structural equation modelling analyses used to disentangle latent preference variance from response noise on a per-stimulus basis, and assesses the relationships between variance types and stimulus features. WARNING: This script contains a bootstrapping procedure that works out to running 680,000 structural equation models. It is not recommended to run this portion of the script on a system with limited computational resources.

## Optional extra: create_audio.R

This script contains code for creating the audio stimuli used in the experiment, output in .wav format. The script allows for generation of the chords with highest absolute loadings on the principal components, as well as generation of the full set of chords heard by participants in the experiment. The latter may take a while to run depending on your system.






