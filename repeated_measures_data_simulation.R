# ==============================================================================
# Project: Simulating Repeated Measures Data with Gender-Based Variability
# Script:  simulate_repeated_measures.R
# Author:  Simon Greaves
# Date:    17-AUG-2024
# 
# Description:
# This script generates a simulated dataset of repeated measurements taken
# from individual subjects over time, incorporating gender-based differences 
# in baseline values and slopes. The simulation includes time-varying effects,
# probabilistic no-shows, and random intervals between measurement appointments.
# This dataset can be used to validate statistical models and explore data
# analysis techniques.
#
# Dependencies:
# - R version 4.1.3
# - Packages: dplyr, ggplot2
#
# Inputs:
# - None
#
# Outputs:
# - A data set containing the repeated measurements by id in a LONG format.
# - A ggplot visualizing the relationship between time and measurement values
#   stratified by gender.
#
# Notes:
# - Gender is modeled as a binary variable with equal probability.
# 
# ==============================================================================

pkgs <- c('dplyr', 'ggplot2')
inst <- lapply(pkgs, library, character.only = TRUE)

set.seed(42)

# number of individual id instances
n_instances <- 10000

# number of measurements
lambda <- 0.75
# model gender
prob_male <- 0.5
# set up measurement baseline and slope varying by gender
baseline <- 100
male_baseline_offset <- 10
slope <- -0.25
male_slope_offset <- -0.1
# simulate a 10% change of no-shows
measurement_prob <- 0.9
# maximum of 7 days between measurement appointments
max_days <- 7

# set up individual profiles
wide <- data.frame(id = 1:n_instances,
                   total_measurements = ceiling(rexp(n_instances, lambda)),
                   gender = rbinom(n_instances,
                                   size = 1,
                                   prob = prob_male))
head(wide, 7)

# create measurements data set with as many rows as measurements
long <- wide[rep(seq_len(nrow(wide)), wide$total_measurements), ] %>%
  mutate(
    # Days between measurements
    days = runif(n = n(), min = 0, max = max_days)) %>%
  group_by(id) %>%
  mutate(
    # Gender
    sex = c('F', 'M')[gender + 1],
    # Day of measurement
    day = cumsum(days),
    # Number of appointment
    n_appointment = row_number(),
    # Was measurement successful or is it NA?
    successful_measurement = rbinom(n = n(), size = 1, prob = measurement_prob),
    # Successful measurements up to this point
    n_measurements = cumsum(successful_measurement),
    # Total successful measurements
    n_succesfull_measurements = sum(successful_measurement),
    # Measurement value
    value = if_else(successful_measurement == 1,
                 baseline + gender * male_baseline_offset +
                   slope * day + gender * male_slope_offset * day +
                   rnorm(n = n(), mean = 0, sd = 1),
                 NA)
  )
head(data.frame(long), 8)

# plot the data
long %>%
  subset(successful_measurement == TRUE) %>%
  ggplot(aes(x = day, y = value, color = sex)) +
  geom_point(alpha = 0.2) +  # Add points with some transparency
  geom_smooth(method = "lm", se = FALSE) +  # Add regression lines
  labs(
    title = "Regression of values by gender",
    x = "Days",
    y = "Value",
    color = "Gender"
  ) +
  theme(plot.title = element_text(hjust = 0.5))
