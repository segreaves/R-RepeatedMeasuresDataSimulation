# =============================================================================
# Project: Repeated Measures Data Simulation
# Script:  repeated_measures_data_simulation.R
# Author:  Simon Greaves
# Date:    17AUG2024
# 
# Description:
# This script will create a data.frame object containing time-varying repeated
# measurements within subjects. This is an example data set meant to be
# used as an example of how data simulation can be used for model validation
# and more.
#
# Dependencies:
# - R version 4.1.3
# - Packages: dplyr
#
# Inputs:
# - None
#
# Outputs:
# - 
#
# Notes:
# [Any additional notes or assumptions]
#
# =============================================================================

pkgs <- c('dplyr', 'ggplot2')
inst <- lapply(pkgs, library, character.only = TRUE)

set.seed(42)

# number of individual id instances
n_instances <- 10000

# measurement frequency
lambda <- 0.75
measurements <- ceiling(rexp(n_instances, lambda))

# model gender
prob_male <- 0.5

# set up individual profiles
wide <- data.frame(id = 1:n_instances,# unique id
                          total_measurements = measurements,
                          gender = rbinom(n_instances,
                                          size = 1,
                                          prob = prob_male))

# set up measurement baseline and slope varying by gender
baseline <- 100
male_baseline_offset <- 10
slope <- -0.25
male_slope_offset <- -0.1
# we'll also simulate a 10% probability of no-shows
no_show_prob <- 0.9
# we'll simulate a maximum of 7 days between measurement appointments
max_days <- 7
# create measurements data set with as many rows as measurements
long <- wide[rep(seq_len(nrow(wide)), wide$total_measurements), ]
long <- long %>%
  mutate(
    successful_measurement = rbinom(n = n(), size = 1, prob = no_show_prob),
    days = runif(n = n(), min = 0, max = max_days)) %>%
  group_by(id) %>%
  mutate(
    sex = c('F', 'M')[gender + 1],
    day = cumsum(days),
    n_appointment = row_number(),
    n_measurements = cumsum(successful_measurement),
    tot_donations = sum(successful_measurement),
    value = if_else(successful_measurement == 1,
                 baseline + gender * male_baseline_offset +
                   slope * day + gender * male_slope_offset * day +
                   rnorm(n = n(), mean = 0, sd = 1),
                 NA)
  )

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
  theme_minimal()
