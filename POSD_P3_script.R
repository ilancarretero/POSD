# Implementation of a classifier of 3 different classes

# Install libraries
if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load(tidyverse)

# STEP 1: Generation of the training and testing samples
m1 = -1.5
m2 = 0
m3 = 1.5
train = 10
test = 100

set.seed(100)
# Train
xi1 = rnorm(train, mean = m1, sd = 1)
xi2 = rnorm(train, mean = m2, sd = 1)
xi3 = rnorm(train, mean = m3, sd = 1)

# Test
xj1 = rnorm(test, mean = m1, sd = 1)
xj2 = rnorm(test, mean = m2, sd = 1)
xj3 = rnorm(test, mean = m3, sd = 1)

# STEP 2: Estimate the sample means for every class for the train set
m1_estimate = mean(xi1)
m2_estimate = mean(xi2)
m3_estimate = mean(xi3)

# STEP 3: For test sample, measure distance to estimated means and select
# the class with the closest distance
predicted_class_xj1 = rep(NA, test)
for (i in 1:test){
  d_m1_estimate = (xj1[i] - m1_estimate)^2
  d_m2_estimate = (xj1[i] - m2_estimate)^2
  d_m3_estimate = (xj1[i] - m3_estimate)^2
  d_vector = c(d_m1_estimate, d_m2_estimate, d_m3_estimate)
  predicted_class_xj1[i] = which.min(d_vector)
}

predicted_class_xj2 = rep(NA, test)
for (i in 1:test){
  d_m1_estimate = (xj2[i] - m1_estimate)^2
  d_m2_estimate = (xj2[i] - m2_estimate)^2
  d_m3_estimate = (xj2[i] - m3_estimate)^2
  d_vector = c(d_m1_estimate, d_m2_estimate, d_m3_estimate)
  predicted_class_xj2[i] = which.min(d_vector)
}

predicted_class_xj3 = rep(NA, test)
for (i in 1:test){
  d_m1_estimate = (xj3[i] - m1_estimate)^2
  d_m2_estimate = (xj3[i] - m2_estimate)^2
  d_m3_estimate = (xj3[i] - m3_estimate)^2
  d_vector = c(d_m1_estimate, d_m2_estimate, d_m3_estimate)
  predicted_class_xj3[i] = which.min(d_vector)
}

# Compute metrics
class_xj1 = rep(1, test)
class_xj2 = rep(2, test)
class_xj3 = rep(3, test)

conf_mat_11 = sum(class_xj1 == predicted_class_xj1) / test
conf_mat_12 = sum(predicted_class_xj1 == 2) / test
conf_mat_13 = sum(predicted_class_xj1 == 3) / test

conf_mat_21 = sum(predicted_class_xj2 == 1) / test
conf_mat_22 = sum(class_xj2 == predicted_class_xj2) / test
conf_mat_23 = sum(predicted_class_xj2 == 3) / test

conf_mat_31 = sum(predicted_class_xj3 == 1) / test
conf_mat_32 = sum(predicted_class_xj3 == 2) / test
conf_mat_33 = sum(class_xj3 == predicted_class_xj3) / test

prior_prob = 1/3
prob_error = (1-conf_mat_11) * prior_prob + (1-conf_mat_22) * prior_prob + (1 - conf_mat_33) * prior_prob

