library(testthat)

source(here::here("R/cohort-model.R"))
source(here::here("R/parameters.R"))

test_that("update_probabilities() works", {
  expect_equal(update_probabilities(probs, transitions),
               c(0, 0, 0.4, 0.5, 0.05, 0, 0, 0.05))
  expect_equal(update_probabilities(c(0, 0, 0.4, 0.5, 0.05, 0, 0, 0.05), transitions),
               c(0, 0, 0.12, 0.6, 0.125, 0.005, 0, 0.15))
})

test_that("run_cohort_model() works", {
  expect_equal(run_cohort_model(probs, transitions, 2),
               rbind(c(0, 1, 0, 0, 0, 0, 0, 0),
                     c(0, 0, 0.4, 0.5, 0.05, 0, 0, 0.05),
                     c(0, 0, 0.12, 0.6, 0.125, 0.005, 0, 0.15)))
})

test_that("calculate_outcome_value() works", {
  expect_equal(calculate_outcome_value(probs, costs), 7000)
})

test_that("calculate_outcomes() works", {
  cohort <- run_cohort_model(probs, transitions, 2)
  expect_equal(calculate_outcomes(cohort, costs), 8310)
  expect_equal(calculate_outcomes(cohort, costs, 0.05), 8215.05)
})
