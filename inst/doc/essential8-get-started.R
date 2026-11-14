## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----create-data--------------------------------------------------------------
library(essential8)
library(dplyr)

adult_data <- tibble(
  id = c("patient_1", "patient_2"),
  age = c(42, 61),
  sex = c("female", "male"),
  # Daily servings
  olive_oil = c(2, 1),
  green_leafy_vegetables = c(1, 0.5),
  other_vegetables = c(2, 1),
  whole_grains = c(2, 1),
  # Weekly servings
  berries = c(3, 1),
  other_fruit = c(5, 2),
  meat = c(2, 5),
  fish = c(3, 1),
  chicken = c(2, 4),
  cheese = c(1, 4),
  butter_cream = c(1, 5),
  beans = c(3, 1),
  sweets_and_pastries = c(1, 5),
  nuts = c(4, 1),
  fast_food = c(0, 2),
  alcohol = c(4, 0),
  moderate_activity_minutes = c(100, 60),
  vigorous_activity_minutes = c(25, 0),
  smoking_status = c("never", "former"),
  years_since_quit = c(0, 6),
  current_inhaled_nds = c(FALSE, FALSE),
  secondhand_smoke_home = c(FALSE, FALSE),
  sleep_hours = c(7.5, 6.5),
  bmi = c(24.2, 24.0),
  bmi_profile = c("general", "asian_pacific"),
  non_hdl_cholesterol = c(125, 145),
  lipid_lowering_treatment = c(FALSE, TRUE),
  diabetes = c(FALSE, FALSE),
  glucose_measure = c("fasting_glucose", "hba1c"),
  glucose_value = c(95, 6.0),
  systolic_bp = c(118, 132),
  diastolic_bp = c(76, 84),
  antihypertensive_treatment = c(FALSE, TRUE)
)

## ----score-data---------------------------------------------------------------
scored <- adult_data |>
  score_le8(diet_method = "mepa")

scored |>
  select(id, mepa_total, diet_score, le8_composite_score, le8_category)

## ----component-scores---------------------------------------------------------
scored |>
  select(id, ends_with("_score"), -le8_composite_score)

## ----percentile---------------------------------------------------------------
adult_data |>
  mutate(diet_value = c(35, 95)) |>
  score_le8(diet_method = "percentile") |>
  select(id, diet_value, diet_score, le8_composite_score)

## ----incomplete-data----------------------------------------------------------
adult_data |>
  mutate(sleep_hours = c(NA_real_, 6.5)) |>
  score_le8(min_components = 7) |>
  select(id, le8_composite_score, n_components, le8_complete)

## ----summary------------------------------------------------------------------
summary(scored)

## ----summarise----------------------------------------------------------------
summarise(scored)

## ----tidy---------------------------------------------------------------------
tidy(scored, id_cols = "id")

## ----plot, eval = requireNamespace("ggplot2", quietly = TRUE), fig.alt = "Bar chart of the mean LE8 composite and component scores, with dashed lines at the moderate (50) and high (80) thresholds."----
plot(scored)

