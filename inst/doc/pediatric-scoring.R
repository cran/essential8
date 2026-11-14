## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
has_references <- requireNamespace("cdcanthro", quietly = TRUE) &&
  requireNamespace("pedbp", quietly = TRUE)

## ----packages, message = FALSE------------------------------------------------
library(essential8)
library(dplyr)

## ----references, eval = has_references----------------------------------------
measurements <- tibble(
  id = c("toddler", "school_age", "teen"),
  age = c(1.5, 8.25, 15.5),
  sex = c("male", "female", "male"),
  height_cm = c(82, 128, 172),
  weight_kg = c(11.5, 27, 64)
)

measurements |>
  add_pediatric_references() |>
  select(id, bmi_percentile, bmi_percent_of_p95, infant_weight_percentile,
         systolic_bp_p90, systolic_bp_p95)

## ----z-change, eval = FALSE---------------------------------------------------
# visits |>
#   add_pediatric_references() |>
#   arrange(child_id, age) |>
#   mutate(
#     infant_weight_z_change = infant_weight_z - lag(infant_weight_z),
#     .by = child_id
#   )

## ----preschool----------------------------------------------------------------
preschooler <- tibble(
  id = "preschooler",
  age = 4,
  # Servings per day
  green_leafy_vegetables = 1,
  other_vegetables = 1,
  whole_grains = 2,
  olive_oil = 1,
  # Servings per week
  berries = 7,
  other_fruit = 14,
  sweets_and_pastries = 2,
  meat = 2,
  fish = 1,
  nuts = 2,
  butter_cream = 1,
  # Times per week
  fast_food = 0.25,
  active_play_minutes = 160,
  smoking_status = "never",
  current_inhaled_nds = FALSE,
  secondhand_smoke_home = FALSE,
  sleep_hours = 13.5,
  bmi_percentile = 60,
  bmi_percent_of_p95 = 82,
  systolic_bp = 108,
  diastolic_bp = 60,
  systolic_bp_p90 = 103,
  systolic_bp_p95 = 107,
  diastolic_bp_p90 = 63,
  diastolic_bp_p95 = 67,
  antihypertensive_treatment = FALSE
)

preschooler |>
  score_le8(min_components = 5) |>
  select(mepa_total, ends_with("_score"), n_components, le8_category)

## ----infant-------------------------------------------------------------------
infant <- tibble(
  id = "infant",
  age = 0.75,
  age_months = 9,
  green_leafy_vegetables = 0.5,
  other_vegetables = 0.5,
  berries = 3,
  other_fruit = 4,
  whole_grains = 1,
  sweets_and_pastries = 0,
  fast_food = 0,
  milk_source = "human",
  smoking_status = "never",
  current_inhaled_nds = FALSE,
  secondhand_smoke_home = TRUE,
  sleep_hours = 13,
  infant_weight_percentile = 98.5,
  infant_weight_z_change = 0.4
)

infant |>
  score_le8(min_components = 4) |>
  select(mepa_total, ends_with("_score"), n_components, le8_category)

