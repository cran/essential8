## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----example-data-------------------------------------------------------------
library(essential8)
library(dplyr)

example_file <- system.file(
  "extdata", "le8-adult-example.csv", package = "essential8"
)

adults <- read.csv(example_file) |>
  as_tibble() |>
  slice_head(n = 2) |>
  select(-starts_with("expected_")) |>
  mutate(id = c("adult_1", "adult_2"))

## ----existing-score-----------------------------------------------------------
adults |>
  mutate(hei2015 = c(45, 70)) |>
  score_le8(diet_method = "hei2015", reference = "nhanes_2015_2018") |>
  select(id, hei2015, diet_score)

## ----existing-percentile------------------------------------------------------
adults |>
  mutate(diet_value = c(35, 98)) |>
  score_le8(diet_method = "dash") |>
  select(id, diet_value, diet_score)

## ----raw-dash-----------------------------------------------------------------
dash_inputs <- tibble(
  dash_fruits = c(1, 2),
  dash_vegetables = c(2, 3),
  dash_nuts_and_legumes = c(0.5, 1),
  dash_whole_grains = c(1, 2),
  dash_low_fat_dairy = c(1, 2),
  dash_sodium = c(3000, 2000),
  dash_red_and_processed_meat = c(1, 0.5),
  dash_sweetened_beverages = c(1, 0),
  dash_energy_kcal = c(2000, 2000)
)

adults |>
  bind_cols(dash_inputs) |>
  score_le8(diet_method = "dash", reference = "nhanes_2015_2018") |>
  select(id, dash_total, diet_score)

## ----raw-hei------------------------------------------------------------------
hei_inputs <- tibble(
  hei2015_energy_kcal = c(2000, 2000),
  hei2015_total_fruits = c(1, 2),
  hei2015_whole_fruits = c(0.5, 1),
  hei2015_total_vegetables = c(2, 3),
  hei2015_greens_and_beans = c(0.2, 0.5),
  hei2015_whole_grains = c(1, 3),
  hei2015_dairy = c(2, 3),
  hei2015_total_protein_foods = c(5, 5),
  hei2015_seafood_and_plant_proteins = c(1, 2),
  hei2015_polyunsaturated_fat = c(15, 20),
  hei2015_monounsaturated_fat = c(20, 25),
  hei2015_saturated_fat = c(25, 15),
  hei2015_refined_grains = c(5, 3),
  hei2015_sodium = c(3000, 2000),
  hei2015_added_sugars = c(12, 8)
)

adults |>
  bind_cols(hei_inputs) |>
  score_le8(diet_method = "hei2015", reference = "nhanes_2015_2018") |>
  select(id, hei2015_total, diet_score)

## ----custom-reference---------------------------------------------------------
custom <- diet_reference(
  p25 = 40, p50 = 55, p75 = 65, p95 = 80,
  method = "hei2015", label = "Example reference"
)

adults |>
  mutate(hei2015 = c(45, 70)) |>
  score_le8(diet_method = "hei2015", reference = custom) |>
  select(id, hei2015, diet_score)

