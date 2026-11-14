## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 4.5,
  out.width = "100%"
)
has_ggplot2 <- requireNamespace("ggplot2", quietly = TRUE)
has_forest <- has_ggplot2 && requireNamespace("ggforestplotR", quietly = TRUE)
has_mice <- requireNamespace("mice", quietly = TRUE)

## ----packages, message = FALSE------------------------------------------------
library(essential8)
library(dplyr)

## ----references, eval = FALSE-------------------------------------------------
# survey <- add_pediatric_references(
#   survey,
#   sex = "sex",
#   height = "height_cm",
#   weight = "weight_kg"
# )

## ----simulate-----------------------------------------------------------------
set.seed(2026)
n <- 500

age <- c(
  sample(2:5, 50, replace = TRUE),
  sample(6:12, 60, replace = TRUE),
  sample(13:19, 60, replace = TRUE),
  sample(20:79, n - 170, replace = TRUE)
)
child <- age < 20
preschool <- age < 6
under_13 <- age < 13
sex <- sample(c("female", "male"), n, replace = TRUE)
clamp <- function(x, lower = -Inf, upper = Inf) pmin(pmax(x, lower), upper)
diabetes <- !child & runif(n) < plogis(-4.2 + 0.05 * age)
smoking_status <- case_when(
  age < 13 ~ "never",
  child ~ sample(c("never", "former", "current"), n, TRUE, c(0.8, 0.15, 0.05)),
  .default = sample(c("never", "former", "current"), n, TRUE, c(0.58, 0.28, 0.14))
)
bmi_percentile <- if_else(child, round(clamp(rbeta(n, 2.2, 1.2) * 100, 5, 99.9), 1), NA_real_)
adult_bmi <- if_else(child, NA_real_, round(clamp(exp(rnorm(n, log(28), 0.17)), 18.5), 1))
# Heavier participants are more likely to have blood drawn, and their
# cholesterol and glucose run higher: the labs are missing at random given BMI.
weight_z <- if_else(child, (bmi_percentile - 50) / 29, (adult_bmi - 28) / 5)
blood_drawn <- runif(n) < if_else(
  child,
  plogis(-0.4 + 1.2 * weight_z),
  plogis(1.6 + 0.03 * (age - 50) + 0.9 * weight_z)
)

survey <- tibble(
  id = sprintf("S%03d", seq_len(n)),
  age = age,
  sex = sex,
  # MEPA: servings per day
  olive_oil = rpois(n, 0.8),
  green_leafy_vegetables = rpois(n, if_else(child, 0.6, 0.8)),
  other_vegetables = rpois(n, 1.2),
  whole_grains = rpois(n, 1.6),
  # MEPA: servings per week (sweets include sugary drinks under age 6)
  berries = rpois(n, 2),
  other_fruit = rpois(n, if_else(child, 7, 5)),
  meat = rpois(n, 4),
  fish = rpois(n, 1.2),
  chicken = rpois(n, 2.5),
  cheese = rpois(n, 3),
  butter_cream = rpois(n, 3),
  beans = rpois(n, 1.5),
  sweets_and_pastries = rpois(n, if_else(child, 6, 4)),
  nuts = rpois(n, 2),
  alcohol = if_else(age < 18, 0L, rpois(n, 3)),
  fast_food = rpois(n, 1.2),
  # Physical activity
  active_play_minutes = if_else(preschool, 
                                round(rgamma(n, 6, scale = 25)),
                                NA_real_),
  moderate_activity_minutes = 
    if_else(preschool, 
            NA_real_, 
            round(
              rgamma(n, 1.3, scale = if_else(child, 140, 85)))
            ),
  vigorous_activity_minutes = 
    if_else(preschool, 
            NA_real_, 
            round(rbinom(n, 1, 0.5) * rgamma(n, 1.2, scale = if_else(child, 80,
                                                                     45)))
            ),
  # Nicotine and sleep
  smoking_status = smoking_status,
  years_since_quit = if_else(!child & smoking_status == "former", 
                             round(runif(n, 0, 25), 1), NA_real_),
  
  current_inhaled_nds = smoking_status != "current" & 
    age >= 13 & runif(n) < if_else(child, 0.12, 0.05),
  
  secondhand_smoke_home = runif(n) < 0.12,
  
  sleep_hours = round(2 * rnorm(n, 
                                case_when(preschool ~ 11, 
                                          under_13 ~ 9.5, 
                                          child ~ 7.6, .default = 7), 1)) / 2,
  
  # BMI: adults in kg/m^2, children as CDC percentiles
  bmi = adult_bmi,
  bmi_profile = if_else(child, 
                        NA_character_, 
                        if_else(runif(n) < 0.1, "asian_pacific", "general")),
  bmi_percentile = bmi_percentile,
  bmi_percent_of_p95 = if_else(
    bmi_percentile >= 95,
    round(100 + rexp(n, 1 / 12), 1),
    round(clamp(100 - (95 - bmi_percentile) * 0.45, 60, 99.9), 1)
  ),
  
  # Laboratory values (masked below where no blood was drawn)
  non_hdl_cholesterol = 
    round(
      clamp(
        rnorm(n,
              if_else(child, 
                      112 + 14 * weight_z, 
                      138 + 0.3 * (age - 50) + 12 * weight_z),
              if_else(child, 22, 32)), 50)),
  
  lipid_lowering_treatment = !child & runif(n) < plogis(-3.5 + 0.05 * age),
  diabetes = diabetes,
  glucose_measure = if_else(diabetes, "hba1c", "fasting_glucose"),
  
  glucose_value = case_when(
    diabetes ~ round(clamp(rnorm(n, 7.4, 1.1), 6.5), 1),
    .default = round(
      clamp(rnorm(n, if_else(child, 88, 95) + 5 * weight_z, 10), 65, 125)
      )
  ),
  
  # Blood pressure, with AAP 2017 thresholds for ages 1-12
  systolic_bp = 
    round(
      rnorm(n,
            if_else(child, 96 + 1.4 * age, 121 + 0.35 * (age - 50)),
            if_else(child, 9, 14))),
  
  diastolic_bp = round(
    rnorm(n, if_else(child, 56 + 0.9 * age, 77), if_else(child, 7, 9))),
  
  systolic_bp_p90 = if_else(under_13, round(100 + 1.3 * age), NA_real_),
  systolic_bp_p95 = if_else(under_13, round(104 + 1.3 * age), NA_real_),
  diastolic_bp_p90 = if_else(under_13, round(62 + 0.9 * age), NA_real_),
  diastolic_bp_p95 = if_else(under_13, round(66 + 0.9 * age), NA_real_),
  
  antihypertensive_treatment = !child & runif(n) < plogis(-3.8 + 0.055 * age)
)

# Keep the full values to check the imputation against later.
survey_complete <- survey
survey <- survey |>
  mutate(
    non_hdl_cholesterol = if_else(blood_drawn, non_hdl_cholesterol, NA_real_),
    glucose_value = if_else(blood_drawn, glucose_value, NA_real_),
    glucose_measure = if_else(
      blood_drawn | diabetes, glucose_measure, NA_character_
      )
  )

## ----survey-size--------------------------------------------------------------
survey |>
  mutate(child = age < 20) |>
  count(child)

## ----threshold-check----------------------------------------------------------
default_threshold <- score_le8(survey)

default_threshold |>
  summarise(
    participants = n(),
    with_composite = sum(!is.na(le8_composite_score)),
    .by = le8_population
  )

## ----score--------------------------------------------------------------------
scores <- score_le8(survey, diet_method = "mepa", min_components = 6)

## ----scored-columns-----------------------------------------------------------
scores |>
  select(id, age, le8_population, ends_with("_score"), 
         n_components, le8_category) |>
  head(5)

## ----life-stage---------------------------------------------------------------
scores <- scores |>
  mutate(
    life_stage = cut(
      age,
      breaks = c(2, 6, 13, 20, 40, 60, 80),
      labels = c("2-5", "6-12", "13-19", "20-39", "40-59", "60-79"),
      right = FALSE
    )
  )

scores |> 
  select(id, life_stage) |> 
  head(5)

## ----summary------------------------------------------------------------------
summary(scores)

## ----summarise-life-stage-----------------------------------------------------
by_stage <- summarise(scores, .by = life_stage)

by_stage |>
  filter(measure == "composite") |>
  arrange(life_stage) |>
  select(life_stage, n, mean, sd, median, q25, q75)

## ----categories---------------------------------------------------------------
scores |>
  filter(!is.na(le8_category)) |>
  summarise(
    n = n(),
    low = mean(le8_category == "low") * 100,
    moderate = mean(le8_category == "moderate") * 100,
    high = mean(le8_category == "high") * 100,
    .by = life_stage
  ) |>
  arrange(life_stage)

## ----plot-components, eval = has_ggplot2, fig.alt = "Bar chart of mean LE8 composite and component scores for all participants, with dashed lines at the moderate and high thresholds."----
plot(scores)

## ----plot-distribution, eval = has_ggplot2, fig.alt = "Box plots of the LE8 composite and component scores for all participants."----
plot(scores, type = "distribution")

## ----plot-composite, eval = has_ggplot2, fig.height = 4, fig.alt = "Histogram of LE8 composite scores, shaded by low, moderate, and high category."----
plot(scores, type = "composite")

## ----plot-forest, eval = has_forest, fig.height = 7, fig.alt = "Forest plot of mean LE8 scores with 95% confidence intervals for each life stage."----
plot(
  scores,
  type = "forest",
  by = "life_stage",
  striped_rows = TRUE,
  dodge_width = 0.75
)

## ----weakest------------------------------------------------------------------
long <- tidy(scores, id_cols = c("id", "life_stage"), composite = FALSE)
long

stage_sizes <- count(scores, life_stage, name = "stage_n")

weakest <- long |>
  filter_out(is.na(score)) |>
  slice_min(score, n = 1, by = id) |>
  count(life_stage, measure, name = "participants") |>
  slice_max(participants, n = 1, by = life_stage) |>
  left_join(stage_sizes, by = join_by(life_stage)) |>
  mutate(percent = round(100 * participants / stage_n, 1)) |>
  select(life_stage, measure, participants, percent) |>
  arrange(life_stage)

weakest

## ----imputation-setup, eval = has_mice----------------------------------------
le8_imputation_setup(survey)

## ----score-imputed, eval = has_mice-------------------------------------------
imputed_scores <- survey |>
  score_le8(impute = le8_imputation_control(m = 10, maxit = 10, seed = 2026)) |>
  mutate(
    life_stage = cut(
      age,
      breaks = c(2, 6, 13, 20, 40, 60, 80),
      labels = c("2-5", "6-12", "13-19", "20-39", "40-59", "60-79"),
      right = FALSE
    )
  )

summary(imputed_scores)

## ----compare-approaches, eval = has_mice--------------------------------------
true_scores <- score_le8(survey_complete)

compare <- function(scored, approach) {
  scored |>
    summarise(.by = le8_population) |>
    filter(measure %in% c("composite", "blood_lipids")) |>
    transmute(approach, le8_population, measure, n, mean = round(mean, 1))
}

bind_rows(
  compare(filter(scores, n_components == 8), "Complete cases"),
  compare(scores, "Available components"),
  compare(imputed_scores, "Multiple imputation"),
  compare(true_scores, "True values")
) |>
  arrange(le8_population, measure)

## ----plot-imputed-forest, eval = has_mice && has_forest, fig.height = 7, fig.alt = "Forest plot of pooled mean LE8 scores with 95% confidence intervals from multiple imputation, by life stage."----
plot(imputed_scores, type = "forest", by = "life_stage", striped_rows = TRUE)

