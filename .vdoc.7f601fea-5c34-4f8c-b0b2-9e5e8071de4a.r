#
#
#
#
#
#
# Data building

# Set seed
set.seed(13)

# grid (use your existing design_grid if present)
Age <- c("Young", "Middle-aged", "Old")
Gender <- c("Female", "Male", "Non-binary")
design_grid <- expand.grid(age = Age, gender = Gender, stringsAsFactors = FALSE)

# parameters
n_per <- 50
base_mean <- 50
sd_score <- 5
age_eff <- c(Young = -2, `Middle-aged` = 0, Old = 3)
gender_eff <- c(Female = 0, Male = 1, `Non-binary` = -1)

# simulate
sim <- design_grid |>
  tidyr::uncount(n_per) |>
  mutate(
    id = row_number(),
    score_mean = base_mean + age_eff[age] + gender_eff[gender],
    score = rnorm(n(), mean = score_mean, sd = sd_score),
    event_prob = plogis(-1 + 0.2 * (score - base_mean) / sd_score + 0.3 * (gender == "Male")),
    event = rbinom(n(), 1, event_prob)
  ) |>
  select(id, age, gender, score, event, event_prob)

sim
#
#
#
