
library(tidyverse)

# start
census <- read_csv("../intro_R-main/data/census.csv") |>
  janitor::clean_names()

census_small <- census |>
  select(county, subcounty_code, ea_type, p11, p12, p17, p19, p52) |>
  rename(
    religion = p17,
    age = p12,
    sex = p11,
    hours_worked_if_work = p52
  )

census_small <- census_small |>
  mutate(religion = case_when(religion == 99 ~ NA, TRUE ~ religion))

religion_by_county <- census_small |>
  group_by(county, religion) |>
  summarise(total = n()) |>
  mutate(share = total/sum(total))

# migration

migration_prop <- census_small |>
  mutate(migrant = if_else(birthplace == county, 0, 1)) |>
  summarise(migration_prop = mean(migrant))

# working_hours

census_small <- census_small |>
  mutate(
    rural = if_else(ea_type == 1, 1, 0),
    female = if_else(sex == 2, 1, 0),
    hours_worked = if_else(is.na(hours_worked_if_work), 0, hours_worked_if_work)
  )

working_hours_mean_adult <- census_small |>
  filter(age >= 18) |>
  group_by(rural, female) |>
  summarise(
    hours_worked = mean(hours_worked, na.rm = TRUE),
    hours_worked_if_work = mean(hours_worked_if_work, na.rm = TRUE)
  )
