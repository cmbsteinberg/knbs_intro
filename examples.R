library(tidyverse)


# Example 1

census <- read_csv("data/census.csv") |>
  janitor::clean_names()

sum(is.na(census$p19))

migration <- census |>
  select(county, subcounty_code, p19) |>
  rename(birthplace = p19) |>
  mutate(migrant = if_else(birthplace == county, 0, 1))

mig_nairobi <- migration |>
  group_by(subcounty_code) |>
  summarise(migration_prop = mean(migrant))

# Example 2

working_hours <- census |>
  select(ea_type, p11, p52, p12) |>
  rename(hours_worked_if_work = p52,
         age = p12,
         sex = p11) |>
  filter(age >= 18) |>
  mutate(
    rural = if_else(ea_type == 1, 1, 0),
    female = if_else(sex == 2, 1, 0),
    hours_worked = replace_na(hours_worked_if_work, 0)
  )


working_hours_mean_adult <- working_hours |>
  group_by(rural, female) |>
  summarise(
    hours_worked = mean(hours_worked, na.rm = TRUE),
    hours_worked_if_work = mean(hours_worked_if_work, na.rm = TRUE)
  )

working_hours_mean_adult |>
  ggplot(aes(y = hours_worked,  x = as.factor(female), fill = as.factor(rural))) +
  geom_bar(stat = "identity", position = "dodge")
