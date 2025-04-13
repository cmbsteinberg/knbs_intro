
library(tidyverse)


# start
census <- read_csv("../intro_R-main/data/census.csv") |>
  janitor::clean_names()

census_small <- census |>
  select(county, subcounty_code, ea_type, p11, p12, p17, p19, p52)

# religion
religion <- census |>
  select(p17) |>
  rename(religion = p17)

religion_without_dk <- religion |>
  filter(religion != 99)

religion_by_county <- religion_without_dk |>
  group_by(county, religion) |>
  summarise(total = n())

# migration
migration <- census_small |>
  select(county, subcounty_code, p19) |>
  rename(birthplace = p19)

migration_col <- migration |>
  mutate(migrant = if_else(birthplace == county, 0, 1))

migration_prop <- migration_col |>
  summarise(migration_prop = mean(migrant))

# working_hours

working_hours <- census_small |>
  select(ea_type, p11, p52, p12) |>
  rename(age = p12,
         sex = p11,
         hours_worked_if_work = p52)

working_hours_18 <- working_hours |>
  filter(age >= 18)

working_hours_gender_rural <- working_hours_18 |>
  mutate(rural = if_else(ea_type == 1, 1, 0),
         female = if_else(sex == 2, 1, 0))

working_hours_no_nas <-  working_hours_gender_rural |>
  mutate(hours_worked = if_else(is.na(hours_worked_if_work), 0, hours_worked_if_work))


working_hours_mean_adult <- working_hours_no_nas |>
  group_by(rural, female) |>
  summarise(
    hours_worked = mean(hours_worked, na.rm = TRUE),
    hours_worked_if_work = mean(hours_worked_if_work, na.rm = TRUE)
  )


working_hours_mean_adult |>
  ggplot(aes(y = hours_worked,  x = as.factor(female), fill = as.factor(rural))) +
  geom_bar(stat = "identity", position = "dodge")
