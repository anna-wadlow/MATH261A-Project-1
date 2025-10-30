library(tidyverse)
library(lubridate)
library(janitor)
library(scales)
library(stringr)
library(ggthemes)
library(ggpubr)
library(patchwork)
library(here)
library(tidyr)
library(broom)

# ---- load & combine all years ----

# Point to MATH261A-Project-1/data/ relative to the repo root
data_dir <- here("data")

# List all CSVs in that folder
csv_files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)

# Read & combine
raw <- purrr::map_dfr(
  csv_files,
  ~ suppressMessages(readr::read_csv(.x, guess_max = 100000, show_col_types = FALSE))
)

# Standardize column names to snake_case and make sure expected columns exist
df_raw <- raw %>%
  janitor::clean_names()

# ---- parse dates and filter data ----
df <- df_raw %>%
  mutate(
    dob_dt         = parse_date_time(dob, orders = c("ymd", "mdy", "dmy")),
    intake_dt      = parse_date_time(intake_date, orders = c("ymd", "mdy", "dmy")),
    outcome_dt     = parse_date_time(outcome_date, orders = c("ymd", "mdy", "dmy")),
    los_days       = as.numeric(difftime(outcome_dt, intake_dt, units="days")),
    age_years_dob  = as.numeric(difftime(intake_dt, dob_dt, units="days")) / 365.25,
    age_years      = dplyr::coalesce(age_years_dob)
  )
    
df_clean <- df %>%
  filter(
    !is.na(age_years), !is.na(los_days),   # remove na values
    los_days >= 0,                         # drop impossible negatives
    age_years >= 0,                        # drop impossible negatives
    age_years <= 25
  )

dogs_adopt <- df_clean %>%
  filter(
    !is.na(animal_type),
    str_trim(str_to_upper(animal_type)) == "DOG",
    !is.na(outcome_type),
    str_trim(str_to_upper(outcome_type)) == "ADOPTION"
  )

# ---- produce summary tables of original and removed data by outcome type ----
dat_rem <- df %>%
  dplyr::filter(animal_type == "DOG") %>%
  dplyr::filter(is.na(los_days) | is.na(age_years) | age_years < 0 | age_years >= 25)

dat_rem %>%
  count(outcome_type, sort = TRUE)

dat_rem_counts <- dat_rem %>%
  group_by(outcome_type) %>%
  count()

df_dogs_clean_counts <- df_clean %>% 
  dplyr::filter(animal_type == "DOG") %>% 
  group_by(outcome_type) %>% 
  count()

df_dogs_counts <- df %>% 
  dplyr::filter(animal_type == "DOG") %>% 
  group_by(outcome_type) %>% 
  count()

counts_join <- df_dogs_counts %>%
  full_join(df_dogs_clean_counts, by = "outcome_type") %>%
  full_join(dat_rem_counts, by = "outcome_type")

counts_join <- counts_join %>%
  rename(original = n.x,
         clean = n.y,
         removed = n)

counts_join <- counts_join %>%
  arrange(desc(clean))

# ---- simple linear regression: LOS ~ Age ----
m1 = lm(los_days ~ age_years, data = dogs_adopt)
print(summary(m1))
m1_summary <- summary(m1)

dogs_adopt$resid = residuals(m1) # save model residuals to data.frame
dogs_adopt$stand_resid = rstandard(m1) # save standardized residuals to data.frame








