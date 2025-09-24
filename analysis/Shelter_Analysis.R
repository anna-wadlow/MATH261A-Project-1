library(tidyverse)
library(lubridate)
library(janitor)
library(scales)
library(stringr)
library(ggthemes)
library(ggpubr)
library(patchwork)
library(here)

# ---- load & combine all years ----

# Point to MATH261A-Project-1/data/ relative to the repo root
data_dir = here("data")

# List all CSVs in that folder
csv_files = list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)

# Read & combine
raw = purrr::map_dfr(
  csv_files,
  ~ suppressMessages(readr::read_csv(.x, guess_max = 100000, show_col_types = FALSE))
)

# Standardize column names to snake_case and make sure expected columns exist
df = raw %>%
  janitor::clean_names()

# ---- parse dates ----
df = df %>%
  mutate(
    dob_dt         = parse_date_time(dob, orders = c("ymd", "mdy", "dmy")),
    intake_dt      = parse_date_time(intake_date, orders = c("ymd", "mdy", "dmy")),
    outcome_dt     = parse_date_time(outcome_date, orders = c("ymd", "mdy", "dmy"))
  )

# ---- filter to dogs adopted ----
dogs_adopt = df %>%
  filter(
    !is.na(animal_type),
    str_trim(str_to_upper(animal_type)) == "DOG",
    !is.na(outcome_type),
    str_trim(str_to_upper(outcome_type)) == "ADOPTION"
  )

# ---- compute age at intake (years) ----
dogs_adopt = dogs_adopt %>%
  mutate(
    age_years_dob = as.numeric(difftime(intake_dt, dob_dt, units="days")) / 365.25,
    age_years     = dplyr::coalesce(age_years_dob)
  )

# ---- compute length of stay (days) ----
dogs_adopt = dogs_adopt %>%
  mutate(
    los_days = as.numeric(difftime(outcome_dt, intake_dt, units="days"))
  )


# ---- basic quality filters ----
dogs_clean = dogs_adopt %>%
  filter(
    !is.na(age_years), !is.na(los_days),   # remove na values
    los_days >= 0,                         # drop impossible negatives
    age_years >= 0,                        # drop impossible negatives
    age_years <= 25                        # sanity cap for dogs
  ) %>%
  # trim extreme LOS outliers at the 99th percentile for a simple first pass
  mutate(
    los_cap = quantile(los_days, 0.99, na.rm = TRUE)
  ) %>%
  filter(los_days <= los_cap) %>%
  select(-los_cap)


# ---- quick EDA ----
summary_stats = dogs_clean %>%
  summarise(
    n = n(),
    median_age_years = median(age_years, na.rm = TRUE),
    mean_age_years = mean(age_years, na.rm = TRUE),
    median_los_days  = median(los_days, na.rm = TRUE),
    p95_los_days     = quantile(los_days, 0.95, na.rm = TRUE)
  )
print(summary_stats)

# ---- simple linear regression: LOS ~ Age ----
m1 = lm(los_days ~ age_years, data = dogs_clean)
print(summary(m1))

# ---- visualization ----
dogs_clean %>%
  ggplot(aes(x = age_years, y = los_days)) +
  geom_point(alpha = 0.3, size = .5) +
  geom_smooth(method = "lm", 
              formula = y ~ x, 
              se = FALSE) +
  stat_regline_equation(label.x = 15, label.y = 50) + 
  scale_x_continuous("Dog age at intake (years)", breaks = pretty_breaks()) +
  scale_y_continuous("Length of stay (days)", breaks = pretty_breaks()) +
  labs(
    title = "Length of Stay vs. Age (of Adopted Dogs)",
    subtitle = "July 2018 to September 2025")+
  theme(plot.title = element_text(size=15))+
  theme_light()+
  theme(plot.margin = unit(c(.5,.5,.5,.5),"cm"))

# ---- simple diagnostics ----

# ------ fitted vs residual plot 
dogs_clean$resid = residuals(m1) # save model residuals to data.frame
dogs_clean$stand_resid = rstandard(m1) # save standardized residuals to data.frame

dogs_clean %>%
  ggplot(data = ., aes(x = age_years, y = resid))+
  geom_point(alpha = .3, size = .5)+
  geom_hline(yintercept = 0, linewidth = .4) + 
  xlab("Dog age at intake (years)") +
  ylab("Residual Value") + 
  ggtitle("Residual vs. Predictor Variable")+
  theme_light()+
  theme(plot.margin = unit(c(0.5,0,0,0.5),"cm"))+
  theme(plot.title = element_text(size=10))


# ------ intake year vs residual plot 
dogs_clean %>%
  ggplot(data = ., aes(x = intake_date, y = resid))+
  geom_point(alpha = .3, size = .5)+
  geom_hline(yintercept = 0, linewidth = .4) + 
  xlab("Intake Date (year)") +
  ylab("Residual Value") + 
  ggtitle("Residual vs. Intake Date")+
  theme_light()+
  theme(plot.margin = unit(c(0.5,0.5,0,0.5),"cm"))+
  theme(plot.title = element_text(size=10))

# ----- qq plot
dogs_clean %>%
  ggplot(dogs_clean, mapping = aes(sample = stand_resid))+
  geom_qq_line()+
  geom_qq()+
  xlab("Theoretical quantile (Normal distribution)") +
  ylab("Sample quantile") + 
  ggtitle("Quantile-quantile plot of residuals")+
  theme_light() +
  theme(plot.margin = unit(c(1,1,.5,.5),"cm"))



