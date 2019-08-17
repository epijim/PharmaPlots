## Building the input data

library(tidyverse)

location <- "data-raw/synthea_sample_data_csv_jun2019"

files <- list.files(location)

files

## demographics -------------------------

demographics <- read_csv(paste0(location,"/patients.csv")) %>%
  select(
    PatientID = Id,
    BirthDate = BIRTHDATE,
    Race = RACE,
    Ethnicity = ETHNICITY,
    Gender = GENDER,
    State = STATE
  )

usethis::use_data(demographics, overwrite = TRUE)

## medications -------------------------

medications <- read_csv(paste0(location,"/medications.csv")) %>%
  select(
    PatientID = PATIENT,
    Drug = DESCRIPTION,
    StartDate = START,
    EndDate = STOP,
    Doses = DISPENSES
  )

usethis::use_data(medications, overwrite = TRUE)


## Line of therapies -------------------------

lot <- medications %>%
  mutate(
    # if EndDate unknown, set to 0 days
    EndDate = case_when(
      is.na(EndDate) ~ StartDate,
      TRUE ~ EndDate
    )
  ) %>%
  group_by(PatientID) %>%
  arrange(StartDate) %>%
  mutate(
    # is next in same period
    next_StartDate = lead(StartDate),
    overlap = case_when(
      next_StartDate <= EndDate ~ TRUE,
      TRUE ~ FALSE
    ),
    # count lines
    LineNumber = cumsum(ifelse(overlap != lag(overlap) | is.na(lag(overlap)), 1, 0))
  ) %>%
  ungroup %>%
  group_by(
    PatientID,LineNumber
  ) %>%
  mutate(
    LoT = paste(unique(Drug),collapse = ", ")
  ) %>% slice(1) %>%
  select(
    PatientID,LineNumber,LoT
  )

usethis::use_data(lot, overwrite = TRUE)
