#### Preamble ####
# Purpose: Load and clean the survey data for model making purposes
# Author: Name, Name, Name
# Data: 2 November 2020
# Contact: xudanny1@gmail.com, OTHER EMAILS
# License: MIT
# Pre-requisites: 
# - ULCA Democracy Fund Voter Study Group Data
# - Select Wave 50 data (ns20200625.dta)
# - Place it in the root directory !!!!! Might change this later
# - Make sure that the directory and file path are correct for your computer


#### Workspace setup ####
library(haven)
library(tidyverse)

# Read in the raw data
# MAKE SURE THESE ARE BOTH CORRECT PATHS
setwd("X:/R studio proj/STA304-PS3")
raw_data <- read_dta("ns20200625.dta")


# Add the labels
raw_data <- labelled::to_factor(raw_data)

reduced_data <- raw_data

# Tidy Race_Ethnicity
reduced_data <- reduced_data %>%
  mutate(race_ethnicity = case_when(race_ethnicity == "White" ~ "White",
                                    race_ethnicity == "Black, or African American" ~ "Black, or African American",
                                    race_ethnicity == "American Indian or Alaska Native" ~ "American Indian or Alaska Native",
                                    race_ethnicity == "Asian (Chinese)" ~ "Asian (Chinese)",
                                    race_ethnicity == "Asian (Japanese)" ~ "Asian (Japanese)",
                                    race_ethnicity == "Asian (Asian Indian)" ~ "Other Asian or Pacific Islander",
                                    race_ethnicity == "Asian (Korean)" ~ "Other Asian or Pacific Islander",
                                    race_ethnicity == "Asian (Vietnamese)" ~ "Other Asian or Pacific Islander",
                                    race_ethnicity == "Asian (Other)" ~ "Other Asian or Pacific Islander",
                                    race_ethnicity == "Pacific Islander (Native Hawaiian)" ~ "Other Asian or Pacific Islander",
                                    race_ethnicity == "Pacific Islander (Guamanian)" ~ "Other Asian or Pacific Islander",
                                    race_ethnicity == "Pacific Islander (Samoan)" ~ "Other Asian or Pacific Islander",
                                    race_ethnicity == "Pacific Islander (Other)" ~ "Other Asian or Pacific Islander",
                                    race_ethnicity == "Some other race" ~ "Some other race",
                                    TRUE ~ "Respondent Skipped"))

# Filter out the vocational and graduate but no degree observations
reduced_data <- reduced_data %>%
  filter(education != "Other post high school vocational training") %>%
  filter(education != "Completed some graduate, but no degree")

# Turn age into a categorical variable
reduced_data <- reduced_data %>%
  mutate(age_bin = case_when(age <= 17 ~ "Not eligible",
                             age <= 24 ~ "18 to 24",
                             age <= 34 ~ "25 to 34",
                             age <= 44 ~ "35 to 44",
                             age <= 54 ~ "45 to 54",
                             age <= 64 ~ "55 to 64",
                             age <= 74 ~ "65 to 74",
                             age >= 75  ~ "75 or older",
                             TRUE ~ "Respondent Skipped"))

reduced_data <-
  reduced_data %>%
  mutate(vote_trump = 
           ifelse(vote_2020 == "Donald Trump", 1, 0))

reduced_data <-
  reduced_data %>%
  mutate(vote_biden = 
           ifelse(vote_2020 == "Joe Biden", 1, 0))

# Just keep some variables
reduced_data <- 
  reduced_data %>% 
  select(vote_trump,
         vote_biden,
         foreign_born,
         gender,
         race_ethnicity,
         household_income,
         education,
         state,
         age_bin,
         weight)

# Saving the survey/sample data as a csv file in my
# working directory
write_csv(reduced_data, "survey_data.csv")

