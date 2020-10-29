#### Preamble ####
# Purpose: Load and clean the census data for post stratification purposes
# Authors: Name, Name, Name
# Date: 2 November 2020
# Contact: xudanny1@gmail.com, OTHER EMAILS
# License: MIT
# Pre-requisites: 
# - IPMUS ACS 5 year 2018 data downloaded with the following variables:
# - HHINCOME, US2018C_ST, SEX, RACE, BPL, US2018C_SCHL, AGE
# - Make sure the full file name is usa_0002.dta.gz
# - Place it in the root directory !!!!! Might change this later
# - Make sure that the directory and file path are correct for your computer


#### Workspace setup ####
library(haven)
library(tidyverse)

# Read in the raw data
# MAKE SURE THESE ARE BOTH CORRECT PATHS
setwd("X:/R studio proj/STA304-PS3")
raw_data <- read_dta("usa_00002.dta.gz")

# Add the labels
raw_data <- labelled::to_factor(raw_data)

reduced_data <- raw_data

## Foreign born variable creation (using birthplace)
# Create a numeric version of the birthplace data for ease of cleaning
reduced_data <- reduced_data %>%
  mutate(birthplace = as.numeric(bpl))
# Birthplace tidying into categorical variable
reduced_data <- reduced_data %>%
  mutate(foreign_born = case_when(is.element(birthplace, 1:51) ~ "The United States",
                                  is.element(birthplace, 52:162) ~ "Another country",
                                  TRUE ~ "Respondent Skipped"))

# Sex tidying (just capitalizing)
reduced_data <- reduced_data %>%
  mutate(sex = case_when(sex == "male" ~ "Male",
                         sex == "female" ~ "Female"))

# Race tidying
# We will ignore 2+ major races as its too hard to parse
reduced_data <- reduced_data %>%
  mutate(race_ethnicity = case_when(race == "white" ~ "White",
                                    race == "black/african american/negro" ~ "Black, or African American",
                                    race == "american indian or alaska native" ~ "American Indian or Alaska Native",
                                    race == "chinese" ~ "Asian (Chinese)",
                                    race == "japanese" ~ "Asian (Japanese)",
                                    race == "other asian or pacific islander" ~ "Other Asian or Pacific Islander",
                                    race == "other race, nec" ~ "Some other race",
                                    race == "two major races" ~ "Two Major Races",
                                    race == "three or more major races" ~ "Three or more Major Races",
                                    TRUE ~ "Respondent Skipped")) %>%
  filter(race_ethnicity != "Two Major Races") %>%
  filter(race_ethnicity != "Three or more Major Races")

# HH Income tidying
reduced_data <- reduced_data %>%
  mutate(household_income = case_when(hhincome <= 14999 ~ "Less than $14,999",
                                      hhincome <= 19999 ~ "$15,000 to $19,999",
                                      hhincome <= 24999 ~ "$20,000 to $24,999",
                                      hhincome <= 29999 ~ "$25,000 to $29,999",
                                      hhincome <= 34999 ~ "$30,000 to $34,999",
                                      hhincome <= 39999 ~ "$35,000 to $39,999",
                                      hhincome <= 44999 ~ "$40,000 to $44,999",
                                      hhincome <= 49999 ~ "$45,000 to $49,999",
                                      hhincome <= 54999 ~ "$50,000 to $54,999",
                                      hhincome <= 59999 ~ "$55,000 to $59,999",
                                      hhincome <= 64999 ~ "$60,000 to $64,999",
                                      hhincome <= 69999 ~ "$65,000 to $69,999",
                                      hhincome <= 74999 ~ "$70,000 to $74,999",
                                      hhincome <= 79999 ~ "$75,000 to $79,999",
                                      hhincome <= 84999 ~ "$80,000 to $84,999",
                                      hhincome <= 89999 ~ "$85,000 to $89,999",
                                      hhincome <= 94999 ~ "$90,000 to $94,999",
                                      hhincome <= 99999 ~ "$95,000 to $99,999",
                                      hhincome <= 124999 ~ "$100,000 to $124,999",
                                      hhincome <= 149999 ~ "$125,000 to $149,999",
                                      hhincome <= 174999 ~ "$150,000 to $174,999",
                                      hhincome <= 199999 ~ "$175,000 to $199,999",
                                      hhincome <= 249999 ~ "$200,000 to $249,999",
                                      hhincome == 9999999 ~ "Respondent Skipped",
                                      hhincome >= 250000 ~ "$250,000 and above",
                                      TRUE ~ "Respondent Skipped")) %>%
  filter(household_income != "Respondent Skipped")

# Education tidying
reduced_data <- reduced_data %>%
  mutate(education = case_when(us2018c_schl == "01" ~ "3rd Grade or less",
                               us2018c_schl == "02" ~ "3rd Grade or less",
                               us2018c_schl == "03" ~ "3rd Grade or less",
                               us2018c_schl == "04" ~ "3rd Grade or less",
                               us2018c_schl == "05" ~ "3rd Grade or less",
                               us2018c_schl == "06" ~ "3rd Grade or less",
                               us2018c_schl == "07" ~ "Middle School - Grades 4 - 8",
                               us2018c_schl == "08" ~ "Middle School - Grades 4 - 8",
                               us2018c_schl == "09" ~ "Middle School - Grades 4 - 8",
                               us2018c_schl == "10" ~ "Middle School - Grades 4 - 8",
                               us2018c_schl == "11" ~ "Middle School - Grades 4 - 8",
                               us2018c_schl == "12" ~ "Completed some high school",
                               us2018c_schl == "13" ~ "Completed some high school",
                               us2018c_schl == "14" ~ "Completed some high school",
                               us2018c_schl == "15" ~ "Completed some high school",
                               us2018c_schl == "16" ~ "High school graduate",
                               us2018c_schl == "17" ~ "High school graduate",
                               us2018c_schl == "18" ~ "Completed some college, but no degree",
                               us2018c_schl == "19" ~ "Completed some college, but no degree",
                               us2018c_schl == "20" ~ "Associate Degree",
                               us2018c_schl == "21" ~ "College Degree (such as B.A., B.S.)",
                               us2018c_schl == "22" ~ "Masters degree",
                               us2018c_schl == "24" ~ "Doctorate degree",
                               TRUE ~ "Respondent Skipped")) %>%
  filter(education != "Respondent Skipped")

# State code tidying
reduced_data <- reduced_data %>%
  mutate(state = case_when(us2018c_st == "01" ~ "AL",
                           us2018c_st == "02" ~ "AK",
                           us2018c_st == "04" ~ "AZ",
                           us2018c_st == "05" ~ "AR",
                           us2018c_st == "06" ~ "CA",
                           us2018c_st == "08" ~ "CO",
                           us2018c_st == "09" ~ "CT",
                           us2018c_st == "10" ~ "DE",
                           us2018c_st == "11" ~ "DC",
                           us2018c_st == "12" ~ "FL",
                           us2018c_st == "13" ~ "GA",
                           us2018c_st == "15" ~ "HI",
                           us2018c_st == "16" ~ "ID",
                           us2018c_st == "17" ~ "IL",
                           us2018c_st == "18" ~ "IN",
                           us2018c_st == "19" ~ "IA",
                           us2018c_st == "20" ~ "KS",
                           us2018c_st == "21" ~ "KY",
                           us2018c_st == "22" ~ "LA",
                           us2018c_st == "23" ~ "ME",
                           us2018c_st == "24" ~ "MD",
                           us2018c_st == "25" ~ "MA",
                           us2018c_st == "26" ~ "MI",
                           us2018c_st == "27" ~ "MN",
                           us2018c_st == "28" ~ "MS",
                           us2018c_st == "29" ~ "MO",
                           us2018c_st == "30" ~ "MT",
                           us2018c_st == "31" ~ "NE",
                           us2018c_st == "32" ~ "NV",
                           us2018c_st == "33" ~ "NH",
                           us2018c_st == "34" ~ "NJ",
                           us2018c_st == "35" ~ "NM",
                           us2018c_st == "36" ~ "NY",
                           us2018c_st == "37" ~ "NC",
                           us2018c_st == "38" ~ "ND",
                           us2018c_st == "39" ~ "OH",
                           us2018c_st == "40" ~ "OK",
                           us2018c_st == "41" ~ "OR",
                           us2018c_st == "42" ~ "PA",
                           us2018c_st == "44" ~ "RI",
                           us2018c_st == "45" ~ "SC",
                           us2018c_st == "46" ~ "SD",
                           us2018c_st == "47" ~ "TN",
                           us2018c_st == "48" ~ "TX",
                           us2018c_st == "49" ~ "UT",
                           us2018c_st == "50" ~ "VT",
                           us2018c_st == "51" ~ "VA",
                           us2018c_st == "53" ~ "WA",
                           us2018c_st == "54" ~ "WV",
                           us2018c_st == "55" ~ "WI",
                           us2018c_st == "56" ~ "WY",
                           TRUE ~ "Respondent Skipped"))
# Age cleaning
reduced_data <- reduced_data %>%
  mutate(age_tidied = as.numeric(age) - 1)
# Bin the ages
reduced_data <- reduced_data %>%
  mutate(age_bin = case_when(age_tidied <= 17 ~ "Not eligible",
                             age_tidied <= 24 ~ "18 to 24",
                             age_tidied <= 34 ~ "25 to 34",
                             age_tidied <= 44 ~ "35 to 44",
                             age_tidied <= 54 ~ "45 to 54",
                             age_tidied <= 64 ~ "55 to 64",
                             age_tidied <= 74 ~ "65 to 74",
                             age_tidied >= 75  ~ "75 or older",
                             TRUE ~ "Respondent Skipped")) %>%
  filter(age_bin != "Not eligible")

# Rename some variables
reduced_data <- reduced_data %>%
  mutate(gender = sex)

# Keep the variables that are relevant to the model
reduced_data <- reduced_data %>%
  select(foreign_born,
         gender,
         race_ethnicity,
         household_income,
         education,
         state,
         age_bin)
         
# Post Stratification cell counts
reduced_data <- reduced_data %>%
  count(foreign_born,
        gender,
        race_ethnicity,
        household_income,
        education,
        state,
        age_bin) %>%
  group_by(foreign_born,
           gender,
           race_ethnicity,
           household_income,
           education,
           state,
           age_bin) 

# Saving the census data as a csv file in root directory
write_csv(reduced_data, "census_data.csv")
