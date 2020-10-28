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

## Foreign born variable creation (using birthplace)
# Create a numeric version of the birthplace data for ease of cleaning
raw_data <- raw_data %>%
  mutate(birthplace = as.numeric(bpl))
# Birthplace tidying into categorical variable
raw_data <- raw_data %>%
  mutate(foreign_born = case_when(is.element(birthplace, 1:51) ~ "no",
                                  is.element(birthplace, 52:162) ~ "yes",
                                  TRUE ~ "not available"))

# Sex tidying doesn't need to happen

# Race tidying
# We will ignore 2+ major races as its too hard to parse
raw_data <- raw_data %>%
  mutate(race_ethnicity = case_when(race == "white" ~ "white",
                                    race == "black/african american/negro" ~ "black, or african american",
                                    race == "american indian or alaska native" ~ "american indian or alaska native",
                                    race == "chinese" ~ "asian (chinese)",
                                    race == "japanese" ~ "asian (japanese)",
                                    race == "other asian or pacific islander" ~ "other asian or pacific islander",
                                    race == "other race, nec" ~ "some other race",
                                    TRUE ~ "not relevant"))

# HH Income tidying
raw_data <- raw_data %>%
  mutate(household_income = case_when(hhincome <= 14999 ~ "less than 14999",
                                      hhincome <= 19999 ~ "15000 to 19999",
                                      hhincome <= 24999 ~ "20000 to 24999",
                                      hhincome <= 29999 ~ "25000 to 29999",
                                      hhincome <= 34999 ~ "30000 to 34999",
                                      hhincome <= 39999 ~ "35000 to 39999",
                                      hhincome <= 44999 ~ "40000 to 44999",
                                      hhincome <= 49999 ~ "45000 to 49999",
                                      hhincome <= 54999 ~ "50000 to 54999",
                                      hhincome <= 59999 ~ "55000 to 59999",
                                      hhincome <= 64999 ~ "60000 to 64999",
                                      hhincome <= 69999 ~ "65000 to 69999",
                                      hhincome <= 74999 ~ "70000 to 74999",
                                      hhincome <= 79999 ~ "75000 to 79999",
                                      hhincome <= 84999 ~ "80000 to 84999",
                                      hhincome <= 89999 ~ "85000 to 89999",
                                      hhincome <= 94999 ~ "90000 to 94999",
                                      hhincome <= 99999 ~ "95000 to 99999",
                                      hhincome <= 124999 ~ "100000 to 124999",
                                      hhincome <= 149999 ~ "125000 to 149999",
                                      hhincome <= 174999 ~ "150000 to 179999",
                                      hhincome <= 199999 ~ "175000 to 199999",
                                      hhincome <= 249999 ~ "200000 to 249999",
                                      hhincome == 9999999 ~ "not available",
                                      hhincome >= 250000 ~ "greater than 250000",
                                      TRUE ~ "not available"))

# Education tidying
# REMOVE VOCATIONAL FROM SURVEY DATASET
# REMOVE COMPLETED SOME GRADUATE BUT NO DEGREE
raw_data <- raw_data %>%
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
                               TRUE ~ "not relevant"))

# State code tidying
raw_data <- raw_data %>%
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
                           TRUE ~ "not available"))
# Age cleaning
raw_data <- raw_data %>%
  mutate(age_tidied = as.numeric(age) - 1)
# Bin the ages
raw_data <- raw_data %>%
  mutate(age_bin = case_when(age_tidied <= 17 ~ "Not eligible",
                             age_tidied <= 24 ~ "18 to 24",
                             age_tidied <= 34 ~ "25 to 34",
                             age_tidied <= 44 ~ "35 to 44",
                             age_tidied <= 54 ~ "45 to 54",
                             age_tidied <= 64 ~ "55 to 64",
                             age_tidied <= 74 ~ "65 to 74",
                             age_tidied >= 75  ~ "75 and older",
                             TRUE ~ "not available"))

# Keep the variables that are relevant to the model
reduced_data <- 
  raw_data %>% 
  select(foreign_born,
         sex,
         race_ethnicity,
         household_income,
         education,
         state,
         age_bin)
         
# Post Stratification cell counts
reduced_data <- reduced_data %>%
  count(foreign_born,
        sex,
        race_ethnicity,
        household_income,
        education,
        state,
        age_bin) %>%
  group_by(foreign_born,
           sex,
           race_ethnicity,
           household_income,
           education,
           state,
           age_bin) 

# Saving the census data as a csv file in my
# working directory
write_csv(reduced_data, "census_data.csv")
