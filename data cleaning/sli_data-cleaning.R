########### Preamble ########### 
# Author: Hyo-Eun Park
# Title: Data Cleaning
# Context: Cleaning data about Specific Language Impairment (SLI) children
# Date: December 9, 2020
# Link to data set: https://www.kaggle.com/dgokeeffe/specific-language-impairment
# Pre-requisites:
# - Need to download the data set
# and save it in a folder called 'input'
# - Create a folder called 'output'
# which is where you are going to save the cleaned data
# - Both 'input' and 'output' should be in the working directory


### How to Set Working Directory ###
# Session > Set Working Directory > To Source File Location

### Workspace Set-up ###
library(tidyverse)

# Reading in raw data
rawdata <- read_csv("input/archive/all_data_R.csv")
# Adding labels
rawdata <- labelled::to_factor(rawdata)

### Random Sampling ###
set.seed(8181)
sampsize <- 1000
cleaned <- tibble(rawdata[sample(nrow(rawdata), size = sampsize, replace=FALSE), ])

### Selecting Variables ###
cleaned <- cleaned %>% 
  dplyr::select(group,
                age_years,
                child_TNS,
                fillers,
                mlu_words,
                dss,
                word_errors)

### Omit NA's ###
cleaned <- cleaned %>% na.omit() # none!

### Rounding Age to two decimal places
cleaned <- cleaned %>% 
  mutate(age_years = round(age_years, 2))
### Changing name of variable
names(cleaned)[names(cleaned) == 'age_years'] <- 'age'

### Dividing into three different groups ###
cleaned <- cleaned %>%
  mutate(age_group = 
           ifelse(
             4 <= age & age <= 6.99, "younger",
             ifelse(6.99 < age & age <= 10.99,"middle",
                    "older")))

### New dummy variable for treatment group ###
cleaned <- cleaned %>% 
  mutate(sli = ifelse(group == "SLI", 1, 0))

### Saving the sampled data ###
write_csv(cleaned, "output/sample_data.csv")
