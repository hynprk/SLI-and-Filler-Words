########### Preamble ########### 
# Author: Hyo-Eun Park
# Title: Data Cleaning
# Context: Propensity Score Matching will be for Specific Language Impairment (SLI) propensity.
# Date: December 9, 2020
# Link to data set: https://www.kaggle.com/dgokeeffe/specific-language-impairment
# Pre-requisites:
# - Have the sample_data.csv ready
# (this will be created by `data_cleaning.R`)

### How to Set Working Directory ###
# Session > Set Working Directory > To Source File Location

### Workspace Set-up ###
library(tidyverse)
library(broom)
library(arm)

### Reading in sampled data ###
sli_data <- read_csv("output/sample_data.csv")

# Fix the class on some

sli_data <-
  sli_data %>% 
  mutate_at(vars(group, age_group), ~as.factor(.)) 

# Change some to factors

table(sli_data$group) # 230 treatments

# Treatment: SLI
# Control: Typical Development (TD)
# Outcome of interest: number of filler words

### Logistic Model ###
# Whether a child has SLI or TD

propensity_score <- glm(sli ~ age + 
                          child_TNS + 
                          mlu_words + 
                          word_errors + 
                          dss +
                          retracing, 
                        family = binomial,
                        data = sli_data)

### Forecasting Data set ###

sli_data <- 
  augment(propensity_score, 
          data = sli_data,
          type.predict = "response") %>% 
  dplyr::select(-.resid, -.std.resid, -.hat, -.sigma, -.cooksd) 

# Creating matches
# We want to find those td children
# who were considered similar to sli children

sli_data <- 
  sli_data %>% 
  arrange(.fitted, sli)

# Using matching function (arm package)
## This finds which is the 
## closest of the ones that were not treated, to 
## each one that was treated.

sli_data$treated <- 
  ifelse(sli_data$sli == 1, 1, 0)

sli_data$treated <- 
  as.integer(sli_data$treated)

matches <- arm::matching(z = sli_data$treated, 
                         score = sli_data$.fitted)

sli_data <- cbind(sli_data, matches)

# Only leaving those that are matched in the data set
# Data reduction
# We had 230 treated, so we expect matched data set of 460 observations
sli_matched <- 
  sli_data %>% 
  filter(match.ind != 0) %>% 
  dplyr::select(-match.ind, -pairs, -treated)

head(sli_matched)

# Examining the 'effect' of being treated on 
# average number of fillers
# in the 'usual' way.

propensity_score_regression <- 
  lm(fillers ~ age + 
       mlu_words + 
       word_errors + 
       child_TNS + 
       sli + 
       dss + 
       retracing, 
     data = sli_matched)

# Summary
summary(propensity_score_regression)

# Backward AIC
step(propensity_score_regression, direction = "backward")

# Backward BIC
n <- length(sli_matched$group)
step(propensity_score_regression, direction = "backward", k = log(n))
