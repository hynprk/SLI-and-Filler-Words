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
SLIdata <- read_csv("output/sample_data.csv")

# Fix the class on some

SLIdata <-
  SLIdata %>% 
  mutate_at(vars(group), ~as.factor(.)) 

# Change some to factors

table(SLIdata$group) # 214 treatments

# Treatment: SLI
# Control: Typical Development (TD)
# Outcome of interest: number of filler words

### Logistic Model ###
# Whether a child has SLI or TD

propensity_score <- glm(sli ~ age + 
                          child_TNS + 
                          mlu_words + 
                          word_errors + 
                          dss, 
                        family = binomial,
                        data = SLIdata)

summary(propensity_score)

### Forecasting Data set ###

SLIdata <- 
  augment(propensity_score, 
          data = SLIdata,
          type.predict = "response") %>% 
  dplyr::select(-.resid, -.std.resid, -.hat, -.sigma, -.cooksd) 

# Creating matches
# We want to find those td children
# who were considered similar to sli children

SLIdata <- 
  SLIdata %>% 
  arrange(.fitted, sli)

# Using matching function (arm package)
## This finds which is the 
## closest of the ones that were not treated, to 
## each one that was treated.

SLIdata$treated <- 
  ifelse(SLIdata$sli == 1, 1, 0)

SLIdata$treated <- 
  as.integer(SLIdata$treated)

matches <- arm::matching(z = SLIdata$treated, 
                         score = SLIdata$.fitted)

SLIdata <- cbind(SLIdata, matches)

# Only leaving those that are matched in the data set
# Data reduction
# We had 214 treated, so we expect matched data set of 428 observations
sli_matched <- 
  SLIdata %>% 
  filter(match.ind != 0) %>% 
  dplyr::select(-match.ind, -pairs, -treated)

head(sli_matched)

# Examining the 'effect' of being treated on 
# average number of fillers
# in the 'usual' way.

propensity_score_regression <- 
  lm(fillers ~ age + 
       child_TNS +
       mlu_words + 
       word_errors + 
       dss + 
       sli, 
     data = sli_matched)

# Summary
summary(propensity_score_regression)
