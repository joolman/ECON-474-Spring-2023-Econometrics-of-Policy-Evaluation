##======================##
## -------------------- ##
#### 0) Preliminaries ####
## -------------------- ##
##======================##
library(data.table) # fread()
library(tidyverse)  # Quality of life
library(fixest)     # feols(), extended regressions [install]

cps = fread('path\to\your\date')
names(cps) = cps %>% names %>% tolower

summary(cps)
##==========================##
## ------------------------ ##
#### 1) Cleaning the data ####
## ------------------------ ##
##==========================##
cps = 
  cps %>%
  filter(age >= 30 & age <= 55 & # prime age individual
           empstat %in% c(10, 12) & # Employed
           incwage > 0 & incwage < 99999999 & # positive, non-NA wages
           wkswork1 >= 40 & # worked at least 40 weeks last year
           uhrsworkly >= 30 & uhrsworkly != 999) %>% # works at least 30 hrs a week last year
  mutate(yos = case_when(educ == 1 ~ NaN, # This line is optional
                         educ == 2 ~ 0,
                         educ == 10 ~ 3,   # Grades 1, 2, 3, or 4
                         educ == 20 ~ 5.5, # Grades 5 or 6
                         educ == 30 ~ 7.5, # Grades 7 or 8
                         educ == 40 ~ 9,
                         educ == 50 ~ 10,
                         educ == 60 ~ 11,
                         educ == 71 ~ 12,
                         educ == 73 ~ 12, # HS Diploma
                         educ == 85 ~ 13,
                         educ == 91 ~ 14, # Associate's
                         educ == 111 ~ 16, # Bachelor's
                         educ == 123 ~ 18, # Master's
                         educ == 124 ~ 21, # Professional
                         educ == 125 ~ 21), # Ph.D.,
         exp = age - yos - 6,
         degree = case_when(educ == 125 ~ 'Ph.D.',
                            educ == 124 ~ 'Professional',
                            educ == 123 ~ "Master's",
                            educ == 111 ~ "Bachelor's",
                            educ %in% c(91, 92) ~ "Associate's",
                            educ >= 73 ~ 'High School',
                            educ >= 2 ~ 'None'),
         female = (sex == 2)*1,
         children = (nchild > 0)*1,
         black = (race == 200)*1,
         asian = (race %in% 651:652)*1,
         hrly_wage = incwage/(50*40),
         renter = (hhtenure %in% c(2,3))*1) %>%
  na.omit() # Removing observations with NA degree


##==========================##
## ------------------------ ##
#### 2) Basic Regressions ####
## ------------------------ ##
##==========================##

# Our first regression: the Mincer equation:

# I prefer to specify my polynomials like so:


# we can save regressions as objects


# which permits obtaining more details


# If you instead want to simply output the summary to the console:


# We can add our female variable like so


# check out what R does when we input an interaction


# How predictive of wages are college degrees?


# let's change the baseline degree to be the "none" category
# to do so, we will convert the character `degree` to a factor
# with the first level set to "None."
# I will also unecessarily order the degrees (useful for plotting)


# Ta-Dah!


##=============================##
## --------------------------- ##
#### 3) Extended regressions ####
## --------------------------- ##
##=============================##


# `feols()` is an extended `lm()`


# using heteroskedastic standard errors is straightforward


# You can run many regressions at once


# And with "multiple outcomes"


# Notice that the class of the state variable is numeric


# Fixed effects demeans for unique values of the variable


# Clustering standard errors requires inputting as a formula or string

