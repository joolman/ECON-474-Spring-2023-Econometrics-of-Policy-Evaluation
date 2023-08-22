##======================##
## -------------------- ##
#### 0) Preliminaries ####
## -------------------- ##
##======================##
library(data.table) # fread()
library(tidyverse)  # Quality of life
library(fixest)     # feols(), extended regressions [install]

cps = fread('C:/Users/johnj/Dropbox/Julian/Academia/UIUC/Semester 12 Spring 2023/ECON474sp23/Data/Reg1 CPS 2021.csv')
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
lm(log(hrly_wage) ~ yos + exp + I(exp^2),
   data = cps)

# I prefer to specify my polynomials like so:
lm(log(hrly_wage) ~ yos + poly(exp, 2),
   data = cps)

# we can save regressions as objects
fit = lm(log(hrly_wage) ~ yos + poly(exp, 2),
         data = cps)

# which permits obtaining more details
summary(fit)

# If you instead want to simply output the summary to the console:
lm(log(hrly_wage) ~ yos + poly(exp, 2),
   data = cps) %>% 
  summary()

# We can add our female variable like so
lm(log(hrly_wage) ~ yos + poly(exp, 2) + female,
   data = cps) %>% summary

# check out what R does when we input an interaction
lm(log(hrly_wage) ~ yos + poly(exp, 2) + female*children,
   data = cps) %>% summary

# How predictive of wages are college degrees?
lm(log(hrly_wage) ~ yos + poly(exp, 2) + female*children + degree,
   data = cps) %>% summary

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
?feols

# `feols()` is an extended `lm()`
feols(log(hrly_wage) ~ yos + poly(exp, 2), data = cps)

# using heteroskedastic standard errors is straightforward
feols(log(hrly_wage) ~ yos + poly(exp, 2), 
      data = cps, se = 'hetero')

# You can run many regressions at once
feols(log(hrly_wage) ~ csw(yos + poly(exp, 2), female*children, degree), 
      data = cps, se = 'hetero')

# And with "multiple outcomes"

feols(c(log(hrly_wage), renter) ~ csw(yos + poly(exp, 2), female*children, degree), 
      data = cps, se = 'hetero')

# Notice that the class of the state variable is numeric
class(cps$statefip)

# Fixed effects demeans for unique values of the variable
feols(log(hrly_wage) ~ yos + poly(exp, 2) | statefip, 
      data = cps, se = 'hetero')

# Clustering standard errors requires inputting as a formula or string

feols(log(hrly_wage) ~ yos + poly(exp, 2) | statefip, 
      data = cps, cluster = ~ statefip)

