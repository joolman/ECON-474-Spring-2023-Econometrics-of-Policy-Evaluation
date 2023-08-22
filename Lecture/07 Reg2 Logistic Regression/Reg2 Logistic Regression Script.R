##======================##
## -------------------- ##
#### 0) Preliminaries ####
## -------------------- ##
##======================##
library(tidyverse)
library(fixest)
library(marginaleffects) # [install]


acs = read_csv('C:/Users/johnj/Dropbox/Julian/Academia/UIUC/Semester 12 Spring 2023/ECON474sp23/Data/Reg2 ACS 2021 - Illinois.csv')

names(acs) = acs %>% names %>% tolower

##=================##
## --------------- ##
#### 1) Cleaning ####
## --------------- ##
##=================##
acs = acs %>%
  filter(gq %in% 1:2 &         # not living in group quarters: https://usa.ipums.org/usa-action/variables/GQTYPE#codes_section
           ownershp != 0 &     # either a home owner or renter
           age >= 16 & 
           empstat > 0 &       # defined employment status
           ftotinc < 9999999 & # defined total family income
           migrate1 > 0) %>%   # defined migration status
  mutate(in_lf = (empstat != 3)*1,
         renter = (ownershp == 2)*1,
         has_children = (nchild > 0)*1,
         female = (sex == 2)*1,
         married = (marst <= 3)*1,
         black = (race == 2)*1,
         asian = (race %in% 4:5)*1,
         hispanic = (hispan > 1)*1,
         degree = case_when(educd == 116 ~ 'Ph.D.',
                            educd == 115 ~ 'Professional',
                            educd == 114 ~ "Master's",
                            educd == 101 ~ "Bachelor's",
                            educd == 81 ~ "Associate's",
                            educd >= 63 ~ 'High School',
                            educd >= 2 ~ 'None'),
         yos = case_when(educd %in% 2:12  ~ 0,
                         educd == 14 ~ 1,
                         educd == 15 ~ 2,
                         educd == 16 ~ 3,
                         educd == 17 ~ 4,
                         educd == 22 ~ 5,
                         educd == 23 ~ 6,
                         educd == 25 ~ 7,
                         educd == 26 ~ 8,
                         educd == 30 ~ 9,
                         educd == 40 ~ 10,
                         educd == 50 ~ 11,
                         educd %in% c(61:65)  ~ 12,
                         educd == 71 ~ 13,
                         educd == 81 ~ 14,
                         educd == 101 ~ 16,
                         educd == 114 ~ 18,
                         educd == 115 ~ 20,
                         educd == 116 ~ 21),
         moved_ly = (migrate1 > 1)*1) %>%
  na.omit()

##============================##
## -------------------------- ##
#### 2) Logistic Regression ####
## -------------------------- ##
##============================##


## ------------------ ##
#### 2.1) R's logit ####
## ------------------ ##
?glm
names(acs)

# A simple logit
fit1 = glm(in_lf ~ age, data = acs, family = 'binomial')
summary(fit1)

# Let's add some more variables
fit2 = glm(in_lf ~ age + yos + female*has_children*married + black + asian + hispanic +
             moved_ly + density + ftotinc,
           data = acs, family = 'binomial')
# No `log(ftotinc)` b/c negative values exist
summary(fit2)

# marginaleffects(fit2, newdata = 'mean')
?marginaleffects

marginaleffects(fit2, newdata = 'mean')
# note interaction terms are dropped 

# let's save this and compare to a model without interactions
me_fit2_mean = marginaleffects(fit2, newdata = 'mean')

# Manually obtaining marginal effects to closely match `marginaleffects()`
fit3 = glm(in_lf ~ age + yos + female + has_children +married + black + asian + hispanic +
      moved_ly + density + ftotinc,
    data = acs, family = 'binomial')

# storing the coefficients
b = fit3$coefficients
# Plugging into the logistic first derivative
mean(  exp(fit3$fitted.values)/(  (1 + exp(fit3$fitted.values))^2  )  )*b

# comparing (note: no intercept term)
me_fit2_mean$dydx
# the package `marginaleffects` uses an algorithm for dy/dx


## --------------------- ##
#### 2.2) fixest logit ####
## --------------------- ##
?feglm

# Explicit fixed effects 
system.time({
  glm(in_lf ~ age + yos + female*has_children*married + black + asian + hispanic +
        moved_ly + ftotinc + as.character(puma),
      data = acs, family = 'binomial')
})

# Absorbed fixed effects
system.time({
  feglm(in_lf ~ age + yos + female*has_children*married + black + asian + hispanic +
        moved_ly + ftotinc | puma,
      data = acs, family = 'binomial')
})


fe_fit2 = feglm(in_lf ~ age + yos + female*has_children*married + black + asian + hispanic +
                  moved_ly + ftotinc|puma,
                data = acs, family = 'binomial')
fe_fit2

marginaleffects(fe_fit2, newdata = 'mean')

