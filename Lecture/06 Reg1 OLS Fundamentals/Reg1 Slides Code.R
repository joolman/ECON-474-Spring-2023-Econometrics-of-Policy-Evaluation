##======================##
## -------------------- ##
#### 0) Preliminaries ####
## -------------------- ##
##======================##
library(data.table) # fread()
library(tidyverse)  # Quality of life
library(fixest)     # feols(), advanced regressions [install]
library(latex2exp)

cps = fread('C:/Users/JJPWade/Dropbox/Julian/Academia/UIUC/Semester 12 Spring 2023/ECON474sp23/Data/Reg1 CPS 2021.csv')
# cps = fread('C:/Users/johnj/Dropbox/Julian/Academia/UIUC/Semester 12 Spring 2023/ECON474sp23/Data/Reg1 CPS 2021.csv')
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
  na.omit()


##===========================##
## ------------------------- ##
#### 2) Output and figures ####
## ------------------------- ##
##===========================##

cps %>%
  select(hrly_wage, yos, exp) %>% head


fml = log(hrly_wage) ~ yos + exp + I(exp^2) + female

feols(fml, data = cps) %>% 
  etable(se.row = FALSE,
         fitstat = ~ r2 + n,
         poly_dict = c('', '^2', '^3'))
mean(log(cps$hrly_wage)) %>% round(3)

fit = lm(fml, data = cps)

bind_cols(fit$model[, 1], 
          model.matrix(fit)) %>%
  rename(`log(hrly_wage)` = `...1`) %>%
  head

fit$model %>% tibble() %>% select(`log(hrly_wage)`) %>% head
model.matrix(fit) %>% head() 

b = coef(fit)
mean_exp = mean(cps$exp)
min_exp = min(cps$exp)
max_exp = max(cps$exp )

exp_partial = function(x){b['exp'] + b['I(exp^2)']*x}
exp_partial(mean_exp)


ggplot() +
  xlim(0, 50) +
  geom_function(fun = exp_partial) +
  geom_vline(aes(xintercept = min_exp, color = 'Minimum \nExperience'), 
             linetype = 'dashed') +
  geom_vline(aes(xintercept = max_exp, color = 'Maximum \nExperience'), 
             linetype = 'dashed') +
  geom_vline(aes(xintercept = mean_exp, color = 'Average \nExperience'), 
             linetype = 'dashed') +
  labs(x = 'Experience (years)', y = TeX('%\\Delta Wages'),
       color = '') +
  theme_minimal()
ggsave('C:/Users/JJPWade/Dropbox/Julian/Academia/UIUC/Semester 12 Spring 2023/ECON474sp23/Lecture/06 Reg1 OLS Fundamentals/partial experience.png',
       width = 16/4, height = 9/4, dpi = 600)


feols(log(hrly_wage) ~ 1, data = cps) %>% 
  etable(se.below = TRUE, se.row = FALSE,
         fitstat = ~ r2 + n,
         poly_dict = c('', '^2', '^3'))
mean(log(cps$hrly_wage)) %>% round(3)


feols(log(hrly_wage) ~ female, data = cps) %>% 
  etable(se.below = TRUE, se.row = FALSE,
         fitstat = ~ r2 + n,
         poly_dict = c('', '^2', '^3'))
mean(log(cps$hrly_wage)[cps$female == 0]) %>% round(3)
mean(log(cps$hrly_wage)[cps$female == 1]) %>% round(3)


feols(log(hrly_wage) ~ female + yos, data = cps) %>% 
  etable(se.below = TRUE, se.row = FALSE,
         fitstat = ~ r2 + n,
         poly_dict = c('', '^2', '^3'))


ggplot(cps, aes(x = yos, y = log(hrly_wage))) +
  geom_point() +
  theme_minimal() +
  labs(x = 'Years of Schooling', y = 'log Hourly Wage')
ggsave('C:/Users/JJPWade/Dropbox/Julian/Academia/UIUC/Semester 12 Spring 2023/ECON474sp23/Lecture/06 Reg1 OLS Fundamentals/heteroskedastic.png',
       width = 16/4, height = 9/4, dpi = 600)
