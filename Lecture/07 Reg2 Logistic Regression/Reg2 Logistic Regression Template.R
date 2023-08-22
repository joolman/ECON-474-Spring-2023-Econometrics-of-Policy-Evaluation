##======================##
## -------------------- ##
#### 0) Preliminaries ####
## -------------------- ##
##======================##


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


## --------------------- ##
#### 2.2) fixest logit ####
## --------------------- ##


