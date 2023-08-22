# Prepare to open CPS IPUMS page

# ---------------------------------------------------------------------------- #

# Plot state-level u-rate and n COVID cases for 2020

##======================##
## -------------------- ##
#### 0) Preliminaries ####
## -------------------- ##
##======================##
# Packages that make us happy:
library(tidyverse)

# Loading in Current population survey data
?read.csv
?read_csv
cps = read_csv('C:/Users/JJPWade/Dropbox/Julian/Academia/UIUC/Semester 12 Spring 2023/ECON474sp23/Data/DwD2 CPS.csv')
# Where class data are saved
path_data = 'C:/Users/JJPWade/Dropbox/Julian/Academia/UIUC/Semester 12 Spring 2023/ECON474sp23/Data/'
path_cps = 
  paste0(path_data, 'DwD2 CPS.csv')
cps = read_csv(path_cps)

# looking at the data
head(cps)
summary(cps) # look for weird things like 999: OCC2010, UHRSWORKT, EARNWEEK

# What's up with STATEFIP?
table(cps$STATEFIP)

# (optional) Julian doesn't like holding shift:
names(cps)
tolower(names(cps))
names(cps) %>% tolower
cps %>% names %>% tolower
names(cps) = cps %>% names %>% tolower

##=====================##
## ------------------- ##
#### 1) Cleaning CPS ####
## ------------------- ##
##=====================##
# We are going to clean the variables:
# - OCC2010: https://cps.ipums.org/cps-action/variables/OCC2010
# - EARNWEEK: https://cps.ipums.org/cps-action/variables/EARNWEEK
# - MARST: https://cps.ipums.org/cps-action/variables/MARST
# - SEX: https://cps.ipums.org/cps-action/variables/SEX
# - EDUC: https://cps.ipums.org/cps-action/variables/EDUC
# - EMPSTAT: https://cps.ipums.org/cps-action/variables/EMPSTAT

cps = 
cps %>%
  mutate(occ2010 = ifelse(occ2010 == 9999, NA, occ2010),
         earnweek = ifelse(earnweek == 9999.99, NA, earnweek),
         married = (marst <= 3)*1, # logic statement: TRUE == 1, FALSE == 0
         female = ifelse(sex == 2, 1, 0), # ifelse() male == 1,
         degree = case_when(educ == 125 ~ 'phd',
                            educ == 124 ~ 'professional',
                            educ == 123 ~ "master's",
                            educ == 111 ~ "bachelor's",
                            educ >= 91 ~ "associate's",
                            educ >= 73 ~ 'high school',
                            educ >= 2 ~ 'none'), # if educ code is 1, then label as NA
         empstat = case_when(empstat %in% c(10, 12) ~ 'employed',
                             empstat %in% 21:22 ~ 'unemployed',
                             TRUE ~ 'NILF'))

## ------------------------ ##
#### 1.1) class: datetime ####
## ------------------------ ##
# install.packages('lubridate') # Ctrl-Shift-C
library(lubridate)

# names(cps)
# select(cps, month, year)
# cps %>% select(month, year)
# paste0(cps$year, '-', cps$month)
# ym(paste0(cps$year, '-', cps$month))
# ymd(paste0(cps$year, '-', cps$month, '-1'))

cps = cps %>%
  mutate(date = ym(paste0(cps$year, '-', cps$month)))

class(cps$date)

##===============================================##
## --------------------------------------------- ##
#### 2) Creating state-level unemployment rate ####
## --------------------------------------------- ##
##===============================================##
cps$empstat == 'employed'

urate_state = 
cps %>% 
  group_by(statefip, date) %>%
  summarise(U = sum(wtfinl*(empstat == 'unemployed')), 
            E = sum(wtfinl*(empstat == 'employed')),
            urate = U/(U + E)*100,
            pop = sum(wtfinl))

##======================##
## -------------------- ##
#### 3) COVID-19 data ####
## -------------------- ##
##======================##

## ---------------------------------- ##
#### 3.1) remotely downloading data ####
## ---------------------------------- ##

# download.file('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv',
#               paste0(path_data, 'DwD2 covid19.csv'))

covid = read_csv(paste0(path_data, 'DwD2 covid19.csv'))

## ------------------------------- ##
#### 3.2) Cleaning COVID-19 data ####
## ------------------------------- ##
head(covid)

covid_state =
  covid %>%
  filter(code3 == 840) %>% # US states only
  select(-UID, -iso3, -iso2, -code3, -Admin2, -Country_Region, -Lat, -Long_, -Combined_Key) %>%
  mutate(FIPS = floor(FIPS/1000)) %>% # creating state FIPS codes
  group_by(FIPS, Province_State) %>% # group by states (codes and labels)
  summarise(across(everything(), sum)) %>%
  filter(FIPS <= 56) %>% # omit non-state locations (e.g. "Diamond Princess" cruise ship)
  pivot_longer(cols = `1/22/20`:names(covid)[ncol(covid)], # selecting to last column
               names_to = 'date',
               values_to = 'total_cases') %>%
  mutate(date = mdy(date),
         date = floor_date(date, unit = 'month')) %>% # month flooring to match CPS
  group_by(FIPS, Province_State, date) %>%
  summarise(total_cases = sum(total_cases)) %>%
  mutate(total_lag = lag(total_cases),
         new_cases = total_cases - total_lag,
         new_cases = ifelse(is.na(new_cases), 0, new_cases)) # manually adding Dec.

##=====================##
## ------------------- ##
#### 4) Merging data ####
## ------------------- ##
##=====================##
?inner_join

df_state = 
inner_join(urate_state, covid_state,
           by = c('statefip' = 'FIPS',
                  'date' = 'date')) %>%
  mutate(new_cases_per_cap = new_cases/pop)

##========================##
## ---------------------- ##
#### 5) A pretty figure ####
## ---------------------- ##
##========================##
df_usa = 
  df_state %>%
  group_by(date) %>%
  summarise(urate = sum(U/(U + E)),
            new_cases = sum(new_cases)) %>%
  pivot_longer(!date)

ggplot(df_usa, aes(x = date, y = value)) +
  geom_point() +
  geom_line() +
  facet_grid(rows = vars(name),
             scales = 'free_y')


