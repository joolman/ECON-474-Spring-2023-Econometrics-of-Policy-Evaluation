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
cps = read_csv('C:/Users/johnj/Dropbox/Julian/Academia/UIUC/Semester 12 Spring 2023/ECON474sp23/Data/DwD2 CPS.csv')

path_data = 'C:/Users/johnj/Dropbox/Julian/Academia/UIUC/Semester 12 Spring 2023/ECON474sp23/Data/'
paste0(path_data, 'DwD2 CPS.csv')
cps = read_csv(paste0(path_data,
                      'DwD2 CPS.csv'))

# looking at the data
head(cps)
summary(cps)

# What's up with STATEFIP?
table(cps$STATEFIP)

# (optional) Julian doesn't like holding shift:
tolower(names(cps))
names(cps) %>% tolower
cps %>% names %>% tolower

names(cps) = cps %>% names %>% tolower
names(cps)

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
?ifelse
?case_when

cps = cps %>%
  mutate(occ2010 = ifelse(occ2010 < 9800, occ2010, NA),
         earnweek = ifelse(earnweek == 9999.99, NA, earnweek),
         married = ifelse(marst <= 3, 1, 0),
         married = (marst <= 3)*1,
         female = (sex == 2)*1,
         degree = case_when(educ == 125 ~ 'Ph.D.',
                            educ == 124 ~ 'Professional',
                            educ == 123 ~ "Master's",
                            educ == 111 ~ "Bachelor's",
                            educ >= 91 ~ "Associate's",
                            educ >= 73 ~ "High School",
                            educ >= 2 ~ "None"),
         empstat = case_when(empstat %in% c(10, 12) ~ 'employed',
                             empstat %in% 21:22 ~ 'unemployed',
                             TRUE ~ 'NILF'))

## ------------------------ ##
#### 1.1) class: datetime ####
## ------------------------ ##
# install.packages('lubridate')
library(lubridate)

names(cps)
cps %>% select(month, year)
paste0(cps$year, '-', cps$month, '-01')
ym(paste0(cps$year, '-', cps$month))
ymd(paste0(cps$year, '-', cps$month, '-', 1))

cps = cps %>%
  mutate(date = ym(paste0(year, '-', month)))

class(cps$date)
##===============================================##
## --------------------------------------------- ##
#### 2) Creating state-level unemployment rate ####
## --------------------------------------------- ##
##===============================================##
urate_state = cps %>%
  group_by(statefip, date) %>%
  summarise(U = sum(wtfinl*(empstat == 'unemployed')),
            E = sum(wtfinl*(empstat == 'employed')),
            urate = (U)/(U + E)*100,
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

covid_state = covid %>%
  filter(code3 == 840) %>% # US states only: Sorry Puerto Rico
  select(-UID, -iso2, -iso3, -code3, -Admin2, -Country_Region,
         -Lat, -Long_, -Combined_Key) %>% # Nuisance keys
  mutate(FIPS = floor(FIPS/1000)) %>% # creating state fips codes
  group_by(FIPS, Province_State) %>%
  summarise(across(everything(), sum)) %>%
  filter(FIPS <= 56) %>% 
  # omit non-state locations (e.g. "Diamond Princess",
  # cruise ship)
  pivot_longer(cols = `1/22/20`:names(covid)[ncol(covid)],
               names_to = 'date',
               values_to = 'cum_cases') %>%
  mutate(date = mdy(date),
         date = floor_date(date, unit = 'month')) %>%
  group_by(FIPS, Province_State, date) %>%
  summarise(cum_cases = sum(cum_cases)) %>%
  mutate(cum_cases_lag = lag(cum_cases),
         new_cases = cum_cases - cum_cases_lag,
         new_cases = ifelse(is.na(new_cases), 0, new_cases))


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
  summarise(urate = sum(U)/sum(U + E),
            new_cases = sum(new_cases)) %>%
  pivot_longer(!date)

ggplot(df_usa, aes(x = date, y = value)) + 
  geom_point() +
  geom_line() +
  facet_grid(rows = vars(name),
             scales = 'free_y')
