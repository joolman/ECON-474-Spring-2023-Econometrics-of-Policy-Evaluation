##======================##
## -------------------- ##
#### 0) Preliminaries ####
## -------------------- ##
##======================##
# Packages that make us happy:

# Loading in Current population survey data


# looking at the data

# What's up with STATEFIP?

# (optional) Julian doesn't like holding shift:

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


## ------------------------ ##
#### 1.1) class: datetime ####
## ------------------------ ##


##===============================================##
## --------------------------------------------- ##
#### 2) Creating state-level unemployment rate ####
## --------------------------------------------- ##
##===============================================##


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


## ------------------------------- ##
#### 3.2) Cleaning COVID-19 data ####
## ------------------------------- ##


##=====================##
## ------------------- ##
#### 4) Merging data ####
## ------------------- ##
##=====================##


##========================##
## ---------------------- ##
#### 5) A pretty figure ####
## ---------------------- ##
##========================##



