# This script creates the figure(s) for the first lecture
#
# Data sources:
# - Median Income:    https://www.census.gov/data/tables/time-series/demo/income-poverty/historical-income-people.html
# - Real GDP per cap: https://fred.stlouisfed.org/series/A939RX0Q048SBEA
#
#
##===================##
## ----------------- ##
#### Preliminaries ####
## ----------------- ##
##===================##
# Loading in required packages of functions with
library(readxl)    # read_excel()
library(stringr)   # str_sub()
library(tidyverse) # A collection of packages:
                   # magrittr: %>%
                   # dplyr: inner_join(), arrange(), mutate(), select()
                   # tidyr: pivot_longer()
                   # ggplot: ggplot()
                   # tibble: as_tibble()

# Telling R where the class folder is located
setwd('C:/Users/johnj/Dropbox/Julian/Academia/UIUC/Semester 11 Fall 2022/ECON474/')
list.files()
list.files('Data')
# [1] "BEA rGDPc.csv"          "Census Bureau p02.xlsx"

##=========================##
## ----------------------- ##
#### Loading in the data ####
## ----------------------- ##
##=========================##

##-------------------------## 
# BEA - Real GDP per capita #
##-------------------------##
rgdpc = read.csv("Data/BEA rGDPc.csv")
# Looking at the top 6 rows of the data
head(rgdpc)
# And the bottom 6 rows
tail(rgdpc)

##--------------------------------##
# Census - Real Median Male Income #
##--------------------------------##
?read_excel
# Take a look at the Census data by opening the Excel file.
# It is not so nicely formatted. Luckily, R can handle that.
med_inc = read_excel('Data/Census Bureau p02.xlsx',
                     range = 'A10:G86')
head(med_inc); tail(med_inc)

##=======================##
## --------------------- ##
#### Cleaning the data ####
## --------------------- ##
##=======================##

##-------------------------## 
# BEA - Real GDP per capita #
##-------------------------##
# Creating friendly names
names(rgdpc) = c('year', 'rgdpc')
# Remember that the year data were fromatted like dates?
rgdpc$year

# The quotes around each cell tell us that they are characters, not numbers
class(rgdpc$year)
# we can make it date if we want:
?as.Date
as.Date(rgdpc$year, format = "%d/%m/%Y")
# But we only need the year, so we are going to change the variable
# from a character to a numeirc
#
# Grabbing the last four characters (the year) of each string
str_sub(rgdpc$year, -4, -1)
# And making them numbers
as.numeric(str_sub(rgdpc$year, -4, -1))
# Note that so far we have only output these results into the console!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# 
# We need to change the `year` variable in the data frame.
# To do so, we are going to overwrite it with the corrected version.
rgdpc$year = as.numeric(str_sub(rgdpc$year, -4, -1))

head(rgdpc); tail(rgdpc) # Tah-dah!

# Since we are checking classes, let's look at the `rgdpc` variable 
class(rgdpc$rgdpc)
# all is good!

##--------------------------------##
# Census - Real Median Male Income #
##--------------------------------##
head(med_inc)
# A few things to notice:
# 1) We need to rename the variables
# 2) The "year" variable has notes in it (e.g. "(40)")
# 3) There is more output than:
head(rgdpc)
#    That is because...
class(rgdpc)
class(med_inc)
#    ... the median income data are stored as a tibble: a modified data frame.
#    We can make `rgdpc` a tibble by:
as_tibble(rgdpc)
# Tibbles are helpful beyond standard data frames because they
# tell us the class of each variable. I prefer them, but there
# is no need to change, so we won't.

# 1) Renaming the variables for the median income data
names(med_inc) = c('year',
                   'number_thousands_male', 'income_nominal_male', 'income_real_male',
                   'number_thousands_female', 'income_nominal_female', 'income_real_female')

# 2) Cleaning the year variable
# Using the "pipe operator" (%>%) from magrittr (compare to line 79)
med_inc$year = med_inc$year %>% str_sub(1, 4) %>% as.numeric


# Combining the data together and preparing for plotting
df = inner_join(med_inc, rgdpc, by = 'year') %>%
  # I am picky and want the years to be ascending
  arrange(year) %>%
  # We want median income and rgdpc to start at 100 for the figure for comparison.
  # In other words, we are creating an index.
  # Let's make that adjustment.
  mutate(rgdpc_100 = rgdpc/rgdpc[1]*100,
         med_inc_100 = income_real_male/income_real_male[1]*100)

df_plot = df %>%
  # Changing the format of the data to make it easier to plot
  # choosing the variables we need
  select(year, rgdpc_100, med_inc_100) %>%
  # Adjusting the format such that the data are in columns of:
  # x: year
  # y: index
  # group: variable
  pivot_longer(-year, 
               names_to = 'variable', 
               values_to = 'index')

##=======================##
## --------------------- ##
#### Plotting the Data ####
## --------------------- ##
##=======================##

ggplot(df_plot, aes(x = year, y = index, color = variable, linetype = variable)) +
  geom_line() +
  labs(title = 'Real GDP per Capita versus Real Male Median Income',
       x = 'Year', y = 'Index') +
  theme(legend.title = element_blank()) +
  scale_color_discrete(labels = c('Real Male Median Income',
                                  'Real GDP per Capita')) +
  scale_linetype_discrete(labels = c('Real Male Median Income',
                                     'Real GDP per Capita'))
# Saving the plot
ggsave('Lectures/01 Overview/rgdpc vs r med inc.png',
       width = 8, height = 4.5, units = 'in')



cor(df$rgdpc_100, df$med_inc_100)
fit = lm(rgdpc_100 ~ med_inc_100, data = df[df$year >= 1975, ])
summary(fit)
