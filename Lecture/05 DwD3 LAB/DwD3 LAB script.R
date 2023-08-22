##======================##
## -------------------- ##
#### 0) Preliminaries ####
## -------------------- ##
##======================##

# * In the console, install the packages "data.table" and "readxl"
# * load the packages
#   - data.table
#   - readxl
#   - lubridate
#   - tidyverse
library(data.table)
library(readxl)
library(lubridate)
library(tidyverse)

# * create an object called `path_data` that is the path to the folder containing
#   the data set "DwD3 CPS degree earning 2010-2022.csv" and "DwD3 CPI 2010-2022.xlsx"

path_data = 'C:/Users/JJPWade/Dropbox/Julian/Academia/UIUC/Semester 12 Spring 2023/ECON474sp23/Data/'

# * read the CPS data in and save as an object called `cps` 
#   - using `path_data` and "DwD3 CPS degree earning 2010-2022.csv" in `paste0()`
#     as the first argument of `fread()` (from data.table)

cps = fread(paste0(path_data,
                   'DwD3 CPS degree earning 2010-2022.CSV'))

# optionally, make the names of `cps` lower case

names(cps) = cps %>% names %>% tolower

# Print the head of `cps` to the console

head(cps)

# * Before reading in the CPI data, open the file in Excel and look at the layout
# * Similarly to loading in the CPS data, use `paste0()` with `read_excel()` to
#   load in the data accounting for the strucutre of the data
#   - Option 1: use the `read_excel()` argument `skip`
#   - Option 2: use the `read_excel()` argument `range`
#
# hint: ?read_excel

cpi = read_excel(paste0(path_data,
                        'DwD3 CPI 2010-2022.xlsx'),
           skip = 11)

##=====================##
## ------------------- ##
#### 1) cleaning CPS ####
## ------------------- ##
##=====================##

# * In one line (e.g. using %>%) create the object `cps_clean` from `cps` by:
#   1. overwrite the variable EARNWEEK with EARNWEEK where the NA coded numbers
#       are replaced with `NA`s using the function `ifelse()` within `mutate()`
#   2. chain the function `na.omit()` to remove observations with empty cells
#   3. Using mutate()
#     3.1) create the variable `degree` in by copying and pasting YOUR code
#       from the previous lecture
#     3.2) create the variable `date` using `ym()` by pasting together the
#       variables `year` and `month` (don't forget the dash!)

cps_clean = cps %>%
  mutate(earnweek = ifelse(earnweek == 9999.99,
                           NA,
                           earnweek)) %>%
  na.omit() %>%
  mutate(degree = case_when(educ == 125 ~ 'phd',
                            educ == 124 ~ 'professional',
                            educ == 123 ~ "master's",
                            educ == 111 ~ "bachelor's",
                            educ >= 91 ~ "associate's",
                            educ >= 73 ~ 'high school',
                            educ >= 2 ~ 'none'),
         date = ym(paste(year, month, sep = '-')))

##=====================##
## ------------------- ##
#### 2) cleaning CPI ####
## ------------------- ##
##=====================##

# * In one line, create the object `cpi_long` from `cpi` by
#   1. negatively `select()` (e.g. `select(-HALF1, variable2, ...)` ) variables 
#       that are neither the column `Year` or a month
#       Note: this may not apply to you if you used the `range` argument of
#         `read_excel()`
#   2. chain on `pivot_longer()` with 
#       - the first argument set to `!Year`
#       - set the names to "month"
#       - set the values to "cpi"
#   3. chain on the function `mutate()` to create the variable `date` like the
#       previous section. (notice the capitalization change)
#   4. negatively `select()` the variables `Year` and `month`

cpi_long = cpi %>%
  select(-HALF1, -HALF2) %>%
  pivot_longer(!Year,
               names_to = 'month',
               values_to = 'cpi') %>%
  mutate(date = ym(paste(Year, month, sep = '-'))) %>%
  select(-Year, -month)

# * Create the 1x1 data frame `cpi_2019` from `cpi_long` in one line by
#   1. `filter()` observations such that the `year()` of `date` is equal to 2019
#   2. `summarise()` the variable `cpi_2019` to be the `mean()` of the variabel `cpi`

cpi_2019 = cpi_long %>%
  filter(year(date) == 2019) %>%
  summarise(cpi_2019 = mean(cpi))

##============================##
## -------------------------- ##
#### 3) merging CPS and CPI ####
## -------------------------- ##
##============================##

# * Create the object `df_plot` in one line by:
#   1. `inner_join()`ing `cps_clean` and `cpi_long`
#       Hint: do you need to specify the join argument `by`? why or why not
#   2. chain the function `bind_cols()` with the first argument of `cpi_2019`
#   3. use `mutate()` to create `r_earnweek`: the real weekly earnings in $2019
#   4. `group_by()` the variables `degree` and `date`
#   5. `summarise()` the variable `real_earnweek` by using the function
#       `weighted.mean()` on `r_earnweek` and `earnwt` appropriately

df_plot = inner_join(cps_clean, cpi_long) %>%
  bind_cols(cpi_2019) %>%
  mutate(r_earnweek = earnweek * cpi_2019/cpi) %>%
  group_by(degree, date) %>%
  summarise(real_earnweek = weighted.mean(r_earnweek, earnwt))

##==========================================##
## ---------------------------------------- ##
#### 4) Degree-level real weekly earnings ####
## ---------------------------------------- ##
##==========================================##


# Use `ggplot()` to create a time series of degree-level real wages from 2010-2022
# Hints: 
#   - set the x-axis to `date`, y-axis to `real_earnweek`, and `color` to `degree`
#   - use `geom_line()`
#   - label your axis. Some arguments you can play with are:
#     `x`, `y`, `title`, `subtitle`, `caption`, `color`

ggplot(df_plot, aes(x = date, y = real_earnweek,
                    color = degree)) +
  geom_line() +
  scale_x_continuous(breaks = seq.Date(ym('2010-01'),
                                       ym('2024-01'),
                                       by = '2 year'),
                     labels = seq(2010, 2024, by = 2)) +
  labs(x = '', y = 'Real Weekly Earnings ($2019)',
       title = 'Real Weekly Earnings by Highest Degree Obtained',
       color = 'Degree',
       caption = 'Source: CPS weekly earnings and BLS CPI-U') 

# For which degree levels (if any) did real wages increase in the past decade?




