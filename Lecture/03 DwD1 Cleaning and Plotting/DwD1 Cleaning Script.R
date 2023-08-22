# Reading data into R
# 1) (top right) Environment > Import Dataset > From Text (base)...
# 2) (middle right) Files > locate file > Import Dataset...
# Problem w/ (1) and (2) Slow with large datasets
#
# 3) Algorithmically:

# Help page
?read.csv
gm = read.csv('Data/DwD1 gapminder.csv')

# data are small, so okay, but same issue as reading (1) and (2)
View(gm) # [pop out. Mention doesn't always show all columns]

# How many countries per continent?
table(gm$continent)

##====================##
## ------------------ ##
#### 1) Basic plots ####
## ------------------ ##
##====================##

# Histogram
?hist
hist(gm$gdpPercap_2007,
     main = 'Global 2007 GDPc Distribution',
     xlab = 'GDP per Capita ($2007)')

# Scatterplots
?plot
plot(gm$gdpPercap_2007, gm$lifeExp_2007) # option 1
plot(lifeExp_2007 ~ gdpPercap_2007, data = gm) # option 2
plot(lifeExp_2007 ~ gdpPercap_2007, data = gm,
     xlab = 'GDP per Capita ($2007)',
     ylab = 'Life Expectancy (years)',
     main = 'GDP vs. Life Expectancy',
     pch = 16)
?pch

##=================##
## --------------- ##
#### 2) Packages ####
## --------------- ##
##=================##
# User written functions that improve the 20+ year old R

# install.packages('ggplot2')
library(ggplot2)

ggplot(gm, aes(x = gdpPercap_2007, y = lifeExp_2007)) +
  geom_point()

ggplot(gm, aes(x = gdpPercap_2007, y = lifeExp_2007,
               color = continent, size = pop_2007)) +
  geom_point()


ggplot(gm, aes(x = gdpPercap_2007, y = lifeExp_2007,
               color = continent, size = pop_2007)) +
  geom_point() +
  scale_size_continuous(guide = 'none')


ggplot(gm, aes(x = gdpPercap_2007, y = lifeExp_2007,
               color = continent, size = pop_2007)) +
  geom_point() +
  scale_size_continuous(guide = 'none') +
  theme_minimal() +
  labs(x = 'GDP per Capita', y = 'Life Expectancy',
       title = 'GDPc vs. Life Expectancy 2007',
       color = 'Continent') +
  theme(text = element_text(size = 20))


ggplot(gm, aes(x = log(gdpPercap_2007), y = lifeExp_2007,
               color = continent, size = pop_2007)) +
  geom_point(alpha = 0.5) +
  scale_size_continuous(guide = 'none') +
  theme_minimal() +
  labs(x = 'log(GDP per Capita)', y = 'Life Expectancy', #######################
       title = 'GDPc vs. Life Expectancy 2007',
       color = 'Continent') +
  theme(text = element_text(size = 20)) +
  geom_text(aes(label = ifelse(pop_2007 >= quantile(pop_2007, 0.90),
                               country, # TRUE
                               '')),    # FALSE
            size = 5, hjust = 0, vjust = 0, nudge_x = .1) +
  xlim(NA, 11.7)
# Note: changing figure theme (`theme_minimal()`) must come before `theme()`


##===========================##
## ------------------------- ##
#### 3) "Tidying" the data ####
## ------------------------- ##
##===========================##
# Are the data in a tidy format? If not, what format are they in?
head(gm)
# Wide!
# The variable year is some column names

# install.packages('tidyverse')
library(tidyverse) # ggplot2 is in here
# The dplyr package is used to tidy the data

?pivot_longer
vignette("pivot") # multiple observations per row
names(gm) # Notice the pattern

gm_long =
  pivot_longer(gm, contains('_'),
             names_to = c('.value', 'year'),
             names_sep = '_')
# Tibbles!
# year is a character!!!

##===========================##
## ------------------------- ##
#### 4) Data manipulations ####
## ------------------------- ##
##===========================##

# some more useful dplyr functions!

## ------------------------ ##
#### 4.1) subsetting data ####
## ------------------------ ##
gm_2007 = 
  filter(gm_long, year == 2007)

## --------------------- ##
#### 4.2) sorting data ####
## --------------------- ##
gm_2007 =
  arrange(gm_2007,
          continent, desc(gdpPercap))

## ------------------------- ##
#### 4.3) adding variables ####
## ------------------------- ##
gm_2007 = 
  mutate(gm_2007,
       gdp = gdpPercap*pop,
       year = as.numeric(year)) # overwrite with numeric version of year variables

##------------------------------------ ##
#### 4.4) the powerful pipe operator ####
##------------------------------------ ##
gm %>%
  pivot_longer(contains('_'),
               names_to = c('.value', 'year'),
               names_sep = '_') %>%
  filter(year == 2007) %>%
  arrange(continent, desc(gdpPercap)) %>%
  mutate(gdp = gdpPercap*pop,
         year = as.numeric(year))
# note that we are only outputting in to the console.
# We did not save the object

##================================##
## ------------------------------ ##
#### 5) An illustrative example ####
## ------------------------------ ##
##================================##

# Let's plot a time series of each continents average life expectancy

gm_continents = gm_long %>%
  mutate(year = as.numeric(year)) %>%
  group_by(continent, year) %>%
  summarise(avg_lifeExp = mean(lifeExp),
            max_country_pop = max(pop))

ggplot(gm_continents, aes(x = year, y = avg_lifeExp,
                          color = continent,
                          shape = continent,
                          linetype = continent)) +
  geom_point() +
  geom_line()
# two lines of code!

# I don't recommend doing in one line of code!! 
gm_long %>%
  mutate(year = as.numeric(year)) %>%
  group_by(continent, year) %>%
  summarise(avg_lifeExp = mean(lifeExp),
            max_country_pop = max(pop)) %>%
  ggplot(aes(x = year, y = avg_lifeExp,
                          color = continent,
                          shape = continent,
                          linetype = continent)) +
  geom_point() +
  geom_line()

# Next time: R4DS Ch. 13-16
