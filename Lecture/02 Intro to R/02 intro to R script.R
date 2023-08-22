# Set up:
# - Set theme to Textmate
# - close all scripts
# - Update R
#
#
# ---------------------------------------------------------
# [Tools > Global options.. > appearance]
# [Quality of life: General > save workspace as .RData on exit => Never]
#                 [Code > display > highlight R function calls]
#                                   [rainbow parenthesis]

# [R is just a fancy caluclator]
# [OPEN UP CALCULATOR]
# [IN CONSOLE]
1 + 1
15/3
2^3

# [order of operations apply]
(3 + 4)*5

# [Scientific notation]
1e3

# [Want to save our hard work, so let's open up a script]
#
# [+ icon > R script OR Ctrl+Shift+N]
#
# ---------------------------------------------------------------------------- #

# In scripts, we can leave comments for ourselves like so
# The keyboard shortcut for new scripts is Ctrl-Shift-N

# Let's create an objects named `a`
a = 7
# to run this line of code, we either can click the `Run` button on the top right
# or use the keyboard shortcut Ctrl-Enter

# We can perform math on this object and have it output to the console
a*2

# or we can perform math on multiple objects
b = 8
a*b

# if we want to call this value later on, we can save it as an object
c = a*b
c

# object names must begin with any letter and may contain numbers and `.` or `_`
Name4_1WeiRd0.ExaMple = a + b

# if you want spaces or special characters, the name must be wrapped in back ticks ``
`50 + c = ?` = 50 + c

# You may also see objects being created with `<-` instead of `=`
d <- Name4_1WeiRd0.ExaMple/2

# this permits us to:
the <- same <- value <- 474

# You can create sections in your script like so

# section 1 ----
# Section 2 ====
# Section 3 ####
# Take a look at the bottom left of the script window

##=====================================##
## ----------------------------------- ##
#### 1) Different classes of objects ####
## ----------------------------------- ##
##=====================================##

# In R, floats are labelled "numeric"
class(2)
class(2.1) # float

# You can obtain integers with a capital L
class(2L)
class(2.1L) # our first warning message! What does it say?

# You can have non-numerical text
"Econometrics"
# These are strings and are labelled "character" in R
class("Econometrics")

# You can wrap strings with single quotes
'Econometrics'
# Which permits
'"Hello, world!", says R.'

# Some other useful numbers to know:
class(Inf) # infinity
class(NaN) # Not a Number

# A particularly useful object used in subsetting data are booleans
1 == 1
class(1 == 1)
# In R, booleans are labelled "logical"
class(TRUE)

# Note booleans are actually numbers
TRUE == 1
FALSE == 0
TRUE*2

# something you will come across with real world data is the NA
NA
class(NA)
# it stands for Not Available and is an indicator for a missing value

# there is the NULL or an object with nothing in it
NULL
class(NULL)
e = NULL
# NULL objects are useful when creating placeholder objects

# Functions are the name of the game in coding
# for example, `class()` is a function
class(class)

# Here we are going to create a few VERY basic functions
divide = function(argument_1, argument_2){
  argument_1 / argument_2
}
# argument need to be set to values
divide(argument_1 = 3, argument_2 = 4)
# But you need not specify the argument names if your values are in order
divide(3, 4)
divide(argument_2 = 3, 4)

# arguments can be called anything
divide = function(x, y){
  x / y
}
divide(3, 4)

##=========================##
## ----------------------- ##
#### 2) Types of objects ####
## ----------------------- ##
##=========================##

## ---------------- ##
#### 2.1) Vectors ####
## ---------------- ##

# It can be useful to store multiple values in a vector
# To do these, we use the `c()` function, where c stands for concatenate
vec = c(42, 474, pi)

# YOu can call specific elements with square brackets
vec[2]

# You can perform elementwise arithmatic
vec + 10

# Or you can append existing vectors
c(vec, 10)

# But note that if one element of the vector is a string, all values are
# coerced to be strings
c('econ', 490) 

## ----------------- ##
#### 2.2) Matrices ####
## ----------------- ##
# Matrices - let's create a two by two matrix
# Create a two by two matrix with rows 1, 2 and 3, 4
matrix(1,2
       3,4)
# Our first error! Let's try this
matrix(c(1,2,
         3,4))
# Well that didn't do what we want
# All built in functions have help pages. 
# We can access them like so (Step 2 of Coding Assistance Checklist)
?matrix

matrix(c(1,2,3,4), nrow = 2, ncol = 2)
# matrix(c(1,2,3,4), nrow = 2, byrow = TRUE)
m = matrix(c(1,2,3,4), nrow = 2, byrow = TRUE)

## -------------------- ##
#### 2.3) data.frames ####
## -------------------- ##
# How your data will appear

data.frame(m) # NAMED ROWS!!!!!
df = data.frame(m)
df$X1         # woah!

# Let's create a bigger data.frame
set.seed(474)
ex_df = data.frame(states = seq(1,50),
                   year = 2023,
                   share = runif(50))

# Calling a variable
ex_df$states
ex_df$country = 'USA'

# Taking a look at the data
head(ex_df)
head(ex_df, 1)
tail(ex_df)
summary(ex_df)

##=========================##
## ----------------------- ##
#### 3) Logic Statements ####
## ----------------------- ##
##=========================##

# Logic statements are incredibly useful for subsetting data
ex_df$share <= 0.2
ex_df[ex_df$share <= 0.2, ]
ex_df$share[ex_df$states > 45]

# other examples
1 == 3
1 != 3
2 >  2
2 >= 2
1 %in% c(1,2,3)
!1 %in% c(1,2,3)

# joint statements
1 > 3 & 1 < 4
TRUE & TRUE
TRUE & FALSE
FALSE & FALSE

1 > 3 | 1 < 4 # inclusive 'or': okay if both conditions are true
TRUE | TRUE
TRUE | FALSE
FALSE | FALSE


##==============##
## ------------ ##
#### 4) Loops ####
## ------------ ##
##==============##

# Loops are useful for repetitive tasks such as reading and combining many
# data sets. 


10/3
10 %/% 3 # integer division
10 %% 3 # modulus (remainder)
?"+"

for(i in c(1, 3, 6:10)){
  if(i %% 2 != 0){
    print(paste(i, 'is odd'))
  } else if(i %% 10 == 0) {
    print(paste(i, 'is multiple of 10'))
  } # else, do nothing
}


# However, loops are computationally inefficient compared to vectorized operations
n = 1e5
x = runif(n)

system.time({y = 2*x})

system.time(
  {
    y = NULL
    for(i in 1:length(x)){
      y = c(y, x[i]*2)
    }
  }
)


# Reading: 
# Practice - R4DS Ch. 4, 6, 21.1.1 - 21.3.5
# Next lecture - R4DS Ch. 7, 10 - 11.2, 12.1 - 12.3, 18.1 - 18.3

