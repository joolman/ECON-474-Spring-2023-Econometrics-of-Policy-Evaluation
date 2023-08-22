# In scripts, we can leave comments for ourselves
# the keyboard shortcut for new scripts is Ctrl-Shift-N

# Let's creat an object called `a`
a = 7

# we can perform math on this object and have it output into
# the console
a*2

# or we can perform math on multiple objects
b = 8
a*b

# if we, want we can save this value for later
c = a*b

# object names must begin with any letter and may contain
# numbers and "." or "_"
Name4_oNe_Weidr0.ExaMple = a + b

# You can create objects with names that contain special
# characters
`50 + c = ?` = 50 + c

# You can also create objects using `<-` instead of `=`
d <- Name4_oNe_Weidr0.ExaMple

# this permits us to:
the <- same <- value <- 474

# You can create sections in your scripts like so

# section 1 ----
# Julian Two ====
# Section 3 ####

##=====================================##
## ----------------------------------- ##
#### 1) Different Classes of Objects ####
## ----------------------------------- ##
##=====================================##

# In R, floats are labelled "nuermic"
class(2)
class(2.1)

# You can obtain integers with a capital L after the number
class(2L)
class(2.1L)

# You can have non-numerical text
"Econometrics"
# These are strings and are labelled "character" in R
class("Econometrics")

'"Hello, World!" says R.'

# Some useful numbers to know:
class(Inf) # Infinity
class(NaN) # Not a Number

# A useful object in R for subsetting data are booleans
1 == 1
class(1 == 1)
class(TRUE)

# Note: booleans are actually numbers
TRUE * 10
FALSE * 1e7
TRUE == 1
FALSE == 0
T
F

# Something you will come across (a lot) in the real world
# is the NA
NA
class(NA)
# It stands for Not Available and is an indicator for missing
# values

# There is the NULL object (nothing in it/empty)
NULL
class(NULL)
e = NULL
# NULL objects are REEEEEEALLY useful when creating placeholder
# objects, such as loading in many data sets

# Functions are the name of the game in coding
class(class)

# Here we are going to create a few VERY basic functions
divide = function(argument_1, argument_2){
  argument_1 / argument_2
}
divide(3, 4)
divide(argument_2 = 3, 4)

# Arguments can be called anything
divide = function(Julian, purple){
  Julian / purple
}

divide(3, 4)

##=========================##
## ----------------------- ##
#### 2) Types of Objects ####
## ----------------------- ##
##=========================##

## ---------------- ##
#### 2.1) Vectors ####
## ---------------- ##

# It can be useful to store multiple values in a vector
# To do this, we use the `c()`, which stands for
# concatenate (combine)
vec = c(42, 474, pi)

# You can select specific elements with square brackets
vec[2]

# You can perform element-wise arithmatic
vec + 10

# OR! you can append a new value!
c(vec, 10)

# But note that if on element of the vector is a string,
# then all values are coerced to be strings
c('ECON', 474)


## ----------------- ##
#### 2.2) Matrices ####
## ----------------- ##
# Matrices - let's create a 2x2 matrix with the rows
# 1, 2
# 3, 4
matrix(1, 2, 
       3, 4)
# let's try this
matrix(c(1, 2,
         3, 4))
?matrix
matrix(c(1, 2,
         3, 4), nrow = 2, ncol = 2)
m = matrix(c(1, 2, 3, 4),
           nrow = 2, byrow = TRUE)

## ----------------------- ##
#### 2.3) data frames!!! ####
## ----------------------- ##
# How your data will appear

df = data.frame(m)
df$X1 # Woah! Selected a column!

# Let's create a bigger `data.frame()`
set.seed(474)
ex_df = data.frame(states = 1:50,
                   year = 2023,
                   share = runif(50))
# Calling a variable
ex_df$states

# Add a new variable
ex_df$country = 'USA'

# Take a look at the data
head(ex_df)
tail(ex_df, 2)
summary(ex_df)

##=========================##
## ----------------------- ##
#### 3) Logic Statements ####
## ----------------------- ##
##=========================##

# Logic statements are incredibly useful for subsetting
# data
ex_df$share <= 0.2
ex_df[3, 2] # third row, second column
ex_df[3, ] # third row, all columns
ex_df[c(3, 20:25), 1:3] # rows 3 and 20-25 and columns 1-3
# i can row, but j cannot

ex_df[ex_df$share <= 0.2, ]

# other example
1 == 3
1 != 3
2 > 2
2 >= 2
1 %in% c(1, 3:5)
!1 %in% c(1, 3:5)

# Joint logic statements
1 > 3 & 1 < 4
TRUE & TRUE
TRUE & FALSE
FALSE & FALSE

# "or" logic statement
1 > 3 | 1 < 4
TRUE | TRUE
TRUE | FALSE
FALSE | FALSE

##==============##
## ------------ ##
#### 4) Loops ####
## ------------ ##
##==============##

# Loops are useful for repetitive tasks such as reading in
# and combining many data sets.

10/3
10 %/% 3 # integer division
10 %% 3 # modulus (remainder)
?"+"

for(i in c(1, 3, 6:10)){
  if(i %% 2 != 0){
    print(paste(i, "is odd"))
  } else if(i %% 10 == 0){
    print(paste(i, 'is a multiple of 10'))
  } # else, do nothing
}

# However, loops are computationally inefficient compared to
# vectorizszszed operations
n = 1e5
x = runif(n)

# vectorized
system.time({y = 2*x})

# Julian's hyper inefficient for loop
system.time(
  {
    y = NULL
    for(i in 1:length(x)){
      y = c(y, 2*x[i])
    }
  }
)

# Reading:
# Practice - R4DS Ch. 4, 6, 21.1.1 - 21.3.5
# Next lecture - R4DS Ch.7, 10-11.2, 12.1 - 12.3, 18.1 - 18.3




