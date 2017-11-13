library(magrittr)
library(tidyverse)
library(stringr)

# Conditionals -----------------------------------------------------------------

# We start with a concept not entirely unfamiliar. The if condition. We have
# seen if variants already in mutate(), notably the ifelse, and the case_when.
# if functions around a similar principle.

# For example, let's check if a number is a multiple of 3. For example, is 2
# a multiple of 3? Let's check our grade 4 math. 3 goes into 2 zero times,
# giving us a remainder of 2.

4 %% 3

# But how can we say, if a number is a multiple of 3, print that a number is
# a multiple of 3.

if (4 %% 3 == 0) {
  print('Number is a multiple of 3.')
}

# This will return NOTHING, since 2 is not a multiple of 3. Maybe we want to
# add something to print if the number is not a multiple of 3.
if (4 %% 3 == 0) {
  print('Number is a multiple of 3.')
} else {
  print('Number is not a multiple of 3.')
}

# What if we want to check if a number is a multiple of 3, and if it is not,
# THEN check if it is a multiple of 2, then if it's neither, print it's not
# a multiple of either 2 or 3.
if (4 %% 3 == 0) {
  print('Number is a multiple of 3.')
} else if (4 %% 2 == 0) {
  print('Number is not a multiple of 3, but is a multiple of 2.')
} else {
  print('Number is not a multiple of 3 or 2.')
}

# This is all well and good, but we manually enter a number here. Let's say
# we go back to the original problem, and try to find out if the number is a
# multiple of 3? But we want to find the answer for any arbitrary number we
# input?

# Functions --------------------------------------------------------------------

# The answer to the quandary in the previous section is: FUNCTIONS. You know
# these things because you've been using them all along in R. Functions produce
# an output for a given set of inputs.

# Almost everything in R is a function. You know those operators you're using?
# Just sneakily hidden functions.
`+`(3, 2)
`*`(5, 10)
`:`(1, 5)

# Extra examples...
example <- c('a', 'b', 'c')
example[1]
`[`(example, 1)
`<-`(some_var, 'hello')
some_var

# The last line is returned, which also happens to be the first and only line
# of this function. The last evaluated line in R is automatically
# returned, or you can manually use the return function.
add_two_numbers <- function(x, y) {
  x + y
}

# OR, if this helps you:
add_two_numbers <- function(x, y) {
  return(x + y)
}

# Beware that in addition to a pure function, there is also an impure function.
# This function produces side effects and may not always produce the same set
# of outputs for a given set of inputs.
add_numbers_badly <- function(x, y) {
  x + y + z
}

add_numbers_badly(1, 2)

z <- 1

add_numbers_badly(1, 2)

z <- 4

add_numbers_badly(1, 2)

# Unless there's a very good reason, avoid writing impure functions. They
# introduce a rich source of bugs, and lots of potential head banging.

# Note that variables in the global scope can come inside the function, but
# this doesn't work the other way around.
rm(z)
scope_function <- function(x, y) {
  z <- 3
  x + y + 1
}

scope_function(1, 2)

# but:
z
# object 'y' not found. You defined it in the function but it does not leak.

# Let's go back to our multiple example. We're going to create a new function
# is_multiple, which requires a number. In addition, we will create an OPTIONAL
# parameter, multiple, which specifies what multiple a number should be of,
# with a DEFAULT value of 2. This argument is therefore NOT required, and if
# not entered will default to 2. If a number is a multiple, it will return
# the number back, otherwise it will return a NA value.
is_multiple <- function(x, multiple = 2) {
  if (x %% multiple == 0) {
    x
  } else {
    NA
  }
}

is_multiple(2)
is_multiple(3)

# To change the multiple, you can do any of the following.
# If the optional argument name is not specified, arguments proceed by the
# order of the position the parameters are defined in.
is_multiple(3, multiple = 3)
is_multiple(multiple = 3, 3)
is_multiple(3, 3)

# Practical application --------------------------------------------------------
# What is all this good for? Functions are all over the place in R. They are
# very, very powerful. It's good to have an understanding of the fact that you
# have been using functions all this time in R. But it can also allow you to
# use a function to modify the default behaviour of another function.

# Let's start with a very basic example. Say we want to sum over a bunch of
# numbers... but some have NAs in them. But we don't care about NAs, we WANT
# them ignored.
nums <- c(1, 5, NA, 10, 130, 40, 11, NA)

sum(nums)

# will not work. We can of course do:
sum(nums, na.rm = TRUE)

# But what if you wanted to have a sum that automatically removes NAs over and
# over again, but typing na.rm = TRUE all the time is annoying if you want that
# as a default. We can wrap our own sum2 function around that.
sum2 <- function(values) {
  sum(values, na.rm = TRUE)
}

sum2(nums)

# Let's go way back to trade data. Download all the trade data we have used
# in the course and set your path to point to the correct directory.
file_path1 <- 'exports_naics_3341-3342.csv'
file_path2 <- 'exports_naics_3343-3344.csv'
file_path3 <- 'exports_naics_3345-3346.csv'

d1 <- read.csv(file_path1, skip = 8,  as.is = TRUE,  na.strings = ' ')
d2 <- read.csv(file_path2, skip = 8,  as.is = TRUE,  na.strings = ' ')
d3 <- read.csv(file_path3, skip = 8,  as.is = TRUE,  na.strings = ' ')

# We have to do this 3 times and every time for any trade data we get from the
# government, but we will always use arguments skip = 8, as.is = TRUE, and
# na.strings = ' '. Well we can define our own function. For example:
read_trade <- function(file_path) {
  read.csv(file_path, skip = 8, as.is = TRUE, na.strings = ' ')
}

# Then we can just do:
d1 <- read_trade(file_path1)
d2 <- read_trade(file_path2)
d3 <- read_trade(file_path3)
