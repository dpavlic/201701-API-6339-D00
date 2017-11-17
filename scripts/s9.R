library(tidyverse)
library(stringr)

# Joining Tables ---------------------------------------------------------------

# This is a deep subject. We know how to 'stack' the rows on the bottom via
# bind rows. But... how do we join columns of two datasets together based on
# a common key. Let's say we have:

# ID A B               ID C D
# 1  a 2               1  Z 4
# 2  v 3               2  A 5
# 3  q 4               3  P 1

# and based on the common ID, we wanted:
# ID A B C D
# 1  a 2 Z 4
# 2  v 3 A 5
# 3  q 4 P 1

# A more concrete example.

chars <- tibble(
  id = 1:5,
  sex = c('F', 'M', 'F', 'M', 'F'),
  age = c(22, 25, 29, 30, 40)
)

address <- tibble(
  id = 1:5,
  region = c(1, 2, 1, 3, 8),
  postal_code = c('K', 'N', 'A', 'M', 'D')
)

unemp <- tibble(
  region = c(1:6),
  unemp_rate = c(.05, .07, .03, .09, .1, .04)
)

# What if we wanted the unemployment rate for all of the people in the chars
# df? We need to do two things, join 'region' from addresses to chars df, then
# join region df to get the unemployment rate.

# left_join is probably the most common join and the one we will focus on. It
# joins information from the right table to the left table based on a key.
# what are the keys? What is the key which links chars <-> address and
# then what is the key that links address <-> region?
chars_address <- left_join(chars, address, by = 'id')
chars_address

# Now to join this to region to get unemployment rate.
all <- left_join(chars_address, unemp, by = 'region')
all

# Anything interesting happen? Well, notice the unemp_rate is NA for the 5th
# observation. This is because region 8 is not in the region data frame. This
# is why this is a left join. All the information from the left table comes
# along for the ride along with information from the right table that matches.
# Where it does not match, you get an NA. That's how an inner_join is
# different.
all_inner <- inner_join(chars_address, unemp, by = 'region')

# How is this different? It is very similar, BUT there has to be a match on
# both the left and right tables. If there is not a match in one of the tables,
# the observation is dropped.

# Think... How else might we go about these join operations, to get unemployment
# information? Is there another order we could have done?

# Full joins are the final join we'll cover, since pretty much everything else
# is a riff on these three.
address
unemp
full_add_unemp <- full_join(address, unemp, by = 'region')
full_add_unemp

# The information from BOTH left and right table comes along for the ride, and
# any missing information becomes NA on both sides.

# One final, and important note about joins. You may get more than the original
# number of rows when joining! For example...
address2 <- tibble(
  id = c(1, 1, 2, 3, 4, 5),
  region = c(2, 1, 2, 1, 3, 8),
  postal_code = c('N', 'K', 'N', 'A', 'M', 'D')
)
chars

chars_address2 <- left_join(chars, address2, by = 'id')
chars_address2

# We had 5 observations in the original chars data, but now we have 6 (!)
# because the address2 has 2 observations for id 1, one where the
# region is 2 and 1, and postal_code is N and K, respectively. This type of
# situation is very common in the real world, and in case you want a 'flat'
# one observation per person in the final data (which is fairly often the case),
# you will need to figure out how to pare down this information. For example,
# assume there was a year field in address, which indicated the year a person
# was recorded at that address. Take the most recent year.
address3 <- tibble(
  id = c(1, 1, 2, 3, 4, 5),
  region = c(2, 1, 2, 1, 3, 8),
  postal_code = c('N', 'K', 'N', 'A', 'M', 'D'),
  year = c(2016, 2017, 2017, 2017, 2017, 2017)
)

chars_address3 <- left_join(
  chars,
  address3 %>%
    group_by(id) %>%
      filter(year == max(year)) %>%
    ungroup()
  by = 'id'
)

# Mapping ----------------------------------------------------------------------
# Mapping over something is in essence applying the same function to a list or
# a vector.

# Say we have a following vector. How do we add 1 to all elements of a vector?
a <- c(1, 4, 9, 15, 100, 98)

# Well the adding operation in R is vectorized, so:
a + 1

# It will do what you want. But what if an operation like that wasn't
# vectorized? Indeed, a lot of things you want to do to a vector or a list are
# NOT vectorized. Well, we could create a new vector, then loop to get the
# addition. append kind of serves the same function as our bind_rows example
# from weeks before. Create a new vector b, then keep appending to it.
b <- NULL
for (i in a) {
  b <- append(b, i + 1)
}

# This has two main problems. For one, it is SLOW (it can be sped up by doing
# something known as preallocation, but it is still not ideal). It also seems
# confusing. Is there not a better way? How about creating a function that
# has one parameter and adds 1 to it.
add_one <- function(x) {
  x + 1
}

# _dbl stands for double, it is the default numeric class in R (there are
# also integers, but not to worry about that right now). This reads as:
# for EACH element in vector a, apply the function add_one to it. The "x"
# in that function is substituted for each number in a.
map_dbl(a, add_one)

# Ok, great. What is that _dbl doing there anyway? Well, it is there to say
# that we expect the output to be numeric. What if we use map_chr,
# which says: apply the function and then make sure this is a character
# (string) vector.
map_chr(a, add_one)

# And that's really all there is to it! Any function which is not vectorized,
# you can vectorize yourself. Addition is obviously vectorized, but let's think
# of an actual unvectorized operation. Let's introduce our previous example.
# Find out if something is a multiple of 2. If it is, return the number,
# otherwise return the NA.
multiple_of <- function(x, multiple = 2) {
  if (x %% multiple == 0) {
    x
  } else {
    NA
  }
}

# Well, this works no problem like this:
multiple_of(6)
multiple_of(7)

# But the function we wrote accepts one number. Not a vector of numbers. We
# could rewrite this function to be vectorized but this is  not always
# possible. What is an alternative approach?
multiple_of(a)

# Well, once again, we can use map to apply that function to EVERY element of
# the vector.
map_dbl(a, multiple_of)

# Note we can pass extra parameters of the function.
map_dbl(a, multiple_of, multiple = 3)

# Which map you use depends on what you want returned:
# map_dbl = returns a vector of numbers
# map_lgl = returns a vector of booleans (TRUE / FALSE)
# map = will always return a list

# Here's map that returns a list.
map(a, multiple_of, multiple = 3)

# Remember, lists are a bag of anything in R, so when you want to
# map something that isn't necessarily just one thing. Like dataframes... a
# list of dataframes.

# Let's go once more to our trade files. Make sure you are in the correct
# working directory. We will make things a bit more sophisticated by extracting
# a dir pattern this time, so the exports files do _NOT_ need to be the only
# files in the directory.
file_paths <- dir('.', 'exports', full.names = TRUE)

# Here is our read trade function from last week.
read_trade <- function(file_path) {
  read.csv(file_path, skip = 8, as.is = TRUE, na.strings = ' ')
}

# We have used loops to read the files before.
trade <- NULL
for (f in file_paths) {
  trade <- bind_rows(trade, read_trade(f))
}

# bind_rows() however can take a LIST of dataframes and bind them all together,
# so how would we do this using map?
trade <- map(file_paths, read_trade) %>%
  bind_rows()

# OR, an equivalent:
trade <- bind_rows(map(file_paths, read_trade))

# This is a common enough operation that you actually get a shortcut:
# map_dfr, but you do not need to worry too much about that.
trade <- map_dfr(file_paths, read_trade)

# Final note: note that you do not need to use an existing (named) function in
# a map call. You can define a function inline without giving it a name. This
# is called an anonymous function, and useful when you're sure you will not
# need a function again except for this time in program execution. Note you can
# omit curly braces when your function is just one expression.
trade <- map(
  file_paths,
  function(x) read.csv(x, skip = 8, as.is = TRUE, na.strings = ' ')
) %>%
  bind_rows()

# Note, since our anonymous function is just wrapping over an existing read.csv
# function, we can just pass the extra arguments like so:
trade <- map(
  file_paths,
  read.csv, skip = 8, as.is = TRUE, na.strings = ' '
) %>%
  bind_rows()

# EXTRA KNOWLEDGE
# The purrr functional library that enables all of this also has some shortcuts
# for defining an anonymous function. You can do a tilde sign to define an
# anonymous function, and the argument for the function becomes .x.
trade <- map(
  file_paths, ~ read.csv(.x, skip = 8, as.is = TRUE, na.strings = ' ')
) %>%
  bind_rows()

# That is the same as:
trade <- map(
  file_paths,
  function(.x) read.csv(.x, skip = 8, as.is = TRUE, na.strings = ' ')
) %>%
  bind_rows()
