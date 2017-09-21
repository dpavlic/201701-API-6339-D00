# Libraries & Data ------------------------------------------------------------
library(gapminder)

# Although gapminder is loaded, it is 'hidden' from view at
# the moment.
# Remember: we can use the data() function to show us what
#           datasets are available to us.
gap <- gapminder

# Base R Basics --------------------------------------------------------------

# We can use == to give us a boolean (TRUE / FALSE) of where the selection is
# true and where it is false.
# Remember: == is IMPORTANT. What below does is it sets the column continent
#           in the gap dataset as ALL 1. NOT WHAT WE WANTED!
# gap$continent = 1

# The example below will return TRUE for each observation in the gap data
# where continent is equal to Africa, and FALSE for each observation that is
# not.
gap$continent == 'Africa'

# Remember: we select a subset of rows or columns in base R by writing the
#           name of our dataset enclosed in square brackets. For a data frame,
#           you provide which rows you want to choose. You select columns you
#           want to choose after a comma. You MUST do both, so if you want to
#           select all columns you have a trailing comma. For example:
#           gap[1:5, ] selects rows 1 through 5 and ALL columns.

# We can use the strategy of returned TRUE / FALSE booleans to subset data.
# In the example below, any row which returns TRUE for the condition we have
# outlined (i.e., continent in the gap dataset is equal to Africa) will be
# stored in the variable gap_africa.
gap_africa <- gap[gap$continent == 'Africa', ]

# Here another condition is added: Look at only rows where the year column
# of gap_africa dataset comes after 1980. When dealing with numeric columns
# basic principles of math apply. We can use the >, <, >=, <= operators for
# greater than, less than, and greater or equal than, or less than or equals
# operations.
gap_africa_after80 <- gap_africa[gap_africa$year > 1980, ]

# We can also do != which means NOT EQUALS. The example below returns
# all rows where continent in the gap dataset is not equal to Africa.
gap_not_africa <- gap[gap$continent != 'Africa', ]

# We can do the selection of of the continent of Africa AFTER 1980, as we
# have done above, at the same time. We can use the & (AND) operator to do
# this. This example is identical to our gap_africa_after80 dataset we have
# done above, we just do it all in one go: subset our gap dataset where the
# continent is Africa _AND_ the year is after 1980.
gap_africa_after80_2 <- gap[gap$continent == 'Africa' & gap$year > 1980, ]

# In addition to the & operator there is also the | (OR) operator. Below
# tells us: select all the rows in the gap dataset where the continent is
# Africa OR the year is after 1980. In other words, if the continent is
# anything but Africa (e.g., Europe) it will be returned as well but only
# for observations AFTER 1980.
gap_africa_or_after80 <- gap[gap$continent == 'Africa' | gap$year > 1980, ]

# Here we create a 'toy' dataset which returns only the rows in the gap
# dataset for 1952. We will then run head() to only pick up a few observations
# for it.
gap1952 <- gap[gap$year == 1952, ]
head_gap1952 <- head(gap1952)

# What happens if you want to compare every row of your data set with a vector
# of possible values? In other words, say you want to see whether the
# continent row is ANY of Asia, Europe or Africa?

# This will NOT DO WHAT YOU THINK IT DOES
head_gap1952$continent == c('Asia', 'Europe', 'Africa')

# Why doesn't this work? Well, our small toy dataset has these values for the
# continent:
# Asia Europe Africa Africa Americas Oceania
#
# Our comparison is:
# Asia Europe Africa
#
# What it thinks you're doing is you want to compare each of the values in
# sequence: first value of continent to the first value of our comparison,
# second value of continent to the second value of comparison, and so on.
# But why does this work and not throw an error? There is only three values
# in our comparison, but 6 in continent. Because R _RECYCLES_ values, starting
# from first to last again. So the comparison expands to:
#
# Asia Europe Africa Asia Europe Africa

# So how do we actually do what we wanted all along? To compare each row
# for continent for any of Asia Europe or Africa? Use the %in% operator:
head_gap1952$continent %in% c('Asia', 'Europe', 'Africa')

# Adding columns to a data frame

# Below will add a pop2 column to our data frame. Here just as a demonstration
# we take original population in our gap data frame and multiply by 2. The
# result will be stored in the pop2 column of that data frame.
gap$pop2 <- gap$pop * 2

# dplyr basics ---------------------------------------------------------------
library(dplyr)

# Let's reset gap to the original gapminder dataset.
gap <- gapminder

# To choose only certain rows based on the condition, we use dplyr's filter
# function. As with any dplyr function, the first argument to filter is the
# dataset you want to operate on. This is followed by the conditions on which
# to filter on. In the example below, we do the same thing we did with base R
# and choose only the rows in the gap dataset where the continent is Africa.
gap_africa <- filter(gap, continent == 'Africa')

# And here as before, we choose only the rows in the gap dataset where
# the continent is Africa and year is after 1980.
gap_africa1980 <- filter(
  gap, continent == 'Africa' & year > 1980
)

# To choose only certain columns with dplyr, use the select function. As before
# the first argument is the name of the dataset, followed by columns you want
# to select.
gap_somecols <- select(gap, country, continent, year)

# You can also use - to choose which columns NOT to select. This will return
# everything EXCEPT the country and continent columns
gap_somecols2 <- select(gap, -country, -continent)

# To add columns with dplyr you use the mutate function. Once again, the first
# argument is the dataset we want to operate on (add a column to), followed by
# one or more columns we want to add. The example below adds a column pop2 to
# the dataset.
gap2 <- mutate(gap, pop2 = pop * 2)

# The pipe operator allows us to chain dplyr operations together. It looks
# like this %>% and the rule of the pipe operator is this: the object on the
# left-hand side (for our purporses, the dataset on the left-hand side) is
# passed as the FIRST argument of the function of the right-hand side. That
# is why all dplyr functions ask for data as the first argument, so we can
# efficiently chain operations together.

# This is how it looks without the pipe operator
gap_africa <- filter(gap, continent == 'Africa')

# And with the pipe operator:
gap_africa <- gap %>% filter(continent == 'Africa')

# Of course, in that example, there is no advantage to the pipe. The
# advantage starts to make itself more apparent when we need to do several
# operations at once. Here, we take the original gap dataset, choose only
# the rows where continent is Africa, select the pop and continent columns
# and finally create a new column pop2, all in one 'go'.
gap_pipe <- gap %>%
  filter(continent == 'Africa') %>%
  select(pop, continent) %>%
  mutate(pop2 = pop * 2)
