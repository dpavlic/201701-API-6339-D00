library(tidyverse)
library(stringr)

# Last Week Exercise -----------------------------------------------------------
# Please change your working directory to where the downloaded earnings_male
# and earnings_female csv files are located.
a <- read_csv('earnings_male.csv')
b <- read_csv('earnings_female.csv')

# We "stack" the male and female rows together.
earn <- bind_rows(a, b) %>%
  # Since there are these duelling male / female identifiers, we'll transform
  # them over to sex. We then use the trick we've used before to detect where
  # the actual field heading is so that we can NA out the row index and then
  # use fill to fill in the heading.
  mutate(
    sex = case_when(male == 1 ~ 'Male', female == 1 ~ 'Female'),
    field = ifelse(str_detect(X1, fixed('field')), X1, NA)
  ) %>%
  select(-X1, -male, -female) %>%
  fill(field) %>%
  # Since the data frame is wide and we would like it long, we can use gather
  # to transform. Note we are specifically saying not to use sex and field
  # columns, but all other columns. It would also be perfectly fine to actually
  # specify which columns you would like. Either is a fine choice.
  gather(year, earnings, -sex, -field) %>%
  # We remove the yr_ prefix and change year to an integer. We also remove the
  # superfulous "field - " part of the field column, and put the whole thing
  # into title case.
  mutate(
    year = as.integer(str_replace(year, fixed('yr_'), '')),
    field = str_replace(field, 'field - ', '') %>%
      str_to_title()
  ) %>%
  # Finally, we remove any NA earnings since we cannot do much with them.
  filter(!is.na(earnings))

# The graphing is pretty straight-forward. We need to summarize the results
# first, however.
earn %>%
  group_by(year, sex, field) %>%
    summarise(earnings = mean(earnings)) %>%
  ungroup() %>%
  ggplot(aes(year, earnings / 1000, colour = sex, group = sex)) +
    geom_line() +
    facet_grid(field ~ .) +
    scale_y_continuous(limits = c(25, 100)) +
    xlab('Year') +
    ylab('Earnings (thousand $)') +
    theme_bw() +
    theme(
      legend.title = element_blank(),
      legend.position = c(0.1, 0.9),
      legend.background = element_blank(),
      legend.key = element_blank()
    )

# Questions
# ifelse + NA ------------------------------------------------------------------

# Say we have a simple vector, with some NAs. Roughly as in our exercise,
# where say 1s are Female, and NAs are (presumably) Male.
x <- c(1, 1, 1, 1, 1, NA, NA, NA)

# You may expect this to work and to give you the right result:
ifelse(x == 1, 'Male', 'Female')

# But it doesn't!
#   [1] "Male" "Male" "Male" "Male" "Male" NA     NA     NA
# The reason is that ifelse evaluates whether a statement is true or false. It
# returns NA when it's neither. Remember NA == 1 is NA, not False. This is
# because NA is something UNKNOWN. Is an unknown number equal to 1? We don't 
# know, so R returns NA (unknown) back.
# So how can we make this work?
ifelse(x == 1 & !is.na(x), 'Male', 'Female')

# Why does this work? x == 1 will still return NA, and !is.na(x) will return
# FALSE. Why does NA & FALSE return FALSE and allows us to properly evaluate
# our expression. Should it not be NA? Well, let's evaluate:
TRUE & TRUE
FALSE & FALSE
FALSE & TRUE
TRUE & NA
FALSE & NA

# Essentially, it is a matter of some philosophy... If something is true and
# also true it is true. But if we have something that is true AND false, that
# means it is false. Let's think of it this way, if I say a fact that's partly
# true and partly false, we don't say it is true. A _PART_ of it is true, but
# the statement is overall false.

# But if something is true and unknown (NA) we can't know if it is
# true or false: it could be either, we don't know. BUT, if something is false
# and not known, it IS false, because whether it is true or not doesn't matter:
# as it is partly false it means it IS false no matter what the unknown value
# holds.

# ifelse and factors -----------------------------------------------------------
fc <- as.factor(c('x', 'y', 'z'))

# Now, this is a factor. So what if we wish to replace z to an NA for example?
ifelse(fc == 'x', NA, y)

# Evaluates to:
#   [1] NA  2  3

# Why? The reason is that a factor is stored as a number internally, with
# extra attribute information denoting factor levels. So, x == 1, y == 2,
# z == 3. 
attributes(fc)

# ifelse essentially drops the attributes in evaluation, only the storage mode
# is kept (i.e., the fact that factors -- and also dates! -- are numeric).
attributes(ifelse(fc == 'x', NA, fc))

# We can use dplyr's if_else to get around but we have to be very specific,
# making sure both the true + false horns evaluate to the same class.
if_else(fc == 'x', fc, factor(NA))

# Note that the attributes are taken from the TRUE factor so the following will
# not work:
if_else(fc != 'x', factor(NA), fc)

# Note that this also makes changing factors annoying, since you have to wrap
# things within factor().
if_else(y == 'x', fc, factor('y'))

# Note that since once again the full factor attributes are taken from the TRUE
# horn, reversing things will not work.
if_else(y != 'x', factor('y'), fc)

# Loops ------------------------------------------------------------------------
# It is a sequence that repeats until a condition is reached... usually when the
# end of the loop is reached. R has two (technically 3, but never mind that)
# variants of a loop, a while loop and a for loop.
# We will cover the for loop here.

# Teaching loops in R is odd -- you should know them, but usually there's a
# better way to do things in R, not least because loops (used carelessly) are
# slow.
# The best use of the loop is where the information is quite clearly repeating
# and there's no vectorized solution to do the job. There are also other
# functional approaches that solve many (though not all) of the problems loop
# solve, but we will not get into that just now.

# From what we have covered in this course, it should be clear that as far as
# data frame maniupulation goes, loops are not USUALLY very interesting as a
# dplyr solution will have our back.

# HOWEVER: there are exceptions. Sometimes, it is actually easier to reason
#          about loops than dplyr solutions. It is also possible to make your
#          loops efficient with some preparation and care. We'll see that in
#          a moment.

# The general rule is repeating yourself twice is fine enough, unless the
# repetition is long and dense, in which case even twice will not do.

# A loop looks something like this. The condition is within the paranthesis
# following the for statement.
for (i in 1:10) {
  print(i)
}

# The braces mark the beginning and the end of the loop. Anything between the
# braces will repeat for an X amount of times. i stores the current value of
# whatever we're looping. So the first iteration of the loop, i will be 1. Then
# 2, then 3 and so on until the loop ends at 10. In this case, what we're doing
# doesn't have much point but nonetheless, the loop will print 1 through 10.

# Here we have a loop that will loop through elements of a vector instead of
# numbers. As we can see, the basic form is exactly the same.
for (val in c('R', 'is', 'fantastic')) {
  print(val)
}

# Loops Extra Knowledge --------------------------------------------------------

# We have not covered this, but it is good to know.
# Remember that data frames in R are just a very particular list of vectors of
# the same size. What this means is that you can loop through vectors of a
# data frame. Or even a data frame itself, with some extra work.
earn_small <- head(a, 100)
for (val in earn_small$yr_2010) {
  print(val)
}

# Here we loop through a data frame with some extra work by using some indexing
# options we learned way back in the first session. Keep in mind that table
# lookups like this are very slow... this is often not what you want to do!
for (row in 1:nrow(earn_small)) {
  print(earn_small[row, c('yr_2010', 'yr_2011')])
}

# Loop Applications ------------------------------------------------------------

# This is all very well and good but what the heck is its purpose?

# For one, we can combine a large amount of data frames into one. While this is
# not generally the preferred way to do it among the members of the R community
# it is probably the easiest way to reason about for newcomes.

# Let's extract all our individual gapminder data countries, and recombine
# together back into one data frame. Extract gapminder.zip. Now make sure
# you can reach it by setting your working directory to where the files are
# located. Make sure there are NO OTHER files in that directory.
files <- dir()
gapminder <- NULL
for (f in files) {
  gapminder <- bind_rows(gapminder, read_csv(f))
}

# Now let us graph the result where y is life expectancy and x is year for
# _each_ country individually. 
# NOTE: An R loop does not return the results without an explicit print,
#       so we need to print the plot in the end. Printing 140+ plots is not
#       something you are likely to do in real life though, and you would
#       probably choose to ggsave the result for each plot.
for (cn in unique(gapminder$country)) {
  gapminder_subsample <- filter(gapminder, country == cn)
  plt <- ggplot(gapminder_subsample, aes(year, lifeExp)) +
    geom_line() +
    scale_y_continuous(limits = c(20, 90)) +
    ggtitle(str_interp('Life Expectancy in ${cn}')) +
    ylab('Life Expectancy (Years)') +
    xlab('Year') +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  print(plt)
}
