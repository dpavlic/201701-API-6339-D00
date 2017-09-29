library(stringr)
library(tidyverse)

# Errata: mutate_each ----------------------------------------------------------
# Chapter 11: mutate_each
# https://campus.datacamp.com/courses/cleaning-data-in-r/1829?ex=11

# The example used is:
#   weather6 <- mutate_each(weather5, funs(as.numeric), CloudCover:WindDirDegrees)

# mutate_each is a legacy function. Use:
#   weather6 <- mutate_at(weather5, vars(ClourCover:WindDirDegrees), as.numeric)

# Advanced Cleaning ------------------------------------------------------------

# We have industry export data in high-tech manufacture. We want to see what the
# exports are between our primary trade partner (US) and other countries.
# But these data are a mess! They come in 3 separate files and have all sorts
# of problems.

# Open up the first set in the list.
url_start <- str_c(
  'https://raw.githubusercontent.com/dpavlic/',
  '201701-API-6339-D00/master/data/',
  'exports_naics_'
)

d1_attempt1 <- read.csv(url(str_interp('${url_start}3341-3342.csv')))

# This is not what we expected at all. This is because the first bits of
# information are garbage. We can fix this up by using the optional skip
# argument. We will also pass the as.is argument so that strings are not
# loaded up as factors. Finally, there's a few country rows with no
# information which are coded as " ". We want to treat those rows as NA.
d1 <- read.csv(
  url(str_interp('${url_start}3341-3342.csv')),
  skip = 8,
  as.is = TRUE,
  na.strings = ' '
)

# Load the other two datasets. This is a bit inefficient... there may be a
# better way to do this down the line.
d2 <- read.csv(
  url(str_interp('${url_start}3343-3344.csv')),
  skip = 8,
  as.is = TRUE,
  na.strings = ' '
)

d3 <- read.csv(
  url(str_interp('${url_start}3345-3346.csv')),
  skip = 8,
  as.is = TRUE,
  na.strings = ' '
)

# We can also immediately collapse this set of data back into one data frame,
# one stacked on top of the other.
exp <- bind_rows(d1, d2, d3)

# This is still quite a mess. Years are wide, and multiple columns are stacked
# together as rows. We also have Total, Sub-Total, and Source rows which just
# serve no purpose at all. How to make sense of this all?
exp2 <- exp %>%
  # We have a returned X column. Let's rename that to something more useful.
  rename(country = X) %>%
  # Remove the empty country rows
  filter(!is.na(country)) %>%
  # Remove Total / Sub-Total, Source rows.
  filter(!str_detect(country, 'Total'), !str_detect(country, 'Source')) %>%
  # What we are doing here is creating a new column called industry. If
  # it contains 'NAICS' then we will copy out the value from the country row,
  # otherwise we will assign an NA value. The whole 'magic' stems from the
  # fact that we now have a column with industry values and can fill down
  # NA values using fill.
  mutate(
    industry = ifelse(str_detect(country, 'NAICS'), country, NA)
  ) %>%
  # As mentioned use fill from tidyr to fill values down (fill can fill either
  # down or up, but default is down).
  fill(industry) %>%
  # Now remove the rows that contain NAICS in the country column. These are
  # useless.
  filter(!str_detect(country, 'NAICS')) %>%
  # Simplify the industry to just the first word of the industry, except
  # where additional clarification is needed.
  # NOTE: this is a bit messy. If you want to be absolutely clear, it may be
  #       worthwhile passing an absolute list of replacements.
  mutate(
    industry = word(industry, 2, sep = '- ') %>%
      word(1) %>%
      str_replace(',', '') %>%
      str_replace('Manufacturing', 'Magnetic_Optical')
  )

# Now we have something we can work with, but we need to clean
# it up a bit since this is not exactly tidy data.
exp_tidy <- exp2 %>%
  gather(year, dollars, X2007:X2016) %>%
  # or...
  #gather(year, dollars, -country, -industry) %>%
  mutate(year = as.integer(str_replace(year, 'X', '')))

# This is it. But what if you want the industry values
# to be their own columns?
exp_wider <- exp_tidy %>%
  mutate(industry = tolower(industry)) %>%
  spread(industry, dollars)

# Back to our question. What is the % of trade in each of our examples between
# the US and the rest of the world.
us_percent <- exp_tidy %>%
  # Add a trade partner (US / NON-US)
  mutate(
    trade_partner = ifelse(country == 'United States', 'us', 'non_us')
  ) %>%
  # Calculate the totals.
  group_by(year, industry, trade_partner) %>%
    summarise(total = sum(dollars, na.rm = TRUE)) %>%
  ungroup() %>%
  # Spread US / NON-US into its own columns.
  spread(trade_partner, total) %>%
  # Calculate the proportion of US trade.
  mutate(us_trade = round(us / (non_us + us), 2)) %>%
  # Remove us, non_us columns, we've calculate the percentage.
  select(-us, -non_us) %>%
  # Spread each industry as a column.
  mutate(industry = tolower(industry)) %>%
  spread(industry, us_trade)
