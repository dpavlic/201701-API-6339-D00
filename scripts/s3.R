library(magrittr)
library(dplyr)
library(gapminder)
library(tidyr)

# dplyr review (1) -----------------------------------------------------------
gap <- gapminder

# Calculate gdp for each observation. How about we filter to 2007? Summarize
# the results for the whole dataset.
gap %>%
  mutate(gdp = gdpPercap * pop / 1000000000) %>%
  filter(year == 2007) %>%
  summarise(
    world_gdp = sum(gdp),
    min = min(gdp),
    max = max(gdp),
    avg = mean(gdp)
  )

# Let's do the same thing again. This time, let us return the values
# by continent.
gap %>%
  mutate(gdp = gdpPercap * pop / 1000000000) %>%
  filter(year == 2007) %>%
  group_by(continent) %>%
    summarise(
      cont_gdp = sum(gdp),
      min = min(gdp),
      max = max(gdp),
      avg = mean(gdp)
    )

# dplyr review (2) -----------------------------------------------------------
# We want to compare life expectencies of 4 countries closest to the GDP per
# capita of Canada in 2007. However, we will plot the historical life
# life expectancy of these countries.
gdp_can <- gap %>%
  filter(year == 2007, country == 'Canada') %>%
  # pull is merely a convenience to 'grab' a column as a vector instead of
  # doing that as a separate step.
  pull(gdpPercap)

gdp_close <- gap %>%
  filter(year == 2007) %>%
  # abs computes absolute value. e.g.: abs(-1) == 1
  mutate(gdp_distance = abs(gdp_can - gdpPercap)) %>%
  # Arranges values from lowest to highest in a dataset, or A-Z.
  arrange(gdp_distance) %>%
  # A dplyr alternative to [1:5, ]. We are doing 5 and not 4 because Canada
  # will be one of the results.
  slice(1:5) %>%
  pull(country)

# Note %$% which allows us to pipe the 'environment' so we can just type in
# variable names withot storing this temporary dataset into a variable, for
# example var and then using var$year and var$lifeExp
# NOTE for later: there are inefficiencies in this code...
# perhaps there is a way to make this cleaner?
gap %>% filter(country == gdp_close[1]) %$%
  plot(year, lifeExp, 'l', col = 'red', ylim = c(65, 90))
gap %>% filter(country == gdp_close[2]) %$%
  lines(year, lifeExp, 'l', col = 'blue', ylim = c(65, 90))
gap %>% filter(country == gdp_close[3]) %$%
  lines(year, lifeExp, 'l', col = 'green', ylim = c(65, 90))
gap %>% filter(country == gdp_close[4]) %$%
  lines(year, lifeExp, 'l', col = 'gray', ylim = c(65, 90))
gap %>% filter(country == gdp_close[5]) %$%
  lines(year, lifeExp, 'l', col = 'black', ylim = c(65, 90))
legend('topleft', legend = gdp_close,
       pch = 19,
       pt.cex = 1,
       col = c('red', 'blue', 'green', 'gray', 'black'))

# NA Values ------------------------------------------------------------------
# Full URL to SLID. We'll cover very quickly what the SLID is later, but for
# now, just note that we have a new value which crops up when we open up
# this dataset. NAs.
slid <- read.csv(url(
  'https://raw.githubusercontent.com/dpavlic/201701-API-6339-D00/master/data/slid_1994.csv'
))

# NA values. What they are and what they are not. All this will return NA.
NA == NA
NA != NA
NA < NA
NA > NA

# But note matching NAs is fine, will not return NA
1 %in% c(NA, 1)
NA %in% c(NA, 1)
c(1, 2, NA) %in% c(1, 3, 2, NA)

# Idiomatic way to check whether a value is + is not NA.
is.na(NA)
!is.na(NA)

# Research Methods 101
# NAs ARE NOT (LIKELY) ZEROS. What we should do with NAs will depend on
# the situation but turning NA values to 0 is WRONG WRONG WRONG.

# What is the relationship between education and wage?
# ifelse evaluates for every row. Takes three arguments:
#   the condition, value if condifion true, value of condition false.
slid_0na <- mutate(slid, wages = ifelse(is.na(wages), 0, wages))

# A plot which shows how the simplest OLS regression changes once we add the
# zeros. Note that plot stacking is 'dumb' which is why we need to add a few
# arguments to our plot.
plot(slid$education, slid$wages, col = 'gray', ylim = c(0, 50),
     xlab = '', ylab = '')
par(new = TRUE)
slid_0na %$%
  plot(education, wages, col = 'gray', axes = FALSE)
# ASIDE...
# Fit a linear model: lm function. This is just an example, and we will not
# go in depth in this class at all all about it. Very briefly, R has a formula
# 'language' built in. The left-hand side of formula is a dependent variable
# (Y) and RHS is 1+ indipendent variables (Xs). Two sides are separed by ~.
slid %$% abline(lm(wages ~ education), col = 'blue')
slid_0na %$% abline(lm(wages ~ education), col = 'red')

# Notice what happened to the coefficients.
summary(lm(wages ~ education, data = slid))
summary(lm(wages ~ education, data = slid_0na))

# We are getting two very different estimates, but we COULD be getting a bias.
# In this case NA does look like 0. Strictly speaking that is probably a bad
# practice but not entirely uncommon.

# What the heck do we do with NAs otherwise?
# That depends. Throw them out, categorize out linear variables.
slid_nowna <- filter(slid, !is.na(wages))

# but notice... we still have NA values elsewhere. In this case, that is not a
# 'big' deal (few NAs in comparison to NAs for wage). But if we throw out all
# NAs from every variable, in some datasets we may throw out a LOT of
# information and introduce a whole host of biases.
summary(slid_nowna)

# Pitfalls: Beware of doing operations with NA values.
na_df <- data.frame(x = 1:3, y = c(NA, 2, NA))
mutate(na_df, z = x + y)

# NA Pitfalls with summary statistics and NA values
min(slid$wages)
median(slid$wages)
max(slid$wages)

# To make this work... use na.rm = TRUE to remove NAs before calculating
# the statistic.
min(slid$wages, na.rm = TRUE)

# However this works -- but not a 'programatic' function.
summary(slid$wages)

# Pitfalls with selection and NA values... where did the NA go?
slid_over15 <- filter(slid, wages > 15)
summary(slid_over15$wages)

# if you want to haul them along
slid_over15 <- filter(slid, wages > 15 | is.na(wages))
summary(slid_over15$wages)

# Wage Gap -------------------------------------------------------------------
# Let's investigate the wage gap between men and women based on the SLID 1994
# (Survey of Labour and Income Dynamics) data. This is pretty naive, no real
# controls but nontheless.
wgap <- filter(slid, !is.na(wages))

# Let's get a histogram. While we're at it, we're also going to get a 'fancy'
# density plot on top.
hist(wgap$age, breaks = 100)
par(new = TRUE)
plot(density(wgap$age),
     axes = FALSE, ylab = '', xlab = '', main = '', col = 'red')

# Note the %<>% : shortcut to:
# gap <- gap %>% ...
wgap %<>%
  mutate(
    # case_when evaluates in order: short-circuit logic.
    age_c = case_when(
      age %in% 1:19 ~ '1-19',
      age %in% 20:29 ~ '21-29',
      age %in% 30:39 ~ '30-39',
      age %in% 40:49 ~ '40-49',
      age >= 50 ~ '50+'
      # NOTE: We can use:
      #         TRUE ~ some_value
      #       to capture anything not captured in the condition above.
      #       Otherwise anything not matched becomes an NA.
    )
  )

# Now let's see the mean wages by sex & age
wgap_summ <- wgap %>%
  group_by(sex, age_c) %>%
    summarise(mean_wage = mean(wages)) %>%
  ungroup()

# Let's introduce a 'hint' of tidyr. Although we prefer to work with
# 'long' data the vast majority of time, the summary above makes it difficult
# to directly calculate out the difference in wages between males and females.
# We can fix that with spread. Spread spread out a 'key' and a 'value' across
# multiple columns. In this example, sex is the 'key' and the value mean_wage.
wgap_summ %>%
  mutate(sex = tolower(sex)) %>%
  spread(sex, mean_wage) %>%
  mutate(wage_difference = male - female) %>%
  # Here we show something quite fancy: mutate_at. Understand it exists,
  # to make your life easier by applying the same operation to multiple
  # columns of your data frame.
  mutate_at(vars(female, male, wage_difference), round, 2)
