library(gapminder)
library(tidyverse)
library(stringr)

# Pipes in function calls ------------------------------------------------------
gap <- gapminder

# What is going here, why is there a pipe inside that... pipe? You can have a
# pipe within a function call. What is happening here is that the string
# replacement is evaulated, and then we pipe the return values of that vector
# to do a second replacement. The principle is really the same as with
# pipes we have seen before. The pipe _within_ the mutate function call and the
# pipe _outside_ will not trip each other up.
gap2 <- gap %>%
  mutate(
    country = str_replace(country, 'Congo, Dem. Rep.', 'Zaire') %>%
      str_replace('Sri Lanka', 'Ceylon')
  )

# Alternatively, the less lazy could do perhaps something like so instead, but
# we are introducing lots of repetition by doing something like that.
gap2 <- gap %>%
  mutate(
    country = str_replace(country, 'Congo, Dem. Rep.', 'Zaire'),
    country = str_replace(country, 'Sri Lanka', 'Ceylon')
  )

# Library conflicts ------------------------------------------------------------

# Conflicts in R happen often enough that there is a base R function called
# conflicts() which can identify them for us. Most of the time, the conflicts
# are unlikely to affect us, particularly within the tidyverse paradigm. But
# not always. Take for example magrittr and tidyr. They both have an extract
# function. So how do we specify which one do we want?

# QUESTION: Are these two sets of library loads the same?
# NOTE: You're going to need to restart R between uses to reload the library.
library(magrittr)
library(tidyr)
extract

# Restart R, and now try:
library(tidyr)
library(magrittr)
extract

# The point is this: the last package loaded by library 'wins' and is going to
# mask off anything that conflicts.

# What about if we want to use both? There's multiple possibilities:
# Let's restart R again, and now let's assume you want tidyr to 'win'. What to
# do if you want to call magrittr extract?

library(magrittr)
library(tidyr)

# We can use the package::function to ensure we get the right function.

magrittr::extract

# But if you want to alias it, we can do that too.
mextract <- magrittr::extract

# Notice we can post-mask the functions.
extract <- magrittr::extract

# We can then reset back to tidyr (since that is the last loaded library), by
# just using rm() to remove the global environment object extract.
rm(extract)

# If you want to avoid the ambiguity entirely, but are too lazy and annoyed to
# type magrittr::extract every single time you can alias the package. This has
# some limitations but nothing we need to worry about too much. We can do this
# with loadNamespace() function and then access the function with $:
mt <- loadNamespace('magrittr')
ty <- loadNamespace('tidyr')
mt$extract
ty$extract

# There are other 3rd party packages that attempt to solve this problem in
# somewhat different ways, but this is good enough for now.

# ggplot review ----------------------------------------------------------------
# Let's restart R and now reload all of the packages we need for the rest of
# the session.
library(gapminder)
library(magrittr)
library(tidyverse)
library(stringr)
library(scales)

# Let's load up gapminder.
gap <- gapminder

# Let's look at how the population has grown over time.
gap %>%
  ggplot(aes(x = year, y = pop)) +
  geom_bar(stat = 'identity')

# Now, firstly, that's a boring graph and pretty limited info. Just population.
# Let's try to see if we can get more info from this.
gap %>%
  ggplot(aes(x = year, y = pop, fill = continent)) +
  geom_bar(stat = 'identity')

# This works, but you're actually stacking all of the different bar values
# together. To make this obvious, let's add a black line.
gap %>%
  ggplot(aes(x = year, y = pop, fill = continent)) +
  geom_bar(stat = 'identity', colour = 'black')

# This is important when it comes to trying to make things clearer by using the
# dodge position. You can't 'stack' and 'dodge' at the same time. Let's try
# doing the dodge. Notice how the scale is clearly off even in scientific
# notation. If we add alpha = 0.5, we can see that ggplot2 is overplotting.
# That is a problem! As of right now, dodge expects only y per x.
gap %>%
  ggplot(aes(x = year, y = pop, fill = continent)) +
  geom_bar(stat = 'identity', position = "dodge", alpha = 0.5)

# How do we fix this? We can summarise the information ourselves. Then it will
# work no problem! Note we need to add as.numeric to pop since it is an
# integer and the value is over 2 billion, so we will get an integer overflow!
# (thank you 32 bit integers in R).
gap %>%
  group_by(year, continent) %>%
  summarise(pop = sum(as.numeric(pop))) %>%
  ggplot(aes(x = year, y = pop, fill = continent)) +
  geom_bar(stat = 'identity', position = "dodge")

# Is the geom_bar representation the best way to deal with these data? Perhaps
# we want to see if a line plot might fit better.
gap %>%
  group_by(year, continent) %>%
  summarise(pop = sum(as.numeric(pop))) %>%
  ggplot(aes(x = year, y = pop, colour = continent)) +
  geom_line()

# Let's make our gdp variable. We'll need it later.
gap <- gap %>%
  mutate(gdp = gdpPercap * pop)

# What does the GDP per capita look for different continents?
gap %>%
  filter(year == 2007) %>%
  ggplot(aes(x = continent, y = gdpPercap)) +
  geom_bar(stat = 'identity')

# Let's dial in on Africa and how the total GDP changes over time.
gap %>%
  filter(continent == "Africa") %>%
  ggplot(aes(x = year, y = gdp)) +
  geom_bar(stat = 'identity')

# What's the relationship between lifeexp and gdpPercap
gap %>%
  ggplot(aes(x = lifeExp , y = gdpPercap, colour = continent)) +
  geom_point(alpha = .6, shape = 1, size = 2) +
  scale_y_continuous(limits = c(0, 50000))

# Note this includes all years. We can use the usual dplyr filtering methods
# to filter things. For example, let's look at just 2007.
gap %>%
  filter(year == 2007) %>%
  ggplot(aes(x = lifeExp , y = gdpPercap, colour = continent)) +
  geom_point(alpha = .6, shape = 1, size = 2) +
  scale_y_continuous(limits = c(0, 50000))

# That doesn't look particularly linear, but let's try to fit a line.
# NOTE: aes() here is defined separately for geom_point and geom_smooth.
#       If it wasn't, you'd get a different line for each continent which is
#       not what we want here.
gap %>%
  filter(year == 2007) %>%
  ggplot() +
  geom_point(aes(x = lifeExp, y = gdpPercap, colour = continent),
             alpha = .6, shape = 1, size = 2) +
  geom_smooth(aes(x = lifeExp, y = gdpPercap), method = 'lm', se = FALSE) +
  scale_y_continuous(limits = c(0, 50000))

# Looks terrible. We can also use a LOESS smoother instead. This is more
# interesting and implies a more subtle relationship.
gap %>%
  filter(year == 2007) %>%
  ggplot() +
  geom_point(aes(x = lifeExp, y = gdpPercap, colour = continent),
             alpha = .6, shape = 1, size = 2) +
  geom_smooth(aes(x = lifeExp, y = gdpPercap), method = 'loess', se = FALSE) +
  scale_y_continuous(limits = c(0, 50000))

# These are fine as exploratory graphs, and fitted line graphs may have a
# purpose elsewhere too. But generally, these graphs have some serious problems.
# These graphs do not "stand alone".

# Let's do something a bit different. Let's look at the MEAN GDP Per Capita
# for each of the contnents, that's nice and informative.
summarise(gap, mean_gdp_cap = mean(gdpPercap))

# Whoops, we need grouped by continent. Also... Ocenia isn't really a
# particulary large continent. Should we take them out? How about we group
# Asia and Oceania together? Let's look at only 2007 for the purposes of this
# exercise, otherwise things are misleading.
gap_o <- gap %>%
    mutate(
    continent = continent %>%
      str_replace('Oceania', 'Asia') %>%
      str_replace('Asia', 'Asia & Oceania')
  ) %>%
  group_by(continent, year) %>%
    summarise(mean_gdp_cap = mean(gdpPercap)) %>%
  ungroup()

# Great... except, can anyone notice what is wrong methodologically with this
# approach? We need a weighted mean.
gap_o <- gap %>%
    mutate(
    continent = continent %>%
      str_replace('Oceania', 'Asia') %>%
      str_replace('Asia', 'Asia & Oceania')
  ) %>%
  group_by(continent, year) %>%
    summarise(mean_gdp_cap = weighted.mean(gdpPercap, pop)) %>%
  ungroup()

gap_o_2007 <- gap_o %>%
  filter(year == 2007)

# We can now graph something with mean GDP per capita.
gap_o_2007 %>%
  ggplot(aes(x = continent, y = mean_gdp_cap, fill = continent)) +
  geom_bar(stat = 'identity')

# This works, but it's a mess. Let's clean up a bit. This is what we want to
# fix:
#   a) Way too many digits! Let's divide the GDP per Capita by 1000
#   b) We need x / y labels to make this more understandable.
#   c) We want the label numbers to use dollars. The scales package can do that
#      and we then pass that information over to labels part of
#      scale_y_continous.
#   d) We need a title, and let's adjust it a bit.
#   e) We need to remove the legend since it adds nothing to the plot.
#   f) Let's use some nicer colours.
#   g) Finally, let's add a source for the graph.
gap_o_2007 %>%
  ggplot(aes(x = continent, y = mean_gdp_cap / 1000, fill = continent)) +
  geom_bar(stat = 'identity') +
  ylab(label = "GDP Per Capita (thousand)") +
  xlab(label = "Continent") +
  scale_y_continuous(labels = dollar) +
  ggtitle("Mean GDP Per Capita in 2007 (Weighted)") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'none') +
  scale_fill_brewer(palette = "Set1") +
  labs(caption = "Source: Gapminder")

# Faceting ---------------------------------------------------------------------

# What we have done so far is all well and good. But notice how, for example,
# we have restricted a lot of our analysis to one year? What if we want to
# show the results for multiple years? Facets to the rescue. Let's look at the
# relationship between gdp per capita and life expectancy once again, but this
# time use facet_grid to plot it for all years.
ggplot(gap) +
  geom_point(aes(x = lifeExp, y = gdpPercap, colour = continent),
             alpha = .6, shape = 1, size = 2) +
  geom_smooth(aes(x = lifeExp, y = gdpPercap), method = 'loess', se = FALSE) +
  facet_wrap(~ year) +
  scale_y_continuous(limits = c(0, 50000))

# Let's get our fancy graph going too. Same idea applies.
gap_o %>%
  ggplot(aes(x = continent, y = mean_gdp_cap / 1000, fill = continent)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~ year) +
  ylab(label = "GDP Per Capita (thousand)") +
  xlab(label = "Continent") +
  scale_y_continuous(labels = dollar) +
  scale_x_discrete(labels = c('AF', 'AM', 'AO', 'EU')) +
  ggtitle("Mean GDP Per Capita in 2007") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'none') +
  scale_fill_brewer(palette = "Set1") +
  labs(caption = "Source: Gapminder")
