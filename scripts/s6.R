library(tidyverse)
library(stringr)

# Exercise ---------------------------------------------------------------------
a <- read_csv('data/earnings_male.csv')
b <- read_csv('data/earnings_female.csv')

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

earn %>%
  # In order to graph the resullt, we need to summarize our data first, and
  # we want a 3-level group-by operation.
  group_by(year, sex, field) %>%
    summarise(earnings = mean(earnings)) %>%
  ungroup() %>%
  # We will divide the earnings in order to give a cleaner scale, and use sex
  # for both group and colour. The final bit will be then to facet the results
  # by field. Then on to graph tidying.
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

