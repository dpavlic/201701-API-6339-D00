library(gapminder)
library(magrittr)
library(dplyr)
library(stringr)

gap <- gapminder
gap %<>%
  mutate(
    continent = str_replace(continent, 'Oceania', 'Asia & Oceania') %>% 
                  str_replace('^Asia$', 'Asia & Oceania')
  )
