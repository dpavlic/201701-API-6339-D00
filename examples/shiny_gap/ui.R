library(shiny)

continents <- as.list(unique(as.character(gap$continent)))
continents <- append(continents, list('All'), 0)

shinyUI(fluidPage(
  titlePanel("Our Gapminder Data"),

  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "years",
        "Year:",
        min = 1952,
        max = 2007,
        step = 5,
        value = 2007,
        sep = ''
      ),
      selectInput(
        "continents",
        label = h3("Continent"),
        choices = continents
      ),
      checkboxGroupInput(
        "models",
        label = h3("Models"),
        choices = list("Linear Model" = 'lm', "LOESS" = "loess")
      )
    ),
    mainPanel(
      plotOutput("gapPlot")
    )
  )
))
