library(shiny)
library(dplyr)
library(ggplot2)

shinyServer(function(input, output) {
  output$gapPlot <- renderPlot({
    if (input$continents == 'All') {
      gap_out <- gap
      g_point <- geom_point(
        aes(colour = continent), size = 2, shape = 1, alpha = 0.7
      )
    } else {
      gap_out <- gap %>%
        filter(continent == input$continents)
      g_point <- geom_point(size = 2, shape = 1, colour = 'black')
    }

    plt <- gap_out %>%
      filter(year == input$years) %>%
      ggplot(aes(lifeExp, gdpPercap / 1000)) +
      g_point

    if (any(input$models == 'lm')) {
      plt <- plt + geom_smooth(method = 'lm', colour = 'red', se = FALSE)
    }

    if (any(input$models == 'loess')) {
      plt <- plt + geom_smooth(method = 'loess', colour = 'blue', se = FALSE)
    }

    plt +
      coord_fixed() +
      scale_x_continuous(limits = c(20, 85)) +
      scale_y_continuous(limits = c(0, 50)) +
      xlab('Life Expectancy') +
      ylab('GDP Per Capita (thousand $)') +
      theme_bw() +
      theme(
        text = element_text(size = 15),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.15, 0.85)
      )
  })

})
