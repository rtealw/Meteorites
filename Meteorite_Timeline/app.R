library(shiny)
library(tidyverse)
library(shinythemes)
library(ggplot2)
library(data.table)
library(stringr)
library(shiny)
library(leaflet)
library(rvest)
library(htmltools)
library(sp)
library(rworldmap)
library(geojsonio)
library(lme4)
library(fmsb)

# prepare data

meteorite <- fread("final-meteorite.csv")

# Define UI for application that draws a histogram
ui <- navbarPage("Meteorites",
                theme = shinytheme("sandstone"),
                tabPanel("Latitude",
                fluidRow(column(width = 2,
                  radioButtons(inputId = "fall1",
                               label = "Fall",
                               choices = c("Fell", "Found"))),
                  column(width = 2, radioButtons(inputId = "point1",
                               label = "Point?",
                               choices = c("Include", "Not Include"))),
                  column(width = 2, radioButtons(inputId = "density1",
                               label = "Density?",
                               choices = c("Include", "Not Include"))),
                  column(width = 6, sliderInput(inputId = "start1",
                              label = "Start Year",
                              min = 0,
                              max = 2020,
                              value = 1900,
                              sep = ""))
                ),
                fluidRow(column(width = 12, plotOutput("plot1")))),
                tabPanel("Longitude",
                fluidRow(column(width = 2,
                  radioButtons(inputId = "fall2",
                               label = "Fall",
                               choices = c("Fell", "Found"))),
                  column(width = 2, radioButtons(inputId = "point2",
                               label = "Point?",
                               choices = c("Include", "Not Include"))),
                  column(width = 2, radioButtons(inputId = "density2",
                               label = "Density?",
                               choices = c("Include", "Not Include"))),
                  column(width = 6, sliderInput(inputId = "start2",
                              label = "Start Year",
                              min = 0,
                              max = 2020,
                              value = 1900,
                              sep = ""))
                ),
                fluidRow(column(width = 12, plotOutput("plot2"))))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$plot1 <- renderPlot({
    p <- meteorite %>%
      filter(fall == input$fall1) %>%
      ggplot(aes(x = year, lat)) +
      geom_smooth(aes(color = freq.class), se = FALSE) +
      xlab("Year") +
      ylab("Latitude") +
      ggtitle("Latitude vs. Year") +
      labs(color = "Most Common Classes") +
      xlim(input$start1, 2020) +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), panel.background = element_blank())
    if(input$point1 == "Include") {
      p = p + geom_point(aes(color = freq.class))
    }
    if(input$density1 == "Include") {
      p = p + geom_density2d()
    }
    p
  })
  output$plot2 <- renderPlot({
    p <- meteorite %>%
      filter(fall == input$fall2) %>%
      ggplot(aes(x = year, lon)) +
      geom_smooth(aes(color = freq.class), se = FALSE) +
      xlab("Year") +
      ylab("Longitude") +
      ggtitle("Longitude vs. Year") +
      labs(color = "Most Common Classes") +
      xlim(input$start2, 2020) +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), panel.background = element_blank())
    if(input$point2 == "Include") {
      p = p + geom_point(aes(color = freq.class))
    }
    if(input$density2 == "Include") {
      p = p + geom_density2d()
    }
    p
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

