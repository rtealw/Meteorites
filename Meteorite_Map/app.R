library(shiny)
library(shinythemes)
library(tidyverse)
library(leaflet)
library(sp)
library(data.table)
library(readr)
library(geojsonio)


meteorite <- fread("final-meteorite.csv")

countries <- geojson_read("countries.json", what = "sp")


ui <- navbarPage("Meteorites",
             theme = shinytheme("darkly"),
             
             tabPanel("Median Years",   
                        mainPanel(
                        fluidPage(
                          h1("Median Years"),
                          leafletOutput("map1"),
                          br(),
                          p("Over the years, meteorites have entered our atmosphere and landed in various locations. 
                            Our records date back to the year 860 and reach forward to the year 2013. In this 
                            world map above, each country is highlighted to show the median year of meteorite 
                            landings in that specific country. To be more specific, there is an equal number of 
                            years when meteorites have landed before the representing year and after. Countries 
                            like Morocco, Argentina, and Madagascar are a darker red meaning the meteorites that 
                            have landed and documented from that country are more recent. Countries like Russia, 
                            Ireland, and Croatia are from earlier years meaning the meteorites that have landed and 
                            documented from that country are older.")))),
             
             tabPanel("Median Mass",
                      mainPanel(
                        fluidPage(
                          h1("Median Mass"),
                          leafletOutput("map2"),
                          br(),
                          p("Our map shows the weight in grams of the median meteorite mass per country including 
                            the continent Antarctica. Some of the most interesting findings were how small the 
                            meteorites in Antarctica were. Thousands of meteorites are documented to have 
                            landed in Antarctica, but the median mass is only 13.45g. Countries such as 
                            Australia, Kenya, and Oman are also very low weighted meteorites. On the other  
                            side of the spectrum, Somalia, Mongolia, and Greenland all have some of the  
                            heaviest recorded massed meteorites, but Columbia has the heaviest median mass 
                            with 412,739g and we wonder if this is really a meteorite.")))),

             tabPanel("Most Common Classes",
                      mainPanel(
                        fluidPage(
                          h1("Most Common Classes"),
                          leafletOutput("map3"),
                          br(),
                          p("Each meteorite is classified and we have mapped the most common classifications of 
                            meteorites by country including the continent Antarctica to find if classification has 
                            any correlation with global location. Looking at the graph the dark red can be seen 
                            in the larger countries of our world, Russia, Canada, Greenland, and the continent 
                            Antarctica. The North and South poles of our world represent the most common class 
                            of L6, an ordinary chondrite from the L group that is petrologic type 6. The African 
                            country Chad has the most common of meteorite class types, H. The H type is an 
                            ordinary chondrites, which seems to be more common in more central countries.
                            Hover the mouse of the country you wish to see it's most common meteorite class")))),
             
             tabPanel("Meteorite Density",
                      mainPanel(
                        fluidPage(
                          h1("Meteorite Density"),
                          leafletOutput("map4"),
                          br(),
                          p("This map shows the number of meteorites there are in a country/continent. 
                            When a continent has smaller countries, one can see how concentrated meteorites 
                            fall in specific locations of a continent. When there are larger countries, 
                            covering more space of the continent, it doesn’t show how concentrated parts 
                            of the continent really are. For example in the United States of America, the 
                            state Alaska is also apart of the collective USA meteorite data even though it 
                            is in a very different location than the other 49 states. Smaller countries 
                            such as the central African countries and the countries surrounding Brazil, 
                            each have about 5 or less meteorites land in their territory. The continent 
                            Antarctica is the darkest color on the map because it has 22,090 documented 
                            meteorite landings, but more interestingly the clusters of each meteorite 
                            landing looks like a larger meteorite erupted into thousands of smaller 
                            pieces and froze in that landed location. At the same time, Antarctica has 
                            more two times the surface area in its territory the the largest country 
                            (Russia), therefore, it would be expected that more meteorites would land 
                            there. It is still interesting to point out that the ocean covers the majority 
                            of our earth and most likely holds the most meteorites due to the lack of 
                            ability to record them. Still the idea of meteorites landing near the south 
                            pole looks like an accurate description of meteorite landing in comparison to 
                            this world choropleth map.")))))
  

server <- function(input, output) {
  
  #PAGE 1
  output$map1 <- renderLeaflet({
    bins <- c(1800, 1850, 1900, 1950, 1975, 2000, Inf)
    colors <- colorBin(palette = "YlOrRd",
                       domain = countries@data$year,
                       bins = bins)
    
    map1 <- countries %>%
      leaflet() %>%
      addTiles() %>%
      setView(50, 0, 1)
    
    map1 %>%
      addPolygons(fillColor = ~colors(year),
                  weight = 2,
                  opacity = 1,
                  fillOpacity = 0.7,
                  label = ~as.character(year),
                  popup = ~name) %>%
      addLegend(pal = colors,
                values = ~year)
  })
  
  #PAGE 2
  output$map2 <- renderLeaflet({
    bins <- c(0, 250, 500, 1000, 2000, 5000, 10000, Inf)
    colors <- colorBin(palette = "YlOrRd",
                       domain = countries@data$mass,
                       bins = bins)
    
    map2 <- countries %>%
      leaflet() %>%
      addTiles() %>%
      setView(50, 0, 1)
    
    map2 %>%
      addPolygons(fillColor = ~colors(mass),
                  weight = 2,
                  opacity = 1,
                  fillOpacity = 0.7,
                  label = ~as.character(mass),
                  popup = ~name) %>%
      addLegend(pal = colors,
                values = ~mass)
  })
  
  #PAGE 3
  output$map3 <- renderLeaflet({
    colors <- colorFactor(palette = "YlOrRd",
                          domain = countries@data$class)
    
    map3 <- countries %>%
      leaflet() %>%
      addTiles() %>%
      setView(50, 0, 1)
    
    map3 %>%
      addPolygons(fillColor = ~colors(class),
                  weight = 2,
                  opacity = 1,
                  fillOpacity = 0.7,
                  label = ~as.character(class),
                  popup = ~name) 
  })
  
  #PAGE 4
  output$map4 <- renderLeaflet({
    bins <- c(0, 5, 10, 15, 20, 25, 50, 100, 1000, Inf)
    colors <- colorBin(palette = "YlOrRd",
                       domain = countries@data$count,
                       bins = bins)
    
    map4 <- countries %>%
      leaflet() %>%
      addTiles() %>%
      setView(50, 0, 1)
    
    map4 %>%
      addPolygons(fillColor = ~colors(count),
                  weight = 2,
                  opacity = 1,
                  fillOpacity = 0.7,
                  label = ~as.character(count),
                  popup = ~name) %>%
      addLegend(pal = colors,
                values = ~count)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

