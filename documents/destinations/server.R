library(leaflet)
library(rCharts)

countries <- read.csv("countries.csv", header=TRUE)
cities <- read.csv("cities.csv", header=TRUE)

shinyServer(
  function(input,output){
      
    output$otext1 <- renderText({ 
      number <- as.numeric(input$country)
      selcountry <- countries[number,2]
      subcities <- subset(cities,selected == number)
      city1 <- subcities[1,3]
      city2 <- subcities[2,3]
      city3 <- subcities[3,3]
      paste("Hi", input$name, "you have selected", selcountry,". Top destinations are", city1,",", city2, ", and", city3,".")
    })
    
    output$otext2 <- renderText({ 
    paste("Please click on the marker to show the name of the city.")  
    })
    
    output$oMap <- renderMap({
      number <- as.numeric(input$country)
      selcountry <- countries[number,2]
      subcities <- subset(cities,selected == number)
      city1 <- subcities[1,3]
      city2 <- subcities[2,3]
      city3 <- subcities[3,3]
      
      Map <- Leaflet$new()
      
      selcountrylat <- countries[number,3]
      selcountrylong <- countries[number,4]
      
      Map$setView(c(selcountrylat,selcountrylong), zoom=5)
      
      city1lat <- subcities[1,4]
      city1long <- subcities[1,5]
      Map$marker(c(city1lat, city1long),bindPopup=city1)
      city2lat <- subcities[2,4]
      city2long <- subcities[2,5]
      Map$marker(c(city2lat, city2long),bindPopup=city2)
      city3lat <- subcities[3,4]
      city3long <- subcities[3,5]
      Map$marker(c(city3lat, city3long),bindPopup=city3)
      
      Map
      
    })
  
  }
)