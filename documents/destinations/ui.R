library(rCharts)

shinyUI(pageWithSidebar(
  headerPanel("Your next European Destination"),
  sidebarPanel(
    textInput("name", label = h3("Your name:"), 
              value = " "),
    radioButtons("country", label = h3("Choose your destination:"),
                 choices = list("Italy" = 1, "France" = 2, "Spain" = 3,
                                "United Kingdom" = 4),selected = 1)),
  mainPanel(
    h3("Your choice"),
    textOutput("otext1"),
    textOutput("otext2"),
    h4("Map"), chartOutput("oMap", "leaflet"))
  
))