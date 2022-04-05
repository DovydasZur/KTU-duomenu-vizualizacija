install.packages("shiny")
install.packages("shinyWidgets")
install.packages("shinythemes")
library(readr)
library(tidyverse)
library(shiny)
library(shinythemes)
library(shinyWidgets)

data = read.csv("lab_sodra.csv", fileEncoding="UTF-8")
data1 = data %>%
  filter(ecoActCode == 412000)

ui = fluidPage(
  titlePanel("Gyvenamuju ir negyvenamuju pastatu statyba"),
  #dateRangeInput("dates", label = h3("Date range"), format = "yyyymmdd"),
  #hr(),
  #fluidRow(column(4, verbatimTextOutput("value"))),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(inputId = "imon_kod", label = "Imones pavadinimas", choices = NULL, selected = NULL)),
    mainPanel(tabsetPanel(
      tabPanel("Grafikas", plotOutput("plot")),
      tabPanel("Lentele", tableOutput("table"))
    )
    )
   ),
  setBackgroundColor(
    color = c("#f2dcda","silver")
  )
  )

server = function(input, output, session) {
  updateSelectizeInput(session, "imon_kod", choices = data1$name, server = TRUE)
  
  output$table = renderTable(
    data1 %>%
      filter(name == input$imon_kod) , digits = 0
  )
  
  output$plot = renderPlot(
    data1 %>%
      filter(name == input$imon_kod) %>%
      ggplot(aes(x = month, y = avgWage, group = name, colour = name)) +
      theme_linedraw() + 
      geom_point(color = "orange") +
      geom_line(colour = "gold")      
  )
  #output$value = renderPrint({ month == input$dates })
}

shinyApp(ui, server)

