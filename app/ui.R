library(shiny)
shinyUI(
  fluidPage(
    titlePanel("Boxplots about the events"),
    sidebarLayout(
      sidebarPanel("caption"),
    mainPanel(tableOutput("user"),
              tableOutput("tripper"))
)))