library(shiny)
shinyUI(
  fluidPage(
    titlePanel("Vanza Recommender System"),
    sidebarLayout(
      sidebarPanel(
        h2("Personal Characteristics"),
        h3("Choosen user"),
        textInput(inputId="user",label="The choosen User is:",value=user),
        h3("Preferences for the vacation")
        ),
    mainPanel(
      tableOutput("tripper"))
)))