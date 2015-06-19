library(shiny)
shinyUI(
  fluidPage(
    titlePanel("Vanza Recommender System"),
    sidebarLayout(
      sidebarPanel(
        h2("Personal Characteristics"),
        # the chance to select wich user to test about
        selectInput("userC","Choose the user for a recommendation:",
                    list("User1"="1",
                         "User2"="2",
                         "User3"="3",
                         "User4"="4",
                         "User5"="5",
                         "User6"="6",
                         "User7"="7",
                         "User8"="8",
                         "User9"="9",
                         "User10"="10")),
        textOutput("userC"),
        h3("Preferences for the vacation"),
        numericInput(inputId="NatCityUser",value=sample(x=1:3,size=1),label="How much are you interested in staying in a City",min=0,max=3,step=1)
        ),
    mainPanel(
      tableOutput("tripper"),
      tableOutput("user"))
)))