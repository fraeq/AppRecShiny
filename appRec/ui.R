library(shiny)
shinyUI(
  fluidPage(
    titlePanel("Vanza Recommender System"),
    sidebarLayout(
      sidebarPanel(
        h2("Personal Characteristics"),
        #actionButton(inputId="shuffle",label="start"),
        # the chance to select wich user to test about
        selectInput(inputId="userC",label="Choose the user for a recommendation:",""),
        textOutput("userC"),
        h3("Preferences for the vacation"),
        numericInput(inputId="NatCityUser",value=sample(x=0:3,size=1),label="How much are you interested in staying in a City",min=0,max=3,step=1),
        radioButtons(inputId="NatMountUser",label="How much are you interested in staying in the mountains",choices=c(0,1,2,3),selected=sample(x=0:3,size=1),inline=T),
        sliderInput(inputId="NatSea",label="How much are you interested in staying at the beach",min=0,max=3,value=sample(x=0:3,size=1),step=1,round=T)
        ),
    mainPanel(
      tableOutput("tripper"),
      tableOutput("user"))
)))